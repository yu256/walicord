//! Application-layer use case for recording an expense entry.
//!
//! Owns the full critical section — pre-lock fast bail, per-ledger lock,
//! post-lock recheck, lock-internal load, compose, render, commit — so the
//! adapter only has to fetch the live roster, build the command DTO, and
//! translate the typed outcome into a Discord response. The transport context
//! (serenity `Context`, target `ChannelId`) and the presentation render are
//! both crossed through per-request ports (`CanonicalThreadAppender`,
//! `LocatorBindingPublisher`, `CanonicalThreadReader`, `ExpenseEntryRenderer`)
//! so this layer never references serenity or walicord-presentation types.

use walicord_domain::model::MemberId;

use crate::{
    Clock,
    ledger::{
        DiscordLedgerSourceDescriptor, ExpenseRecorded, LedgerEntryId, LedgerId,
        canonical_read::{CanonicalReadError, CanonicalThreadReader},
        canonical_write::{
            CanonicalThreadAppender, CommitOrchestrationError, CommitOutcome,
            LocatorBindingPublisher, commit_authoritative_v1,
        },
        expense_session::{ExpenseParticipantSelection, ExpenseSession},
        expense_write::{
            ExpenseWriteOrchestrationError, RecordTimeOutcome, RecordableExpenseEntry,
            build_canonical_envelope, compose_expense_entry,
        },
        ledger_chain_genesis_sha256_v1,
        observability::LedgerObservability,
        participant_resolution::{ParticipantDrift, RosterSnapshot},
        projection::NextLedgerEntryIdError,
        write_coordinator::{
            UncertainWriteRegistry, WriteCoordinator, WriteTargetKey, resolve_uncertain_write_v1,
        },
    },
};

/// Command issued by the adapter to record an expense. Carries everything the
/// use case needs that the adapter must fetch (live roster, source descriptor,
/// actor id, target binding); the in-flight session itself is borrowed since
/// the adapter still owns the `ClaimedExpenseSession` lifecycle for its own
/// response handling.
#[derive(Debug, Clone)]
pub struct RecordExpenseCommand<'a> {
    pub session: &'a ExpenseSession,
    pub roster: &'a RosterSnapshot,
    pub source_descriptor: DiscordLedgerSourceDescriptor,
    pub actor_id: MemberId,
    pub ledger_id: LedgerId,
    pub write_target: WriteTargetKey,
}

/// Closed outcome of [`record_expense_v1`]. Drift detection short-circuits
/// before any canonical write so the adapter can re-render the confirmation
/// page against the refreshed roster (criterion 81 / 111). The commit outcomes
/// mirror [`CommitOutcome`] plus the pre/post-lock `UncertainBlocked` case so
/// the adapter renders a uniform recovery banner.
#[derive(Debug, Clone, PartialEq)]
pub enum RecordExpenseOutcome {
    Recorded {
        entry_id: LedgerEntryId,
        recorded_event: ExpenseRecorded,
    },
    DriftDetected {
        drift: Vec<ParticipantDrift>,
        refreshed: Vec<ExpenseParticipantSelection>,
        defaulted_members: Vec<MemberId>,
    },
    UncertainBlocked,
    UncertainAppendFailed,
}

/// Output port that turns a freshly composed [`RecordableExpenseEntry`] into the
/// validated public body the canonical thread expects. Adapter implementations
/// own the presentation dependency (walicord-presentation + walicord-i18n);
/// the application boundary surfaces only the validated `String`.
pub trait ExpenseEntryRenderer: Send + Sync {
    fn render_public_body(
        &self,
        recordable: &RecordableExpenseEntry,
        ledger_id: LedgerId,
    ) -> Result<String, ExpenseRenderError>;
}

/// Application-pure error returned by [`ExpenseEntryRenderer`]. Adapters MAY
/// log the typed presentation error before erasing it into this envelope so the
/// typed source stays observable in production logs.
#[derive(Debug, thiserror::Error)]
#[error("expense canonical body render failed")]
pub struct ExpenseRenderError {
    #[source]
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl ExpenseRenderError {
    pub fn new() -> Self {
        Self { source: None }
    }

    pub fn with_source(source: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self {
            source: Some(Box::new(source)),
        }
    }
}

impl Default for ExpenseRenderError {
    fn default() -> Self {
        Self::new()
    }
}

/// Failures that prevent [`record_expense_v1`] from reaching a terminal
/// outcome. Drift / uncertain blocked / append-failed cases are returned as
/// [`RecordExpenseOutcome`] variants rather than errors so the adapter branches
/// uniformly on outcome.
#[derive(Debug, thiserror::Error)]
pub enum RecordExpenseError {
    #[error("compose: {0}")]
    Compose(#[from] ExpenseWriteOrchestrationError),
    #[error("read: {0}")]
    Read(#[from] CanonicalReadError),
    #[error("next entry id: {0}")]
    NextEntryId(#[from] NextLedgerEntryIdError),
    #[error("body render: {0}")]
    Render(#[from] ExpenseRenderError),
    #[error("commit: {0}")]
    Commit(#[from] CommitOrchestrationError),
}

/// Record an expense entry through the canonical-write critical section.
///
/// Sequence: pre-lock fast bail (resolve_uncertain_write_v1) → acquire the
/// per-ledger lock → post-lock recheck → in-lock `load_ledger_head` →
/// `compose_expense_entry` (drift returns early) → render body → encode
/// envelope → `commit_authoritative_v1`. The lock guard is held across the
/// entire post-bail flow; load happens inside the lock so the
/// next-entry-id / previous-hash pair is race-free against concurrent writers.
#[allow(clippy::too_many_arguments)]
pub async fn record_expense_v1<A, L, R, E>(
    appender: &A,
    locator: &L,
    reader: &R,
    renderer: &E,
    uncertain_writes: &UncertainWriteRegistry,
    write_coordinator: &WriteCoordinator,
    observability: &dyn LedgerObservability,
    clock: &dyn Clock,
    command: RecordExpenseCommand<'_>,
) -> Result<RecordExpenseOutcome, RecordExpenseError>
where
    A: CanonicalThreadAppender,
    L: LocatorBindingPublisher,
    R: CanonicalThreadReader,
    E: ExpenseEntryRenderer,
{
    let RecordExpenseCommand {
        session,
        roster,
        source_descriptor,
        actor_id,
        ledger_id,
        write_target,
    } = command;

    if !resolve_uncertain_write_v1(reader, uncertain_writes, observability, clock, ledger_id).await
    {
        return Ok(RecordExpenseOutcome::UncertainBlocked);
    }

    let lock = write_coordinator.lock_for(write_target);
    let _guard = lock.lock().await;

    if !resolve_uncertain_write_v1(reader, uncertain_writes, observability, clock, ledger_id).await
    {
        return Ok(RecordExpenseOutcome::UncertainBlocked);
    }

    let load = reader.load_verified_thread().await?;
    let next_entry_id = load.next_entry_id()?;
    let previous_hash = load
        .snapshot()
        .current_head_hash()
        .unwrap_or_else(|| ledger_chain_genesis_sha256_v1(ledger_id));

    let compose = compose_expense_entry(
        session,
        roster,
        next_entry_id,
        source_descriptor,
        actor_id,
        clock,
    )?;

    let recordable = match compose {
        RecordTimeOutcome::DriftDetected {
            drift,
            refreshed,
            defaulted_members,
            dropped_overrides: _,
        } => {
            return Ok(RecordExpenseOutcome::DriftDetected {
                drift,
                refreshed,
                defaulted_members,
            });
        }
        RecordTimeOutcome::Ready { entry, .. } => entry,
    };

    let envelope = build_canonical_envelope(ledger_id, previous_hash, recordable.entry().clone())?;
    let body = renderer.render_public_body(&recordable, ledger_id)?;

    let outcome = commit_authoritative_v1(
        appender,
        locator,
        uncertain_writes,
        observability,
        clock,
        write_target,
        ledger_id,
        recordable.entry(),
        &envelope,
        &body,
    )
    .await?;

    Ok(match outcome {
        CommitOutcome::Recorded => RecordExpenseOutcome::Recorded {
            entry_id: recordable.id(),
            recorded_event: recordable.event().clone(),
        },
        CommitOutcome::UncertainAppendFailed => RecordExpenseOutcome::UncertainAppendFailed,
    })
}
