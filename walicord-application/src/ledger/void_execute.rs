//! Application-layer use case for committing a confirmed void.
//!
//! Owns the full critical section — pre-lock fast bail, per-ledger lock,
//! post-lock recheck, lock-internal load, target re-validation (criterion 112
//! / 135), envelope compose, render, commit — so the adapter only has to
//! claim the in-flight void session, build the command DTO, and translate the
//! typed outcome into a Discord response. The render crosses through the
//! [`VoidEntryRenderer`] output port so this layer never depends on
//! walicord-presentation.

use walicord_domain::model::MemberId;

use crate::{
    Clock,
    ledger::{
        DiscordLedgerSourceDescriptor, LedgerEntry, LedgerEntryId, LedgerId,
        canonical_read::{CanonicalReadError, CanonicalThreadReader},
        canonical_write::{
            CanonicalThreadAppender, CommitOrchestrationError, CommitOutcome,
            LocatorBindingPublisher, commit_authoritative_v1,
        },
        expense_session::{VoidSession, VoidSessionKey, VoidSessionStore},
        observability::LedgerObservability,
        projection::{NextLedgerEntryIdError, VerifiedLedgerEntryView},
        void_flow::{VoidComposeError, compose_void_entry, enumerate_void_candidates},
        write_coordinator::{
            UncertainWriteRegistry, WriteCoordinator, WriteTargetKey, resolve_uncertain_write_v1,
        },
    },
};

/// Output port that turns a freshly composed void entry plus the target view
/// into the validated public body the canonical thread expects. Adapter
/// implementations own the presentation dependency; the application boundary
/// surfaces only the validated `String`.
pub trait VoidEntryRenderer: Send + Sync {
    fn render_public_body(
        &self,
        entry: &LedgerEntry,
        target: &VerifiedLedgerEntryView,
        ledger_id: LedgerId,
    ) -> Result<String, VoidRenderError>;
}

/// Application-pure error returned by [`VoidEntryRenderer`].
#[derive(Debug, thiserror::Error)]
#[error("void canonical body render failed")]
pub struct VoidRenderError {
    #[source]
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl VoidRenderError {
    pub fn new() -> Self {
        Self { source: None }
    }

    pub fn with_source(source: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self {
            source: Some(Box::new(source)),
        }
    }
}

impl Default for VoidRenderError {
    fn default() -> Self {
        Self::new()
    }
}

/// Command issued by the adapter to commit a confirmed void.
#[derive(Debug, Clone)]
pub struct VoidExecuteCommand<'a> {
    pub session: &'a VoidSession,
    pub session_key: VoidSessionKey,
    pub ledger_id: LedgerId,
    pub actor_id: MemberId,
    pub write_target: WriteTargetKey,
    pub source_descriptor: DiscordLedgerSourceDescriptor,
}

/// Closed outcome of [`void_execute_v1`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VoidExecuteOutcome {
    /// Void entry posted; the in-flight session has been cleared.
    Recorded { entry_id: LedgerEntryId },
    /// The chosen void target is no longer in the latest voidable window
    /// (criterion 112 / 135): another writer voided it, or it was pushed past
    /// the window. The in-flight session has been cleared.
    TargetGone,
    /// Uncertain-write retain blocks the per-ledger write target.
    UncertainBlocked,
    /// `append_authoritative` reported an ambiguous failure; the retain stays
    /// `Live` for criterion-217 / 279 lazy retry.
    UncertainAppendFailed,
}

/// Failures that prevent [`void_execute_v1`] from reaching a terminal outcome.
#[derive(Debug, thiserror::Error)]
pub enum VoidExecuteError {
    #[error("read: {0}")]
    Read(#[from] CanonicalReadError),
    #[error("next entry id: {0}")]
    NextEntryId(#[from] NextLedgerEntryIdError),
    #[error("candidate enumeration: {0}")]
    Candidates(#[from] crate::ledger::void_flow::VoidCandidateEnumerationError),
    #[error("compose: {0}")]
    Compose(#[from] VoidComposeError),
    #[error("body render: {0}")]
    Render(#[from] VoidRenderError),
    #[error("commit: {0}")]
    Commit(#[from] CommitOrchestrationError),
}

/// Commit a confirmed void through the canonical-write critical section.
///
/// Sequence: pre-lock fast bail → per-ledger lock → post-lock recheck →
/// in-lock load → enumerate the voidable window and look up the chosen
/// target (if missing, clear the void session and return `TargetGone`) →
/// `compose_void_entry` → render → `commit_authoritative_v1`. On success the
/// in-flight void session is cleared.
#[allow(clippy::too_many_arguments)]
pub async fn void_execute_v1<A, L, R, E>(
    appender: &A,
    locator: &L,
    reader: &R,
    renderer: &E,
    void_sessions: &VoidSessionStore,
    uncertain_writes: &UncertainWriteRegistry,
    write_coordinator: &WriteCoordinator,
    observability: &dyn LedgerObservability,
    clock: &dyn Clock,
    command: VoidExecuteCommand<'_>,
) -> Result<VoidExecuteOutcome, VoidExecuteError>
where
    A: CanonicalThreadAppender,
    L: LocatorBindingPublisher,
    R: CanonicalThreadReader,
    E: VoidEntryRenderer,
{
    let VoidExecuteCommand {
        session,
        session_key,
        ledger_id,
        actor_id,
        write_target,
        source_descriptor,
    } = command;

    if !resolve_uncertain_write_v1(reader, uncertain_writes, observability, clock, ledger_id).await
    {
        return Ok(VoidExecuteOutcome::UncertainBlocked);
    }

    let lock = write_coordinator.lock_for(write_target);
    let _guard = lock.lock().await;

    if !resolve_uncertain_write_v1(reader, uncertain_writes, observability, clock, ledger_id).await
    {
        return Ok(VoidExecuteOutcome::UncertainBlocked);
    }

    let load = reader.load_verified_thread().await?;
    let next_entry_id = load.next_entry_id()?;
    let target_id = session
        .selection()
        .map(|selection| selection.target_entry_id())
        .ok_or(VoidComposeError::SessionNotConfirming)?;

    let Some(target_view) = enumerate_void_candidates(&load)?
        .into_iter()
        .find(|view| view.entry().id == target_id)
    else {
        void_sessions.clear(session_key);
        return Ok(VoidExecuteOutcome::TargetGone);
    };

    let (entry, envelope) = compose_void_entry(
        session,
        &load,
        ledger_id,
        actor_id,
        next_entry_id,
        source_descriptor,
        clock,
    )?;

    let body = renderer.render_public_body(&entry, &target_view, ledger_id)?;

    let outcome = commit_authoritative_v1(
        appender,
        locator,
        uncertain_writes,
        observability,
        clock,
        write_target,
        ledger_id,
        &entry,
        &envelope,
        &body,
    )
    .await?;

    Ok(match outcome {
        CommitOutcome::Recorded => {
            void_sessions.clear(session_key);
            VoidExecuteOutcome::Recorded { entry_id: entry.id }
        }
        CommitOutcome::UncertainAppendFailed => VoidExecuteOutcome::UncertainAppendFailed,
    })
}
