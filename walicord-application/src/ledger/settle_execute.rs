//! Application-layer use case for committing a previewed settlement.
//!
//! Owns the full critical section — pre-lock fast bail, per-ledger lock,
//! post-lock recheck, lock-internal load, compose-from-preview,
//! preview-commit guard, render, commit — so the adapter only has to defer
//! the interaction, build the command DTO, and translate the typed outcome
//! into a Discord response. The presentation render crosses through the
//! [`SettlementEntryRenderer`] output port so this layer never depends on
//! walicord-presentation.

use walicord_domain::model::MemberId;

use crate::{
    Clock,
    ledger::{
        DiscordLedgerSourceDescriptor, LedgerEntryId, LedgerId, SettlementRecordError,
        canonical_read::{CanonicalReadError, CanonicalThreadReader},
        canonical_write::{
            CanonicalThreadAppender, CommitOrchestrationError, CommitOutcome,
            LocatorBindingPublisher, commit_authoritative_v1,
        },
        observability::LedgerObservability,
        preview_store::{
            PreviewCommitGuard, PreviewStore, PreviewStoreKey, PreviewStoreTransition,
        },
        projection::NextLedgerEntryIdError,
        settle_flow::{
            RecordableSettlementEntry, SettleAttemptError, SettleAttemptOutcome,
            compose_settlement_entry_from_preview,
        },
        write_coordinator::{
            UncertainWriteRegistry, WriteCoordinator, WriteTargetKey, resolve_uncertain_write_v1,
        },
    },
};

/// Output port that turns a freshly composed [`RecordableSettlementEntry`] into
/// the validated public body the canonical thread expects. Adapter
/// implementations own the presentation dependency; the application boundary
/// surfaces only the validated `String`.
pub trait SettlementEntryRenderer: Send + Sync {
    fn render_public_body(
        &self,
        recordable: &RecordableSettlementEntry,
        ledger_id: LedgerId,
    ) -> Result<String, SettlementRenderError>;
}

/// Application-pure error returned by [`SettlementEntryRenderer`]. Adapters
/// MAY log the typed presentation error before erasing it into this envelope
/// so the typed source stays observable in production logs.
#[derive(Debug, thiserror::Error)]
#[error("settlement canonical body render failed")]
pub struct SettlementRenderError {
    #[source]
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl SettlementRenderError {
    pub fn new() -> Self {
        Self { source: None }
    }

    pub fn with_source(source: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self {
            source: Some(Box::new(source)),
        }
    }
}

impl Default for SettlementRenderError {
    fn default() -> Self {
        Self::new()
    }
}

/// Command issued by the adapter to commit a previewed settlement.
#[derive(Debug, Clone, Copy)]
pub struct SettleExecuteCommand {
    pub ledger_id: LedgerId,
    pub actor_id: MemberId,
    pub write_target: WriteTargetKey,
    pub source_descriptor: DiscordLedgerSourceDescriptor,
}

/// Closed outcome of [`settle_execute_v1`]. The compose-from-preview validation
/// errors fold into `AttemptFailed` so the adapter looks up the matching
/// user-facing copy via a single helper.
#[derive(Debug, thiserror::Error)]
pub enum SettleExecuteOutcome {
    /// Settlement entry posted; preview commit finished.
    #[error("settle execute: recorded entry {entry_id:?}")]
    Recorded { entry_id: LedgerEntryId },
    /// No preview is currently stored for this actor on this ledger. The actor
    /// must run `/review` (or panel `清算確認`) first (criterion 136).
    #[error("settle execute: no preview was stored before /settle")]
    NoPreviewRequired,
    /// The previewed plan resolved to zero transfers — no canonical entry was
    /// posted. The preview is cleared so the actor can retry against a fresh
    /// snapshot.
    #[error("settle execute: previewed plan resolved to no transfers")]
    NoTransferNeeded,
    /// Uncertain-write retain blocks the per-ledger write target. The adapter
    /// renders the recovery banner.
    #[error("settle execute: uncertain write retain blocks the canonical write")]
    UncertainBlocked,
    /// `append_authoritative` reported an ambiguous failure; the retain stays
    /// `Live` for criterion-217 / 279 lazy retry. The adapter renders the
    /// recovery banner.
    #[error("settle execute: canonical append failed; retain remains Live")]
    UncertainAppendFailed,
    /// Compose-from-preview failed; the application has already applied the
    /// clear-preview policy. The adapter maps the typed error to user copy.
    #[error("settle execute: compose-from-preview failed: {error}")]
    AttemptFailed {
        #[source]
        error: SettleAttemptError,
    },
}

/// Pre-port / port-side failures that prevent [`settle_execute_v1`] from
/// reaching a terminal outcome.
#[derive(Debug, thiserror::Error)]
pub enum SettleExecuteError {
    #[error("read: {0}")]
    Read(#[from] CanonicalReadError),
    #[error("next entry id: {0}")]
    NextEntryId(#[from] NextLedgerEntryIdError),
    #[error("body render: {0}")]
    Render(#[from] SettlementRenderError),
    #[error("commit: {0}")]
    Commit(#[from] CommitOrchestrationError),
}

/// Whether the compose-from-preview error implies that the stored preview is
/// no longer recoverable (head moved, expired, record-time validation
/// rejected) and should be cleared. `Store(CommitInProgress)` deliberately
/// stays — that's another concurrent settle still resolving.
pub fn should_clear_preview_after_settle_error(error: &SettleAttemptError) -> bool {
    matches!(
        error,
        SettleAttemptError::StaleHead { .. }
            | SettleAttemptError::Expired { .. }
            | SettleAttemptError::Record(SettlementRecordError::PreviewNotDelivered)
            | SettleAttemptError::Record(_)
            | SettleAttemptError::EnvelopeEncode(_)
    )
}

/// Commit a previewed settlement through the canonical-write critical section.
///
/// Sequence: pre-lock fast bail → per-ledger lock → post-lock recheck →
/// in-lock load → `compose_settlement_entry_from_preview` (validation failures
/// fold to `AttemptFailed` plus the clear-policy from
/// [`should_clear_preview_after_settle_error`]) → render → preview commit
/// guard → `commit_authoritative_v1` → on success `preview_commit.finish()`,
/// on append failure the guard's `Drop` aborts the commit so the preview can
/// be retried.
#[allow(clippy::too_many_arguments)]
pub async fn settle_execute_v1<A, L, R, E>(
    appender: &A,
    locator: &L,
    reader: &R,
    renderer: &E,
    preview_store: &PreviewStore,
    uncertain_writes: &UncertainWriteRegistry,
    write_coordinator: &WriteCoordinator,
    observability: &dyn LedgerObservability,
    clock: &dyn Clock,
    command: SettleExecuteCommand,
) -> Result<SettleExecuteOutcome, SettleExecuteError>
where
    A: CanonicalThreadAppender,
    L: LocatorBindingPublisher,
    R: CanonicalThreadReader,
    E: SettlementEntryRenderer,
{
    let SettleExecuteCommand {
        ledger_id,
        actor_id,
        write_target,
        source_descriptor,
    } = command;
    let key = PreviewStoreKey::new(ledger_id, actor_id);

    if !resolve_uncertain_write_v1(reader, uncertain_writes, observability, clock, ledger_id).await
    {
        return Ok(SettleExecuteOutcome::UncertainBlocked);
    }

    let lock = write_coordinator.lock_for(write_target);
    let _guard = lock.lock().await;

    if !resolve_uncertain_write_v1(reader, uncertain_writes, observability, clock, ledger_id).await
    {
        return Ok(SettleExecuteOutcome::UncertainBlocked);
    }

    let load = reader.load_verified_thread().await?;
    let next_entry_id = load.next_entry_id()?;
    let Some(preview_instance_id) = preview_store
        .current(key)
        .map(|state| state.preview_instance_id())
    else {
        return Ok(SettleExecuteOutcome::NoPreviewRequired);
    };

    let attempt = match compose_settlement_entry_from_preview(
        load.snapshot(),
        ledger_id,
        actor_id,
        next_entry_id,
        source_descriptor,
        preview_store,
        clock,
    ) {
        Ok(outcome) => outcome,
        Err(error) => {
            if should_clear_preview_after_settle_error(&error) {
                // Intentional swallow: ClearMatching only removes when the
                // stored instance still matches; a mismatch means another
                // interaction already replaced the preview and we must not
                // touch it.
                let _ = preview_store.transition(
                    key,
                    PreviewStoreTransition::ClearMatching {
                        preview_instance_id,
                    },
                );
            }
            return Ok(SettleExecuteOutcome::AttemptFailed { error });
        }
    };

    let (entry, envelope) = match attempt {
        SettleAttemptOutcome::RecordableEntry { entry, envelope } => (entry, envelope),
        SettleAttemptOutcome::NoOp => {
            if let Err(error) = preview_store.transition(
                key,
                PreviewStoreTransition::ClearMatching {
                    preview_instance_id,
                },
            ) {
                return Ok(SettleExecuteOutcome::AttemptFailed {
                    error: SettleAttemptError::Store(error),
                });
            }
            return Ok(SettleExecuteOutcome::NoTransferNeeded);
        }
    };

    let body = renderer.render_public_body(&entry, ledger_id)?;
    let preview_commit = match PreviewCommitGuard::begin(preview_store, key, preview_instance_id) {
        Ok(guard) => guard,
        Err(error) => {
            return Ok(SettleExecuteOutcome::AttemptFailed {
                error: SettleAttemptError::Store(error),
            });
        }
    };

    let outcome = commit_authoritative_v1(
        appender,
        locator,
        uncertain_writes,
        observability,
        clock,
        write_target,
        ledger_id,
        entry.entry(),
        &envelope,
        &body,
    )
    .await?;

    match outcome {
        CommitOutcome::Recorded => {
            if let Err(error) = preview_commit.finish() {
                return Ok(SettleExecuteOutcome::AttemptFailed {
                    error: SettleAttemptError::Store(error),
                });
            }
            Ok(SettleExecuteOutcome::Recorded {
                entry_id: entry.id(),
            })
        }
        CommitOutcome::UncertainAppendFailed => {
            // Drop the guard without finishing — AbortCommit fires so the
            // preview stays available for criterion-217 / 279 lazy retry.
            drop(preview_commit);
            Ok(SettleExecuteOutcome::UncertainAppendFailed)
        }
    }
}
