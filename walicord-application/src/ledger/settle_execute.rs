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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        SettleUpPolicy,
        ledger::{
            EntryHash, LedgerEntry, LedgerEntryId, UnverifiedLedgerStoreEnvelope,
            canonical_attachment::CanonicalAttachmentCodec,
            canonical_write::CanonicalAppendError,
            ledger_chain_genesis_sha256_v1,
            load::verified_snapshot_for_test,
            make_unverified_envelope_sha256_v1,
            observability::{AppendFailureReason, LedgerObservabilityEvent},
            preview_store::{PreviewStoreRecord, PreviewStoreTransition},
            projection::VerifiedLedgerThreadLoad,
            settle_flow::mark_preview_delivered,
            write_coordinator::{RetainedCanonicalWrite, UncertainWriteState},
        },
        settle_up::{PreviewConfirmationBinding, PreviewInstanceId, PreviewedSettlement},
    };
    use parking_lot::Mutex;
    use std::{
        collections::BTreeMap,
        sync::atomic::{AtomicU32, Ordering},
        time::{Duration, SystemTime, UNIX_EPOCH},
    };
    use walicord_domain::{
        MemberBalances, Money, Settlement, SettlementContext, SettlementRoundingError, Transfer,
        model::MemberId,
    };

    struct FixedClock;
    impl Clock for FixedClock {
        fn now(&self) -> SystemTime {
            UNIX_EPOCH + Duration::from_secs(1)
        }
        fn today_business_date(&self) -> crate::ledger::LedgerEffectiveDate {
            crate::ledger::LedgerEffectiveDate::new("2026-05-29").unwrap()
        }
    }

    struct TwoMemberPlanner;
    impl crate::SettlementPlanner for TwoMemberPlanner {
        fn plan(
            &self,
            balances: MemberBalances,
            _: &[MemberId],
            _: &[MemberId],
            _: SettlementContext,
        ) -> Result<Settlement, SettlementRoundingError> {
            let mut iter = balances.iter();
            let (&first, &first_balance) = iter.next().expect("two-member input");
            let (&second, &second_balance) = iter.next().expect("two-member input");
            let (from, to, amount) = if first_balance < Money::ZERO {
                (first, second, -first_balance)
            } else {
                (second, first, -second_balance)
            };
            let mut new_balances = MemberBalances::default();
            new_balances.insert(from, Money::ZERO);
            new_balances.insert(to, Money::ZERO);
            Ok(Settlement {
                new_balances,
                transfers: vec![Transfer { from, to, amount }],
            })
        }
    }

    fn ledger_id() -> LedgerId {
        walicord_ledger::test_fixtures::ledger_id(77)
    }

    fn previewed(amount: i64) -> PreviewedSettlement {
        let mut balances = MemberBalances::default();
        balances.insert(MemberId(1), Money::from_i64(-amount));
        balances.insert(MemberId(2), Money::from_i64(amount));
        SettleUpPolicy::preview(
            &TwoMemberPlanner,
            &balances,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            SettlementContext::jpy_default(),
        )
        .unwrap()
    }

    fn seed_delivered_preview(
        store: &PreviewStore,
        head: EntryHash,
    ) -> (PreviewStoreKey, PreviewInstanceId) {
        let actor = MemberId(1);
        let preview = previewed(1000);
        let preview_instance_id = PreviewInstanceId::new(1).unwrap();
        let binding = PreviewConfirmationBinding::capture(
            preview_instance_id,
            ledger_id(),
            head,
            actor,
            UNIX_EPOCH,
            UNIX_EPOCH + Duration::from_secs(600),
            &preview,
        )
        .unwrap();
        let key = PreviewStoreKey::new(ledger_id(), actor);
        store
            .transition(
                key,
                PreviewStoreTransition::Replace(Box::new(PreviewStoreRecord::new(
                    preview, binding,
                ))),
            )
            .unwrap();
        mark_preview_delivered(store, key, preview_instance_id).unwrap();
        (key, preview_instance_id)
    }

    fn empty_load_with_head(head: Option<EntryHash>) -> VerifiedLedgerThreadLoad<()> {
        VerifiedLedgerThreadLoad::new(
            verified_snapshot_for_test(empty_projected(), head, 0),
            Vec::new(),
            BTreeMap::new(),
        )
        .unwrap()
    }

    fn empty_projected() -> walicord_ledger::ProjectedLedger {
        use crate::ledger::{AppendOrderedLedgerEntries, LedgerProjector};
        LedgerProjector::replay(&AppendOrderedLedgerEntries::new(Vec::new()).unwrap()).unwrap()
    }

    struct StubReader {
        load: Mutex<Option<VerifiedLedgerThreadLoad<()>>>,
    }
    impl StubReader {
        fn with(load: VerifiedLedgerThreadLoad<()>) -> Self {
            Self {
                load: Mutex::new(Some(load)),
            }
        }
    }
    impl CanonicalThreadReader for StubReader {
        async fn load_verified_thread(
            &self,
        ) -> Result<VerifiedLedgerThreadLoad<()>, CanonicalReadError> {
            Ok(self.load.lock().clone().unwrap())
        }
        async fn scan_recent(
            &self,
        ) -> Result<Vec<crate::ledger::write_coordinator::CanonicalMessageProbe>, CanonicalReadError>
        {
            Ok(Vec::new())
        }
        async fn scan_all(
            &self,
        ) -> Result<Vec<crate::ledger::write_coordinator::CanonicalMessageProbe>, CanonicalReadError>
        {
            Ok(Vec::new())
        }
    }

    struct StubAppender {
        result: Mutex<Option<Result<(), CanonicalAppendError>>>,
        calls: AtomicU32,
    }
    impl StubAppender {
        fn ok() -> Self {
            Self {
                result: Mutex::new(Some(Ok(()))),
                calls: AtomicU32::new(0),
            }
        }
        fn err(reason: AppendFailureReason) -> Self {
            Self {
                result: Mutex::new(Some(Err(CanonicalAppendError::new(reason)))),
                calls: AtomicU32::new(0),
            }
        }
    }
    impl CanonicalThreadAppender for StubAppender {
        async fn append_authoritative(
            &self,
            _: &UnverifiedLedgerStoreEnvelope<()>,
            _: &str,
        ) -> Result<(), CanonicalAppendError> {
            self.calls.fetch_add(1, Ordering::SeqCst);
            self.result.lock().take().unwrap()
        }
    }

    #[derive(Default)]
    struct StubPublisher {
        calls: AtomicU32,
    }
    impl LocatorBindingPublisher for StubPublisher {
        fn publish_ready(&self) {
            self.calls.fetch_add(1, Ordering::SeqCst);
        }
    }

    struct StubRenderer;
    impl SettlementEntryRenderer for StubRenderer {
        fn render_public_body(
            &self,
            _: &RecordableSettlementEntry,
            _: LedgerId,
        ) -> Result<String, SettlementRenderError> {
            Ok("SETTLE-BODY".to_owned())
        }
    }

    #[derive(Default)]
    struct StubObservability {
        events: Mutex<Vec<LedgerObservabilityEvent>>,
    }
    impl crate::ledger::observability::LedgerObservability for StubObservability {
        fn emit(&self, event: LedgerObservabilityEvent) {
            self.events.lock().push(event);
        }
    }

    fn command() -> SettleExecuteCommand {
        let lid = ledger_id();
        SettleExecuteCommand {
            ledger_id: lid,
            actor_id: MemberId(1),
            write_target: WriteTargetKey::Published(lid),
            source_descriptor: DiscordLedgerSourceDescriptor::settle_thread_v1(),
        }
    }

    #[tokio::test]
    async fn returns_no_preview_required_when_store_has_no_preview_for_actor() {
        let reader = StubReader::with(empty_load_with_head(None));
        let outcome = settle_execute_v1(
            &StubAppender::ok(),
            &StubPublisher::default(),
            &reader,
            &StubRenderer,
            &PreviewStore::new(),
            &UncertainWriteRegistry::new(),
            &WriteCoordinator::new(),
            &StubObservability::default(),
            &FixedClock,
            command(),
        )
        .await
        .unwrap();
        assert!(matches!(outcome, SettleExecuteOutcome::NoPreviewRequired));
    }

    #[tokio::test]
    async fn returns_recorded_when_preview_delivered_and_appender_succeeds() {
        let genesis = ledger_chain_genesis_sha256_v1(ledger_id());
        // Snapshot reports head == genesis so the preview binding's stored head
        // matches at compose time (criterion 18 StaleHead check passes).
        let reader = StubReader::with(empty_load_with_head(Some(genesis)));
        let preview_store = PreviewStore::new();
        seed_delivered_preview(&preview_store, genesis);
        let appender = StubAppender::ok();
        let publisher = StubPublisher::default();

        let outcome = settle_execute_v1(
            &appender,
            &publisher,
            &reader,
            &StubRenderer,
            &preview_store,
            &UncertainWriteRegistry::new(),
            &WriteCoordinator::new(),
            &StubObservability::default(),
            &FixedClock,
            command(),
        )
        .await
        .unwrap();

        assert!(matches!(outcome, SettleExecuteOutcome::Recorded { .. }));
        assert_eq!(appender.calls.load(Ordering::SeqCst), 1);
        assert_eq!(publisher.calls.load(Ordering::SeqCst), 1);
    }

    #[tokio::test]
    async fn returns_uncertain_append_failed_when_appender_errors_and_preview_remains_for_retry() {
        let genesis = ledger_chain_genesis_sha256_v1(ledger_id());
        let reader = StubReader::with(empty_load_with_head(Some(genesis)));
        let preview_store = PreviewStore::new();
        let (key, _) = seed_delivered_preview(&preview_store, genesis);
        let appender = StubAppender::err(AppendFailureReason::Transport);
        let observability = StubObservability::default();

        let outcome = settle_execute_v1(
            &appender,
            &StubPublisher::default(),
            &reader,
            &StubRenderer,
            &preview_store,
            &UncertainWriteRegistry::new(),
            &WriteCoordinator::new(),
            &observability,
            &FixedClock,
            command(),
        )
        .await
        .unwrap();

        assert!(matches!(
            outcome,
            SettleExecuteOutcome::UncertainAppendFailed
        ));
        // Preview is restored to Ready by guard's Drop so retry is possible.
        assert!(preview_store.current(key).is_some());
        assert_eq!(observability.events.lock().len(), 1);
    }

    #[tokio::test]
    async fn returns_attempt_failed_stale_head_when_preview_head_differs_from_snapshot() {
        // Preview binding records `genesis` as its head, but the reader returns a
        // snapshot whose current_head_hash is a different value — compose_from_preview
        // detects StaleHead.
        let preview_head = ledger_chain_genesis_sha256_v1(ledger_id());
        let observed_head = EntryHash([0xAA; 32]);
        let reader = StubReader::with(empty_load_with_head(Some(observed_head)));
        let preview_store = PreviewStore::new();
        seed_delivered_preview(&preview_store, preview_head);

        let outcome = settle_execute_v1(
            &StubAppender::ok(),
            &StubPublisher::default(),
            &reader,
            &StubRenderer,
            &preview_store,
            &UncertainWriteRegistry::new(),
            &WriteCoordinator::new(),
            &StubObservability::default(),
            &FixedClock,
            command(),
        )
        .await
        .unwrap();

        assert!(matches!(
            outcome,
            SettleExecuteOutcome::AttemptFailed {
                error: SettleAttemptError::StaleHead { .. }
            }
        ));
    }

    #[tokio::test]
    async fn returns_uncertain_blocked_when_abandoned_retain_blocks_the_write_target() {
        let genesis = ledger_chain_genesis_sha256_v1(ledger_id());
        let reader = StubReader::with(empty_load_with_head(None));
        let preview_store = PreviewStore::new();
        seed_delivered_preview(&preview_store, genesis);
        let registry = UncertainWriteRegistry::new();
        let target = WriteTargetKey::Published(ledger_id());
        let envelope =
            make_unverified_envelope_sha256_v1(ledger_id(), genesis, (), seed_entry()).unwrap();
        let envelope_bytes =
            CanonicalAttachmentCodec::encode_with_pre_self_link_content(&envelope, Some("X"))
                .unwrap();
        let retained = RetainedCanonicalWrite::new(
            target,
            &envelope,
            envelope_bytes,
            "X".to_owned(),
            "summary".to_owned(),
            UNIX_EPOCH,
        );
        registry.set_live(retained).unwrap();
        registry.abandon(ledger_id());

        let outcome = settle_execute_v1(
            &StubAppender::ok(),
            &StubPublisher::default(),
            &reader,
            &StubRenderer,
            &preview_store,
            &registry,
            &WriteCoordinator::new(),
            &StubObservability::default(),
            &FixedClock,
            command(),
        )
        .await
        .unwrap();

        assert!(matches!(outcome, SettleExecuteOutcome::UncertainBlocked));
        assert!(matches!(
            registry.current(target),
            Some(UncertainWriteState::Abandoned(_))
        ));
    }

    fn seed_entry() -> LedgerEntry {
        use crate::ledger::{AllocationSnapshot, ExpenseNote, ExpenseRecorded, MemberAmount};
        LedgerEntry::expense(
            LedgerEntryId(99),
            ExpenseRecorded::new(
                vec![MemberAmount {
                    member_id: MemberId(1),
                    amount: Money::from_i64(1_000),
                }],
                vec![MemberAmount {
                    member_id: MemberId(2),
                    amount: Money::from_i64(1_000),
                }],
                Some(ExpenseNote::new("seed").unwrap()),
            )
            .unwrap(),
            AllocationSnapshot::Even,
        )
        .unwrap()
    }
}
