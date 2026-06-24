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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        InteractionNonce, NonceProvider,
        ledger::{
            LedgerEffectiveDate, LedgerEntry, LedgerEntryId, UnverifiedLedgerStoreEnvelope,
            canonical_attachment::CanonicalAttachmentCodec,
            canonical_write::CanonicalAppendError,
            expense_flow::{bootstrap_expense_session, build_confirmation_for_session},
            expense_modal::{RawExpenseModalSubmission, validate_expense_modal_submission},
            expense_session::{
                ExpenseDraftScopeId, ExpenseLaunchOrigin, ExpenseSelectionPhase,
                ExpenseSelectionState, ExpenseSessionKey,
            },
            ledger_chain_genesis_sha256_v1,
            load::verified_snapshot_for_test,
            make_unverified_envelope_sha256_v1,
            observability::{AppendFailureReason, LedgerObservabilityEvent},
            projection::VerifiedLedgerThreadLoad,
            write_coordinator::UncertainWriteState,
        },
        settle_up::PreviewInstanceId,
    };
    use parking_lot::Mutex;
    use std::{
        collections::BTreeMap,
        sync::atomic::{AtomicU32, AtomicU64, Ordering},
        time::{Duration, SystemTime, UNIX_EPOCH},
    };

    struct FixedClock;
    impl Clock for FixedClock {
        fn now(&self) -> SystemTime {
            UNIX_EPOCH + Duration::from_secs(1)
        }
        fn today_business_date(&self) -> LedgerEffectiveDate {
            LedgerEffectiveDate::new("2026-05-29").unwrap()
        }
    }

    struct SeqNonces {
        nonce: AtomicU64,
        preview: AtomicU64,
    }
    impl NonceProvider for SeqNonces {
        fn next_interaction_nonce(&self) -> InteractionNonce {
            InteractionNonce::new(self.nonce.fetch_add(1, Ordering::SeqCst)).unwrap()
        }
        fn next_preview_instance_id(&self) -> PreviewInstanceId {
            PreviewInstanceId::new(self.preview.fetch_add(1, Ordering::SeqCst)).unwrap()
        }
    }
    fn nonces() -> SeqNonces {
        SeqNonces {
            nonce: AtomicU64::new(1),
            preview: AtomicU64::new(1),
        }
    }

    fn session_key(actor: u64) -> ExpenseSessionKey {
        ExpenseSessionKey::new(
            ExpenseDraftScopeId::new(42).expect("non-zero draft scope"),
            walicord_domain::model::MemberId(actor),
        )
    }

    fn roster_with(members: &[u64]) -> RosterSnapshot {
        RosterSnapshot {
            all_members: members.iter().map(|id| MemberId(*id)).collect(),
            role_members: std::collections::BTreeMap::new(),
        }
    }

    fn confirmation_session() -> ExpenseSession {
        let raw = RawExpenseModalSubmission {
            raw_amount: "1000".to_owned(),
            raw_note: "ランチ".to_owned(),
            raw_date: "2026-05-01".to_owned(),
        };
        let validated = validate_expense_modal_submission(&raw, &FixedClock).unwrap();
        let (session, _) = bootstrap_expense_session(
            session_key(42),
            ExpenseLaunchOrigin::SlashCommand,
            validated,
            &FixedClock,
            &nonces(),
        )
        .unwrap();
        let mut draft = session.draft().clone();
        draft = draft.with_selection_state(ExpenseSelectionState {
            payer: Some(MemberId(42)),
            individual_members: vec![MemberId(42), MemberId(7)],
            ..Default::default()
        });
        let session = ExpenseSession::new(
            session.key(),
            session.origin(),
            crate::ledger::expense_session::ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::participants(
                    crate::ledger::expense_session::ParticipantSelectionMode::Individual,
                ),
            },
            draft,
            FixedClock.now(),
        )
        .unwrap();
        build_confirmation_for_session(session, &roster_with(&[42, 7]), &FixedClock)
            .unwrap()
            .session
    }

    fn empty_load() -> VerifiedLedgerThreadLoad<()> {
        VerifiedLedgerThreadLoad::new(
            verified_snapshot_for_test(empty_projected(), None, 0),
            Vec::new(),
            BTreeMap::new(),
        )
        .expect("empty load has no missing transports")
    }

    fn empty_projected() -> walicord_ledger::ProjectedLedger {
        use crate::ledger::AppendOrderedLedgerEntries;
        crate::ledger::LedgerProjector::replay(
            &AppendOrderedLedgerEntries::new(Vec::new()).unwrap(),
        )
        .unwrap()
    }

    struct StubReader {
        load: Mutex<Option<VerifiedLedgerThreadLoad<()>>>,
    }
    impl StubReader {
        fn ok(load: VerifiedLedgerThreadLoad<()>) -> Self {
            Self {
                load: Mutex::new(Some(load)),
            }
        }
    }
    impl CanonicalThreadReader for StubReader {
        async fn load_verified_thread(
            &self,
        ) -> Result<VerifiedLedgerThreadLoad<()>, CanonicalReadError> {
            self.load.lock().clone().ok_or_else(CanonicalReadError::new)
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
            _envelope: &UnverifiedLedgerStoreEnvelope<()>,
            _body: &str,
        ) -> Result<(), CanonicalAppendError> {
            self.calls.fetch_add(1, Ordering::SeqCst);
            self.result
                .lock()
                .take()
                .expect("appender called more than once")
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
    impl ExpenseEntryRenderer for StubRenderer {
        fn render_public_body(
            &self,
            _recordable: &RecordableExpenseEntry,
            _ledger_id: LedgerId,
        ) -> Result<String, ExpenseRenderError> {
            Ok("BODY".to_owned())
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

    fn ledger_id() -> LedgerId {
        walicord_ledger::test_fixtures::ledger_id(77)
    }

    fn command<'a>(
        session: &'a ExpenseSession,
        roster: &'a RosterSnapshot,
    ) -> RecordExpenseCommand<'a> {
        let lid = ledger_id();
        RecordExpenseCommand {
            session,
            roster,
            source_descriptor: DiscordLedgerSourceDescriptor::expense_slash_modal_v1(),
            actor_id: MemberId(42),
            ledger_id: lid,
            write_target: WriteTargetKey::Published(lid),
        }
    }

    #[tokio::test]
    async fn returns_recorded_on_successful_commit_against_matching_roster() {
        let session = confirmation_session();
        let roster = roster_with(&[42, 7]);
        let appender = StubAppender::ok();
        let publisher = StubPublisher::default();
        let reader = StubReader::ok(empty_load());
        let renderer = StubRenderer;
        let registry = UncertainWriteRegistry::new();
        let coordinator = WriteCoordinator::new();
        let observability = StubObservability::default();

        let outcome = record_expense_v1(
            &appender,
            &publisher,
            &reader,
            &renderer,
            &registry,
            &coordinator,
            &observability,
            &FixedClock,
            command(&session, &roster),
        )
        .await
        .expect("orchestration succeeds");

        match outcome {
            RecordExpenseOutcome::Recorded { entry_id, .. } => {
                assert_eq!(entry_id, LedgerEntryId(1));
            }
            other => panic!("expected Recorded, got {other:?}"),
        }
        assert_eq!(appender.calls.load(Ordering::SeqCst), 1);
        assert_eq!(publisher.calls.load(Ordering::SeqCst), 1);
        assert!(observability.events.lock().is_empty());
    }

    #[tokio::test]
    async fn returns_drift_detected_when_roster_shrunk_after_confirmation() {
        let session = confirmation_session();
        let shrunk = roster_with(&[42]);
        let appender = StubAppender::ok();
        let publisher = StubPublisher::default();
        let reader = StubReader::ok(empty_load());
        let renderer = StubRenderer;
        let registry = UncertainWriteRegistry::new();
        let coordinator = WriteCoordinator::new();
        let observability = StubObservability::default();

        let outcome = record_expense_v1(
            &appender,
            &publisher,
            &reader,
            &renderer,
            &registry,
            &coordinator,
            &observability,
            &FixedClock,
            command(&session, &shrunk),
        )
        .await
        .expect("orchestration succeeds");

        match outcome {
            RecordExpenseOutcome::DriftDetected {
                drift, refreshed, ..
            } => {
                assert!(!drift.is_empty());
                assert_eq!(refreshed.len(), 1);
            }
            other => panic!("expected DriftDetected, got {other:?}"),
        }
        assert_eq!(appender.calls.load(Ordering::SeqCst), 0);
    }

    #[tokio::test]
    async fn returns_uncertain_blocked_when_abandoned_retain_is_present() {
        let session = confirmation_session();
        let roster = roster_with(&[42, 7]);
        let appender = StubAppender::ok();
        let publisher = StubPublisher::default();
        let reader = StubReader::ok(empty_load());
        let renderer = StubRenderer;
        let registry = UncertainWriteRegistry::new();
        let coordinator = WriteCoordinator::new();
        let observability = StubObservability::default();

        // Seed an Abandoned retain so resolve_uncertain_write_v1 short-circuits to false.
        let lid = ledger_id();
        let target = WriteTargetKey::Published(lid);
        let envelope = make_unverified_envelope_sha256_v1(
            lid,
            ledger_chain_genesis_sha256_v1(lid),
            (),
            sample_existing_entry(),
        )
        .unwrap();
        let envelope_bytes =
            CanonicalAttachmentCodec::encode_with_pre_self_link_content(&envelope, Some("X"))
                .unwrap();
        let retained = crate::ledger::write_coordinator::RetainedCanonicalWrite::new(
            target,
            &envelope,
            envelope_bytes,
            "X".to_owned(),
            "summary".to_owned(),
            UNIX_EPOCH,
        );
        registry.set_live(retained).unwrap();
        registry.abandon(lid);

        let outcome = record_expense_v1(
            &appender,
            &publisher,
            &reader,
            &renderer,
            &registry,
            &coordinator,
            &observability,
            &FixedClock,
            command(&session, &roster),
        )
        .await
        .expect("orchestration returns Ok with UncertainBlocked outcome");

        assert!(matches!(outcome, RecordExpenseOutcome::UncertainBlocked));
        assert_eq!(appender.calls.load(Ordering::SeqCst), 0);
        assert!(matches!(
            registry.current(target),
            Some(UncertainWriteState::Abandoned(_))
        ));
    }

    #[tokio::test]
    async fn returns_uncertain_append_failed_when_appender_errors_and_emits_observability() {
        let session = confirmation_session();
        let roster = roster_with(&[42, 7]);
        let appender = StubAppender::err(AppendFailureReason::Transport);
        let publisher = StubPublisher::default();
        let reader = StubReader::ok(empty_load());
        let renderer = StubRenderer;
        let registry = UncertainWriteRegistry::new();
        let coordinator = WriteCoordinator::new();
        let observability = StubObservability::default();

        let outcome = record_expense_v1(
            &appender,
            &publisher,
            &reader,
            &renderer,
            &registry,
            &coordinator,
            &observability,
            &FixedClock,
            command(&session, &roster),
        )
        .await
        .expect("orchestration returns Ok with UncertainAppendFailed outcome");

        assert!(matches!(
            outcome,
            RecordExpenseOutcome::UncertainAppendFailed
        ));
        assert_eq!(appender.calls.load(Ordering::SeqCst), 1);
        assert_eq!(publisher.calls.load(Ordering::SeqCst), 0);
        let events = observability.events.lock();
        assert!(matches!(
            events.as_slice(),
            [LedgerObservabilityEvent::CanonicalAppendFailed {
                reason: AppendFailureReason::Transport,
                ..
            }]
        ));
        assert!(matches!(
            registry.current(WriteTargetKey::Published(ledger_id())),
            Some(UncertainWriteState::Live(_))
        ));
    }

    fn sample_existing_entry() -> LedgerEntry {
        use crate::ledger::{AllocationSnapshot, ExpenseNote, ExpenseRecorded, MemberAmount};
        use walicord_domain::Money;
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
