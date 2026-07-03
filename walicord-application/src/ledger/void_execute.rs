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
        projection::{ExpenseOrSettlementView, NextLedgerEntryIdError},
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
        target: &ExpenseOrSettlementView,
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
        .find(|view| view.entry_id() == target_id)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        SessionNonce,
        ledger::{
            AllocationSnapshot, ExpenseNote, ExpenseRecorded, LedgerEntryId, MemberAmount,
            UnverifiedLedgerStoreEnvelope,
            canonical_attachment::CanonicalAttachmentCodec,
            canonical_write::CanonicalAppendError,
            expense_session::{VoidCandidateSelection, VoidSession, VoidSessionStage},
            ledger_chain_genesis_sha256_v1,
            load::verified_snapshot_for_test,
            make_unverified_envelope_sha256_v1,
            observability::{AppendFailureReason, LedgerObservabilityEvent},
            projection::{VerifiedEntryTransport, VerifiedLedgerThreadLoad},
            verify_envelope_sha256_v1,
            write_coordinator::{RetainedCanonicalWrite, UncertainWriteState},
        },
    };
    use parking_lot::Mutex;
    use std::{
        collections::BTreeMap,
        sync::atomic::{AtomicU32, Ordering},
        time::{Duration, SystemTime, UNIX_EPOCH},
    };
    use walicord_domain::{Money, model::MemberId};

    struct FixedClock;
    impl Clock for FixedClock {
        fn now(&self) -> SystemTime {
            UNIX_EPOCH + Duration::from_secs(1)
        }
        fn today_business_date(&self) -> crate::ledger::LedgerEffectiveDate {
            crate::ledger::LedgerEffectiveDate::new("2026-05-29").unwrap()
        }
    }

    fn ledger_id() -> LedgerId {
        walicord_ledger::test_fixtures::ledger_id(77)
    }

    fn confirming_session(target: LedgerEntryId) -> VoidSession {
        VoidSession::new(
            crate::ledger::expense_session::VoidSessionKey::new(ledger_id(), MemberId(1)),
            VoidSessionStage::Confirming {
                selection: VoidCandidateSelection::new(target),
            },
            SessionNonce::new(1).unwrap(),
            UNIX_EPOCH,
        )
    }

    fn empty_load() -> VerifiedLedgerThreadLoad<()> {
        use crate::ledger::{AppendOrderedLedgerEntries, LedgerProjector};
        let projected =
            LedgerProjector::replay(&AppendOrderedLedgerEntries::new(Vec::new()).unwrap()).unwrap();
        VerifiedLedgerThreadLoad::new(
            verified_snapshot_for_test(projected, None, 0),
            Vec::new(),
            BTreeMap::new(),
        )
        .unwrap()
    }

    fn voidable_expense_entry(id: u64) -> LedgerEntry {
        let mut entry = LedgerEntry::expense(
            LedgerEntryId(id),
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
        .unwrap();
        // Projection wants populated metadata for recordable entries.
        entry.metadata.recorded_at = Some(UNIX_EPOCH);
        entry.metadata.effective_date =
            Some(crate::ledger::LedgerEffectiveDate::new("2026-05-29").unwrap());
        entry.metadata.recorded_by = Some(MemberId(1));
        entry
    }

    fn load_with_voidable(id: u64) -> VerifiedLedgerThreadLoad<()> {
        let lid = ledger_id();
        let entry = voidable_expense_entry(id);
        let unverified =
            make_unverified_envelope_sha256_v1(lid, ledger_chain_genesis_sha256_v1(lid), (), entry)
                .unwrap();
        let verified = verify_envelope_sha256_v1(unverified, lid).unwrap();
        let snapshot =
            crate::ledger::replay_verified_snapshot(std::slice::from_ref(&verified)).unwrap();
        let mut transport = BTreeMap::new();
        transport.insert(
            LedgerEntryId(id),
            VerifiedEntryTransport::new(format!("https://example.com/{id}"), UNIX_EPOCH),
        );
        VerifiedLedgerThreadLoad::new(snapshot, vec![verified], transport).unwrap()
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
    impl VoidEntryRenderer for StubRenderer {
        fn render_public_body(
            &self,
            _: &LedgerEntry,
            _: &crate::ledger::projection::ExpenseOrSettlementView,
            _: LedgerId,
        ) -> Result<String, VoidRenderError> {
            Ok("VOID-BODY".to_owned())
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

    fn command(session: &VoidSession) -> VoidExecuteCommand<'_> {
        let lid = ledger_id();
        VoidExecuteCommand {
            session,
            session_key: session.key(),
            ledger_id: lid,
            actor_id: MemberId(1),
            write_target: WriteTargetKey::Published(lid),
            source_descriptor: DiscordLedgerSourceDescriptor::void_parent_v1(),
        }
    }

    #[tokio::test]
    async fn returns_target_gone_when_chosen_entry_is_not_in_voidable_window() {
        let session = confirming_session(LedgerEntryId(999));
        let reader = StubReader::with(empty_load());
        let sessions = VoidSessionStore::new();
        sessions.replace(session.clone());

        let outcome = void_execute_v1(
            &StubAppender::ok(),
            &StubPublisher::default(),
            &reader,
            &StubRenderer,
            &sessions,
            &UncertainWriteRegistry::new(),
            &WriteCoordinator::new(),
            &StubObservability::default(),
            &FixedClock,
            command(&session),
        )
        .await
        .unwrap();

        assert_eq!(outcome, VoidExecuteOutcome::TargetGone);
        // Session cleared on TargetGone.
        assert!(!sessions.has_active_session(session.key(), FixedClock.now()));
    }

    #[tokio::test]
    async fn returns_uncertain_blocked_when_abandoned_retain_is_present() {
        let session = confirming_session(LedgerEntryId(1));
        let reader = StubReader::with(load_with_voidable(1));
        let sessions = VoidSessionStore::new();
        sessions.replace(session.clone());
        let registry = UncertainWriteRegistry::new();
        let target = WriteTargetKey::Published(ledger_id());
        let envelope = make_unverified_envelope_sha256_v1(
            ledger_id(),
            ledger_chain_genesis_sha256_v1(ledger_id()),
            (),
            voidable_expense_entry(50),
        )
        .unwrap();
        let envelope_bytes =
            CanonicalAttachmentCodec::encode_with_pre_self_link_content(&envelope, Some("X"))
                .unwrap();
        registry
            .set_live(RetainedCanonicalWrite::new(
                target,
                &envelope,
                envelope_bytes,
                "X".to_owned(),
                "summary".to_owned(),
                UNIX_EPOCH,
            ))
            .unwrap();
        registry.abandon(ledger_id());

        let outcome = void_execute_v1(
            &StubAppender::ok(),
            &StubPublisher::default(),
            &reader,
            &StubRenderer,
            &sessions,
            &registry,
            &WriteCoordinator::new(),
            &StubObservability::default(),
            &FixedClock,
            command(&session),
        )
        .await
        .unwrap();

        assert_eq!(outcome, VoidExecuteOutcome::UncertainBlocked);
        assert!(matches!(
            registry.current(target),
            Some(UncertainWriteState::Abandoned(_))
        ));
    }

    #[tokio::test]
    async fn returns_recorded_when_target_is_voidable_and_appender_succeeds() {
        let session = confirming_session(LedgerEntryId(1));
        let reader = StubReader::with(load_with_voidable(1));
        let sessions = VoidSessionStore::new();
        sessions.replace(session.clone());
        let appender = StubAppender::ok();
        let publisher = StubPublisher::default();

        let outcome = void_execute_v1(
            &appender,
            &publisher,
            &reader,
            &StubRenderer,
            &sessions,
            &UncertainWriteRegistry::new(),
            &WriteCoordinator::new(),
            &StubObservability::default(),
            &FixedClock,
            command(&session),
        )
        .await
        .unwrap();

        assert!(matches!(outcome, VoidExecuteOutcome::Recorded { .. }));
        assert_eq!(appender.calls.load(Ordering::SeqCst), 1);
        assert_eq!(publisher.calls.load(Ordering::SeqCst), 1);
        // Session cleared on Recorded.
        assert!(!sessions.has_active_session(session.key(), FixedClock.now()));
    }

    #[tokio::test]
    async fn returns_uncertain_append_failed_when_appender_errors_and_session_is_retained() {
        let session = confirming_session(LedgerEntryId(1));
        let reader = StubReader::with(load_with_voidable(1));
        let sessions = VoidSessionStore::new();
        sessions.replace(session.clone());
        let appender = StubAppender::err(AppendFailureReason::Transport);
        let observability = StubObservability::default();

        let outcome = void_execute_v1(
            &appender,
            &StubPublisher::default(),
            &reader,
            &StubRenderer,
            &sessions,
            &UncertainWriteRegistry::new(),
            &WriteCoordinator::new(),
            &observability,
            &FixedClock,
            command(&session),
        )
        .await
        .unwrap();

        assert_eq!(outcome, VoidExecuteOutcome::UncertainAppendFailed);
        // Session is NOT cleared on append failure — retry path needs it.
        assert!(sessions.has_active_session(session.key(), FixedClock.now()));
        assert_eq!(observability.events.lock().len(), 1);
    }
}
