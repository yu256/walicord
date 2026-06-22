//! Application-layer ports + types for canonical ledger writes.
//!
//! Defines the I/O boundary that [`commit_authoritative_v1`] sits behind.
//! Adapter implementations construct per-request port instances so the
//! application use case never sees `serenity::Context`, `ChannelId`, or any
//! other transport-native handle.

use std::future::Future;

use crate::{
    Clock,
    ledger::{
        LedgerEntry, LedgerId, UnverifiedLedgerStoreEnvelope,
        canonical_attachment::{AttachmentCodecError, CanonicalAttachmentCodec},
        observability::{AppendFailureReason, LedgerObservability, LedgerObservabilityEvent},
        write_coordinator::{
            RetainedCanonicalWrite, SetLiveError, UncertainWriteRegistry, WriteTargetKey,
        },
    },
};

/// Application-pure error returned by [`CanonicalThreadAppender::append_authoritative`].
///
/// Adapters classify their transport-native errors (e.g. the serenity-wrapping
/// `StoreWriteError`) into the closed [`AppendFailureReason`] taxonomy before
/// constructing this so the application layer never references `serenity` types.
/// The adapter MAY log the typed underlying error before returning; the
/// boundary surfaces only the taxonomy plus an optional erased source so error
/// chains stay observable in tests without piercing the layer.
#[derive(Debug, thiserror::Error)]
#[error("canonical append failed: {reason}")]
pub struct CanonicalAppendError {
    reason: AppendFailureReason,
    #[source]
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl CanonicalAppendError {
    pub fn new(reason: AppendFailureReason) -> Self {
        Self {
            reason,
            source: None,
        }
    }

    pub fn with_source(
        reason: AppendFailureReason,
        source: impl std::error::Error + Send + Sync + 'static,
    ) -> Self {
        Self {
            reason,
            source: Some(Box::new(source)),
        }
    }

    pub fn reason(&self) -> AppendFailureReason {
        self.reason
    }
}

/// Per-request port that posts the prepared canonical message to the bound
/// canonical thread. Adapters construct one instance per interaction so the
/// transport context (`serenity::Context`, target `ChannelId`, etc.) is bound
/// at the boundary and never leaks into application code.
///
/// Implementations MUST treat `body` as already validated against the canonical
/// message contract (recovery reference, truncation cues, Discord budget). The
/// application use case constructs the body via the presentation layer and
/// passes the validated string through; the port performs no re-validation.
pub trait CanonicalThreadAppender: Send + Sync {
    fn append_authoritative(
        &self,
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
        body: &str,
    ) -> impl Future<Output = Result<(), CanonicalAppendError>> + Send;
}

/// Per-request port that publishes a successful canonical write back to the
/// thread locator cache. Adapters bind the canonical thread identifier at
/// construction so the application use case calls a single zero-argument
/// method. Called only on the success path of a verified `append_authoritative`.
pub trait LocatorBindingPublisher: Send + Sync {
    fn publish_ready(&self);
}

/// Closed outcome of [`commit_authoritative_v1`] so callers branch on the two
/// terminal states without inspecting the underlying error. `Recorded` means
/// the canonical post landed and the retain has been cleared; `UncertainAppendFailed`
/// means the retain is still `Live` for criterion-217 / 279 lazy retry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommitOutcome {
    Recorded,
    UncertainAppendFailed,
}

/// Pre-port failures that prevent [`commit_authoritative_v1`] from ever calling
/// [`CanonicalThreadAppender::append_authoritative`]. The append error path is
/// not represented here — append failures fold into [`CommitOutcome::UncertainAppendFailed`]
/// (and the retain stays Live for criterion-217 / 279 lazy retry), not into
/// this error type.
#[derive(Debug, thiserror::Error)]
pub enum CommitOrchestrationError {
    #[error("envelope encoding failed: {0}")]
    Encode(#[from] AttachmentCodecError),
    #[error("uncertain write already live for ledger {ledger_id:?}")]
    UncertainWriteAlreadyLive {
        ledger_id: LedgerId,
        #[source]
        source: SetLiveError,
    },
}

/// Encode → `set_live` → `append_authoritative` → on success: clear retain +
/// publish locator binding; on append failure: emit observability + leave the
/// retain Live so criterion-217 / 279 lazy retry can later decide whether the
/// post landed.
///
/// `appender` and `locator` are per-request ports constructed by the adapter so
/// the transport context (serenity `Context`, target `ChannelId`) never crosses
/// the application boundary. `body` is the validated canonical message body
/// (presentation rendered it from a typed model upstream); this function does
/// no re-validation.
#[allow(clippy::too_many_arguments)]
pub async fn commit_authoritative_v1<A, L>(
    appender: &A,
    locator: &L,
    uncertain_writes: &UncertainWriteRegistry,
    observability: &dyn LedgerObservability,
    clock: &dyn Clock,
    write_target: WriteTargetKey,
    ledger_id: LedgerId,
    entry: &LedgerEntry,
    envelope: &UnverifiedLedgerStoreEnvelope<()>,
    body: &str,
) -> Result<CommitOutcome, CommitOrchestrationError>
where
    A: CanonicalThreadAppender,
    L: LocatorBindingPublisher,
{
    let envelope_bytes =
        CanonicalAttachmentCodec::encode_with_pre_self_link_content(envelope, Some(body))?;

    let retain_live_since = clock.now();
    let retained = RetainedCanonicalWrite::new(
        write_target,
        envelope,
        envelope_bytes,
        body.to_owned(),
        short_summary_for_entry(entry),
        retain_live_since,
    );
    uncertain_writes.set_live(retained).map_err(|source| {
        CommitOrchestrationError::UncertainWriteAlreadyLive { ledger_id, source }
    })?;

    match appender.append_authoritative(envelope, body).await {
        Ok(()) => {
            locator.publish_ready();
            uncertain_writes.clear(write_target);
            Ok(CommitOutcome::Recorded)
        }
        Err(append_error) => {
            observability.emit(LedgerObservabilityEvent::CanonicalAppendFailed {
                ledger_id,
                reason: append_error.reason(),
                retained_live_since: retain_live_since,
            });
            // The adapter has already logged the typed source via tracing
            // before returning the application-pure CanonicalAppendError; we
            // only surface the application-visible outcome here.
            drop(append_error);
            Ok(CommitOutcome::UncertainAppendFailed)
        }
    }
}

/// Short canonical summary retained alongside an in-flight write (criterion 217
/// / 279). The lazy retry scan uses this string as a debug breadcrumb; the value
/// must stay stable across the post / read-back / scan cycle so the retain
/// comparison still matches.
fn short_summary_for_entry(entry: &LedgerEntry) -> String {
    format!("entry:{}", entry.id.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{
        AllocationSnapshot, ExpenseNote, ExpenseRecorded, LedgerEntry, LedgerEntryId, LedgerId,
        MemberAmount, ledger_chain_genesis_sha256_v1, make_unverified_envelope_sha256_v1,
        write_coordinator::UncertainWriteState,
    };
    use parking_lot::Mutex;
    use std::time::{Duration, SystemTime, UNIX_EPOCH};
    use walicord_domain::{Money, model::MemberId};

    #[test]
    fn canonical_append_error_carries_reason() {
        let error = CanonicalAppendError::new(AppendFailureReason::Transport);
        assert_eq!(error.reason(), AppendFailureReason::Transport);
        assert!(std::error::Error::source(&error).is_none());
    }

    #[test]
    fn canonical_append_error_with_source_preserves_chain() {
        let source = std::io::Error::other("simulated transport failure");
        let error = CanonicalAppendError::with_source(AppendFailureReason::Transport, source);
        assert_eq!(error.reason(), AppendFailureReason::Transport);
        assert!(std::error::Error::source(&error).is_some());
    }

    #[test]
    fn canonical_append_error_display_includes_reason() {
        let error = CanonicalAppendError::new(AppendFailureReason::ReadBack);
        assert_eq!(error.to_string(), "canonical append failed: read_back");
    }

    struct FixedClock;
    impl crate::Clock for FixedClock {
        fn now(&self) -> SystemTime {
            UNIX_EPOCH + Duration::from_secs(42)
        }
        fn today_business_date(&self) -> crate::ledger::LedgerEffectiveDate {
            crate::ledger::LedgerEffectiveDate::new("2026-06-04").unwrap()
        }
    }

    struct RecordingAppender {
        result: Mutex<Option<Result<(), CanonicalAppendError>>>,
        calls: Mutex<u32>,
    }
    impl RecordingAppender {
        fn ok() -> Self {
            Self {
                result: Mutex::new(Some(Ok(()))),
                calls: Mutex::new(0),
            }
        }
        fn err(reason: AppendFailureReason) -> Self {
            Self {
                result: Mutex::new(Some(Err(CanonicalAppendError::new(reason)))),
                calls: Mutex::new(0),
            }
        }
    }
    impl CanonicalThreadAppender for RecordingAppender {
        async fn append_authoritative(
            &self,
            _envelope: &UnverifiedLedgerStoreEnvelope<()>,
            _body: &str,
        ) -> Result<(), CanonicalAppendError> {
            *self.calls.lock() += 1;
            self.result
                .lock()
                .take()
                .expect("appender configured to be called exactly once")
        }
    }

    #[derive(Default)]
    struct RecordingPublisher {
        calls: Mutex<u32>,
    }
    impl LocatorBindingPublisher for RecordingPublisher {
        fn publish_ready(&self) {
            *self.calls.lock() += 1;
        }
    }

    #[derive(Default)]
    struct RecordingObservability {
        events: Mutex<Vec<LedgerObservabilityEvent>>,
    }
    impl LedgerObservability for RecordingObservability {
        fn emit(&self, event: LedgerObservabilityEvent) {
            self.events.lock().push(event);
        }
    }

    fn sample_entry() -> LedgerEntry {
        LedgerEntry::expense(
            LedgerEntryId(7),
            ExpenseRecorded::new(
                vec![MemberAmount {
                    member_id: MemberId(1),
                    amount: Money::from_i64(1_000),
                }],
                vec![MemberAmount {
                    member_id: MemberId(2),
                    amount: Money::from_i64(1_000),
                }],
                Some(ExpenseNote::new("test").unwrap()),
            )
            .unwrap(),
            AllocationSnapshot::Even,
        )
        .unwrap()
    }

    fn sample_envelope(
        ledger_id: LedgerId,
        entry: LedgerEntry,
    ) -> UnverifiedLedgerStoreEnvelope<()> {
        make_unverified_envelope_sha256_v1(
            ledger_id,
            ledger_chain_genesis_sha256_v1(ledger_id),
            (),
            entry,
        )
        .expect("sample envelope should build")
    }

    #[tokio::test]
    async fn commit_authoritative_v1_publishes_binding_and_clears_retain_on_success() {
        let ledger_id = walicord_ledger::test_fixtures::ledger_id(77);
        let target = WriteTargetKey::Published(ledger_id);
        let entry = sample_entry();
        let envelope = sample_envelope(ledger_id, entry.clone());
        let body = "BODY";
        let appender = RecordingAppender::ok();
        let publisher = RecordingPublisher::default();
        let registry = UncertainWriteRegistry::new();
        let observability = RecordingObservability::default();

        let outcome = commit_authoritative_v1(
            &appender,
            &publisher,
            &registry,
            &observability,
            &FixedClock,
            target,
            ledger_id,
            &entry,
            &envelope,
            body,
        )
        .await
        .expect("orchestration should succeed");

        assert_eq!(outcome, CommitOutcome::Recorded);
        assert_eq!(*appender.calls.lock(), 1);
        assert_eq!(*publisher.calls.lock(), 1);
        assert!(observability.events.lock().is_empty());
        assert_eq!(registry.current(target), None);
    }

    #[tokio::test]
    async fn commit_authoritative_v1_leaves_retain_live_and_emits_on_append_failure() {
        let ledger_id = walicord_ledger::test_fixtures::ledger_id(77);
        let target = WriteTargetKey::Published(ledger_id);
        let entry = sample_entry();
        let envelope = sample_envelope(ledger_id, entry.clone());
        let body = "BODY";
        let appender = RecordingAppender::err(AppendFailureReason::Transport);
        let publisher = RecordingPublisher::default();
        let registry = UncertainWriteRegistry::new();
        let observability = RecordingObservability::default();

        let outcome = commit_authoritative_v1(
            &appender,
            &publisher,
            &registry,
            &observability,
            &FixedClock,
            target,
            ledger_id,
            &entry,
            &envelope,
            body,
        )
        .await
        .expect("orchestration should still return Ok on append failure");

        assert_eq!(outcome, CommitOutcome::UncertainAppendFailed);
        assert_eq!(*appender.calls.lock(), 1);
        assert_eq!(*publisher.calls.lock(), 0);
        assert!(matches!(
            registry.current(target),
            Some(UncertainWriteState::Live(_))
        ));
        let events = observability.events.lock();
        assert_eq!(events.len(), 1);
        assert!(matches!(
            &events[0],
            LedgerObservabilityEvent::CanonicalAppendFailed {
                reason: AppendFailureReason::Transport,
                ..
            }
        ));
    }

    #[tokio::test]
    async fn commit_authoritative_v1_rejects_concurrent_set_live() {
        let ledger_id = walicord_ledger::test_fixtures::ledger_id(77);
        let target = WriteTargetKey::Published(ledger_id);
        let entry = sample_entry();
        let envelope = sample_envelope(ledger_id, entry.clone());
        let registry = UncertainWriteRegistry::new();
        // Seed a conflicting Live retain so set_live inside the orchestration rejects.
        let conflict = RetainedCanonicalWrite::new(
            target,
            &envelope,
            b"different".to_vec(),
            "other".to_owned(),
            "other".to_owned(),
            UNIX_EPOCH,
        );
        registry.set_live(conflict).expect("seed set_live");

        let appender = RecordingAppender::ok();
        let publisher = RecordingPublisher::default();
        let observability = RecordingObservability::default();
        let result = commit_authoritative_v1(
            &appender,
            &publisher,
            &registry,
            &observability,
            &FixedClock,
            target,
            ledger_id,
            &entry,
            &envelope,
            "BODY",
        )
        .await;

        assert!(matches!(
            result,
            Err(CommitOrchestrationError::UncertainWriteAlreadyLive { .. })
        ));
        assert_eq!(*appender.calls.lock(), 0);
        assert_eq!(*publisher.calls.lock(), 0);
        assert!(observability.events.lock().is_empty());
    }
}
