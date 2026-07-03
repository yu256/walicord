use crate::{
    Clock,
    ledger::{
        EntryHash, LedgerEntryId, LedgerId, UnverifiedLedgerStoreEnvelope,
        append_previous_hash_sha256_v1,
        canonical_read::CanonicalThreadReader,
        observability::{LedgerObservability, LedgerObservabilityEvent},
        time::non_negative_elapsed_since,
    },
};
use dashmap::DashMap;
use parking_lot::Mutex;
use std::{
    collections::HashMap,
    sync::Arc,
    time::{Duration, SystemTime},
};
use tokio::sync::Mutex as AsyncMutex;

/// Number of trailing canonical messages the normal lazy-retry pass scans for an
/// exact-envelope match before retaining the `uncertain_write` block (criterion 176).
pub const LAZY_RETRY_SCAN_WINDOW: usize = 5;
pub const UNCERTAIN_WRITE_RETAIN_TTL: Duration = Duration::from_secs(10 * 60);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BootstrapWriteTarget {
    guild_id: u64,
    tracked_parent_channel_id: u64,
}

impl BootstrapWriteTarget {
    pub fn new(guild_id: u64, tracked_parent_channel_id: u64) -> Self {
        Self {
            guild_id,
            tracked_parent_channel_id,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WriteTargetKey {
    Bootstrap(BootstrapWriteTarget),
    Published(LedgerId),
}

impl From<LedgerId> for WriteTargetKey {
    fn from(ledger_id: LedgerId) -> Self {
        Self::Published(ledger_id)
    }
}

/// Frozen exact-envelope retain state. After an ambiguous post outcome, the same bytes
/// (envelope, prepared render, recovery context) must be re-used on retry; a retry can
/// never mint a new `LedgerEntryId` or mutate the prepared payload (criteria 139, 148,
/// 176, 217, 239, 245, 279, 287; G3 / G18 / AC 18).
#[derive(Debug, Clone, PartialEq)]
pub struct RetainedCanonicalWrite {
    target: WriteTargetKey,
    entry_id: LedgerEntryId,
    previous_hash: EntryHash,
    envelope_bytes: Arc<Vec<u8>>,
    prepared_body: String,
    last_known_summary: String,
    live_since: SystemTime,
}

impl RetainedCanonicalWrite {
    pub fn new(
        target: impl Into<WriteTargetKey>,
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
        envelope_bytes: Vec<u8>,
        prepared_body: String,
        last_known_summary: String,
        live_since: SystemTime,
    ) -> Self {
        Self {
            target: target.into(),
            entry_id: envelope.payload.entry.id,
            previous_hash: envelope.previous_hash,
            envelope_bytes: Arc::new(envelope_bytes),
            prepared_body,
            last_known_summary,
            live_since,
        }
    }

    pub fn target(&self) -> WriteTargetKey {
        self.target
    }
    pub fn entry_id(&self) -> LedgerEntryId {
        self.entry_id
    }
    pub fn previous_hash(&self) -> EntryHash {
        self.previous_hash
    }
    pub fn envelope_bytes(&self) -> &[u8] {
        &self.envelope_bytes
    }
    pub fn prepared_body(&self) -> &str {
        &self.prepared_body
    }
    pub fn last_known_summary(&self) -> &str {
        &self.last_known_summary
    }
    pub fn live_since(&self) -> SystemTime {
        self.live_since
    }

    pub fn requires_full_scan(&self, now: SystemTime) -> bool {
        non_negative_elapsed_since(now, self.live_since) >= UNCERTAIN_WRITE_RETAIN_TTL
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UncertainWriteState {
    /// Retained payload still exists; lazy retry may still clear or confirm.
    Live(RetainedCanonicalWrite),
    /// The bounded retain interval elapsed without proof that the post either landed
    /// or stayed absent. A user-visible recovery acknowledgement is required before
    /// the caller may discard the frozen envelope and start a fresh write.
    Abandoned(RetainedCanonicalWrite),
}

impl UncertainWriteState {
    pub fn target(&self) -> WriteTargetKey {
        match self {
            Self::Live(retained) | Self::Abandoned(retained) => retained.target(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UncertainWriteResolution {
    /// Lazy retry scan matched the retained envelope bytes against an existing canonical
    /// message — the prior post landed. The block clears and writes resume normally.
    ClearedByExistingPost { entry_id: LedgerEntryId },
    /// Lazy retry observed conclusive head movement past the retained `previous_hash`,
    /// meaning the retained envelope was not posted. The block clears and a fresh write
    /// path is unblocked.
    ClearedByConclusiveAbsence,
    /// The bounded retain interval elapsed without conclusive head movement. The
    /// frozen envelope remains visible until an explicit recovery acknowledgement.
    Abandoned,
    /// A recent-window no-match cannot prove absence after canonical head movement.
    RequiresFullHistoryScan,
    /// Scan was inconclusive; the block stays live (criteria 217, 248, 276-279, 287).
    StillBlocked,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum CanonicalWriteError {
    #[error("canonical bootstrap timed out")]
    BootstrapTimeout,
    #[error("no retained state for this write target")]
    NoRetainedState,
}

/// Outcome of the last-window canonical-message scan used by lazy retry. Criterion 176
/// requires the retry to inspect the last five canonical messages; criterion 217 says
/// inconclusive scans must keep the block live. We therefore must distinguish a
/// **verified** no-match (full window observed cleanly) from an **inconclusive** result
/// (rate-limited, partial fetch, fewer than `LAZY_RETRY_SCAN_WINDOW` messages observed,
/// etc.). Callers can only clear `uncertain_write` from `VerifiedNoMatch` combined with
/// head-state evidence; an `Inconclusive` outcome keeps the block live.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExactEnvelopeScanResult {
    Matched { entry_id: LedgerEntryId },
    VerifiedNoMatch,
    Inconclusive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScanCompleteness {
    /// Every message required by the selected scope was successfully retrieved and
    /// decoded.
    Complete,
    /// Fewer than the required messages were retrievable, or some failed to decode.
    /// Per criteria 176/217, callers may not clear `uncertain_write` from this state.
    Incomplete,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExactEnvelopeScanScope {
    RecentWindow,
    FullHistory,
}

/// Distinguishes a transition that genuinely captures new retain state from one that
/// would silently clobber an existing retain. `set_live` rejects the latter.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum SetLiveError {
    /// The target already has a Live retain whose retained payload differs from the
    /// incoming retain. Per criteria 217 / 279 / 287, the original retained envelope
    /// must remain authoritative for retry; a different envelope cannot replace it.
    #[error(
        "target already has a different Live retain (existing: {existing_entry_id:?}, incoming: {incoming_entry_id:?})"
    )]
    AlreadyLiveWithDifferentRetain {
        existing_entry_id: LedgerEntryId,
        incoming_entry_id: LedgerEntryId,
    },
    #[error("target has an abandoned retain awaiting explicit recovery: {entry_id:?}")]
    AbandonedRetain { entry_id: LedgerEntryId },
}

/// Tracks the live `uncertain_write` blocks per write target. The registry has no
/// background sweeper (criterion 154 — idle mutex cache may remain).
pub struct UncertainWriteRegistry {
    by_target: Mutex<HashMap<WriteTargetKey, UncertainWriteState>>,
}

impl Default for UncertainWriteRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl UncertainWriteRegistry {
    pub fn new() -> Self {
        Self {
            by_target: Mutex::new(HashMap::new()),
        }
    }

    pub fn current(&self, target: impl Into<WriteTargetKey>) -> Option<UncertainWriteState> {
        let target = target.into();
        self.by_target.lock().get(&target).cloned()
    }

    /// Capture a new Live retain. Idempotent if the existing Live retain is exactly
    /// equal; rejected if a different retain is already Live. This enforces criterion
    /// 217/279/287's "retained envelope is authoritative" contract.
    pub fn set_live(&self, retained: RetainedCanonicalWrite) -> Result<(), SetLiveError> {
        let mut guard = self.by_target.lock();
        match guard.get(&retained.target) {
            Some(UncertainWriteState::Live(existing)) if existing == &retained => Ok(()),
            Some(UncertainWriteState::Live(existing)) => {
                Err(SetLiveError::AlreadyLiveWithDifferentRetain {
                    existing_entry_id: existing.entry_id(),
                    incoming_entry_id: retained.entry_id(),
                })
            }
            Some(UncertainWriteState::Abandoned(existing)) => Err(SetLiveError::AbandonedRetain {
                entry_id: existing.entry_id(),
            }),
            None => {
                guard.insert(retained.target, UncertainWriteState::Live(retained));
                Ok(())
            }
        }
    }

    pub fn abandon(&self, target: impl Into<WriteTargetKey>) -> Option<UncertainWriteState> {
        let target = target.into();
        let mut guard = self.by_target.lock();
        let state = guard.get_mut(&target)?;
        if let UncertainWriteState::Live(retained) = state {
            *state = UncertainWriteState::Abandoned(retained.clone());
        }
        Some(state.clone())
    }

    pub fn clear(&self, target: impl Into<WriteTargetKey>) -> Option<UncertainWriteState> {
        let target = target.into();
        self.by_target.lock().remove(&target)
    }

    /// Pure function: given the retained envelope bytes, canonical messages observed
    /// on the thread, the requested scan scope, and whether retrieval was complete,
    /// decide the scan outcome.
    ///
    /// An `Incomplete` retrieval cannot produce `VerifiedNoMatch` even when nothing
    /// matched, because the retained envelope may live outside the inspected window.
    pub fn scan_for_exact_envelope(
        retained: &RetainedCanonicalWrite,
        messages: &[CanonicalMessageProbe],
        scope: ExactEnvelopeScanScope,
        completeness: ScanCompleteness,
    ) -> ExactEnvelopeScanResult {
        let matched = match scope {
            ExactEnvelopeScanScope::RecentWindow => messages
                .iter()
                .take(LAZY_RETRY_SCAN_WINDOW)
                .find(|probe| probe.envelope_bytes == retained.envelope_bytes()),
            ExactEnvelopeScanScope::FullHistory => messages
                .iter()
                .find(|probe| probe.envelope_bytes == retained.envelope_bytes()),
        };
        if let Some(probe) = matched {
            return ExactEnvelopeScanResult::Matched {
                entry_id: probe.entry_id,
            };
        }
        match completeness {
            ScanCompleteness::Complete => ExactEnvelopeScanResult::VerifiedNoMatch,
            ScanCompleteness::Incomplete => ExactEnvelopeScanResult::Inconclusive,
        }
    }

    /// Pure function: classify a lazy retry outcome from a typed scan result plus the
    /// canonical head observed at retry time. Head movement after a recent-window
    /// no-match requires a full-history scan before absence is conclusive (criterion
    /// 217).
    pub fn classify_retry(
        retained: &RetainedCanonicalWrite,
        scan: ExactEnvelopeScanResult,
        scope: ExactEnvelopeScanScope,
        observed_head_hash: EntryHash,
        now: SystemTime,
    ) -> UncertainWriteResolution {
        match scan {
            ExactEnvelopeScanResult::Matched { entry_id } => {
                UncertainWriteResolution::ClearedByExistingPost { entry_id }
            }
            ExactEnvelopeScanResult::VerifiedNoMatch
                if observed_head_hash != retained.previous_hash
                    && scope == ExactEnvelopeScanScope::FullHistory =>
            {
                UncertainWriteResolution::ClearedByConclusiveAbsence
            }
            ExactEnvelopeScanResult::VerifiedNoMatch
                if observed_head_hash != retained.previous_hash =>
            {
                UncertainWriteResolution::RequiresFullHistoryScan
            }
            ExactEnvelopeScanResult::VerifiedNoMatch if retained.requires_full_scan(now) => {
                UncertainWriteResolution::Abandoned
            }
            ExactEnvelopeScanResult::VerifiedNoMatch | ExactEnvelopeScanResult::Inconclusive => {
                UncertainWriteResolution::StillBlocked
            }
        }
    }
}

/// Published and bootstrap targets share the same async-mutex registry. Idle entries
/// are retained without a sweeper (criterion 154).
pub struct WriteCoordinator {
    per_target_locks: DashMap<WriteTargetKey, Arc<AsyncMutex<()>>>,
}

impl Default for WriteCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

impl WriteCoordinator {
    pub fn new() -> Self {
        Self {
            per_target_locks: DashMap::new(),
        }
    }

    pub fn lock_for(&self, target: impl Into<WriteTargetKey>) -> Arc<AsyncMutex<()>> {
        self.per_target_locks
            .entry(target.into())
            .or_insert_with(|| Arc::new(AsyncMutex::new(())))
            .clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CanonicalMessageProbe {
    pub entry_id: LedgerEntryId,
    pub envelope_bytes: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreparedCanonicalWrite {
    target: WriteTargetKey,
    envelope_bytes: Vec<u8>,
    prepared_body: String,
    last_known_summary: String,
    entry_id: LedgerEntryId,
    previous_hash: EntryHash,
    issued_at: SystemTime,
}

impl PreparedCanonicalWrite {
    pub fn new(
        target: impl Into<WriteTargetKey>,
        entry_id: LedgerEntryId,
        previous_hash: EntryHash,
        envelope_bytes: Vec<u8>,
        prepared_body: String,
        last_known_summary: String,
        issued_at: SystemTime,
    ) -> Self {
        Self {
            target: target.into(),
            entry_id,
            previous_hash,
            envelope_bytes,
            prepared_body,
            last_known_summary,
            issued_at,
        }
    }

    pub fn target(&self) -> WriteTargetKey {
        self.target
    }
    pub fn entry_id(&self) -> LedgerEntryId {
        self.entry_id
    }
    pub fn previous_hash(&self) -> EntryHash {
        self.previous_hash
    }
    pub fn envelope_bytes(&self) -> &[u8] {
        &self.envelope_bytes
    }
    pub fn prepared_body(&self) -> &str {
        &self.prepared_body
    }
    pub fn last_known_summary(&self) -> &str {
        &self.last_known_summary
    }
    pub fn issued_at(&self) -> SystemTime {
        self.issued_at
    }

    pub fn into_retained(self) -> RetainedCanonicalWrite {
        RetainedCanonicalWrite {
            target: self.target,
            entry_id: self.entry_id,
            previous_hash: self.previous_hash,
            envelope_bytes: Arc::new(self.envelope_bytes),
            prepared_body: self.prepared_body,
            last_known_summary: self.last_known_summary,
            live_since: self.issued_at,
        }
    }
}

/// Resolve a Live `uncertain_write` retain by combining a verified canonical
/// read with the exact-envelope scan defined by [`UncertainWriteRegistry`].
/// Returns `true` when the retain has cleared (post landed, or conclusive
/// absence observed) so the caller may proceed with a fresh write; returns
/// `false` while the retain still blocks new writes.
///
/// On scope `RequiresFullHistoryScan` the function re-runs the scan with full
/// history before classifying. On the `Abandoned` outcome the function emits
/// the criterion-251 / 287 observability event and marks the registry
/// `Abandoned`; the caller still receives `false` because user acknowledgement
/// is required before the block lifts.
///
/// `reader` is a per-request port; the caller binds the transport context
/// (serenity `Context`, canonical thread id, ledger id) at the boundary so
/// this function never sees Discord types.
pub async fn resolve_uncertain_write_v1(
    reader: &impl CanonicalThreadReader,
    uncertain_writes: &UncertainWriteRegistry,
    observability: &dyn LedgerObservability,
    clock: &dyn Clock,
    ledger_id: LedgerId,
) -> bool {
    let retained = match uncertain_writes.current(ledger_id) {
        None => return true,
        Some(UncertainWriteState::Abandoned(_)) => return false,
        Some(UncertainWriteState::Live(retained)) => retained,
    };
    let now = clock.now();
    let retain_expired = retained.requires_full_scan(now);
    let initial_scope = if retain_expired {
        ExactEnvelopeScanScope::FullHistory
    } else {
        ExactEnvelopeScanScope::RecentWindow
    };
    let scan_future = async {
        if retain_expired {
            reader.scan_all().await
        } else {
            reader.scan_recent().await
        }
    };
    let Ok((load, probes)) = tokio::try_join!(reader.load_verified_thread(), scan_future) else {
        return false;
    };
    let observed_head =
        append_previous_hash_sha256_v1(ledger_id, load.snapshot().current_head_hash());

    let scan = UncertainWriteRegistry::scan_for_exact_envelope(
        &retained,
        &probes,
        initial_scope,
        ScanCompleteness::Complete,
    );
    let mut resolution =
        UncertainWriteRegistry::classify_retry(&retained, scan, initial_scope, observed_head, now);

    if resolution == UncertainWriteResolution::RequiresFullHistoryScan {
        let Ok(probes) = reader.scan_all().await else {
            return false;
        };
        let scan = UncertainWriteRegistry::scan_for_exact_envelope(
            &retained,
            &probes,
            ExactEnvelopeScanScope::FullHistory,
            ScanCompleteness::Complete,
        );
        resolution = UncertainWriteRegistry::classify_retry(
            &retained,
            scan,
            ExactEnvelopeScanScope::FullHistory,
            observed_head,
            now,
        );
    }
    match resolution {
        UncertainWriteResolution::ClearedByExistingPost { .. }
        | UncertainWriteResolution::ClearedByConclusiveAbsence => {
            uncertain_writes.clear(ledger_id);
            true
        }
        UncertainWriteResolution::Abandoned => {
            observability.emit(LedgerObservabilityEvent::PersistentUncertainWrite {
                ledger_id,
                live_since: retained.live_since(),
                now,
            });
            uncertain_writes.abandon(ledger_id);
            false
        }
        UncertainWriteResolution::RequiresFullHistoryScan
        | UncertainWriteResolution::StillBlocked => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::ledger_chain_genesis_sha256_v1;
    use rstest::rstest;
    use std::time::{Duration, UNIX_EPOCH};

    fn retained(
        target: impl Into<WriteTargetKey>,
        bytes: Vec<u8>,
        entry_id: u64,
    ) -> RetainedCanonicalWrite {
        RetainedCanonicalWrite {
            target: target.into(),
            entry_id: LedgerEntryId(entry_id),
            previous_hash: ledger_chain_genesis_sha256_v1(
                walicord_ledger::test_fixtures::ledger_id(77),
            ),
            envelope_bytes: Arc::new(bytes),
            prepared_body: "draft".to_owned(),
            last_known_summary: "summary".to_owned(),
            live_since: UNIX_EPOCH,
        }
    }

    fn probe(entry_id: u64, bytes: Vec<u8>) -> CanonicalMessageProbe {
        CanonicalMessageProbe {
            entry_id: LedgerEntryId(entry_id),
            envelope_bytes: bytes,
        }
    }

    #[test]
    fn set_live_then_current_returns_live_state() {
        let registry = UncertainWriteRegistry::new();
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        registry
            .set_live(entry.clone())
            .expect("first set_live should succeed");

        let actual = registry.current(walicord_ledger::test_fixtures::ledger_id(77));

        assert_eq!(actual, Some(UncertainWriteState::Live(entry)));
    }

    #[test]
    fn set_live_rejects_different_retain_when_live_state_already_exists() {
        let registry = UncertainWriteRegistry::new();
        let existing = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        let incoming = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"Y".to_vec(),
            2,
        );
        registry
            .set_live(existing.clone())
            .expect("first set_live succeeds");

        let actual = registry.set_live(incoming);

        assert_eq!(
            actual,
            Err(SetLiveError::AlreadyLiveWithDifferentRetain {
                existing_entry_id: LedgerEntryId(1),
                incoming_entry_id: LedgerEntryId(2),
            })
        );
        assert_eq!(
            registry.current(walicord_ledger::test_fixtures::ledger_id(77)),
            Some(UncertainWriteState::Live(existing))
        );
    }

    #[test]
    fn set_live_is_idempotent_when_retain_is_exactly_equal() {
        let registry = UncertainWriteRegistry::new();
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        registry.set_live(entry.clone()).expect("first");
        let actual = registry.set_live(entry);
        assert_eq!(actual, Ok(()));
    }

    #[test]
    fn clear_removes_entry() {
        let registry = UncertainWriteRegistry::new();
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        registry.set_live(entry.clone()).expect("set_live");

        let removed = registry.clear(walicord_ledger::test_fixtures::ledger_id(77));

        assert_eq!(removed, Some(UncertainWriteState::Live(entry)));
        assert_eq!(
            registry.current(walicord_ledger::test_fixtures::ledger_id(77)),
            None
        );
    }

    #[test]
    fn abandon_preserves_frozen_retain_until_explicit_recovery() {
        let registry = UncertainWriteRegistry::new();
        let ledger_id = walicord_ledger::test_fixtures::ledger_id(77);
        let entry = retained(ledger_id, b"X".to_vec(), 1);
        registry
            .set_live(entry.clone())
            .expect("first set_live should succeed");

        assert_eq!(
            registry.abandon(ledger_id),
            Some(UncertainWriteState::Abandoned(entry.clone()))
        );
        assert_eq!(
            registry.current(ledger_id),
            Some(UncertainWriteState::Abandoned(entry))
        );
    }

    #[test]
    fn complete_scan_with_match_returns_matched() {
        let bytes = b"matching".to_vec();
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            bytes.clone(),
            1,
        );
        let probes = vec![probe(99, b"other".to_vec()), probe(1, bytes)];

        let actual = UncertainWriteRegistry::scan_for_exact_envelope(
            &entry,
            &probes,
            ExactEnvelopeScanScope::RecentWindow,
            ScanCompleteness::Complete,
        );

        assert_eq!(
            actual,
            ExactEnvelopeScanResult::Matched {
                entry_id: LedgerEntryId(1)
            }
        );
    }

    #[test]
    fn complete_scan_with_no_match_returns_verified_no_match() {
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        let probes = vec![probe(99, b"other".to_vec())];

        let actual = UncertainWriteRegistry::scan_for_exact_envelope(
            &entry,
            &probes,
            ExactEnvelopeScanScope::RecentWindow,
            ScanCompleteness::Complete,
        );

        assert_eq!(actual, ExactEnvelopeScanResult::VerifiedNoMatch);
    }

    #[test]
    fn incomplete_scan_with_no_match_returns_inconclusive() {
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        let probes = vec![probe(99, b"other".to_vec())];

        let actual = UncertainWriteRegistry::scan_for_exact_envelope(
            &entry,
            &probes,
            ExactEnvelopeScanScope::RecentWindow,
            ScanCompleteness::Incomplete,
        );

        assert_eq!(actual, ExactEnvelopeScanResult::Inconclusive);
    }

    #[test]
    fn scan_only_inspects_first_window_of_messages() {
        let bytes = b"target".to_vec();
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            bytes.clone(),
            1,
        );
        let mut probes: Vec<CanonicalMessageProbe> = (0..LAZY_RETRY_SCAN_WINDOW)
            .map(|i| probe(100 + i as u64, vec![0xaa]))
            .collect();
        probes.push(probe(7, bytes));

        let actual = UncertainWriteRegistry::scan_for_exact_envelope(
            &entry,
            &probes,
            ExactEnvelopeScanScope::RecentWindow,
            ScanCompleteness::Complete,
        );

        assert_eq!(actual, ExactEnvelopeScanResult::VerifiedNoMatch);
    }

    #[test]
    fn full_history_scan_inspects_messages_after_recent_window() {
        let bytes = b"target".to_vec();
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            bytes.clone(),
            1,
        );
        let mut probes: Vec<CanonicalMessageProbe> = (0..LAZY_RETRY_SCAN_WINDOW)
            .map(|i| probe(100 + i as u64, vec![0xaa]))
            .collect();
        probes.push(probe(7, bytes));

        let actual = UncertainWriteRegistry::scan_for_exact_envelope(
            &entry,
            &probes,
            ExactEnvelopeScanScope::FullHistory,
            ScanCompleteness::Complete,
        );

        assert_eq!(
            actual,
            ExactEnvelopeScanResult::Matched {
                entry_id: LedgerEntryId(7)
            }
        );
    }

    #[test]
    fn classify_retry_with_match_clears_by_existing_post() {
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        let head = entry.previous_hash;

        let actual = UncertainWriteRegistry::classify_retry(
            &entry,
            ExactEnvelopeScanResult::Matched {
                entry_id: LedgerEntryId(1),
            },
            ExactEnvelopeScanScope::RecentWindow,
            head,
            UNIX_EPOCH,
        );

        assert_eq!(
            actual,
            UncertainWriteResolution::ClearedByExistingPost {
                entry_id: LedgerEntryId(1)
            }
        );
    }

    #[test]
    fn classify_retry_with_verified_no_match_and_advanced_head_clears_by_conclusive_absence() {
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        let other_head =
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(99));

        let actual = UncertainWriteRegistry::classify_retry(
            &entry,
            ExactEnvelopeScanResult::VerifiedNoMatch,
            ExactEnvelopeScanScope::FullHistory,
            other_head,
            UNIX_EPOCH,
        );

        assert_eq!(actual, UncertainWriteResolution::ClearedByConclusiveAbsence);
    }

    #[test]
    fn classify_retry_with_recent_no_match_and_advanced_head_does_not_clear() {
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        let other_head =
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(99));

        let actual = UncertainWriteRegistry::classify_retry(
            &entry,
            ExactEnvelopeScanResult::VerifiedNoMatch,
            ExactEnvelopeScanScope::RecentWindow,
            other_head,
            UNIX_EPOCH,
        );

        assert_eq!(actual, UncertainWriteResolution::RequiresFullHistoryScan);
    }

    #[test]
    fn classify_retry_inconclusive_scan_remains_blocked_even_with_head_movement() {
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        let other_head =
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(99));

        let actual = UncertainWriteRegistry::classify_retry(
            &entry,
            ExactEnvelopeScanResult::Inconclusive,
            ExactEnvelopeScanScope::RecentWindow,
            other_head,
            UNIX_EPOCH + UNCERTAIN_WRITE_RETAIN_TTL,
        );

        assert_eq!(actual, UncertainWriteResolution::StillBlocked);
    }

    #[test]
    fn classify_retry_verified_no_match_without_head_movement_remains_blocked() {
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );
        let same_head = entry.previous_hash;

        let actual = UncertainWriteRegistry::classify_retry(
            &entry,
            ExactEnvelopeScanResult::VerifiedNoMatch,
            ExactEnvelopeScanScope::RecentWindow,
            same_head,
            UNIX_EPOCH,
        );

        assert_eq!(actual, UncertainWriteResolution::StillBlocked);
    }

    #[test]
    fn classify_retry_verified_no_match_after_retain_ttl_requires_explicit_abandoned_recovery() {
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );

        let actual = UncertainWriteRegistry::classify_retry(
            &entry,
            ExactEnvelopeScanResult::VerifiedNoMatch,
            ExactEnvelopeScanScope::FullHistory,
            entry.previous_hash,
            UNIX_EPOCH + UNCERTAIN_WRITE_RETAIN_TTL,
        );

        assert_eq!(actual, UncertainWriteResolution::Abandoned);
    }

    #[rstest]
    #[case::before_ttl(UNCERTAIN_WRITE_RETAIN_TTL - Duration::from_secs(1), false)]
    #[case::at_ttl(UNCERTAIN_WRITE_RETAIN_TTL, true)]
    fn retained_write_requires_full_scan_only_after_ttl(
        #[case] elapsed: Duration,
        #[case] expected: bool,
    ) {
        let entry = retained(
            walicord_ledger::test_fixtures::ledger_id(77),
            b"X".to_vec(),
            1,
        );

        assert_eq!(entry.requires_full_scan(UNIX_EPOCH + elapsed), expected);
    }

    #[test]
    fn write_coordinator_returns_same_lock_arc_for_repeated_published_key() {
        let coordinator = WriteCoordinator::new();
        let first = coordinator.lock_for(walicord_ledger::test_fixtures::ledger_id(77));
        let second = coordinator.lock_for(walicord_ledger::test_fixtures::ledger_id(77));

        assert!(Arc::ptr_eq(&first, &second));
    }

    #[test]
    fn write_coordinator_returns_distinct_locks_for_distinct_ledgers() {
        let coordinator = WriteCoordinator::new();
        let first = coordinator.lock_for(walicord_ledger::test_fixtures::ledger_id(1));
        let second = coordinator.lock_for(walicord_ledger::test_fixtures::ledger_id(2));

        assert!(!Arc::ptr_eq(&first, &second));
    }

    #[test]
    fn write_coordinator_keeps_bootstrap_lock_distinct_from_published_lock() {
        let coordinator = WriteCoordinator::new();
        let bootstrap =
            coordinator.lock_for(WriteTargetKey::Bootstrap(BootstrapWriteTarget::new(1, 77)));
        let published = coordinator.lock_for(walicord_ledger::test_fixtures::ledger_id(77));

        assert!(!Arc::ptr_eq(&bootstrap, &published));
    }

    #[tokio::test]
    async fn write_coordinator_lock_for_published_ledger_serializes_holders() {
        let coordinator = Arc::new(WriteCoordinator::new());
        let order = Arc::new(Mutex::new(Vec::new()));

        let coord_a = Arc::clone(&coordinator);
        let order_a = Arc::clone(&order);
        let task_a = tokio::spawn(async move {
            let lock = coord_a.lock_for(walicord_ledger::test_fixtures::ledger_id(77));
            let _guard = lock.lock().await;
            order_a.lock().push("a_acquired");
            tokio::time::sleep(Duration::from_millis(20)).await;
            order_a.lock().push("a_released");
        });

        tokio::time::sleep(Duration::from_millis(5)).await;

        let coord_b = Arc::clone(&coordinator);
        let order_b = Arc::clone(&order);
        let task_b = tokio::spawn(async move {
            let lock = coord_b.lock_for(walicord_ledger::test_fixtures::ledger_id(77));
            let _guard = lock.lock().await;
            order_b.lock().push("b_acquired");
        });

        let _ = tokio::join!(task_a, task_b);

        let final_order: Vec<&str> = order.lock().iter().copied().collect();
        assert_eq!(final_order, vec!["a_acquired", "a_released", "b_acquired"]);
    }

    #[test]
    fn prepared_canonical_write_into_retained_preserves_payload_identity() {
        let prepared = PreparedCanonicalWrite::new(
            walicord_ledger::test_fixtures::ledger_id(77),
            LedgerEntryId(1),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            b"X".to_vec(),
            "body".to_owned(),
            "summary".to_owned(),
            UNIX_EPOCH,
        );

        let retained = prepared.into_retained();

        assert_eq!(retained.entry_id(), LedgerEntryId(1));
        assert_eq!(retained.envelope_bytes(), b"X");
        assert_eq!(retained.prepared_body(), "body");
        assert_eq!(retained.last_known_summary(), "summary");
    }
}
