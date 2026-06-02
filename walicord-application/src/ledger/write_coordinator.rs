use crate::ledger::{EntryHash, LedgerEntryId, LedgerId, UnverifiedLedgerStoreEnvelope};
use dashmap::DashMap;
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    time::{Duration, SystemTime},
};
use tokio::sync::Mutex as AsyncMutex;

/// Number of trailing canonical messages the normal lazy-retry pass scans for an
/// exact-envelope match before retaining the `uncertain_write` block (criterion 176).
pub const LAZY_RETRY_SCAN_WINDOW: usize = 5;
pub const UNCERTAIN_WRITE_RETAIN_TTL: Duration = Duration::from_secs(10 * 60);

/// Write addressing key. Each canonical append targets exactly one ledger; the
/// per-`LedgerId` async mutex serializes them. The adapter issues the identifier
/// before the first append and reuses it for the entire canonical write lifecycle.
pub type WriteTargetKey = LedgerId;

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
        target: WriteTargetKey,
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
        envelope_bytes: Vec<u8>,
        prepared_body: String,
        last_known_summary: String,
        live_since: SystemTime,
    ) -> Self {
        Self {
            target,
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
        now.duration_since(self.live_since).unwrap_or_default() >= UNCERTAIN_WRITE_RETAIN_TTL
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UncertainWriteState {
    /// Retained payload still exists; lazy retry may still clear or confirm.
    Live(RetainedCanonicalWrite),
}

impl UncertainWriteState {
    pub fn target(&self) -> WriteTargetKey {
        match self {
            Self::Live(retained) => retained.target(),
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
    /// A complete scan found no retained envelope after the bounded retain window.
    ClearedByExpiredConclusiveAbsence,
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

    pub fn current(&self, target: WriteTargetKey) -> Option<UncertainWriteState> {
        self.by_target
            .lock()
            .expect("UncertainWriteRegistry mutex poisoned")
            .get(&target)
            .cloned()
    }

    /// Capture a new Live retain. Idempotent if the existing Live retain is exactly
    /// equal; rejected if a different retain is already Live. This enforces criterion
    /// 217/279/287's "retained envelope is authoritative" contract.
    pub fn set_live(&self, retained: RetainedCanonicalWrite) -> Result<(), SetLiveError> {
        let mut guard = self
            .by_target
            .lock()
            .expect("UncertainWriteRegistry mutex poisoned");
        match guard.get(&retained.target) {
            Some(UncertainWriteState::Live(existing)) if existing == &retained => Ok(()),
            Some(UncertainWriteState::Live(existing)) => {
                Err(SetLiveError::AlreadyLiveWithDifferentRetain {
                    existing_entry_id: existing.entry_id(),
                    incoming_entry_id: retained.entry_id(),
                })
            }
            None => {
                guard.insert(retained.target, UncertainWriteState::Live(retained));
                Ok(())
            }
        }
    }

    pub fn clear(&self, target: WriteTargetKey) -> Option<UncertainWriteState> {
        self.by_target
            .lock()
            .expect("UncertainWriteRegistry mutex poisoned")
            .remove(&target)
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
                UncertainWriteResolution::ClearedByExpiredConclusiveAbsence
            }
            ExactEnvelopeScanResult::VerifiedNoMatch | ExactEnvelopeScanResult::Inconclusive => {
                UncertainWriteResolution::StillBlocked
            }
        }
    }
}

/// Per-`LedgerId` async-mutex map. Each ledger has its own async mutex; idle entries
/// are retained without a sweeper (criterion 154).
pub struct WriteCoordinator {
    per_ledger_locks: DashMap<LedgerId, Arc<AsyncMutex<()>>>,
}

impl Default for WriteCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

impl WriteCoordinator {
    pub fn new() -> Self {
        Self {
            per_ledger_locks: DashMap::new(),
        }
    }

    pub fn lock_for(&self, target: WriteTargetKey) -> Arc<AsyncMutex<()>> {
        self.per_ledger_locks
            .entry(target)
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
        target: WriteTargetKey,
        entry_id: LedgerEntryId,
        previous_hash: EntryHash,
        envelope_bytes: Vec<u8>,
        prepared_body: String,
        last_known_summary: String,
        issued_at: SystemTime,
    ) -> Self {
        Self {
            target,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::ledger_chain_genesis_sha256_v1;
    use rstest::rstest;
    use std::time::{Duration, UNIX_EPOCH};

    fn retained(target: WriteTargetKey, bytes: Vec<u8>, entry_id: u64) -> RetainedCanonicalWrite {
        RetainedCanonicalWrite {
            target,
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
    fn classify_retry_verified_no_match_after_retain_ttl_clears_by_expired_absence() {
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

        assert_eq!(
            actual,
            UncertainWriteResolution::ClearedByExpiredConclusiveAbsence
        );
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

    #[tokio::test]
    async fn write_coordinator_lock_for_published_ledger_serializes_holders() {
        let coordinator = Arc::new(WriteCoordinator::new());
        let order = Arc::new(Mutex::new(Vec::new()));

        let coord_a = Arc::clone(&coordinator);
        let order_a = Arc::clone(&order);
        let task_a = tokio::spawn(async move {
            let lock = coord_a.lock_for(walicord_ledger::test_fixtures::ledger_id(77));
            let _guard = lock.lock().await;
            order_a.lock().unwrap().push("a_acquired");
            tokio::time::sleep(Duration::from_millis(20)).await;
            order_a.lock().unwrap().push("a_released");
        });

        tokio::time::sleep(Duration::from_millis(5)).await;

        let coord_b = Arc::clone(&coordinator);
        let order_b = Arc::clone(&order);
        let task_b = tokio::spawn(async move {
            let lock = coord_b.lock_for(walicord_ledger::test_fixtures::ledger_id(77));
            let _guard = lock.lock().await;
            order_b.lock().unwrap().push("b_acquired");
        });

        let _ = tokio::join!(task_a, task_b);

        let final_order: Vec<&str> = order.lock().unwrap().iter().copied().collect();
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
