use crate::{
    InteractionNonce,
    ledger::{EntryHash, ExpenseNote, LedgerEffectiveDate, LedgerEntryId, LedgerId},
};
use dashmap::DashMap;
use std::{
    collections::{BTreeMap, HashMap},
    sync::{Arc, Mutex},
    time::{Duration, SystemTime},
};
use tokio::sync::Mutex as AsyncMutex;
use walicord_domain::{
    Money,
    model::{MemberId, RoleId, Weight},
};

pub const EXPENSE_SESSION_TTL: Duration = Duration::from_secs(10 * 60);
pub const VOID_SESSION_TTL: Duration = Duration::from_secs(10 * 60);
pub const MODAL_RETRY_TTL: Duration = Duration::from_secs(10 * 60);

/// Identity for an in-progress expense draft. Keyed by `(LedgerId, MemberId)` rather
/// than the Discord-side `(GuildId, ChannelId, MemberId)` triple so the application
/// layer never has to import serenity types. The adapter is responsible for
/// translating `(GuildId, ChannelId)` → `LedgerId` at the interaction boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpenseSessionKey {
    ledger_id: LedgerId,
    actor_id: MemberId,
}

impl ExpenseSessionKey {
    pub fn new(ledger_id: LedgerId, actor_id: MemberId) -> Self {
        Self {
            ledger_id,
            actor_id,
        }
    }

    pub fn ledger_id(self) -> LedgerId {
        self.ledger_id
    }
    pub fn actor_id(self) -> MemberId {
        self.actor_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VoidSessionKey {
    ledger_id: LedgerId,
    actor_id: MemberId,
}

impl VoidSessionKey {
    pub fn new(ledger_id: LedgerId, actor_id: MemberId) -> Self {
        Self {
            ledger_id,
            actor_id,
        }
    }

    pub fn ledger_id(self) -> LedgerId {
        self.ledger_id
    }
    pub fn actor_id(self) -> MemberId {
        self.actor_id
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpenseSelectionPhase {
    Payer,
    ParticipantSource,
    IndividualSelection,
    Roles,
    WeightEditor,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpenseSessionStage {
    AwaitingBasicInfo,
    InSelection { phase: ExpenseSelectionPhase },
    InConfirmation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VoidSessionStage {
    SelectingCandidate,
    Confirming,
}

/// Validated basic info captured from a successful expense-modal submission. Once
/// present in the draft, the session has crossed from `AwaitingBasicInfo` into
/// `InSelection`; per criterion 229 it can be re-edited via `基本情報を修正する`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseBasicInfo {
    pub amount: Money,
    pub note: Option<ExpenseNote>,
    pub effective_date: LedgerEffectiveDate,
}

/// Live selection state across the wizard phases. Roles and the `MEMBERS` group are
/// resolved at append time (criterion 81 / 4) so this struct stores only the actor's
/// raw choices, not the resolved participant set; the confirmation snapshot below
/// records the resolved set as observed at confirmation time.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ExpenseSelectionState {
    pub payer: Option<MemberId>,
    pub individual_members: Vec<MemberId>,
    pub selected_roles: Vec<RoleId>,
    pub include_members_group: bool,
    pub weight_overrides: BTreeMap<MemberId, Weight>,
}

/// One resolved participant row as captured at confirmation rebuild time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseParticipantSelection {
    pub member_id: MemberId,
    pub weight: Weight,
}

/// Frozen view of the resolved participants shown to the actor at confirmation. Used
/// to detect participant drift (criterion 111) and to display the exact `×n / 既定値`
/// row template (criteria 216, 226).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseConfirmationSnapshot {
    pub participants: Vec<ExpenseParticipantSelection>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ExpenseDraftSnapshot {
    basic_info: Option<ExpenseBasicInfo>,
    selection_state: ExpenseSelectionState,
    confirmation_snapshot: Option<ExpenseConfirmationSnapshot>,
}

impl ExpenseDraftSnapshot {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn with_basic_info(mut self, basic_info: ExpenseBasicInfo) -> Self {
        self.basic_info = Some(basic_info);
        self
    }

    pub fn with_selection_state(mut self, selection_state: ExpenseSelectionState) -> Self {
        self.selection_state = selection_state;
        self
    }

    pub fn with_confirmation_snapshot(mut self, snapshot: ExpenseConfirmationSnapshot) -> Self {
        self.confirmation_snapshot = Some(snapshot);
        self
    }

    pub fn basic_info(&self) -> Option<&ExpenseBasicInfo> {
        self.basic_info.as_ref()
    }

    pub fn selection_state(&self) -> &ExpenseSelectionState {
        &self.selection_state
    }

    pub fn confirmation_snapshot(&self) -> Option<&ExpenseConfirmationSnapshot> {
        self.confirmation_snapshot.as_ref()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VoidCandidateSelection {
    target_entry_id: LedgerEntryId,
}

impl VoidCandidateSelection {
    pub fn new(target_entry_id: LedgerEntryId) -> Self {
        Self { target_entry_id }
    }

    pub fn target_entry_id(&self) -> LedgerEntryId {
        self.target_entry_id
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ExpenseSessionConstructionError {
    #[error("AwaitingBasicInfo session cannot carry basic info")]
    AwaitingBasicInfoCannotHaveBasicInfo,
    #[error("InSelection stage requires basic info to be present in the draft")]
    InSelectionRequiresBasicInfo,
    #[error("InConfirmation stage requires basic info and a confirmation snapshot")]
    InConfirmationRequiresBasicInfoAndSelection,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum VoidSessionConstructionError {
    #[error("Confirming stage requires a void candidate to be selected")]
    ConfirmingRequiresCandidate,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseSession {
    key: ExpenseSessionKey,
    stage: ExpenseSessionStage,
    draft: ExpenseDraftSnapshot,
    nonce: InteractionNonce,
    last_touched: SystemTime,
}

impl ExpenseSession {
    pub fn new(
        key: ExpenseSessionKey,
        stage: ExpenseSessionStage,
        draft: ExpenseDraftSnapshot,
        nonce: InteractionNonce,
        last_touched: SystemTime,
    ) -> Result<Self, ExpenseSessionConstructionError> {
        match stage {
            ExpenseSessionStage::AwaitingBasicInfo if draft.basic_info.is_some() => {
                Err(ExpenseSessionConstructionError::AwaitingBasicInfoCannotHaveBasicInfo)
            }
            ExpenseSessionStage::InSelection { .. } if draft.basic_info.is_none() => {
                Err(ExpenseSessionConstructionError::InSelectionRequiresBasicInfo)
            }
            ExpenseSessionStage::InConfirmation
                if draft.basic_info.is_none() || draft.confirmation_snapshot.is_none() =>
            {
                Err(ExpenseSessionConstructionError::InConfirmationRequiresBasicInfoAndSelection)
            }
            _ => Ok(Self {
                key,
                stage,
                draft,
                nonce,
                last_touched,
            }),
        }
    }

    pub fn key(&self) -> ExpenseSessionKey {
        self.key
    }
    pub fn stage(&self) -> &ExpenseSessionStage {
        &self.stage
    }
    pub fn draft(&self) -> &ExpenseDraftSnapshot {
        &self.draft
    }
    pub fn nonce(&self) -> InteractionNonce {
        self.nonce
    }
    pub fn last_touched(&self) -> SystemTime {
        self.last_touched
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VoidSession {
    key: VoidSessionKey,
    stage: VoidSessionStage,
    selection: Option<VoidCandidateSelection>,
    nonce: InteractionNonce,
    last_touched: SystemTime,
}

impl VoidSession {
    pub fn new(
        key: VoidSessionKey,
        stage: VoidSessionStage,
        selection: Option<VoidCandidateSelection>,
        nonce: InteractionNonce,
        last_touched: SystemTime,
    ) -> Result<Self, VoidSessionConstructionError> {
        match (&stage, &selection) {
            (VoidSessionStage::Confirming, None) => {
                Err(VoidSessionConstructionError::ConfirmingRequiresCandidate)
            }
            _ => Ok(Self {
                key,
                stage,
                selection,
                nonce,
                last_touched,
            }),
        }
    }

    pub fn key(&self) -> VoidSessionKey {
        self.key
    }
    pub fn stage(&self) -> &VoidSessionStage {
        &self.stage
    }
    pub fn selection(&self) -> Option<&VoidCandidateSelection> {
        self.selection.as_ref()
    }
    pub fn nonce(&self) -> InteractionNonce {
        self.nonce
    }
    pub fn last_touched(&self) -> SystemTime {
        self.last_touched
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModalRetryPreserved {
    pub raw_amount: String,
    pub raw_note: String,
    pub raw_date: String,
}

/// Single-use retry binding for re-opening a modal with preserved values after a
/// validation failure. Single-use is enforced structurally by
/// [`ModalRetryBindingStore::try_consume`] removing the binding from the store on the
/// first successful consume — the type itself does not carry a `consumed` flag because
/// such a flag would be bypassable by cloning the binding outside the store.
///
/// Scope is bound to `(LedgerId, MemberId)` rather than `(ChannelId, MemberId)` so the
/// application layer stays free of serenity types; the adapter is responsible for
/// translating Discord identifiers to `LedgerId` at the interaction boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModalRetryBinding {
    binding_nonce: InteractionNonce,
    actor_id: MemberId,
    ledger_id: LedgerId,
    preserved: ModalRetryPreserved,
    created_at: SystemTime,
    expires_at: SystemTime,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ModalRetryBindingError {
    #[error("modal retry binding not found in store")]
    NotFound,
    #[error("modal retry binding expired at {expires_at:?} (now {now:?})")]
    Expired {
        now: SystemTime,
        expires_at: SystemTime,
    },
    #[error("modal retry actor mismatch: observed {actual:?}, expected {expected:?}")]
    ActorMismatch {
        actual: MemberId,
        expected: MemberId,
    },
    #[error("modal retry ledger mismatch: observed {actual:?}, expected {expected:?}")]
    LedgerMismatch {
        actual: LedgerId,
        expected: LedgerId,
    },
}

impl ModalRetryBinding {
    pub fn capture(
        binding_nonce: InteractionNonce,
        actor_id: MemberId,
        ledger_id: LedgerId,
        preserved: ModalRetryPreserved,
        created_at: SystemTime,
    ) -> Self {
        Self {
            binding_nonce,
            actor_id,
            ledger_id,
            preserved,
            created_at,
            expires_at: created_at + MODAL_RETRY_TTL,
        }
    }

    pub fn binding_nonce(&self) -> InteractionNonce {
        self.binding_nonce
    }
    pub fn actor_id(&self) -> MemberId {
        self.actor_id
    }
    pub fn ledger_id(&self) -> LedgerId {
        self.ledger_id
    }
    pub fn preserved(&self) -> &ModalRetryPreserved {
        &self.preserved
    }
    pub fn created_at(&self) -> SystemTime {
        self.created_at
    }
    pub fn expires_at(&self) -> SystemTime {
        self.expires_at
    }
}

pub struct ModalRetryBindingStore {
    by_nonce: Mutex<HashMap<InteractionNonce, ModalRetryBinding>>,
}

impl Default for ModalRetryBindingStore {
    fn default() -> Self {
        Self::new()
    }
}

impl ModalRetryBindingStore {
    pub fn new() -> Self {
        Self {
            by_nonce: Mutex::new(HashMap::new()),
        }
    }

    pub fn store(&self, binding: ModalRetryBinding) {
        self.by_nonce
            .lock()
            .expect("ModalRetryBindingStore mutex poisoned")
            .insert(binding.binding_nonce, binding);
    }

    /// Atomically validate and remove the binding. Single-use is structural: the binding
    /// is removed from the store on success, so subsequent attempts with the same nonce
    /// return [`ModalRetryBindingError::NotFound`].
    pub fn try_consume(
        &self,
        binding_nonce: InteractionNonce,
        actor_id: MemberId,
        ledger_id: LedgerId,
        now: SystemTime,
    ) -> Result<ModalRetryPreserved, ModalRetryBindingError> {
        let mut guard = self
            .by_nonce
            .lock()
            .expect("ModalRetryBindingStore mutex poisoned");
        let Some(binding) = guard.get(&binding_nonce) else {
            return Err(ModalRetryBindingError::NotFound);
        };
        if now >= binding.expires_at {
            let expires_at = binding.expires_at;
            guard.remove(&binding_nonce);
            return Err(ModalRetryBindingError::Expired { now, expires_at });
        }
        if binding.actor_id != actor_id {
            return Err(ModalRetryBindingError::ActorMismatch {
                actual: actor_id,
                expected: binding.actor_id,
            });
        }
        if binding.ledger_id != ledger_id {
            return Err(ModalRetryBindingError::LedgerMismatch {
                actual: ledger_id,
                expected: binding.ledger_id,
            });
        }
        let binding = guard
            .remove(&binding_nonce)
            .expect("binding was just observed under the same lock");
        Ok(binding.preserved)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PickerSnapshotId(u64);

impl PickerSnapshotId {
    pub fn new(value: u64) -> Self {
        Self(value)
    }
    pub fn get(self) -> u64 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PagedPickerState {
    snapshot_id: PickerSnapshotId,
    current_page: usize,
    query: Option<String>,
    selection: Vec<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum PagedPickerStateError {
    #[error("paged picker snapshot is stale: observed {actual:?}, expected {expected:?}")]
    StaleSnapshot {
        actual: PickerSnapshotId,
        expected: PickerSnapshotId,
    },
}

impl PagedPickerState {
    pub fn new(
        snapshot_id: PickerSnapshotId,
        current_page: usize,
        query: Option<String>,
        selection: Vec<u64>,
    ) -> Self {
        Self {
            snapshot_id,
            current_page,
            query,
            selection,
        }
    }

    pub fn snapshot_id(&self) -> PickerSnapshotId {
        self.snapshot_id
    }
    pub fn current_page(&self) -> usize {
        self.current_page
    }
    pub fn query(&self) -> Option<&str> {
        self.query.as_deref()
    }
    pub fn selection(&self) -> &[u64] {
        &self.selection
    }

    pub fn require_snapshot(
        &self,
        expected: PickerSnapshotId,
    ) -> Result<(), PagedPickerStateError> {
        if self.snapshot_id == expected {
            Ok(())
        } else {
            Err(PagedPickerStateError::StaleSnapshot {
                actual: self.snapshot_id,
                expected,
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PagedReadViewRoute {
    ReviewParent,
    ReviewThread,
    LedgerCommand,
    PanelLedger,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PagedReadViewState {
    snapshot_hash: EntryHash,
    current_page: usize,
    actor_id: MemberId,
    route: PagedReadViewRoute,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum PagedReadViewStateError {
    #[error("paged read view snapshot is stale: observed {actual:?}, expected {expected:?}")]
    StaleSnapshot {
        actual: EntryHash,
        expected: EntryHash,
    },
    #[error("paged read view actor mismatch: observed {actual:?}, expected {expected:?}")]
    ActorMismatch {
        actual: MemberId,
        expected: MemberId,
    },
}

impl PagedReadViewState {
    pub fn new(
        snapshot_hash: EntryHash,
        current_page: usize,
        actor_id: MemberId,
        route: PagedReadViewRoute,
    ) -> Self {
        Self {
            snapshot_hash,
            current_page,
            actor_id,
            route,
        }
    }

    pub fn snapshot_hash(&self) -> EntryHash {
        self.snapshot_hash
    }
    pub fn current_page(&self) -> usize {
        self.current_page
    }
    pub fn actor_id(&self) -> MemberId {
        self.actor_id
    }
    pub fn route(&self) -> PagedReadViewRoute {
        self.route
    }

    pub fn require_snapshot(&self, expected: EntryHash) -> Result<(), PagedReadViewStateError> {
        if self.snapshot_hash == expected {
            Ok(())
        } else {
            Err(PagedReadViewStateError::StaleSnapshot {
                actual: self.snapshot_hash,
                expected,
            })
        }
    }

    pub fn require_actor(&self, expected: MemberId) -> Result<(), PagedReadViewStateError> {
        if self.actor_id == expected {
            Ok(())
        } else {
            Err(PagedReadViewStateError::ActorMismatch {
                actual: self.actor_id,
                expected,
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum SessionAccessError {
    #[error("session has expired")]
    Expired,
    #[error("stale nonce: observed {actual:?}, expected {expected:?}")]
    StaleNonce {
        actual: InteractionNonce,
        expected: InteractionNonce,
    },
    #[error("session has been superseded: observed {actual:?}, expected {expected:?}")]
    Superseded {
        actual: InteractionNonce,
        expected: InteractionNonce,
    },
}

pub struct ExpenseSessionStore {
    by_key: Mutex<HashMap<ExpenseSessionKey, ExpenseSession>>,
    locks: DashMap<ExpenseSessionKey, Arc<AsyncMutex<()>>>,
}

impl Default for ExpenseSessionStore {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpenseSessionStore {
    pub fn new() -> Self {
        Self {
            by_key: Mutex::new(HashMap::new()),
            locks: DashMap::new(),
        }
    }

    fn lock_for(&self, key: ExpenseSessionKey) -> Arc<AsyncMutex<()>> {
        self.locks
            .entry(key)
            .or_insert_with(|| Arc::new(AsyncMutex::new(())))
            .clone()
    }

    pub fn replace(&self, session: ExpenseSession) -> Option<ExpenseSession> {
        self.by_key
            .lock()
            .expect("ExpenseSessionStore mutex poisoned")
            .insert(session.key(), session)
    }

    pub fn clear(&self, key: ExpenseSessionKey) -> Option<ExpenseSession> {
        self.by_key
            .lock()
            .expect("ExpenseSessionStore mutex poisoned")
            .remove(&key)
    }

    pub fn access(
        &self,
        key: ExpenseSessionKey,
        observed_nonce: InteractionNonce,
        now: SystemTime,
    ) -> Result<Option<ExpenseSession>, SessionAccessError> {
        let mut guard = self
            .by_key
            .lock()
            .expect("ExpenseSessionStore mutex poisoned");
        let Some(session) = guard.get(&key).cloned() else {
            return Ok(None);
        };
        let elapsed = now.duration_since(session.last_touched).unwrap_or_default();
        if elapsed >= EXPENSE_SESSION_TTL {
            guard.remove(&key);
            return Err(SessionAccessError::Expired);
        }
        if session.nonce != observed_nonce {
            return Err(SessionAccessError::Superseded {
                actual: observed_nonce,
                expected: session.nonce,
            });
        }
        Ok(Some(session))
    }

    /// Atomic read-validate-mutate-write for a single session key. The per-session
    /// mutex is held across the entire operation, so concurrent interactions targeting
    /// the same key cannot race; nonce and TTL are revalidated inside the lock so any
    /// supersede or expiry observed by a concurrent path takes precedence.
    pub async fn with_session_mutation<R>(
        &self,
        key: ExpenseSessionKey,
        observed_nonce: InteractionNonce,
        now: SystemTime,
        mutate: impl FnOnce(&mut ExpenseSession) -> R,
    ) -> Result<Option<R>, SessionAccessError> {
        let lock = self.lock_for(key);
        let _guard = lock.lock().await;

        let mut session = {
            let mut map = self
                .by_key
                .lock()
                .expect("ExpenseSessionStore mutex poisoned");
            let Some(existing) = map.get(&key) else {
                return Ok(None);
            };
            let elapsed = now
                .duration_since(existing.last_touched)
                .unwrap_or_default();
            if elapsed >= EXPENSE_SESSION_TTL {
                map.remove(&key);
                return Err(SessionAccessError::Expired);
            }
            if existing.nonce != observed_nonce {
                return Err(SessionAccessError::Superseded {
                    actual: observed_nonce,
                    expected: existing.nonce,
                });
            }
            existing.clone()
        };

        let result = mutate(&mut session);

        self.by_key
            .lock()
            .expect("ExpenseSessionStore mutex poisoned")
            .insert(key, session);

        Ok(Some(result))
    }
}

pub struct VoidSessionStore {
    by_key: Mutex<HashMap<VoidSessionKey, VoidSession>>,
    locks: DashMap<VoidSessionKey, Arc<AsyncMutex<()>>>,
}

impl Default for VoidSessionStore {
    fn default() -> Self {
        Self::new()
    }
}

impl VoidSessionStore {
    pub fn new() -> Self {
        Self {
            by_key: Mutex::new(HashMap::new()),
            locks: DashMap::new(),
        }
    }

    fn lock_for(&self, key: VoidSessionKey) -> Arc<AsyncMutex<()>> {
        self.locks
            .entry(key)
            .or_insert_with(|| Arc::new(AsyncMutex::new(())))
            .clone()
    }

    pub fn replace(&self, session: VoidSession) -> Option<VoidSession> {
        self.by_key
            .lock()
            .expect("VoidSessionStore mutex poisoned")
            .insert(session.key(), session)
    }

    pub fn clear(&self, key: VoidSessionKey) -> Option<VoidSession> {
        self.by_key
            .lock()
            .expect("VoidSessionStore mutex poisoned")
            .remove(&key)
    }

    pub fn access(
        &self,
        key: VoidSessionKey,
        observed_nonce: InteractionNonce,
        now: SystemTime,
    ) -> Result<Option<VoidSession>, SessionAccessError> {
        let mut guard = self.by_key.lock().expect("VoidSessionStore mutex poisoned");
        let Some(session) = guard.get(&key).cloned() else {
            return Ok(None);
        };
        let elapsed = now.duration_since(session.last_touched).unwrap_or_default();
        if elapsed >= VOID_SESSION_TTL {
            guard.remove(&key);
            return Err(SessionAccessError::Expired);
        }
        if session.nonce != observed_nonce {
            return Err(SessionAccessError::Superseded {
                actual: observed_nonce,
                expected: session.nonce,
            });
        }
        Ok(Some(session))
    }

    pub async fn with_session_mutation<R>(
        &self,
        key: VoidSessionKey,
        observed_nonce: InteractionNonce,
        now: SystemTime,
        mutate: impl FnOnce(&mut VoidSession) -> R,
    ) -> Result<Option<R>, SessionAccessError> {
        let lock = self.lock_for(key);
        let _guard = lock.lock().await;

        let mut session = {
            let mut map = self.by_key.lock().expect("VoidSessionStore mutex poisoned");
            let Some(existing) = map.get(&key) else {
                return Ok(None);
            };
            let elapsed = now
                .duration_since(existing.last_touched)
                .unwrap_or_default();
            if elapsed >= VOID_SESSION_TTL {
                map.remove(&key);
                return Err(SessionAccessError::Expired);
            }
            if existing.nonce != observed_nonce {
                return Err(SessionAccessError::Superseded {
                    actual: observed_nonce,
                    expected: existing.nonce,
                });
            }
            existing.clone()
        };

        let result = mutate(&mut session);

        self.by_key
            .lock()
            .expect("VoidSessionStore mutex poisoned")
            .insert(key, session);

        Ok(Some(result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{LedgerId, ledger_chain_genesis_sha256_v1};
    use rstest::rstest;
    use std::time::UNIX_EPOCH;

    fn nonce(value: u64) -> InteractionNonce {
        InteractionNonce::new(value).expect("nonce non-zero")
    }

    fn expense_key() -> ExpenseSessionKey {
        ExpenseSessionKey::new(LedgerId(42), MemberId(3))
    }

    fn void_key() -> VoidSessionKey {
        VoidSessionKey::new(LedgerId(42), MemberId(3))
    }

    fn entry_hash() -> EntryHash {
        ledger_chain_genesis_sha256_v1(LedgerId(77))
    }

    fn basic_info() -> ExpenseBasicInfo {
        ExpenseBasicInfo {
            amount: Money::from_i64(1000),
            note: Some(ExpenseNote::new("ランチ").expect("note")),
            effective_date: LedgerEffectiveDate::new("2026-05-01").expect("date"),
        }
    }

    fn confirmation_snapshot() -> ExpenseConfirmationSnapshot {
        ExpenseConfirmationSnapshot {
            participants: vec![ExpenseParticipantSelection {
                member_id: MemberId(2),
                weight: Weight(1),
            }],
        }
    }

    fn draft_with_basic_info() -> ExpenseDraftSnapshot {
        ExpenseDraftSnapshot::empty().with_basic_info(basic_info())
    }

    fn draft_with_basic_info_and_confirmation() -> ExpenseDraftSnapshot {
        ExpenseDraftSnapshot::empty()
            .with_basic_info(basic_info())
            .with_confirmation_snapshot(confirmation_snapshot())
    }

    #[rstest]
    #[case::awaiting_with_basic_info_rejected(
        ExpenseSessionStage::AwaitingBasicInfo,
        draft_with_basic_info(),
        Err(ExpenseSessionConstructionError::AwaitingBasicInfoCannotHaveBasicInfo)
    )]
    #[case::in_selection_without_basic_info_rejected(
        ExpenseSessionStage::InSelection { phase: ExpenseSelectionPhase::Payer },
        ExpenseDraftSnapshot::empty(),
        Err(ExpenseSessionConstructionError::InSelectionRequiresBasicInfo),
    )]
    #[case::in_confirmation_without_snapshot_rejected(
        ExpenseSessionStage::InConfirmation,
        draft_with_basic_info(),
        Err(ExpenseSessionConstructionError::InConfirmationRequiresBasicInfoAndSelection)
    )]
    #[case::awaiting_with_empty_draft_ok(
        ExpenseSessionStage::AwaitingBasicInfo,
        ExpenseDraftSnapshot::empty(),
        Ok(()),
    )]
    #[case::in_selection_with_basic_info_ok(
        ExpenseSessionStage::InSelection { phase: ExpenseSelectionPhase::WeightEditor },
        draft_with_basic_info(),
        Ok(()),
    )]
    #[case::in_confirmation_with_snapshot_ok(
        ExpenseSessionStage::InConfirmation,
        draft_with_basic_info_and_confirmation(),
        Ok(()),
    )]
    fn expense_session_constructor_enforces_stage_draft_invariants(
        #[case] stage: ExpenseSessionStage,
        #[case] draft: ExpenseDraftSnapshot,
        #[case] expected: Result<(), ExpenseSessionConstructionError>,
    ) {
        let actual =
            ExpenseSession::new(expense_key(), stage, draft, nonce(1), UNIX_EPOCH).map(|_| ());
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::confirming_without_selection_rejected(
        VoidSessionStage::Confirming,
        None,
        Err(VoidSessionConstructionError::ConfirmingRequiresCandidate)
    )]
    #[case::confirming_with_selection_ok(
        VoidSessionStage::Confirming,
        Some(VoidCandidateSelection::new(LedgerEntryId(42))),
        Ok(()),
    )]
    #[case::selecting_without_selection_ok(
        VoidSessionStage::SelectingCandidate,
        None,
        Ok(()),
    )]
    fn void_session_constructor_enforces_selection_invariant(
        #[case] stage: VoidSessionStage,
        #[case] selection: Option<VoidCandidateSelection>,
        #[case] expected: Result<(), VoidSessionConstructionError>,
    ) {
        let actual =
            VoidSession::new(void_key(), stage, selection, nonce(1), UNIX_EPOCH).map(|_| ());
        assert_eq!(actual, expected);
    }

    fn fresh_expense_session(now: SystemTime, nonce_value: u64) -> ExpenseSession {
        ExpenseSession::new(
            expense_key(),
            ExpenseSessionStage::AwaitingBasicInfo,
            ExpenseDraftSnapshot::empty(),
            nonce(nonce_value),
            now,
        )
        .expect("session should construct")
    }

    #[test]
    fn expense_session_store_replace_returns_previous_session() {
        let store = ExpenseSessionStore::new();
        let first = fresh_expense_session(UNIX_EPOCH, 1);
        let second = fresh_expense_session(UNIX_EPOCH, 2);

        let prior_after_first = store.replace(first.clone());
        let prior_after_second = store.replace(second);

        assert_eq!(prior_after_first, None);
        assert_eq!(prior_after_second, Some(first));
    }

    #[test]
    fn expense_session_access_returns_none_when_missing() {
        let store = ExpenseSessionStore::new();
        let actual = store.access(expense_key(), nonce(1), UNIX_EPOCH);
        assert_eq!(actual, Ok(None));
    }

    #[rstest]
    #[case::within_ttl(
        UNIX_EPOCH,
        UNIX_EPOCH + Duration::from_secs(599),
        nonce(1),
        Ok(()),
    )]
    #[case::at_exact_ttl_boundary_expires(
        UNIX_EPOCH,
        UNIX_EPOCH + EXPENSE_SESSION_TTL,
        nonce(1),
        Err(SessionAccessError::Expired),
    )]
    #[case::superseded_when_observed_nonce_differs(
        UNIX_EPOCH,
        UNIX_EPOCH + Duration::from_secs(60),
        nonce(99),
        Err(SessionAccessError::Superseded { actual: nonce(99), expected: nonce(1) }),
    )]
    fn expense_session_access_enforces_ttl_and_nonce(
        #[case] last_touched: SystemTime,
        #[case] now: SystemTime,
        #[case] observed_nonce: InteractionNonce,
        #[case] expected: Result<(), SessionAccessError>,
    ) {
        let store = ExpenseSessionStore::new();
        store.replace(fresh_expense_session(last_touched, 1));

        let actual = store
            .access(expense_key(), observed_nonce, now)
            .map(|maybe| {
                assert!(maybe.is_some(), "should return session on Ok");
            });

        assert_eq!(actual, expected);
    }

    #[test]
    fn expense_session_access_reports_superseded_after_replace_keeps_new_session_in_place() {
        let store = ExpenseSessionStore::new();
        let first = fresh_expense_session(UNIX_EPOCH, 1);
        let second = fresh_expense_session(UNIX_EPOCH, 2);
        store.replace(first.clone());
        store.replace(second.clone());

        let observed_with_old_nonce = store.access(
            expense_key(),
            first.nonce(),
            UNIX_EPOCH + Duration::from_secs(60),
        );
        let observed_with_new_nonce = store.access(
            expense_key(),
            second.nonce(),
            UNIX_EPOCH + Duration::from_secs(60),
        );

        assert_eq!(
            observed_with_old_nonce,
            Err(SessionAccessError::Superseded {
                actual: first.nonce(),
                expected: second.nonce(),
            })
        );
        assert_eq!(observed_with_new_nonce, Ok(Some(second)));
    }

    #[test]
    fn void_session_access_reports_superseded_after_replace_keeps_new_session_in_place() {
        let store = VoidSessionStore::new();
        let first = VoidSession::new(
            void_key(),
            VoidSessionStage::SelectingCandidate,
            None,
            nonce(1),
            UNIX_EPOCH,
        )
        .expect("first session");
        let second = VoidSession::new(
            void_key(),
            VoidSessionStage::SelectingCandidate,
            None,
            nonce(2),
            UNIX_EPOCH,
        )
        .expect("second session");
        store.replace(first.clone());
        store.replace(second.clone());

        let observed_with_old_nonce = store.access(
            void_key(),
            first.nonce(),
            UNIX_EPOCH + Duration::from_secs(60),
        );

        assert_eq!(
            observed_with_old_nonce,
            Err(SessionAccessError::Superseded {
                actual: first.nonce(),
                expected: second.nonce(),
            })
        );
    }

    #[test]
    fn expense_session_access_clears_expired_session_inline() {
        let store = ExpenseSessionStore::new();
        store.replace(fresh_expense_session(UNIX_EPOCH, 1));

        let _ = store.access(expense_key(), nonce(1), UNIX_EPOCH + EXPENSE_SESSION_TTL);

        let after = store.access(
            expense_key(),
            nonce(1),
            UNIX_EPOCH + EXPENSE_SESSION_TTL + Duration::from_secs(1),
        );
        assert_eq!(after, Ok(None));
    }

    #[tokio::test]
    async fn with_session_mutation_persists_changes_under_per_key_lock() {
        let store = Arc::new(ExpenseSessionStore::new());
        store.replace(fresh_expense_session(UNIX_EPOCH, 1));

        store
            .with_session_mutation(
                expense_key(),
                nonce(1),
                UNIX_EPOCH + Duration::from_secs(30),
                |session| {
                    session.last_touched = UNIX_EPOCH + Duration::from_secs(30);
                },
            )
            .await
            .expect("mutation should succeed");

        let updated = store.access(
            expense_key(),
            nonce(1),
            UNIX_EPOCH + Duration::from_secs(60),
        );
        assert_eq!(
            updated.map(|maybe| maybe.map(|session| session.last_touched())),
            Ok(Some(UNIX_EPOCH + Duration::from_secs(30)))
        );
    }

    #[tokio::test]
    async fn with_session_mutation_rejects_superseded_observed_nonce() {
        let store = ExpenseSessionStore::new();
        store.replace(fresh_expense_session(UNIX_EPOCH, 1));

        let actual = store
            .with_session_mutation(
                expense_key(),
                nonce(99),
                UNIX_EPOCH + Duration::from_secs(30),
                |_| (),
            )
            .await;

        assert_eq!(
            actual,
            Err(SessionAccessError::Superseded {
                actual: nonce(99),
                expected: nonce(1),
            })
        );
    }

    #[tokio::test]
    async fn with_session_mutation_serializes_concurrent_writes_on_same_key() {
        let store = Arc::new(ExpenseSessionStore::new());
        store.replace(fresh_expense_session(UNIX_EPOCH, 1));

        let (gate_tx_a, gate_rx_a) = tokio::sync::oneshot::channel();
        let (gate_tx_b, gate_rx_b) = tokio::sync::oneshot::channel();

        let store_a = Arc::clone(&store);
        let task_a = tokio::spawn(async move {
            store_a
                .with_session_mutation(
                    expense_key(),
                    nonce(1),
                    UNIX_EPOCH + Duration::from_secs(10),
                    move |session| {
                        // Signal that we acquired the lock, then yield until B observes.
                        let _ = gate_tx_a.send(());
                        std::thread::sleep(Duration::from_millis(20));
                        session.last_touched = UNIX_EPOCH + Duration::from_secs(10);
                    },
                )
                .await
        });

        let _ = gate_rx_a.await;

        let store_b = Arc::clone(&store);
        let task_b = tokio::spawn(async move {
            // Should block until A releases the lock; after A commits its mutation,
            // B revalidates and sees the persisted state from A.
            let result = store_b
                .with_session_mutation(
                    expense_key(),
                    nonce(1),
                    UNIX_EPOCH + Duration::from_secs(20),
                    move |session| {
                        let _ = gate_tx_b.send(session.last_touched);
                        session.last_touched = UNIX_EPOCH + Duration::from_secs(20);
                    },
                )
                .await;
            (result, gate_rx_b.await.expect("gate b should receive"))
        });

        let a_result = task_a.await.expect("task a should join");
        let (b_result, b_observed_before) = task_b.await.expect("task b should join");

        assert!(a_result.is_ok());
        assert!(b_result.is_ok());
        assert_eq!(b_observed_before, UNIX_EPOCH + Duration::from_secs(10));
    }

    fn preserved() -> ModalRetryPreserved {
        ModalRetryPreserved {
            raw_amount: "1000".to_owned(),
            raw_note: "ランチ".to_owned(),
            raw_date: "2026-05-29".to_owned(),
        }
    }

    const RETRY_LEDGER_ID: LedgerId = LedgerId(42);

    #[test]
    fn modal_retry_binding_expires_at_is_creation_plus_ten_minutes() {
        let binding = ModalRetryBinding::capture(
            nonce(1),
            MemberId(3),
            RETRY_LEDGER_ID,
            preserved(),
            UNIX_EPOCH,
        );
        assert_eq!(binding.expires_at(), UNIX_EPOCH + MODAL_RETRY_TTL);
    }

    fn fresh_modal_retry_store() -> ModalRetryBindingStore {
        let store = ModalRetryBindingStore::new();
        store.store(ModalRetryBinding::capture(
            nonce(1),
            MemberId(3),
            RETRY_LEDGER_ID,
            preserved(),
            UNIX_EPOCH,
        ));
        store
    }

    #[rstest]
    #[case::within_ttl(UNIX_EPOCH + Duration::from_secs(599), Ok(preserved()))]
    #[case::at_expiry_boundary(
        UNIX_EPOCH + MODAL_RETRY_TTL,
        Err(ModalRetryBindingError::Expired {
            now: UNIX_EPOCH + MODAL_RETRY_TTL,
            expires_at: UNIX_EPOCH + MODAL_RETRY_TTL,
        }),
    )]
    fn modal_retry_store_try_consume_enforces_lifetime(
        #[case] now: SystemTime,
        #[case] expected: Result<ModalRetryPreserved, ModalRetryBindingError>,
    ) {
        let store = fresh_modal_retry_store();
        let actual = store.try_consume(nonce(1), MemberId(3), RETRY_LEDGER_ID, now);
        assert_eq!(actual, expected);
    }

    #[test]
    fn modal_retry_store_try_consume_is_structurally_single_use() {
        let store = fresh_modal_retry_store();

        let first = store.try_consume(
            nonce(1),
            MemberId(3),
            RETRY_LEDGER_ID,
            UNIX_EPOCH + Duration::from_secs(1),
        );
        let second = store.try_consume(
            nonce(1),
            MemberId(3),
            RETRY_LEDGER_ID,
            UNIX_EPOCH + Duration::from_secs(2),
        );

        assert_eq!(first, Ok(preserved()));
        assert_eq!(second, Err(ModalRetryBindingError::NotFound));
    }

    #[rstest]
    #[case::wrong_actor(
        MemberId(999),
        RETRY_LEDGER_ID,
        Err(ModalRetryBindingError::ActorMismatch { actual: MemberId(999), expected: MemberId(3) }),
    )]
    #[case::wrong_ledger(
        MemberId(3),
        LedgerId(999),
        Err(ModalRetryBindingError::LedgerMismatch { actual: LedgerId(999), expected: RETRY_LEDGER_ID }),
    )]
    fn modal_retry_store_try_consume_enforces_actor_and_ledger(
        #[case] actor: MemberId,
        #[case] ledger_id: LedgerId,
        #[case] expected: Result<ModalRetryPreserved, ModalRetryBindingError>,
    ) {
        let store = fresh_modal_retry_store();

        let actual = store.try_consume(
            nonce(1),
            actor,
            ledger_id,
            UNIX_EPOCH + Duration::from_secs(60),
        );

        assert_eq!(actual, expected);
    }

    #[test]
    fn modal_retry_store_returns_not_found_when_binding_was_never_stored() {
        let store = ModalRetryBindingStore::new();
        let actual = store.try_consume(
            nonce(1),
            MemberId(3),
            RETRY_LEDGER_ID,
            UNIX_EPOCH + Duration::from_secs(1),
        );
        assert_eq!(actual, Err(ModalRetryBindingError::NotFound));
    }

    #[rstest]
    #[case::matching_snapshot(PickerSnapshotId::new(42), Ok(()))]
    #[case::stale_snapshot(
        PickerSnapshotId::new(99),
        Err(PagedPickerStateError::StaleSnapshot {
            actual: PickerSnapshotId::new(42),
            expected: PickerSnapshotId::new(99),
        }),
    )]
    fn paged_picker_state_require_snapshot_rejects_stale_clicks(
        #[case] expected_snapshot: PickerSnapshotId,
        #[case] expected: Result<(), PagedPickerStateError>,
    ) {
        let state = PagedPickerState::new(PickerSnapshotId::new(42), 0, None, vec![]);
        let actual = state.require_snapshot(expected_snapshot);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::matching_snapshot(entry_hash(), Ok(()))]
    #[case::stale_snapshot(
        ledger_chain_genesis_sha256_v1(LedgerId(99)),
        Err(PagedReadViewStateError::StaleSnapshot {
            actual: entry_hash(),
            expected: ledger_chain_genesis_sha256_v1(LedgerId(99)),
        }),
    )]
    fn paged_read_view_state_require_snapshot_rejects_stale_pages(
        #[case] expected_snapshot: EntryHash,
        #[case] expected: Result<(), PagedReadViewStateError>,
    ) {
        let state = PagedReadViewState::new(
            entry_hash(),
            0,
            MemberId(3),
            PagedReadViewRoute::ReviewThread,
        );
        let actual = state.require_snapshot(expected_snapshot);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::matching_actor(MemberId(3), Ok(()))]
    #[case::wrong_actor(
        MemberId(99),
        Err(PagedReadViewStateError::ActorMismatch { actual: MemberId(3), expected: MemberId(99) }),
    )]
    fn paged_read_view_state_require_actor_rejects_cross_actor_clicks(
        #[case] expected_actor: MemberId,
        #[case] expected: Result<(), PagedReadViewStateError>,
    ) {
        let state = PagedReadViewState::new(
            entry_hash(),
            0,
            MemberId(3),
            PagedReadViewRoute::ReviewThread,
        );
        let actual = state.require_actor(expected_actor);
        assert_eq!(actual, expected);
    }
}
