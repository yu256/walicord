use crate::{
    InteractionNonce,
    ledger::{EntryHash, ExpenseNote, LedgerEffectiveDate, LedgerEntryId, LedgerId},
};
use std::{
    collections::{BTreeMap, HashMap},
    num::NonZeroU64,
    sync::Mutex,
    time::{Duration, SystemTime},
};
use walicord_domain::{
    Money,
    model::{MemberId, RoleId, Weight},
};

pub const EXPENSE_SESSION_TTL: Duration = Duration::from_secs(10 * 60);
pub const VOID_SESSION_TTL: Duration = Duration::from_secs(10 * 60);
pub const MODAL_RETRY_TTL: Duration = Duration::from_secs(10 * 60);

/// Adapter-issued scope for an in-progress expense draft. A draft can exist before its
/// first canonical ledger thread, so this is intentionally distinct from [`LedgerId`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpenseDraftScopeId(NonZeroU64);

impl ExpenseDraftScopeId {
    pub fn new(value: u64) -> Result<Self, ExpenseDraftScopeIdError> {
        NonZeroU64::new(value)
            .map(Self)
            .ok_or(ExpenseDraftScopeIdError::Zero)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum ExpenseDraftScopeIdError {
    #[error("expense draft scope id must be non-zero")]
    Zero,
}

/// Identity for an in-progress expense draft.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpenseSessionKey {
    draft_scope_id: ExpenseDraftScopeId,
    actor_id: MemberId,
}

impl ExpenseSessionKey {
    pub fn new(draft_scope_id: ExpenseDraftScopeId, actor_id: MemberId) -> Self {
        Self {
            draft_scope_id,
            actor_id,
        }
    }

    pub fn draft_scope_id(self) -> ExpenseDraftScopeId {
        self.draft_scope_id
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpenseLaunchOrigin {
    SlashCommand,
    PanelButton,
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
    origin: ExpenseLaunchOrigin,
    stage: ExpenseSessionStage,
    draft: ExpenseDraftSnapshot,
    nonce: InteractionNonce,
    last_touched: SystemTime,
}

impl ExpenseSession {
    pub fn new(
        key: ExpenseSessionKey,
        origin: ExpenseLaunchOrigin,
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
                origin,
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
    pub fn origin(&self) -> ExpenseLaunchOrigin {
        self.origin
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpenseModalIntent {
    Create { origin: ExpenseLaunchOrigin },
    ModifyExisting { session_nonce: InteractionNonce },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseModalSubmissionBinding {
    binding_nonce: InteractionNonce,
    actor_id: MemberId,
    draft_scope_id: ExpenseDraftScopeId,
    intent: ExpenseModalIntent,
    expires_at: SystemTime,
}

impl ExpenseModalSubmissionBinding {
    pub fn capture(
        binding_nonce: InteractionNonce,
        actor_id: MemberId,
        draft_scope_id: ExpenseDraftScopeId,
        intent: ExpenseModalIntent,
        created_at: SystemTime,
    ) -> Self {
        Self {
            binding_nonce,
            actor_id,
            draft_scope_id,
            intent,
            expires_at: created_at + MODAL_RETRY_TTL,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ModalSubmissionBindingError {
    #[error("modal submission binding not found in store")]
    NotFound,
    #[error("modal submission binding expired")]
    Expired,
    #[error("modal submission actor mismatch")]
    ActorMismatch,
    #[error("modal submission draft scope mismatch")]
    DraftScopeMismatch,
}

pub struct ExpenseModalSubmissionBindingStore {
    by_nonce: Mutex<HashMap<InteractionNonce, ExpenseModalSubmissionBinding>>,
}

impl Default for ExpenseModalSubmissionBindingStore {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpenseModalSubmissionBindingStore {
    pub fn new() -> Self {
        Self {
            by_nonce: Mutex::new(HashMap::new()),
        }
    }

    pub fn store(&self, binding: ExpenseModalSubmissionBinding) {
        self.by_nonce
            .lock()
            .expect("ExpenseModalSubmissionBindingStore mutex poisoned")
            .insert(binding.binding_nonce, binding);
    }

    pub fn try_consume(
        &self,
        binding_nonce: InteractionNonce,
        actor_id: MemberId,
        draft_scope_id: ExpenseDraftScopeId,
        now: SystemTime,
    ) -> Result<ExpenseModalIntent, ModalSubmissionBindingError> {
        let mut guard = self
            .by_nonce
            .lock()
            .expect("ExpenseModalSubmissionBindingStore mutex poisoned");
        let Some(binding) = guard.get(&binding_nonce) else {
            return Err(ModalSubmissionBindingError::NotFound);
        };
        if now >= binding.expires_at {
            guard.remove(&binding_nonce);
            return Err(ModalSubmissionBindingError::Expired);
        }
        if binding.actor_id != actor_id {
            return Err(ModalSubmissionBindingError::ActorMismatch);
        }
        if binding.draft_scope_id != draft_scope_id {
            return Err(ModalSubmissionBindingError::DraftScopeMismatch);
        }
        Ok(guard
            .remove(&binding_nonce)
            .expect("binding was just observed under the same lock")
            .intent)
    }
}

/// Single-use retry binding for re-opening a modal with preserved values after a
/// validation failure. Single-use is enforced structurally by
/// [`ModalRetryBindingStore::try_consume`] removing the binding from the store on the
/// first successful consume — the type itself does not carry a `consumed` flag because
/// such a flag would be bypassable by cloning the binding outside the store.
///
/// Scope is bound to the expense draft independently of canonical ledger creation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModalRetryBinding {
    binding_nonce: InteractionNonce,
    actor_id: MemberId,
    draft_scope_id: ExpenseDraftScopeId,
    preserved: ModalRetryPreserved,
    intent: ExpenseModalIntent,
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
    #[error("modal retry draft scope mismatch: observed {actual:?}, expected {expected:?}")]
    DraftScopeMismatch {
        actual: ExpenseDraftScopeId,
        expected: ExpenseDraftScopeId,
    },
}

impl ModalRetryBinding {
    pub fn capture(
        binding_nonce: InteractionNonce,
        actor_id: MemberId,
        draft_scope_id: ExpenseDraftScopeId,
        preserved: ModalRetryPreserved,
        intent: ExpenseModalIntent,
        created_at: SystemTime,
    ) -> Self {
        Self {
            binding_nonce,
            actor_id,
            draft_scope_id,
            preserved,
            intent,
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
    pub fn draft_scope_id(&self) -> ExpenseDraftScopeId {
        self.draft_scope_id
    }
    pub fn preserved(&self) -> &ModalRetryPreserved {
        &self.preserved
    }
    pub fn intent(&self) -> ExpenseModalIntent {
        self.intent
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModalRetryPayload {
    pub preserved: ModalRetryPreserved,
    pub intent: ExpenseModalIntent,
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
        draft_scope_id: ExpenseDraftScopeId,
        now: SystemTime,
    ) -> Result<ModalRetryPayload, ModalRetryBindingError> {
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
        if binding.draft_scope_id != draft_scope_id {
            return Err(ModalRetryBindingError::DraftScopeMismatch {
                actual: draft_scope_id,
                expected: binding.draft_scope_id,
            });
        }
        let binding = guard
            .remove(&binding_nonce)
            .expect("binding was just observed under the same lock");
        Ok(ModalRetryPayload {
            preserved: binding.preserved,
            intent: binding.intent,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PickerSnapshotId(u64);

impl PickerSnapshotId {
    pub fn new(value: u64) -> Self {
        Self(value)
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
    #[error("session has been superseded: observed {actual:?}, expected {expected:?}")]
    Superseded {
        actual: InteractionNonce,
        expected: InteractionNonce,
    },
    #[error("session interaction is already in flight")]
    InFlight,
}

pub struct ExpenseSessionStore {
    state: Mutex<ExpenseSessionStoreState>,
}

struct ExpenseSessionStoreState {
    by_key: HashMap<ExpenseSessionKey, ExpenseSessionSlot>,
    next_claim_token: u64,
}

enum ExpenseSessionSlot {
    Available(ExpenseSession),
    Claimed {
        token: ExpenseSessionClaimToken,
        session: ExpenseSession,
    },
}

impl ExpenseSessionSlot {
    fn into_session(self) -> ExpenseSession {
        match self {
            Self::Available(session) | Self::Claimed { session, .. } => session,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExpenseSessionClaimToken {
    key: ExpenseSessionKey,
    value: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClaimedExpenseSession {
    token: ExpenseSessionClaimToken,
    session: ExpenseSession,
}

impl ClaimedExpenseSession {
    pub fn token(&self) -> ExpenseSessionClaimToken {
        self.token
    }

    pub fn session(&self) -> &ExpenseSession {
        &self.session
    }
}

impl Default for ExpenseSessionStore {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpenseSessionStore {
    pub fn new() -> Self {
        Self {
            state: Mutex::new(ExpenseSessionStoreState {
                by_key: HashMap::new(),
                next_claim_token: 1,
            }),
        }
    }

    pub fn replace(&self, session: ExpenseSession) -> Option<ExpenseSession> {
        self.state
            .lock()
            .expect("ExpenseSessionStore mutex poisoned")
            .by_key
            .insert(session.key(), ExpenseSessionSlot::Available(session))
            .map(ExpenseSessionSlot::into_session)
    }

    pub fn claim(
        &self,
        key: ExpenseSessionKey,
        observed_nonce: InteractionNonce,
        now: SystemTime,
    ) -> Result<Option<ClaimedExpenseSession>, SessionAccessError> {
        let mut guard = self
            .state
            .lock()
            .expect("ExpenseSessionStore mutex poisoned");
        let Some(slot) = guard.by_key.get(&key) else {
            return Ok(None);
        };
        let session = match slot {
            ExpenseSessionSlot::Available(session) => session.clone(),
            ExpenseSessionSlot::Claimed { .. } => return Err(SessionAccessError::InFlight),
        };
        let elapsed = now.duration_since(session.last_touched).unwrap_or_default();
        if elapsed >= EXPENSE_SESSION_TTL {
            guard.by_key.remove(&key);
            return Err(SessionAccessError::Expired);
        }
        if session.nonce != observed_nonce {
            return Err(SessionAccessError::Superseded {
                actual: observed_nonce,
                expected: session.nonce,
            });
        }
        let token = ExpenseSessionClaimToken {
            key,
            value: guard.next_claim_token,
        };
        guard.next_claim_token = guard
            .next_claim_token
            .checked_add(1)
            .expect("expense session claim token space exhausted");
        guard.by_key.insert(
            key,
            ExpenseSessionSlot::Claimed {
                token,
                session: session.clone(),
            },
        );
        Ok(Some(ClaimedExpenseSession { token, session }))
    }

    pub fn restore_claim(&self, claimed: ClaimedExpenseSession) -> bool {
        self.resolve_claim(claimed.token, Some(claimed.session))
    }

    pub fn resolve_claim(
        &self,
        token: ExpenseSessionClaimToken,
        replacement: Option<ExpenseSession>,
    ) -> bool {
        let mut guard = self
            .state
            .lock()
            .expect("ExpenseSessionStore mutex poisoned");
        let Some(ExpenseSessionSlot::Claimed {
            token: current_token,
            ..
        }) = guard.by_key.get(&token.key)
        else {
            return false;
        };
        if *current_token != token {
            return false;
        }
        match replacement {
            Some(session) => {
                assert_eq!(
                    session.key(),
                    token.key,
                    "expense session claim replacement key changed"
                );
                guard
                    .by_key
                    .insert(token.key, ExpenseSessionSlot::Available(session));
            }
            None => {
                guard.by_key.remove(&token.key);
            }
        }
        true
    }
}

pub struct VoidSessionStore {
    by_key: Mutex<HashMap<VoidSessionKey, VoidSession>>,
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
        }
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::ledger_chain_genesis_sha256_v1;
    use rstest::rstest;
    use std::time::UNIX_EPOCH;

    fn nonce(value: u64) -> InteractionNonce {
        InteractionNonce::new(value).expect("nonce non-zero")
    }

    #[test]
    fn expense_draft_scope_rejects_zero() {
        assert_eq!(
            ExpenseDraftScopeId::new(0),
            Err(ExpenseDraftScopeIdError::Zero)
        );
    }

    fn expense_key() -> ExpenseSessionKey {
        ExpenseSessionKey::new(draft_scope(42), MemberId(3))
    }

    fn draft_scope(value: u64) -> ExpenseDraftScopeId {
        ExpenseDraftScopeId::new(value).expect("draft scope should be non-zero")
    }

    fn void_key() -> VoidSessionKey {
        VoidSessionKey::new(walicord_ledger::test_fixtures::ledger_id(42), MemberId(3))
    }

    fn entry_hash() -> EntryHash {
        ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77))
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
        let actual = ExpenseSession::new(
            expense_key(),
            ExpenseLaunchOrigin::SlashCommand,
            stage,
            draft,
            nonce(1),
            UNIX_EPOCH,
        )
        .map(|_| ());
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
            ExpenseLaunchOrigin::SlashCommand,
            ExpenseSessionStage::AwaitingBasicInfo,
            ExpenseDraftSnapshot::empty(),
            nonce(nonce_value),
            now,
        )
        .expect("session should construct")
    }

    fn inspect_expense_session(
        store: &ExpenseSessionStore,
        observed_nonce: InteractionNonce,
        now: SystemTime,
    ) -> Result<Option<ExpenseSession>, SessionAccessError> {
        let claimed = store.claim(expense_key(), observed_nonce, now)?;
        Ok(claimed.map(|claimed| {
            let session = claimed.session().clone();
            store.restore_claim(claimed);
            session
        }))
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
        let actual = inspect_expense_session(&store, nonce(1), UNIX_EPOCH);
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

        let actual = inspect_expense_session(&store, observed_nonce, now).map(|maybe| {
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

        let observed_with_old_nonce =
            inspect_expense_session(&store, first.nonce(), UNIX_EPOCH + Duration::from_secs(60));
        let observed_with_new_nonce =
            inspect_expense_session(&store, second.nonce(), UNIX_EPOCH + Duration::from_secs(60));

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

        let _ = inspect_expense_session(&store, nonce(1), UNIX_EPOCH + EXPENSE_SESSION_TTL);

        let after = inspect_expense_session(
            &store,
            nonce(1),
            UNIX_EPOCH + EXPENSE_SESSION_TTL + Duration::from_secs(1),
        );
        assert_eq!(after, Ok(None));
    }

    #[test]
    fn expense_session_claim_blocks_concurrent_access() {
        let store = ExpenseSessionStore::new();
        let session = fresh_expense_session(UNIX_EPOCH, 1);
        store.replace(session.clone());

        let claimed = store.claim(
            expense_key(),
            nonce(1),
            UNIX_EPOCH + Duration::from_secs(60),
        );
        let after = inspect_expense_session(&store, nonce(1), UNIX_EPOCH + Duration::from_secs(60));

        assert_eq!(
            claimed.map(|maybe| maybe.map(|claimed| claimed.session().clone())),
            Ok(Some(session))
        );
        assert_eq!(after, Err(SessionAccessError::InFlight));
    }

    #[test]
    fn expense_session_claim_rejects_stale_nonce_without_removing_current_session() {
        let store = ExpenseSessionStore::new();
        let session = fresh_expense_session(UNIX_EPOCH, 1);
        store.replace(session.clone());

        let claimed = store.claim(
            expense_key(),
            nonce(99),
            UNIX_EPOCH + Duration::from_secs(60),
        );
        let after = inspect_expense_session(&store, nonce(1), UNIX_EPOCH + Duration::from_secs(60));

        assert_eq!(
            claimed,
            Err(SessionAccessError::Superseded {
                actual: nonce(99),
                expected: nonce(1),
            })
        );
        assert_eq!(after, Ok(Some(session)));
    }

    #[test]
    fn stale_claim_restore_does_not_overwrite_new_session() {
        let store = ExpenseSessionStore::new();
        let first = fresh_expense_session(UNIX_EPOCH, 1);
        let second = fresh_expense_session(UNIX_EPOCH, 2);
        store.replace(first.clone());
        let claimed = store
            .claim(expense_key(), first.nonce(), UNIX_EPOCH)
            .expect("claim should succeed")
            .expect("session should exist");
        store.replace(second.clone());

        store.restore_claim(claimed);

        assert_eq!(
            inspect_expense_session(&store, second.nonce(), UNIX_EPOCH),
            Ok(Some(second))
        );
    }

    #[test]
    fn stale_claim_resolution_does_not_overwrite_new_session() {
        let store = ExpenseSessionStore::new();
        let first = fresh_expense_session(UNIX_EPOCH, 1);
        let second = fresh_expense_session(UNIX_EPOCH, 2);
        let stale_replacement = fresh_expense_session(UNIX_EPOCH, 3);
        store.replace(first.clone());
        let claimed = store
            .claim(expense_key(), first.nonce(), UNIX_EPOCH)
            .expect("claim should succeed")
            .expect("session should exist");
        store.replace(second.clone());

        store.resolve_claim(claimed.token(), Some(stale_replacement));

        assert_eq!(
            inspect_expense_session(&store, second.nonce(), UNIX_EPOCH),
            Ok(Some(second))
        );
    }

    fn preserved() -> ModalRetryPreserved {
        ModalRetryPreserved {
            raw_amount: "1000".to_owned(),
            raw_note: "ランチ".to_owned(),
            raw_date: "2026-05-29".to_owned(),
        }
    }

    fn create_from_panel() -> ExpenseModalIntent {
        ExpenseModalIntent::Create {
            origin: ExpenseLaunchOrigin::PanelButton,
        }
    }

    fn retry_payload() -> ModalRetryPayload {
        ModalRetryPayload {
            preserved: preserved(),
            intent: create_from_panel(),
        }
    }

    #[test]
    fn modal_retry_binding_expires_at_is_creation_plus_ten_minutes() {
        let binding = ModalRetryBinding::capture(
            nonce(1),
            MemberId(3),
            draft_scope(42),
            preserved(),
            create_from_panel(),
            UNIX_EPOCH,
        );
        assert_eq!(binding.expires_at(), UNIX_EPOCH + MODAL_RETRY_TTL);
    }

    fn fresh_modal_retry_store() -> ModalRetryBindingStore {
        let store = ModalRetryBindingStore::new();
        store.store(ModalRetryBinding::capture(
            nonce(1),
            MemberId(3),
            draft_scope(42),
            preserved(),
            create_from_panel(),
            UNIX_EPOCH,
        ));
        store
    }

    #[rstest]
    #[case::within_ttl(UNIX_EPOCH + Duration::from_secs(599), Ok(retry_payload()))]
    #[case::at_expiry_boundary(
        UNIX_EPOCH + MODAL_RETRY_TTL,
        Err(ModalRetryBindingError::Expired {
            now: UNIX_EPOCH + MODAL_RETRY_TTL,
            expires_at: UNIX_EPOCH + MODAL_RETRY_TTL,
        }),
    )]
    fn modal_retry_store_try_consume_enforces_lifetime(
        #[case] now: SystemTime,
        #[case] expected: Result<ModalRetryPayload, ModalRetryBindingError>,
    ) {
        let store = fresh_modal_retry_store();
        let actual = store.try_consume(nonce(1), MemberId(3), draft_scope(42), now);
        assert_eq!(actual, expected);
    }

    #[test]
    fn modal_retry_store_try_consume_is_structurally_single_use() {
        let store = fresh_modal_retry_store();

        let first = store.try_consume(
            nonce(1),
            MemberId(3),
            draft_scope(42),
            UNIX_EPOCH + Duration::from_secs(1),
        );
        let second = store.try_consume(
            nonce(1),
            MemberId(3),
            draft_scope(42),
            UNIX_EPOCH + Duration::from_secs(2),
        );

        assert_eq!(first, Ok(retry_payload()));
        assert_eq!(second, Err(ModalRetryBindingError::NotFound));
    }

    #[rstest]
    #[case::wrong_actor(
        MemberId(999),
        draft_scope(42),
        Err(ModalRetryBindingError::ActorMismatch { actual: MemberId(999), expected: MemberId(3) }),
    )]
    #[case::wrong_draft_scope(
        MemberId(3),
        draft_scope(999),
        Err(ModalRetryBindingError::DraftScopeMismatch { actual: draft_scope(999), expected: draft_scope(42) }),
    )]
    fn modal_retry_store_try_consume_enforces_actor_and_draft_scope(
        #[case] actor: MemberId,
        #[case] draft_scope_id: ExpenseDraftScopeId,
        #[case] expected: Result<ModalRetryPayload, ModalRetryBindingError>,
    ) {
        let store = fresh_modal_retry_store();

        let actual = store.try_consume(
            nonce(1),
            actor,
            draft_scope_id,
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
            draft_scope(42),
            UNIX_EPOCH + Duration::from_secs(1),
        );
        assert_eq!(actual, Err(ModalRetryBindingError::NotFound));
    }

    #[test]
    fn modal_submission_binding_is_structurally_single_use() {
        let store = ExpenseModalSubmissionBindingStore::new();
        store.store(ExpenseModalSubmissionBinding::capture(
            nonce(1),
            MemberId(3),
            draft_scope(42),
            create_from_panel(),
            UNIX_EPOCH,
        ));

        let first = store.try_consume(
            nonce(1),
            MemberId(3),
            draft_scope(42),
            UNIX_EPOCH + Duration::from_secs(1),
        );
        let second = store.try_consume(
            nonce(1),
            MemberId(3),
            draft_scope(42),
            UNIX_EPOCH + Duration::from_secs(2),
        );

        assert_eq!(first, Ok(create_from_panel()));
        assert_eq!(second, Err(ModalSubmissionBindingError::NotFound));
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
        ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(99)),
        Err(PagedReadViewStateError::StaleSnapshot {
            actual: entry_hash(),
            expected: ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(99)),
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
