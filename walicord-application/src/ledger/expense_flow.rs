use super::expense_modal::ValidatedExpenseModalSubmission;
use crate::{
    Clock, InteractionNonce, NonceProvider,
    ledger::{
        expense_session::{
            ExpenseBasicInfo, ExpenseConfirmationSnapshot, ExpenseDraftSnapshot,
            ExpenseLaunchOrigin, ExpenseParticipantSelection, ExpensePickerKind,
            ExpenseSelectionPhase, ExpenseSelectionState, ExpenseSession,
            ExpenseSessionConstructionError, ExpenseSessionKey, ExpenseSessionStage,
            PagedPickerState, PickerSnapshotId,
        },
        participant_resolution::{
            ParticipantDrift, RosterSnapshot, drift_between_snapshot_and_resolution,
            resolve_selection_against_roster,
        },
    },
};
use std::collections::BTreeMap;
use walicord_domain::model::{MemberId, RoleId, Weight};

/// Construct a fresh [`ExpenseSession`] in `InSelection { Payer }` from a validated
/// modal submission. The actor is preselected as both payer and the initial individual
/// participant (criterion 144). Time is sourced from [`Clock`] and the session nonce
/// from [`NonceProvider`] so all tests can be made deterministic.
pub fn bootstrap_expense_session(
    key: ExpenseSessionKey,
    origin: ExpenseLaunchOrigin,
    validated_modal: ValidatedExpenseModalSubmission,
    clock: &dyn Clock,
    nonce_provider: &dyn NonceProvider,
) -> Result<(ExpenseSession, InteractionNonce), ExpenseSessionConstructionError> {
    let nonce = nonce_provider.next_interaction_nonce();
    let basic_info = ExpenseBasicInfo::from(validated_modal);
    let actor = key.actor_id();
    let selection_state = preselect_actor_as_payer_and_participant(actor);
    let draft = ExpenseDraftSnapshot::empty()
        .with_basic_info(basic_info)
        .with_selection_state(selection_state);
    let session = ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection {
            phase: ExpenseSelectionPhase::Payer,
        },
        draft,
        nonce,
        clock.now(),
    )?;
    Ok((session, nonce))
}

impl From<ValidatedExpenseModalSubmission> for ExpenseBasicInfo {
    fn from(validated: ValidatedExpenseModalSubmission) -> Self {
        Self {
            amount: validated.amount,
            note: validated.note,
            effective_date: validated.effective_date,
        }
    }
}

fn preselect_actor_as_payer_and_participant(actor: MemberId) -> ExpenseSelectionState {
    ExpenseSelectionState {
        payer: Some(actor),
        individual_members: vec![actor],
        ..ExpenseSelectionState::default()
    }
}

/// Outcome of resolving a session's selection state into a confirmation view. `drift`
/// is non-empty when the resolved participant set differs from the previous
/// confirmation snapshot (criterion 111); the caller surfaces those rows explicitly
/// and re-renders the confirmation page. `dropped_overrides` and `defaulted_members`
/// flow through from the resolution so the caller can update the session's stored
/// selection state, emit the criterion-158 observability event, and render the
/// criterion-216 `既定値 1` cue on affected rows.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConfirmationBuildOutcome {
    pub session: ExpenseSession,
    pub snapshot: ExpenseConfirmationSnapshot,
    pub drift: Vec<ParticipantDrift>,
    pub defaulted_members: Vec<MemberId>,
    pub dropped_overrides: Vec<(MemberId, walicord_domain::model::Weight)>,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ConfirmationBuildError {
    #[error("basic info is missing from the session draft")]
    BasicInfoMissing,
    #[error("no payer is selected in the session")]
    PayerNotSelected,
    #[error("no resolved participants after roster resolution")]
    NoResolvedParticipants,
    #[error("confirmation-stage session construction failed: {0}")]
    ConstructionFailed(#[from] ExpenseSessionConstructionError),
}

/// Resolve the current selection against a fresh roster snapshot and transition the
/// session into `InConfirmation` with the captured snapshot. Stale or removed
/// participants are dropped automatically (criterion 158); newly-added participants
/// default to `Weight(1)` (criterion 157); drift between an existing snapshot and the
/// new resolution is reported to the caller for re-render disclosure (criterion 111).
pub fn build_confirmation_for_session(
    session: ExpenseSession,
    roster: &RosterSnapshot,
    clock: &dyn Clock,
) -> Result<ConfirmationBuildOutcome, ConfirmationBuildError> {
    if session.draft().basic_info().is_none() {
        return Err(ConfirmationBuildError::BasicInfoMissing);
    }
    let mut selection = session.draft().selection_state().clone();
    if selection.payer.is_none() {
        return Err(ConfirmationBuildError::PayerNotSelected);
    }
    let outcome = resolve_selection_against_roster(&selection, roster);
    if outcome.resolved.is_empty() {
        return Err(ConfirmationBuildError::NoResolvedParticipants);
    }

    for (member_id, _) in &outcome.dropped_overrides {
        selection.weight_overrides.remove(member_id);
    }

    let drift = session
        .draft()
        .confirmation_snapshot()
        .map(|previous| drift_between_snapshot_and_resolution(previous, &outcome.resolved))
        .unwrap_or_default();

    let snapshot = ExpenseConfirmationSnapshot {
        participants: outcome.resolved.clone(),
    };
    let basic_info = session
        .draft()
        .basic_info()
        .cloned()
        .expect("basic info verified present above");
    let key = session.key();
    let origin = session.origin();
    let nonce = session.nonce();
    let next_draft = ExpenseDraftSnapshot::empty()
        .with_basic_info(basic_info)
        .with_selection_state(selection)
        .with_confirmation_snapshot(snapshot.clone());
    let next_session = ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InConfirmation,
        next_draft,
        nonce,
        clock.now(),
    )
    .map_err(ConfirmationBuildError::ConstructionFailed)?;

    Ok(ConfirmationBuildOutcome {
        session: next_session,
        snapshot,
        drift,
        defaulted_members: outcome.defaulted_members,
        dropped_overrides: outcome.dropped_overrides,
    })
}

/// Convenience: extract `(payer, participants)` from a session that is already in
/// `InConfirmation`. Used by the record path to feed the application authoring layer.
pub fn confirmation_payer_and_participants(
    session: &ExpenseSession,
) -> Option<(MemberId, &[ExpenseParticipantSelection])> {
    if !matches!(session.stage(), ExpenseSessionStage::InConfirmation) {
        return None;
    }
    let payer = session.draft().selection_state().payer?;
    let participants = session
        .draft()
        .confirmation_snapshot()?
        .participants
        .as_slice();
    Some((payer, participants))
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum NavigationError {
    #[error("already at the first selection step (caller should fall back to cancel)")]
    AlreadyAtFirstStep,
    #[error("session is not in the InSelection stage")]
    NotInSelection,
    #[error("session is not in the InConfirmation stage")]
    NotInConfirmation,
    #[error("basic info is missing from the session draft")]
    BasicInfoMissing,
    /// The actor pressed a forward-navigation button whose target phase is not a legal
    /// next step from the current phase (e.g. pressing `重みへ` from `Payer`). The
    /// session has not been mutated; the caller surfaces the criterion-201 cancel cue
    /// or refreshes the current step.
    #[error("illegal forward selection transition: {from:?} → {to:?}")]
    IllegalForwardTransition {
        from: ExpenseSelectionPhase,
        to: ExpenseSelectionPhase,
    },
    #[error("session construction failed during navigation: {0}")]
    ConstructionFailed(#[from] ExpenseSessionConstructionError),
}

/// Walk one selection phase backwards (criterion 200). At the first selection phase,
/// the caller falls back to the cancel path (criterion 201).
pub fn previous_phase(phase: &ExpenseSelectionPhase) -> Option<ExpenseSelectionPhase> {
    match phase {
        ExpenseSelectionPhase::Payer => None,
        ExpenseSelectionPhase::ParticipantSource => Some(ExpenseSelectionPhase::Payer),
        ExpenseSelectionPhase::IndividualSelection => {
            Some(ExpenseSelectionPhase::ParticipantSource)
        }
        ExpenseSelectionPhase::Roles => Some(ExpenseSelectionPhase::ParticipantSource),
        ExpenseSelectionPhase::WeightEditor => Some(ExpenseSelectionPhase::ParticipantSource),
    }
}

/// Move the session one phase back without touching draft state (criterion 200/202).
/// Returns `AlreadyAtFirstStep` when the caller should instead invoke the cancel path
/// (criterion 201).
pub fn navigate_back(
    session: ExpenseSession,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    let ExpenseSessionStage::InSelection { phase } = session.stage() else {
        return Err(NavigationError::NotInSelection);
    };
    let previous = previous_phase(phase).ok_or(NavigationError::AlreadyAtFirstStep)?;
    let key = session.key();
    let origin = session.origin();
    let nonce = session.nonce();
    let draft = session.draft().clone();
    ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection { phase: previous },
        draft,
        nonce,
        clock.now(),
    )
    .map_err(NavigationError::ConstructionFailed)
}

/// Transition a confirmation-stage session back to the first selection phase with
/// state preserved (criterion 145 / G17). The confirmation snapshot is dropped so the
/// next confirmation rebuild observes drift and refreshes participant rows.
pub fn navigate_modify_selection(
    session: ExpenseSession,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    if !matches!(session.stage(), ExpenseSessionStage::InConfirmation) {
        return Err(NavigationError::NotInConfirmation);
    }
    let key = session.key();
    let origin = session.origin();
    let nonce = session.nonce();
    let basic_info = session
        .draft()
        .basic_info()
        .cloned()
        .ok_or(NavigationError::BasicInfoMissing)?;
    let selection = session.draft().selection_state().clone();
    let draft = ExpenseDraftSnapshot::empty()
        .with_basic_info(basic_info)
        .with_selection_state(selection);
    ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection {
            phase: ExpenseSelectionPhase::Payer,
        },
        draft,
        nonce,
        clock.now(),
    )
    .map_err(NavigationError::ConstructionFailed)
}

/// Allowed forward transitions inside the selection wizard. Adding a `Members` toggle
/// is *not* a transition — it mutates the draft and stays on `ParticipantSource`.
pub fn legal_forward_target(phase: &ExpenseSelectionPhase) -> &'static [ExpenseSelectionPhase] {
    match phase {
        ExpenseSelectionPhase::Payer => &[ExpenseSelectionPhase::ParticipantSource],
        ExpenseSelectionPhase::ParticipantSource => &[
            ExpenseSelectionPhase::IndividualSelection,
            ExpenseSelectionPhase::Roles,
            ExpenseSelectionPhase::WeightEditor,
        ],
        ExpenseSelectionPhase::IndividualSelection => &[ExpenseSelectionPhase::WeightEditor],
        ExpenseSelectionPhase::Roles => &[ExpenseSelectionPhase::WeightEditor],
        ExpenseSelectionPhase::WeightEditor => &[],
    }
}

/// Forward-navigate one selection phase. Draft state (basic_info, selection_state) is
/// preserved verbatim; only the stage transitions. Illegal transitions return
/// `IllegalForwardTransition` so the caller can refresh the current step without
/// corrupting the session.
pub fn navigate_to_phase(
    session: ExpenseSession,
    target: ExpenseSelectionPhase,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    let ExpenseSessionStage::InSelection { phase } = session.stage() else {
        return Err(NavigationError::NotInSelection);
    };
    if !legal_forward_target(phase).contains(&target) {
        return Err(NavigationError::IllegalForwardTransition {
            from: phase.clone(),
            to: target,
        });
    }
    let key = session.key();
    let origin = session.origin();
    let nonce = session.nonce();
    let draft = session.draft().clone();
    ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection { phase: target },
        draft,
        nonce,
        clock.now(),
    )
    .map_err(NavigationError::ConstructionFailed)
}

/// Toggle the `MEMBERS` (全メンバー) virtual group in the current selection. Allowed
/// from the `ParticipantSource` phase only; the stage does not change. Per criterion
/// 214 the MEMBERS group is resolved at record time, so the toggle simply flips a flag
/// in the draft.
pub fn toggle_members_group(
    session: ExpenseSession,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    let ExpenseSessionStage::InSelection {
        phase: ExpenseSelectionPhase::ParticipantSource,
    } = session.stage()
    else {
        return Err(NavigationError::NotInSelection);
    };
    let key = session.key();
    let origin = session.origin();
    let nonce = session.nonce();
    let mut selection = session.draft().selection_state().clone();
    selection.include_members_group = !selection.include_members_group;
    let basic_info = session
        .draft()
        .basic_info()
        .cloned()
        .ok_or(NavigationError::BasicInfoMissing)?;
    let draft = ExpenseDraftSnapshot::empty()
        .with_basic_info(basic_info)
        .with_selection_state(selection);
    ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection {
            phase: ExpenseSelectionPhase::ParticipantSource,
        },
        draft,
        nonce,
        clock.now(),
    )
    .map_err(NavigationError::ConstructionFailed)
}

pub fn replace_payer(
    session: ExpenseSession,
    payer: Option<MemberId>,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    update_selection_in_phase(session, ExpenseSelectionPhase::Payer, clock, |selection| {
        selection.payer = payer;
    })
}

pub fn replace_individual_members(
    session: ExpenseSession,
    mut members: Vec<MemberId>,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    members.sort_unstable();
    members.dedup();
    update_selection_in_phase(
        session,
        ExpenseSelectionPhase::IndividualSelection,
        clock,
        |selection| {
            selection.individual_members = members;
        },
    )
}

pub fn replace_selected_roles(
    session: ExpenseSession,
    mut roles: Vec<RoleId>,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    roles.sort_unstable();
    roles.dedup();
    update_selection_in_phase(session, ExpenseSelectionPhase::Roles, clock, |selection| {
        selection.selected_roles = roles;
    })
}

pub fn replace_weight_overrides(
    session: ExpenseSession,
    weights: BTreeMap<MemberId, Weight>,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    update_selection_in_phase(
        session,
        ExpenseSelectionPhase::WeightEditor,
        clock,
        |selection| {
            selection.weight_overrides = weights;
        },
    )
}

pub fn set_picker_view_state(
    session: ExpenseSession,
    kind: ExpensePickerKind,
    snapshot_id: PickerSnapshotId,
    current_page: usize,
    query: Option<String>,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    update_selection_in_picker_kind(session, kind, clock, |selection| {
        let selected_values = picker_selection_values(kind, selection);
        selection.picker_states.insert(
            kind,
            PagedPickerState::new(snapshot_id, current_page, query, selected_values),
        );
    })
}

pub fn clear_picker_selection(
    session: ExpenseSession,
    kind: ExpensePickerKind,
    snapshot_id: PickerSnapshotId,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    update_selection_in_picker_kind(session, kind, clock, |selection| {
        match kind {
            ExpensePickerKind::Payer => selection.payer = None,
            ExpensePickerKind::Individuals => selection.individual_members.clear(),
            ExpensePickerKind::Roles => selection.selected_roles.clear(),
        }
        selection.picker_states.insert(
            kind,
            PagedPickerState::new(snapshot_id, 0, None, Vec::new()),
        );
    })
}

fn update_selection_in_picker_kind(
    session: ExpenseSession,
    kind: ExpensePickerKind,
    clock: &dyn Clock,
    update: impl FnOnce(&mut ExpenseSelectionState),
) -> Result<ExpenseSession, NavigationError> {
    update_selection_in_phase(session, picker_phase(kind), clock, update)
}

fn picker_phase(kind: ExpensePickerKind) -> ExpenseSelectionPhase {
    match kind {
        ExpensePickerKind::Payer => ExpenseSelectionPhase::Payer,
        ExpensePickerKind::Individuals => ExpenseSelectionPhase::IndividualSelection,
        ExpensePickerKind::Roles => ExpenseSelectionPhase::Roles,
    }
}

fn picker_selection_values(kind: ExpensePickerKind, selection: &ExpenseSelectionState) -> Vec<u64> {
    match kind {
        ExpensePickerKind::Payer => selection
            .payer
            .map(|member_id| member_id.0)
            .into_iter()
            .collect(),
        ExpensePickerKind::Individuals => selection
            .individual_members
            .iter()
            .map(|member_id| member_id.0)
            .collect(),
        ExpensePickerKind::Roles => selection
            .selected_roles
            .iter()
            .map(|role_id| role_id.0)
            .collect(),
    }
}

fn update_selection_in_phase(
    session: ExpenseSession,
    expected_phase: ExpenseSelectionPhase,
    clock: &dyn Clock,
    update: impl FnOnce(&mut ExpenseSelectionState),
) -> Result<ExpenseSession, NavigationError> {
    let ExpenseSessionStage::InSelection { phase } = session.stage() else {
        return Err(NavigationError::NotInSelection);
    };
    if phase != &expected_phase {
        return Err(NavigationError::NotInSelection);
    }
    let key = session.key();
    let origin = session.origin();
    let nonce = session.nonce();
    let mut selection = session.draft().selection_state().clone();
    update(&mut selection);
    let basic_info = session
        .draft()
        .basic_info()
        .cloned()
        .ok_or(NavigationError::BasicInfoMissing)?;
    ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection {
            phase: expected_phase,
        },
        ExpenseDraftSnapshot::empty()
            .with_basic_info(basic_info)
            .with_selection_state(selection),
        nonce,
        clock.now(),
    )
    .map_err(NavigationError::ConstructionFailed)
}

/// Apply a re-edited basic-info submission to an existing session. Per criterion 229
/// the actor presses `基本情報を修正する`, the modal re-opens prefilled from the
/// current draft, and on submit the new values replace the old basic_info while
/// **selection state persists** (criterion 229: "draft / choices persist"). The
/// confirmation snapshot is dropped — basic_info edits invalidate the captured
/// amounts so the next confirmation rebuild observes drift and refreshes.
pub fn apply_modified_basic_info(
    session: ExpenseSession,
    new_basic_info: ExpenseBasicInfo,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    let key = session.key();
    let origin = session.origin();
    let nonce = session.nonce();
    let selection = session.draft().selection_state().clone();
    let stage = match session.stage() {
        ExpenseSessionStage::AwaitingBasicInfo
        | ExpenseSessionStage::InSelection {
            phase: ExpenseSelectionPhase::Payer,
        }
        | ExpenseSessionStage::InConfirmation => ExpenseSessionStage::InSelection {
            phase: ExpenseSelectionPhase::Payer,
        },
        ExpenseSessionStage::InSelection { phase } => ExpenseSessionStage::InSelection {
            phase: phase.clone(),
        },
    };
    let draft = ExpenseDraftSnapshot::empty()
        .with_basic_info(new_basic_info)
        .with_selection_state(selection);
    ExpenseSession::new(key, origin, stage, draft, nonce, clock.now())
        .map_err(NavigationError::ConstructionFailed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ledger::{
            ExpenseNote, LedgerEffectiveDate,
            expense_modal::{RawExpenseModalSubmission, validate_expense_modal_submission},
            expense_session::ExpenseDraftScopeId,
        },
        settle_up::PreviewInstanceId,
    };
    use std::{
        sync::atomic::{AtomicU64, Ordering},
        time::{Duration, SystemTime, UNIX_EPOCH},
    };
    use walicord_domain::Money;

    struct FixedClock {
        today: &'static str,
        now: SystemTime,
    }

    impl Clock for FixedClock {
        fn now(&self) -> SystemTime {
            self.now
        }
        fn today_business_date(&self) -> LedgerEffectiveDate {
            LedgerEffectiveDate::new(self.today).expect("test today")
        }
    }

    struct SequentialNonceProvider {
        nonce: AtomicU64,
        preview: AtomicU64,
    }

    impl NonceProvider for SequentialNonceProvider {
        fn next_interaction_nonce(&self) -> InteractionNonce {
            InteractionNonce::new(self.nonce.fetch_add(1, Ordering::SeqCst)).expect("non-zero")
        }
        fn next_preview_instance_id(&self) -> PreviewInstanceId {
            PreviewInstanceId::new(self.preview.fetch_add(1, Ordering::SeqCst)).expect("non-zero")
        }
    }

    fn nonce_provider() -> SequentialNonceProvider {
        SequentialNonceProvider {
            nonce: AtomicU64::new(1),
            preview: AtomicU64::new(1),
        }
    }

    fn fixed_clock() -> FixedClock {
        FixedClock {
            today: "2026-05-29",
            now: UNIX_EPOCH + Duration::from_secs(1_748_400_000),
        }
    }

    fn key() -> ExpenseSessionKey {
        ExpenseSessionKey::new(
            ExpenseDraftScopeId::new(42).expect("draft scope should be non-zero"),
            MemberId(42),
        )
    }

    #[test]
    fn bootstrap_creates_in_selection_session_with_basic_info_and_actor_preselected() {
        let raw = RawExpenseModalSubmission {
            raw_amount: "1500".to_owned(),
            raw_note: "ランチ".to_owned(),
            raw_date: "2026-05-01".to_owned(),
        };
        let validated =
            validate_expense_modal_submission(&raw, &fixed_clock()).expect("modal validates");

        let (session, nonce) = bootstrap_expense_session(
            key(),
            ExpenseLaunchOrigin::SlashCommand,
            validated,
            &fixed_clock(),
            &nonce_provider(),
        )
        .expect("bootstrap succeeds");

        assert_eq!(
            session.stage(),
            &ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::Payer
            }
        );
        let basic = session.draft().basic_info().expect("basic info present");
        assert_eq!(basic.amount, Money::from_i64(1500));
        assert_eq!(basic.note, Some(ExpenseNote::new("ランチ").unwrap()));
        assert_eq!(
            basic.effective_date,
            LedgerEffectiveDate::new("2026-05-01").unwrap()
        );
        let selection = session.draft().selection_state();
        assert_eq!(selection.payer, Some(MemberId(42)));
        assert_eq!(selection.individual_members, vec![MemberId(42)]);
        assert!(selection.selected_roles.is_empty());
        assert!(!selection.include_members_group);
        assert!(selection.weight_overrides.is_empty());
        assert_eq!(nonce, InteractionNonce::new(1).unwrap());
    }

    #[test]
    fn bootstrap_uses_clock_now_for_last_touched_and_nonce_provider_for_nonce() {
        let raw = RawExpenseModalSubmission {
            raw_amount: "100".to_owned(),
            raw_note: "".to_owned(),
            raw_date: "".to_owned(),
        };
        let validated = validate_expense_modal_submission(&raw, &fixed_clock()).unwrap();

        let (session, nonce) = bootstrap_expense_session(
            key(),
            ExpenseLaunchOrigin::SlashCommand,
            validated,
            &fixed_clock(),
            &nonce_provider(),
        )
        .expect("bootstrap");

        assert_eq!(session.nonce(), nonce);
        assert_eq!(session.last_touched(), fixed_clock().now);
    }

    #[test]
    fn bootstrap_drops_blank_note_so_basic_info_carries_no_note() {
        let raw = RawExpenseModalSubmission {
            raw_amount: "100".to_owned(),
            raw_note: "   ".to_owned(),
            raw_date: "2026-05-01".to_owned(),
        };
        let validated = validate_expense_modal_submission(&raw, &fixed_clock()).unwrap();

        let (session, _) = bootstrap_expense_session(
            key(),
            ExpenseLaunchOrigin::SlashCommand,
            validated,
            &fixed_clock(),
            &nonce_provider(),
        )
        .expect("bootstrap");

        assert_eq!(session.draft().basic_info().unwrap().note, None);
    }

    fn roster_with(members: &[u64]) -> RosterSnapshot {
        RosterSnapshot {
            all_members: members.iter().map(|id| MemberId(*id)).collect(),
            role_members: std::collections::BTreeMap::new(),
        }
    }

    fn bootstrapped() -> ExpenseSession {
        let raw = RawExpenseModalSubmission {
            raw_amount: "1000".to_owned(),
            raw_note: "ランチ".to_owned(),
            raw_date: "2026-05-01".to_owned(),
        };
        let validated = validate_expense_modal_submission(&raw, &fixed_clock()).unwrap();
        bootstrap_expense_session(
            key(),
            ExpenseLaunchOrigin::SlashCommand,
            validated,
            &fixed_clock(),
            &nonce_provider(),
        )
        .expect("bootstrap")
        .0
    }

    #[test]
    fn confirmation_build_transitions_session_to_in_confirmation_with_resolved_snapshot() {
        let session = bootstrapped();
        let roster = roster_with(&[42]);

        let outcome = build_confirmation_for_session(session, &roster, &fixed_clock())
            .expect("confirmation builds");

        assert_eq!(
            outcome.session.stage(),
            &ExpenseSessionStage::InConfirmation
        );
        assert_eq!(outcome.snapshot.participants.len(), 1);
        assert_eq!(outcome.snapshot.participants[0].member_id, MemberId(42));
        assert!(outcome.drift.is_empty());
    }

    #[test]
    fn confirmation_build_rejects_session_with_no_payer_selected() {
        let session = bootstrapped();
        let mut draft = session.draft().clone();
        let key = session.key();
        let nonce = session.nonce();
        draft = draft.with_selection_state(ExpenseSelectionState {
            payer: None,
            individual_members: vec![MemberId(42)],
            ..Default::default()
        });
        let session = ExpenseSession::new(
            key,
            ExpenseLaunchOrigin::SlashCommand,
            ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::Payer,
            },
            draft,
            nonce,
            fixed_clock().now,
        )
        .unwrap();
        let roster = roster_with(&[42]);

        let actual = build_confirmation_for_session(session, &roster, &fixed_clock());

        assert_eq!(actual, Err(ConfirmationBuildError::PayerNotSelected));
    }

    #[test]
    fn confirmation_build_rejects_session_with_no_resolved_participants() {
        let session = bootstrapped();
        let roster = roster_with(&[100]);

        let actual = build_confirmation_for_session(session, &roster, &fixed_clock());

        assert_eq!(actual, Err(ConfirmationBuildError::NoResolvedParticipants));
    }

    #[test]
    fn confirmation_build_reports_drift_when_previous_snapshot_differs_from_resolution() {
        let session = bootstrapped();
        let roster_first = roster_with(&[42]);
        let first = build_confirmation_for_session(session, &roster_first, &fixed_clock())
            .expect("first confirmation");

        let roster_after_drift = roster_with(&[42, 7]);
        let mut draft = first.session.draft().clone();
        draft = draft.with_selection_state(ExpenseSelectionState {
            payer: Some(MemberId(42)),
            individual_members: vec![MemberId(42), MemberId(7)],
            ..Default::default()
        });
        let session = ExpenseSession::new(
            first.session.key(),
            ExpenseLaunchOrigin::SlashCommand,
            ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::IndividualSelection,
            },
            draft,
            first.session.nonce(),
            fixed_clock().now,
        )
        .unwrap();

        let rebuilt = build_confirmation_for_session(session, &roster_after_drift, &fixed_clock())
            .expect("rebuilt confirmation");

        assert_eq!(
            rebuilt.drift,
            vec![ParticipantDrift::Added {
                member_id: MemberId(7),
                weight: walicord_domain::model::Weight(1),
            }]
        );
        assert_eq!(rebuilt.snapshot.participants.len(), 2);
    }

    #[test]
    fn confirmation_payer_and_participants_returns_some_when_session_is_in_confirmation() {
        let session = bootstrapped();
        let roster = roster_with(&[42]);
        let outcome =
            build_confirmation_for_session(session, &roster, &fixed_clock()).expect("confirmation");

        let actual = confirmation_payer_and_participants(&outcome.session);

        assert!(actual.is_some());
        let (payer, participants) = actual.unwrap();
        assert_eq!(payer, MemberId(42));
        assert_eq!(participants.len(), 1);
    }

    #[test]
    fn confirmation_payer_and_participants_returns_none_for_in_selection_session() {
        let session = bootstrapped();
        let actual = confirmation_payer_and_participants(&session);
        assert_eq!(actual, None);
    }

    fn session_in_phase(phase: ExpenseSelectionPhase) -> ExpenseSession {
        let session = bootstrapped();
        let key = session.key();
        let nonce = session.nonce();
        let draft = session.draft().clone();
        ExpenseSession::new(
            key,
            ExpenseLaunchOrigin::SlashCommand,
            ExpenseSessionStage::InSelection { phase },
            draft,
            nonce,
            fixed_clock().now,
        )
        .expect("session in phase")
    }

    #[rstest::rstest]
    #[case::from_participant_source(
        ExpenseSelectionPhase::ParticipantSource,
        Some(ExpenseSelectionPhase::Payer)
    )]
    #[case::from_individual_selection(
        ExpenseSelectionPhase::IndividualSelection,
        Some(ExpenseSelectionPhase::ParticipantSource)
    )]
    #[case::from_roles(
        ExpenseSelectionPhase::Roles,
        Some(ExpenseSelectionPhase::ParticipantSource)
    )]
    #[case::from_weight_editor(
        ExpenseSelectionPhase::WeightEditor,
        Some(ExpenseSelectionPhase::ParticipantSource)
    )]
    #[case::from_payer_is_first_step(ExpenseSelectionPhase::Payer, None)]
    fn previous_phase_returns_documented_previous_step(
        #[case] from: ExpenseSelectionPhase,
        #[case] expected: Option<ExpenseSelectionPhase>,
    ) {
        let actual = previous_phase(&from);
        assert_eq!(actual, expected);
    }

    #[test]
    fn navigate_back_returns_already_at_first_step_when_in_payer_phase() {
        let session = session_in_phase(ExpenseSelectionPhase::Payer);
        let actual = navigate_back(session, &fixed_clock());
        assert_eq!(actual.unwrap_err(), NavigationError::AlreadyAtFirstStep);
    }

    #[test]
    fn navigate_back_preserves_draft_state_when_moving_one_phase_back() {
        let session = session_in_phase(ExpenseSelectionPhase::IndividualSelection);
        let original_draft = session.draft().clone();

        let next = navigate_back(session, &fixed_clock()).expect("back navigation");

        assert_eq!(
            next.stage(),
            &ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::ParticipantSource
            }
        );
        assert_eq!(next.draft(), &original_draft);
    }

    #[test]
    fn navigate_modify_selection_returns_to_payer_phase_dropping_confirmation_snapshot() {
        let session = bootstrapped();
        let roster = roster_with(&[42]);
        let outcome =
            build_confirmation_for_session(session, &roster, &fixed_clock()).expect("confirmation");

        let next =
            navigate_modify_selection(outcome.session, &fixed_clock()).expect("modify selection");

        assert_eq!(
            next.stage(),
            &ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::Payer
            }
        );
        assert_eq!(next.draft().confirmation_snapshot(), None);
        assert!(next.draft().basic_info().is_some());
    }

    fn updated_basic_info() -> ExpenseBasicInfo {
        ExpenseBasicInfo {
            amount: walicord_domain::Money::from_i64(2500),
            note: Some(crate::ledger::ExpenseNote::new("再編集").expect("note")),
            effective_date: crate::ledger::LedgerEffectiveDate::new("2026-05-15").expect("date"),
        }
    }

    #[test]
    fn apply_modified_basic_info_preserves_selection_state_and_drops_confirmation_snapshot() {
        let session = bootstrapped();
        let roster = roster_with(&[42]);
        let outcome =
            build_confirmation_for_session(session, &roster, &fixed_clock()).expect("confirmation");

        let next = apply_modified_basic_info(outcome.session, updated_basic_info(), &fixed_clock())
            .expect("apply modified basic info");

        assert_eq!(
            next.draft().basic_info().unwrap().amount,
            walicord_domain::Money::from_i64(2500)
        );
        assert_eq!(next.draft().selection_state().payer, Some(MemberId(42)));
        assert_eq!(next.draft().confirmation_snapshot(), None);
        assert_eq!(
            next.stage(),
            &ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::Payer
            }
        );
    }

    #[rstest::rstest]
    #[case::payer_to_participant_source(
        ExpenseSelectionPhase::Payer,
        ExpenseSelectionPhase::ParticipantSource
    )]
    #[case::participant_source_to_individual(
        ExpenseSelectionPhase::ParticipantSource,
        ExpenseSelectionPhase::IndividualSelection
    )]
    #[case::participant_source_to_roles(
        ExpenseSelectionPhase::ParticipantSource,
        ExpenseSelectionPhase::Roles
    )]
    #[case::participant_source_to_weight_editor(
        ExpenseSelectionPhase::ParticipantSource,
        ExpenseSelectionPhase::WeightEditor
    )]
    #[case::individual_to_weight_editor(
        ExpenseSelectionPhase::IndividualSelection,
        ExpenseSelectionPhase::WeightEditor
    )]
    #[case::roles_to_weight_editor(
        ExpenseSelectionPhase::Roles,
        ExpenseSelectionPhase::WeightEditor
    )]
    fn navigate_to_phase_accepts_legal_forward_transitions(
        #[case] from: ExpenseSelectionPhase,
        #[case] to: ExpenseSelectionPhase,
    ) {
        let actual = navigate_to_phase(session_in_phase(from), to.clone(), &fixed_clock())
            .expect("transition should succeed");

        assert_eq!(
            actual.stage(),
            &ExpenseSessionStage::InSelection { phase: to }
        );
    }

    #[rstest::rstest]
    #[case::payer_to_weight_editor_rejected(
        ExpenseSelectionPhase::Payer,
        ExpenseSelectionPhase::WeightEditor
    )]
    #[case::individual_to_roles_rejected(
        ExpenseSelectionPhase::IndividualSelection,
        ExpenseSelectionPhase::Roles
    )]
    #[case::weight_editor_has_no_forward_target(
        ExpenseSelectionPhase::WeightEditor,
        ExpenseSelectionPhase::WeightEditor
    )]
    fn navigate_to_phase_rejects_illegal_forward_transitions(
        #[case] from: ExpenseSelectionPhase,
        #[case] to: ExpenseSelectionPhase,
    ) {
        let actual = navigate_to_phase(session_in_phase(from.clone()), to.clone(), &fixed_clock());

        assert_eq!(
            actual,
            Err(NavigationError::IllegalForwardTransition { from, to })
        );
    }

    #[test]
    fn navigate_to_phase_preserves_draft_state_across_the_transition() {
        let from = session_in_phase(ExpenseSelectionPhase::Payer);
        let original_draft = from.draft().clone();

        let next = navigate_to_phase(
            from,
            ExpenseSelectionPhase::ParticipantSource,
            &fixed_clock(),
        )
        .expect("legal transition");

        assert_eq!(next.draft(), &original_draft);
    }

    #[test]
    fn toggle_members_group_flips_the_include_members_group_flag() {
        let session = session_in_phase(ExpenseSelectionPhase::ParticipantSource);
        assert!(!session.draft().selection_state().include_members_group);

        let toggled = toggle_members_group(session, &fixed_clock()).expect("toggle should succeed");

        assert!(toggled.draft().selection_state().include_members_group);

        let toggled_back =
            toggle_members_group(toggled, &fixed_clock()).expect("toggle should succeed");

        assert!(!toggled_back.draft().selection_state().include_members_group);
    }

    #[test]
    fn toggle_members_group_rejects_when_not_in_participant_source_phase() {
        let session = session_in_phase(ExpenseSelectionPhase::Payer);
        let actual = toggle_members_group(session, &fixed_clock());
        assert_eq!(actual.unwrap_err(), NavigationError::NotInSelection);
    }

    #[test]
    fn picker_replacements_update_only_the_active_phase_selection() {
        let payer = replace_payer(
            session_in_phase(ExpenseSelectionPhase::Payer),
            Some(MemberId(7)),
            &fixed_clock(),
        )
        .expect("payer replacement");
        assert_eq!(payer.draft().selection_state().payer, Some(MemberId(7)));

        let individuals = replace_individual_members(
            session_in_phase(ExpenseSelectionPhase::IndividualSelection),
            vec![MemberId(7), MemberId(42), MemberId(7)],
            &fixed_clock(),
        )
        .expect("individual replacement");
        assert_eq!(
            individuals.draft().selection_state().individual_members,
            vec![MemberId(7), MemberId(42)]
        );

        let roles = replace_selected_roles(
            session_in_phase(ExpenseSelectionPhase::Roles),
            vec![
                walicord_domain::model::RoleId(9),
                walicord_domain::model::RoleId(3),
            ],
            &fixed_clock(),
        )
        .expect("role replacement");
        assert_eq!(
            roles.draft().selection_state().selected_roles,
            vec![
                walicord_domain::model::RoleId(3),
                walicord_domain::model::RoleId(9)
            ]
        );

        let weights = replace_weight_overrides(
            session_in_phase(ExpenseSelectionPhase::WeightEditor),
            BTreeMap::from([(MemberId(7), walicord_domain::model::Weight(2))]),
            &fixed_clock(),
        )
        .expect("weight replacement");
        assert_eq!(
            weights.draft().selection_state().weight_overrides,
            BTreeMap::from([(MemberId(7), walicord_domain::model::Weight(2))])
        );
    }

    #[test]
    fn picker_view_state_is_saved_in_the_active_session_phase() {
        let session = session_in_phase(ExpenseSelectionPhase::IndividualSelection);

        let actual = set_picker_view_state(
            session,
            ExpensePickerKind::Individuals,
            PickerSnapshotId::new(11),
            2,
            Some("tanaka".to_owned()),
            &fixed_clock(),
        )
        .expect("picker state update");

        assert_eq!(
            actual
                .draft()
                .selection_state()
                .picker_states
                .get(&ExpensePickerKind::Individuals),
            Some(&PagedPickerState::new(
                PickerSnapshotId::new(11),
                2,
                Some("tanaka".to_owned()),
                vec![42],
            ))
        );
    }

    #[test]
    fn picker_clear_updates_the_matching_selection_and_resets_picker_state() {
        let session = replace_selected_roles(
            session_in_phase(ExpenseSelectionPhase::Roles),
            vec![walicord_domain::model::RoleId(7)],
            &fixed_clock(),
        )
        .expect("role selection");

        let actual = clear_picker_selection(
            session,
            ExpensePickerKind::Roles,
            PickerSnapshotId::new(17),
            &fixed_clock(),
        )
        .expect("clear role picker");

        assert!(actual.draft().selection_state().selected_roles.is_empty());
        assert_eq!(
            actual
                .draft()
                .selection_state()
                .picker_states
                .get(&ExpensePickerKind::Roles),
            Some(&PagedPickerState::new(
                PickerSnapshotId::new(17),
                0,
                None,
                Vec::new(),
            ))
        );
    }

    #[test]
    fn apply_modified_basic_info_from_awaiting_stage_advances_to_in_selection() {
        let session = bootstrapped();
        let key = session.key();
        let nonce = session.nonce();
        let awaiting = ExpenseSession::new(
            key,
            ExpenseLaunchOrigin::SlashCommand,
            ExpenseSessionStage::AwaitingBasicInfo,
            ExpenseDraftSnapshot::empty(),
            nonce,
            fixed_clock().now,
        )
        .unwrap();

        let next = apply_modified_basic_info(awaiting, updated_basic_info(), &fixed_clock())
            .expect("first submit from awaiting");

        assert_eq!(
            next.stage(),
            &ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::Payer
            }
        );
        assert!(next.draft().basic_info().is_some());
    }
}
