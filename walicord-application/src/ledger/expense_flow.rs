use super::expense_modal::ValidatedExpenseModalSubmission;
use crate::{
    Clock, SessionNonce, SessionNonceProvider,
    ledger::{
        expense_session::{
            ExpenseBasicInfo, ExpenseConfirmationSnapshot, ExpenseDraftSnapshot,
            ExpenseLaunchOrigin, ExpenseParticipantSelection, ExpenseSelectionPhase,
            ExpenseSelectionState, ExpenseSession, ExpenseSessionConstructionError,
            ExpenseSessionKey, ExpenseSessionStage, ParticipantSelectionMode,
        },
        participant_resolution::{
            ParticipantDrift, RosterSnapshot, drift_between_snapshot_and_resolution,
            resolve_selection_against_roster,
        },
    },
};
use std::collections::BTreeMap;
use walicord_domain::model::{MemberId, RoleId, Weight};

pub fn bootstrap_expense_session(
    key: ExpenseSessionKey,
    origin: ExpenseLaunchOrigin,
    validated_modal: ValidatedExpenseModalSubmission,
    clock: &dyn Clock,
    nonce_provider: &dyn SessionNonceProvider,
) -> Result<(ExpenseSession, SessionNonce), ExpenseSessionConstructionError> {
    let nonce = nonce_provider.next_session_nonce();
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
            phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual),
        },
        draft,
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
    #[error("session is not in the InConfirmation stage")]
    NotInConfirmation,
    #[error("basic info is missing from the session draft")]
    BasicInfoMissing,
    #[error("no payer is selected in the session")]
    PayerNotSelected,
    #[error("no resolved participants after roster resolution")]
    NoResolvedParticipants,
    #[error("confirmation-stage session construction failed: {0}")]
    ConstructionFailed(#[from] ExpenseSessionConstructionError),
}

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

    let drift = confirmation_drift_from_previous_snapshot(
        session.draft().confirmation_snapshot(),
        &outcome.resolved,
    );

    let snapshot = ExpenseConfirmationSnapshot {
        participants: outcome.resolved.clone(),
    };
    let key = session.key();
    let origin = session.origin();
    let draft = session.into_draft();
    let next_draft = draft
        .with_selection_state(selection)
        .with_confirmation_snapshot(snapshot.clone());
    let next_session = ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InConfirmation,
        next_draft,
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

fn confirmation_drift_from_previous_snapshot(
    previous: Option<&ExpenseConfirmationSnapshot>,
    resolved: &[ExpenseParticipantSelection],
) -> Vec<ParticipantDrift> {
    match previous {
        Some(previous) => drift_between_snapshot_and_resolution(previous, resolved),
        None => Vec::new(),
    }
}

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
    #[error("session is not in the InSelection stage")]
    NotInSelection,
    #[error("session is not in the InConfirmation stage")]
    NotInConfirmation,
    #[error("basic info is missing from the session draft")]
    BasicInfoMissing,
    #[error("session construction failed during navigation: {0}")]
    ConstructionFailed(#[from] ExpenseSessionConstructionError),
}

pub fn switch_participant_mode(
    session: ExpenseSession,
    target: ParticipantSelectionMode,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    let ExpenseSessionStage::InSelection {
        phase: ExpenseSelectionPhase::Participants { .. },
    } = session.stage()
    else {
        return Err(NavigationError::NotInSelection);
    };
    let key = session.key();
    let origin = session.origin();
    let draft = session.into_draft();
    ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection {
            phase: ExpenseSelectionPhase::participants(target),
        },
        draft,
        clock.now(),
    )
    .map_err(NavigationError::ConstructionFailed)
}

pub fn navigate_modify_selection(
    session: ExpenseSession,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    if !matches!(session.stage(), ExpenseSessionStage::InConfirmation) {
        return Err(NavigationError::NotInConfirmation);
    }
    let key = session.key();
    let origin = session.origin();
    let draft = session.into_draft();
    let draft = draft.without_confirmation_snapshot();
    ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection {
            phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual),
        },
        draft,
        clock.now(),
    )
    .map_err(NavigationError::ConstructionFailed)
}

pub fn toggle_members_group(
    session: ExpenseSession,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    let ExpenseSessionStage::InSelection {
        phase: ExpenseSelectionPhase::Participants { mode },
    } = session.stage()
    else {
        return Err(NavigationError::NotInSelection);
    };
    let mode = *mode;
    let mut selection = session.draft().selection_state().clone();
    selection.include_members_group = !selection.include_members_group;
    let key = session.key();
    let origin = session.origin();
    let draft = session.into_draft();
    let draft = draft.with_selection_state(selection);
    ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection {
            phase: ExpenseSelectionPhase::participants(mode),
        },
        draft,
        clock.now(),
    )
    .map_err(NavigationError::ConstructionFailed)
}

pub fn replace_payer(
    session: ExpenseSession,
    payer: Option<MemberId>,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    update_selection_in_phase(
        session,
        ExpenseSelectionPhase::participants(ParticipantSelectionMode::Payer),
        clock,
        |selection| {
            selection.payer = payer;
        },
    )
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
        ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual),
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
    update_selection_in_phase(
        session,
        ExpenseSelectionPhase::participants(ParticipantSelectionMode::Roles),
        clock,
        |selection| {
            selection.selected_roles = roles;
        },
    )
}

pub fn replace_weight_overrides_and_rebuild(
    session: ExpenseSession,
    weights: BTreeMap<MemberId, Weight>,
    roster: &RosterSnapshot,
    clock: &dyn Clock,
) -> Result<ConfirmationBuildOutcome, ConfirmationBuildError> {
    if !matches!(session.stage(), ExpenseSessionStage::InConfirmation) {
        return Err(ConfirmationBuildError::NotInConfirmation);
    }
    let mut selection = session.draft().selection_state().clone();
    selection.weight_overrides = weights;
    let key = session.key();
    let origin = session.origin();
    let draft = session.into_draft();
    let intermediate = ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection {
            phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual),
        },
        draft
            .with_selection_state(selection)
            .without_confirmation_snapshot(),
        clock.now(),
    )
    .map_err(ConfirmationBuildError::ConstructionFailed)?;
    build_confirmation_for_session(intermediate, roster, clock)
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
    let mut selection = session.draft().selection_state().clone();
    update(&mut selection);
    let key = session.key();
    let origin = session.origin();
    let draft = session.into_draft();
    ExpenseSession::new(
        key,
        origin,
        ExpenseSessionStage::InSelection {
            phase: expected_phase,
        },
        draft.with_selection_state(selection),
        clock.now(),
    )
    .map_err(NavigationError::ConstructionFailed)
}

pub fn apply_modified_basic_info(
    session: ExpenseSession,
    new_basic_info: ExpenseBasicInfo,
    clock: &dyn Clock,
) -> Result<ExpenseSession, NavigationError> {
    let stage = match session.stage() {
        ExpenseSessionStage::AwaitingBasicInfo | ExpenseSessionStage::InConfirmation => {
            ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual),
            }
        }
        ExpenseSessionStage::InSelection { phase } => ExpenseSessionStage::InSelection {
            phase: phase.clone(),
        },
    };
    let key = session.key();
    let origin = session.origin();
    let draft = session.into_draft();
    let draft = draft
        .with_basic_info(new_basic_info)
        .without_confirmation_snapshot();
    ExpenseSession::new(key, origin, stage, draft, clock.now())
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

    struct SequentialSessionNonceProvider {
        nonce: AtomicU64,
        preview: AtomicU64,
    }

    impl SessionNonceProvider for SequentialSessionNonceProvider {
        fn next_session_nonce(&self) -> SessionNonce {
            SessionNonce::new(self.nonce.fetch_add(1, Ordering::SeqCst)).expect("non-zero")
        }
        fn next_preview_instance_id(&self) -> PreviewInstanceId {
            PreviewInstanceId::new(self.preview.fetch_add(1, Ordering::SeqCst)).expect("non-zero")
        }
    }

    fn nonce_provider() -> SequentialSessionNonceProvider {
        SequentialSessionNonceProvider {
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
                phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual)
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
        assert_eq!(nonce, SessionNonce::new(1).unwrap());
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

        assert_eq!(nonce, SessionNonce::new(1).unwrap());
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
        draft = draft.with_selection_state(ExpenseSelectionState {
            payer: None,
            individual_members: vec![MemberId(42)],
            ..Default::default()
        });
        let session = ExpenseSession::new(
            key,
            ExpenseLaunchOrigin::SlashCommand,
            ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual),
            },
            draft,
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
                phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual),
            },
            draft,
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

    fn session_in_mode(mode: ParticipantSelectionMode) -> ExpenseSession {
        let session = bootstrapped();
        let key = session.key();
        let draft = session.draft().clone();
        ExpenseSession::new(
            key,
            ExpenseLaunchOrigin::SlashCommand,
            ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::participants(mode),
            },
            draft,
            fixed_clock().now,
        )
        .expect("session in mode")
    }

    #[rstest::rstest]
    #[case::individual_to_roles(
        ParticipantSelectionMode::Individual,
        ParticipantSelectionMode::Roles
    )]
    #[case::individual_to_payer(
        ParticipantSelectionMode::Individual,
        ParticipantSelectionMode::Payer
    )]
    #[case::roles_to_individual(
        ParticipantSelectionMode::Roles,
        ParticipantSelectionMode::Individual
    )]
    #[case::roles_to_payer(ParticipantSelectionMode::Roles, ParticipantSelectionMode::Payer)]
    #[case::payer_to_individual(
        ParticipantSelectionMode::Payer,
        ParticipantSelectionMode::Individual
    )]
    #[case::payer_to_roles(ParticipantSelectionMode::Payer, ParticipantSelectionMode::Roles)]
    fn switch_participant_mode_changes_mode_preserving_draft(
        #[case] from: ParticipantSelectionMode,
        #[case] to: ParticipantSelectionMode,
    ) {
        let session = session_in_mode(from);
        let original_draft = session.draft().clone();

        let switched =
            switch_participant_mode(session, to, &fixed_clock()).expect("switch should succeed");

        assert_eq!(
            switched.stage(),
            &ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::participants(to)
            }
        );
        assert_eq!(switched.draft(), &original_draft);
    }

    #[test]
    fn switch_participant_mode_rejects_non_selection_stage() {
        let session = bootstrapped();
        let roster = roster_with(&[42]);
        let outcome =
            build_confirmation_for_session(session, &roster, &fixed_clock()).expect("confirmation");

        let actual = switch_participant_mode(
            outcome.session,
            ParticipantSelectionMode::Roles,
            &fixed_clock(),
        );

        assert_eq!(actual.unwrap_err(), NavigationError::NotInSelection);
    }

    #[test]
    fn navigate_modify_selection_returns_to_individual_mode_dropping_confirmation_snapshot() {
        let session = bootstrapped();
        let roster = roster_with(&[42]);
        let outcome =
            build_confirmation_for_session(session, &roster, &fixed_clock()).expect("confirmation");

        let next =
            navigate_modify_selection(outcome.session, &fixed_clock()).expect("modify selection");

        assert_eq!(
            next.stage(),
            &ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual)
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
                phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual)
            }
        );
    }

    #[test]
    fn toggle_members_group_flips_the_include_members_group_flag() {
        let session = session_in_mode(ParticipantSelectionMode::Individual);
        assert!(!session.draft().selection_state().include_members_group);

        let toggled = toggle_members_group(session, &fixed_clock()).expect("toggle should succeed");
        assert!(toggled.draft().selection_state().include_members_group);

        let toggled_back =
            toggle_members_group(toggled, &fixed_clock()).expect("toggle should succeed");
        assert!(!toggled_back.draft().selection_state().include_members_group);
    }

    #[rstest::rstest]
    #[case::from_individual(ParticipantSelectionMode::Individual)]
    #[case::from_roles(ParticipantSelectionMode::Roles)]
    #[case::from_payer(ParticipantSelectionMode::Payer)]
    fn toggle_members_group_preserves_current_mode(#[case] mode: ParticipantSelectionMode) {
        let session = session_in_mode(mode);
        let toggled = toggle_members_group(session, &fixed_clock()).expect("toggle");
        assert_eq!(
            toggled.stage(),
            &ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::participants(mode)
            }
        );
    }

    #[test]
    fn replace_payer_updates_payer_in_payer_mode() {
        let updated = replace_payer(
            session_in_mode(ParticipantSelectionMode::Payer),
            Some(MemberId(7)),
            &fixed_clock(),
        )
        .expect("payer replacement");
        assert_eq!(updated.draft().selection_state().payer, Some(MemberId(7)));
    }

    #[test]
    fn replace_individual_members_deduplicates_and_sorts() {
        let updated = replace_individual_members(
            session_in_mode(ParticipantSelectionMode::Individual),
            vec![MemberId(7), MemberId(42), MemberId(7)],
            &fixed_clock(),
        )
        .expect("individual replacement");
        assert_eq!(
            updated.draft().selection_state().individual_members,
            vec![MemberId(7), MemberId(42)]
        );
    }

    #[test]
    fn replace_selected_roles_deduplicates_and_sorts() {
        let updated = replace_selected_roles(
            session_in_mode(ParticipantSelectionMode::Roles),
            vec![
                walicord_domain::model::RoleId(9),
                walicord_domain::model::RoleId(3),
            ],
            &fixed_clock(),
        )
        .expect("role replacement");
        assert_eq!(
            updated.draft().selection_state().selected_roles,
            vec![
                walicord_domain::model::RoleId(3),
                walicord_domain::model::RoleId(9)
            ]
        );
    }

    #[test]
    fn weight_overrides_are_applied_and_confirmation_is_rebuilt() {
        let session = bootstrapped();
        let roster = roster_with(&[42]);
        let outcome =
            build_confirmation_for_session(session, &roster, &fixed_clock()).expect("confirmation");

        let rebuilt = replace_weight_overrides_and_rebuild(
            outcome.session,
            BTreeMap::from([(MemberId(42), walicord_domain::model::Weight(2))]),
            &roster,
            &fixed_clock(),
        )
        .expect("weight replacement and rebuild");

        assert_eq!(
            rebuilt.session.stage(),
            &ExpenseSessionStage::InConfirmation
        );
        assert_eq!(
            rebuilt.session.draft().selection_state().weight_overrides,
            BTreeMap::from([(MemberId(42), walicord_domain::model::Weight(2))])
        );
        assert_eq!(rebuilt.snapshot.participants.len(), 1);
    }

    #[test]
    fn replace_selected_roles_with_empty_vec_clears_roles() {
        let session = replace_selected_roles(
            session_in_mode(ParticipantSelectionMode::Roles),
            vec![walicord_domain::model::RoleId(7)],
            &fixed_clock(),
        )
        .expect("role selection");

        let actual = replace_selected_roles(session, vec![], &fixed_clock()).expect("clear roles");

        assert!(actual.draft().selection_state().selected_roles.is_empty());
    }

    #[test]
    fn apply_modified_basic_info_from_awaiting_stage_advances_to_in_selection() {
        let session = bootstrapped();
        let key = session.key();
        let awaiting = ExpenseSession::new(
            key,
            ExpenseLaunchOrigin::SlashCommand,
            ExpenseSessionStage::AwaitingBasicInfo,
            ExpenseDraftSnapshot::empty(),
            fixed_clock().now,
        )
        .unwrap();

        let next = apply_modified_basic_info(awaiting, updated_basic_info(), &fixed_clock())
            .expect("first submit from awaiting");

        assert_eq!(
            next.stage(),
            &ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::participants(ParticipantSelectionMode::Individual)
            }
        );
        assert!(next.draft().basic_info().is_some());
    }
}
