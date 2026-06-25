use super::expense_component_id::ExpenseComponentId;
use crate::discord_ledger::{
    ExpenseConfirmationParticipantRow, ExpenseDraftSummary, ExpenseSuccessSurfaceModel,
    ExpenseSurfaceModel, SafeLiteralText, SurfaceActionRow, SurfaceButton,
    SurfaceInteractiveButtonStyle, SurfaceMemberLabels,
};
use smol_str::SmolStr;
use std::collections::HashMap;
use walicord_application::{
    SessionNonce,
    ledger::{
        ExpenseAuthoringError, MemberWeight, compute_expense_owed_amounts,
        expense_session::{
            ExpenseBasicInfo, ExpenseParticipantSelection, ExpenseSelectionPhase,
            ParticipantSelectionMode,
        },
    },
};
use walicord_domain::{Money, model::MemberId};
use walicord_i18n as i18n;

pub fn build_expense_confirmation_surface(
    basic_info: &ExpenseBasicInfo,
    participants: &[ExpenseParticipantSelection],
    display_names: &HashMap<MemberId, SmolStr>,
    nonce: SessionNonce,
) -> Result<ExpenseSurfaceModel, ExpenseAuthoringError> {
    let labels = SurfaceMemberLabels::from_member_names(participants.iter().map(|row| {
        (
            row.member_id,
            display_names.get(&row.member_id).map(|s| s.as_str()),
        )
    }));

    let summary_note = basic_info.note.as_ref().map(|note| {
        SafeLiteralText::from_note(note.as_str())
            .expect("validated ExpenseNote should always produce a SafeLiteralText")
    });
    let summary = ExpenseDraftSummary {
        amount: basic_info.amount.to_string(),
        effective_date: basic_info.effective_date,
        note: summary_note,
    };

    let canonical_weights: Vec<MemberWeight> = participants
        .iter()
        .map(|row| MemberWeight {
            member_id: row.member_id,
            weight: row.weight,
        })
        .collect();
    let owed = compute_expense_owed_amounts(&canonical_weights, basic_info.amount)?;
    let share_by_member: HashMap<MemberId, Money> = owed
        .into_iter()
        .map(|amount| (amount.member_id, amount.amount))
        .collect();

    let show_weight = !participants
        .iter()
        .all(|p| p.weight == participants[0].weight);
    let mut detail_lines: Vec<String> = Vec::new();
    for row in participants {
        let display_name = labels
            .member(row.member_id)
            .map(|label| label.visible().clone())
            .unwrap_or_else(|| {
                SafeLiteralText::from_roster_label(
                    &i18n::unknown_user_label(row.member_id.0).to_string(),
                )
                .expect("unknown_user_label is a fixed fallback that always sanitises")
            });
        let share_amount = share_by_member
            .get(&row.member_id)
            .map(|amount| amount.to_string());
        let confirmation_row = ExpenseConfirmationParticipantRow {
            display_name,
            share_amount,
            weight: row.weight.0,
        };
        detail_lines.push(confirmation_row.render(show_weight));
    }

    let summary_lines: Vec<String> = summary.render_lines().to_vec();

    let action_rows = vec![
        SurfaceActionRow::Buttons(vec![
            SurfaceButton::Interactive {
                label: i18n::EXPENSE_RECORD_LABEL.to_owned(),
                custom_id: ExpenseComponentId::Record(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Primary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::EXPENSE_CANCEL_LABEL.to_owned(),
                custom_id: ExpenseComponentId::Cancel(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Danger,
                disabled: false,
            },
        ]),
        SurfaceActionRow::Buttons(vec![
            SurfaceButton::Interactive {
                label: i18n::EXPENSE_REVISE_LABEL.to_owned(),
                custom_id: ExpenseComponentId::ModifySelection(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::EXPENSE_BASIC_INFO_EDIT_LABEL.to_owned(),
                custom_id: ExpenseComponentId::BasicEdit(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::EXPENSE_WEIGHT_EDIT_LABEL.to_owned(),
                custom_id: ExpenseComponentId::WeightEdit(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
        ]),
    ];

    Ok(ExpenseSurfaceModel {
        title: i18n::EXPENSE_STEP_TITLE_CONFIRM.to_owned(),
        summary_lines,
        detail_lines,
        validation_message: None,
        action_rows,
        ephemeral: true,
    })
}

pub fn build_expense_selection_step_surface(
    phase: &ExpenseSelectionPhase,
    nonce: SessionNonce,
    include_members_group: bool,
) -> ExpenseSurfaceModel {
    let ExpenseSelectionPhase::Participants { mode } = phase;

    let members_toggle_label = if include_members_group {
        i18n::PARTICIPANT_SOURCE_CLEAR_MEMBERS_LABEL
    } else {
        i18n::PARTICIPANT_SOURCE_MEMBERS_LABEL
    };

    let members_toggle_button = SurfaceButton::Interactive {
        label: members_toggle_label.to_owned(),
        custom_id: ExpenseComponentId::MembersToggle(nonce).to_string(),
        style: SurfaceInteractiveButtonStyle::Secondary,
        disabled: false,
    };

    let phase_specific: Vec<SurfaceButton> = match mode {
        ParticipantSelectionMode::Individual => vec![
            SurfaceButton::Interactive {
                label: i18n::PARTICIPANT_SOURCE_ROLE_LABEL.to_owned(),
                custom_id: ExpenseComponentId::SwitchRoles(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            members_toggle_button,
            SurfaceButton::Interactive {
                label: i18n::EXPENSE_SWITCH_TO_PAYER_LABEL.to_owned(),
                custom_id: ExpenseComponentId::SwitchPayer(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
        ],
        ParticipantSelectionMode::Roles => vec![
            SurfaceButton::Interactive {
                label: i18n::PARTICIPANT_SOURCE_INDIVIDUAL_LABEL.to_owned(),
                custom_id: ExpenseComponentId::SwitchIndividual(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            members_toggle_button,
            SurfaceButton::Interactive {
                label: i18n::EXPENSE_SWITCH_TO_PAYER_LABEL.to_owned(),
                custom_id: ExpenseComponentId::SwitchPayer(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
        ],
        ParticipantSelectionMode::Payer => vec![
            SurfaceButton::Interactive {
                label: i18n::PARTICIPANT_SOURCE_INDIVIDUAL_LABEL.to_owned(),
                custom_id: ExpenseComponentId::SwitchIndividual(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::PARTICIPANT_SOURCE_ROLE_LABEL.to_owned(),
                custom_id: ExpenseComponentId::SwitchRoles(nonce).to_string(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
        ],
    };

    let mut action_rows = Vec::new();
    if !phase_specific.is_empty() {
        action_rows.push(SurfaceActionRow::Buttons(phase_specific));
    }
    action_rows.push(SurfaceActionRow::Buttons(vec![
        SurfaceButton::Interactive {
            label: i18n::EXPENSE_TO_CONFIRM_LABEL.to_owned(),
            custom_id: ExpenseComponentId::ToConfirm(nonce).to_string(),
            style: SurfaceInteractiveButtonStyle::Primary,
            disabled: false,
        },
        SurfaceButton::Interactive {
            label: i18n::EXPENSE_CANCEL_LABEL.to_owned(),
            custom_id: ExpenseComponentId::Cancel(nonce).to_string(),
            style: SurfaceInteractiveButtonStyle::Danger,
            disabled: false,
        },
    ]));

    let mode_label = match mode {
        ParticipantSelectionMode::Individual => i18n::PARTICIPANT_SOURCE_INDIVIDUAL_LABEL,
        ParticipantSelectionMode::Roles => i18n::PARTICIPANT_SOURCE_ROLE_LABEL,
        ParticipantSelectionMode::Payer => i18n::EXPENSE_SUB_VIEW_PAYER_TITLE,
    };

    ExpenseSurfaceModel {
        title: format!("{} — {mode_label}", i18n::EXPENSE_STEP_TITLE_PARTICIPANTS),
        summary_lines: Vec::new(),
        detail_lines: Vec::new(),
        validation_message: None,
        action_rows,
        ephemeral: true,
    }
}

pub fn build_expense_success_surface(
    basic_info: &ExpenseBasicInfo,
    participant_count: usize,
    canonical_thread_id: Option<u64>,
) -> ExpenseSuccessSurfaceModel {
    let mut body_lines = vec![
        i18n::EXPENSE_RECORDED_MESSAGE.to_owned(),
        i18n::expense_recorded_summary(
            basic_info.amount,
            basic_info.effective_date,
            participant_count,
        )
        .to_string(),
    ];
    if let Some(thread_id) = canonical_thread_id {
        body_lines.push(i18n::void_success_thread_line(format!("<#{thread_id}>")).to_string());
    }
    ExpenseSuccessSurfaceModel {
        body_lines,
        action_rows: Vec::new(),
        ephemeral: true,
    }
}

#[cfg(test)]
mod tests {
    use super::build_expense_success_surface;
    use walicord_application::ledger::{LedgerEffectiveDate, expense_session::ExpenseBasicInfo};
    use walicord_domain::Money;
    use walicord_i18n as i18n;

    fn basic_info() -> ExpenseBasicInfo {
        ExpenseBasicInfo {
            amount: Money::new(1500, 0),
            note: None,
            effective_date: LedgerEffectiveDate::new("2026-06-01").expect("date should be valid"),
        }
    }

    #[test]
    fn success_surface_with_thread_id_has_three_lines_ending_with_thread_link() {
        let model = build_expense_success_surface(&basic_info(), 3, Some(999));

        assert_eq!(model.body_lines[0], i18n::EXPENSE_RECORDED_MESSAGE);
        assert_eq!(model.body_lines.len(), 3);
        assert!(model.body_lines[2].contains("<#999>"));
    }

    #[test]
    fn success_surface_without_thread_id_has_two_lines() {
        let model = build_expense_success_surface(&basic_info(), 2, None);

        assert_eq!(model.body_lines[0], i18n::EXPENSE_RECORDED_MESSAGE);
        assert_eq!(model.body_lines.len(), 2);
    }
}
