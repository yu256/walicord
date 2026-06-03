//! Confirmation-step and selection-step surface model builders.
//!
//! The adapter (router) used to compose the confirmation body and step bodies inline,
//! by interleaving i18n calls, format!() and direct serenity component construction.
//! That was a Tactical DDD violation: presentation rendering lived in the Discord
//! adapter. This module pulls the rendering into walicord-presentation so the adapter
//! only owns the conversion from `RenderedSurface` to serenity types.

use crate::discord_ledger::{
    ExpenseConfirmationParticipantRow, ExpenseDraftSummary, ExpenseParticipantSourceBadge,
    ExpenseSurfaceModel, SafeLiteralText, SurfaceActionRow, SurfaceButton,
    SurfaceInteractiveButtonStyle, SurfaceMemberLabels, confirmation_source_disclosure_line,
};
use smol_str::SmolStr;
use std::collections::{BTreeSet, HashMap};
use walicord_application::ledger::{
    ExpenseAuthoringError, MemberWeight, compute_expense_owed_amounts,
    expense_session::{ExpenseBasicInfo, ExpenseParticipantSelection, ExpenseSelectionPhase},
};
use walicord_domain::{Money, model::MemberId};
use walicord_i18n as i18n;

/// Custom-id strings the adapter renders into the confirmation-page buttons. Owned by
/// the adapter (Discord component identity) and passed in so the presentation layer
/// stays unaware of the custom_id format.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseConfirmationButtonIds {
    pub record: String,
    pub modify_selection: String,
    pub basic_edit: String,
    pub cancel: String,
}

/// Build the confirmation-page surface (4/4 step). The per-member share is computed
/// via `compute_expense_owed_amounts` — the same function the record path uses to
/// produce on-ledger amounts, so the preview the actor confirms equals the canonical
/// amounts by construction.
pub fn build_expense_confirmation_surface(
    basic_info: &ExpenseBasicInfo,
    participants: &[ExpenseParticipantSelection],
    defaulted_members: &[MemberId],
    display_names: &HashMap<MemberId, SmolStr>,
    button_ids: &ExpenseConfirmationButtonIds,
) -> Result<ExpenseSurfaceModel, ExpenseAuthoringError> {
    let defaulted: BTreeSet<MemberId> = defaulted_members.iter().copied().collect();
    let labels = SurfaceMemberLabels::from_member_names(participants.iter().map(|row| {
        (
            row.member_id,
            display_names.get(&row.member_id).map(|s| s.as_str()),
        )
    }));

    let summary_note = basic_info.note.as_ref().map(|note| {
        // ExpenseNote::new validates non-empty canonical form; from_note returning None
        // would be an invariant violation between the two types.
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
            badges: vec![ExpenseParticipantSourceBadge::DirectSelection],
            defaulted_weight: defaulted.contains(&row.member_id),
        };
        detail_lines.push(confirmation_row.render());
    }
    detail_lines.push(confirmation_source_disclosure_line().to_owned());

    let summary_lines: Vec<String> = summary.render_lines().to_vec();

    let action_rows = vec![
        SurfaceActionRow::Buttons(vec![
            SurfaceButton::Interactive {
                label: i18n::expense_record_label().to_owned(),
                custom_id: button_ids.record.clone(),
                style: SurfaceInteractiveButtonStyle::Primary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::expense_revise_label().to_owned(),
                custom_id: button_ids.modify_selection.clone(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::expense_basic_info_edit_label().to_owned(),
                custom_id: button_ids.basic_edit.clone(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
        ]),
        SurfaceActionRow::Buttons(vec![SurfaceButton::Interactive {
            label: i18n::expense_cancel_label().to_owned(),
            custom_id: button_ids.cancel.clone(),
            style: SurfaceInteractiveButtonStyle::Danger,
            disabled: false,
        }]),
    ];

    Ok(ExpenseSurfaceModel {
        title: i18n::expense_step_title_confirm().to_owned(),
        summary_lines,
        detail_lines,
        validation_message: None,
        action_rows,
        ephemeral: true,
    })
}

/// Custom-id strings the adapter renders into each selection-step phase's buttons.
/// Like `ExpenseConfirmationButtonIds`, these stay adapter-owned so presentation never
/// has to know how Discord encodes button identities.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseSelectionStepButtonIds {
    pub to_participants: String,
    pub source_individual: String,
    pub source_roles: String,
    pub source_members: String,
    pub to_weights: String,
    pub weight_edit: String,
    pub to_confirm: String,
    pub back: String,
    pub cancel: String,
}

/// Build the selection-step surface model for the supplied phase. Each phase shows the
/// step title and a phase-specific forward action row, ending with a Back/Cancel row.
/// Picker select menus are not part of this model yet (they're built separately as the
/// roster data flows in).
pub fn build_expense_selection_step_surface(
    phase: &ExpenseSelectionPhase,
    button_ids: &ExpenseSelectionStepButtonIds,
) -> ExpenseSurfaceModel {
    let title = match phase {
        ExpenseSelectionPhase::Payer => i18n::expense_step_title_payer(),
        ExpenseSelectionPhase::ParticipantSource
        | ExpenseSelectionPhase::IndividualSelection
        | ExpenseSelectionPhase::Roles => i18n::expense_step_title_participants(),
        ExpenseSelectionPhase::WeightEditor => i18n::expense_step_title_weight(),
    };

    let phase_specific: Vec<SurfaceButton> = match phase {
        ExpenseSelectionPhase::Payer => vec![SurfaceButton::Interactive {
            label: i18n::expense_next_label().to_owned(),
            custom_id: button_ids.to_participants.clone(),
            style: SurfaceInteractiveButtonStyle::Primary,
            disabled: false,
        }],
        ExpenseSelectionPhase::ParticipantSource => vec![
            SurfaceButton::Interactive {
                label: i18n::participant_source_individual_label().to_owned(),
                custom_id: button_ids.source_individual.clone(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::participant_source_role_label().to_owned(),
                custom_id: button_ids.source_roles.clone(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::participant_source_members_label().to_owned(),
                custom_id: button_ids.source_members.clone(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::expense_to_weights_label().to_owned(),
                custom_id: button_ids.to_weights.clone(),
                style: SurfaceInteractiveButtonStyle::Primary,
                disabled: false,
            },
        ],
        ExpenseSelectionPhase::IndividualSelection | ExpenseSelectionPhase::Roles => vec![
            SurfaceButton::Interactive {
                label: i18n::expense_to_weights_label().to_owned(),
                custom_id: button_ids.to_weights.clone(),
                style: SurfaceInteractiveButtonStyle::Primary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::expense_to_confirm_label().to_owned(),
                custom_id: button_ids.to_confirm.clone(),
                style: SurfaceInteractiveButtonStyle::Primary,
                disabled: false,
            },
        ],
        ExpenseSelectionPhase::WeightEditor => vec![
            SurfaceButton::Interactive {
                label: i18n::expense_weight_edit_label().to_owned(),
                custom_id: button_ids.weight_edit.clone(),
                style: SurfaceInteractiveButtonStyle::Secondary,
                disabled: false,
            },
            SurfaceButton::Interactive {
                label: i18n::expense_to_confirm_label().to_owned(),
                custom_id: button_ids.to_confirm.clone(),
                style: SurfaceInteractiveButtonStyle::Primary,
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
            label: i18n::expense_back_label().to_owned(),
            custom_id: button_ids.back.clone(),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: false,
        },
        SurfaceButton::Interactive {
            label: i18n::expense_cancel_label().to_owned(),
            custom_id: button_ids.cancel.clone(),
            style: SurfaceInteractiveButtonStyle::Danger,
            disabled: false,
        },
    ]));

    ExpenseSurfaceModel {
        title: title.to_owned(),
        summary_lines: Vec::new(),
        detail_lines: Vec::new(),
        validation_message: None,
        action_rows,
        ephemeral: true,
    }
}
