//! Void-specific presentation helpers extracted from `router::mod`. Each function is a
//! free helper that the dispatch methods on `LedgerRouter` call to compose the void
//! selection menu, confirmation surface, recovery references, and select-menu labels.
//! Keeping them here keeps the void presentation glue out of the dispatcher and lets
//! the dispatcher focus on session / write-coordinator orchestration.

use std::fmt::Write as _;

use serenity::all::{ComponentInteraction, ComponentInteractionDataKind};
use walicord_application::ledger::{
    LedgerEntry, LedgerEntryId, LedgerEvent, LedgerId, projection::VerifiedLedgerEntryView,
    void_flow::VoidComposeError,
};
use walicord_domain::Money;
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    RecoveryReference, SafeLiteralText, SurfaceActionRow, SurfaceButton,
    SurfaceInteractiveButtonStyle, SurfaceMemberLabels, SurfaceSelectMenu, SurfaceSelectOption,
    VoidCandidateRow, VoidConfirmationRecap, VoidSurfaceModel, summary_for_view,
};

use super::{
    InternalLedgerRouteError, LedgerRouteError, VOID_CANCEL_CUSTOM_ID_PREFIX,
    VOID_CONFIRM_CUSTOM_ID_PREFIX, VOID_PICK_CUSTOM_ID_PREFIX, VOID_RESELECT_CUSTOM_ID_PREFIX,
};

pub(super) fn selected_void_target(component: &ComponentInteraction) -> Option<LedgerEntryId> {
    match &component.data.kind {
        ComponentInteractionDataKind::StringSelect { values } => values
            .first()
            .and_then(|value| value.parse::<u64>().ok())
            .map(LedgerEntryId),
        _ => None,
    }
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
pub(super) fn void_candidate_rows(
    candidates: &[VerifiedLedgerEntryView],
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
) -> Result<Vec<VoidCandidateRow>, LedgerRouteError> {
    candidates
        .iter()
        .map(|view| {
            Ok(VoidCandidateRow {
                summary: summary_for_view(view, labels)?,
                recovery_reference: void_recovery_reference(view, ledger_id),
            })
        })
        .collect()
}

pub(super) fn void_recovery_reference(
    view: &VerifiedLedgerEntryView,
    ledger_id: LedgerId,
) -> RecoveryReference {
    RecoveryReference {
        ledger_id_short: format!("{ledger_id:08x}"),
        entry_id: view.entry().id,
        message_link: Some(view.message_link().to_owned()),
    }
}

pub(super) fn void_selection_action_rows(
    nonce: walicord_application::InteractionNonce,
    candidates: &[VerifiedLedgerEntryView],
    labels: &SurfaceMemberLabels,
) -> Vec<SurfaceActionRow> {
    let n = nonce;
    vec![SurfaceActionRow::Select(SurfaceSelectMenu {
        custom_id: format!("{VOID_PICK_CUSTOM_ID_PREFIX}{n}"),
        placeholder: Some(i18n::void_select_placeholder().to_owned()),
        options: candidates
            .iter()
            .map(|view| SurfaceSelectOption {
                value: view.entry().id.0.to_string(),
                label: void_candidate_select_label(view, labels),
                description: None,
                selected: false,
            })
            .collect(),
        min_values: 1,
        max_values: 1,
        disabled: false,
    })]
}

pub(super) fn void_confirmation_action_rows(
    nonce: walicord_application::InteractionNonce,
) -> Vec<SurfaceActionRow> {
    let n = nonce;
    vec![SurfaceActionRow::Buttons(vec![
        SurfaceButton::Interactive {
            label: i18n::void_confirm_label().to_owned(),
            custom_id: format!("{VOID_CONFIRM_CUSTOM_ID_PREFIX}{n}"),
            style: SurfaceInteractiveButtonStyle::Danger,
            disabled: false,
        },
        SurfaceButton::Interactive {
            label: i18n::void_reselect_label().to_owned(),
            custom_id: format!("{VOID_RESELECT_CUSTOM_ID_PREFIX}{n}"),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: false,
        },
        SurfaceButton::Interactive {
            label: i18n::void_cancel_label().to_owned(),
            custom_id: format!("{VOID_CANCEL_CUSTOM_ID_PREFIX}{n}"),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: false,
        },
    ])]
}

fn void_candidate_select_label(
    view: &VerifiedLedgerEntryView,
    labels: &SurfaceMemberLabels,
) -> SafeLiteralText {
    let mut label = String::new();
    match &view.entry().event {
        LedgerEvent::ExpenseRecorded(event) => {
            let payer = event
                .paid_by()
                .first()
                .map(|paid| labels.safe_member_label(paid.member_id));
            let amount = event
                .paid_by()
                .iter()
                .map(|paid| paid.amount)
                .sum::<Money>();
            let _ = match payer {
                Some(payer) => write!(label, "#{} 経費 {amount}円 {payer}", view.entry().id.0),
                None => write!(label, "#{} 経費 {amount}円", view.entry().id.0),
            };
        }
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
            let _ = write!(label, "#{} 清算", view.entry().id.0);
            if let Some(first) = event.transfers().first() {
                let _ = write!(
                    label,
                    " {}->{} {}円",
                    labels.safe_member_label(first.from),
                    labels.safe_member_label(first.to),
                    first.amount
                );
            }
            let additional = event.transfers().len().saturating_sub(1);
            if additional > 0 {
                let _ = write!(label, " {}", i18n::additional_items(additional));
            }
        }
        _ => {
            let _ = write!(label, "#{}", view.entry().id.0);
        }
    }
    SafeLiteralText::from_roster_label(&label).expect("void candidate select label should sanitize")
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
pub(super) fn void_confirmation_model(
    target: &VerifiedLedgerEntryView,
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
    nonce: walicord_application::InteractionNonce,
) -> Result<VoidSurfaceModel, LedgerRouteError> {
    Ok(VoidSurfaceModel::confirmation(
        i18n::void_confirmation_title(),
        VoidConfirmationRecap {
            summary: summary_for_view(target, labels)?,
            total_amount: void_confirmation_total_amount(target.entry())?,
            recovery_reference: void_recovery_reference(target, ledger_id),
        },
        void_confirmation_action_rows(nonce),
        true,
    ))
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
pub(super) fn void_confirmation_total_amount(
    entry: &LedgerEntry,
) -> Result<String, LedgerRouteError> {
    match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => Ok(event
            .paid_by()
            .iter()
            .map(|paid| paid.amount)
            .sum::<Money>()
            .to_string()),
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => Ok(event
            .transfers()
            .iter()
            .map(|transfer| transfer.amount)
            .sum::<Money>()
            .to_string()),
        _ => Err(LedgerRouteError::Internal(
            InternalLedgerRouteError::VoidCompose(VoidComposeError::TargetNoLongerVoidable {
                target_entry_id: entry.id,
            }),
        )),
    }
}
