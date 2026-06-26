//! Void-specific presentation helpers. Each function composes a void selection
//! menu, confirmation surface, recovery reference, or select-menu label; the
//! dispatcher methods on `LedgerRouter` (in `router::mod`) call into them to
//! keep the presentation glue out of the dispatcher itself.

use std::fmt::Write as _;

use serenity::all::{ComponentInteraction, ComponentInteractionDataKind};
use walicord_application::ledger::{
    LedgerEntryId, LedgerId,
    projection::{ExpenseOrSettlementEvent, ExpenseOrSettlementView},
};
use walicord_domain::Money;
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    RecoveryReference, SafeLiteralText, SurfaceActionRow, SurfaceButton,
    SurfaceInteractiveButtonStyle, SurfaceMemberLabels, SurfaceSelectMenu, SurfaceSelectOption,
    VoidCandidateRow, VoidConfirmationRecap, VoidSurfaceModel, expense_or_settlement_summary,
};

use super::{
    LedgerRouteError, VOID_CANCEL_CUSTOM_ID_PREFIX, VOID_CONFIRM_CUSTOM_ID_PREFIX,
    VOID_PICK_CUSTOM_ID_PREFIX, VOID_RESELECT_CUSTOM_ID_PREFIX,
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
    candidates: &[ExpenseOrSettlementView],
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
) -> Result<Vec<VoidCandidateRow>, LedgerRouteError> {
    candidates
        .iter()
        .map(|view| {
            Ok(VoidCandidateRow {
                summary: expense_or_settlement_summary(view, labels),
                recovery_reference: void_recovery_reference(view, ledger_id),
            })
        })
        .collect()
}

pub(super) fn void_recovery_reference(
    view: &ExpenseOrSettlementView,
    ledger_id: LedgerId,
) -> RecoveryReference {
    RecoveryReference {
        ledger_id_short: format!("{ledger_id:08x}"),
        entry_id: view.entry_id(),
        message_link: Some(view.message_link().to_owned()),
    }
}

pub(super) fn void_selection_action_rows(
    nonce: walicord_application::SessionNonce,
    candidates: &[ExpenseOrSettlementView],
    labels: &SurfaceMemberLabels,
) -> Vec<SurfaceActionRow> {
    let n = nonce;
    vec![SurfaceActionRow::Select(SurfaceSelectMenu {
        custom_id: format!("{VOID_PICK_CUSTOM_ID_PREFIX}{n}"),
        placeholder: Some(i18n::VOID_SELECT_PLACEHOLDER.to_owned()),
        options: candidates
            .iter()
            .map(|view| SurfaceSelectOption {
                value: view.entry_id().to_string(),
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
    nonce: walicord_application::SessionNonce,
) -> Vec<SurfaceActionRow> {
    let n = nonce;
    vec![SurfaceActionRow::Buttons(vec![
        SurfaceButton::Interactive {
            label: i18n::VOID_CONFIRM_LABEL.to_owned(),
            custom_id: format!("{VOID_CONFIRM_CUSTOM_ID_PREFIX}{n}"),
            style: SurfaceInteractiveButtonStyle::Danger,
            disabled: false,
        },
        SurfaceButton::Interactive {
            label: i18n::VOID_RESELECT_LABEL.to_owned(),
            custom_id: format!("{VOID_RESELECT_CUSTOM_ID_PREFIX}{n}"),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: false,
        },
        SurfaceButton::Interactive {
            label: i18n::VOID_CANCEL_LABEL.to_owned(),
            custom_id: format!("{VOID_CANCEL_CUSTOM_ID_PREFIX}{n}"),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: false,
        },
    ])]
}

fn void_candidate_select_label(
    view: &ExpenseOrSettlementView,
    labels: &SurfaceMemberLabels,
) -> SafeLiteralText {
    let mut label = String::new();
    let entry_id = view.entry_id();
    match view.event() {
        ExpenseOrSettlementEvent::Expense(event) => {
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
                Some(payer) => write!(label, "#{entry_id} 支出 {amount}円 {payer}"),
                None => write!(label, "#{entry_id} 支出 {amount}円"),
            };
        }
        ExpenseOrSettlementEvent::Settlement(event) => {
            let first = event.transfers().first();
            let _ = write!(
                label,
                "#{entry_id} 清算 {}->{} {}円",
                labels.safe_member_label(first.from),
                labels.safe_member_label(first.to),
                first.amount
            );
            let additional = event.transfers().len().saturating_sub(1);
            if additional > 0 {
                let _ = write!(label, " {}", i18n::additional_items(additional));
            }
        }
    }
    SafeLiteralText::from_roster_label(&label).expect("void candidate select label should sanitize")
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
pub(super) fn void_confirmation_model(
    target: &ExpenseOrSettlementView,
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
    nonce: walicord_application::SessionNonce,
) -> Result<VoidSurfaceModel, LedgerRouteError> {
    Ok(VoidSurfaceModel::confirmation(
        i18n::VOID_CONFIRMATION_TITLE,
        VoidConfirmationRecap {
            summary: expense_or_settlement_summary(target, labels),
            total_amount: void_confirmation_total_amount(target),
            recovery_reference: void_recovery_reference(target, ledger_id),
        },
        void_confirmation_action_rows(nonce),
        true,
    ))
}

pub(super) fn void_confirmation_total_amount(view: &ExpenseOrSettlementView) -> String {
    match view.event() {
        ExpenseOrSettlementEvent::Expense(event) => event
            .paid_by()
            .iter()
            .map(|paid| paid.amount)
            .sum::<Money>()
            .to_string(),
        ExpenseOrSettlementEvent::Settlement(event) => event
            .transfers()
            .iter()
            .map(|transfer| transfer.amount)
            .sum::<Money>()
            .to_string(),
    }
}
