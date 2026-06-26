use walicord_application::{
    ledger::{
        LedgerEffectiveDate, LedgerEntryId, LedgerId, LedgerState,
        projection::{ExpenseOrSettlementEvent, ExpenseOrSettlementView},
    },
    settle_up::PreviewedSettlement,
};
use walicord_domain::Money;
use walicord_i18n as i18n;

use super::{
    member_labels::SurfaceMemberLabels,
    sanitizer::SafeLiteralText,
    surfaces::{
        BalanceDirection, BalanceRow, ExpenseOrSettlementSummary, ReadViewContent, RecentEntryRow,
        RecoveryReference, TransferRow,
    },
};

pub fn balance_rows_for_state(
    state: &LedgerState,
    labels: &SurfaceMemberLabels,
) -> Vec<BalanceRow> {
    let mut rows = state
        .balances()
        .iter()
        .map(|(member_id, amount)| {
            let direction = if *amount >= Money::ZERO {
                BalanceDirection::Receive
            } else {
                BalanceDirection::Pay
            };
            let magnitude = if *amount >= Money::ZERO {
                *amount
            } else {
                -*amount
            };
            (
                *member_id,
                BalanceRow {
                    display_name: labels.safe_member_label(*member_id),
                    amount: magnitude.to_string(),
                    direction,
                },
            )
        })
        .collect::<Vec<_>>();
    rows.sort_by(|(lhs_id, _), (rhs_id, _)| labels.compare_members(*lhs_id, *rhs_id));
    rows.into_iter().map(|(_, row)| row).collect()
}

pub fn preview_transfer_rows(
    previewed: &PreviewedSettlement,
    labels: &SurfaceMemberLabels,
) -> Vec<TransferRow> {
    previewed
        .plan()
        .transfers
        .iter()
        .map(|transfer| TransferRow {
            from_display_name: labels.safe_member_label(transfer.from),
            to_display_name: labels.safe_member_label(transfer.to),
            amount: transfer.amount.to_string(),
        })
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ReadViewBuildError {
    #[error("void target {target_id:?} is missing from the verified load")]
    MissingVoidTarget { target_id: LedgerEntryId },
    #[error("sealed-through entry {through_id:?} is missing from the verified load")]
    MissingSealedThrough { through_id: LedgerEntryId },
}

pub fn expense_or_settlement_summary(
    view: &ExpenseOrSettlementView,
    labels: &SurfaceMemberLabels,
) -> ExpenseOrSettlementSummary {
    match view.event() {
        ExpenseOrSettlementEvent::Expense(event) => ExpenseOrSettlementSummary::Expense {
            date: view
                .metadata()
                .effective_date
                .unwrap_or_else(|| LedgerEffectiveDate::from_system_time(view.recorded_at())),
            payer_display_name: event
                .paid_by()
                .first()
                .map(|paid| labels.safe_member_label(paid.member_id))
                .unwrap_or_else(super::member_labels::unknown_member_label),
            amount: event
                .paid_by()
                .iter()
                .map(|paid| paid.amount)
                .sum::<Money>()
                .to_string(),
            note: event
                .note()
                .and_then(|note| SafeLiteralText::from_note(note.as_str())),
        },
        ExpenseOrSettlementEvent::Settlement(event) => {
            let first = event.transfers().first();
            ExpenseOrSettlementSummary::Settlement {
                date: LedgerEffectiveDate::from_system_time(view.recorded_at()),
                from_display_name: labels.safe_member_label(first.from),
                to_display_name: labels.safe_member_label(first.to),
                amount: first.amount.to_string(),
                additional_transfers: event.transfers().len().saturating_sub(1),
            }
        }
    }
}

pub fn recent_entry_rows(
    views: &[ExpenseOrSettlementView],
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
) -> Vec<RecentEntryRow> {
    views
        .iter()
        .map(|view| RecentEntryRow {
            entry_id: view.entry_id(),
            summary: expense_or_settlement_summary(view, labels),
            recovery_reference: RecoveryReference {
                ledger_id_short: format!("{ledger_id:08x}"),
                entry_id: view.entry_id(),
                message_link: Some(view.message_link().to_owned()),
            },
        })
        .collect()
}

pub struct LedgerPageInputs<'a> {
    pub route: super::surfaces::ReadViewRoute,
    pub recent_views: &'a [ExpenseOrSettlementView],
    pub state: &'a LedgerState,
    pub labels: &'a SurfaceMemberLabels,
    pub ledger_id: LedgerId,
    pub uncertain_write: bool,
}

pub fn build_ledger_page_model(
    inputs: LedgerPageInputs<'_>,
) -> Result<super::surfaces::ReadViewPageModel, ReadViewBuildError> {
    use super::surfaces::ReadViewPageModel;
    Ok(ReadViewPageModel {
        route: inputs.route,
        title: std::borrow::Cow::Borrowed(i18n::PANEL_LEDGER_BUTTON_LABEL),
        uncertain_write: inputs.uncertain_write,
        stale_page: false,
        page_indicator: None,
        snapshot_notice: None,
        route_guidance_lines: Vec::new(),
        recovery_cta: super::surfaces::RecoveryCta::None,
        recovery_url: None,
        missing_thread_note: false,
        balances: Some(balance_rows_for_state(inputs.state, inputs.labels)),
        footer_lines: Vec::new(),
        empty_state: None,
        action_rows: Vec::new(),
        ephemeral: true,
        content: ReadViewContent::Ledger {
            recent_entries: Some(recent_entry_rows(
                inputs.recent_views,
                inputs.labels,
                inputs.ledger_id,
            )),
        },
    })
}

pub fn build_ledger_empty_page_model(
    route: super::surfaces::ReadViewRoute,
    uncertain_write: bool,
) -> super::surfaces::ReadViewPageModel {
    use super::surfaces::ReadViewPageModel;
    ReadViewPageModel {
        route,
        title: std::borrow::Cow::Borrowed(i18n::PANEL_LEDGER_BUTTON_LABEL),
        uncertain_write,
        stale_page: false,
        page_indicator: None,
        snapshot_notice: None,
        route_guidance_lines: Vec::new(),
        recovery_cta: super::surfaces::RecoveryCta::None,
        recovery_url: None,
        missing_thread_note: false,
        balances: Some(Vec::new()),
        footer_lines: Vec::new(),
        empty_state: Some(std::borrow::Cow::Borrowed(i18n::LEDGER_EMPTY_STATE)),
        action_rows: Vec::new(),
        ephemeral: true,
        content: ReadViewContent::Ledger {
            recent_entries: Some(Vec::new()),
        },
    }
}

pub struct ReviewPageInputs<'a> {
    pub route: super::surfaces::ReadViewRoute,
    pub state: &'a LedgerState,
    pub previewed: &'a PreviewedSettlement,
    pub labels: &'a SurfaceMemberLabels,
    pub uncertain_write: bool,
    pub recovery_cta: super::surfaces::RecoveryCta,
    pub recovery_url: Option<String>,
}

pub fn build_review_page_model(inputs: ReviewPageInputs<'_>) -> super::surfaces::ReadViewPageModel {
    use super::surfaces::ReadViewPageModel;
    ReadViewPageModel {
        route: inputs.route,
        title: std::borrow::Cow::Borrowed(i18n::PANEL_REVIEW_BUTTON_LABEL),
        uncertain_write: inputs.uncertain_write,
        stale_page: false,
        page_indicator: None,
        snapshot_notice: None,
        route_guidance_lines: Vec::new(),
        recovery_cta: inputs.recovery_cta,
        recovery_url: inputs.recovery_url,
        missing_thread_note: false,
        balances: Some(balance_rows_for_state(inputs.state, inputs.labels)),
        footer_lines: Vec::new(),
        empty_state: None,
        action_rows: Vec::new(),
        ephemeral: true,
        content: ReadViewContent::Review {
            transfers: Some(preview_transfer_rows(inputs.previewed, inputs.labels)),
        },
    }
}

pub fn build_review_empty_page_model(
    route: super::surfaces::ReadViewRoute,
    uncertain_write: bool,
    recovery_url: Option<String>,
) -> super::surfaces::ReadViewPageModel {
    use super::surfaces::{ReadViewPageModel, RecoveryCta};
    let (empty_state, recovery_cta, recovery_url) = match route {
        super::surfaces::ReadViewRoute::ReviewParent => (
            std::borrow::Cow::Borrowed(i18n::REVIEW_PARENT_EMPTY_STATE),
            RecoveryCta::None,
            None,
        ),
        _ => (
            std::borrow::Cow::Borrowed(i18n::REVIEW_THREAD_EMPTY_STATE),
            RecoveryCta::ParentLink,
            recovery_url,
        ),
    };
    ReadViewPageModel {
        route,
        title: std::borrow::Cow::Borrowed(i18n::PANEL_REVIEW_BUTTON_LABEL),
        uncertain_write,
        stale_page: false,
        page_indicator: None,
        snapshot_notice: None,
        route_guidance_lines: Vec::new(),
        recovery_cta,
        recovery_url,
        missing_thread_note: false,
        balances: Some(Vec::new()),
        footer_lines: Vec::new(),
        empty_state: Some(empty_state),
        action_rows: Vec::new(),
        ephemeral: true,
        content: ReadViewContent::Review {
            transfers: Some(Vec::new()),
        },
    }
}

pub fn build_review_no_transfers_page_model(
    route: super::surfaces::ReadViewRoute,
    uncertain_write: bool,
) -> super::surfaces::ReadViewPageModel {
    use super::surfaces::ReadViewPageModel;
    use std::fmt::Write as _;
    let mut body = String::new();
    let _ = write!(
        body,
        "{}\n{}",
        i18n::SETTLEMENT_ALREADY_NOT_NEEDED_MESSAGE,
        i18n::SETTLEMENT_PREVIEW_NOT_SAVED_MESSAGE,
    );
    ReadViewPageModel {
        route,
        title: std::borrow::Cow::Borrowed(i18n::PANEL_REVIEW_BUTTON_LABEL),
        uncertain_write,
        stale_page: false,
        page_indicator: None,
        snapshot_notice: None,
        route_guidance_lines: Vec::new(),
        recovery_cta: super::surfaces::RecoveryCta::None,
        recovery_url: None,
        missing_thread_note: false,
        balances: Some(Vec::new()),
        footer_lines: Vec::new(),
        empty_state: Some(std::borrow::Cow::Owned(body)),
        action_rows: Vec::new(),
        ephemeral: true,
        content: ReadViewContent::Review {
            transfers: Some(Vec::new()),
        },
    }
}
