use walicord_application::{
    ledger::{
        LedgerEffectiveDate, LedgerEntryId, LedgerId, LedgerState,
        projection::{ExpenseOrSettlementEvent, ExpenseOrSettlementView},
    },
    settle_up::PreviewedSettlement,
};
use walicord_domain::{Money, NonEmptyVec};

use super::{
    member_labels::{SurfaceMemberLabels, unknown_member_label},
    sanitizer::{SafeLiteralText, SafeLiteralTextError},
    surfaces::{
        BalanceDirection, BalanceRow, ExpenseOrSettlementSummary, LedgerRoute, ReadViewDocument,
        RecentEntryRow, RecoveryAction, RecoveryReference, ReviewRoute, ReviewSettleAction,
        Section, TransferRow,
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

fn balance_section(state: &LedgerState, labels: &SurfaceMemberLabels) -> Section<BalanceRow> {
    let rows = balance_rows_for_state(state, labels);
    Section::from_rows(rows)
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
    #[error("safe literal text: {0}")]
    SafeLiteral(#[from] SafeLiteralTextError),
}

pub fn expense_or_settlement_summary(
    view: &ExpenseOrSettlementView,
    labels: &SurfaceMemberLabels,
) -> Result<ExpenseOrSettlementSummary, ReadViewBuildError> {
    match view.event() {
        ExpenseOrSettlementEvent::Expense(event) => {
            let payer_display_name = match event.paid_by().first() {
                Some(paid) => labels.safe_member_label(paid.member_id),
                None => unknown_member_label(),
            };
            Ok(ExpenseOrSettlementSummary::Expense {
                date: match view.metadata().effective_date {
                    Some(effective_date) => effective_date,
                    None => LedgerEffectiveDate::from_system_time(view.recorded_at()),
                },
                payer_display_name,
                amount: event
                    .paid_by()
                    .iter()
                    .map(|paid| paid.amount)
                    .sum::<Money>()
                    .to_string(),
                note: event
                    .note()
                    .map(|note| SafeLiteralText::parse_note(note.as_str()))
                    .transpose()?,
            })
        }
        ExpenseOrSettlementEvent::Settlement(event) => {
            let first = event.transfers().first();
            Ok(ExpenseOrSettlementSummary::Settlement {
                date: LedgerEffectiveDate::from_system_time(view.recorded_at()),
                from_display_name: labels.safe_member_label(first.from),
                to_display_name: labels.safe_member_label(first.to),
                amount: first.amount.to_string(),
                additional_transfers: event.transfers().len().saturating_sub(1),
            })
        }
    }
}

pub fn recent_entry_rows(
    views: &[ExpenseOrSettlementView],
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
) -> Result<Vec<RecentEntryRow>, ReadViewBuildError> {
    views
        .iter()
        .map(|view| {
            Ok(RecentEntryRow {
                entry_id: view.entry_id(),
                summary: expense_or_settlement_summary(view, labels)?,
                recovery_reference: RecoveryReference {
                    ledger_id_short: format!("{ledger_id:08x}"),
                    entry_id: view.entry_id(),
                    message_link: Some(view.message_link().to_owned()),
                },
            })
        })
        .collect()
}

fn recent_entries_section(
    views: &[ExpenseOrSettlementView],
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
) -> Result<Section<RecentEntryRow>, ReadViewBuildError> {
    let rows = recent_entry_rows(views, labels, ledger_id)?;
    Ok(Section::from_rows(rows))
}

pub struct LedgerPageInputs<'a> {
    pub route: LedgerRoute,
    pub recent_views: &'a [ExpenseOrSettlementView],
    pub state: &'a LedgerState,
    pub labels: &'a SurfaceMemberLabels,
    pub ledger_id: LedgerId,
    pub uncertain_write: bool,
}

pub fn build_ledger_document(
    inputs: LedgerPageInputs<'_>,
) -> Result<ReadViewDocument, ReadViewBuildError> {
    Ok(ReadViewDocument::ledger(
        inputs.route,
        balance_section(inputs.state, inputs.labels),
        recent_entries_section(inputs.recent_views, inputs.labels, inputs.ledger_id)?,
        inputs.uncertain_write,
    ))
}

pub struct ReviewPageInputs<'a> {
    pub route: ReviewRoute,
    pub state: &'a LedgerState,
    pub previewed: &'a PreviewedSettlement,
    pub labels: &'a SurfaceMemberLabels,
    pub uncertain_write: bool,
    pub recovery_action: RecoveryAction,
    pub settle_action: ReviewSettleAction,
}

pub fn build_review_document(inputs: ReviewPageInputs<'_>) -> ReadViewDocument {
    let transfer_rows = preview_transfer_rows(inputs.previewed, inputs.labels);
    match NonEmptyVec::new(transfer_rows) {
        Ok(transfers) => ReadViewDocument::review(
            inputs.route,
            balance_section(inputs.state, inputs.labels),
            transfers,
            inputs.uncertain_write,
            inputs.recovery_action,
            inputs.settle_action,
        ),
        Err(_) => ReadViewDocument::review_no_transfers(
            inputs.route,
            inputs.uncertain_write,
            inputs.recovery_action,
        ),
    }
}
