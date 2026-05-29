use std::collections::HashMap;

use walicord_application::{
    ledger::{AllocationSnapshot, BalanceAdjusted, LedgerEntry, LedgerEvent, LedgerState},
    settle_up::PreviewedSettlement,
};
use walicord_domain::Money;

use super::{
    member_labels::SurfaceMemberLabels,
    surfaces::{BalanceDirection, BalanceImpactRow, BalanceRow, ParticipantShareRow, TransferRow},
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

pub fn participant_names_for_state(
    state: &LedgerState,
    labels: &SurfaceMemberLabels,
) -> Vec<super::sanitizer::SafeLiteralText> {
    let mut participants = state
        .participants()
        .iter()
        .copied()
        .map(|member_id| (member_id, labels.safe_member_label(member_id)))
        .collect::<Vec<_>>();
    participants.sort_by(|(lhs_id, _), (rhs_id, _)| labels.compare_members(*lhs_id, *rhs_id));
    participants.into_iter().map(|(_, label)| label).collect()
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

pub fn public_participant_rows(
    entry: &LedgerEntry,
    labels: &SurfaceMemberLabels,
) -> Vec<ParticipantShareRow> {
    let LedgerEvent::ExpenseRecorded(event) = &entry.event else {
        return Vec::new();
    };
    let owed_by = event
        .owed_by()
        .iter()
        .map(|owed| (owed.member_id, owed.amount))
        .collect::<HashMap<_, _>>();
    let mut rows = match &entry.metadata.allocation_snapshot {
        Some(AllocationSnapshot::Weighted { resolved_weights }) => resolved_weights
            .iter()
            .map(|member_weight| {
                (
                    member_weight.member_id,
                    ParticipantShareRow {
                        display_name: labels.safe_member_label(member_weight.member_id),
                        share_amount: owed_by
                            .get(&member_weight.member_id)
                            .copied()
                            .unwrap_or(Money::ZERO)
                            .to_string(),
                    },
                )
            })
            .collect::<Vec<_>>(),
        _ => event
            .owed_by()
            .iter()
            .map(|owed| {
                (
                    owed.member_id,
                    ParticipantShareRow {
                        display_name: labels.safe_member_label(owed.member_id),
                        share_amount: owed.amount.to_string(),
                    },
                )
            })
            .collect::<Vec<_>>(),
    };
    rows.sort_by(|(lhs_id, _), (rhs_id, _)| labels.compare_members(*lhs_id, *rhs_id));
    rows.into_iter().map(|(_, row)| row).collect()
}

pub fn balance_adjustment_rows(
    event: &BalanceAdjusted,
    labels: &SurfaceMemberLabels,
) -> Vec<BalanceImpactRow> {
    let mut rows = event
        .adjustments()
        .iter()
        .map(|adjustment| {
            let direction = if adjustment.amount >= Money::ZERO {
                BalanceDirection::Receive
            } else {
                BalanceDirection::Pay
            };
            let magnitude = if adjustment.amount >= Money::ZERO {
                adjustment.amount
            } else {
                -adjustment.amount
            };
            (
                adjustment.member_id,
                BalanceImpactRow {
                    display_name: labels.safe_member_label(adjustment.member_id),
                    amount: magnitude.to_string(),
                    direction,
                },
            )
        })
        .collect::<Vec<_>>();
    rows.sort_by_key(|(member_id, _)| *member_id);
    rows.into_iter().map(|(_, row)| row).collect()
}
