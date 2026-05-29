use std::{collections::HashMap, fmt::Write as _};

use walicord_application::{
    ledger::{
        AllocationSnapshot, BalanceAdjusted, LedgerEffectiveDate, LedgerEntry, LedgerEntryId,
        LedgerEvent, LedgerId, LedgerState, projection::VerifiedLedgerEntryView,
    },
    settle_up::PreviewedSettlement,
};
use walicord_domain::Money;
use walicord_i18n as i18n;

use super::{
    member_labels::SurfaceMemberLabels,
    sanitizer::{BusinessDateTime, SafeLiteralText},
    surfaces::{
        BalanceAdjustmentSummary, BalanceDirection, BalanceImpactRow, BalanceRow,
        LedgerSurfaceSummary, ParticipantShareRow, RecoveryReference, SealedRangeSummary,
        TransferRow, VoidedEntryRow,
    },
};

pub fn balance_rows_for_state(state: &LedgerState, labels: &SurfaceMemberLabels) -> Vec<BalanceRow> {
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
) -> Vec<SafeLiteralText> {
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

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ReadViewBuildError {
    #[error("void target {target_id:?} is missing from the verified load")]
    MissingVoidTarget { target_id: LedgerEntryId },
    #[error("settlement entry {entry_id:?} has no transfers")]
    EmptySettlement { entry_id: LedgerEntryId },
    #[error("sealed-through entry {through_id:?} is missing from the verified load")]
    MissingSealedThrough { through_id: LedgerEntryId },
}

pub fn summary_for_view(
    view: &VerifiedLedgerEntryView,
    labels: &SurfaceMemberLabels,
) -> Result<LedgerSurfaceSummary, ReadViewBuildError> {
    let entry = view.entry();
    match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => Ok(LedgerSurfaceSummary::Expense {
            date: entry
                .metadata
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
            note: event.note().and_then(|note| SafeLiteralText::from_note(note.as_str())),
        }),
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
            let Some(first) = event.transfers().first() else {
                return Err(ReadViewBuildError::EmptySettlement { entry_id: entry.id });
            };
            Ok(LedgerSurfaceSummary::Settlement {
                date: LedgerEffectiveDate::from_system_time(view.recorded_at()),
                from_display_name: labels.safe_member_label(first.from),
                to_display_name: labels.safe_member_label(first.to),
                amount: first.amount.to_string(),
                additional_transfers: event.transfers().len().saturating_sub(1),
            })
        }
        LedgerEvent::EntryVoided(_) => Ok(LedgerSurfaceSummary::Void {
            date: LedgerEffectiveDate::from_system_time(view.recorded_at()),
            voider_display_name: labels.safe_actor_label(entry),
            recorded_at: BusinessDateTime::from_system_time(view.recorded_at()),
        }),
        LedgerEvent::LedgerHistorySealed(_) => Ok(LedgerSurfaceSummary::Sealed {
            date: LedgerEffectiveDate::from_system_time(view.recorded_at()),
            actor_display_name: labels.safe_actor_label(entry),
        }),
        LedgerEvent::BalanceAdjusted(event) => {
            let impacts = balance_adjustment_rows(event, labels);
            Ok(LedgerSurfaceSummary::BalanceAdjustment {
                date: LedgerEffectiveDate::from_system_time(view.recorded_at()),
                actor_display_name: labels.safe_actor_label(entry),
                impact_summary: SafeLiteralText::from_note(&impact_summary_text(&impacts))
                    .expect("impact summary should sanitize"),
            })
        }
    }
}

fn impact_summary_text(rows: &[BalanceImpactRow]) -> String {
    let mut out = String::new();
    for (idx, row) in rows.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        let _ = match row.direction {
            BalanceDirection::Receive => write!(
                out,
                "{}",
                i18n::impact_summary_receive(row.display_name.as_str(), &row.amount),
            ),
            BalanceDirection::Pay => write!(
                out,
                "{}",
                i18n::impact_summary_pay(row.display_name.as_str(), &row.amount),
            ),
        };
    }
    out
}

pub fn voided_entry_rows(
    views: &[VerifiedLedgerEntryView],
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
) -> Result<Vec<VoidedEntryRow>, ReadViewBuildError> {
    views
        .iter()
        .filter_map(|view| match &view.entry().event {
            LedgerEvent::EntryVoided(event) => Some((view, event.target())),
            _ => None,
        })
        .map(|(view, target_id)| {
            let target = views
                .iter()
                .find(|candidate| candidate.entry().id == target_id)
                .ok_or(ReadViewBuildError::MissingVoidTarget { target_id })?;
            Ok(VoidedEntryRow {
                void_entry_id: view.entry().id,
                voider_display_name: labels.safe_actor_label(view.entry()),
                voided_at: BusinessDateTime::from_system_time(view.recorded_at()),
                original_summary: summary_for_view(target, labels)?,
                recovery_reference: RecoveryReference {
                    ledger_id_short: format!("{:08x}", ledger_id.0),
                    entry_id: view.entry().id,
                    message_link: Some(view.message_link().to_owned()),
                },
            })
        })
        .collect()
}

pub fn sealed_range_summary(
    views: &[VerifiedLedgerEntryView],
    sealed_through: Option<LedgerEntryId>,
    labels: &SurfaceMemberLabels,
) -> Result<Option<SealedRangeSummary>, ReadViewBuildError> {
    let Some(through_entry_id) = sealed_through else {
        return Ok(None);
    };
    let through_view = views
        .iter()
        .find(|view| view.entry().id == through_entry_id)
        .ok_or(ReadViewBuildError::MissingSealedThrough {
            through_id: through_entry_id,
        })?;
    Ok(Some(SealedRangeSummary {
        through_entry_id,
        through_summary: summary_for_view(through_view, labels)?,
    }))
}

pub fn balance_adjustment_summaries(
    views: &[VerifiedLedgerEntryView],
    labels: &SurfaceMemberLabels,
) -> Vec<BalanceAdjustmentSummary> {
    views
        .iter()
        .filter_map(|view| {
            let LedgerEvent::BalanceAdjusted(event) = &view.entry().event else {
                return None;
            };
            Some(BalanceAdjustmentSummary {
                actor_display_name: labels.safe_actor_label(view.entry()),
                reason: SafeLiteralText::from_note(event.reason().as_str()).unwrap_or_else(|| {
                    SafeLiteralText::from_note(i18n::expense_note_none())
                        .expect("fallback reason should sanitize")
                }),
                impacts: balance_adjustment_rows(event, labels),
            })
        })
        .collect()
}

pub struct LedgerPageInputs<'a> {
    pub route: super::surfaces::ReadViewRoute,
    pub views: &'a [VerifiedLedgerEntryView],
    pub state: &'a LedgerState,
    pub labels: &'a SurfaceMemberLabels,
    pub ledger_id: LedgerId,
    pub uncertain_write: bool,
}

pub fn build_ledger_page_model(
    inputs: LedgerPageInputs<'_>,
) -> Result<super::surfaces::ReadViewPageModel, ReadViewBuildError> {
    use super::surfaces::{ReadViewKind, ReadViewPageModel};
    Ok(ReadViewPageModel {
        kind: ReadViewKind::Ledger,
        route: inputs.route,
        title: i18n::panel_ledger_button_label().to_owned(),
        uncertain_write: inputs.uncertain_write,
        balances: balance_rows_for_state(inputs.state, inputs.labels),
        participants: participant_names_for_state(inputs.state, inputs.labels),
        voided_entries: voided_entry_rows(inputs.views, inputs.labels, inputs.ledger_id)?,
        sealed_range: sealed_range_summary(
            inputs.views,
            inputs.state.sealed_through(),
            inputs.labels,
        )?,
        balance_adjustments: balance_adjustment_summaries(inputs.views, inputs.labels),
        ephemeral: true,
        ..Default::default()
    })
}

pub fn build_ledger_empty_page_model(
    route: super::surfaces::ReadViewRoute,
    uncertain_write: bool,
) -> super::surfaces::ReadViewPageModel {
    use super::surfaces::{ReadViewKind, ReadViewPageModel};
    ReadViewPageModel {
        kind: ReadViewKind::Ledger,
        route,
        title: i18n::panel_ledger_button_label().to_owned(),
        uncertain_write,
        empty_state: Some(i18n::ledger_empty_state().to_owned()),
        ephemeral: true,
        ..Default::default()
    }
}
