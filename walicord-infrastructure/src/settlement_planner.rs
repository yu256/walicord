use walicord_application::ports::SettlementPlanner;
use walicord_domain::{
    AtomicUnitConversionError, MemberBalances, Money, Settlement, SettlementContext,
    SettlementRoundingError, Transfer, model::MemberId,
};
use walicord_transfer_construction::{
    HighsCommonSolveOptions, PersonBalance, SettlementError, SettlementTransferOptions,
    construct_settlement_transfers_with_options,
};

const CASH_GRID_G1_JPY: i64 = 1000;
const CASH_GRID_G2_JPY: i64 = 100;

/// HiGHS-backed implementation of `SettlementPlanner`. Boundary translation between
/// `Money`/`MemberBalances` and the solver's atomic-i64 representation lives here so the
/// application layer stays solver-agnostic.
#[derive(Default)]
pub struct HighsSettlementPlanner;

impl SettlementPlanner for HighsSettlementPlanner {
    fn plan(
        &self,
        balances: MemberBalances,
        settle_members: &[MemberId],
        cash_members: &[MemberId],
        context: SettlementContext,
    ) -> Result<Settlement, SettlementRoundingError> {
        if settle_members.is_empty() {
            return Ok(Settlement {
                new_balances: balances,
                transfers: Vec::new(),
            });
        }

        let calc_balances: Vec<PersonBalance<MemberId>> = balances
            .iter()
            .map(|(member, balance)| {
                context
                    .to_atomic_units_i64(*balance)
                    .map_err(|err| match err {
                        AtomicUnitConversionError::NonIntegral
                        | AtomicUnitConversionError::OutOfRange => {
                            SettlementRoundingError::NonIntegral
                        }
                        AtomicUnitConversionError::UnsupportedScale {
                            scale,
                            max_supported,
                        } => SettlementRoundingError::UnsupportedScale {
                            scale,
                            max_supported,
                        },
                    })
                    .and_then(|atomic| {
                        atomic
                            .checked_neg()
                            .map(|balance| PersonBalance {
                                id: *member,
                                balance,
                            })
                            .ok_or(SettlementRoundingError::NonIntegral)
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let options = SettlementTransferOptions::default().with_solve(
            walicord_transfer_construction::TransferSolveOptions {
                highs: HighsCommonSolveOptions {
                    time_limit_seconds: Some(30.0),
                    accept_feasible_on_limit: true,
                    ..HighsCommonSolveOptions::default()
                },
                ..Default::default()
            },
        );
        let payments = construct_settlement_transfers_with_options(
            calc_balances,
            settle_members,
            cash_members,
            CASH_GRID_G1_JPY,
            CASH_GRID_G2_JPY,
            options,
        )
        .map_err(map_settlement_error)?;

        let mut transfers: Vec<Transfer> = payments
            .into_iter()
            .map(|payment| Transfer {
                from: payment.from,
                to: payment.to,
                amount: Money::new(payment.amount, context.scale),
            })
            .collect();
        transfers.sort_unstable_by_key(|transfer| (transfer.from, transfer.to));

        let mut new_balances = balances;
        for transfer in &transfers {
            if let Some(balance) = new_balances.get_mut(&transfer.from) {
                *balance += transfer.amount;
            }
            if let Some(balance) = new_balances.get_mut(&transfer.to) {
                *balance -= transfer.amount;
            }
        }

        debug_assert!(
            settle_members
                .iter()
                .all(|member| { new_balances.get(member).copied().is_none_or(Money::is_zero) })
        );

        Ok(Settlement {
            new_balances,
            transfers,
        })
    }
}

fn map_settlement_error(err: SettlementError) -> SettlementRoundingError {
    match err {
        SettlementError::ImbalancedTotal(total) => {
            SettlementRoundingError::TransferConstructionImbalancedTotal(total)
        }
        SettlementError::InvalidGrid { g1, g2 } => {
            SettlementRoundingError::TransferConstructionInvalidGrid { g1, g2 }
        }
        SettlementError::ModelTooLarge {
            edge_count,
            max_edges,
        } => SettlementRoundingError::TransferConstructionModelTooLarge {
            edge_count,
            max_edges,
        },
        SettlementError::NoSolution => SettlementRoundingError::TransferConstructionNoSolution,
        SettlementError::InvalidWeights { .. }
        | SettlementError::BalancesTooLargeForF64
        | SettlementError::NonFiniteSolution
        | SettlementError::OutOfRangeSolution
        | SettlementError::NonBinarySolution => {
            SettlementRoundingError::TransferConstructionNoSolution
        }
        SettlementError::RoundingMismatch => {
            SettlementRoundingError::TransferConstructionRoundingMismatch
        }
    }
}
