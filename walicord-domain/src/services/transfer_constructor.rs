use crate::{
    model::{MemberBalances, MemberId, Money, Settlement, Transfer},
    services::{AtomicUnitConversionError, SettlementContext, SettlementRoundingError},
};
use walicord_transfer_construction::{
    HighsCommonSolveOptions, PersonBalance, SettlementError, SettlementTransferOptions,
    construct_settlement_transfers_with_options,
};

const CASH_GRID_G1_JPY: i64 = 1000;
const CASH_GRID_G2_JPY: i64 = 100;

/// Transfer-construction service for settle-up flows.
pub struct TransferConstructor;

impl TransferConstructor {
    /// Calculate settlement for specified members
    pub fn calculate(
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

        // Invariant for deterministic transfer tie-break:
        // `MemberBalances` is a BTreeMap keyed by MemberId, so iteration order is stable.
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
                    .map(|atomic| PersonBalance {
                        id: *member,
                        balance: atomic,
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;

        // NOTE: cash-divisibility objectives are intentionally fixed to JPY-style
        // 1000/100-unit grid in the current domain service.
        // TODO: derive this from SettlementContext when non-JPY settlement profiles
        // are supported end-to-end.
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
        .map_err(SettlementRoundingError::from)?;

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
                *balance -= transfer.amount;
            }
            if let Some(balance) = new_balances.get_mut(&transfer.to) {
                *balance += transfer.amount;
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

impl From<SettlementError> for SettlementRoundingError {
    fn from(err: SettlementError) -> Self {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        model::{
            BalanceAccumulator, MemberBalances, MemberSetExpr, MemberSetOp, Payment, Statement,
        },
        services::SettlementContext,
    };
    use rstest::{fixture, rstest};

    #[fixture]
    fn constructor() -> TransferConstructor {
        TransferConstructor
    }

    #[rstest]
    #[case::simple_positive_balance(
        MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(-100)),
        ]),
        &[MemberId(1)],
        &[],
        MemberBalances::from_iter([(MemberId(1), Money::ZERO), (MemberId(2), Money::ZERO)]),
        vec![(MemberId(1), MemberId(2), 100)]
    )]
    #[case::simple_negative_balance(
        MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(-100)),
            (MemberId(2), Money::from_i64(100)),
        ]),
        &[MemberId(1)],
        &[],
        MemberBalances::from_iter([(MemberId(1), Money::ZERO), (MemberId(2), Money::ZERO)]),
        vec![(MemberId(2), MemberId(1), 100)]
    )]
    #[case::zero_balance(
        MemberBalances::from_iter([(MemberId(1), Money::ZERO), (MemberId(2), Money::ZERO)]),
        &[MemberId(1)],
        &[],
        MemberBalances::from_iter([(MemberId(1), Money::ZERO), (MemberId(2), Money::ZERO)]),
        vec![]
    )]
    #[case::multiple_settle_members(
        MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(100)),
            (MemberId(3), Money::from_i64(-200)),
        ]),
        &[MemberId(1), MemberId(2)],
        &[],
        MemberBalances::from_iter([
            (MemberId(1), Money::ZERO),
            (MemberId(2), Money::ZERO),
            (MemberId(3), Money::ZERO),
        ]),
        vec![(MemberId(1), MemberId(3), 100), (MemberId(2), MemberId(3), 100)]
    )]
    #[case::cross_group_transfer(
        MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(-50)),
            (MemberId(3), Money::from_i64(-50)),
        ]),
        &[MemberId(1)],
        &[],
        MemberBalances::from_iter([
            (MemberId(1), Money::ZERO),
            (MemberId(2), Money::ZERO),
            (MemberId(3), Money::ZERO),
        ]),
        vec![(MemberId(1), MemberId(2), 50), (MemberId(1), MemberId(3), 50)]
    )]
    #[case::empty_settle_members(
        MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(-100)),
        ]),
        &[],
        &[],
        MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(-100)),
        ]),
        vec![]
    )]
    #[case::missing_balance_member(
        MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(-100)),
            (MemberId(3), Money::ZERO),
        ]),
        &[MemberId(1), MemberId(3)],
        &[],
        MemberBalances::from_iter([
            (MemberId(1), Money::ZERO),
            (MemberId(2), Money::ZERO),
            (MemberId(3), Money::ZERO),
        ]),
        vec![(MemberId(1), MemberId(2), 100)]
    )]
    fn transfer_constructor_cases(
        constructor: TransferConstructor,
        #[case] balances: MemberBalances,
        #[case] settle_members: &[MemberId],
        #[case] cash_members: &[MemberId],
        #[case] expected_balances: MemberBalances,
        #[case] expected_transfers: Vec<(MemberId, MemberId, i64)>,
    ) {
        let result = constructor
            .calculate(
                balances,
                settle_members,
                cash_members,
                SettlementContext::jpy_default(),
            )
            .expect("transfer construction should succeed");

        assert_eq!(result.new_balances, expected_balances);

        let expected: Vec<Transfer> = expected_transfers
            .into_iter()
            .map(|(from, to, amount)| Transfer {
                from,
                to,
                amount: Money::from_i64(amount),
            })
            .collect();
        assert_eq!(result.transfers, expected);
    }

    #[test]
    fn prefers_internal_counterparts_after_cash_objectives() {
        let constructor = TransferConstructor;
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(1200)),
            (MemberId(2), Money::from_i64(-1000)),
            (MemberId(3), Money::from_i64(-200)),
            (MemberId(4), Money::from_i64(0)),
        ]);

        let result = constructor
            .calculate(
                balances,
                &[MemberId(1), MemberId(2), MemberId(3)],
                &[MemberId(1)],
                SettlementContext::jpy_default(),
            )
            .expect("cash-aware transfer construction should succeed");

        assert_eq!(
            result.transfers,
            vec![
                Transfer {
                    from: MemberId(1),
                    to: MemberId(2),
                    amount: Money::from_i64(1000),
                },
                Transfer {
                    from: MemberId(1),
                    to: MemberId(3),
                    amount: Money::from_i64(200),
                },
            ]
        );
    }

    #[test]
    fn payment_sign_convention_flows_to_transfer_direction() {
        let mut accumulator = BalanceAccumulator::new_with_members(&[MemberId(1), MemberId(2)]);
        accumulator.apply(&Statement::Payment(Payment::even(
            Money::from_i64(100),
            MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
            MemberSetExpr::new([MemberSetOp::Push(MemberId(2))]),
        )));
        let balances = accumulator.into_balances();

        assert_eq!(balances.get(&MemberId(1)), Some(&Money::from_i64(100)));
        assert_eq!(balances.get(&MemberId(2)), Some(&Money::from_i64(-100)));

        let result = TransferConstructor
            .calculate(
                balances,
                &[MemberId(1), MemberId(2)],
                &[],
                SettlementContext::jpy_default(),
            )
            .expect("transfer construction should succeed");

        assert_eq!(result.transfers.len(), 1);
        assert_eq!(result.transfers[0].from, MemberId(1));
        assert_eq!(result.transfers[0].to, MemberId(2));
        assert_eq!(result.transfers[0].amount, Money::from_i64(100));
    }

    #[test]
    fn partial_settlement_zeroes_target_members_only() {
        let constructor = TransferConstructor;
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(60)),
            (MemberId(2), Money::from_i64(100)),
            (MemberId(3), Money::from_i64(-160)),
        ]);

        let result = constructor
            .calculate(
                balances,
                &[MemberId(1)],
                &[],
                SettlementContext::jpy_default(),
            )
            .expect("partial transfer construction should succeed");

        assert_eq!(result.new_balances.get(&MemberId(1)), Some(&Money::ZERO));
        assert_eq!(
            result.new_balances.get(&MemberId(2)),
            Some(&Money::from_i64(100))
        );
        assert_eq!(
            result.new_balances.get(&MemberId(3)),
            Some(&Money::from_i64(-100))
        );
    }

    #[test]
    fn supports_non_zero_scale_balances() {
        let constructor = TransferConstructor;
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::new(123, 2)),
            (MemberId(2), Money::new(-123, 2)),
        ]);
        let context = SettlementContext {
            scale: 2,
            ..SettlementContext::jpy_default()
        };

        let result = constructor
            .calculate(balances, &[MemberId(1), MemberId(2)], &[], context)
            .expect("scale-aware transfer construction should succeed");

        assert_eq!(
            result.transfers,
            vec![Transfer {
                from: MemberId(1),
                to: MemberId(2),
                amount: Money::new(123, 2),
            }]
        );
        assert_eq!(result.new_balances.get(&MemberId(1)), Some(&Money::ZERO));
        assert_eq!(result.new_balances.get(&MemberId(2)), Some(&Money::ZERO));
    }
}
