use walicord_application::{PersonBalance, SettlementOptimizationError, SettlementOptimizer};
use walicord_domain::{
    AtomicUnitConversionError, Money, SettlementContext, Transfer, model::MemberId,
};
use walicord_transfer_construction::{
    HighsCommonSolveOptions, PersonBalance as CalcBalance, SettlementError,
    SettlementTransferOptions, construct_settlement_transfers_with_options,
};

#[derive(Default)]
pub struct WalicordSettlementOptimizer;

fn map_calc_settlement_error(err: SettlementError) -> SettlementOptimizationError {
    match err {
        SettlementError::ImbalancedTotal(total) => {
            SettlementOptimizationError::ImbalancedTotal(total)
        }
        SettlementError::InvalidGrid { g1, g2 } => {
            SettlementOptimizationError::InvalidGrid { g1, g2 }
        }
        SettlementError::ModelTooLarge {
            edge_count,
            max_edges,
        } => SettlementOptimizationError::ModelTooLarge {
            edge_count,
            max_edges,
        },
        SettlementError::NoSolution => SettlementOptimizationError::NoSolution,
        SettlementError::RoundingMismatch => SettlementOptimizationError::RoundingMismatch,
    }
}

fn map_atomic_unit_error(err: AtomicUnitConversionError) -> SettlementOptimizationError {
    match err {
        AtomicUnitConversionError::NonIntegral => {
            SettlementOptimizationError::QuantizationNonIntegral
        }
        AtomicUnitConversionError::OutOfRange => {
            SettlementOptimizationError::QuantizationOutOfRange
        }
        AtomicUnitConversionError::UnsupportedScale {
            scale,
            max_supported,
        } => SettlementOptimizationError::QuantizationUnsupportedScale {
            scale,
            max_supported,
        },
    }
}

impl SettlementOptimizer for WalicordSettlementOptimizer {
    fn optimize(
        &self,
        balances: &[PersonBalance],
        settle_members: &[MemberId],
        cash_members: &[MemberId],
        context: SettlementContext,
    ) -> Result<Vec<Transfer>, SettlementOptimizationError> {
        let calc_balances = balances
            .iter()
            .map(
                |balance| match context.to_atomic_units_i64(balance.balance) {
                    Ok(value) => Ok(CalcBalance {
                        id: balance.id,
                        balance: value,
                    }),
                    Err(err) => Err(map_atomic_unit_error(err)),
                },
            )
            .collect::<Result<Vec<_>, SettlementOptimizationError>>()?;

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
        let settlements = construct_settlement_transfers_with_options(
            calc_balances,
            settle_members,
            cash_members,
            1000,
            100,
            options,
        )
        .map_err(map_calc_settlement_error)?;

        let optimized_transfers = settlements
            .iter()
            .map(|payment| Transfer {
                from: payment.from,
                to: payment.to,
                amount: Money::new(payment.amount, context.scale),
            })
            .collect();

        Ok(optimized_transfers)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::jpy_integer(Money::from_i64(42), 0, Ok(42))]
    #[case::usd_two_dp(Money::new(123, 2), 2, Ok(123))]
    #[case::usd_non_integral_units(
        Money::new(1234, 3),
        2,
        Err(AtomicUnitConversionError::NonIntegral)
    )]
    #[case::unsupported_scale(
        Money::from_i64(42),
        30,
        Err(AtomicUnitConversionError::UnsupportedScale {
            scale: 30,
            max_supported: 22,
        })
    )]
    #[case::out_of_range(
        Money::from_decimal("10000000000000000000".parse().expect("decimal")),
        0,
        Err(AtomicUnitConversionError::OutOfRange)
    )]
    fn to_atomic_units_i64_converts_by_scale(
        #[case] amount: Money,
        #[case] scale: u32,
        #[case] expected: Result<i64, AtomicUnitConversionError>,
    ) {
        let context = SettlementContext {
            scale,
            ..SettlementContext::jpy_default()
        };
        let converted = context.to_atomic_units_i64(amount);
        assert_eq!(converted, expected);
    }

    #[test]
    fn optimize_respects_settle_member_subset() {
        let optimizer = WalicordSettlementOptimizer;
        let context = SettlementContext::jpy_default();
        let balances = [
            PersonBalance {
                id: MemberId(1),
                balance: Money::from_i64(100),
            },
            PersonBalance {
                id: MemberId(2),
                balance: Money::from_i64(-100),
            },
            PersonBalance {
                id: MemberId(3),
                balance: Money::ZERO,
            },
        ];

        let subset = [MemberId(3)];
        let subset_result = optimizer
            .optimize(&balances, &subset, &[], context)
            .expect("subset optimization should succeed");
        assert!(
            subset_result.is_empty(),
            "non-target members should not be forced to settle"
        );

        let all = [MemberId(1), MemberId(2), MemberId(3)];
        let all_result = optimizer
            .optimize(&balances, &all, &[], context)
            .expect("full optimization should succeed");
        assert_eq!(all_result.len(), 1);
        assert_eq!(all_result[0].from, MemberId(1));
        assert_eq!(all_result[0].to, MemberId(2));
        assert_eq!(all_result[0].amount, Money::from_i64(100));
    }
}
