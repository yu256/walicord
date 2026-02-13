use walicord_application::{PersonBalance, SettlementOptimizationError, SettlementOptimizer};
use walicord_calc::{PersonBalance as CalcBalance, SettlementError, minimize_transactions};
use walicord_domain::{AtomicUnitConversionError, Money, SettlementContext, Transfer};

#[derive(Default)]
pub struct WalicordSettlementOptimizer;

fn map_calc_settlement_error(err: SettlementError) -> SettlementOptimizationError {
    match err {
        SettlementError::ImbalancedTotal(total) => {
            SettlementOptimizationError::ImbalancedTotal(total)
        }
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
        context: SettlementContext,
    ) -> Result<Vec<Transfer>, SettlementOptimizationError> {
        let calc_balances = balances.iter().map(|balance| {
            let amount = context
                .to_atomic_units_i64(balance.balance)
                .map_err(map_atomic_unit_error);
            amount.map(|value| CalcBalance {
                id: balance.id.0,
                balance: value,
            })
        });

        let calc_balances: Result<Vec<_>, _> = calc_balances.collect();
        let calc_balances = calc_balances?;

        let settlements =
            minimize_transactions(calc_balances, 1.0, 0.001).map_err(map_calc_settlement_error)?;

        let optimized_transfers = settlements
            .iter()
            .map(|payment| Transfer {
                from: walicord_domain::model::MemberId(payment.from),
                to: walicord_domain::model::MemberId(payment.to),
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
}
