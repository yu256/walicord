use walicord_application::{PersonBalance, SettlementOptimizationError, SettlementOptimizer};
use walicord_calc::{PersonBalance as CalcBalance, SettlementError, minimize_transactions};
use walicord_domain::{Money, Transfer};

#[derive(Default)]
pub struct WalicordSettlementOptimizer;

impl SettlementOptimizer for WalicordSettlementOptimizer {
    fn optimize(
        &self,
        balances: &[PersonBalance],
    ) -> Result<Vec<Transfer>, SettlementOptimizationError> {
        let calc_balances = balances.iter().map(|balance| CalcBalance {
            id: balance.id.0,
            balance: balance.balance.amount(),
        });

        let settlements =
            minimize_transactions(calc_balances, 1.0, 0.001).map_err(|err| match err {
                SettlementError::ImbalancedTotal(total) => {
                    SettlementOptimizationError::ImbalancedTotal(total)
                }
                SettlementError::NoSolution => SettlementOptimizationError::NoSolution,
                SettlementError::RoundingMismatch => SettlementOptimizationError::RoundingMismatch,
            })?;

        let optimized_transfers = settlements
            .iter()
            .map(|payment| Transfer {
                from: walicord_domain::model::MemberId(payment.from),
                to: walicord_domain::model::MemberId(payment.to),
                amount: Money::from_i64(payment.amount),
            })
            .collect();

        Ok(optimized_transfers)
    }
}
