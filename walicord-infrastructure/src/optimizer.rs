use walicord_application::{PersonBalance, SettlementOptimizationError, SettlementOptimizer};
use walicord_calc::{PersonBalance as CalcBalance, SettlementError, minimize_transactions};
use walicord_domain::{Money, Transfer};

#[derive(Default)]
pub struct WalicordSettlementOptimizer;

impl SettlementOptimizer for WalicordSettlementOptimizer {
    fn optimize<'a>(
        &self,
        balances: &[PersonBalance<'a>],
    ) -> Result<Vec<Transfer<'a>>, SettlementOptimizationError> {
        let calc_balances = balances.iter().map(|balance| CalcBalance {
            name: balance.name,
            balance: balance.balance.amount(),
        });

        let settlements =
            minimize_transactions(calc_balances, 1.0, 0.001).map_err(|err| match err {
                SettlementError::ImbalancedTotal(total) => {
                    SettlementOptimizationError::ImbalancedTotal(total)
                }
                SettlementError::NoSolution => SettlementOptimizationError::NoSolution,
            })?;

        let optimized_transfers = settlements
            .iter()
            .map(|payment| Transfer {
                from: payment.from,
                to: payment.to,
                amount: Money::from_i64(payment.amount),
            })
            .collect();

        Ok(optimized_transfers)
    }
}
