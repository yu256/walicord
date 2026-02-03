use crate::model::{Money, Settlement, Transfer};
use std::collections::{HashMap, HashSet};

/// Settlement calculation service
pub struct SettlementCalculator;

impl SettlementCalculator {
    /// Calculate settlement for specified members
    ///
    /// Takes ownership of balances and returns new state.
    ///
    /// # Arguments
    /// * `balances` - Current balance table
    /// * `participants` - List of all participants
    /// * `settle_members` - Members to settle
    ///
    /// # Returns
    /// New balances after settlement and transfer list
    pub fn calculate<'a>(
        &self,
        balances: HashMap<&'a str, Money>,
        participants: &[&'a str],
        settle_members: &[&'a str],
    ) -> Settlement<'a> {
        let mut working_balances = balances;
        let mut transfers = Vec::new();
        let settle_lookup: HashSet<&str> = settle_members.iter().copied().collect();

        for &member in settle_members {
            let balance = match working_balances.get(member).copied() {
                Some(b) if !b.is_zero() => b,
                _ => continue,
            };

            let (from_member, to_member, target_sign) = if balance.amount() > 0 {
                (true, false, -1)
            } else {
                (false, true, 1)
            };

            let mut remaining = balance.abs();
            let mut transferred = 0;

            for &other in settle_members {
                if other == member {
                    continue;
                }
                if let Some(other_balance) = working_balances.get_mut(other) {
                    let can_transfer = if target_sign < 0 {
                        other_balance.amount() < 0
                    } else {
                        other_balance.amount() > 0
                    };
                    if !can_transfer {
                        continue;
                    }

                    let available = other_balance.abs();
                    let amount = remaining.min(available);
                    if amount > 0 {
                        *other_balance -= Money::from_i64(target_sign * amount);
                        remaining -= amount;
                        transferred += amount;
                        transfers.push(Transfer {
                            from: if from_member { member } else { other },
                            to: if to_member { member } else { other },
                            amount: Money::from_i64(amount),
                        });
                    }
                    if remaining == 0 {
                        break;
                    }
                }
            }

            if remaining > 0 {
                for &other in participants {
                    if other == member || settle_lookup.contains(other) {
                        continue;
                    }
                    if let Some(other_balance) = working_balances.get_mut(other) {
                        let can_transfer = if target_sign < 0 {
                            other_balance.amount() < 0
                        } else {
                            other_balance.amount() > 0
                        };
                        if !can_transfer {
                            continue;
                        }

                        let available = other_balance.abs();
                        let amount = remaining.min(available);
                        if amount > 0 {
                            *other_balance -= Money::from_i64(target_sign * amount);
                            remaining -= amount;
                            transferred += amount;
                            transfers.push(Transfer {
                                from: if from_member { member } else { other },
                                to: if to_member { member } else { other },
                                amount: Money::from_i64(amount),
                            });
                        }
                        if remaining == 0 {
                            break;
                        }
                    }
                }
            }

            if let Some(member_balance) = working_balances.get_mut(member) {
                *member_balance = if remaining == 0 {
                    Money::zero()
                } else {
                    Money::from_i64(balance.signum() * (balance.abs() - transferred))
                };
            }
            debug_assert_eq!(remaining, 0);
        }

        Settlement {
            new_balances: working_balances,
            transfers,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::{fixture, rstest};
    use std::collections::HashMap;

    #[fixture]
    fn calculator() -> SettlementCalculator {
        SettlementCalculator
    }

    #[rstest]
    #[case::simple_positive_balance(
        HashMap::from([("A", Money::from_i64(100)), ("B", Money::from_i64(-100))]),
        &["A", "B"],
        &["A"],
        HashMap::from([("A", Money::zero()), ("B", Money::zero())]),
        vec![("A", "B", 100)]
    )]
    #[case::simple_negative_balance(
        HashMap::from([("A", Money::from_i64(-100)), ("B", Money::from_i64(100))]),
        &["A", "B"],
        &["A"],
        HashMap::from([("A", Money::zero()), ("B", Money::zero())]),
        vec![("B", "A", 100)]
    )]
    #[case::zero_balance(
        HashMap::from([("A", Money::zero()), ("B", Money::zero())]),
        &["A", "B"],
        &["A"],
        HashMap::from([("A", Money::zero()), ("B", Money::zero())]),
        vec![]
    )]
    #[case::multiple_settle_members(
        HashMap::from([
            ("A", Money::from_i64(100)),
            ("B", Money::from_i64(100)),
            ("C", Money::from_i64(-200)),
        ]),
        &["A", "B", "C"],
        &["A", "B"],
        HashMap::from([
            ("A", Money::zero()),
            ("B", Money::zero()),
            ("C", Money::zero()),
        ]),
        vec![("A", "C", 100), ("B", "C", 100)]
    )]
    #[case::cross_group_transfer(
        HashMap::from([
            ("A", Money::from_i64(100)),
            ("B", Money::from_i64(-50)),
            ("C", Money::from_i64(-50)),
        ]),
        &["A", "B", "C"],
        &["A"],
        HashMap::from([
            ("A", Money::zero()),
            ("B", Money::zero()),
            ("C", Money::zero()),
        ]),
        vec![("A", "B", 50), ("A", "C", 50)]
    )]
    fn settlement_calculator_cases(
        calculator: SettlementCalculator,
        #[case] balances: HashMap<&str, Money>,
        #[case] participants: &[&str],
        #[case] settle_members: &[&str],
        #[case] expected_balances: HashMap<&str, Money>,
        #[case] expected_transfers: Vec<(&str, &str, i64)>,
    ) {
        let result = calculator.calculate(balances, participants, settle_members);

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
}
