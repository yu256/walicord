use std::collections::{HashMap, HashSet};

/// Transfer information for settlement processing
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Transfer<'a> {
    pub from: &'a str,
    pub to: &'a str,
    pub amount: i64,
}

/// Result of settlement processing
#[derive(Debug, PartialEq)]
pub struct Settlement<'a> {
    pub new_balances: HashMap<&'a str, i64>,
    pub transfers: Vec<Transfer<'a>>,
}

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
        balances: HashMap<&'a str, i64>,
        participants: &[&'a str],
        settle_members: &[&'a str],
    ) -> Settlement<'a> {
        let mut working_balances = balances;
        let mut transfers = Vec::new();
        let settle_lookup: HashSet<&str> = settle_members.iter().copied().collect();

        for &member in settle_members {
            let balance = match working_balances.get(member).copied() {
                Some(b) if b != 0 => b,
                _ => continue,
            };

            let (from_member, to_member, target_sign) = if balance > 0 {
                (true, false, -1) // Receive: look for negative balances
            } else {
                (false, true, 1) // Pay: look for positive balances
            };

            let mut remaining = balance.abs();
            let mut transferred = 0;

            // Try to settle with other settle members first
            for &other in settle_members {
                if other == member {
                    continue;
                }
                if let Some(other_balance) = working_balances.get_mut(other) {
                    let can_transfer = if target_sign < 0 {
                        *other_balance < 0
                    } else {
                        *other_balance > 0
                    };
                    if !can_transfer {
                        continue;
                    }

                    let available = other_balance.abs();
                    let amount = remaining.min(available);
                    if amount > 0 {
                        *other_balance -= target_sign * amount;
                        remaining -= amount;
                        transferred += amount;
                        transfers.push(Transfer {
                            from: if from_member { member } else { other },
                            to: if to_member { member } else { other },
                            amount,
                        });
                    }
                    if remaining == 0 {
                        break;
                    }
                }
            }

            // Then try with other participants
            if remaining > 0 {
                for &other in participants {
                    if other == member || settle_lookup.contains(other) {
                        continue;
                    }
                    if let Some(other_balance) = working_balances.get_mut(other) {
                        let can_transfer = if target_sign < 0 {
                            *other_balance < 0
                        } else {
                            *other_balance > 0
                        };
                        if !can_transfer {
                            continue;
                        }

                        let available = other_balance.abs();
                        let amount = remaining.min(available);
                        if amount > 0 {
                            *other_balance -= target_sign * amount;
                            remaining -= amount;
                            transferred += amount;
                            transfers.push(Transfer {
                                from: if from_member { member } else { other },
                                to: if to_member { member } else { other },
                                amount,
                            });
                        }
                        if remaining == 0 {
                            break;
                        }
                    }
                }
            }

            // Update member's balance
            if let Some(member_balance) = working_balances.get_mut(member) {
                *member_balance = if remaining == 0 {
                    0
                } else {
                    balance.signum() * (balance.abs() - transferred)
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
        HashMap::from([("A", 100), ("B", -100)]),
        &["A", "B"],
        &["A"],
        HashMap::from([("A", 0), ("B", 0)]),
        vec![("A", "B", 100)]
    )]
    #[case::simple_negative_balance(
        HashMap::from([("A", -100), ("B", 100)]),
        &["A", "B"],
        &["A"],
        HashMap::from([("A", 0), ("B", 0)]),
        vec![("B", "A", 100)]
    )]
    #[case::zero_balance(
        HashMap::from([("A", 0), ("B", 0)]),
        &["A", "B"],
        &["A"],
        HashMap::from([("A", 0), ("B", 0)]),
        vec![]
    )]
    #[case::multiple_settle_members(
        HashMap::from([("A", 100), ("B", 100), ("C", -200)]),
        &["A", "B", "C"],
        &["A", "B"],
        HashMap::from([("A", 0), ("B", 0), ("C", 0)]),
        vec![("A", "C", 100), ("B", "C", 100)]
    )]
    #[case::cross_group_transfer(
        HashMap::from([("A", 100), ("B", -50), ("C", -50)]),
        &["A", "B", "C"],
        &["A"],
        HashMap::from([("A", 0), ("B", 0), ("C", 0)]),
        vec![("A", "B", 50), ("A", "C", 50)]
    )]
    fn settlement_calculator_cases(
        calculator: SettlementCalculator,
        #[case] balances: HashMap<&str, i64>,
        #[case] participants: &[&str],
        #[case] settle_members: &[&str],
        #[case] expected_balances: HashMap<&str, i64>,
        #[case] expected_transfers: Vec<(&str, &str, i64)>,
    ) {
        let result = calculator.calculate(balances, participants, settle_members);

        assert_eq!(result.new_balances, expected_balances);

        let expected: Vec<Transfer> = expected_transfers
            .into_iter()
            .map(|(from, to, amount)| Transfer { from, to, amount })
            .collect();
        assert_eq!(result.transfers, expected);
    }
}
