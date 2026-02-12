use crate::model::{MemberBalances, MemberId, Money, Settlement, Transfer};
use fxhash::FxHashSet;

/// Settlement calculation service
pub struct SettlementCalculator;

impl SettlementCalculator {
    /// Calculate settlement for specified members
    ///
    /// Takes ownership of balances and returns new state.
    ///
    /// # Arguments
    /// * `balances` - Current balance table (MemberId -> Money)
    /// * `settle_members` - Members to settle (MemberIds)
    ///
    /// # Returns
    /// New balances after settlement and transfer list
    pub fn calculate(&self, balances: MemberBalances, settle_members: &[MemberId]) -> Settlement {
        let mut working_balances = balances;
        let mut transfers = Vec::new();
        let settle_lookup: FxHashSet<MemberId> = settle_members.iter().copied().collect();
        let participants: Vec<MemberId> = working_balances.keys().copied().collect();

        for &member in settle_members {
            let balance = match working_balances.get(&member).copied() {
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
                if let Some(other_balance) = working_balances.get_mut(&other) {
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
                for &other in &participants {
                    if other == member || settle_lookup.contains(&other) {
                        continue;
                    }
                    if let Some(other_balance) = working_balances.get_mut(&other) {
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

            if let Some(member_balance) = working_balances.get_mut(&member) {
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
    use crate::model::MemberBalances;
    use rstest::{fixture, rstest};

    fn id(n: u64) -> MemberId {
        MemberId(n)
    }

    #[fixture]
    fn calculator() -> SettlementCalculator {
        SettlementCalculator
    }

    #[rstest]
    #[case::simple_positive_balance(
        MemberBalances::from_iter([
            (id(1), Money::from_i64(100)),
            (id(2), Money::from_i64(-100)),
        ]),
        &[id(1)],
        MemberBalances::from_iter([(id(1), Money::zero()), (id(2), Money::zero())]),
        vec![(id(1), id(2), 100)]
    )]
    #[case::simple_negative_balance(
        MemberBalances::from_iter([
            (id(1), Money::from_i64(-100)),
            (id(2), Money::from_i64(100)),
        ]),
        &[id(1)],
        MemberBalances::from_iter([(id(1), Money::zero()), (id(2), Money::zero())]),
        vec![(id(2), id(1), 100)]
    )]
    #[case::zero_balance(
        MemberBalances::from_iter([(id(1), Money::zero()), (id(2), Money::zero())]),
        &[id(1)],
        MemberBalances::from_iter([(id(1), Money::zero()), (id(2), Money::zero())]),
        vec![]
    )]
    #[case::multiple_settle_members(
        MemberBalances::from_iter([
            (id(1), Money::from_i64(100)),
            (id(2), Money::from_i64(100)),
            (id(3), Money::from_i64(-200)),
        ]),
        &[id(1), id(2)],
        MemberBalances::from_iter([
            (id(1), Money::zero()),
            (id(2), Money::zero()),
            (id(3), Money::zero()),
        ]),
        vec![(id(1), id(3), 100), (id(2), id(3), 100)]
    )]
    #[case::cross_group_transfer(
        MemberBalances::from_iter([
            (id(1), Money::from_i64(100)),
            (id(2), Money::from_i64(-50)),
            (id(3), Money::from_i64(-50)),
        ]),
        &[id(1)],
        MemberBalances::from_iter([
            (id(1), Money::zero()),
            (id(2), Money::zero()),
            (id(3), Money::zero()),
        ]),
        vec![(id(1), id(2), 50), (id(1), id(3), 50)]
    )]
    #[case::empty_settle_members(
        MemberBalances::from_iter([
            (id(1), Money::from_i64(100)),
            (id(2), Money::from_i64(-100)),
        ]),
        &[],
        MemberBalances::from_iter([
            (id(1), Money::from_i64(100)),
            (id(2), Money::from_i64(-100)),
        ]),
        vec![]
    )]
    #[case::missing_balance_member(
        MemberBalances::from_iter([
            (id(1), Money::from_i64(100)),
            (id(2), Money::from_i64(-100)),
            (id(3), Money::zero()),
        ]),
        &[id(1), id(3)],
        MemberBalances::from_iter([
            (id(1), Money::zero()),
            (id(2), Money::zero()),
            (id(3), Money::zero()),
        ]),
        vec![(id(1), id(2), 100)]
    )]
    fn settlement_calculator_cases(
        calculator: SettlementCalculator,
        #[case] balances: MemberBalances,
        #[case] settle_members: &[MemberId],
        #[case] expected_balances: MemberBalances,
        #[case] expected_transfers: Vec<(MemberId, MemberId, i64)>,
    ) {
        let result = calculator.calculate(balances, settle_members);

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
