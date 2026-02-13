use crate::model::{MemberBalances, MemberId, Money, Settlement, Transfer};
use fxhash::FxHashSet;
use rust_decimal::Decimal;

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

            let (from_member, to_member, target_sign) = if balance.signum() > 0 {
                (true, false, -1)
            } else {
                (false, true, 1)
            };

            let mut remaining = balance.abs();
            let mut transferred = Money::ZERO;

            for &other in settle_members {
                if other == member {
                    continue;
                }
                if let Some(other_balance) = working_balances.get_mut(&other) {
                    let can_transfer = if target_sign < 0 {
                        other_balance.signum() < 0
                    } else {
                        other_balance.signum() > 0
                    };
                    if !can_transfer {
                        continue;
                    }

                    let available = other_balance.abs();
                    let amount = remaining.min(available);
                    if !amount.is_zero() {
                        *other_balance -= amount * Decimal::from(target_sign);
                        remaining -= amount;
                        transferred += amount;
                        transfers.push(Transfer {
                            from: if from_member { member } else { other },
                            to: if to_member { member } else { other },
                            amount,
                        });
                    }
                    if remaining.is_zero() {
                        break;
                    }
                }
            }

            if !remaining.is_zero() {
                for &other in &participants {
                    if other == member || settle_lookup.contains(&other) {
                        continue;
                    }
                    if let Some(other_balance) = working_balances.get_mut(&other) {
                        let can_transfer = if target_sign < 0 {
                            other_balance.signum() < 0
                        } else {
                            other_balance.signum() > 0
                        };
                        if !can_transfer {
                            continue;
                        }

                        let available = other_balance.abs();
                        let amount = remaining.min(available);
                        if !amount.is_zero() {
                            *other_balance -= amount * Decimal::from(target_sign);
                            remaining -= amount;
                            transferred += amount;
                            transfers.push(Transfer {
                                from: if from_member { member } else { other },
                                to: if to_member { member } else { other },
                                amount,
                            });
                        }
                        if remaining.is_zero() {
                            break;
                        }
                    }
                }
            }

            if let Some(member_balance) = working_balances.get_mut(&member) {
                *member_balance = if remaining.is_zero() {
                    Money::ZERO
                } else {
                    (balance.abs() - transferred) * Decimal::from(balance.signum())
                };
            }
            debug_assert!(remaining.is_zero());
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

    #[fixture]
    fn calculator() -> SettlementCalculator {
        SettlementCalculator
    }

    #[rstest]
    #[case::simple_positive_balance(
        MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(-100)),
        ]),
        &[MemberId(1)],
        MemberBalances::from_iter([(MemberId(1), Money::ZERO), (MemberId(2), Money::ZERO)]),
        vec![(MemberId(1), MemberId(2), 100)]
    )]
    #[case::simple_negative_balance(
        MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(-100)),
            (MemberId(2), Money::from_i64(100)),
        ]),
        &[MemberId(1)],
        MemberBalances::from_iter([(MemberId(1), Money::ZERO), (MemberId(2), Money::ZERO)]),
        vec![(MemberId(2), MemberId(1), 100)]
    )]
    #[case::zero_balance(
        MemberBalances::from_iter([(MemberId(1), Money::ZERO), (MemberId(2), Money::ZERO)]),
        &[MemberId(1)],
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
        MemberBalances::from_iter([
            (MemberId(1), Money::ZERO),
            (MemberId(2), Money::ZERO),
            (MemberId(3), Money::ZERO),
        ]),
        vec![(MemberId(1), MemberId(2), 100)]
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
