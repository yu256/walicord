#![warn(clippy::uninlined_format_args)]

mod model;

use good_lp::{Expression, Solution, SolverModel, Variable, default_solver, variable, variables};
use thiserror::Error;

pub use model::{Payment, PersonBalance};

#[derive(Debug, Error)]
pub enum SettlementError {
    #[error("Sum of balances must be zero (found {0})")]
    ImbalancedTotal(i64),
    #[error("Solver failed to find an optimal solution")]
    NoSolution,
    #[error("LP solver rounding caused balance mismatch")]
    RoundingMismatch,
}

pub fn minimize_transactions(
    people: impl IntoIterator<Item = PersonBalance>,
    alpha: f64,
    beta: f64,
) -> Result<Vec<Payment>, SettlementError> {
    let people: Vec<PersonBalance> = people.into_iter().collect();
    let total: i64 = people.iter().map(|p| p.balance).sum();
    if total != 0 {
        return Err(SettlementError::ImbalancedTotal(total));
    }

    let mut vars = variables!();

    let mut debtors = Vec::new();
    let mut creditors = Vec::new();
    for (idx, person) in people.iter().enumerate() {
        if person.balance < 0 {
            debtors.push((idx, -person.balance));
        } else if person.balance > 0 {
            creditors.push((idx, person.balance));
        }
    }

    if debtors.is_empty() || creditors.is_empty() {
        return Ok(Vec::new());
    }

    let mut pairs = Vec::with_capacity(debtors.len() * creditors.len());
    for &(debtor_idx, debtor_amount) in &debtors {
        for &(creditor_idx, creditor_amount) in &creditors {
            let max_amount = debtor_amount.min(creditor_amount) as f64;
            pairs.push((debtor_idx, creditor_idx, max_amount));
        }
    }

    let mut how_much: Vec<Variable> = Vec::with_capacity(pairs.len());
    let mut who_pays: Vec<Variable> = Vec::with_capacity(pairs.len());
    let mut objective = Expression::with_capacity(pairs.len() * 2);

    for _ in 0..pairs.len() {
        let flow_var = vars.add(variable().integer().min(0.0));
        objective.add_mul(beta, flow_var);
        how_much.push(flow_var);

        let binary_var = vars.add(variable().binary());
        objective.add_mul(alpha, binary_var);
        who_pays.push(binary_var);
    }

    let mut problem = vars.minimise(objective).using(default_solver);

    // Constraint: how_much[i][j] <= big_m * who_pays[i][j]
    for ((&hm, &wp), &(_, _, max_amount)) in how_much.iter().zip(&who_pays).zip(&pairs) {
        problem = problem.with((hm - max_amount * wp).leq(0.0));
    }

    // Balance constraints: inflow - outflow = balance
    for (i, person) in people.iter().enumerate() {
        let mut constraint = Expression::default();
        for (idx, &(from, to, _)) in pairs.iter().enumerate() {
            if to == i {
                constraint.add_mul(1.0, how_much[idx]);
            }
            if from == i {
                constraint.add_mul(-1.0, how_much[idx]);
            }
        }
        problem = problem.with(constraint.eq(person.balance as f64));
    }

    let Ok(solution) = problem.solve() else {
        return Err(SettlementError::NoSolution);
    };

    let mut results = Vec::with_capacity(pairs.len());
    for (idx, &(i, j, _)) in pairs.iter().enumerate() {
        let amount = round_bankers(solution.value(how_much[idx]));
        if amount > 0 {
            results.push(Payment {
                from: people[i].id,
                to: people[j].id,
                amount,
            });
        }
    }

    let mut index_by_id = std::collections::HashMap::with_capacity(people.len());
    for (idx, person) in people.iter().enumerate() {
        index_by_id.insert(person.id, idx);
    }

    let mut net = vec![0i64; people.len()];
    for p in &results {
        let Some(&from_idx) = index_by_id.get(&p.from) else {
            return Err(SettlementError::RoundingMismatch);
        };
        let Some(&to_idx) = index_by_id.get(&p.to) else {
            return Err(SettlementError::RoundingMismatch);
        };
        net[from_idx] -= p.amount;
        net[to_idx] += p.amount;
    }
    let expected = people.iter().map(|p| p.balance).collect::<Vec<_>>();
    if net != expected {
        return Err(SettlementError::RoundingMismatch);
    }

    Ok(results)
}

fn round_bankers(value: f64) -> i64 {
    value.round_ties_even() as i64
}

#[cfg(test)]
mod tests {
    use super::{Payment, PersonBalance, SettlementError, minimize_transactions, round_bankers};
    use proptest::prelude::*;
    use rstest::rstest;
    use std::collections::HashMap;

    fn balances_from_payments(people: &[PersonBalance], payments: &[Payment]) -> HashMap<u64, i64> {
        let mut balances = HashMap::with_capacity(people.len());
        for person in people {
            balances.insert(person.id, 0);
        }
        for payment in payments {
            *balances.entry(payment.from).or_insert(0) -= payment.amount;
            *balances.entry(payment.to).or_insert(0) += payment.amount;
        }
        balances
    }

    fn assert_balances_match(people: &[PersonBalance], payments: &[Payment]) {
        let balances = balances_from_payments(people, payments);
        for person in people {
            let actual = balances.get(&person.id).copied().unwrap_or(0);
            assert_eq!(
                actual, person.balance,
                "balance mismatch for id {}",
                person.id
            );
        }
    }

    #[rstest]
    #[case::simple_two_people(&[
        PersonBalance {
            id: 1,
            balance: 100,
        },
        PersonBalance {
            id: 2,
            balance: -100,
        },
    ])]
    fn settles_two_people(#[case] people: &[PersonBalance]) {
        let payments =
            minimize_transactions(people.iter().copied(), 1.0, 0.01).expect("expected solution");

        assert_eq!(payments.len(), 1);
        assert_eq!(payments[0].from, 2);
        assert_eq!(payments[0].to, 1);
        assert_eq!(payments[0].amount, 100);
        assert_balances_match(people, &payments);
    }

    #[rstest]
    #[case::imbalanced(&[
        PersonBalance { id: 1, balance: 50 },
        PersonBalance {
            id: 2,
            balance: -40,
        },
    ])]
    fn rejects_imbalanced_total(#[case] people: &[PersonBalance]) {
        let result = minimize_transactions(people.iter().copied(), 1.0, 0.01);
        match result {
            Err(SettlementError::ImbalancedTotal(total)) => {
                assert_eq!(total, 10);
            }
            _ => panic!("expected imbalanced total error"),
        }
    }

    #[rstest]
    #[case::all_zero(&[
        PersonBalance { id: 1, balance: 0 },
        PersonBalance { id: 2, balance: 0 },
        PersonBalance { id: 3, balance: 0 },
    ])]
    fn zero_balances_produce_no_payments(#[case] people: &[PersonBalance]) {
        let payments =
            minimize_transactions(people.iter().copied(), 1.0, 0.01).expect("expected solution");
        assert!(payments.is_empty());
    }

    #[rstest]
    #[case::empty(&[])]
    #[case::single_zero(&[PersonBalance {
        id: 1,
        balance: 0,
    }])]
    fn small_inputs_produce_no_payments(#[case] people: &[PersonBalance]) {
        let payments =
            minimize_transactions(people.iter().copied(), 1.0, 0.01).expect("expected solution");
        assert!(payments.is_empty());
    }

    #[rstest]
    #[case::single_nonzero(&[PersonBalance {
        id: 1,
        balance: 50,
    }], 50)]
    fn small_inputs_reject_imbalanced(
        #[case] people: &[PersonBalance],
        #[case] expected_total: i64,
    ) {
        let result = minimize_transactions(people.iter().copied(), 1.0, 0.01);
        match result {
            Err(SettlementError::ImbalancedTotal(total)) => {
                assert_eq!(total, expected_total);
            }
            _ => panic!("expected imbalanced total error"),
        }
    }

    #[rstest]
    #[case::alpha_focus(1.0, 0.0)]
    #[case::beta_focus(0.0, 1.0)]
    fn balances_settle_with_weight_variations(#[case] alpha: f64, #[case] beta: f64) {
        let people = [
            PersonBalance { id: 1, balance: 80 },
            PersonBalance {
                id: 2,
                balance: -50,
            },
            PersonBalance {
                id: 3,
                balance: -30,
            },
        ];

        let payments =
            minimize_transactions(people.iter().copied(), alpha, beta).expect("expected solution");

        assert_balances_match(&people, &payments);
    }

    #[rstest]
    #[case::tie_to_even_low(0.5, 0)]
    #[case::tie_to_even_high(1.5, 2)]
    #[case::tie_to_even_upper(2.5, 2)]
    #[case::negative_tie(-0.5, 0)]
    #[case::negative_high(-1.5, -2)]
    #[case::non_tie_up(2.6, 3)]
    #[case::non_tie_down(-2.4, -2)]
    fn bankers_rounding(#[case] value: f64, #[case] expected: i64) {
        assert_eq!(round_bankers(value), expected);
    }

    proptest! {
        #[test]
        fn payments_settle_balances(
            people_count in 2usize..=6,
            balances in prop::collection::vec(-200i64..=200, 1..=5),
        ) {
            let mut people = Vec::with_capacity(people_count);
            let mut sum = 0i64;
            for idx in 0..people_count.saturating_sub(1) {
                let balance = *balances.get(idx).unwrap_or(&0);
                sum += balance;
                people.push(PersonBalance { id: idx as u64 + 1, balance });
            }
            people.push(PersonBalance {
                id: people_count as u64,
                balance: -sum,
            });

            let payments = minimize_transactions(people.iter().copied(), 1.0, 0.01)
                .expect("expected solution");

            for payment in &payments {
                prop_assert!(payment.amount > 0);
                prop_assert_ne!(payment.from, payment.to);
            }
            assert_balances_match(&people, &payments);
        }

        #[test]
        fn zero_balances_have_no_payments(
            people_count in 2usize..=6,
        ) {
            let people: Vec<PersonBalance> = (1..=people_count as u64)
                .map(|id| PersonBalance { id, balance: 0 })
                .collect();

            let payments = minimize_transactions(people.iter().copied(), 1.0, 0.01)
                .expect("expected solution");

            prop_assert!(payments.is_empty());
        }
    }
}
