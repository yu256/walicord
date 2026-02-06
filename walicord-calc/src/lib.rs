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
}

pub fn minimize_transactions<'a>(
    people: impl IntoIterator<Item = PersonBalance<'a>>,
    alpha: f64,
    beta: f64,
) -> Result<Vec<Payment<'a>>, SettlementError> {
    let people: Vec<PersonBalance<'a>> = people.into_iter().collect();
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
    #[cfg(feature = "coin_cbc")]
    problem.set_parameter("log", "0");

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
        let amount = solution.value(how_much[idx]).round() as i64;
        if amount > 0 {
            results.push(Payment {
                from: people[i].name,
                to: people[j].name,
                amount,
            });
        }
    }

    Ok(results)
}

#[cfg(test)]
mod tests {
    use super::{Payment, PersonBalance, SettlementError, minimize_transactions};
    use proptest::prelude::*;
    use rstest::rstest;
    use std::collections::HashMap;

    fn balances_from_payments<'a>(
        people: &[PersonBalance<'a>],
        payments: &[Payment<'a>],
    ) -> HashMap<&'a str, i64> {
        let mut balances = HashMap::with_capacity(people.len());
        for person in people {
            balances.insert(person.name, 0);
        }
        for payment in payments {
            *balances.entry(payment.from).or_insert(0) -= payment.amount;
            *balances.entry(payment.to).or_insert(0) += payment.amount;
        }
        balances
    }

    fn assert_balances_match<'a>(people: &[PersonBalance<'a>], payments: &[Payment<'a>]) {
        let balances = balances_from_payments(people, payments);
        for person in people {
            let actual = balances.get(person.name).copied().unwrap_or(0);
            assert_eq!(
                actual, person.balance,
                "balance mismatch for {}",
                person.name
            );
        }
    }

    #[rstest]
    #[case::simple_two_people(&[
        PersonBalance {
            name: "A",
            balance: 100,
        },
        PersonBalance {
            name: "B",
            balance: -100,
        },
    ])]
    fn settles_two_people(#[case] people: &[PersonBalance<'static>]) {
        let payments =
            minimize_transactions(people.iter().copied(), 1.0, 0.01).expect("expected solution");

        assert_eq!(payments.len(), 1);
        assert_eq!(payments[0].from, "B");
        assert_eq!(payments[0].to, "A");
        assert_eq!(payments[0].amount, 100);
        assert_balances_match(people, &payments);
    }

    #[rstest]
    #[case::imbalanced(&[
        PersonBalance { name: "A", balance: 50 },
        PersonBalance {
            name: "B",
            balance: -40,
        },
    ])]
    fn rejects_imbalanced_total(#[case] people: &[PersonBalance<'static>]) {
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
        PersonBalance { name: "A", balance: 0 },
        PersonBalance { name: "B", balance: 0 },
        PersonBalance { name: "C", balance: 0 },
    ])]
    fn zero_balances_produce_no_payments(#[case] people: &[PersonBalance<'static>]) {
        let payments =
            minimize_transactions(people.iter().copied(), 1.0, 0.01).expect("expected solution");
        assert!(payments.is_empty());
    }

    #[rstest]
    #[case::empty(&[])]
    #[case::single_zero(&[PersonBalance {
        name: "A",
        balance: 0,
    }])]
    fn small_inputs_produce_no_payments(#[case] people: &[PersonBalance<'static>]) {
        let payments =
            minimize_transactions(people.iter().copied(), 1.0, 0.01).expect("expected solution");
        assert!(payments.is_empty());
    }

    #[rstest]
    #[case::single_nonzero(&[PersonBalance {
        name: "A",
        balance: 50,
    }], 50)]
    fn small_inputs_reject_imbalanced(
        #[case] people: &[PersonBalance<'static>],
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
            PersonBalance {
                name: "A",
                balance: 80,
            },
            PersonBalance {
                name: "B",
                balance: -50,
            },
            PersonBalance {
                name: "C",
                balance: -30,
            },
        ];

        let payments =
            minimize_transactions(people.iter().copied(), alpha, beta).expect("expected solution");

        assert_balances_match(&people, &payments);
    }

    proptest! {
        #[test]
        fn payments_settle_balances(
            people_count in 2usize..=6,
            balances in prop::collection::vec(-200i64..=200, 1..=5),
        ) {
            let names = ["A", "B", "C", "D", "E", "F"];
            let mut people = Vec::with_capacity(people_count);
            let mut sum = 0i64;
            for idx in 0..people_count.saturating_sub(1) {
                let balance = *balances.get(idx).unwrap_or(&0);
                sum += balance;
                people.push(PersonBalance { name: names[idx], balance });
            }
            people.push(PersonBalance {
                name: names[people_count - 1],
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
            let names = ["A", "B", "C", "D", "E", "F"];
            let people: Vec<PersonBalance<'_>> = names[..people_count]
                .iter()
                .map(|&name| PersonBalance { name, balance: 0 })
                .collect();

            let payments = minimize_transactions(people.iter().copied(), 1.0, 0.01)
                .expect("expected solution");

            prop_assert!(payments.is_empty());
        }
    }
}
