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

    let n = people.len();

    let mut index_map = vec![vec![None; n]; n];
    let expected_pairs = n.saturating_mul(n.saturating_sub(1));
    let mut pairs = Vec::with_capacity(expected_pairs);
    for (i, row) in index_map.iter_mut().enumerate() {
        for (j, slot) in row.iter_mut().enumerate() {
            if i == j {
                continue;
            }
            let idx = pairs.len();
            *slot = Some(idx);
            pairs.push((i, j));
        }
    }

    let mut how_much: Vec<Variable> = Vec::with_capacity(pairs.len());
    let mut who_pays: Vec<Variable> = Vec::with_capacity(pairs.len());
    let mut objective = Expression::with_capacity(pairs.len() * 2);

    for _ in 0..pairs.len() {
        let flow_var = vars.add(variable().min(0.0));
        objective.add_mul(beta, flow_var);
        how_much.push(flow_var);

        let binary_var = vars.add(variable().binary());
        objective.add_mul(alpha, binary_var);
        who_pays.push(binary_var);
    }

    let big_m = 1_000_000_000.0;
    let mut problem = vars.minimise(objective).using(default_solver);

    // Constraint: how_much[i][j] <= big_m * who_pays[i][j]
    for (&hm, &wp) in how_much.iter().zip(&who_pays) {
        problem = problem.with((hm - big_m * wp).leq(0.0));
    }

    // Balance constraints: inflow - outflow = balance
    for (i, person) in people.iter().enumerate() {
        let mut constraint = Expression::default();
        for row in &index_map {
            if let Some(Some(idx)) = row.get(i) {
                constraint.add_mul(1.0, how_much[*idx]); // inflow
            }
        }
        for &maybe_idx in &index_map[i] {
            if let Some(idx) = maybe_idx {
                constraint.add_mul(-1.0, how_much[idx]); // outflow
            }
        }
        problem = problem.with(constraint.eq(person.balance as f64));
    }

    let Ok(solution) = problem.solve() else {
        return Err(SettlementError::NoSolution);
    };

    let mut results = Vec::with_capacity(pairs.len());
    for (idx, &(i, j)) in pairs.iter().enumerate() {
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
