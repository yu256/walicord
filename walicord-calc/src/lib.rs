#![warn(clippy::uninlined_format_args)]

mod model;

use microlp::{ComparisonOp, OptimizationDirection, Problem, Variable};
use thiserror::Error;

pub use model::{PersonBalance, SettlementResult};

#[derive(Debug, Error)]
pub enum SettlementError {
    #[error("Sum of balances must be zero (found {0})")]
    ImbalancedTotal(i64),
    #[error("Solver failed to find an optimal solution")]
    NoSolution,
}

pub fn minimize_transactions<'a>(
    people: &'a [PersonBalance<'a>],
    alpha: f64,
    beta: f64,
) -> Result<Vec<SettlementResult<'a>>, SettlementError> {
    let n = people.len();
    let total: i64 = people.iter().map(|p| p.balance).sum();
    if total != 0 {
        return Err(SettlementError::ImbalancedTotal(total));
    }

    let mut problem = Problem::new(OptimizationDirection::Minimize);

    // Create variables: how_much[i][j] = amount paid from i to j (non-negative)
    // who_pays[i][j] = binary variable (1 if i pays j, 0 otherwise)
    let mut how_much: Vec<Vec<Option<Variable>>> = vec![vec![None; n]; n];
    let mut who_pays: Vec<Vec<Option<Variable>>> = vec![vec![None; n]; n];

    for i in 0..n {
        for j in 0..n {
            if i != j {
                // add_var(objective_coefficient, (lower_bound, upper_bound))
                how_much[i][j] = Some(problem.add_var(beta, (0.0, f64::INFINITY)));
                who_pays[i][j] = Some(problem.add_var(alpha, (0.0, 1.0)));
            }
        }
    }

    let big_m = 1_000_000_000.0;

    // Constraint: how_much[i][j] <= big_m * who_pays[i][j]
    for i in 0..n {
        for j in 0..n {
            if i != j
                && let (Some(hm), Some(wp)) = (how_much[i][j], who_pays[i][j])
            {
                problem.add_constraint([(hm, 1.0), (wp, -big_m)], ComparisonOp::Le, 0.0);
            }
        }
    }

    // Balance constraints: inflow - outflow = balance
    for i in 0..n {
        let mut constraint = vec![];
        for (j, _) in how_much.iter().enumerate().take(n) {
            if i != j {
                if let Some(hm_ji) = how_much[j][i] {
                    constraint.push((hm_ji, 1.0)); // inflow
                }
                if let Some(hm_ij) = how_much[i][j] {
                    constraint.push((hm_ij, -1.0)); // outflow
                }
            }
        }
        problem.add_constraint(&constraint, ComparisonOp::Eq, people[i].balance as f64);
    }

    let solution = problem.solve().map_err(|_| SettlementError::NoSolution)?;

    let mut results = Vec::new();
    for i in 0..n {
        for j in 0..n {
            if i != j
                && let Some(hm) = how_much[i][j]
            {
                let amount = solution[hm].round() as i64;
                if amount > 0 {
                    results.push(SettlementResult {
                        from: people[i].name,
                        to: people[j].name,
                        amount,
                    });
                }
            }
        }
    }

    Ok(results)
}
