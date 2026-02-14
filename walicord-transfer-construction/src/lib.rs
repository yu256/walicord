#![warn(clippy::uninlined_format_args)]

mod model;

use good_lp::{Expression, Solution, SolverModel, Variable, default_solver, variable, variables};
use thiserror::Error;

pub use model::{Payment, PersonBalance};

#[derive(Debug, Error)]
pub enum SettlementError {
    #[error("Sum of balances must be zero (found {0})")]
    ImbalancedTotal(i64),
    #[error("Invalid cash grid parameters (g1={g1}, g2={g2})")]
    InvalidGrid { g1: i64, g2: i64 },
    #[error(
        "Transfer graph too large for lexicographic solve (edges={edge_count}, max={max_edges})"
    )]
    ModelTooLarge { edge_count: usize, max_edges: usize },
    #[error("Solver failed to find an optimal solution")]
    NoSolution,
    #[error("LP solver rounding caused balance mismatch")]
    RoundingMismatch,
}

const MAX_LEXICOGRAPHIC_EDGES: usize = 120;

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

pub fn construct_settlement_transfers(
    people: impl IntoIterator<Item = PersonBalance>,
    settle_members: &[u64],
    cash_members: &[u64],
    g1: i64,
    g2: i64,
) -> Result<Vec<Payment>, SettlementError> {
    let mut people: Vec<PersonBalance> = people.into_iter().collect();
    people.sort_unstable_by_key(|person| person.id);
    let total: i64 = people.iter().map(|p| p.balance).sum();
    if total != 0 {
        return Err(SettlementError::ImbalancedTotal(total));
    }

    if people.is_empty() || settle_members.is_empty() {
        return Ok(Vec::new());
    }
    if g1 <= 0 || g2 <= 0 || g1 % g2 != 0 {
        return Err(SettlementError::InvalidGrid { g1, g2 });
    }

    let model = TransferModel::from_people(&people, settle_members, cash_members);
    if model.edges.is_empty() {
        return Ok(Vec::new());
    }
    if model.edges.len() > MAX_LEXICOGRAPHIC_EDGES {
        return Err(SettlementError::ModelTooLarge {
            edge_count: model.edges.len(),
            max_edges: MAX_LEXICOGRAPHIC_EDGES,
        });
    }

    let Some(lex_fixed) = solve_full_lexicographic(&model, g1, g2) else {
        return Err(SettlementError::NoSolution);
    };

    let mut transfers: Vec<Payment> = model
        .edges
        .iter()
        .zip(lex_fixed)
        .filter_map(|(edge, amount)| {
            let amount = round_bankers(amount);
            if amount <= 0 {
                return None;
            }
            Some(Payment {
                from: edge.from,
                to: edge.to,
                amount,
            })
        })
        .collect();
    transfers.sort_unstable_by_key(|payment| (payment.from, payment.to));

    if !is_transfer_result_consistent(&people, settle_members, &transfers) {
        return Err(SettlementError::RoundingMismatch);
    }

    Ok(transfers)
}

fn is_transfer_result_consistent(
    people: &[PersonBalance],
    settle_members: &[u64],
    transfers: &[Payment],
) -> bool {
    let initial_balances: std::collections::HashMap<u64, i64> = people
        .iter()
        .map(|person| (person.id, person.balance))
        .collect();
    let mut balances: std::collections::HashMap<u64, i64> = people
        .iter()
        .map(|person| (person.id, person.balance))
        .collect();

    for payment in transfers {
        if payment.amount <= 0 || payment.from == payment.to {
            return false;
        }

        let Some(from_initial) = initial_balances.get(&payment.from).copied() else {
            return false;
        };
        let Some(to_initial) = initial_balances.get(&payment.to).copied() else {
            return false;
        };

        if from_initial <= 0 || to_initial >= 0 {
            return false;
        }

        let Some(from_balance) = balances.get_mut(&payment.from) else {
            return false;
        };
        *from_balance -= payment.amount;

        let Some(to_balance) = balances.get_mut(&payment.to) else {
            return false;
        };
        *to_balance += payment.amount;
    }

    settle_members
        .iter()
        .all(|member| balances.get(member).copied().unwrap_or(0) == 0)
}

fn solve_full_lexicographic(model: &TransferModel, g1: i64, g2: i64) -> Option<Vec<f64>> {
    let mut fixed = FixedTargets::default();

    let has_cash_objective = model.edges.iter().any(|edge| edge.payer_is_cash);
    if has_cash_objective {
        let stage1 = solve_transfer_stage(model, ObjectiveKind::Obj1, &fixed, &[], g1, g2)?;
        fixed.obj1 = Some(round_count_objective(stage1.obj1));

        let stage2 = solve_transfer_stage(model, ObjectiveKind::Obj2, &fixed, &[], g1, g2)?;
        fixed.obj2 = Some(round_count_objective(stage2.obj2));
    } else {
        fixed.obj1 = Some(0.0);
        fixed.obj2 = Some(0.0);
    }

    let stage3 = solve_transfer_stage(model, ObjectiveKind::ObjW, &fixed, &[], g1, g2)?;
    fixed.objw = Some(round_count_objective(stage3.objw));

    let stage4 = solve_transfer_stage(model, ObjectiveKind::Obj3, &fixed, &[], g1, g2)?;
    fixed.obj3 = Some(round_count_objective(stage4.obj3));

    let mut lex_fixed: Vec<f64> = Vec::with_capacity(model.edges.len());
    for idx in 0..model.edges.len() {
        let stage =
            solve_transfer_stage(model, ObjectiveKind::Lex(idx), &fixed, &lex_fixed, g1, g2)?;
        lex_fixed.push(stage.x_values[idx]);
    }
    Some(lex_fixed)
}

#[derive(Clone, Copy)]
enum ObjectiveKind {
    Obj1,
    Obj2,
    ObjW,
    Obj3,
    Lex(usize),
}

#[derive(Default)]
struct FixedTargets {
    obj1: Option<f64>,
    obj2: Option<f64>,
    objw: Option<f64>,
    obj3: Option<f64>,
}

#[derive(Clone, Copy)]
struct Edge {
    from: u64,
    to: u64,
    upper_bound: i64,
    payer_is_cash: bool,
    touches_non_settle: bool,
}

struct TransferModel {
    payers: Vec<(u64, i64)>,
    receivers: Vec<(u64, i64)>,
    edges: Vec<Edge>,
    payer_edges: Vec<Vec<usize>>,
    receiver_edges: Vec<Vec<usize>>,
    settle_lookup: std::collections::HashSet<u64>,
}

impl TransferModel {
    fn from_people(people: &[PersonBalance], settle_members: &[u64], cash_members: &[u64]) -> Self {
        let settle_lookup: std::collections::HashSet<u64> =
            settle_members.iter().copied().collect();
        let cash_lookup: std::collections::HashSet<u64> = cash_members.iter().copied().collect();

        let mut payers = Vec::new();
        let mut receivers = Vec::new();
        for person in people {
            if person.balance > 0 {
                payers.push((person.id, person.balance));
            } else if person.balance < 0 {
                receivers.push((person.id, -person.balance));
            }
        }

        let mut payer_edges: Vec<Vec<usize>> = vec![Vec::new(); payers.len()];
        let mut receiver_edges: Vec<Vec<usize>> = vec![Vec::new(); receivers.len()];
        let mut edges = Vec::with_capacity(payers.len() * receivers.len());

        for (payer_idx, (payer_member, pay)) in payers.iter().enumerate() {
            for (receiver_idx, (receiver_member, recv)) in receivers.iter().enumerate() {
                let edge_idx = edges.len();
                edges.push(Edge {
                    from: *payer_member,
                    to: *receiver_member,
                    upper_bound: (*pay).min(*recv),
                    payer_is_cash: cash_lookup.contains(payer_member),
                    touches_non_settle: !(settle_lookup.contains(payer_member)
                        && settle_lookup.contains(receiver_member)),
                });
                payer_edges[payer_idx].push(edge_idx);
                receiver_edges[receiver_idx].push(edge_idx);
            }
        }

        Self {
            payers,
            receivers,
            edges,
            payer_edges,
            receiver_edges,
            settle_lookup,
        }
    }
}

struct StageSolution {
    obj1: f64,
    obj2: f64,
    objw: f64,
    obj3: f64,
    x_values: Vec<f64>,
}

fn build_obj1_expr(model: &TransferModel, z1_vars: &[Variable]) -> Expression {
    let mut expr = Expression::default();
    for (edge_idx, edge) in model.edges.iter().enumerate() {
        if edge.payer_is_cash {
            expr.add_mul(1.0, z1_vars[edge_idx]);
        }
    }
    expr
}

fn build_obj2_expr(model: &TransferModel, z2_vars: &[Variable]) -> Expression {
    let mut expr = Expression::default();
    for (edge_idx, edge) in model.edges.iter().enumerate() {
        if edge.payer_is_cash {
            expr.add_mul(1.0, z2_vars[edge_idx]);
        }
    }
    expr
}

fn build_objw_expr(model: &TransferModel, y_vars: &[Variable]) -> Expression {
    let mut expr = Expression::default();
    for (edge_idx, edge) in model.edges.iter().enumerate() {
        if edge.touches_non_settle {
            expr.add_mul(1.0, y_vars[edge_idx]);
        }
    }
    expr
}

fn build_obj3_expr(y_vars: &[Variable]) -> Expression {
    let mut expr = Expression::default();
    for var in y_vars {
        expr.add_mul(1.0, *var);
    }
    expr
}

#[allow(clippy::too_many_lines)]
fn solve_transfer_stage(
    model: &TransferModel,
    objective_kind: ObjectiveKind,
    fixed: &FixedTargets,
    lex_prefix: &[f64],
    g1: i64,
    g2: i64,
) -> Option<StageSolution> {
    const EPS: f64 = 1e-6;
    let mut vars = variables!();

    let mut x_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());
    let mut y_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());
    let mut z1_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());
    let mut z2_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());
    let mut a1_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());
    let mut a2_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());
    let mut r1_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());
    let mut r2_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());

    for _ in &model.edges {
        x_vars.push(vars.add(variable().integer().min(0.0)));
        y_vars.push(vars.add(variable().binary()));
        z1_vars.push(vars.add(variable().binary()));
        z2_vars.push(vars.add(variable().binary()));
        a1_vars.push(vars.add(variable().integer().min(0.0)));
        a2_vars.push(vars.add(variable().integer().min(0.0)));
        r1_vars.push(vars.add(variable().integer().min(0.0)));
        r2_vars.push(vars.add(variable().integer().min(0.0)));
    }

    let mut s_vars: Vec<Variable> = Vec::with_capacity(model.payers.len());
    for _ in &model.payers {
        s_vars.push(vars.add(variable().integer().min(0.0)));
    }

    let mut t_vars: Vec<Variable> = Vec::with_capacity(model.receivers.len());
    for _ in &model.receivers {
        t_vars.push(vars.add(variable().integer().min(0.0)));
    }

    let mut obj1_expr = Some(build_obj1_expr(model, &z1_vars));
    let mut obj2_expr = Some(build_obj2_expr(model, &z2_vars));
    let mut objw_expr = Some(build_objw_expr(model, &y_vars));
    let mut obj3_expr = Some(build_obj3_expr(&y_vars));

    let objective_expr = match objective_kind {
        ObjectiveKind::Obj1 => obj1_expr
            .take()
            .unwrap_or_else(|| build_obj1_expr(model, &z1_vars)),
        ObjectiveKind::Obj2 => obj2_expr
            .take()
            .unwrap_or_else(|| build_obj2_expr(model, &z2_vars)),
        ObjectiveKind::ObjW => objw_expr
            .take()
            .unwrap_or_else(|| build_objw_expr(model, &y_vars)),
        ObjectiveKind::Obj3 => obj3_expr.take().unwrap_or_else(|| build_obj3_expr(&y_vars)),
        ObjectiveKind::Lex(index) => {
            let mut expr = Expression::default();
            expr.add_mul(1.0, x_vars[index]);
            expr
        }
    };

    let mut problem = vars.minimise(objective_expr).using(default_solver);

    for (payer_idx, (payer, pay)) in model.payers.iter().enumerate() {
        let mut flow = Expression::default();
        for &edge_idx in &model.payer_edges[payer_idx] {
            flow.add_mul(1.0, x_vars[edge_idx]);
        }
        flow.add_mul(1.0, s_vars[payer_idx]);
        problem = problem.with(flow.eq(*pay as f64));
        if model.settle_lookup.contains(payer) {
            problem = problem.with((s_vars[payer_idx] - 0.0).eq(0.0));
        }
    }

    for (receiver_idx, (receiver, recv)) in model.receivers.iter().enumerate() {
        let mut flow = Expression::default();
        for &edge_idx in &model.receiver_edges[receiver_idx] {
            flow.add_mul(1.0, x_vars[edge_idx]);
        }
        flow.add_mul(1.0, t_vars[receiver_idx]);
        problem = problem.with(flow.eq(*recv as f64));
        if model.settle_lookup.contains(receiver) {
            problem = problem.with((t_vars[receiver_idx] - 0.0).eq(0.0));
        }
    }

    for (edge_idx, edge) in model.edges.iter().enumerate() {
        let upper = edge.upper_bound as f64;
        problem = problem.with((x_vars[edge_idx] - upper * y_vars[edge_idx]).leq(0.0));

        if edge.payer_is_cash {
            problem = problem
                .with(
                    (x_vars[edge_idx] - (g1 as f64) * a1_vars[edge_idx] - r1_vars[edge_idx])
                        .eq(0.0),
                )
                .with(
                    (r1_vars[edge_idx] - (g2 as f64) * a2_vars[edge_idx] - r2_vars[edge_idx])
                        .eq(0.0),
                )
                .with((r1_vars[edge_idx] - ((g1 - 1) as f64) * z1_vars[edge_idx]).leq(0.0))
                .with((r2_vars[edge_idx] - ((g2 - 1) as f64) * z2_vars[edge_idx]).leq(0.0))
                .with((z2_vars[edge_idx] - z1_vars[edge_idx]).leq(0.0))
                .with((z1_vars[edge_idx] - y_vars[edge_idx]).leq(0.0))
                .with((z2_vars[edge_idx] - y_vars[edge_idx]).leq(0.0));
        } else {
            problem = problem
                .with((z1_vars[edge_idx] - 0.0).eq(0.0))
                .with((z2_vars[edge_idx] - 0.0).eq(0.0))
                .with((a1_vars[edge_idx] - 0.0).eq(0.0))
                .with((a2_vars[edge_idx] - 0.0).eq(0.0))
                .with((r1_vars[edge_idx] - 0.0).eq(0.0))
                .with((r2_vars[edge_idx] - 0.0).eq(0.0));
        }
    }

    if let Some(target) = fixed.obj1 {
        let expr = obj1_expr
            .take()
            .unwrap_or_else(|| build_obj1_expr(model, &z1_vars));
        problem = problem.with(expr.leq(round_count_objective(target) + EPS));
    }
    if let Some(target) = fixed.obj2 {
        let expr = obj2_expr
            .take()
            .unwrap_or_else(|| build_obj2_expr(model, &z2_vars));
        problem = problem.with(expr.leq(round_count_objective(target) + EPS));
    }
    if let Some(target) = fixed.objw {
        let expr = objw_expr
            .take()
            .unwrap_or_else(|| build_objw_expr(model, &y_vars));
        problem = problem.with(expr.leq(round_count_objective(target) + EPS));
    }
    if let Some(target) = fixed.obj3 {
        let expr = obj3_expr.take().unwrap_or_else(|| build_obj3_expr(&y_vars));
        problem = problem.with(expr.leq(round_count_objective(target) + EPS));
    }
    for (idx, center) in lex_prefix.iter().copied().enumerate() {
        problem = problem
            .with((x_vars[idx] - center).leq(1e-6))
            .with((center - x_vars[idx]).leq(1e-6));
    }

    let solution = problem.solve().ok()?;
    let x_values: Vec<f64> = x_vars.iter().map(|var| solution.value(*var)).collect();
    let obj1 = model
        .edges
        .iter()
        .enumerate()
        .filter(|(_, edge)| edge.payer_is_cash)
        .map(|(idx, _)| solution.value(z1_vars[idx]))
        .sum();
    let obj2 = model
        .edges
        .iter()
        .enumerate()
        .filter(|(_, edge)| edge.payer_is_cash)
        .map(|(idx, _)| solution.value(z2_vars[idx]))
        .sum();
    let objw = model
        .edges
        .iter()
        .enumerate()
        .filter(|(_, edge)| edge.touches_non_settle)
        .map(|(idx, _)| solution.value(y_vars[idx]))
        .sum();
    let obj3 = y_vars.iter().map(|var| solution.value(*var)).sum();

    Some(StageSolution {
        obj1,
        obj2,
        objw,
        obj3,
        x_values,
    })
}

fn round_bankers(value: f64) -> i64 {
    value.round_ties_even() as i64
}

fn round_count_objective(value: f64) -> f64 {
    (value + 0.5).floor()
}

#[cfg(test)]
mod tests {
    use super::{
        Payment, PersonBalance, SettlementError, construct_settlement_transfers,
        minimize_transactions, round_bankers, round_count_objective,
    };
    use proptest::prelude::*;
    use rstest::rstest;
    use std::collections::HashMap;

    fn apply_transfers(people: &[PersonBalance], payments: &[Payment]) -> HashMap<u64, i64> {
        let mut balances: HashMap<u64, i64> = people
            .iter()
            .map(|person| (person.id, person.balance))
            .collect();
        for payment in payments {
            *balances
                .get_mut(&payment.from)
                .expect("payer must exist in balances") -= payment.amount;
            *balances
                .get_mut(&payment.to)
                .expect("receiver must exist in balances") += payment.amount;
        }
        balances
    }

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

    #[rstest]
    #[case(2.999_999_9, 3.0)]
    #[case(3.000_000_1, 3.0)]
    #[case(0.0, 0.0)]
    fn count_objective_rounding_stabilizes_near_integer_targets(
        #[case] value: f64,
        #[case] expected: f64,
    ) {
        assert_eq!(round_count_objective(value), expected);
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

        #[test]
        fn transfer_construction_zeroes_requested_members(
            people_count in 2usize..=6,
            balances in prop::collection::vec(-200i64..=200, 1..=5),
            settle_indices in prop::collection::vec(0usize..6, 1..=6),
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

            let mut settle_members: Vec<u64> = settle_indices
                .iter()
                .map(|idx| (*idx % people_count) as u64 + 1)
                .collect();
            settle_members.sort_unstable();
            settle_members.dedup();

            let payments = construct_settlement_transfers(
                people.iter().copied(),
                &settle_members,
                &[],
                1000,
                100,
            )
            .expect("transfer construction should succeed");

            let final_balances = apply_transfers(&people, &payments);
            for member in &settle_members {
                prop_assert_eq!(final_balances.get(member).copied().unwrap_or(0), 0);
            }
        }
    }

    #[test]
    fn transfer_construction_settles_target_subset_only() {
        let people = [
            PersonBalance { id: 1, balance: 60 },
            PersonBalance {
                id: 2,
                balance: 100,
            },
            PersonBalance {
                id: 3,
                balance: -160,
            },
        ];

        let payments = construct_settlement_transfers(people, &[1], &[], 1000, 100)
            .expect("expected solution");

        assert_eq!(
            payments,
            vec![Payment {
                from: 1,
                to: 3,
                amount: 60,
            }]
        );
    }

    #[test]
    fn transfer_construction_is_deterministic() {
        let people = [
            PersonBalance {
                id: 1,
                balance: 100,
            },
            PersonBalance {
                id: 2,
                balance: 100,
            },
            PersonBalance {
                id: 3,
                balance: -200,
            },
        ];

        let first = construct_settlement_transfers(people, &[1, 2], &[1], 1000, 100)
            .expect("expected solution");
        let second = construct_settlement_transfers(people, &[1, 2], &[1], 1000, 100)
            .expect("expected solution");

        assert_eq!(first, second);
    }

    #[test]
    fn transfer_construction_is_deterministic_across_input_order() {
        let sorted_people = [
            PersonBalance {
                id: 1,
                balance: 1200,
            },
            PersonBalance {
                id: 2,
                balance: -1000,
            },
            PersonBalance {
                id: 3,
                balance: -200,
            },
            PersonBalance { id: 4, balance: 0 },
        ];
        let shuffled_people = [
            PersonBalance {
                id: 3,
                balance: -200,
            },
            PersonBalance { id: 4, balance: 0 },
            PersonBalance {
                id: 1,
                balance: 1200,
            },
            PersonBalance {
                id: 2,
                balance: -1000,
            },
        ];

        let sorted_result =
            construct_settlement_transfers(sorted_people, &[1, 2, 3], &[1], 1000, 100)
                .expect("expected solution");
        let shuffled_result =
            construct_settlement_transfers(shuffled_people, &[1, 2, 3], &[1], 1000, 100)
                .expect("expected solution");

        assert_eq!(sorted_result, shuffled_result);
    }

    #[rstest]
    #[case::obj1_prioritized(
        vec![
            PersonBalance { id: 1, balance: 2000 },
            PersonBalance { id: 2, balance: 100 },
            PersonBalance { id: 3, balance: -1000 },
            PersonBalance { id: 4, balance: -1100 },
        ],
        vec![1, 2, 3, 4],
        vec![1],
        vec![
            Payment { from: 1, to: 3, amount: 1000 },
            Payment { from: 1, to: 4, amount: 1000 },
            Payment { from: 2, to: 4, amount: 100 },
        ]
    )]
    #[case::obj2_breaks_obj1_ties(
        vec![
            PersonBalance { id: 1, balance: 1300 },
            PersonBalance { id: 2, balance: 100 },
            PersonBalance { id: 3, balance: -750 },
            PersonBalance { id: 4, balance: -650 },
        ],
        vec![1, 2, 3, 4],
        vec![1],
        vec![
            Payment { from: 1, to: 3, amount: 700 },
            Payment { from: 1, to: 4, amount: 600 },
            Payment { from: 2, to: 3, amount: 50 },
            Payment { from: 2, to: 4, amount: 50 },
        ]
    )]
    #[case::objw_prefers_internal_counterparts(
        vec![
            PersonBalance { id: 1, balance: 1200 },
            PersonBalance { id: 2, balance: -1000 },
            PersonBalance { id: 3, balance: -200 },
            PersonBalance { id: 4, balance: -200 },
            PersonBalance { id: 5, balance: 200 },
        ],
        vec![1, 2, 3],
        vec![1],
        vec![
            Payment { from: 1, to: 2, amount: 1000 },
            Payment { from: 1, to: 3, amount: 200 },
        ]
    )]
    fn transfer_construction_lexicographic_objectives(
        #[case] people: Vec<PersonBalance>,
        #[case] settle_members: Vec<u64>,
        #[case] cash_members: Vec<u64>,
        #[case] expected: Vec<Payment>,
    ) {
        let payments =
            construct_settlement_transfers(people, &settle_members, &cash_members, 1000, 100)
                .expect("solution");

        assert_eq!(payments, expected);
    }

    #[test]
    fn transfer_construction_obj3_minimizes_transfer_count() {
        let people = [
            PersonBalance {
                id: 1,
                balance: 100,
            },
            PersonBalance {
                id: 2,
                balance: 100,
            },
            PersonBalance {
                id: 3,
                balance: -100,
            },
            PersonBalance {
                id: 4,
                balance: -100,
            },
        ];

        let payments = construct_settlement_transfers(people, &[1, 2, 3, 4], &[], 1000, 100)
            .expect("solution");

        assert_eq!(payments.len(), 2);
    }

    #[test]
    fn transfer_construction_rejects_oversized_edge_set() {
        let mut people = Vec::new();
        for id in 1..=11u64 {
            people.push(PersonBalance { id, balance: 1 });
        }
        for id in 12..=22u64 {
            people.push(PersonBalance { id, balance: -1 });
        }

        let settle_members: Vec<u64> = people.iter().map(|person| person.id).collect();
        let result = construct_settlement_transfers(people, &settle_members, &[], 1000, 100);

        assert!(matches!(
            result,
            Err(SettlementError::ModelTooLarge {
                edge_count: 121,
                max_edges: 120
            })
        ));
    }

    #[rstest]
    #[case::g1_non_positive(0, 100)]
    #[case::g2_non_positive(1000, 0)]
    #[case::g1_not_multiple_of_g2(1000, 300)]
    fn transfer_construction_rejects_invalid_grid(#[case] g1: i64, #[case] g2: i64) {
        let people = [
            PersonBalance {
                id: 1,
                balance: 100,
            },
            PersonBalance {
                id: 2,
                balance: -100,
            },
        ];

        let result = construct_settlement_transfers(people, &[1, 2], &[1], g1, g2);
        assert!(
            matches!(result, Err(SettlementError::InvalidGrid { g1: eg1, g2: eg2 }) if eg1 == g1 && eg2 == g2)
        );
    }

    #[rstest]
    #[case::duplicate_settle_members(&[1, 1, 2, 2], &[1])]
    #[case::duplicate_cash_members(&[1, 2], &[1, 1])]
    #[case::unknown_cash_member_ignored(&[1, 2], &[99])]
    fn transfer_construction_is_robust_to_duplicate_or_unknown_member_inputs(
        #[case] settle_members: &[u64],
        #[case] cash_members: &[u64],
    ) {
        let people = [
            PersonBalance {
                id: 1,
                balance: 100,
            },
            PersonBalance {
                id: 2,
                balance: -100,
            },
        ];

        let payments =
            construct_settlement_transfers(people, settle_members, cash_members, 1000, 100)
                .expect("solution");

        assert_eq!(
            payments,
            vec![Payment {
                from: 1,
                to: 2,
                amount: 100,
            }]
        );
    }
}
