#![warn(clippy::uninlined_format_args)]

mod model;

use good_lp::{
    Expression, Solution, SolverModel, Variable, WithInitialSolution, default_solver, variable,
    variables,
};
use thiserror::Error;

pub use model::{Payment, PersonBalance};

pub trait MemberIdTrait: Copy + Eq + std::hash::Hash + Ord {}
impl MemberIdTrait for u64 {}

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

pub fn minimize_transactions<MemberId: MemberIdTrait>(
    people: impl IntoIterator<Item = PersonBalance<MemberId>>,
    alpha: f64,
    beta: f64,
) -> Result<Vec<Payment<MemberId>>, SettlementError> {
    let people: Vec<PersonBalance<MemberId>> = people.into_iter().collect();
    let total: i64 = people.iter().map(|p| p.balance).sum();
    if total != 0 {
        return Err(SettlementError::ImbalancedTotal(total));
    }

    let amount_scale = scaling_divisor_for_balances(&people);
    let solver_people: Vec<PersonBalance<MemberId>> = if amount_scale > 1 {
        people
            .iter()
            .map(|person| PersonBalance {
                id: person.id,
                balance: person.balance / amount_scale,
            })
            .collect()
    } else {
        people.clone()
    };

    let mut vars = variables!();

    let mut debtors = Vec::new();
    let mut creditors = Vec::new();
    for (idx, person) in solver_people.iter().enumerate() {
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

    for &(_, _, max_amount) in &pairs {
        let flow_var = vars.add(variable().integer().min(0.0).max(max_amount));
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

    let mut out_edges: Vec<Vec<usize>> = vec![Vec::new(); people.len()];
    let mut in_edges: Vec<Vec<usize>> = vec![Vec::new(); people.len()];
    for (edge_idx, &(from, to, _)) in pairs.iter().enumerate() {
        out_edges[from].push(edge_idx);
        in_edges[to].push(edge_idx);
    }

    // Balance constraints: inflow - outflow = balance
    for (member_idx, person) in solver_people.iter().enumerate() {
        let mut constraint = Expression::default();
        for &edge_idx in &in_edges[member_idx] {
            constraint.add_mul(1.0, how_much[edge_idx]);
        }
        for &edge_idx in &out_edges[member_idx] {
            constraint.add_mul(-1.0, how_much[edge_idx]);
        }
        problem = problem.with(constraint.eq(person.balance as f64));
    }

    let Ok(solution) = problem.solve() else {
        return Err(SettlementError::NoSolution);
    };

    let mut results = Vec::with_capacity(pairs.len());
    for (idx, &(i, j, _)) in pairs.iter().enumerate() {
        let amount = round_bankers(solution.value(how_much[idx]))
            .checked_mul(amount_scale)
            .ok_or(SettlementError::RoundingMismatch)?;
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

pub fn construct_settlement_transfers<MemberId: MemberIdTrait>(
    people: impl IntoIterator<Item = PersonBalance<MemberId>>,
    settle_members: &[MemberId],
    cash_members: &[MemberId],
    g1: i64,
    g2: i64,
) -> Result<Vec<Payment<MemberId>>, SettlementError> {
    let mut people: Vec<PersonBalance<MemberId>> = people.into_iter().collect();
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

    let amount_scale = scaling_divisor_for_balances_and_grid(&people, g1, g2);
    let (solver_people, solver_g1, solver_g2): (Vec<PersonBalance<MemberId>>, i64, i64) =
        if amount_scale > 1 {
            (
                people
                    .iter()
                    .map(|person| PersonBalance {
                        id: person.id,
                        balance: person.balance / amount_scale,
                    })
                    .collect(),
                g1 / amount_scale,
                g2 / amount_scale,
            )
        } else {
            (people.clone(), g1, g2)
        };

    let model = TransferModel::from_people(&solver_people, settle_members, cash_members);
    if model.edges.is_empty() {
        return Ok(Vec::new());
    }
    if model.edges.len() > MAX_LEXICOGRAPHIC_EDGES {
        return Err(SettlementError::ModelTooLarge {
            edge_count: model.edges.len(),
            max_edges: MAX_LEXICOGRAPHIC_EDGES,
        });
    }

    let Some(lex_fixed) = solve_full_lexicographic(&model, solver_g1, solver_g2) else {
        return Err(SettlementError::NoSolution);
    };

    let mut transfers: Vec<Payment<MemberId>> = Vec::new();
    for (edge, amount) in model.edges.iter().zip(lex_fixed) {
        let amount = round_bankers(amount)
            .checked_mul(amount_scale)
            .ok_or(SettlementError::RoundingMismatch)?;
        if amount <= 0 {
            continue;
        }
        transfers.push(Payment {
            from: edge.from,
            to: edge.to,
            amount,
        });
    }
    transfers.sort_unstable_by_key(|payment| (payment.from, payment.to));

    if !is_transfer_result_consistent(&people, settle_members, &transfers) {
        return Err(SettlementError::RoundingMismatch);
    }

    Ok(transfers)
}

fn is_transfer_result_consistent<MemberId: MemberIdTrait>(
    people: &[PersonBalance<MemberId>],
    settle_members: &[MemberId],
    transfers: &[Payment<MemberId>],
) -> bool {
    let initial_balances: std::collections::HashMap<MemberId, i64> = people
        .iter()
        .map(|person| (person.id, person.balance))
        .collect();
    let mut balances: std::collections::HashMap<MemberId, i64> = people
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

fn solve_full_lexicographic<MemberId: MemberIdTrait>(
    model: &TransferModel<MemberId>,
    g1: i64,
    g2: i64,
) -> Option<Vec<f64>> {
    let mut fixed = FixedTargets::default();
    let mut warm_start: Option<WarmStartValues> = None;

    let has_cash_objective = model.edges.iter().any(|edge| edge.payer_is_cash);
    if has_cash_objective {
        let stage1 = solve_transfer_stage(
            model,
            ObjectiveKind::Obj1,
            &fixed,
            &[],
            warm_start.as_ref(),
            g1,
            g2,
        )?;
        warm_start = Some(stage1.warm_start.clone());
        fixed.obj1 = Some(round_count_objective(stage1.obj1));

        let stage2 = solve_transfer_stage(
            model,
            ObjectiveKind::Obj2,
            &fixed,
            &[],
            warm_start.as_ref(),
            g1,
            g2,
        )?;
        warm_start = Some(stage2.warm_start.clone());
        fixed.obj2 = Some(round_count_objective(stage2.obj2));
    } else {
        fixed.obj1 = Some(0.0);
        fixed.obj2 = Some(0.0);
    }

    let stage3 = solve_transfer_stage(
        model,
        ObjectiveKind::ObjW,
        &fixed,
        &[],
        warm_start.as_ref(),
        g1,
        g2,
    )?;
    warm_start = Some(stage3.warm_start.clone());
    fixed.objw = Some(round_count_objective(stage3.objw));

    let stage4 = solve_transfer_stage(
        model,
        ObjectiveKind::Obj3,
        &fixed,
        &[],
        warm_start.as_ref(),
        g1,
        g2,
    )?;
    warm_start = Some(stage4.warm_start.clone());
    fixed.obj3 = Some(round_count_objective(stage4.obj3));

    let mut lex_fixed: Vec<f64> = Vec::with_capacity(model.edges.len());
    let max_upper = model
        .edges
        .iter()
        .map(|edge| edge.upper_bound)
        .max()
        .unwrap_or(0);
    let lex_base = (max_upper + 1) as f64;
    let lex_block_size = choose_lex_block_size(max_upper);

    for start in (0..model.edges.len()).step_by(lex_block_size) {
        let end = (start + lex_block_size).min(model.edges.len());
        let stage = solve_transfer_stage(
            model,
            ObjectiveKind::LexBlock {
                start,
                end,
                base: lex_base,
            },
            &fixed,
            &lex_fixed,
            warm_start.as_ref(),
            g1,
            g2,
        )?;
        warm_start = Some(stage.warm_start.clone());
        for idx in start..end {
            lex_fixed.push(stage.x_values[idx]);
        }
    }
    Some(lex_fixed)
}

fn choose_lex_block_size(max_upper: i64) -> usize {
    const TARGET_BLOCK_SIZE: usize = 10;
    const MAX_SAFE_INTEGER: f64 = 9_007_199_254_740_992.0;

    let base = (max_upper + 1) as f64;
    if !base.is_finite() || base <= 1.0 {
        return TARGET_BLOCK_SIZE;
    }

    let mut safe_size = 1usize;
    let mut objective_bound = base;
    while safe_size < TARGET_BLOCK_SIZE && objective_bound * base <= MAX_SAFE_INTEGER {
        objective_bound *= base;
        safe_size += 1;
    }

    safe_size
}

#[derive(Clone, Copy)]
enum ObjectiveKind {
    Obj1,
    Obj2,
    ObjW,
    Obj3,
    LexBlock { start: usize, end: usize, base: f64 },
}

#[derive(Default)]
struct FixedTargets {
    obj1: Option<f64>,
    obj2: Option<f64>,
    objw: Option<f64>,
    obj3: Option<f64>,
}

#[derive(Clone, Copy)]
struct Edge<MemberId> {
    from: MemberId,
    to: MemberId,
    upper_bound: i64,
    payer_is_cash: bool,
    touches_non_settle: bool,
}

struct TransferModel<MemberId> {
    payers: Vec<(MemberId, i64)>,
    receivers: Vec<(MemberId, i64)>,
    edges: Vec<Edge<MemberId>>,
    cash_edge_indices: Vec<usize>,
    payer_edges: Vec<Vec<usize>>,
    receiver_edges: Vec<Vec<usize>>,
    settle_lookup: std::collections::HashSet<MemberId>,
}

impl<MemberId: MemberIdTrait> TransferModel<MemberId> {
    fn from_people(
        people: &[PersonBalance<MemberId>],
        settle_members: &[MemberId],
        cash_members: &[MemberId],
    ) -> Self {
        let settle_lookup: std::collections::HashSet<MemberId> =
            settle_members.iter().copied().collect();
        let cash_lookup: std::collections::HashSet<MemberId> =
            cash_members.iter().copied().collect();

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
        let mut cash_edge_indices = Vec::new();

        for (payer_idx, (payer_member, pay)) in payers.iter().enumerate() {
            for (receiver_idx, (receiver_member, recv)) in receivers.iter().enumerate() {
                let upper_bound = (*pay).min(*recv);
                if upper_bound <= 0 {
                    continue;
                }

                let payer_is_settle = settle_lookup.contains(payer_member);
                let receiver_is_settle = settle_lookup.contains(receiver_member);
                // Non-settle <-> non-settle edges cannot help settle requested members,
                // so we prune them to keep the MILP graph minimal.
                if !payer_is_settle && !receiver_is_settle {
                    continue;
                }

                let payer_is_cash = cash_lookup.contains(payer_member);
                let edge_idx = edges.len();
                edges.push(Edge {
                    from: *payer_member,
                    to: *receiver_member,
                    upper_bound,
                    payer_is_cash,
                    touches_non_settle: !(payer_is_settle && receiver_is_settle),
                });
                if payer_is_cash {
                    cash_edge_indices.push(edge_idx);
                }
                payer_edges[payer_idx].push(edge_idx);
                receiver_edges[receiver_idx].push(edge_idx);
            }
        }

        Self {
            payers,
            receivers,
            edges,
            cash_edge_indices,
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
    warm_start: WarmStartValues,
}

#[derive(Clone)]
struct WarmStartValues {
    x_values: Vec<f64>,
    y_values: Vec<f64>,
    cash_z1_values: Vec<f64>,
    cash_z2_values: Vec<f64>,
    cash_a1_values: Vec<f64>,
    cash_a2_values: Vec<f64>,
    cash_r1_values: Vec<f64>,
    cash_r2_values: Vec<f64>,
    s_values: Vec<f64>,
    t_values: Vec<f64>,
}

fn build_obj1_expr(z1_vars: &[Variable]) -> Expression {
    let mut expr = Expression::default();
    for var in z1_vars {
        expr.add_mul(1.0, *var);
    }
    expr
}

fn build_obj2_expr(z2_vars: &[Variable]) -> Expression {
    let mut expr = Expression::default();
    for var in z2_vars {
        expr.add_mul(1.0, *var);
    }
    expr
}

fn build_objw_expr<MemberId: MemberIdTrait>(
    model: &TransferModel<MemberId>,
    y_vars: &[Variable],
) -> Expression {
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

fn build_lex_block_expr(x_vars: &[Variable], start: usize, end: usize, base: f64) -> Expression {
    let mut expr = Expression::default();
    let mut weight = 1.0;
    for _ in (start + 1)..end {
        weight *= base;
    }
    for var in x_vars.iter().take(end).skip(start) {
        expr.add_mul(weight, *var);
        weight /= base;
    }
    expr
}

struct WarmStartTargets<'a> {
    x_vars: &'a [Variable],
    y_vars: &'a [Variable],
    z1_vars: &'a [Variable],
    z2_vars: &'a [Variable],
    a1_vars: &'a [Variable],
    a2_vars: &'a [Variable],
    r1_vars: &'a [Variable],
    r2_vars: &'a [Variable],
    s_vars: &'a [Variable],
    t_vars: &'a [Variable],
}

fn build_warm_start_seed(
    seed: &WarmStartValues,
    targets: &WarmStartTargets<'_>,
) -> Vec<(Variable, f64)> {
    let mut initial_solution: Vec<(Variable, f64)> = Vec::with_capacity(
        seed.x_values.len() * 2
            + seed.cash_z1_values.len() * 6
            + seed.s_values.len()
            + seed.t_values.len(),
    );
    initial_solution.extend(
        targets
            .x_vars
            .iter()
            .copied()
            .zip(seed.x_values.iter().copied()),
    );
    initial_solution.extend(
        targets
            .y_vars
            .iter()
            .copied()
            .zip(seed.y_values.iter().copied()),
    );
    initial_solution.extend(
        targets
            .z1_vars
            .iter()
            .copied()
            .zip(seed.cash_z1_values.iter().copied()),
    );
    initial_solution.extend(
        targets
            .z2_vars
            .iter()
            .copied()
            .zip(seed.cash_z2_values.iter().copied()),
    );
    initial_solution.extend(
        targets
            .a1_vars
            .iter()
            .copied()
            .zip(seed.cash_a1_values.iter().copied()),
    );
    initial_solution.extend(
        targets
            .a2_vars
            .iter()
            .copied()
            .zip(seed.cash_a2_values.iter().copied()),
    );
    initial_solution.extend(
        targets
            .r1_vars
            .iter()
            .copied()
            .zip(seed.cash_r1_values.iter().copied()),
    );
    initial_solution.extend(
        targets
            .r2_vars
            .iter()
            .copied()
            .zip(seed.cash_r2_values.iter().copied()),
    );
    initial_solution.extend(
        targets
            .s_vars
            .iter()
            .copied()
            .zip(seed.s_values.iter().copied()),
    );
    initial_solution.extend(
        targets
            .t_vars
            .iter()
            .copied()
            .zip(seed.t_values.iter().copied()),
    );
    initial_solution
}

#[allow(clippy::too_many_lines)]
fn solve_transfer_stage<MemberId: MemberIdTrait>(
    model: &TransferModel<MemberId>,
    objective_kind: ObjectiveKind,
    fixed: &FixedTargets,
    lex_prefix: &[f64],
    warm_start: Option<&WarmStartValues>,
    g1: i64,
    g2: i64,
) -> Option<StageSolution> {
    const EPS: f64 = 1e-6;
    let mut vars = variables!();

    // Phase 1: declare variables with tight bounds.
    let mut x_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());
    let mut y_vars: Vec<Variable> = Vec::with_capacity(model.edges.len());
    let mut cash_edge_to_idx: Vec<Option<usize>> = vec![None; model.edges.len()];
    let mut z1_vars: Vec<Variable> = Vec::with_capacity(model.cash_edge_indices.len());
    let mut z2_vars: Vec<Variable> = Vec::with_capacity(model.cash_edge_indices.len());
    let mut a1_vars: Vec<Variable> = Vec::with_capacity(model.cash_edge_indices.len());
    let mut a2_vars: Vec<Variable> = Vec::with_capacity(model.cash_edge_indices.len());
    let mut r1_vars: Vec<Variable> = Vec::with_capacity(model.cash_edge_indices.len());
    let mut r2_vars: Vec<Variable> = Vec::with_capacity(model.cash_edge_indices.len());

    let g1f = g1 as f64;
    let g2f = g2 as f64;
    let max_a2 = ((g1 - 1) / g2) as f64;

    for edge in &model.edges {
        let upper = edge.upper_bound as f64;

        x_vars.push(vars.add(variable().integer().min(0.0).max(upper)));
        y_vars.push(vars.add(variable().binary()));
    }

    for &edge_idx in &model.cash_edge_indices {
        cash_edge_to_idx[edge_idx] = Some(z1_vars.len());
        let max_a1 = (model.edges[edge_idx].upper_bound / g1) as f64;

        z1_vars.push(vars.add(variable().binary()));
        z2_vars.push(vars.add(variable().binary()));
        a1_vars.push(vars.add(variable().integer().min(0.0).max(max_a1)));
        a2_vars.push(vars.add(variable().integer().min(0.0).max(max_a2)));
        r1_vars.push(vars.add(variable().integer().min(0.0).max((g1 - 1) as f64)));
        r2_vars.push(vars.add(variable().integer().min(0.0).max((g2 - 1) as f64)));
    }

    let mut s_vars: Vec<Variable> = Vec::with_capacity(model.payers.len());
    for (_, pay) in &model.payers {
        s_vars.push(vars.add(variable().integer().min(0.0).max(*pay as f64)));
    }

    let mut t_vars: Vec<Variable> = Vec::with_capacity(model.receivers.len());
    for (_, recv) in &model.receivers {
        t_vars.push(vars.add(variable().integer().min(0.0).max(*recv as f64)));
    }

    let mut obj1_expr = Some(build_obj1_expr(&z1_vars));
    let mut obj2_expr = Some(build_obj2_expr(&z2_vars));
    let mut objw_expr = Some(build_objw_expr(model, &y_vars));
    let mut obj3_expr = Some(build_obj3_expr(&y_vars));

    // Phase 2: pick this stage objective while preserving reusable expressions
    // for later fixed-target constraints.
    let objective_expr = match objective_kind {
        ObjectiveKind::Obj1 => obj1_expr
            .take()
            .unwrap_or_else(|| build_obj1_expr(&z1_vars)),
        ObjectiveKind::Obj2 => obj2_expr
            .take()
            .unwrap_or_else(|| build_obj2_expr(&z2_vars)),
        ObjectiveKind::ObjW => objw_expr
            .take()
            .unwrap_or_else(|| build_objw_expr(model, &y_vars)),
        ObjectiveKind::Obj3 => obj3_expr.take().unwrap_or_else(|| build_obj3_expr(&y_vars)),
        ObjectiveKind::LexBlock { start, end, base } => {
            build_lex_block_expr(&x_vars, start, end, base)
        }
    };

    let mut problem = vars.minimise(objective_expr).using(default_solver);
    // Phase 3: warm-start from the previous lexicographic stage.
    if let Some(seed) = warm_start {
        let targets = WarmStartTargets {
            x_vars: &x_vars,
            y_vars: &y_vars,
            z1_vars: &z1_vars,
            z2_vars: &z2_vars,
            a1_vars: &a1_vars,
            a2_vars: &a2_vars,
            r1_vars: &r1_vars,
            r2_vars: &r2_vars,
            s_vars: &s_vars,
            t_vars: &t_vars,
        };
        let initial_solution = build_warm_start_seed(seed, &targets);
        problem = problem.with_initial_solution(initial_solution);
    }

    // Phase 4: enforce flow conservation and settle-member exact zeroing.
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

    // Phase 5: edge activation and cash-divisibility structure.
    for (edge_idx, edge) in model.edges.iter().enumerate() {
        let upper = edge.upper_bound as f64;
        problem = problem.with((x_vars[edge_idx] - upper * y_vars[edge_idx]).leq(0.0));

        if let Some(cash_idx) = cash_edge_to_idx[edge_idx] {
            problem = problem
                .with((x_vars[edge_idx] - g1f * a1_vars[cash_idx] - r1_vars[cash_idx]).eq(0.0))
                .with((r1_vars[cash_idx] - g2f * a2_vars[cash_idx] - r2_vars[cash_idx]).eq(0.0))
                .with((r1_vars[cash_idx] - ((g1 - 1) as f64) * z1_vars[cash_idx]).leq(0.0))
                .with((r2_vars[cash_idx] - ((g2 - 1) as f64) * z2_vars[cash_idx]).leq(0.0))
                .with((r1_vars[cash_idx] - z1_vars[cash_idx]).geq(0.0))
                .with((r2_vars[cash_idx] - z2_vars[cash_idx]).geq(0.0))
                .with((z2_vars[cash_idx] - z1_vars[cash_idx]).leq(0.0))
                .with((z1_vars[cash_idx] - y_vars[edge_idx]).leq(0.0))
                .with((z2_vars[cash_idx] - y_vars[edge_idx]).leq(0.0));
        }
    }

    // Phase 6: lock objective values from earlier stages and fix lex prefix.
    if let Some(target) = fixed.obj1 {
        let expr = obj1_expr
            .take()
            .unwrap_or_else(|| build_obj1_expr(&z1_vars));
        problem = problem.with(expr.leq(round_count_objective(target) + EPS));
    }
    if let Some(target) = fixed.obj2 {
        let expr = obj2_expr
            .take()
            .unwrap_or_else(|| build_obj2_expr(&z2_vars));
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
    let y_values: Vec<f64> = y_vars.iter().map(|var| solution.value(*var)).collect();
    let cash_z1_values: Vec<f64> = z1_vars.iter().map(|var| solution.value(*var)).collect();
    let cash_z2_values: Vec<f64> = z2_vars.iter().map(|var| solution.value(*var)).collect();
    let cash_a1_values: Vec<f64> = a1_vars.iter().map(|var| solution.value(*var)).collect();
    let cash_a2_values: Vec<f64> = a2_vars.iter().map(|var| solution.value(*var)).collect();
    let cash_r1_values: Vec<f64> = r1_vars.iter().map(|var| solution.value(*var)).collect();
    let cash_r2_values: Vec<f64> = r2_vars.iter().map(|var| solution.value(*var)).collect();
    let s_values: Vec<f64> = s_vars.iter().map(|var| solution.value(*var)).collect();
    let t_values: Vec<f64> = t_vars.iter().map(|var| solution.value(*var)).collect();
    let obj1 = z1_vars.iter().map(|var| solution.value(*var)).sum();
    let obj2 = z2_vars.iter().map(|var| solution.value(*var)).sum();
    let objw = model
        .edges
        .iter()
        .enumerate()
        .filter(|(_, edge)| edge.touches_non_settle)
        .map(|(idx, _)| solution.value(y_vars[idx]))
        .sum();
    let obj3 = y_vars.iter().map(|var| solution.value(*var)).sum();

    let warm_start = WarmStartValues {
        x_values: x_values.clone(),
        y_values,
        cash_z1_values,
        cash_z2_values,
        cash_a1_values,
        cash_a2_values,
        cash_r1_values,
        cash_r2_values,
        s_values,
        t_values,
    };

    Some(StageSolution {
        obj1,
        obj2,
        objw,
        obj3,
        x_values,
        warm_start,
    })
}

fn gcd_u64(mut lhs: u64, mut rhs: u64) -> u64 {
    while rhs != 0 {
        let rem = lhs % rhs;
        lhs = rhs;
        rhs = rem;
    }
    lhs
}

fn gcd_of_nonzero_balances<MemberId>(people: &[PersonBalance<MemberId>]) -> u64 {
    people
        .iter()
        .map(|person| person.balance.unsigned_abs())
        .filter(|&value| value != 0)
        .reduce(gcd_u64)
        .unwrap_or(0)
}

fn normalize_scale(raw_scale: u64) -> i64 {
    if raw_scale <= 1 || raw_scale > i64::MAX as u64 {
        return 1;
    }
    raw_scale as i64
}

fn scaling_divisor_for_balances<MemberId>(people: &[PersonBalance<MemberId>]) -> i64 {
    normalize_scale(gcd_of_nonzero_balances(people))
}

fn scaling_divisor_for_balances_and_grid<MemberId>(
    people: &[PersonBalance<MemberId>],
    g1: i64,
    g2: i64,
) -> i64 {
    let mut scale = gcd_of_nonzero_balances(people);
    if scale == 0 {
        return 1;
    }
    scale = gcd_u64(scale, g1.unsigned_abs());
    scale = gcd_u64(scale, g2.unsigned_abs());
    normalize_scale(scale)
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
