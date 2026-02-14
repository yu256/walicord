#![warn(clippy::uninlined_format_args)]

mod model;

use highs::{HighsModelStatus, HighsSolutionStatus, Model, RowProblem, Sense};
pub use model::{Payment, PersonBalance};
use thiserror::Error;

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
const MAX_SAFE_EXACT_INT_IN_F64: i128 = (1_i128 << 53) - 1;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum HighsSolvePreset {
    Deterministic,
    Fast,
}

#[derive(Clone, Copy, PartialEq)]
pub struct HighsCommonSolveOptions {
    pub preset: HighsSolvePreset,
    pub threads: Option<i32>,
    pub time_limit_seconds: Option<f64>,
    pub mip_rel_gap: Option<f64>,
    pub mip_abs_gap: Option<f64>,
    pub accept_feasible_on_limit: bool,
}

impl Default for HighsCommonSolveOptions {
    fn default() -> Self {
        Self {
            preset: HighsSolvePreset::Deterministic,
            threads: Some(1),
            time_limit_seconds: None,
            mip_rel_gap: None,
            mip_abs_gap: None,
            accept_feasible_on_limit: false,
        }
    }
}

impl HighsCommonSolveOptions {
    pub fn fast() -> Self {
        Self {
            preset: HighsSolvePreset::Fast,
            threads: default_fast_threads(),
            ..Self::default()
        }
    }
}

fn default_fast_threads() -> Option<i32> {
    std::thread::available_parallelism()
        .ok()
        .and_then(|parallelism| i32::try_from(parallelism.get()).ok())
        .map(|threads| threads.min(8))
        .filter(|threads| *threads > 0)
}

#[derive(Clone, PartialEq)]
pub struct TransferSolveOptions {
    pub highs: HighsCommonSolveOptions,
    pub mod_100_policy: Mod100Policy,
    pub emit_failure_diagnostics: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Mod100Policy {
    /// Ignore mod-100 decomposition objective and constraints.
    Off,
    /// Include mod-100 decomposition objective and constraints.
    Prioritize,
}

impl Default for TransferSolveOptions {
    fn default() -> Self {
        Self {
            highs: HighsCommonSolveOptions::default(),
            mod_100_policy: Mod100Policy::Prioritize,
            emit_failure_diagnostics: false,
        }
    }
}

impl TransferSolveOptions {
    pub fn fast() -> Self {
        Self {
            highs: HighsCommonSolveOptions::fast(),
            ..Self::default()
        }
    }

    pub fn with_highs(mut self, highs: HighsCommonSolveOptions) -> Self {
        self.highs = highs;
        self
    }
}

#[derive(Clone, Copy, Default)]
pub struct TransferBuildOptions {
    pub non_settle_topk: Option<usize>,
}

#[derive(Clone, Default)]
pub struct SettlementTransferOptions {
    pub build: TransferBuildOptions,
    pub solve: TransferSolveOptions,
}

impl SettlementTransferOptions {
    pub fn with_solve(mut self, solve: TransferSolveOptions) -> Self {
        self.solve = solve;
        self
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SolveQuality {
    Optimal,
    AcceptedFeasibleOnLimit {
        active_edge_count: i64,
        objective_value: f64,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MinimizeSolveOutcome<MemberId> {
    pub payments: Vec<Payment<MemberId>>,
    pub quality: SolveQuality,
}

pub fn minimize_transactions<MemberId: MemberIdTrait>(
    people: impl IntoIterator<Item = PersonBalance<MemberId>>,
    alpha: f64,
    beta: f64,
) -> Result<Vec<Payment<MemberId>>, SettlementError> {
    minimize_transactions_with_options(people, alpha, beta, HighsCommonSolveOptions::default())
}

pub fn minimize_transactions_with_options<MemberId: MemberIdTrait>(
    people: impl IntoIterator<Item = PersonBalance<MemberId>>,
    alpha: f64,
    beta: f64,
    solve_options: HighsCommonSolveOptions,
) -> Result<Vec<Payment<MemberId>>, SettlementError> {
    Ok(
        minimize_transactions_with_options_and_outcome(people, alpha, beta, solve_options)?
            .payments,
    )
}

pub(crate) fn minimize_transactions_with_options_and_outcome<MemberId: MemberIdTrait>(
    people: impl IntoIterator<Item = PersonBalance<MemberId>>,
    alpha: f64,
    beta: f64,
    solve_options: HighsCommonSolveOptions,
) -> Result<MinimizeSolveOutcome<MemberId>, SettlementError> {
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
        return Ok(MinimizeSolveOutcome {
            payments: Vec::new(),
            quality: SolveQuality::Optimal,
        });
    }

    let mut pairs = Vec::with_capacity(debtors.len() * creditors.len());
    for &(debtor_idx, debtor_amount) in &debtors {
        for &(creditor_idx, creditor_amount) in &creditors {
            let max_amount = debtor_amount.min(creditor_amount) as f64;
            pairs.push((debtor_idx, creditor_idx, max_amount));
        }
    }

    let mut problem = RowProblem::new();
    let mut how_much = Vec::with_capacity(pairs.len());
    let mut who_pays = Vec::with_capacity(pairs.len());

    for &(_, _, max_amount) in &pairs {
        let flow_var = problem.add_integer_column(beta, 0.0..=max_amount);
        // highs::RowProblem has no dedicated binary helper, so we model binary with integer [0, 1].
        let binary_var = problem.add_integer_column(alpha, 0.0..=1.0);
        how_much.push(flow_var);
        who_pays.push(binary_var);
    }

    for ((&hm, &wp), &(_, _, max_amount)) in how_much.iter().zip(&who_pays).zip(&pairs) {
        problem.add_row(..=0.0, [(hm, 1.0), (wp, -max_amount)]);
        problem.add_row(0.0.., [(hm, 1.0), (wp, -1.0)]);
    }

    let mut out_edges: Vec<Vec<usize>> = vec![Vec::new(); people.len()];
    let mut in_edges: Vec<Vec<usize>> = vec![Vec::new(); people.len()];
    for (edge_idx, &(from, to, _)) in pairs.iter().enumerate() {
        out_edges[from].push(edge_idx);
        in_edges[to].push(edge_idx);
    }

    // Balance constraints: inflow - outflow = balance
    for (member_idx, person) in solver_people.iter().enumerate() {
        let mut factors =
            Vec::with_capacity(in_edges[member_idx].len() + out_edges[member_idx].len());
        for &edge_idx in &in_edges[member_idx] {
            factors.push((how_much[edge_idx], 1.0));
        }
        for &edge_idx in &out_edges[member_idx] {
            factors.push((how_much[edge_idx], -1.0));
        }
        problem.add_row(person.balance as f64..=person.balance as f64, factors);
    }

    let mut model = problem.optimise(Sense::Minimise);
    model.make_quiet();
    apply_highs_options(&mut model, &solve_options);
    let solved = model.solve();
    let status = solved.status();
    let accepted_feasible_on_limit = status != HighsModelStatus::Optimal
        && matches!(
            status,
            HighsModelStatus::ReachedTimeLimit
                | HighsModelStatus::ReachedIterationLimit
                | HighsModelStatus::ReachedSolutionLimit
                | HighsModelStatus::ReachedInterrupt
                | HighsModelStatus::ReachedMemoryLimit
        )
        && solve_options.accept_feasible_on_limit
        && solved.primal_solution_status() == HighsSolutionStatus::Feasible;
    if status != HighsModelStatus::Optimal && !accepted_feasible_on_limit {
        return Err(SettlementError::NoSolution);
    }

    let solution = solved.get_solution();
    let quality = if accepted_feasible_on_limit {
        let active_edge_count = who_pays
            .iter()
            .map(|col| i64::from(round_bankers(solution[*col]) > 0))
            .sum();
        let objective_value = who_pays
            .iter()
            .map(|col| alpha * f64::from(i32::from(round_bankers(solution[*col]) > 0)))
            .sum::<f64>()
            + how_much
                .iter()
                .map(|col| beta * round_bankers(solution[*col]) as f64)
                .sum::<f64>();
        SolveQuality::AcceptedFeasibleOnLimit {
            active_edge_count,
            objective_value,
        }
    } else {
        SolveQuality::Optimal
    };

    let mut results = Vec::with_capacity(pairs.len());
    for (idx, &(i, j, _)) in pairs.iter().enumerate() {
        let amount = round_bankers(solution[how_much[idx]])
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

    Ok(MinimizeSolveOutcome {
        payments: results,
        quality,
    })
}

pub fn construct_settlement_transfers<MemberId: MemberIdTrait>(
    people: impl IntoIterator<Item = PersonBalance<MemberId>>,
    settle_members: &[MemberId],
    cash_members: &[MemberId],
    g1: i64,
    g2: i64,
) -> Result<Vec<Payment<MemberId>>, SettlementError> {
    construct_settlement_transfers_with_options(
        people,
        settle_members,
        cash_members,
        g1,
        g2,
        SettlementTransferOptions::default(),
    )
}

pub fn construct_settlement_transfers_with_options<MemberId: MemberIdTrait>(
    people: impl IntoIterator<Item = PersonBalance<MemberId>>,
    settle_members: &[MemberId],
    cash_members: &[MemberId],
    g1: i64,
    g2: i64,
    options: SettlementTransferOptions,
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

    let mut model = TransferModel::from_people_with_options(
        &solver_people,
        settle_members,
        cash_members,
        options.build,
    );
    if model.edges.is_empty() {
        return Ok(Vec::new());
    }
    if model.edges.len() > MAX_LEXICOGRAPHIC_EDGES {
        return Err(SettlementError::ModelTooLarge {
            edge_count: model.edges.len(),
            max_edges: MAX_LEXICOGRAPHIC_EDGES,
        });
    }

    let attempted_pruned = options.build.non_settle_topk.is_some();
    let mut attempted_unpruned = !attempted_pruned;
    let mut lex_fixed = solve_transfers_highs(&model, solver_g1, solver_g2, &options.solve);
    if matches!(lex_fixed, Err(SolveTransfersError::Infeasible))
        && options.build.non_settle_topk.is_some()
    {
        model = TransferModel::from_people_with_options(
            &solver_people,
            settle_members,
            cash_members,
            TransferBuildOptions::default(),
        );
        if model.edges.is_empty() {
            return Ok(Vec::new());
        }
        if model.edges.len() > MAX_LEXICOGRAPHIC_EDGES {
            return Err(SettlementError::ModelTooLarge {
                edge_count: model.edges.len(),
                max_edges: MAX_LEXICOGRAPHIC_EDGES,
            });
        }
        attempted_unpruned = true;
        lex_fixed = solve_transfers_highs(&model, solver_g1, solver_g2, &options.solve);
    }

    if matches!(lex_fixed, Err(SolveTransfersError::LimitReached))
        && let Some(strict) = strict_retry_solve_options(&options.solve.highs)
    {
        let strict_transfer_options = options.solve.clone().with_highs(strict);
        lex_fixed = solve_transfers_highs(&model, solver_g1, solver_g2, &strict_transfer_options);
    }

    let lex_fixed = match lex_fixed {
        Ok(values) => values,
        Err(error) => {
            if options.solve.emit_failure_diagnostics {
                if let SolveTransfersError::UnsafeWeights(diagnostics) = error {
                    tracing::warn!(
                        error = "unsafe_weights",
                        attempted_pruned,
                        attempted_unpruned,
                        edge_count_usize = diagnostics.edge_count_usize,
                        cash_edge_count_usize = diagnostics.cash_edge_count_usize,
                        non_settle_edge_count_usize = diagnostics.non_settle_edge_count_usize,
                        edge_count = diagnostics.edge_count,
                        cash_edge_count = diagnostics.cash_edge_count,
                        non_settle_edge_count = diagnostics.non_settle_edge_count,
                        failed_stage = ?diagnostics.failed_stage,
                        lower_max = ?diagnostics.lower_max,
                        obj1_weight = ?diagnostics.obj1_weight,
                        obj2_weight = ?diagnostics.obj2_weight,
                        max_objective_value = ?diagnostics.max_objective_value,
                        "transfer solve failed due to unsafe objective weights"
                    );
                } else {
                    tracing::warn!(
                        ?error,
                        attempted_pruned,
                        attempted_unpruned,
                        edge_count = model.edges.len(),
                        cash_edge_count = model.cash_edge_indices.len(),
                        "transfer solve failed"
                    );
                }
            }
            return Err(SettlementError::NoSolution);
        }
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

#[derive(Debug)]
enum SolveTransfersError {
    Infeasible,
    UnsafeWeights(Box<UnsafeWeightDiagnostics>),
    LimitReached,
    SolverStatus,
}

#[derive(Debug, Clone, Copy)]
enum UnsafeWeightStage {
    ConvertEdgeCount,
    ConvertCashEdgeCount,
    ConvertNonSettleEdgeCount,
    LowerMax,
    Obj2,
    Obj1,
    Total,
    F64Check,
}

#[derive(Debug, Clone, Copy)]
struct UnsafeWeightDiagnostics {
    failed_stage: Option<UnsafeWeightStage>,
    edge_count_usize: usize,
    cash_edge_count_usize: usize,
    non_settle_edge_count_usize: usize,
    edge_count: i128,
    cash_edge_count: i128,
    non_settle_edge_count: i128,
    lower_max: Option<i128>,
    obj1_weight: Option<i128>,
    obj2_weight: Option<i128>,
    max_objective_value: Option<i128>,
}

struct ObjectiveBounds {
    include_obj2: bool,
    max_y: i128,
    max_z1: i128,
    max_z2: i128,
    objw_weight: i128,
    obj2_weight: i128,
    obj1_weight: i128,
    total: i128,
}

fn compute_objective_bounds<MemberId: MemberIdTrait>(
    model: &TransferModel<MemberId>,
    solve_options: &TransferSolveOptions,
) -> Result<ObjectiveBounds, SolveTransfersError> {
    let include_obj2 = matches!(solve_options.mod_100_policy, Mod100Policy::Prioritize);
    let edge_count_usize = model.edges.len();
    let cash_edge_count_usize = model.cash_edge_indices.len();
    let non_settle_edge_count_usize = model
        .edges
        .iter()
        .filter(|edge| edge.touches_non_settle)
        .count();

    let max_y = i128::try_from(edge_count_usize).map_err(|_| {
        SolveTransfersError::UnsafeWeights(Box::new(UnsafeWeightDiagnostics {
            failed_stage: Some(UnsafeWeightStage::ConvertEdgeCount),
            edge_count_usize,
            cash_edge_count_usize,
            non_settle_edge_count_usize,
            edge_count: 0,
            cash_edge_count: 0,
            non_settle_edge_count: 0,
            lower_max: None,
            obj1_weight: None,
            obj2_weight: None,
            max_objective_value: None,
        }))
    })?;
    let max_z1 = i128::try_from(cash_edge_count_usize).map_err(|_| {
        SolveTransfersError::UnsafeWeights(Box::new(UnsafeWeightDiagnostics {
            failed_stage: Some(UnsafeWeightStage::ConvertCashEdgeCount),
            edge_count_usize,
            cash_edge_count_usize,
            non_settle_edge_count_usize,
            edge_count: max_y,
            cash_edge_count: 0,
            non_settle_edge_count: 0,
            lower_max: None,
            obj1_weight: None,
            obj2_weight: None,
            max_objective_value: None,
        }))
    })?;
    let max_z2 = if include_obj2 { max_z1 } else { 0 };
    let max_non_settle_y = i128::try_from(non_settle_edge_count_usize).map_err(|_| {
        SolveTransfersError::UnsafeWeights(Box::new(UnsafeWeightDiagnostics {
            failed_stage: Some(UnsafeWeightStage::ConvertNonSettleEdgeCount),
            edge_count_usize,
            cash_edge_count_usize,
            non_settle_edge_count_usize,
            edge_count: max_y,
            cash_edge_count: max_z1,
            non_settle_edge_count: 0,
            lower_max: None,
            obj1_weight: None,
            obj2_weight: None,
            max_objective_value: None,
        }))
    })?;

    let mut diagnostics = UnsafeWeightDiagnostics {
        failed_stage: None,
        edge_count_usize,
        cash_edge_count_usize,
        non_settle_edge_count_usize,
        edge_count: max_y,
        cash_edge_count: max_z1,
        non_settle_edge_count: max_non_settle_y,
        lower_max: None,
        obj1_weight: None,
        obj2_weight: None,
        max_objective_value: None,
    };

    let obj3_weight = 1_i128;
    let objw_weight = max_y + 1;
    let Some(lower_max) = max_non_settle_y
        .checked_mul(objw_weight)
        .and_then(|value| value.checked_add(max_y))
    else {
        diagnostics.failed_stage = Some(UnsafeWeightStage::LowerMax);
        return Err(SolveTransfersError::UnsafeWeights(Box::new(diagnostics)));
    };
    diagnostics.lower_max = Some(lower_max);
    let obj2_weight = if include_obj2 {
        let Some(value) = lower_max.checked_add(1) else {
            diagnostics.failed_stage = Some(UnsafeWeightStage::Obj2);
            return Err(SolveTransfersError::UnsafeWeights(Box::new(diagnostics)));
        };
        value
    } else {
        0
    };
    diagnostics.obj2_weight = Some(obj2_weight);
    let Some(obj1_weight) = max_z2
        .checked_mul(obj2_weight)
        .and_then(|value| value.checked_add(lower_max))
        .and_then(|value| value.checked_add(1))
    else {
        diagnostics.failed_stage = Some(UnsafeWeightStage::Obj1);
        return Err(SolveTransfersError::UnsafeWeights(Box::new(diagnostics)));
    };
    diagnostics.obj1_weight = Some(obj1_weight);
    let Some(total) = max_z1
        .checked_mul(obj1_weight)
        .and_then(|value| value.checked_add(max_z2.checked_mul(obj2_weight)?))
        .and_then(|value| value.checked_add(lower_max))
    else {
        diagnostics.failed_stage = Some(UnsafeWeightStage::Total);
        return Err(SolveTransfersError::UnsafeWeights(Box::new(diagnostics)));
    };
    diagnostics.max_objective_value = Some(total);

    if !is_exact_integer_in_f64(objw_weight + obj3_weight)
        || !is_exact_integer_in_f64(obj1_weight)
        || (include_obj2 && !is_exact_integer_in_f64(obj2_weight))
        || !is_exact_integer_in_f64(total)
    {
        diagnostics.failed_stage = Some(UnsafeWeightStage::F64Check);
        return Err(SolveTransfersError::UnsafeWeights(Box::new(diagnostics)));
    }

    Ok(ObjectiveBounds {
        include_obj2,
        max_y,
        max_z1,
        max_z2,
        objw_weight,
        obj2_weight,
        obj1_weight,
        total,
    })
}

fn classify_non_optimal_status(status: HighsModelStatus) -> SolveTransfersError {
    match status {
        HighsModelStatus::Infeasible | HighsModelStatus::UnboundedOrInfeasible => {
            SolveTransfersError::Infeasible
        }
        HighsModelStatus::ReachedTimeLimit
        | HighsModelStatus::ReachedIterationLimit
        | HighsModelStatus::ReachedSolutionLimit
        | HighsModelStatus::ReachedInterrupt
        | HighsModelStatus::ReachedMemoryLimit => SolveTransfersError::LimitReached,
        _ => {
            let _ = status;
            SolveTransfersError::SolverStatus
        }
    }
}

fn strict_retry_solve_options(
    options: &HighsCommonSolveOptions,
) -> Option<HighsCommonSolveOptions> {
    let strict = HighsCommonSolveOptions {
        preset: HighsSolvePreset::Deterministic,
        threads: Some(1),
        time_limit_seconds: None,
        mip_rel_gap: None,
        mip_abs_gap: None,
        accept_feasible_on_limit: false,
    };
    (options != &strict).then_some(strict)
}

fn solve_transfers_highs<MemberId: MemberIdTrait>(
    model: &TransferModel<MemberId>,
    g1: i64,
    g2: i64,
    solve_options: &TransferSolveOptions,
) -> Result<Vec<f64>, SolveTransfersError> {
    let bounds = compute_objective_bounds(model, solve_options)?;
    let include_obj2 = bounds.include_obj2;
    let obj3_weight = 1_i128;

    let mut pb = RowProblem::new();
    let mut row_count = 0usize;
    let mut col_count = 0usize;
    let mut x_cols = Vec::with_capacity(model.edges.len());
    let mut y_cols = Vec::with_capacity(model.edges.len());
    let mut cash_edge_to_idx = vec![None; model.edges.len()];

    for edge in &model.edges {
        let y_weight = obj3_weight
            + if edge.touches_non_settle {
                bounds.objw_weight
            } else {
                0
            };
        x_cols.push(pb.add_integer_column(0.0, 0.0..=edge.upper_bound as f64));
        y_cols.push(pb.add_integer_column(y_weight as f64, 0.0..=1.0));
        col_count += 2;
    }

    let mut z1_cols = Vec::with_capacity(model.cash_edge_indices.len());
    let mut q1000_cols = Vec::with_capacity(model.cash_edge_indices.len());
    let mut r1000_cols = Vec::with_capacity(model.cash_edge_indices.len());
    let mut z2_cols = include_obj2.then(|| Vec::with_capacity(model.cash_edge_indices.len()));
    let mut q100_cols = include_obj2.then(|| Vec::with_capacity(model.cash_edge_indices.len()));
    let mut r100_cols = include_obj2.then(|| Vec::with_capacity(model.cash_edge_indices.len()));

    let max_q100 = ((g1 - 1) / g2) as f64;
    for &edge_idx in &model.cash_edge_indices {
        cash_edge_to_idx[edge_idx] = Some(z1_cols.len());
        let edge = model.edges[edge_idx];
        let max_q1000 = (edge.upper_bound / g1) as f64;

        z1_cols.push(pb.add_integer_column(bounds.obj1_weight as f64, 0.0..=1.0));
        if let Some(cols) = &mut z2_cols {
            cols.push(pb.add_integer_column(bounds.obj2_weight as f64, 0.0..=1.0));
        }
        q1000_cols.push(pb.add_integer_column(0.0, 0.0..=max_q1000));
        if let Some(cols) = &mut q100_cols {
            cols.push(pb.add_integer_column(0.0, 0.0..=max_q100));
        }
        r1000_cols.push(pb.add_integer_column(0.0, 0.0..=(g1 - 1) as f64));
        if let Some(cols) = &mut r100_cols {
            cols.push(pb.add_integer_column(0.0, 0.0..=(g2 - 1) as f64));
        }
        col_count += if include_obj2 { 6 } else { 3 };
    }

    let mut s_cols = Vec::with_capacity(model.payers.len());
    for (payer, pay) in &model.payers {
        let upper = if model.settle_lookup.contains(payer) {
            0.0
        } else {
            *pay as f64
        };
        s_cols.push(pb.add_integer_column(0.0, 0.0..=upper));
        col_count += 1;
    }

    let mut t_cols = Vec::with_capacity(model.receivers.len());
    for (receiver, recv) in &model.receivers {
        let upper = if model.settle_lookup.contains(receiver) {
            0.0
        } else {
            *recv as f64
        };
        t_cols.push(pb.add_integer_column(0.0, 0.0..=upper));
        col_count += 1;
    }

    for (payer_idx, (_, pay)) in model.payers.iter().enumerate() {
        let mut factors = Vec::with_capacity(model.payer_edges[payer_idx].len() + 1);
        for &edge_idx in &model.payer_edges[payer_idx] {
            factors.push((x_cols[edge_idx], 1.0));
        }
        factors.push((s_cols[payer_idx], 1.0));
        let rhs = *pay as f64;
        pb.add_row(rhs..=rhs, factors);
        row_count += 1;
    }

    for (receiver_idx, (_, recv)) in model.receivers.iter().enumerate() {
        let mut factors = Vec::with_capacity(model.receiver_edges[receiver_idx].len() + 1);
        for &edge_idx in &model.receiver_edges[receiver_idx] {
            factors.push((x_cols[edge_idx], 1.0));
        }
        factors.push((t_cols[receiver_idx], 1.0));
        let rhs = *recv as f64;
        pb.add_row(rhs..=rhs, factors);
        row_count += 1;
    }

    for (edge_idx, edge) in model.edges.iter().enumerate() {
        let x = x_cols[edge_idx];
        let y = y_cols[edge_idx];
        pb.add_row(..=0.0, [(x, 1.0), (y, -(edge.upper_bound as f64))]);
        pb.add_row(0.0.., [(x, 1.0), (y, -1.0)]);
        row_count += 2;

        if let Some(cash_idx) = cash_edge_to_idx[edge_idx] {
            let z1 = z1_cols[cash_idx];
            let q1000 = q1000_cols[cash_idx];
            let r1000 = r1000_cols[cash_idx];

            pb.add_row(0.0..=0.0, [(x, 1.0), (q1000, -(g1 as f64)), (r1000, -1.0)]);
            pb.add_row(..=0.0, [(r1000, 1.0), (z1, -((g1 - 1) as f64))]);
            pb.add_row(0.0.., [(r1000, 1.0), (z1, -1.0)]);
            pb.add_row(..=0.0, [(z1, 1.0), (y, -1.0)]);
            row_count += 4;

            if include_obj2 {
                let z2 = z2_cols.as_ref().expect("z2 columns exist")[cash_idx];
                let q100 = q100_cols.as_ref().expect("q100 columns exist")[cash_idx];
                let r100 = r100_cols.as_ref().expect("r100 columns exist")[cash_idx];
                pb.add_row(
                    0.0..=0.0,
                    [(r1000, 1.0), (q100, -(g2 as f64)), (r100, -1.0)],
                );
                pb.add_row(..=0.0, [(r100, 1.0), (z2, -((g2 - 1) as f64))]);
                pb.add_row(0.0.., [(r100, 1.0), (z2, -1.0)]);
                pb.add_row(..=0.0, [(z2, 1.0), (z1, -1.0)]);
                pb.add_row(..=0.0, [(z2, 1.0), (y, -1.0)]);
                row_count += 5;
            }
        }
    }

    let mut highs_model = pb.optimise(Sense::Minimise);
    highs_model.make_quiet();
    apply_highs_options(&mut highs_model, &solve_options.highs);
    let solved = highs_model.solve();
    let status = solved.status();
    let accepted_feasible_on_limit = status != HighsModelStatus::Optimal
        && matches!(
            status,
            HighsModelStatus::ReachedTimeLimit
                | HighsModelStatus::ReachedIterationLimit
                | HighsModelStatus::ReachedSolutionLimit
                | HighsModelStatus::ReachedInterrupt
                | HighsModelStatus::ReachedMemoryLimit
        )
        && solve_options.highs.accept_feasible_on_limit
        && solved.primal_solution_status() == HighsSolutionStatus::Feasible;
    if status != HighsModelStatus::Optimal && !accepted_feasible_on_limit {
        return Err(classify_non_optimal_status(status));
    }

    let solution = solved.get_solution();
    let _ = (col_count, row_count);

    if accepted_feasible_on_limit {
        let obj1 = z1_cols
            .iter()
            .map(|col| i64::from(round_bankers(solution[*col]) > 0))
            .sum::<i64>();
        let obj2 = z2_cols
            .as_ref()
            .map(|cols| {
                cols.iter()
                    .map(|col| i64::from(round_bankers(solution[*col]) > 0))
                    .sum::<i64>()
            })
            .unwrap_or(0);
        let objw = model
            .edges
            .iter()
            .enumerate()
            .filter(|(_, edge)| edge.touches_non_settle)
            .map(|(idx, _)| i64::from(round_bankers(solution[y_cols[idx]]) > 0))
            .sum::<i64>();
        let obj3 = y_cols
            .iter()
            .map(|col| i64::from(round_bankers(solution[*col]) > 0))
            .sum::<i64>();
        tracing::info!(
            ?status,
            obj1,
            obj2,
            objw,
            obj3,
            mod_100_policy = ?solve_options.mod_100_policy,
            max_y = bounds.max_y,
            max_z1 = bounds.max_z1,
            max_z2 = bounds.max_z2,
            objective_total = bounds.total,
            "accepted feasible solution after solve limit"
        );
    }

    Ok(x_cols
        .iter()
        .map(|col| round_bankers(solution[*col]) as f64)
        .collect())
}

#[derive(Clone, Copy)]
struct Edge<MemberId> {
    from: MemberId,
    to: MemberId,
    upper_bound: i64,
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
    fn from_people_with_options(
        people: &[PersonBalance<MemberId>],
        settle_members: &[MemberId],
        cash_members: &[MemberId],
        options: TransferBuildOptions,
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

        #[derive(Clone, Copy)]
        struct CandidateEdge<MemberId> {
            edge: Edge<MemberId>,
            payer_idx: usize,
            receiver_idx: usize,
            payer_is_cash: bool,
        }

        let mut candidates = Vec::with_capacity(payers.len() * receivers.len());

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
                candidates.push(CandidateEdge {
                    edge: Edge {
                        from: *payer_member,
                        to: *receiver_member,
                        upper_bound,
                        touches_non_settle: !(payer_is_settle && receiver_is_settle),
                    },
                    payer_idx,
                    receiver_idx,
                    payer_is_cash,
                });
            }
        }

        if let Some(topk) = options.non_settle_topk
            && topk > 0
        {
            let mut keep = vec![false; candidates.len()];
            for (idx, candidate) in candidates.iter().enumerate() {
                if !candidate.edge.touches_non_settle {
                    keep[idx] = true;
                }
            }

            let mut payer_lists = vec![Vec::<(usize, i64, usize)>::new(); payers.len()];
            let mut receiver_lists = vec![Vec::<(usize, i64, usize)>::new(); receivers.len()];

            for (idx, candidate) in candidates.iter().enumerate() {
                if candidate.edge.touches_non_settle {
                    payer_lists[candidate.payer_idx].push((
                        idx,
                        candidate.edge.upper_bound,
                        candidate.receiver_idx,
                    ));
                    receiver_lists[candidate.receiver_idx].push((
                        idx,
                        candidate.edge.upper_bound,
                        candidate.payer_idx,
                    ));
                }
            }

            for list in &mut payer_lists {
                list.sort_unstable_by(|lhs, rhs| rhs.1.cmp(&lhs.1).then(lhs.2.cmp(&rhs.2)));
                for (idx, _, _) in list.iter().take(topk) {
                    keep[*idx] = true;
                }
            }
            for list in &mut receiver_lists {
                list.sort_unstable_by(|lhs, rhs| rhs.1.cmp(&lhs.1).then(lhs.2.cmp(&rhs.2)));
                for (idx, _, _) in list.iter().take(topk) {
                    keep[*idx] = true;
                }
            }

            candidates = candidates
                .into_iter()
                .enumerate()
                .filter_map(|(idx, candidate)| keep[idx].then_some(candidate))
                .collect();
        }

        let mut payer_edges: Vec<Vec<usize>> = vec![Vec::new(); payers.len()];
        let mut receiver_edges: Vec<Vec<usize>> = vec![Vec::new(); receivers.len()];
        let mut edges = Vec::with_capacity(candidates.len());
        let mut cash_edge_indices = Vec::new();

        for candidate in candidates {
            let edge_idx = edges.len();
            edges.push(candidate.edge);
            if candidate.payer_is_cash {
                cash_edge_indices.push(edge_idx);
            }
            payer_edges[candidate.payer_idx].push(edge_idx);
            receiver_edges[candidate.receiver_idx].push(edge_idx);
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

fn is_exact_integer_in_f64(value: i128) -> bool {
    value.unsigned_abs() <= MAX_SAFE_EXACT_INT_IN_F64 as u128
}

fn apply_highs_options(model: &mut Model, solve_options: &HighsCommonSolveOptions) {
    // highs::Model::set_option panics on HiGHS error status.
    // Keep option keys conservative and deterministic-oriented.
    model.set_option("presolve", "on");
    match solve_options.preset {
        HighsSolvePreset::Deterministic => {
            model.set_option("threads", 1);
            model.set_option("parallel", "off");
        }
        HighsSolvePreset::Fast => {
            if let Some(threads) = solve_options
                .threads
                .filter(|threads| *threads > 0)
                .map(|threads| threads.min(256))
            {
                model.set_option("threads", threads);
            }
            model.set_option("parallel", "on");
        }
    }
    if let Some(limit) = solve_options
        .time_limit_seconds
        .filter(|value| value.is_finite() && *value > 0.0)
        .map(|value| value.min(86_400.0))
    {
        model.set_option("time_limit", limit);
    }
    if let Some(gap) = solve_options
        .mip_rel_gap
        .filter(|value| value.is_finite() && *value >= 0.0)
        .map(|value| value.min(1.0))
    {
        model.set_option("mip_rel_gap", gap);
    }
    if let Some(gap) = solve_options
        .mip_abs_gap
        .filter(|value| value.is_finite() && *value >= 0.0)
        .map(|value| value.min(1_000_000_000.0))
    {
        model.set_option("mip_abs_gap", gap);
    }
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

#[cfg(test)]
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
