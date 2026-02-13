//! Settlement rounding logic for zero-sum minimal adjustment.
//!
//! This module implements the deferred rounding strategy described in the
//! Settlement Mathematics design document. It ensures that:
//! 1. Balances are rounded to the atomic unit (e.g., 1 yen for JPY)
//! 2. The sum of rounded balances equals zero (zero-sum constraint)
//! 3. Adjustments are assigned deterministically with one-step bounded impact

use crate::model::{MemberBalances, MemberId, Money};
use fxhash::FxHashSet;
use rust_decimal::{Decimal, RoundingStrategy, prelude::ToPrimitive};
use sha2::{Digest, Sha256};

/// Rounding mode for settlement quantization.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RoundingMode {
    /// Round half away from zero (e.g., 0.5 -> 1, -0.5 -> -1).
    /// This is the default mode for JPY.
    HalfUp,
    /// Round half to nearest even number (banker's rounding).
    /// Reduces bias in repeated rounding operations.
    HalfEven,
}

/// Policy for deterministic rounding-adjustment assignment.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FairnessPolicy {
    /// Minimize the sum of absolute differences between original and adjusted values.
    /// Adjustments are distributed to those who gained most from rounding.
    ZeroSumMinimalAdjustment,
}

/// Context for settlement quantization.
///
/// This struct configures how balances are rounded during settlement,
/// including the scale (decimal places), rounding mode, and fairness policy.
///
/// # Example
/// ```
/// use walicord_domain::services::{SettlementContext, RoundingMode, FairnessPolicy};
///
/// let ctx = SettlementContext {
///     scale: 0,  // JPY (no decimal places)
///     rounding_mode: RoundingMode::HalfUp,
///     fairness_policy: FairnessPolicy::ZeroSumMinimalAdjustment,
/// };
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SettlementContext {
    /// Number of decimal places for the atomic unit (e.g., 0 for JPY, 2 for USD).
    pub scale: u32,
    /// Rounding strategy to use.
    pub rounding_mode: RoundingMode,
    /// Policy for distributing adjustments to achieve zero-sum.
    pub fairness_policy: FairnessPolicy,
}

impl SettlementContext {
    /// Default context for JPY (scale=0, HalfUp rounding).
    pub fn jpy_default() -> Self {
        Self {
            scale: 0,
            rounding_mode: RoundingMode::HalfUp,
            fairness_policy: FairnessPolicy::ZeroSumMinimalAdjustment,
        }
    }

    /// Converts a money amount to integer atomic units under this context scale.
    pub fn to_atomic_units_i64(self, amount: Money) -> Option<i64> {
        let factor = Decimal::from_i128_with_scale(10_i128.checked_pow(self.scale)?, 0);
        let units = amount.as_decimal().checked_mul(factor)?;
        if units.fract() != Decimal::ZERO {
            return None;
        }
        units.to_i64()
    }
}

/// Errors that can occur during settlement quantization.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SettlementRoundingError {
    /// The total sum of balances exceeds the epsilon tolerance.
    /// This indicates data corruption or calculation errors.
    ImbalancedTotal(Money),
    /// V_int or adjustment_count could not be converted to a valid integer.
    InvalidAdjustmentCount,
    /// Not enough candidates available for adjustment distribution.
    ///
    /// Kept for backward compatibility with application-layer error mapping.
    InsufficientCandidates,
    /// Internal invariant violation: quantized balances failed to restore exact zero-sum.
    ZeroSumInvariantViolation,
    /// Quantized units could not be represented as integral values.
    NonIntegral,
    /// Settlement context scale is not supported by decimal precision constraints.
    UnsupportedScale { scale: u32, max_supported: u32 },
}

const STABLE_KEY_PROFILE_VERSION: &str = "stable_key_v1_sha256_member_context_be";
const STABLE_KEY_PROFILE_FIELD_SET: &str = "member_id,scale,rounding_mode,fairness_policy";
const EPSILON_OP_COUNT_BUDGET: i64 = 1_000_000;
const EPSILON_SAFETY_FACTOR: i64 = 100;
const MAX_SETTLEMENT_SCALE: u32 = 22;

/// Quantizes member balances to the atomic unit with zero-sum constraint.
///
/// This function implements the deferred rounding strategy:
/// 1. Validates that the sum of balances is within epsilon of zero
/// 2. Rounds each balance to the specified scale
/// 3. Computes V_int = Σ q_i and applies |V_int| unit adjustments to achieve zero-sum
///
/// The adjustment strategy prioritizes members who gained most from rounding
/// (i.e., those with the largest diff when V_int > 0, or
/// smallest diff when V_int < 0).
///
/// # Arguments
/// * `balances` - Map of member IDs to their current balances (Decimal precision)
/// * `context` - Settlement context specifying scale, rounding mode, and fairness policy
///
/// # Returns
/// * `Ok(MemberBalances)` - Quantized balances that sum to zero
/// * `Err(SettlementRoundingError)` - If validation fails or adjustment is impossible
///
/// # Example
/// ```
/// use walicord_domain::{MemberBalances, Money, model::MemberId};
/// use walicord_domain::services::{quantize_balances, SettlementContext};
///
/// let mut balances = MemberBalances::new();
/// // Balances must sum to approximately zero
/// balances.insert(MemberId(1), Money::new(667, 0));
/// balances.insert(MemberId(2), Money::new(-333, 0));
/// balances.insert(MemberId(3), Money::new(-334, 0));
///
/// let rounded = quantize_balances(&balances, SettlementContext::jpy_default()).unwrap();
/// // Sum of rounded balances will be exactly zero
/// ```
pub fn quantize_balances(
    balances: &MemberBalances,
    context: SettlementContext,
) -> Result<MemberBalances, SettlementRoundingError> {
    quantize_balances_with_preferred_members(balances, context, &[])
}

/// Quantizes member balances with optional preferred members for adjustment targeting.
///
/// Preferred members are ranked before non-preferred members during the adjustment phase.
/// This is used by partial settle-up flows to avoid assigning quantization deltas to
/// non-settling members unless necessary.
///
/// Adjustment candidate ranking is lexicographic:
/// 1. Preferred members first (when provided)
/// 2. Primary ordering objective (`diff` ordering by violation direction)
///    - `V_int > 0`: larger `diff` first (take back from those who gained most)
///    - `V_int < 0`: smaller `diff` first (give to those who lost most)
/// 3. Stable deterministic key (`stable_key`) for exact ties
pub fn quantize_balances_with_preferred_members(
    balances: &MemberBalances,
    context: SettlementContext,
    preferred_members: &[MemberId],
) -> Result<MemberBalances, SettlementRoundingError> {
    validate_scale(context.scale)?;
    match context.fairness_policy {
        FairnessPolicy::ZeroSumMinimalAdjustment => {}
    }
    let atomic_unit = Decimal::new(1, context.scale);
    let epsilon = settlement_epsilon(context.scale);
    let original_sum: Money = balances.values().sum();
    let sum_original = original_sum.as_decimal();
    if original_sum.abs().as_decimal() > epsilon {
        tracing::error!(
            reject_reason = "input_imbalance",
            member_count = balances.len(),
            atomic_unit = %atomic_unit,
            epsilon = %epsilon,
            rounding_mode = ?context.rounding_mode,
            sum_original = %sum_original,
            "Settlement quantization rejected due to input imbalance"
        );
        return Err(SettlementRoundingError::ImbalancedTotal(original_sum));
    }

    let rounding_strategy = match context.rounding_mode {
        RoundingMode::HalfUp => RoundingStrategy::MidpointAwayFromZero,
        RoundingMode::HalfEven => RoundingStrategy::MidpointNearestEven,
    };

    let preferred_lookup: FxHashSet<MemberId> = preferred_members.iter().copied().collect();

    tracing::debug!(
        stable_key_profile = STABLE_KEY_PROFILE_VERSION,
        stable_key_profile_fields = STABLE_KEY_PROFILE_FIELD_SET,
        atomic_unit = %atomic_unit,
        epsilon = %epsilon,
        expected_mode = ?context.rounding_mode,
        rounding_mode = ?context.rounding_mode,
        fairness_policy = ?context.fairness_policy,
        scale = context.scale,
        preferred_member_count = preferred_lookup.len(),
        member_count = balances.len(),
        sum_original = %sum_original,
        "Settlement quantization started"
    );

    let mut entries: Vec<(MemberId, Decimal, i128, Decimal, Decimal)> = balances
        .iter()
        .map(|(id, money)| {
            let original = money.as_decimal();
            let q_i = quantize_to_int(original, atomic_unit, rounding_strategy)?;
            let rounded = Decimal::from(q_i) * atomic_unit;
            let diff = rounded - original;
            Ok((*id, original, q_i, rounded, diff))
        })
        .collect::<Result<Vec<_>, SettlementRoundingError>>()?;

    let mut v_int = entries.iter().try_fold(0_i128, |acc, (_, _, q_i, _, _)| {
        acc.checked_add(*q_i)
            .ok_or(SettlementRoundingError::InvalidAdjustmentCount)
    })?;

    if v_int != 0 {
        let adjustment_count_i128 = v_int
            .checked_abs()
            .ok_or(SettlementRoundingError::InvalidAdjustmentCount)?;
        let adjustment_count = usize::try_from(adjustment_count_i128)
            .map_err(|_| SettlementRoundingError::InvalidAdjustmentCount)?;
        let sum_rounded = Decimal::from(v_int) * atomic_unit;

        let epsilon_units = (epsilon / atomic_unit)
            .ceil()
            .to_i128()
            .ok_or(SettlementRoundingError::InvalidAdjustmentCount)?;
        let (
            theoretical_upper_bound,
            operational_upper_bound,
            exceeds_theoretical,
            exceeds_operational,
        ) = bound_diagnostics(adjustment_count_i128, balances.len(), epsilon_units);

        if exceeds_theoretical {
            tracing::info!(
                adjustment_count = adjustment_count_i128,
                theoretical_upper_bound,
                member_count = balances.len(),
                sum_original = %sum_original,
                sum_rounded = %sum_rounded,
                epsilon_over_u = epsilon_units,
                "Nearest-rounding theoretical bound violated during settlement quantization"
            );
        }

        if exceeds_operational {
            tracing::warn!(
                adjustment_count = adjustment_count_i128,
                operational_upper_bound,
                epsilon_units,
                member_count = balances.len(),
                sum_original = %sum_original,
                sum_rounded = %sum_rounded,
                epsilon_over_u = epsilon_units,
                "Operational nearest-rounding bound exceeded during settlement quantization"
            );
        }

        // Defensive safety guard: if this trips, the quantization pipeline is inconsistent
        // (for example, corrupted intermediate values or integer-state anomalies).
        if adjustment_count > balances.len() {
            tracing::error!(
                reject_reason = "k_gt_n",
                failure_reason = "adjustment_count_exceeds_member_count",
                v_int,
                adjustment_count,
                member_count = balances.len(),
                atomic_unit = %atomic_unit,
                epsilon = %epsilon,
                rounding_mode = ?context.rounding_mode,
                sum_original = %sum_original,
                sum_rounded = %sum_rounded,
                epsilon_over_u = epsilon_units,
                "Adjustment count exceeds participant count during settlement quantization"
            );
            return Err(SettlementRoundingError::InvalidAdjustmentCount);
        }

        let score_sign = if v_int > 0 {
            Decimal::ONE
        } else {
            Decimal::NEGATIVE_ONE
        };

        let mut ranked_candidates: Vec<(usize, Decimal, Decimal, bool, [u8; 32], MemberId)> =
            entries
                .iter()
                .enumerate()
                .map(|(idx, (id, _, _, _, diff))| {
                    (
                        idx,
                        *diff,
                        *diff * score_sign,
                        preferred_lookup.contains(id),
                        stable_key(*id, context),
                        *id,
                    )
                })
                .collect();

        ranked_candidates.sort_by(
            |(_, _, score_a, preferred_a, stable_key_a, id_a),
             (_, _, score_b, preferred_b, stable_key_b, id_b)| {
                preferred_b
                    .cmp(preferred_a)
                    .then_with(|| {
                        score_b
                            .partial_cmp(score_a)
                            .unwrap_or(std::cmp::Ordering::Equal)
                    })
                    .then_with(|| stable_key_a.cmp(stable_key_b))
                    .then_with(|| id_a.cmp(id_b))
            },
        );

        let adjustment = if v_int > 0 { -atomic_unit } else { atomic_unit };
        let selected_indices: Vec<usize> = ranked_candidates
            .iter()
            .take(adjustment_count)
            .map(|(idx, _, _, _, _, _)| *idx)
            .collect();
        let selected_ids: Vec<u64> = selected_indices
            .iter()
            .map(|idx| entries[*idx].0.0)
            .collect();
        let selected_diffs: Vec<&Decimal> = ranked_candidates
            .iter()
            .take(adjustment_count)
            .map(|(_, diff, _, _, _, _)| diff)
            .collect();
        let preferred_hit_count = selected_indices
            .iter()
            .filter(|idx| preferred_lookup.contains(&entries[**idx].0))
            .count();
        let kth_score = ranked_candidates
            .get(adjustment_count.saturating_sub(1))
            .map(|(_, _, score, _, _, _)| score);
        let next_score = ranked_candidates
            .get(adjustment_count)
            .map(|(_, _, score, _, _, _)| score);

        tracing::debug!(
            v_int,
            adjustment_count,
            exceeds_theoretical,
            exceeds_operational,
            selected_ids = ?selected_ids,
            selected_diffs = ?selected_diffs,
            preferred_hit_count,
            member_count = balances.len(),
            sum_original = %sum_original,
            sum_rounded = %sum_rounded,
            epsilon_over_u = epsilon_units,
            kth_score = ?kth_score,
            next_score = ?next_score,
            "Settlement quantization diagnostics"
        );

        let opposite_sign_selected = selected_indices
            .iter()
            .filter(|idx| {
                let diff = entries[**idx].4;
                (v_int > 0 && diff < Decimal::ZERO) || (v_int < 0 && diff > Decimal::ZERO)
            })
            .count();

        for idx in &selected_indices {
            entries[*idx].3 += adjustment;
        }

        let max_final_error_units = selected_indices
            .iter()
            .filter_map(|idx| {
                let original = entries[*idx].1;
                let adjusted = entries[*idx].3;
                ((adjusted - original).abs() / atomic_unit).to_f64()
            })
            .fold(0.0_f64, f64::max);

        if opposite_sign_selected > 0 {
            tracing::warn!(
                opposite_sign_selected,
                adjustment_count,
                max_final_error_units,
                member_count = balances.len(),
                "Opposite-sign adjustment candidates were selected during zero-sum repair"
            );
        }

        v_int = entries
            .iter()
            .try_fold(0_i128, |acc, (_, _, _, rounded, _)| {
                let q_decimal = *rounded / atomic_unit;
                let q_i = q_decimal
                    .to_i128()
                    .ok_or(SettlementRoundingError::NonIntegral)?;
                if Decimal::from(q_i) != q_decimal {
                    tracing::error!(
                        reject_reason = "quantize_failure",
                        member_count = balances.len(),
                        atomic_unit = %atomic_unit,
                        epsilon = %epsilon,
                        rounding_mode = ?context.rounding_mode,
                        sum_original = %sum_original,
                        "Settlement quantization rejected due to non-integral repaired unit"
                    );
                    return Err(SettlementRoundingError::NonIntegral);
                }
                acc.checked_add(q_i)
                    .ok_or(SettlementRoundingError::InvalidAdjustmentCount)
            })?;
        if v_int != 0 {
            tracing::error!(
                reject_reason = "zero_sum_invariant_violation",
                member_count = balances.len(),
                atomic_unit = %atomic_unit,
                epsilon = %epsilon,
                rounding_mode = ?context.rounding_mode,
                sum_original = %sum_original,
                v_int,
                "Settlement quantization failed zero-sum invariant check"
            );
            return Err(SettlementRoundingError::ZeroSumInvariantViolation);
        }
    }

    let rounded_balances = entries
        .into_iter()
        .map(|(id, _, _, rounded, _)| (id, Money::from_decimal(rounded)))
        .collect();

    Ok(rounded_balances)
}

fn settlement_epsilon(scale: u32) -> Decimal {
    let baseline = Decimal::new(1, scale + 6);
    let epsilon_min = Decimal::from(EPSILON_SAFETY_FACTOR * EPSILON_OP_COUNT_BUDGET)
        * Decimal::from_i128_with_scale(1, 28);
    baseline.max(epsilon_min)
}

fn validate_scale(scale: u32) -> Result<(), SettlementRoundingError> {
    if scale <= MAX_SETTLEMENT_SCALE {
        return Ok(());
    }
    Err(SettlementRoundingError::UnsupportedScale {
        scale,
        max_supported: MAX_SETTLEMENT_SCALE,
    })
}

fn stable_key(member_id: MemberId, context: SettlementContext) -> [u8; 32] {
    let rounding_mode_tag = match context.rounding_mode {
        RoundingMode::HalfUp => 0_u8,
        RoundingMode::HalfEven => 1_u8,
    };
    let fairness_policy_tag = match context.fairness_policy {
        FairnessPolicy::ZeroSumMinimalAdjustment => 0_u8,
    };

    let mut framed = [0_u8; 15];
    framed[0] = 1_u8; // format version
    framed[1..9].copy_from_slice(&member_id.0.to_be_bytes());
    framed[9..13].copy_from_slice(&context.scale.to_be_bytes());
    framed[13] = rounding_mode_tag;
    framed[14] = fairness_policy_tag;

    let digest = Sha256::digest(framed);
    let mut out = [0_u8; 32];
    out.copy_from_slice(&digest);
    out
}

fn quantize_to_int(
    original: Decimal,
    atomic_unit: Decimal,
    rounding_strategy: RoundingStrategy,
) -> Result<i128, SettlementRoundingError> {
    let q_decimal = (original / atomic_unit).round_dp_with_strategy(0, rounding_strategy);
    let Some(q_i) = q_decimal.to_i128() else {
        tracing::warn!(
            reject_reason = "quantize_failure",
            original = %original,
            atomic_unit = %atomic_unit,
            rounded_units = %q_decimal,
            "Quantization unit conversion failed"
        );
        return Err(SettlementRoundingError::NonIntegral);
    };
    if Decimal::from(q_i) != q_decimal {
        tracing::warn!(
            reject_reason = "quantize_failure",
            original = %original,
            atomic_unit = %atomic_unit,
            rounded_units = %q_decimal,
            "Quantization produced non-integer unit value"
        );
        return Err(SettlementRoundingError::NonIntegral);
    }
    Ok(q_i)
}

fn bound_diagnostics(
    adjustment_count: i128,
    member_count: usize,
    epsilon_units: i128,
) -> (i128, i128, bool, bool) {
    let theoretical_upper_bound = (member_count / 2) as i128;
    let operational_upper_bound = theoretical_upper_bound + epsilon_units;
    (
        theoretical_upper_bound,
        operational_upper_bound,
        adjustment_count > theoretical_upper_bound,
        adjustment_count > operational_upper_bound,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn settlement_context_converts_to_atomic_units() {
        let jpy = SettlementContext::jpy_default();
        assert_eq!(jpy.to_atomic_units_i64(Money::from_i64(42)), Some(42));

        let usd = SettlementContext {
            scale: 2,
            ..SettlementContext::jpy_default()
        };
        assert_eq!(usd.to_atomic_units_i64(Money::new(123, 2)), Some(123));
        assert_eq!(usd.to_atomic_units_i64(Money::new(1234, 3)), None);
    }

    #[test]
    fn rejects_unsupported_scale() {
        let mut balances = MemberBalances::default();
        balances.insert(MemberId(1), Money::ZERO);
        let context = SettlementContext {
            scale: MAX_SETTLEMENT_SCALE + 1,
            ..SettlementContext::jpy_default()
        };

        assert_eq!(
            quantize_balances(&balances, context),
            Err(SettlementRoundingError::UnsupportedScale {
                scale: MAX_SETTLEMENT_SCALE + 1,
                max_supported: MAX_SETTLEMENT_SCALE,
            })
        );
    }

    fn dec(value: &str) -> Decimal {
        Decimal::from_str(value).expect("valid decimal")
    }

    fn strategy(mode: RoundingMode) -> RoundingStrategy {
        match mode {
            RoundingMode::HalfUp => RoundingStrategy::MidpointAwayFromZero,
            RoundingMode::HalfEven => RoundingStrategy::MidpointNearestEven,
        }
    }

    fn quantize_to_int_scale_shift(
        original: Decimal,
        scale: u32,
        rounding_strategy: RoundingStrategy,
    ) -> i128 {
        let factor = Decimal::new(1, 0)
            .checked_mul(Decimal::from_i128_with_scale(10_i128.pow(scale), 0))
            .expect("scale factor should be representable");
        let shifted = original
            .checked_mul(factor)
            .expect("shifted value should be representable in test envelope");
        shifted
            .round_dp_with_strategy(0, rounding_strategy)
            .to_i128()
            .expect("shifted quantized value should fit i128")
    }

    #[test]
    fn adjusts_negative_error_with_stable_tie_break() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.4"))),
            (MemberId(2), Money::from_decimal(dec("0.4"))),
            (MemberId(3), Money::from_decimal(dec("-0.8"))),
        ]);

        let rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("quantize should succeed");

        let m1_key = stable_key(MemberId(1), SettlementContext::jpy_default());
        let m2_key = stable_key(MemberId(2), SettlementContext::jpy_default());
        let selected = if m1_key <= m2_key {
            MemberId(1)
        } else {
            MemberId(2)
        };
        let other = if selected == MemberId(1) {
            MemberId(2)
        } else {
            MemberId(1)
        };

        assert_eq!(rounded.get(&selected).copied(), Some(Money::from_i64(1)));
        assert_eq!(rounded.get(&other).copied(), Some(Money::from_i64(0)));
        assert_eq!(
            rounded.get(&MemberId(3)).copied(),
            Some(Money::from_i64(-1))
        );
    }

    #[test]
    fn adjusts_positive_error_with_stable_tie_break() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("-0.4"))),
            (MemberId(2), Money::from_decimal(dec("-0.4"))),
            (MemberId(3), Money::from_decimal(dec("0.8"))),
        ]);

        let rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("quantize should succeed");

        let m1_key = stable_key(MemberId(1), SettlementContext::jpy_default());
        let m2_key = stable_key(MemberId(2), SettlementContext::jpy_default());
        let selected = if m1_key <= m2_key {
            MemberId(1)
        } else {
            MemberId(2)
        };
        let other = if selected == MemberId(1) {
            MemberId(2)
        } else {
            MemberId(1)
        };

        assert_eq!(rounded.get(&selected).copied(), Some(Money::from_i64(-1)));
        assert_eq!(rounded.get(&other).copied(), Some(Money::from_i64(0)));
        assert_eq!(rounded.get(&MemberId(3)).copied(), Some(Money::from_i64(1)));
    }

    #[test]
    fn rejects_imbalanced_total() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.1"))),
            (MemberId(2), Money::from_decimal(dec("0.0"))),
        ]);

        let result = quantize_balances(&balances, SettlementContext::jpy_default());
        assert!(matches!(
            result,
            Err(SettlementRoundingError::ImbalancedTotal(_))
        ));
    }

    #[test]
    fn repeated_split_accumulates_within_epsilon() {
        // After 10,000 splits of 100 by 3, drift must stay within epsilon
        let original_amount = Money::from_i64(100);
        let n = 3_usize;
        let iterations = 10_000;
        let epsilon = Decimal::new(1, 6);

        let mut total_deviation = Decimal::ZERO;
        for _ in 0..iterations {
            let shares = original_amount.split_even(n, crate::model::RemainderPolicy::FrontLoad);
            let sum: Money = shares.sum();
            let deviation = (sum.as_decimal() - original_amount.as_decimal()).abs();
            total_deviation += deviation;
        }

        assert!(
            total_deviation <= epsilon,
            "total drift after {iterations} splits must be within epsilon (drift: {total_deviation})"
        );
    }

    #[test]
    fn sign_inversion_preserves_adjustment_ordering() {
        // Flipping all balances preserves the adjustment selection ordering
        // Both cases have MemberId(1) with the largest magnitude diff (gained most from rounding)
        // and should be selected for adjustment first
        let balances_positive = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("10.6"))),
            (MemberId(2), Money::from_decimal(dec("-5.3"))),
            (MemberId(3), Money::from_decimal(dec("-5.3"))),
        ]);

        let balances_negative = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("-10.6"))),
            (MemberId(2), Money::from_decimal(dec("5.3"))),
            (MemberId(3), Money::from_decimal(dec("5.3"))),
        ]);

        let rounded_positive =
            quantize_balances(&balances_positive, SettlementContext::jpy_default())
                .expect("quantize should succeed");
        let rounded_negative =
            quantize_balances(&balances_negative, SettlementContext::jpy_default())
                .expect("quantize should succeed");

        // Both should sum to zero after adjustment
        let sum_positive: Money = rounded_positive.values().sum();
        let sum_negative: Money = rounded_negative.values().sum();
        assert!(sum_positive.is_zero());
        assert!(sum_negative.is_zero());

        // Member 1 has the largest |diff| in both cases and gets adjusted
        // Positive: 10.6 rounds to 11 (diff +0.4), V_int = +1, MemberId(1) adjusted to 10
        // Negative: -10.6 rounds to -11 (diff -0.4), V_int = -1, MemberId(1) adjusted to -10
        let m1_positive = rounded_positive
            .get(&MemberId(1))
            .copied()
            .unwrap()
            .as_decimal();
        let m1_negative = rounded_negative
            .get(&MemberId(1))
            .copied()
            .unwrap()
            .as_decimal();
        assert_eq!(m1_positive, dec("10"));
        assert_eq!(m1_negative, dec("-10"));
    }

    #[test]
    fn zero_adjustment_when_all_diffs_are_zero() {
        // When all diffs are zero, no adjustment is applied
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("10"))),
            (MemberId(2), Money::from_decimal(dec("-5"))),
            (MemberId(3), Money::from_decimal(dec("-5"))),
        ]);

        let rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("quantize should succeed");

        // Values should remain unchanged (already integers)
        assert_eq!(
            rounded.get(&MemberId(1)).copied(),
            Some(Money::from_i64(10))
        );
        assert_eq!(
            rounded.get(&MemberId(2)).copied(),
            Some(Money::from_i64(-5))
        );
        assert_eq!(
            rounded.get(&MemberId(3)).copied(),
            Some(Money::from_i64(-5))
        );

        // Sum should be zero
        let sum: Money = rounded.values().sum();
        assert!(sum.is_zero());
    }

    #[test]
    fn gain_from_rounding_is_minimized() {
        // "Who gained" case: those who gained from rounding are prioritized for adjustment
        // Original: A +10.6, B -10.6
        // Rounded: A +11 (gained +0.4), B -11 (gained -0.4 in absolute terms)
        // V_int = 0, so no adjustment needed in this symmetric case
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("10.6"))),
            (MemberId(2), Money::from_decimal(dec("-10.6"))),
        ]);

        let rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("quantize should succeed");

        // Both round to 11 and -11, sum is 0, no adjustment needed
        assert_eq!(
            rounded.get(&MemberId(1)).copied(),
            Some(Money::from_i64(11))
        );
        assert_eq!(
            rounded.get(&MemberId(2)).copied(),
            Some(Money::from_i64(-11))
        );

        let sum: Money = rounded.values().sum();
        assert!(sum.is_zero());
    }

    #[test]
    fn unambiguous_gain_case_adjusts_correctly() {
        // Case where "who gained" is unambiguous:
        // A: +10.4 (rounds to 10, loses 0.4)
        // B: +0.6 (rounds to 1, gains 0.4)
        // C: -11.0 (rounds to -11, no change)
        // Rounded sum: 10 + 1 - 11 = 0, so no adjustment needed
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("10.4"))),
            (MemberId(2), Money::from_decimal(dec("0.6"))),
            (MemberId(3), Money::from_decimal(dec("-11.0"))),
        ]);

        let rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("quantize should succeed");

        assert_eq!(
            rounded.get(&MemberId(1)).copied(),
            Some(Money::from_i64(10))
        );
        assert_eq!(rounded.get(&MemberId(2)).copied(), Some(Money::from_i64(1)));
        assert_eq!(
            rounded.get(&MemberId(3)).copied(),
            Some(Money::from_i64(-11))
        );

        let sum: Money = rounded.values().sum();
        assert!(sum.is_zero());
    }

    #[test]
    fn half_up_and_half_even_produce_different_quantization() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.5"))),
            (MemberId(2), Money::from_decimal(dec("-0.5"))),
        ]);

        let half_up = quantize_balances(
            &balances,
            SettlementContext {
                scale: 0,
                rounding_mode: RoundingMode::HalfUp,
                fairness_policy: FairnessPolicy::ZeroSumMinimalAdjustment,
            },
        )
        .expect("half up should succeed");

        let half_even = quantize_balances(
            &balances,
            SettlementContext {
                scale: 0,
                rounding_mode: RoundingMode::HalfEven,
                fairness_policy: FairnessPolicy::ZeroSumMinimalAdjustment,
            },
        )
        .expect("half even should succeed");

        assert_eq!(half_up.get(&MemberId(1)).copied(), Some(Money::from_i64(1)));
        assert_eq!(
            half_up.get(&MemberId(2)).copied(),
            Some(Money::from_i64(-1))
        );

        assert_eq!(
            half_even.get(&MemberId(1)).copied(),
            Some(Money::from_i64(0))
        );
        assert_eq!(
            half_even.get(&MemberId(2)).copied(),
            Some(Money::from_i64(0))
        );
    }

    #[test]
    fn single_member_near_zero_quantizes_to_zero() {
        let balances =
            MemberBalances::from_iter([(MemberId(1), Money::from_decimal(dec("0.0000004")))]);

        let rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("single-member near-zero balance should be accepted");

        assert_eq!(rounded.get(&MemberId(1)).copied(), Some(Money::from_i64(0)));
    }

    #[test]
    fn single_member_outside_epsilon_is_rejected() {
        let balances =
            MemberBalances::from_iter([(MemberId(1), Money::from_decimal(dec("0.001")))]);

        let result = quantize_balances(&balances, SettlementContext::jpy_default());
        assert!(matches!(
            result,
            Err(SettlementRoundingError::ImbalancedTotal(_))
        ));
    }

    #[test]
    fn two_member_boundary_crosses_from_vint_zero_to_one() {
        let zero_case = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.4999994"))),
            (MemberId(2), Money::from_decimal(dec("-0.4999994"))),
        ]);
        let one_case = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.5000004"))),
            (MemberId(2), Money::from_decimal(dec("-0.4999997"))),
        ]);

        let rounded_zero = quantize_balances(&zero_case, SettlementContext::jpy_default())
            .expect("V_int=0 boundary case should succeed");
        let rounded_one = quantize_balances(&one_case, SettlementContext::jpy_default())
            .expect("V_int=1 boundary case should succeed");

        assert_eq!(
            rounded_zero.get(&MemberId(1)).copied(),
            Some(Money::from_i64(0))
        );
        assert_eq!(
            rounded_zero.get(&MemberId(2)).copied(),
            Some(Money::from_i64(0))
        );

        assert_eq!(
            rounded_one.get(&MemberId(1)).copied(),
            Some(Money::from_i64(1))
        );
        assert_eq!(
            rounded_one.get(&MemberId(2)).copied(),
            Some(Money::from_i64(-1))
        );

        let sum_zero: Money = rounded_zero.values().sum();
        let sum_one: Money = rounded_one.values().sum();
        assert!(sum_zero.is_zero());
        assert!(sum_one.is_zero());
    }

    #[test]
    fn two_member_vint_negative_one_repairs_to_zero_sum() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("-0.5000004"))),
            (MemberId(2), Money::from_decimal(dec("0.4999997"))),
        ]);

        let rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("V_int=-1 boundary case should succeed");

        assert_eq!(
            rounded.get(&MemberId(1)).copied(),
            Some(Money::from_i64(-1))
        );
        assert_eq!(rounded.get(&MemberId(2)).copied(), Some(Money::from_i64(1)));

        let sum: Money = rounded.values().sum();
        assert!(sum.is_zero());
    }

    #[test]
    fn large_member_set_remains_zero_sum() {
        let mut balances = MemberBalances::new();
        for i in 1..=60 {
            balances.insert(MemberId(i), Money::from_decimal(dec("0.49")));
        }
        for i in 61..=120 {
            balances.insert(MemberId(i), Money::from_decimal(dec("-0.49")));
        }

        let rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("quantize should succeed for large member set");

        let sum: Money = rounded.values().sum();
        assert!(sum.is_zero());
    }

    #[test]
    fn preferred_members_are_ranked_first_for_adjustment() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.8"))),
            (MemberId(2), Money::from_decimal(dec("0.6"))),
            (MemberId(3), Money::from_decimal(dec("-1.4"))),
        ]);

        let default_rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("default quantization should succeed");
        assert_eq!(
            default_rounded.get(&MemberId(1)).copied(),
            Some(Money::from_i64(1))
        );
        let m2_key = stable_key(MemberId(2), SettlementContext::jpy_default());
        let m3_key = stable_key(MemberId(3), SettlementContext::jpy_default());
        let adjusted_member = if m2_key <= m3_key {
            MemberId(2)
        } else {
            MemberId(3)
        };
        if adjusted_member == MemberId(2) {
            assert_eq!(
                default_rounded.get(&MemberId(2)).copied(),
                Some(Money::from_i64(0))
            );
            assert_eq!(
                default_rounded.get(&MemberId(3)).copied(),
                Some(Money::from_i64(-1))
            );
        } else {
            assert_eq!(
                default_rounded.get(&MemberId(2)).copied(),
                Some(Money::from_i64(1))
            );
            assert_eq!(
                default_rounded.get(&MemberId(3)).copied(),
                Some(Money::from_i64(-2))
            );
        }

        let preferred_rounded = quantize_balances_with_preferred_members(
            &balances,
            SettlementContext::jpy_default(),
            &[MemberId(1)],
        )
        .expect("preferred-member quantization should succeed");
        assert_eq!(
            preferred_rounded.get(&MemberId(1)).copied(),
            Some(Money::from_i64(0))
        );
        assert_eq!(
            preferred_rounded.get(&MemberId(2)).copied(),
            Some(Money::from_i64(1))
        );
        assert_eq!(
            preferred_rounded.get(&MemberId(3)).copied(),
            Some(Money::from_i64(-1))
        );
    }

    #[test]
    fn preferred_priority_can_override_local_diff_order() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.4"))),
            (MemberId(2), Money::from_decimal(dec("0.6"))),
            (MemberId(3), Money::from_decimal(dec("0.6"))),
            (MemberId(4), Money::from_decimal(dec("-1.4"))),
            (MemberId(5), Money::from_decimal(dec("-0.2"))),
        ]);

        let default_rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("default quantization should succeed");
        assert_eq!(
            default_rounded.get(&MemberId(1)).copied(),
            Some(Money::from_i64(0))
        );

        let preferred_rounded = quantize_balances_with_preferred_members(
            &balances,
            SettlementContext::jpy_default(),
            &[MemberId(1)],
        )
        .expect("preferred quantization should succeed");

        assert_eq!(
            preferred_rounded.get(&MemberId(1)).copied(),
            Some(Money::from_i64(-1))
        );

        let sum_default: Money = default_rounded.values().sum();
        let sum_preferred: Money = preferred_rounded.values().sum();
        assert!(sum_default.is_zero());
        assert!(sum_preferred.is_zero());
    }

    #[test]
    fn equal_diff_mass_tie_is_deterministic_and_preferred_changes_selection() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.6"))),
            (MemberId(2), Money::from_decimal(dec("0.6"))),
            (MemberId(3), Money::from_decimal(dec("-0.4"))),
            (MemberId(4), Money::from_decimal(dec("-0.4"))),
            (MemberId(5), Money::from_decimal(dec("-0.4"))),
        ]);

        let first = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("default quantization should succeed");
        let second = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("default quantization should be deterministic");
        assert_eq!(first, second);

        let preferred = quantize_balances_with_preferred_members(
            &balances,
            SettlementContext::jpy_default(),
            &[MemberId(4), MemberId(5)],
        )
        .expect("preferred quantization should succeed");

        assert_eq!(
            preferred.get(&MemberId(1)).copied(),
            Some(Money::from_i64(1))
        );
        assert_eq!(
            preferred.get(&MemberId(2)).copied(),
            Some(Money::from_i64(1))
        );
        assert_eq!(
            preferred.get(&MemberId(3)).copied(),
            Some(Money::from_i64(0))
        );
        assert_eq!(
            preferred.get(&MemberId(4)).copied(),
            Some(Money::from_i64(-1))
        );
        assert_eq!(
            preferred.get(&MemberId(5)).copied(),
            Some(Money::from_i64(-1))
        );
    }

    #[test]
    fn preferred_tie_with_equal_score_uses_stable_key() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.6"))),
            (MemberId(2), Money::from_decimal(dec("0.6"))),
            (MemberId(3), Money::from_decimal(dec("-1.2"))),
        ]);

        let rounded = quantize_balances_with_preferred_members(
            &balances,
            SettlementContext::jpy_default(),
            &[MemberId(1), MemberId(2)],
        )
        .expect("preferred tie quantization should succeed");

        let m1_key = stable_key(MemberId(1), SettlementContext::jpy_default());
        let m2_key = stable_key(MemberId(2), SettlementContext::jpy_default());
        let selected = if m1_key <= m2_key {
            MemberId(1)
        } else {
            MemberId(2)
        };
        let other = if selected == MemberId(1) {
            MemberId(2)
        } else {
            MemberId(1)
        };

        assert_eq!(rounded.get(&selected).copied(), Some(Money::from_i64(0)));
        assert_eq!(rounded.get(&other).copied(), Some(Money::from_i64(1)));
        assert_eq!(
            rounded.get(&MemberId(3)).copied(),
            Some(Money::from_i64(-1))
        );
    }

    #[test]
    fn decimal_representation_scale_does_not_change_quantization_result() {
        let balances_a = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.6"))),
            (MemberId(2), Money::from_decimal(dec("0.60"))),
            (MemberId(3), Money::from_decimal(dec("-1.2"))),
        ]);
        let balances_b = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.60"))),
            (MemberId(2), Money::from_decimal(dec("0.6"))),
            (MemberId(3), Money::from_decimal(dec("-1.20"))),
        ]);

        let rounded_a = quantize_balances(&balances_a, SettlementContext::jpy_default())
            .expect("quantization should succeed for mixed decimal scales");
        let rounded_b = quantize_balances(&balances_b, SettlementContext::jpy_default())
            .expect("quantization should succeed for equivalent decimal values");

        assert_eq!(rounded_a, rounded_b);
    }

    #[test]
    fn large_magnitude_scale8_values_quantize_without_overflow() {
        let context = SettlementContext {
            scale: 8,
            rounding_mode: RoundingMode::HalfUp,
            fairness_policy: FairnessPolicy::ZeroSumMinimalAdjustment,
        };
        let balances = MemberBalances::from_iter([
            (
                MemberId(1),
                Money::from_decimal(dec("12345678901234567890.12345678")),
            ),
            (
                MemberId(2),
                Money::from_decimal(dec("-12345678901234567890.12345678")),
            ),
        ]);

        let rounded = quantize_balances(&balances, context)
            .expect("scale=8 large magnitude quantization should succeed");
        let sum: Money = rounded.values().sum();
        assert!(sum.is_zero());
    }

    #[test]
    fn quantize_to_int_matches_scale_shift_within_supported_envelope() {
        let cases = [
            (dec("0.50000000"), 8_u32, RoundingMode::HalfUp),
            (dec("-0.50000000"), 8_u32, RoundingMode::HalfUp),
            (dec("123456789.12345678"), 8_u32, RoundingMode::HalfEven),
            (dec("-987654321.87654321"), 8_u32, RoundingMode::HalfEven),
            (dec("42.5"), 0_u32, RoundingMode::HalfUp),
            (dec("42.5"), 0_u32, RoundingMode::HalfEven),
        ];

        for (original, scale, mode) in cases {
            let atomic_unit = Decimal::new(1, scale);
            let rounding = strategy(mode);
            let by_division = quantize_to_int(original, atomic_unit, rounding)
                .expect("division path should work");
            let by_scale_shift = quantize_to_int_scale_shift(original, scale, rounding);
            assert_eq!(by_division, by_scale_shift);
        }
    }

    #[test]
    fn quantize_to_int_remainder_stays_within_nearest_bound() {
        let cases = [
            (dec("0.50000000"), 8_u32, RoundingMode::HalfUp),
            (dec("-0.50000000"), 8_u32, RoundingMode::HalfUp),
            (dec("0.50000000"), 8_u32, RoundingMode::HalfEven),
            (dec("-0.50000000"), 8_u32, RoundingMode::HalfEven),
            (dec("1234.56789012"), 8_u32, RoundingMode::HalfEven),
            (dec("-1234.56789012"), 8_u32, RoundingMode::HalfEven),
        ];

        for (original, scale, mode) in cases {
            let atomic_unit = Decimal::new(1, scale);
            let q_i = quantize_to_int(original, atomic_unit, strategy(mode))
                .expect("quantization should succeed");
            let rounded = Decimal::from(q_i) * atomic_unit;
            let remainder = original - rounded;
            let half_u = atomic_unit / Decimal::new(2, 0);
            assert!(
                remainder.abs() <= half_u,
                "remainder must be within nearest bound (remainder={remainder}, half_u={half_u})"
            );
        }
    }

    #[test]
    fn preferred_set_variants_work_for_positive_and_negative_vint() {
        let positive_vint = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.6"))),
            (MemberId(2), Money::from_decimal(dec("0.6"))),
            (MemberId(3), Money::from_decimal(dec("-1.2"))),
        ]);
        let negative_vint = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("-0.6"))),
            (MemberId(2), Money::from_decimal(dec("-0.6"))),
            (MemberId(3), Money::from_decimal(dec("1.2"))),
        ]);

        for balances in [&positive_vint, &negative_vint] {
            let default_rounded = quantize_balances(balances, SettlementContext::jpy_default())
                .expect("default quantization should succeed");
            let all_preferred = quantize_balances_with_preferred_members(
                balances,
                SettlementContext::jpy_default(),
                &[MemberId(1), MemberId(2), MemberId(3)],
            )
            .expect("all-preferred quantization should succeed");
            let partial_preferred = quantize_balances_with_preferred_members(
                balances,
                SettlementContext::jpy_default(),
                &[MemberId(1)],
            )
            .expect("partial-preferred quantization should succeed");

            let sum_default: Money = default_rounded.values().sum();
            let sum_all: Money = all_preferred.values().sum();
            let sum_partial: Money = partial_preferred.values().sum();
            assert!(sum_default.is_zero());
            assert!(sum_all.is_zero());
            assert!(sum_partial.is_zero());

            assert_eq!(default_rounded, all_preferred);
        }
    }

    #[test]
    fn bound_diagnostics_distinguishes_theoretical_and_operational_edges() {
        let (_, _, n2_theoretical_ok, n2_operational_ok) = bound_diagnostics(1, 2, 0);
        assert!(!n2_theoretical_ok);
        assert!(!n2_operational_ok);

        let (_, _, n2_theoretical_over, n2_operational_over) = bound_diagnostics(2, 2, 0);
        assert!(n2_theoretical_over);
        assert!(n2_operational_over);

        let (_, _, theo_floor, op_floor) = bound_diagnostics(2, 5, 1);
        assert!(!theo_floor);
        assert!(!op_floor);

        let (_, _, theo_plus_one, op_plus_one) = bound_diagnostics(3, 5, 1);
        assert!(theo_plus_one);
        assert!(!op_plus_one);

        let (_, _, theo_both, op_both) = bound_diagnostics(4, 5, 1);
        assert!(theo_both);
        assert!(op_both);
    }

    #[test]
    fn epsilon_signed_boundary_cases_are_accepted() {
        let plus_epsilon = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.5000005"))),
            (MemberId(2), Money::from_decimal(dec("-0.4999995"))),
        ]);
        let minus_epsilon = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.4999995"))),
            (MemberId(2), Money::from_decimal(dec("-0.5000005"))),
        ]);

        let rounded_plus = quantize_balances(&plus_epsilon, SettlementContext::jpy_default())
            .expect("+epsilon boundary should be accepted");
        let rounded_minus = quantize_balances(&minus_epsilon, SettlementContext::jpy_default())
            .expect("-epsilon boundary should be accepted");

        let sum_plus: Money = rounded_plus.values().sum();
        let sum_minus: Money = rounded_minus.values().sum();
        assert!(sum_plus.is_zero());
        assert!(sum_minus.is_zero());
    }

    #[test]
    fn vint_zero_with_small_original_drift_keeps_rounded_zero_sum() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("1.4"))),
            (MemberId(2), Money::from_decimal(dec("-1.3999995"))),
        ]);

        let rounded = quantize_balances(&balances, SettlementContext::jpy_default())
            .expect("epsilon-drift input should be accepted");

        let sum_original: Money = balances.values().sum();
        assert!(sum_original.abs().as_decimal() <= Decimal::new(1, 6));

        let sum_rounded: Money = rounded.values().sum();
        assert!(sum_rounded.is_zero());
        assert_eq!(rounded.get(&MemberId(1)).copied(), Some(Money::from_i64(1)));
        assert_eq!(
            rounded.get(&MemberId(2)).copied(),
            Some(Money::from_i64(-1))
        );
    }

    #[test]
    fn preferred_reordering_preserves_l1_minimal_intervention() {
        let context = SettlementContext::jpy_default();
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("0.4"))),
            (MemberId(2), Money::from_decimal(dec("0.6"))),
            (MemberId(3), Money::from_decimal(dec("0.6"))),
            (MemberId(4), Money::from_decimal(dec("-1.4"))),
            (MemberId(5), Money::from_decimal(dec("-0.2"))),
        ]);

        let rounding_strategy = match context.rounding_mode {
            RoundingMode::HalfUp => RoundingStrategy::MidpointAwayFromZero,
            RoundingMode::HalfEven => RoundingStrategy::MidpointNearestEven,
        };
        let atomic_unit = Decimal::new(1, context.scale);

        let mut rounded_before_repair = MemberBalances::new();
        let mut v_int = 0_i128;
        for (id, money) in &balances {
            let q_i = quantize_to_int(money.as_decimal(), atomic_unit, rounding_strategy)
                .expect("pre-repair quantization should succeed");
            v_int += q_i;
            rounded_before_repair
                .insert(*id, Money::from_decimal(Decimal::from(q_i) * atomic_unit));
        }
        let expected_l1 = Decimal::from(v_int.abs()) * atomic_unit;

        let default_rounded = quantize_balances(&balances, context).expect("default quantization");
        let preferred_rounded =
            quantize_balances_with_preferred_members(&balances, context, &[MemberId(1)])
                .expect("preferred quantization");

        let l1_default = balances
            .keys()
            .map(|id| {
                let adjusted = default_rounded
                    .get(id)
                    .expect("adjusted member exists")
                    .as_decimal();
                let rounded = rounded_before_repair
                    .get(id)
                    .expect("pre-repair member exists")
                    .as_decimal();
                (adjusted - rounded).abs()
            })
            .fold(Decimal::ZERO, |acc, v| acc + v);
        let l1_preferred = balances
            .keys()
            .map(|id| {
                let adjusted = preferred_rounded
                    .get(id)
                    .expect("adjusted member exists")
                    .as_decimal();
                let rounded = rounded_before_repair
                    .get(id)
                    .expect("pre-repair member exists")
                    .as_decimal();
                (adjusted - rounded).abs()
            })
            .fold(Decimal::ZERO, |acc, v| acc + v);

        assert_eq!(l1_default, expected_l1);
        assert_eq!(l1_preferred, expected_l1);
    }

    #[test]
    fn negative_midpoints_follow_mode_and_diff_sign_interpretation() {
        let balances = MemberBalances::from_iter([
            (MemberId(1), Money::from_decimal(dec("-2.5"))),
            (MemberId(2), Money::from_decimal(dec("2.5"))),
        ]);

        let half_up = quantize_balances(
            &balances,
            SettlementContext {
                scale: 0,
                rounding_mode: RoundingMode::HalfUp,
                fairness_policy: FairnessPolicy::ZeroSumMinimalAdjustment,
            },
        )
        .expect("half up should succeed");
        let half_even = quantize_balances(
            &balances,
            SettlementContext {
                scale: 0,
                rounding_mode: RoundingMode::HalfEven,
                fairness_policy: FairnessPolicy::ZeroSumMinimalAdjustment,
            },
        )
        .expect("half even should succeed");

        assert_eq!(
            half_up.get(&MemberId(1)).copied(),
            Some(Money::from_i64(-3))
        );
        assert_eq!(
            half_even.get(&MemberId(1)).copied(),
            Some(Money::from_i64(-2))
        );

        let original_debt = dec("-2.5");
        let half_up_diff = half_up
            .get(&MemberId(1))
            .expect("member present")
            .as_decimal()
            - original_debt;
        let half_even_diff = half_even
            .get(&MemberId(1))
            .expect("member present")
            .as_decimal()
            - original_debt;

        assert!(half_up_diff < Decimal::ZERO);
        assert!(half_even_diff > Decimal::ZERO);
    }

    #[test]
    fn stable_key_changes_across_settlement_contexts() {
        let member = MemberId(42);
        let jpy_half_up = SettlementContext::jpy_default();
        let usd_half_up = SettlementContext {
            scale: 2,
            rounding_mode: RoundingMode::HalfUp,
            fairness_policy: FairnessPolicy::ZeroSumMinimalAdjustment,
        };
        let jpy_half_even = SettlementContext {
            scale: 0,
            rounding_mode: RoundingMode::HalfEven,
            fairness_policy: FairnessPolicy::ZeroSumMinimalAdjustment,
        };

        assert_ne!(
            stable_key(member, jpy_half_up),
            stable_key(member, usd_half_up)
        );
        assert_ne!(
            stable_key(member, jpy_half_up),
            stable_key(member, jpy_half_even)
        );
    }
}
