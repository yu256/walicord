use fxhash::{FxHashMap, FxHashSet};
use rust_decimal::{Decimal, prelude::ToPrimitive};
use smallvec::SmallVec;
use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt,
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Neg, Not,
        Shl, Sub, SubAssign,
    },
    sync::Arc,
};

use crate::services::MemberSetResolver;

pub struct Declaration<'a> {
    pub name: &'a str,
    pub expression: MemberSetExpr<'a>,
}

pub struct Payment<'a> {
    pub amount: Money,
    pub payer: MemberSetExpr<'a>,
    pub payee: MemberSetExpr<'a>,
}

pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Payment(Payment<'a>),
}

pub struct Program<'a> {
    members: Vec<MemberId>,
    statements: Vec<Statement<'a>>,
}

pub struct BalanceAccumulator<'a> {
    balances: MemberBalances,
    resolver: MemberSetResolver<'a>,
}

pub struct StatementWithLine<'a> {
    pub line: usize,
    pub statement: Statement<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProgramBuildError<'a> {
    UndefinedGroup { name: &'a str, line: usize },
    FailedToEvaluateGroup { name: &'a str, line: usize },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RemainderPolicy {
    /// Distribute remainder one unit at a time, starting from the front
    /// (currently unused with deferred rounding).
    FrontLoad,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AmountExpr {
    ops: SmallVec<[AmountOp; 1]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AmountOp {
    Literal(Decimal),
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AmountError {
    Overflow,
    DivisionByZero,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SplitError {
    ZeroRecipients,
    EmptyRatios,
    ZeroTotalRatio,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ratios {
    weights: Vec<u64>,
    total: u64,
}

impl Ratios {
    pub fn try_new(weights: Vec<u64>) -> Result<Self, SplitError> {
        if weights.is_empty() {
            return Err(SplitError::EmptyRatios);
        }

        let total: u64 = weights.iter().sum();
        if total == 0 {
            return Err(SplitError::ZeroTotalRatio);
        }

        Ok(Self { weights, total })
    }

    pub fn len(&self) -> usize {
        self.weights.len()
    }

    pub fn is_empty(&self) -> bool {
        self.weights.is_empty()
    }

    pub fn weights(&self) -> &[u64] {
        &self.weights
    }

    pub fn total(&self) -> u64 {
        self.total
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Money type with high-precision decimal arithmetic.
///
/// Uses 28-digit precision internally to ensure accurate financial calculations.
/// Supports fractions during intermediate calculations, with rounding deferred until settlement.
///
/// # Design Principles
/// * **Internal precision**: Keep full precision during calculations (splits, aggregations)
/// * **Deferred rounding**: Only round at settlement time
/// * **Zero-sum invariant**: Settlement ensures sum of balances equals zero
///
/// # Example
/// ```
/// use walicord_domain::{Money, RemainderPolicy};
///
/// let amount = Money::new(1005, 1); // 100.5
/// let shares: Vec<_> = amount.split_even(3, RemainderPolicy::FrontLoad).collect();
/// // Each share has full precision: 33.5 exactly
/// ```
pub struct Money(Decimal);

impl Money {
    /// Zero value (0.0).
    pub const ZERO: Self = Self(Decimal::ZERO);

    /// Creates a Money value from an i64.
    ///
    /// # Example
    /// ```
    /// use walicord_domain::Money;
    ///
    /// let money = Money::from_i64(100);
    /// ```
    pub fn from_i64(value: i64) -> Self {
        Self(Decimal::from(value))
    }

    /// Creates a Money value from mantissa and scale.
    ///
    /// The value is calculated as: `mantissa * 10^(-scale)`
    ///
    /// # Example
    /// ```
    /// use walicord_domain::Money;
    ///
    /// // 100.5 = 1005 * 10^-1
    /// let money = Money::new(1005, 1);
    /// assert_eq!(money.to_string(), "100.5");
    ///
    /// // 100 = 100 * 10^0
    /// let integer = Money::new(100, 0);
    /// assert_eq!(integer.to_string(), "100");
    ///
    /// // -50.25 = -5025 * 10^-2
    /// let negative = Money::new(-5025, 2);
    /// assert_eq!(negative.to_string(), "-50.25");
    /// ```
    pub fn new(mantissa: i64, scale: u32) -> Self {
        Self(Decimal::new(mantissa, scale))
    }

    /// Creates a Money value from a Decimal.
    ///
    /// This is intended for advanced use cases where you need to perform
    /// operations not directly supported by the Money API. Prefer `new()`
    /// or `from_i64()` for standard use.
    pub fn from_decimal(value: Decimal) -> Self {
        Self(value)
    }

    /// Returns the underlying Decimal value.
    ///
    /// This is intended for advanced use cases where you need to perform
    /// operations not directly supported by the Money API (e.g., custom rounding
    /// or serialization). Prefer using Money's standard methods when possible.
    pub fn as_decimal(self) -> Decimal {
        self.0
    }

    /// Returns the absolute value.
    pub fn abs(self) -> Self {
        Self(self.0.abs())
    }

    /// Returns true if the value is zero.
    pub fn is_zero(self) -> bool {
        self == Self::ZERO
    }

    /// Returns the sign of the value: -1, 0, or 1.
    pub fn signum(self) -> i64 {
        if self.0 > Decimal::ZERO {
            1
        } else if self.0 < Decimal::ZERO {
            -1
        } else {
            0
        }
    }

    /// Converts to i64 if the value has no fractional part.
    ///
    /// Returns `None` if the value has a fractional component.
    pub fn to_i64_checked(self) -> Option<i64> {
        if self.0.fract() != Decimal::ZERO {
            return None;
        }
        self.0.to_i64()
    }

    /// Rounds to the specified number of decimal places.
    ///
    /// Used for display and settlement. Note that rounding during settlement
    /// requires additional adjustment to maintain zero-sum invariant.
    ///
    /// # Arguments
    /// * `scale` - Number of decimal places (0 for JPY, 2 for USD)
    pub fn round(self, scale: u32) -> Self {
        Self(self.0.round_dp(scale))
    }

    /// Splits the amount evenly among `n` recipients with full precision.
    ///
    /// # Safety
    /// * `n` must be > 0 (panics if zero).
    ///
    /// # Semantics
    /// * **Logical split, not monetary distribution.**
    /// * This represents balance allocation (accounting), not physical payment units.
    /// * Each share has the exact same value: `self / n`
    /// * The sum of shares equals the original amount (within epsilon for Decimal precision)
    ///
    /// # Example
    /// ```
    /// use walicord_domain::{Money, RemainderPolicy};
    ///
    /// let amount = Money::from_i64(100);
    /// let shares: Vec<_> = amount.split_even(3, RemainderPolicy::FrontLoad).collect();
    /// // Each share is 33.33333333333333333333333333 (28-digit precision)
    /// ```
    pub fn split_even(self, n: usize, _policy: RemainderPolicy) -> impl Iterator<Item = Money> {
        assert!(n > 0, "Cannot split by zero");
        let share = self.0 / Decimal::from(n as i64);
        std::iter::repeat_n(Self(share), n)
    }

    /// Splits the amount by the given ratios with full precision.
    ///
    /// Each recipient gets a proportion of the total based on their weight.
    /// Shares are calculated as: `amount * weight / sum(weights)`
    ///
    /// # Arguments
    /// * `ratios` - The ratio weights for distribution
    /// * `policy` - Remainder distribution policy (currently unused with deferred rounding)
    pub fn split_ratio(self, ratios: &Ratios, policy: RemainderPolicy) -> Vec<Self> {
        let _ = policy;
        let total = self.0;
        let sum = Decimal::from(ratios.total());
        ratios
            .weights()
            .iter()
            .map(|&w| Self(total * Decimal::from(w) / sum))
            .collect()
    }
}

impl TryFrom<u64> for Money {
    type Error = AmountError;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(Self(Decimal::from(value)))
    }
}

impl AmountExpr {
    pub fn new<I>(ops: I) -> Self
    where
        I: IntoIterator<Item = AmountOp>,
        <I as IntoIterator>::IntoIter: ExactSizeIterator,
    {
        let iter = ops.into_iter();
        let mut values = SmallVec::with_capacity(iter.len());
        values.extend(iter);
        Self { ops: values }
    }

    pub fn ops(&self) -> &[AmountOp] {
        &self.ops
    }

    pub fn evaluate(&self) -> Result<Money, AmountError> {
        let value = self.evaluate_decimal()?;
        Ok(Money::from_decimal(value))
    }

    fn evaluate_decimal(&self) -> Result<Decimal, AmountError> {
        let mut stack: Vec<Decimal> = Vec::with_capacity(self.ops.len());
        for op in &self.ops {
            match op {
                AmountOp::Literal(value) => stack.push(*value),
                AmountOp::Add => apply_binary(&mut stack, checked_add)?,
                AmountOp::Sub => apply_binary(&mut stack, checked_sub)?,
                AmountOp::Mul => apply_binary(&mut stack, checked_mul)?,
                AmountOp::Div => apply_binary(&mut stack, checked_div)?,
            }
        }

        if stack.len() == 1 {
            Ok(stack.pop().unwrap_or(Decimal::ZERO))
        } else {
            Err(AmountError::Overflow)
        }
    }
}

fn apply_binary(
    stack: &mut Vec<Decimal>,
    op: fn(Decimal, Decimal) -> Result<Decimal, AmountError>,
) -> Result<(), AmountError> {
    let rhs = stack.pop().ok_or(AmountError::Overflow)?;
    let lhs = stack.pop().ok_or(AmountError::Overflow)?;
    stack.push(op(lhs, rhs)?);
    Ok(())
}

fn checked_add(lhs: Decimal, rhs: Decimal) -> Result<Decimal, AmountError> {
    lhs.checked_add(rhs).ok_or(AmountError::Overflow)
}

fn checked_sub(lhs: Decimal, rhs: Decimal) -> Result<Decimal, AmountError> {
    lhs.checked_sub(rhs).ok_or(AmountError::Overflow)
}

fn checked_mul(lhs: Decimal, rhs: Decimal) -> Result<Decimal, AmountError> {
    lhs.checked_mul(rhs).ok_or(AmountError::Overflow)
}

fn checked_div(lhs: Decimal, rhs: Decimal) -> Result<Decimal, AmountError> {
    if rhs == Decimal::ZERO {
        return Err(AmountError::DivisionByZero);
    }
    lhs.checked_div(rhs).ok_or(AmountError::Overflow)
}

impl fmt::Display for AmountError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AmountError::Overflow => write!(f, "amount is out of range"),
            AmountError::DivisionByZero => write!(f, "division by zero"),
        }
    }
}

impl fmt::Display for Money {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add for Money {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl AddAssign for Money {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl Sub for Money {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl SubAssign for Money {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
    }
}

impl Neg for Money {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl std::iter::Sum for Money {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::ZERO, Self::add)
    }
}

impl<'a> std::iter::Sum<&'a Money> for Money {
    fn sum<I: Iterator<Item = &'a Money>>(iter: I) -> Self {
        iter.fold(Self::ZERO, |acc, value| acc + *value)
    }
}

impl std::ops::Mul<Decimal> for Money {
    type Output = Self;

    fn mul(self, rhs: Decimal) -> Self::Output {
        Self(self.0 * rhs)
    }
}

impl std::ops::Div<Decimal> for Money {
    type Output = Self;

    fn div(self, rhs: Decimal) -> Self::Output {
        Self(self.0 / rhs)
    }
}

/// Discord user ID
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MemberId(pub u64);
impl walicord_transfer_construction::MemberIdTrait for MemberId {}

pub type MemberBalances = BTreeMap<MemberId, Money>;

trait MaskWord:
    Copy
    + Eq
    + BitOr<Output = Self>
    + BitOrAssign
    + BitAnd<Output = Self>
    + BitAndAssign
    + BitXor<Output = Self>
    + BitXorAssign
    + Not<Output = Self>
    + Shl<u32, Output = Self>
{
    const BITS: usize;
    const ZERO: Self;
    const ONE: Self;
    const COUNT_ONES: fn(Self) -> u32;
    const TRAILING_ZEROS: fn(Self) -> u32;
}

impl MaskWord for u32 {
    const BITS: usize = Self::BITS as usize;
    const ZERO: Self = 0;
    const ONE: Self = 1;
    const COUNT_ONES: fn(Self) -> u32 = Self::count_ones;
    const TRAILING_ZEROS: fn(Self) -> u32 = Self::trailing_zeros;
}

impl MaskWord for u64 {
    const BITS: usize = Self::BITS as usize;
    const ZERO: Self = 0;
    const ONE: Self = 1;
    const COUNT_ONES: fn(Self) -> u32 = Self::count_ones;
    const TRAILING_ZEROS: fn(Self) -> u32 = Self::trailing_zeros;
}

impl MaskWord for u128 {
    const BITS: usize = Self::BITS as usize;
    const ZERO: Self = 0;
    const ONE: Self = 1;
    const COUNT_ONES: fn(Self) -> u32 = Self::count_ones;
    const TRAILING_ZEROS: fn(Self) -> u32 = Self::trailing_zeros;
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemberSetOp<'a> {
    Push(MemberId),     // Discord user ID (from mention)
    PushGroup(&'a str), // Group name reference
    Union,
    Intersection,
    Difference,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MemberSetExpr<'a> {
    ops: SmallVec<[MemberSetOp<'a>; 3]>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemberSet {
    members: Vec<MemberId>,
}

/// Maintains stable index mapping between `MemberId` and bit positions.
#[derive(Clone, Debug, PartialEq, Eq)]
struct MemberIndex {
    idx_to_id: Vec<MemberId>,
    id_to_idx: FxHashMap<MemberId, u8>,
}

impl MemberIndex {
    fn try_new<M>(members: impl IntoIterator<Item = MemberId>) -> Option<Self>
    where
        M: MaskWord,
    {
        let members = members.into_iter();
        let (_, upper) = members.size_hint();
        let cap = upper.unwrap_or_default().min(M::BITS);

        let mut idx_to_id = Vec::with_capacity(cap);
        let mut id_to_idx = FxHashMap::with_capacity_and_hasher(cap, Default::default());

        for id in members {
            let idx = idx_to_id.len();
            if idx >= M::BITS {
                return None;
            }

            if let std::collections::hash_map::Entry::Vacant(entry) = id_to_idx.entry(id) {
                idx_to_id.push(id);
                entry.insert(idx as u8);
            }
        }

        Some(Self {
            idx_to_id,
            id_to_idx,
        })
    }

    #[inline]
    fn bit_of<M>(&self, id: MemberId) -> Option<M>
    where
        M: MaskWord,
    {
        let idx = *self.id_to_idx.get(&id)? as u32;
        Some(M::ONE << idx)
    }

    fn set_to_mask<I, M>(&self, members: I) -> Option<M>
    where
        I: IntoIterator<Item = MemberId>,
        M: MaskWord,
    {
        let mut mask = M::ZERO;
        for id in members {
            let bit = self.bit_of::<M>(id)?;
            mask |= bit;
        }
        Some(mask)
    }

    fn mask_to_set<M>(&self, mask: M) -> FxHashSet<MemberId>
    where
        M: MaskWord,
    {
        let mut out =
            FxHashSet::with_capacity_and_hasher(M::COUNT_ONES(mask) as usize, Default::default());
        let mut m = mask;
        while m != M::ZERO {
            let idx = M::TRAILING_ZEROS(m);
            let bit = M::ONE << idx;
            if let Some(&id) = self.idx_to_id.get(idx as usize) {
                out.insert(id);
            }
            m ^= bit;
        }
        out
    }
}

impl MemberSet {
    pub fn new(members: Vec<MemberId>) -> Self {
        Self { members }
    }

    pub fn members(&self) -> &[MemberId] {
        &self.members
    }

    pub fn iter(&self) -> impl Iterator<Item = MemberId> + '_ {
        self.members.iter().copied()
    }

    pub fn is_empty(&self) -> bool {
        self.members.is_empty()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemberInfo {
    pub id: MemberId,
    pub display_name: Arc<str>,
    pub username: Arc<str>,
    pub avatar_url: Option<Arc<str>>,
}

impl MemberInfo {
    pub fn effective_name(&self) -> &str {
        &self.display_name
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fxhash::FxHashMap;
    use rstest::rstest;
    use rust_decimal::Decimal;

    fn dec(value: i64) -> Decimal {
        Decimal::from(value)
    }

    fn assert_sum_within_epsilon(values: &[Money], expected: Decimal) {
        let sum: Money = values.iter().sum();
        let epsilon = Decimal::new(1, 6);
        let diff = (sum.as_decimal() - expected).abs();
        assert!(diff <= epsilon, "sum must be within epsilon (diff: {diff})");
    }

    #[rstest]
    #[case::literal(AmountExpr::new([AmountOp::Literal(dec(100))]), 100)]
    #[case::add(
        AmountExpr::new([
            AmountOp::Literal(dec(100)),
            AmountOp::Literal(dec(50)),
            AmountOp::Add,
        ]),
        150
    )]
    #[case::mul(
        AmountExpr::new([
            AmountOp::Literal(dec(20)),
            AmountOp::Literal(dec(5)),
            AmountOp::Mul,
        ]),
        100
    )]
    #[case::mixed(
        AmountExpr::new([
            AmountOp::Literal(dec(100)),
            AmountOp::Literal(dec(200)),
            AmountOp::Literal(dec(3)),
            AmountOp::Mul,
            AmountOp::Add,
        ]),
        700
    )]
    fn amount_expr_evaluates(#[case] expr: AmountExpr, #[case] expected: i64) {
        let money = expr.evaluate().expect("amount expr should evaluate");
        assert_eq!(money.as_decimal(), dec(expected));
    }

    #[rstest]
    #[case::division_by_zero(AmountExpr::new([
        AmountOp::Literal(dec(10)),
        AmountOp::Literal(dec(0)),
        AmountOp::Div,
    ]))]
    fn amount_expr_rejects_invalid(#[case] expr: AmountExpr) {
        let result = expr.evaluate();
        assert!(result.is_err());
    }

    #[rstest]
    #[case::negative_result(
        AmountExpr::new([
            AmountOp::Literal(dec(10)),
            AmountOp::Literal(dec(20)),
            AmountOp::Sub,
        ]),
        -10
    )]
    fn amount_expr_allows_negative(#[case] expr: AmountExpr, #[case] expected: i64) {
        let money = expr.evaluate().expect("amount expr should allow negative");
        assert_eq!(money.as_decimal(), dec(expected));
    }

    #[rstest]
    #[case::even_3(100, 3)]
    #[case::even_2(100, 2)]
    #[case::even_1(100, 1)]
    #[case::no_remainder(99, 3)]
    #[case::zero_amount(0, 3)]
    fn split_even_distributes_correctly(#[case] amount: i64, #[case] n: usize) {
        let money = Money::from_i64(amount);
        let result: Vec<_> = money.split_even(n, RemainderPolicy::FrontLoad).collect();
        let expected = dec(amount) / dec(n as i64);
        for share in &result {
            assert_eq!(share.as_decimal(), expected);
        }
        assert_sum_within_epsilon(&result, dec(amount));
    }

    #[rstest]
    #[should_panic(expected = "Cannot split by zero")]
    fn split_even_zero_count_panics() {
        let _ = Money::from_i64(100).split_even(0, RemainderPolicy::FrontLoad);
    }

    #[rstest]
    #[case::negative_even(-100, 3)]
    #[case::negative_no_remainder(-99, 3)]
    fn split_even_negative(#[case] amount: i64, #[case] n: usize) {
        let money = Money::from_i64(amount);
        let result: Vec<_> = money.split_even(n, RemainderPolicy::FrontLoad).collect();
        let expected = dec(amount) / dec(n as i64);
        for share in &result {
            assert_eq!(share.as_decimal(), expected);
        }
        assert_sum_within_epsilon(&result, dec(amount));
    }

    #[rstest]
    #[case::ratio_2_1(100, vec![2, 1])]
    #[case::ratio_1_1(100, vec![1, 1])]
    #[case::ratio_1_1_1(100, vec![1, 1, 1])]
    #[case::ratio_3_1(100, vec![3, 1])]
    fn split_ratio_distributes_correctly(#[case] amount: i64, #[case] ratios: Vec<u64>) {
        let money = Money::from_i64(amount);
        let ratios = Ratios::try_new(ratios).expect("ratios must be non-empty and non-zero");
        let result = money.split_ratio(&ratios, RemainderPolicy::FrontLoad);
        let total = dec(amount);
        let sum = dec(ratios.total() as i64);
        for (share, &weight) in result.iter().zip(ratios.weights()) {
            let expected = total * dec(weight as i64) / sum;
            assert_eq!(share.as_decimal(), expected);
        }
        assert_sum_within_epsilon(&result, dec(amount));
    }

    #[rstest]
    fn ratios_reject_empty() {
        let result = Ratios::try_new(vec![]);
        assert_eq!(result, Err(SplitError::EmptyRatios));
    }

    #[rstest]
    fn ratios_reject_zero_sum() {
        let result = Ratios::try_new(vec![0, 0]);
        assert_eq!(result, Err(SplitError::ZeroTotalRatio));
    }

    #[rstest]
    #[case::display_name("Nickname")]
    fn member_info_effective_name_returns_display_name(#[case] display_name: &str) {
        let info = MemberInfo {
            id: MemberId(1),
            display_name: Arc::from(display_name),
            username: Arc::from("username"),
            avatar_url: None,
        };
        assert_eq!(info.effective_name(), display_name);
    }

    #[rstest]
    fn member_info_clone_shares_arc() {
        let info1 = MemberInfo {
            id: MemberId(1),
            display_name: Arc::from("Test"),
            username: Arc::from("test"),
            avatar_url: None,
        };
        let info2 = info1.clone();

        assert_eq!(info1.display_name.as_ptr(), info2.display_name.as_ptr());
    }

    #[rstest]
    fn member_index_mask_roundtrip() {
        let members = [MemberId(10), MemberId(20), MemberId(30)];
        let index = MemberIndex::try_new::<u32>(members).expect("index should fit u32");

        let mask = index
            .set_to_mask::<_, u32>([MemberId(30), MemberId(10)])
            .expect("all members should be indexed");
        let roundtrip = index.mask_to_set(mask);

        assert_eq!(roundtrip.len(), 2);
        assert!(roundtrip.contains(&MemberId(10)));
        assert!(roundtrip.contains(&MemberId(30)));
    }

    #[rstest]
    fn member_set_expr_evaluate_mask_supports_set_operations() {
        let index =
            MemberIndex::try_new::<u32>([MemberId(1), MemberId(2), MemberId(3)]).expect("fit");
        let mut groups: FxHashMap<&str, u32> = FxHashMap::default();
        groups.insert(
            "A",
            index
                .set_to_mask::<_, u32>([MemberId(1), MemberId(2)])
                .expect("group A members should be indexed"),
        );
        groups.insert(
            "B",
            index
                .set_to_mask::<_, u32>([MemberId(2), MemberId(3)])
                .expect("group B members should be indexed"),
        );

        let expr = MemberSetExpr::new([
            MemberSetOp::PushGroup("A"),
            MemberSetOp::PushGroup("B"),
            MemberSetOp::Union,
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Difference,
        ]);

        let mask = expr
            .evaluate_mask::<u32, _>(&index, &|name| groups.get(name).copied())
            .expect("expression should evaluate");
        let mut result = index.mask_to_set(mask).into_iter().collect::<Vec<_>>();
        result.sort_unstable();

        assert_eq!(result, vec![MemberId(1), MemberId(3)]);
    }

    #[rstest]
    fn member_set_expr_evaluate_returns_none_when_member_count_exceeds_u128() {
        let member_count = 129usize;
        let mut ops = Vec::with_capacity(member_count + member_count - 1);
        for i in 1..=member_count {
            ops.push(MemberSetOp::Push(MemberId(i as u64)));
        }
        for _ in 1..member_count {
            ops.push(MemberSetOp::Union);
        }

        let expr = MemberSetExpr::new(ops);
        let result = expr.evaluate(&|_| None);

        assert!(result.is_none());
    }

    #[rstest]
    #[case::payment_undefined_group(
        StatementWithLine {
            line: 1,
            statement: Statement::Payment(Payment {
                amount: Money::try_from(100).expect("amount should fit in i64"),
                payer: MemberSetExpr::new([MemberSetOp::PushGroup("team")]),
                payee: MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
            }),
        },
        "team",
        1
    )]
    #[case::declaration_undefined_group(
        StatementWithLine {
            line: 1,
            statement: Statement::Declaration(Declaration {
                name: "group_b",
                expression: MemberSetExpr::new([MemberSetOp::PushGroup("group_a")]),
            }),
        },
        "group_a",
        1
    )]
    fn program_rejects_undefined_group_references(
        #[case] statement: StatementWithLine<'static>,
        #[case] expected_name: &str,
        #[case] expected_line: usize,
    ) {
        let result = Program::try_new(vec![statement], &[]);

        match result {
            Err(ProgramBuildError::UndefinedGroup { name, line }) => {
                assert_eq!(name, expected_name);
                assert_eq!(line, expected_line);
            }
            _ => panic!("expected undefined group error"),
        }
    }

    #[rstest]
    fn program_accepts_declaration_then_reference() {
        let declaration = StatementWithLine {
            line: 1,
            statement: Statement::Declaration(Declaration {
                name: "group_a",
                expression: MemberSetExpr::new([
                    MemberSetOp::Push(MemberId(1)),
                    MemberSetOp::Push(MemberId(2)),
                    MemberSetOp::Union,
                ]),
            }),
        };
        let payment = StatementWithLine {
            line: 2,
            statement: Statement::Payment(Payment {
                amount: Money::try_from(120).expect("amount should fit in i64"),
                payer: MemberSetExpr::new([MemberSetOp::PushGroup("group_a")]),
                payee: MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
            }),
        };

        let result = Program::try_new(vec![declaration, payment], &[]);
        let program = result.expect("program should accept declaration before reference");
        assert_eq!(program.statements().len(), 2);
    }
}

impl<'a> MemberSetExpr<'a> {
    pub fn new<I>(ops: I) -> Self
    where
        I: IntoIterator<Item = MemberSetOp<'a>>,
        <I as IntoIterator>::IntoIter: ExactSizeIterator,
    {
        let iter = ops.into_iter();
        let mut values = SmallVec::with_capacity(iter.len());
        values.extend(iter);
        Self { ops: values }
    }

    /// Evaluate the expression to produce a set of member IDs
    ///
    /// # Arguments
    /// * `member_resolver` - Resolves a group name to a borrowed set of member IDs
    pub fn evaluate<'r, F>(&self, member_resolver: &F) -> Option<Cow<'r, FxHashSet<MemberId>>>
    where
        F: Fn(&str) -> Option<&'r FxHashSet<MemberId>>,
    {
        if self.ops.len() == 1 {
            return match self.ops[0] {
                MemberSetOp::Push(id) => {
                    let mut set = FxHashSet::with_capacity_and_hasher(1, Default::default());
                    set.insert(id);
                    Some(Cow::Owned(set))
                }
                MemberSetOp::PushGroup(name) => Some(Cow::Borrowed(member_resolver(name)?)),
                _ => None,
            };
        }

        let mut referenced_groups = FxHashMap::default();
        for name in self.referenced_groups() {
            if let std::collections::hash_map::Entry::Vacant(entry) = referenced_groups.entry(name)
            {
                entry.insert(member_resolver(name)?);
            }
        }

        if let Some(result) = self.evaluate_with_mask::<u32>(&referenced_groups) {
            return Some(Cow::Owned(result));
        }
        if let Some(result) = self.evaluate_with_mask::<u64>(&referenced_groups) {
            return Some(Cow::Owned(result));
        }
        if let Some(result) = self.evaluate_with_mask::<u128>(&referenced_groups) {
            return Some(Cow::Owned(result));
        }

        None
    }

    fn evaluate_with_mask<M>(
        &self,
        referenced_groups: &FxHashMap<&'a str, &'_ FxHashSet<MemberId>>,
    ) -> Option<FxHashSet<MemberId>>
    where
        M: MaskWord,
    {
        let index = MemberIndex::try_new::<M>(
            self.referenced_ids().chain(
                referenced_groups
                    .values()
                    .flat_map(|set| set.iter().copied()),
            ),
        )?;

        let mut group_masks =
            FxHashMap::with_capacity_and_hasher(referenced_groups.len(), Default::default());
        for (&name, members) in referenced_groups {
            group_masks.insert(name, index.set_to_mask::<_, M>(members.iter().copied())?);
        }

        let mask = self.evaluate_mask::<M, _>(&index, &|name| group_masks.get(name).copied())?;
        Some(index.mask_to_set(mask))
    }

    fn evaluate_mask<M, F>(&self, index: &MemberIndex, group_resolver: &F) -> Option<M>
    where
        M: MaskWord,
        F: Fn(&str) -> Option<M>,
    {
        let mut stack: Vec<M> = Vec::with_capacity(self.ops.len());

        for op in &self.ops {
            match *op {
                MemberSetOp::Push(id) => {
                    let bit = index.bit_of::<M>(id)?;
                    stack.push(bit);
                }
                MemberSetOp::PushGroup(name) => {
                    let mask = group_resolver(name)?;
                    stack.push(mask);
                }
                MemberSetOp::Union => {
                    let rhs = stack.pop()?;
                    let lhs = stack.pop()?;
                    stack.push(lhs | rhs);
                }
                MemberSetOp::Intersection => {
                    let rhs = stack.pop()?;
                    let lhs = stack.pop()?;
                    stack.push(lhs & rhs);
                }
                MemberSetOp::Difference => {
                    let rhs = stack.pop()?;
                    let lhs = stack.pop()?;
                    stack.push(lhs & !rhs);
                }
            }
        }

        if stack.len() == 1 { stack.pop() } else { None }
    }

    /// Returns all directly referenced member IDs
    pub fn referenced_ids(&self) -> impl Iterator<Item = MemberId> + '_ {
        self.ops.iter().filter_map(|op| match op {
            MemberSetOp::Push(id) => Some(*id),
            _ => None,
        })
    }

    /// Returns all referenced group names
    pub fn referenced_groups(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.ops.iter().filter_map(|op| match op {
            MemberSetOp::PushGroup(name) => Some(*name),
            _ => None,
        })
    }
}

impl<'a> Program<'a> {
    pub fn try_new(
        statements: Vec<StatementWithLine<'a>>,
        member_ids: &[MemberId],
    ) -> Result<Self, ProgramBuildError<'a>> {
        let mut validated_statements = Vec::with_capacity(statements.len());
        let mut resolver = MemberSetResolver::new_with_members(member_ids.iter().copied());

        for StatementWithLine { line, statement } in statements {
            match &statement {
                Statement::Declaration(decl) => {
                    // Check that all referenced groups are defined
                    for group_name in decl.expression.referenced_groups() {
                        if !resolver.is_defined(group_name) {
                            return Err(ProgramBuildError::UndefinedGroup {
                                name: group_name,
                                line,
                            });
                        }
                    }

                    let members_vec = resolver.evaluate_members(&decl.expression).ok_or(
                        ProgramBuildError::FailedToEvaluateGroup {
                            name: decl.name,
                            line,
                        },
                    )?;
                    resolver.register_group_members(decl.name, members_vec.iter());
                }
                Statement::Payment(payment) => {
                    // Check that all referenced groups in payment are defined
                    for group_name in payment
                        .payer
                        .referenced_groups()
                        .chain(payment.payee.referenced_groups())
                    {
                        if !resolver.is_defined(group_name) {
                            return Err(ProgramBuildError::UndefinedGroup {
                                name: group_name,
                                line,
                            });
                        }
                    }
                }
            }

            validated_statements.push(statement);
        }

        Ok(Self {
            members: member_ids.to_vec(),
            statements: validated_statements,
        })
    }

    pub fn statements(&self) -> &[Statement<'a>] {
        &self.statements
    }

    pub fn calculate_balances(&self) -> MemberBalances {
        let mut accumulator = BalanceAccumulator::new_with_members(&self.members);
        for stmt in &self.statements {
            accumulator.apply(stmt);
        }
        accumulator.into_balances()
    }
}

impl<'a> Default for BalanceAccumulator<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> BalanceAccumulator<'a> {
    pub fn new() -> Self {
        Self::new_with_members(&[])
    }

    pub fn new_with_members(member_ids: &[MemberId]) -> Self {
        let balances: MemberBalances = MemberBalances::default();
        let resolver = MemberSetResolver::new_with_members(member_ids.iter().copied());

        Self { balances, resolver }
    }

    pub fn apply(&mut self, statement: &Statement<'a>) {
        match statement {
            Statement::Declaration(decl) => {
                for member_id in decl.expression.referenced_ids() {
                    self.balances.entry(member_id).or_insert(Money::ZERO);
                }
                let Some(members_vec) = self.resolver.evaluate_members(&decl.expression) else {
                    return;
                };
                for member in members_vec.iter() {
                    self.balances.entry(member).or_insert(Money::ZERO);
                }
                self.resolver
                    .register_group_members(decl.name, members_vec.iter());
            }
            Statement::Payment(payment) => {
                for member_id in payment
                    .payer
                    .referenced_ids()
                    .chain(payment.payee.referenced_ids())
                {
                    self.balances.entry(member_id).or_insert(Money::ZERO);
                }
                let Some(payer_members) = self.resolver.evaluate_members(&payment.payer) else {
                    return;
                };
                let Some(payee_members) = self.resolver.evaluate_members(&payment.payee) else {
                    return;
                };

                distribute_balances(&mut self.balances, &payer_members, payment.amount, 1);
                distribute_balances(&mut self.balances, &payee_members, payment.amount, -1);
            }
        }
    }

    pub fn balances(&self) -> &MemberBalances {
        &self.balances
    }

    pub fn into_balances(self) -> MemberBalances {
        self.balances
    }

    pub fn set_balances(&mut self, balances: MemberBalances) {
        self.balances = balances;
    }

    pub fn evaluate_members(&self, expr: &MemberSetExpr<'a>) -> Option<MemberSet> {
        self.resolver.evaluate_members(expr)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Transfer {
    pub from: MemberId,
    pub to: MemberId,
    pub amount: Money,
}

#[derive(Debug, PartialEq)]
pub struct Settlement {
    pub new_balances: MemberBalances,
    pub transfers: Vec<Transfer>,
}

/// Distributes an amount evenly among members, updating their balances.
///
/// This function performs a logical split of the amount and adds/subtracts
/// shares from each member's balance. It uses deferred rounding - shares
/// maintain full precision, with rounding only happening at settlement time.
///
/// # Arguments
/// * `balances` - Map of member balances to update
/// * `members` - Set of members to distribute among
/// * `amount` - Total amount to distribute
/// * `direction` - +1 for debt/net-pay side (payer), -1 for credit/net-receive side (payee)
///
/// # Semantics
/// * Uses `split_even` to divide amount with full precision
/// * Each member gets exactly `amount / n` (no remainder absorption)
/// * Guarantees sum of shares equals original amount (within Decimal epsilon)
///
/// # Example
/// ```
/// use walicord_domain::model::{MemberBalances, MemberSet, MemberId, Money, distribute_balances};
/// use rust_decimal::Decimal;
///
/// let mut balances = MemberBalances::new();
/// let members = MemberSet::new(vec![MemberId(1), MemberId(2), MemberId(3)]);
/// let amount = Money::from_decimal(Decimal::new(100, 0));
///
/// // Debt side (payer)
/// distribute_balances(&mut balances, &members, amount, 1);
/// // Each member's balance is +33.33333333333333333333333333
///
/// // Credit side (payee)
/// distribute_balances(&mut balances, &members, amount, -1);
/// // Each member's balance is 0 (credited then debited)
/// ```
pub fn distribute_balances(
    balances: &mut MemberBalances,
    members: &MemberSet,
    amount: Money,
    direction: i64,
) {
    if members.is_empty() {
        return;
    }

    let shares = amount.split_even(members.members().len(), RemainderPolicy::FrontLoad);

    for (member, share) in members.iter().zip(shares) {
        let signed = share * Decimal::from(direction);
        *balances.entry(member).or_insert(Money::ZERO) += signed;
    }
}
