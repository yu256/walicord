use arcstr::ArcStr;
use fxhash::{FxHashMap, FxHashSet};
use rust_decimal::{Decimal, prelude::ToPrimitive};
use smallvec::SmallVec;
use smol_str::SmolStr;
use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt,
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Neg, Not,
        Shl, Sub, SubAssign,
    },
    sync::OnceLock,
};

use crate::services::{MemberSetResolutionError, MemberSetResolver};

mod balance_accumulator_impl;
mod settlement_support;

pub use settlement_support::{BalanceDeltaDirection, Settlement, Transfer, distribute_balances};

pub struct Declaration<'a> {
    pub name: &'a str,
    pub expression: MemberSetExpr<'a>,
}

/// Represents the distribution weight of a member.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Weight(pub u64);

impl Weight {
    pub const ZERO: Weight = Weight(u64::ZERO);
    pub const MAX: Weight = Weight(u64::MAX);

    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        self.0.checked_add(rhs.0).map(Weight)
    }
}

impl Add for Weight {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl std::iter::Sum for Weight {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Weight::ZERO, Self::add)
    }
}

impl<'a> std::iter::Sum<&'a Weight> for Weight {
    fn sum<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        iter.fold(Weight::ZERO, |a, b| a + *b)
    }
}

impl From<Weight> for Decimal {
    fn from(weight: Weight) -> Self {
        Decimal::from(weight.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WeightOverrideTarget {
    Member(MemberId),
    Role(RoleId),
    Group(SmolStr),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WeightOverride {
    pub target: WeightOverrideTarget,
    pub weight: Weight,
}

impl WeightOverride {
    pub fn member(member_id: MemberId, weight: Weight) -> Self {
        Self {
            target: WeightOverrideTarget::Member(member_id),
            weight,
        }
    }

    pub fn role(role_id: RoleId, weight: Weight) -> Self {
        Self {
            target: WeightOverrideTarget::Role(role_id),
            weight,
        }
    }

    pub fn group(name: impl Into<SmolStr>, weight: Weight) -> Self {
        Self {
            target: WeightOverrideTarget::Group(name.into()),
            weight,
        }
    }
}

/// Ordered explicit weights applied to a payment payee set.
///
/// Overrides are applied in insertion order and later entries win when they
/// target the same resolved member.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct WeightOverrides {
    entries: Vec<WeightOverride>,
}

impl WeightOverrides {
    pub fn new<I>(entries: I) -> Self
    where
        I: IntoIterator<Item = WeightOverride>,
    {
        Self {
            entries: entries.into_iter().collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn entries(&self) -> &[WeightOverride] {
        &self.entries
    }

    pub fn from_member_weights(weights: BTreeMap<MemberId, Weight>) -> Self {
        Self::new(
            weights
                .into_iter()
                .map(|(member_id, weight)| WeightOverride::member(member_id, weight)),
        )
    }

    fn resolved_weight_vector<'a>(
        &self,
        members: &MemberSet,
        resolver: &MemberSetResolver<'a>,
    ) -> Option<Vec<Weight>> {
        let mut weights = vec![Weight(1); members.members().len()];
        if self.entries.is_empty() {
            return Some(weights);
        }

        let member_to_index: FxHashMap<MemberId, usize> = members
            .iter()
            .enumerate()
            .map(|(idx, member_id)| (member_id, idx))
            .collect();

        for entry in &self.entries {
            match &entry.target {
                WeightOverrideTarget::Member(member_id) => {
                    if let Some(&idx) = member_to_index.get(member_id) {
                        weights[idx] = entry.weight;
                    }
                }
                WeightOverrideTarget::Role(role_id) => {
                    let role_members = resolver.role_members(*role_id)?;
                    for member_id in role_members {
                        if let Some(&idx) = member_to_index.get(member_id) {
                            weights[idx] = entry.weight;
                        }
                    }
                }
                WeightOverrideTarget::Group(group_name) => {
                    let group_members = resolver.group_members(group_name.as_str())?;
                    for member_id in group_members {
                        if let Some(&idx) = member_to_index.get(member_id) {
                            weights[idx] = entry.weight;
                        }
                    }
                }
            }
        }

        Some(weights)
    }
}

/// Strategy for distributing an amount among members.
///
/// This value object encapsulates the allocation logic, providing
/// a clear domain intent rather than exposing raw collections.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum AllocationStrategy {
    /// Distribute the amount evenly (`split_even`).
    #[default]
    Even,
    /// Distribute the amount proportionally with ordered explicit weight overrides.
    /// Members not matched by any override default to a weight of 1.
    Weighted(WeightOverrides),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResolvedAllocationStrategy {
    Even,
    Weighted(Ratios),
}

impl AllocationStrategy {
    /// Returns true if this is an even distribution strategy.
    pub fn is_even(&self) -> bool {
        matches!(self, Self::Even)
    }

    /// Returns the explicit weight overrides if this is a weighted strategy.
    pub fn weight_overrides(&self) -> Option<&WeightOverrides> {
        match self {
            Self::Even => None,
            Self::Weighted(overrides) => Some(overrides),
        }
    }

    fn resolve_for_payee_members<'a>(
        &self,
        members: &MemberSet,
        resolver: &MemberSetResolver<'a>,
    ) -> Result<Option<ResolvedAllocationStrategy>, BalanceError> {
        if members.is_empty() {
            return Ok(Some(ResolvedAllocationStrategy::Even));
        }

        match self {
            Self::Even => Ok(Some(ResolvedAllocationStrategy::Even)),
            Self::Weighted(overrides) => {
                let Some(weight_vec) = overrides.resolved_weight_vector(members, resolver) else {
                    return Ok(None);
                };
                let ratios = Ratios::try_new(weight_vec).map_err(|err| match err {
                    SplitError::WeightOverflow => BalanceError::WeightOverflow,
                    SplitError::ZeroTotalRatio => BalanceError::ZeroTotalWeight,
                    SplitError::EmptyRatios | SplitError::ZeroRecipients => {
                        unreachable!("non-empty payee members should produce non-empty ratios")
                    }
                })?;
                Ok(Some(ResolvedAllocationStrategy::Weighted(ratios)))
            }
        }
    }
}

pub struct Payment<'a> {
    pub amount: Money,
    pub payer: MemberSetExpr<'a>,
    pub payee: MemberSetExpr<'a>,
    pub allocation: AllocationStrategy,
}

impl<'a> Payment<'a> {
    /// Constructs a Payment with even distribution.
    pub fn even(amount: Money, payer: MemberSetExpr<'a>, payee: MemberSetExpr<'a>) -> Self {
        Self {
            amount,
            payer,
            payee,
            allocation: AllocationStrategy::Even,
        }
    }

    /// Constructs a Payment with weighted distribution.
    pub fn weighted(
        amount: Money,
        payer: MemberSetExpr<'a>,
        payee: MemberSetExpr<'a>,
        weights: BTreeMap<MemberId, Weight>,
    ) -> Self {
        Self::weighted_with_overrides(
            amount,
            payer,
            payee,
            WeightOverrides::from_member_weights(weights),
        )
    }

    /// Constructs a Payment with weighted distribution using ordered overrides.
    pub fn weighted_with_overrides(
        amount: Money,
        payer: MemberSetExpr<'a>,
        payee: MemberSetExpr<'a>,
        overrides: WeightOverrides,
    ) -> Self {
        Self {
            amount,
            payer,
            payee,
            allocation: AllocationStrategy::Weighted(overrides),
        }
    }
}

pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Payment(Payment<'a>),
}

pub struct Program<'a> {
    members: Vec<MemberId>,
    roles: &'a RoleMembers,
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
    UndefinedGroup { name: Cow<'a, str>, line: usize },
    UndefinedRole { id: RoleId, line: usize },
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
pub enum BalanceError {
    WeightOverflow,
    ZeroTotalWeight,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SplitError {
    ZeroRecipients,
    EmptyRatios,
    ZeroTotalRatio,
    WeightOverflow,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ratios {
    weights: Vec<Weight>,
    total: Weight,
}

impl Ratios {
    pub fn try_new(weights: Vec<Weight>) -> Result<Self, SplitError> {
        if weights.is_empty() {
            return Err(SplitError::EmptyRatios);
        }

        let total: Option<Weight> = weights
            .iter()
            .copied()
            .try_fold(Weight::ZERO, Weight::checked_add);
        match total {
            None => Err(SplitError::WeightOverflow),
            Some(Weight::ZERO) => Err(SplitError::ZeroTotalRatio),
            Some(total) => Ok(Self { weights, total }),
        }
    }

    pub fn len(&self) -> usize {
        self.weights.len()
    }

    pub fn is_empty(&self) -> bool {
        self.weights.is_empty()
    }

    pub fn weights(&self) -> &[Weight] {
        &self.weights
    }

    pub fn total(&self) -> Weight {
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

/// Discord role ID
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RoleId(pub u64);

pub type MemberBalances = BTreeMap<MemberId, Money>;
pub type RoleMembers = FxHashMap<RoleId, FxHashSet<MemberId>>;

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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MemberSetOp<'a> {
    Push(MemberId),     // Discord user ID (from mention)
    PushRole(RoleId),   // Discord role ID (from role mention)
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
    pub display_name: SmolStr,
    pub username: SmolStr,
    pub avatar_url: Option<ArcStr>,
}

impl MemberInfo {
    pub fn effective_name(&self) -> &str {
        &self.display_name
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

    /// Evaluate the expression to produce a set of member IDs.
    pub fn evaluate<'r, FG, FR>(
        &self,
        group_resolver: &FG,
        role_resolver: &FR,
    ) -> Option<Cow<'r, FxHashSet<MemberId>>>
    where
        FG: Fn(&str) -> Option<&'r FxHashSet<MemberId>>,
        FR: Fn(RoleId) -> Option<&'r FxHashSet<MemberId>>,
    {
        if self.ops.len() == 1 {
            return match self.ops[0] {
                MemberSetOp::Push(id) => {
                    let mut set = FxHashSet::with_capacity_and_hasher(1, Default::default());
                    set.insert(id);
                    Some(Cow::Owned(set))
                }
                MemberSetOp::PushRole(id) => Some(Cow::Borrowed(role_resolver(id)?)),
                MemberSetOp::PushGroup(name) => Some(Cow::Borrowed(group_resolver(name)?)),
                _ => None,
            };
        }

        let mut referenced_groups = FxHashMap::default();
        for name in self.referenced_groups() {
            if let std::collections::hash_map::Entry::Vacant(entry) = referenced_groups.entry(name)
            {
                entry.insert(group_resolver(name)?);
            }
        }

        let mut referenced_roles = FxHashMap::default();
        for role_id in self.referenced_role_ids() {
            if let std::collections::hash_map::Entry::Vacant(entry) =
                referenced_roles.entry(role_id)
            {
                entry.insert(role_resolver(role_id)?);
            }
        }

        if let Some(result) = self.evaluate_with_mask::<u32>(&referenced_groups, &referenced_roles)
        {
            return Some(Cow::Owned(result));
        }
        if let Some(result) = self.evaluate_with_mask::<u64>(&referenced_groups, &referenced_roles)
        {
            return Some(Cow::Owned(result));
        }
        if let Some(result) = self.evaluate_with_mask::<u128>(&referenced_groups, &referenced_roles)
        {
            return Some(Cow::Owned(result));
        }

        None
    }

    fn evaluate_with_mask<M>(
        &self,
        referenced_groups: &FxHashMap<&'a str, &'_ FxHashSet<MemberId>>,
        referenced_roles: &FxHashMap<RoleId, &'_ FxHashSet<MemberId>>,
    ) -> Option<FxHashSet<MemberId>>
    where
        M: MaskWord,
    {
        let index = MemberIndex::try_new::<M>(
            self.referenced_ids()
                .chain(
                    referenced_groups
                        .values()
                        .flat_map(|set| set.iter().copied()),
                )
                .chain(
                    referenced_roles
                        .values()
                        .flat_map(|set| set.iter().copied()),
                ),
        )?;

        let mut group_masks =
            FxHashMap::with_capacity_and_hasher(referenced_groups.len(), Default::default());
        for (&name, members) in referenced_groups {
            group_masks.insert(name, index.set_to_mask::<_, M>(members.iter().copied())?);
        }

        let mut role_masks =
            FxHashMap::with_capacity_and_hasher(referenced_roles.len(), Default::default());
        for (&role_id, members) in referenced_roles {
            role_masks.insert(role_id, index.set_to_mask::<_, M>(members.iter().copied())?);
        }

        let mask = self.evaluate_mask::<M>(&index, &group_masks, &role_masks)?;
        Some(index.mask_to_set(mask))
    }

    fn evaluate_mask<M>(
        &self,
        index: &MemberIndex,
        group_masks: &FxHashMap<&'a str, M>,
        role_masks: &FxHashMap<RoleId, M>,
    ) -> Option<M>
    where
        M: MaskWord,
    {
        let mut stack: Vec<M> = Vec::with_capacity(self.ops.len());

        for op in &self.ops {
            match *op {
                MemberSetOp::Push(id) => {
                    let bit = index.bit_of::<M>(id)?;
                    stack.push(bit);
                }
                MemberSetOp::PushRole(role_id) => {
                    let mask = role_masks.get(&role_id).copied()?;
                    stack.push(mask);
                }
                MemberSetOp::PushGroup(name) => {
                    let mask = group_masks.get(name).copied()?;
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

    /// Returns all referenced role IDs.
    pub fn referenced_role_ids(&self) -> impl Iterator<Item = RoleId> + '_ {
        self.ops.iter().filter_map(|op| match op {
            MemberSetOp::PushRole(id) => Some(*id),
            _ => None,
        })
    }
}

impl<'a> Program<'a> {
    fn empty_roles() -> &'static RoleMembers {
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(RoleMembers::default)
    }

    pub fn try_new(
        statements: Vec<StatementWithLine<'a>>,
        member_ids: &[MemberId],
    ) -> Result<Self, ProgramBuildError<'a>> {
        Self::try_new_with_roles(statements, member_ids, Self::empty_roles())
    }

    pub fn try_new_with_roles(
        statements: Vec<StatementWithLine<'a>>,
        member_ids: &[MemberId],
        role_members: &'a RoleMembers,
    ) -> Result<Self, ProgramBuildError<'a>> {
        let mut validated_statements = Vec::with_capacity(statements.len());
        let mut resolver =
            MemberSetResolver::new_with_context(member_ids.iter().copied(), role_members);

        for StatementWithLine { line, statement } in statements {
            match &statement {
                Statement::Declaration(decl) => {
                    // Check that all referenced groups are defined
                    for group_name in decl.expression.referenced_groups() {
                        if !resolver.is_defined(group_name) {
                            return Err(ProgramBuildError::UndefinedGroup {
                                name: Cow::Borrowed(group_name),
                                line,
                            });
                        }
                    }
                    for role_id in decl.expression.referenced_role_ids() {
                        if !resolver.is_role_defined(role_id) {
                            return Err(ProgramBuildError::UndefinedRole { id: role_id, line });
                        }
                    }

                    let members_vec =
                        resolver
                            .try_evaluate_members(&decl.expression)
                            .map_err(|_| ProgramBuildError::FailedToEvaluateGroup {
                                name: decl.name,
                                line,
                            })?;
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
                                name: Cow::Borrowed(group_name),
                                line,
                            });
                        }
                    }
                    for role_id in payment
                        .payer
                        .referenced_role_ids()
                        .chain(payment.payee.referenced_role_ids())
                    {
                        if !resolver.is_role_defined(role_id) {
                            return Err(ProgramBuildError::UndefinedRole { id: role_id, line });
                        }
                    }
                    if let Some(overrides) = payment.allocation.weight_overrides() {
                        for entry in overrides.entries() {
                            match &entry.target {
                                WeightOverrideTarget::Member(_) => {}
                                WeightOverrideTarget::Role(role_id) => {
                                    if !resolver.is_role_defined(*role_id) {
                                        return Err(ProgramBuildError::UndefinedRole {
                                            id: *role_id,
                                            line,
                                        });
                                    }
                                }
                                WeightOverrideTarget::Group(group_name) => {
                                    if !resolver.is_defined(group_name.as_str()) {
                                        return Err(ProgramBuildError::UndefinedGroup {
                                            name: Cow::Owned(group_name.to_string()),
                                            line,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }

            validated_statements.push(statement);
        }

        Ok(Self {
            members: member_ids.to_vec(),
            roles: role_members,
            statements: validated_statements,
        })
    }

    pub fn statements(&self) -> &[Statement<'a>] {
        &self.statements
    }

    pub fn calculate_balances(&self) -> Result<MemberBalances, BalanceError> {
        let mut accumulator = BalanceAccumulator::new_with_context(&self.members, self.roles);
        for stmt in &self.statements {
            accumulator.apply(stmt)?;
        }
        Ok(accumulator.into_balances())
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

    #[test]
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
        let weight_vec = ratios.into_iter().map(Weight).collect();
        let ratios = Ratios::try_new(weight_vec).expect("ratios must be non-empty and non-zero");
        let result = money.split_ratio(&ratios, RemainderPolicy::FrontLoad);
        let total = dec(amount);
        let sum = Decimal::from(ratios.total());
        for (share, &weight) in result.iter().zip(ratios.weights()) {
            let expected = total * Decimal::from(weight) / sum;
            assert_eq!(share.as_decimal(), expected);
        }
        assert_sum_within_epsilon(&result, dec(amount));
    }

    #[rstest]
    #[case::empty(Vec::new(), SplitError::EmptyRatios)]
    #[case::zero_sum(vec![Weight::ZERO, Weight::ZERO], SplitError::ZeroTotalRatio)]
    #[case::overflow(vec![Weight::MAX, Weight(1)], SplitError::WeightOverflow)]
    fn ratios_reject(#[case] weights: Vec<Weight>, #[case] expected: SplitError) {
        let result = Ratios::try_new(weights);
        assert_eq!(result, Err(expected));
    }

    #[rstest]
    #[case::display_name("Nickname")]
    fn member_info_effective_name_returns_display_name(#[case] display_name: &str) {
        let info = MemberInfo {
            id: MemberId(1),
            display_name: SmolStr::from(display_name),
            username: SmolStr::from("username"),
            avatar_url: None,
        };
        assert_eq!(info.effective_name(), display_name);
    }

    #[test]
    fn member_info_clone_preserves_fields() {
        let info1 = MemberInfo {
            id: MemberId(1),
            display_name: SmolStr::from("Test"),
            username: SmolStr::from("test"),
            avatar_url: None,
        };
        let info2 = info1.clone();

        assert_eq!(info1.display_name, info2.display_name);
        assert_eq!(info1.username, info2.username);
        assert_eq!(info1.avatar_url, info2.avatar_url);
    }

    #[test]
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

    #[test]
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

        let roles: FxHashMap<RoleId, u32> = FxHashMap::default();
        let mask = expr
            .evaluate_mask::<u32>(&index, &groups, &roles)
            .expect("expression should evaluate");
        let mut result = index.mask_to_set(mask).into_iter().collect::<Vec<_>>();
        result.sort_unstable();

        assert_eq!(result, vec![MemberId(1), MemberId(3)]);
    }

    #[test]
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
        let result = expr.evaluate(&|_| None, &|_| None);

        assert!(result.is_none());
    }

    #[rstest]
    #[case::payment_undefined_group(
        StatementWithLine {
            line: 1,
            statement: Statement::Payment(Payment::even(
                Money::try_from(100).expect("amount should fit in i64"),
                MemberSetExpr::new([MemberSetOp::PushGroup("team")]),
                MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
            )),
        },
        Err(ProgramBuildError::UndefinedGroup {
            name: Cow::Borrowed("team"),
            line: 1
        })
    )]
    #[case::declaration_undefined_group(
        StatementWithLine {
            line: 1,
            statement: Statement::Declaration(Declaration {
                name: "group_b",
                expression: MemberSetExpr::new([MemberSetOp::PushGroup("group_a")]),
            }),
        },
        Err(ProgramBuildError::UndefinedGroup {
            name: Cow::Borrowed("group_a"),
            line: 1
        })
    )]
    fn program_rejects_undefined_group_references(
        #[case] statement: StatementWithLine<'static>,
        #[case] expected: Result<(), ProgramBuildError<'static>>,
    ) {
        let actual = Program::try_new(vec![statement], &[]).map(|_| ());
        assert_eq!(actual, expected);
    }

    fn roles_with_role_10() -> RoleMembers {
        RoleMembers::from_iter([(RoleId(10), FxHashSet::from_iter([MemberId(1), MemberId(2)]))])
    }

    #[rstest]
    #[case::undefined_role(
        RoleMembers::default(),
        Err(ProgramBuildError::UndefinedRole {
            id: RoleId(10),
            line: 1
        })
    )]
    #[case::defined_role(roles_with_role_10(), Ok(()))]
    fn program_validates_role_references(
        #[case] roles: RoleMembers,
        #[case] expected: Result<(), ProgramBuildError<'static>>,
    ) {
        let statement = StatementWithLine {
            line: 1,
            statement: Statement::Payment(Payment::even(
                Money::try_from(100).expect("amount should fit in i64"),
                MemberSetExpr::new([MemberSetOp::PushRole(RoleId(10))]),
                MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
            )),
        };

        let actual = Program::try_new_with_roles(vec![statement], &[], &roles).map(|_| ());
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::undefined_group_override(
        vec![StatementWithLine {
            line: 1,
            statement: Statement::Payment(make_weighted_payment_with_overrides(
                100,
                WeightOverrides::new([WeightOverride::group("vip", Weight(2))]),
                vec![MemberSetOp::Push(MemberId(1))],
            )),
        }],
        empty_roles_ref(),
        Err(ProgramBuildError::UndefinedGroup {
            name: Cow::Owned("vip".to_string()),
            line: 1
        })
    )]
    #[case::undefined_role_override(
        vec![StatementWithLine {
            line: 1,
            statement: Statement::Payment(make_weighted_payment_with_overrides(
                100,
                WeightOverrides::new([WeightOverride::role(RoleId(10), Weight(2))]),
                vec![MemberSetOp::Push(MemberId(1))],
            )),
        }],
        empty_roles_ref(),
        Err(ProgramBuildError::UndefinedRole {
            id: RoleId(10),
            line: 1
        })
    )]
    #[case::defined_group_override(
        vec![
            StatementWithLine {
                line: 1,
                statement: Statement::Declaration(Declaration {
                    name: "vip",
                    expression: MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                }),
            },
            StatementWithLine {
                line: 2,
                statement: Statement::Payment(make_weighted_payment_with_overrides(
                    100,
                    WeightOverrides::new([WeightOverride::group("vip", Weight(2))]),
                    vec![MemberSetOp::Push(MemberId(1))],
                )),
            },
        ],
        empty_roles_ref(),
        Ok(())
    )]
    #[case::defined_role_override(
        vec![StatementWithLine {
            line: 1,
            statement: Statement::Payment(make_weighted_payment_with_overrides(
                100,
                WeightOverrides::new([WeightOverride::role(RoleId(10), Weight(2))]),
                vec![MemberSetOp::Push(MemberId(1))],
            )),
        }],
        roles_with_members_1_2(),
        Ok(())
    )]
    fn program_validates_weight_override_references(
        #[case] statements: Vec<StatementWithLine<'static>>,
        #[case] roles: &'static RoleMembers,
        #[case] expected: Result<(), ProgramBuildError<'static>>,
    ) {
        let actual = Program::try_new_with_roles(statements, &[], roles).map(|_| ());
        assert_eq!(actual, expected);
    }

    #[test]
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
            statement: Statement::Payment(Payment::even(
                Money::try_from(120).expect("amount should fit in i64"),
                MemberSetExpr::new([MemberSetOp::PushGroup("group_a")]),
                MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
            )),
        };

        let result = Program::try_new(vec![declaration, payment], &[]);
        let program = result.expect("program should accept declaration before reference");
        assert_eq!(program.statements().len(), 2);
    }

    fn make_weighted_payment(
        amount: i64,
        weights: BTreeMap<MemberId, Weight>,
        payee_ops: Vec<MemberSetOp<'static>>,
    ) -> Payment<'static> {
        Payment::weighted(
            Money::from_i64(amount),
            MemberSetExpr::new([MemberSetOp::Push(MemberId(10))]),
            MemberSetExpr::new(payee_ops),
            weights,
        )
    }

    fn make_weighted_payment_with_overrides(
        amount: i64,
        overrides: WeightOverrides,
        payee_ops: Vec<MemberSetOp<'static>>,
    ) -> Payment<'static> {
        Payment::weighted_with_overrides(
            Money::from_i64(amount),
            MemberSetExpr::new([MemberSetOp::Push(MemberId(10))]),
            MemberSetExpr::new(payee_ops),
            overrides,
        )
    }

    fn calculate_payment_balances(payment: Payment<'static>) -> MemberBalances {
        let program = Program::try_new(
            vec![StatementWithLine {
                line: 1,
                statement: Statement::Payment(payment),
            }],
            &[],
        )
        .expect("program should build");
        program
            .calculate_balances()
            .expect("balance calculation should succeed")
    }

    fn calculate_statement_balances_with_roles<'a>(
        statements: Vec<StatementWithLine<'a>>,
        roles: &'a RoleMembers,
    ) -> Result<MemberBalances, BalanceError> {
        let program =
            Program::try_new_with_roles(statements, &[], roles).expect("program should build");
        program.calculate_balances()
    }

    fn apply_single_statement_with_roles<'a>(
        statement: Statement<'a>,
        roles: &'a RoleMembers,
    ) -> Result<MemberBalances, BalanceError> {
        let mut accumulator = BalanceAccumulator::new_with_context(&[], roles);
        accumulator.apply(&statement)?;
        Ok(accumulator.into_balances())
    }

    fn empty_member_set_expr(member_id: MemberId) -> MemberSetExpr<'static> {
        MemberSetExpr::new([
            MemberSetOp::Push(member_id),
            MemberSetOp::Push(member_id),
            MemberSetOp::Difference,
        ])
    }

    fn empty_roles_ref() -> &'static RoleMembers {
        use std::sync::OnceLock;
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(RoleMembers::default)
    }

    fn roles_with_members_1_2() -> &'static RoleMembers {
        use std::sync::OnceLock;
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(|| {
            RoleMembers::from_iter([(RoleId(10), FxHashSet::from_iter([MemberId(1), MemberId(2)]))])
        })
    }

    #[rstest]
    #[case::undefined_group_override(
        Statement::Payment(make_weighted_payment_with_overrides(
            100,
            WeightOverrides::new([WeightOverride::group("vip", Weight(2))]),
            vec![MemberSetOp::Push(MemberId(1))],
        )),
        empty_roles_ref(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::ZERO),
            (MemberId(10), Money::ZERO),
        ]))
    )]
    #[case::undefined_role_override(
        Statement::Payment(make_weighted_payment_with_overrides(
            100,
            WeightOverrides::new([WeightOverride::role(RoleId(10), Weight(2))]),
            vec![MemberSetOp::Push(MemberId(1))],
        )),
        empty_roles_ref(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::ZERO),
            (MemberId(10), Money::ZERO),
        ]))
    )]
    fn balance_accumulator_apply_skips_payments_with_unresolved_weight_overrides(
        #[case] statement: Statement<'static>,
        #[case] roles: &'static RoleMembers,
        #[case] expected: Result<MemberBalances, BalanceError>,
    ) {
        let actual = apply_single_statement_with_roles(statement, roles);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::empty_payee(
        Statement::Payment(Payment::even(
            Money::from_i64(100),
            MemberSetExpr::new([MemberSetOp::Push(MemberId(10))]),
            empty_member_set_expr(MemberId(1)),
        )),
        Ok(MemberBalances::from([
            (MemberId(1), Money::ZERO),
            (MemberId(10), Money::ZERO),
        ]))
    )]
    #[case::empty_payer(
        Statement::Payment(Payment::even(
            Money::from_i64(100),
            empty_member_set_expr(MemberId(10)),
            MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
        )),
        Ok(MemberBalances::from([
            (MemberId(1), Money::ZERO),
            (MemberId(10), Money::ZERO),
        ]))
    )]
    fn balance_accumulator_apply_skips_payments_with_empty_payer_or_payee(
        #[case] statement: Statement<'static>,
        #[case] expected: Result<MemberBalances, BalanceError>,
    ) {
        let actual = apply_single_statement_with_roles(statement, empty_roles_ref());
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::even_empty_payee(
        vec![StatementWithLine {
            line: 1,
            statement: Statement::Payment(Payment::even(
                Money::from_i64(100),
                MemberSetExpr::new([MemberSetOp::Push(MemberId(10))]),
                empty_member_set_expr(MemberId(1)),
            )),
        }],
        empty_roles_ref(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::ZERO),
            (MemberId(10), Money::ZERO),
        ]))
    )]
    #[case::even_empty_payer(
        vec![StatementWithLine {
            line: 1,
            statement: Statement::Payment(Payment::even(
                Money::from_i64(100),
                empty_member_set_expr(MemberId(10)),
                MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
            )),
        }],
        empty_roles_ref(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::ZERO),
            (MemberId(10), Money::ZERO),
        ]))
    )]
    #[case::weighted_empty_payee(
        vec![StatementWithLine {
            line: 1,
            statement: Statement::Payment(Payment::weighted_with_overrides(
                Money::from_i64(100),
                MemberSetExpr::new([MemberSetOp::Push(MemberId(10))]),
                empty_member_set_expr(MemberId(1)),
                WeightOverrides::new([WeightOverride::member(MemberId(1), Weight(2))]),
            )),
        }],
        empty_roles_ref(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::ZERO),
            (MemberId(10), Money::ZERO),
        ]))
    )]
    #[case::weighted_empty_payer(
        vec![StatementWithLine {
            line: 1,
            statement: Statement::Payment(Payment::weighted_with_overrides(
                Money::from_i64(100),
                empty_member_set_expr(MemberId(10)),
                MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                WeightOverrides::new([WeightOverride::member(MemberId(1), Weight(2))]),
            )),
        }],
        empty_roles_ref(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::ZERO),
            (MemberId(10), Money::ZERO),
        ]))
    )]
    fn calculate_balances_skips_payments_with_empty_payer_or_payee(
        #[case] statements: Vec<StatementWithLine<'static>>,
        #[case] roles: &'static RoleMembers,
        #[case] expected: Result<MemberBalances, BalanceError>,
    ) {
        let actual = calculate_statement_balances_with_roles(statements, roles);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::zero_weight_member(
        3000,
        BTreeMap::from([(MemberId(1), Weight(2)), (MemberId(2), Weight::ZERO)]),
        vec![
            MemberSetOp::Push(MemberId(1)),
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Union,
            MemberSetOp::Push(MemberId(3)),
            MemberSetOp::Union,
        ],
        Money::from_i64(-2000), // A: 3000 * 2/3
        Money::from_i64(0),      // B: weight 0
        Money::from_i64(-1000),  // C: 3000 * 1/3 (default weight)
    )]
    #[case::proportional_3_2_1(
        6000,
        BTreeMap::from([(MemberId(1), Weight(3)), (MemberId(2), Weight(2)), (MemberId(3), Weight(1))]),
        vec![
            MemberSetOp::Push(MemberId(1)),
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Union,
            MemberSetOp::Push(MemberId(3)),
            MemberSetOp::Union,
        ],
        Money::from_i64(-3000), // A: 6000 * 3/6
        Money::from_i64(-2000), // B: 6000 * 2/6
        Money::from_i64(-1000), // C: 6000 * 1/6
    )]
    #[case::default_weight_unweighted(
        4000,
        BTreeMap::from([(MemberId(1), Weight(2))]),
        vec![
            MemberSetOp::Push(MemberId(1)),
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Union,
            MemberSetOp::Push(MemberId(3)),
            MemberSetOp::Union,
        ],
        Money::from_i64(-2000), // A: 4000 * 2/4
        Money::from_i64(-1000), // B: 4000 * 1/4 (default weight)
        Money::from_i64(-1000), // C: 4000 * 1/4 (default weight)
    )]
    #[case::empty_weights_equals_even(
        3000,
        BTreeMap::new(), // Empty weights = even distribution
        vec![
            MemberSetOp::Push(MemberId(1)),
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Union,
            MemberSetOp::Push(MemberId(3)),
            MemberSetOp::Union,
        ],
        Money::from_i64(-1000), // A: 3000 / 3
        Money::from_i64(-1000), // B: 3000 / 3
        Money::from_i64(-1000), // C: 3000 / 3
    )]
    #[case::all_weights_equal_one(
        3000,
        BTreeMap::from([(MemberId(1), Weight(1)), (MemberId(2), Weight(1)), (MemberId(3), Weight(1))]),
        vec![
            MemberSetOp::Push(MemberId(1)),
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Union,
            MemberSetOp::Push(MemberId(3)),
            MemberSetOp::Union,
        ],
        Money::from_i64(-1000), // A: 3000 / 3
        Money::from_i64(-1000), // B: 3000 / 3
        Money::from_i64(-1000), // C: 3000 / 3
    )]
    #[case::weight_for_non_payee_ignored(
        3000,
        BTreeMap::from([(MemberId(1), Weight(2)), (MemberId(99), Weight(5))]), // MemberId(99) not in payee set
        vec![
            MemberSetOp::Push(MemberId(1)),
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Union,
        ],
        Money::from_i64(-2000), // A: 3000 * 2/3 (weight 2, total weight = 2+1=3)
        Money::from_i64(-1000), // B: 3000 * 1/3 (default weight 1)
        Money::ZERO,            // C: not in payee set
    )]
    fn weighted_distribution_proportional(
        #[case] amount: i64,
        #[case] weights: BTreeMap<MemberId, Weight>,
        #[case] payee_ops: Vec<MemberSetOp<'static>>,
        #[case] expected_a: Money,
        #[case] expected_b: Money,
        #[case] expected_c: Money,
    ) {
        let payment = make_weighted_payment(amount, weights, payee_ops);
        let balances = calculate_payment_balances(payment);

        assert_eq!(balances.get(&MemberId(1)), Some(&expected_a));
        assert_eq!(balances.get(&MemberId(2)), Some(&expected_b));
        // For cases where C is not in payee set, check it's not in balances or is zero
        if expected_c != Money::ZERO {
            assert_eq!(balances.get(&MemberId(3)), Some(&expected_c));
        }
    }

    #[rstest]
    #[case::last_weight_wins(
        3000,
        BTreeMap::from([(MemberId(1), Weight(3))]),
        vec![MemberSetOp::Push(MemberId(1)), MemberSetOp::Push(MemberId(1)), MemberSetOp::Union],
        Money::from_i64(-3000), // A receives all (only member)
    )]
    fn weighted_distribution_single_member(
        #[case] amount: i64,
        #[case] weights: BTreeMap<MemberId, Weight>,
        #[case] payee_ops: Vec<MemberSetOp<'static>>,
        #[case] expected_a: Money,
    ) {
        let payment = make_weighted_payment(amount, weights, payee_ops);
        let balances = calculate_payment_balances(payment);

        assert_eq!(balances.get(&MemberId(1)), Some(&expected_a));
    }

    #[rstest]
    #[case::excluded_member(
        1000,
        BTreeMap::from([(MemberId(1), Weight(2))]),
        vec![
            MemberSetOp::Push(MemberId(1)),
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Union,
            MemberSetOp::Push(MemberId(1)),
            MemberSetOp::Difference,
        ],
        MemberId(2),
        Money::from_i64(-1000), // B receives all (A excluded)
    )]
    fn weighted_distribution_excluded_member(
        #[case] amount: i64,
        #[case] weights: BTreeMap<MemberId, Weight>,
        #[case] payee_ops: Vec<MemberSetOp<'static>>,
        #[case] recipient: MemberId,
        #[case] expected: Money,
    ) {
        let payment = make_weighted_payment(amount, weights, payee_ops);
        let balances = calculate_payment_balances(payment);

        assert_eq!(balances.get(&recipient), Some(&expected));
    }

    #[rstest]
    #[case::zero_total_weight(
        1000,
        BTreeMap::from([(MemberId(1), Weight::ZERO)]),
        vec![
            MemberSetOp::Push(MemberId(1)),
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Union,
            MemberSetOp::Push(MemberId(2)),
            MemberSetOp::Difference,
        ],
    )]
    fn weighted_distribution_zero_total_weight_returns_error(
        #[case] amount: i64,
        #[case] weights: BTreeMap<MemberId, Weight>,
        #[case] payee_ops: Vec<MemberSetOp<'static>>,
    ) {
        let payment = make_weighted_payment(amount, weights, payee_ops);
        let program = Program::try_new(
            vec![StatementWithLine {
                line: 1,
                statement: Statement::Payment(payment),
            }],
            &[],
        )
        .expect("program should build");
        let result = program.calculate_balances();
        assert_eq!(result, Err(BalanceError::ZeroTotalWeight));
    }

    #[rstest]
    #[case::group_override(
        vec![
            StatementWithLine {
                line: 1,
                statement: Statement::Declaration(Declaration {
                    name: "vip",
                    expression: MemberSetExpr::new([
                        MemberSetOp::Push(MemberId(1)),
                        MemberSetOp::Push(MemberId(2)),
                        MemberSetOp::Union,
                    ]),
                }),
            },
            StatementWithLine {
                line: 2,
                statement: Statement::Payment(make_weighted_payment_with_overrides(
                    5000,
                    WeightOverrides::new([
                        WeightOverride::group("vip", Weight(2)),
                    ]),
                    vec![
                        MemberSetOp::Push(MemberId(1)),
                        MemberSetOp::Push(MemberId(2)),
                        MemberSetOp::Union,
                        MemberSetOp::Push(MemberId(3)),
                        MemberSetOp::Union,
                    ],
                )),
            },
        ],
        empty_roles_ref(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::from_i64(-2000)),
            (MemberId(2), Money::from_i64(-2000)),
            (MemberId(3), Money::from_i64(-1000)),
            (MemberId(10), Money::from_i64(5000)),
        ]))
    )]
    #[case::role_override(
        vec![
            StatementWithLine {
                line: 1,
                statement: Statement::Payment(make_weighted_payment_with_overrides(
                    5000,
                    WeightOverrides::new([
                        WeightOverride::role(RoleId(10), Weight(2)),
                    ]),
                    vec![
                        MemberSetOp::Push(MemberId(1)),
                        MemberSetOp::Push(MemberId(2)),
                        MemberSetOp::Union,
                        MemberSetOp::Push(MemberId(3)),
                        MemberSetOp::Union,
                    ],
                )),
            },
        ],
        roles_with_members_1_2(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::from_i64(-2000)),
            (MemberId(2), Money::from_i64(-2000)),
            (MemberId(3), Money::from_i64(-1000)),
            (MemberId(10), Money::from_i64(5000)),
        ]))
    )]
    #[case::later_member_override_wins_over_role(
        vec![
            StatementWithLine {
                line: 1,
                statement: Statement::Payment(make_weighted_payment_with_overrides(
                    8000,
                    WeightOverrides::new([
                        WeightOverride::role(RoleId(10), Weight(2)),
                        WeightOverride::member(MemberId(1), Weight(5)),
                    ]),
                    vec![
                        MemberSetOp::Push(MemberId(1)),
                        MemberSetOp::Push(MemberId(2)),
                        MemberSetOp::Union,
                        MemberSetOp::Push(MemberId(3)),
                        MemberSetOp::Union,
                    ],
                )),
            },
        ],
        roles_with_members_1_2(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::from_i64(-5000)),
            (MemberId(2), Money::from_i64(-2000)),
            (MemberId(3), Money::from_i64(-1000)),
            (MemberId(10), Money::from_i64(8000)),
        ]))
    )]
    #[case::later_role_override_wins_over_member(
        vec![
            StatementWithLine {
                line: 1,
                statement: Statement::Payment(make_weighted_payment_with_overrides(
                    5000,
                    WeightOverrides::new([
                        WeightOverride::member(MemberId(1), Weight(5)),
                        WeightOverride::role(RoleId(10), Weight(2)),
                    ]),
                    vec![
                        MemberSetOp::Push(MemberId(1)),
                        MemberSetOp::Push(MemberId(2)),
                        MemberSetOp::Union,
                        MemberSetOp::Push(MemberId(3)),
                        MemberSetOp::Union,
                    ],
                )),
            },
        ],
        roles_with_members_1_2(),
        Ok(MemberBalances::from([
            (MemberId(1), Money::from_i64(-2000)),
            (MemberId(2), Money::from_i64(-2000)),
            (MemberId(3), Money::from_i64(-1000)),
            (MemberId(10), Money::from_i64(5000)),
        ]))
    )]
    fn weighted_distribution_supports_runtime_resolved_overrides(
        #[case] statements: Vec<StatementWithLine<'static>>,
        #[case] roles: &'static RoleMembers,
        #[case] expected: Result<MemberBalances, BalanceError>,
    ) {
        let actual = calculate_statement_balances_with_roles(statements, roles);
        assert_eq!(actual, expected);
    }
}
