use fxhash::FxHashSet;
use smallvec::SmallVec;
use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt,
    ops::{Add, AddAssign, Neg, Sub, SubAssign},
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
    FrontLoad,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AmountExpr {
    ops: SmallVec<[AmountOp; 1]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AmountOp {
    Literal(u64),
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AmountError {
    Overflow,
    DivisionByZero,
    NonIntegerDivision,
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
pub struct Money(i64);

impl Money {
    pub fn zero() -> Self {
        Self(0)
    }

    pub fn from_i64(value: i64) -> Self {
        Self(value)
    }

    pub fn from_u64(value: u64) -> Self {
        debug_assert!(value <= i64::MAX as u64, "Money::from_u64 overflow");
        Self(value as i64)
    }

    pub fn amount(self) -> i64 {
        self.0
    }

    pub fn abs(self) -> i64 {
        self.0.abs()
    }

    pub fn is_zero(self) -> bool {
        self.0 == 0
    }

    pub fn signum(self) -> i64 {
        self.0.signum()
    }

    /// Split amount evenly among `n` recipients; remainder is distributed per policy
    pub fn split_even(self, n: usize, policy: RemainderPolicy) -> Result<Vec<Self>, SplitError> {
        if n == 0 {
            return Err(SplitError::ZeroRecipients);
        }

        let total = self.0;
        let divisor = n as i64;
        let base = total / divisor;
        let remainder = total - base * divisor;

        let mut result = vec![Self(base); n];
        apply_remainder(policy, &mut result, remainder);
        Ok(result)
    }

    /// Split amount by `ratios`; remainder is distributed per policy from the front
    pub fn split_ratio(self, ratios: &Ratios, policy: RemainderPolicy) -> Vec<Self> {
        let total = self.0 as i128;
        let sum = ratios.total() as i128;

        let mut allocated: Vec<Self> = ratios
            .weights()
            .iter()
            .map(|&w| Self((total * w as i128 / sum) as i64))
            .collect();

        let distributed: i64 = allocated.iter().map(|m| m.0).sum();
        let remainder = self.0 - distributed;
        apply_remainder(policy, &mut allocated, remainder);

        allocated
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
        let value = self.evaluate_i128()?;
        if value > i64::MAX as i128 || value < i64::MIN as i128 {
            return Err(AmountError::Overflow);
        }
        Ok(Money::from_i64(value as i64))
    }

    fn evaluate_i128(&self) -> Result<i128, AmountError> {
        let mut stack: Vec<i128> = Vec::with_capacity(self.ops.len());
        for op in &self.ops {
            match op {
                AmountOp::Literal(value) => stack.push(*value as i128),
                AmountOp::Add => apply_binary(&mut stack, checked_add)?,
                AmountOp::Sub => apply_binary(&mut stack, checked_sub)?,
                AmountOp::Mul => apply_binary(&mut stack, checked_mul)?,
                AmountOp::Div => apply_binary(&mut stack, checked_div)?,
            }
        }

        if stack.len() == 1 {
            Ok(stack.pop().unwrap_or(0))
        } else {
            Err(AmountError::Overflow)
        }
    }
}

fn apply_binary(
    stack: &mut Vec<i128>,
    op: fn(i128, i128) -> Result<i128, AmountError>,
) -> Result<(), AmountError> {
    let rhs = stack.pop().ok_or(AmountError::Overflow)?;
    let lhs = stack.pop().ok_or(AmountError::Overflow)?;
    stack.push(op(lhs, rhs)?);
    Ok(())
}

fn checked_add(lhs: i128, rhs: i128) -> Result<i128, AmountError> {
    lhs.checked_add(rhs).ok_or(AmountError::Overflow)
}

fn checked_sub(lhs: i128, rhs: i128) -> Result<i128, AmountError> {
    lhs.checked_sub(rhs).ok_or(AmountError::Overflow)
}

fn checked_mul(lhs: i128, rhs: i128) -> Result<i128, AmountError> {
    lhs.checked_mul(rhs).ok_or(AmountError::Overflow)
}

fn checked_div(lhs: i128, rhs: i128) -> Result<i128, AmountError> {
    if rhs == 0 {
        return Err(AmountError::DivisionByZero);
    }
    let remainder = lhs.checked_rem(rhs).ok_or(AmountError::Overflow)?;
    if remainder != 0 {
        return Err(AmountError::NonIntegerDivision);
    }
    lhs.checked_div(rhs).ok_or(AmountError::Overflow)
}

impl fmt::Display for AmountError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AmountError::Overflow => write!(f, "amount is out of range"),
            AmountError::DivisionByZero => write!(f, "division by zero"),
            AmountError::NonIntegerDivision => write!(f, "division must be exact"),
        }
    }
}

fn apply_remainder(policy: RemainderPolicy, slots: &mut [Money], remainder: i64) {
    if remainder == 0 {
        return;
    }

    let bump = remainder.signum();
    let count = remainder.unsigned_abs() as usize;

    match policy {
        RemainderPolicy::FrontLoad => {
            for idx in 0..count {
                let slot = idx % slots.len();
                slots[slot].0 += bump;
            }
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

/// Discord user ID
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MemberId(pub u64);

pub type MemberBalances = BTreeMap<MemberId, Money>;

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
    use rstest::rstest;

    #[rstest]
    #[case::literal(AmountExpr::new(vec![AmountOp::Literal(100)]), 100)]
    #[case::add(
        AmountExpr::new(vec![AmountOp::Literal(100), AmountOp::Literal(50), AmountOp::Add]),
        150
    )]
    #[case::mul(
        AmountExpr::new(vec![AmountOp::Literal(20), AmountOp::Literal(5), AmountOp::Mul]),
        100
    )]
    #[case::mixed(
        AmountExpr::new(vec![
            AmountOp::Literal(100),
            AmountOp::Literal(200),
            AmountOp::Literal(3),
            AmountOp::Mul,
            AmountOp::Add,
        ]),
        700
    )]
    fn amount_expr_evaluates(#[case] expr: AmountExpr, #[case] expected: i64) {
        let money = expr.evaluate().expect("amount expr should evaluate");
        assert_eq!(money.amount(), expected);
    }

    #[rstest]
    #[case::division_by_zero(AmountExpr::new(vec![
        AmountOp::Literal(10),
        AmountOp::Literal(0),
        AmountOp::Div,
    ]))]
    #[case::non_integer_division(AmountExpr::new(vec![
        AmountOp::Literal(10),
        AmountOp::Literal(3),
        AmountOp::Div,
    ]))]
    fn amount_expr_rejects_invalid(#[case] expr: AmountExpr) {
        let result = expr.evaluate();
        assert!(result.is_err());
    }

    #[rstest]
    #[case::negative_result(
        AmountExpr::new(vec![AmountOp::Literal(10), AmountOp::Literal(20), AmountOp::Sub]),
        -10
    )]
    fn amount_expr_allows_negative(#[case] expr: AmountExpr, #[case] expected: i64) {
        let money = expr.evaluate().expect("amount expr should allow negative");
        assert_eq!(money.amount(), expected);
    }

    #[rstest]
    #[case::even_3(100, 3, vec![34, 33, 33])]
    #[case::even_2(100, 2, vec![50, 50])]
    #[case::even_1(100, 1, vec![100])]
    #[case::no_remainder(99, 3, vec![33, 33, 33])]
    #[case::zero_amount(0, 3, vec![0, 0, 0])]
    fn split_even_distributes_correctly(
        #[case] amount: i64,
        #[case] n: usize,
        #[case] expected: Vec<i64>,
    ) {
        let money = Money::from_i64(amount);
        let result = money
            .split_even(n, RemainderPolicy::FrontLoad)
            .expect("non-zero recipient count");
        let amounts: Vec<i64> = result.iter().map(|m| m.amount()).collect();
        assert_eq!(amounts, expected);
        assert_eq!(
            result.iter().map(|m| m.amount()).sum::<i64>(),
            amount,
            "sum must equal original"
        );
    }

    #[rstest]
    fn split_even_zero_count_rejects() {
        let result = Money::from_i64(100).split_even(0, RemainderPolicy::FrontLoad);
        assert_eq!(result, Err(SplitError::ZeroRecipients));
    }

    #[rstest]
    #[case::negative_even(-100, 3, vec![-34, -33, -33])]
    #[case::negative_no_remainder(-99, 3, vec![-33, -33, -33])]
    fn split_even_negative(#[case] amount: i64, #[case] n: usize, #[case] expected: Vec<i64>) {
        let money = Money::from_i64(amount);
        let result = money
            .split_even(n, RemainderPolicy::FrontLoad)
            .expect("non-zero recipient count");
        let amounts: Vec<i64> = result.iter().map(|m| m.amount()).collect();
        assert_eq!(amounts, expected);
        assert_eq!(result.iter().map(|m| m.amount()).sum::<i64>(), amount);
    }

    #[rstest]
    #[case::ratio_2_1(100, vec![2, 1], vec![67, 33])]
    #[case::ratio_1_1(100, vec![1, 1], vec![50, 50])]
    #[case::ratio_1_1_1(100, vec![1, 1, 1], vec![34, 33, 33])]
    #[case::ratio_3_1(100, vec![3, 1], vec![75, 25])]
    fn split_ratio_distributes_correctly(
        #[case] amount: i64,
        #[case] ratios: Vec<u64>,
        #[case] expected: Vec<i64>,
    ) {
        let money = Money::from_i64(amount);
        let ratios = Ratios::try_new(ratios).expect("ratios must be non-empty and non-zero");
        let result = money.split_ratio(&ratios, RemainderPolicy::FrontLoad);
        let amounts: Vec<i64> = result.iter().map(|m| m.amount()).collect();
        assert_eq!(amounts, expected);
        assert_eq!(result.iter().map(|m| m.amount()).sum::<i64>(), amount);
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
    #[case::payment_undefined_group(
        StatementWithLine {
            line: 1,
            statement: Statement::Payment(Payment {
                amount: Money::from_u64(100),
                payer: MemberSetExpr::new(vec![MemberSetOp::PushGroup("team")]),
                payee: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(1))]),
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
                expression: MemberSetExpr::new(vec![MemberSetOp::PushGroup("group_a")]),
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
                expression: MemberSetExpr::new(vec![
                    MemberSetOp::Push(MemberId(1)),
                    MemberSetOp::Push(MemberId(2)),
                    MemberSetOp::Union,
                ]),
            }),
        };
        let payment = StatementWithLine {
            line: 2,
            statement: Statement::Payment(Payment {
                amount: Money::from_u64(120),
                payer: MemberSetExpr::new(vec![MemberSetOp::PushGroup("group_a")]),
                payee: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(3))]),
            }),
        };

        let result = Program::try_new(vec![declaration, payment], &[]);

        assert!(result.is_ok());
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
        let mut stack: Vec<Cow<'r, FxHashSet<MemberId>>> = Vec::with_capacity(self.ops.len());

        for op in &self.ops {
            match op {
                MemberSetOp::Push(id) => {
                    // Direct member reference - create singleton set
                    let mut set = FxHashSet::default();
                    set.insert(*id);
                    stack.push(Cow::Owned(set));
                }
                MemberSetOp::PushGroup(name) => {
                    let set = member_resolver(name)?;
                    stack.push(Cow::Borrowed(set));
                }
                MemberSetOp::Union => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    let merged = a.union(b.as_ref()).copied().collect();
                    stack.push(Cow::Owned(merged));
                }
                MemberSetOp::Intersection => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    let merged = a.intersection(b.as_ref()).copied().collect();
                    stack.push(Cow::Owned(merged));
                }
                MemberSetOp::Difference => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    let merged = a.difference(b.as_ref()).copied().collect();
                    stack.push(Cow::Owned(merged));
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
                    self.balances.entry(member_id).or_insert(Money::zero());
                }
                let Some(members_vec) = self.resolver.evaluate_members(&decl.expression) else {
                    return;
                };
                for member in members_vec.iter() {
                    self.balances.entry(member).or_insert(Money::zero());
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
                    self.balances.entry(member_id).or_insert(Money::zero());
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

pub fn distribute_balances(
    balances: &mut MemberBalances,
    members: &MemberSet,
    amount: Money,
    direction: i64,
) {
    if members.is_empty() {
        return;
    }

    let shares = match amount.split_even(members.members().len(), RemainderPolicy::FrontLoad) {
        Ok(shares) => shares,
        Err(_) => return,
    };

    for (member, share) in members.iter().zip(shares) {
        let signed = Money::from_i64(share.amount() * direction);
        *balances.entry(member).or_insert(Money::zero()) += signed;
    }
}
