use fxhash::{FxHashMap, FxHashSet};
use std::{
    fmt,
    ops::{Add, AddAssign, Neg, Sub, SubAssign},
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
    // members field removed - MEMBERS declaration no longer required
    statements: Vec<Statement<'a>>,
}

pub struct BalanceAccumulator<'a> {
    balances: FxHashMap<MemberId, Money>,
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
    ops: Vec<MemberSetOp<'a>>,
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

impl<'a> MemberSetExpr<'a> {
    pub fn new(ops: Vec<MemberSetOp<'a>>) -> Self {
        Self { ops }
    }

    /// Evaluate the expression to produce a set of member IDs
    ///
    /// # Arguments
    /// * `member_resolver` - Resolves a group name to a borrowed set of member IDs
    pub fn evaluate<'r, F>(&self, member_resolver: &F) -> Option<FxHashSet<MemberId>>
    where
        F: Fn(&str) -> Option<&'r FxHashSet<MemberId>>,
    {
        let mut stack: Vec<FxHashSet<MemberId>> = Vec::with_capacity(self.ops.len());

        for op in &self.ops {
            match op {
                MemberSetOp::Push(id) => {
                    // Direct member reference - create singleton set
                    let mut set = FxHashSet::default();
                    set.insert(*id);
                    stack.push(set);
                }
                MemberSetOp::PushGroup(name) => {
                    // Group reference - resolve through resolver and clone only when needed
                    let set = member_resolver(name)?;
                    stack.push(set.clone());
                }
                MemberSetOp::Union => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(a.union(&b).copied().collect());
                }
                MemberSetOp::Intersection => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(a.intersection(&b).copied().collect());
                }
                MemberSetOp::Difference => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(a.difference(&b).copied().collect());
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
    pub fn try_new(statements: Vec<StatementWithLine<'a>>) -> Result<Self, ProgramBuildError<'a>> {
        // MEMBERS declaration no longer required
        // Collect all unique member IDs from statements
        let mut validated_statements = Vec::with_capacity(statements.len());
        let mut resolver = MemberSetResolver::new();

        for StatementWithLine { line, statement } in statements {
            match &statement {
                Statement::Declaration(decl) => {
                    // Check that all referenced groups are defined
                    for group_name in decl.expression.referenced_groups() {
                        if !resolver.is_group_defined(group_name) {
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
                        if !resolver.is_group_defined(group_name) {
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
            statements: validated_statements,
        })
    }

    pub fn statements(&self) -> &[Statement<'a>] {
        &self.statements
    }

    pub fn calculate_balances(&self) -> FxHashMap<MemberId, Money> {
        let mut accumulator = BalanceAccumulator::new();
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
        let balances: FxHashMap<MemberId, Money> = FxHashMap::default();
        let resolver = MemberSetResolver::new();

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

    pub fn balances(&self) -> &FxHashMap<MemberId, Money> {
        &self.balances
    }

    pub fn into_balances(self) -> FxHashMap<MemberId, Money> {
        self.balances
    }

    pub fn set_balances(&mut self, balances: FxHashMap<MemberId, Money>) {
        self.balances = balances;
    }

    pub fn ensure_members<I>(&mut self, members: I)
    where
        I: IntoIterator<Item = MemberId>,
    {
        for member_id in members {
            self.balances.entry(member_id).or_insert(Money::zero());
        }
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
    pub new_balances: FxHashMap<MemberId, Money>,
    pub transfers: Vec<Transfer>,
}

pub fn distribute_balances(
    balances: &mut FxHashMap<MemberId, Money>,
    members: &MemberSet,
    amount: Money,
    direction: i64,
) {
    if members.is_empty() {
        return;
    }

    let member_count = members.members().len() as i64;
    let total_amount = amount.amount();
    let base = total_amount / member_count;
    let remainder = (total_amount % member_count).unsigned_abs() as usize;

    for (idx, member) in members.iter().enumerate() {
        let mut share = base;
        if idx < remainder {
            share += 1;
        }
        let signed = share * direction;
        *balances.entry(member).or_insert(Money::zero()) += Money::from_i64(signed);
    }
}
