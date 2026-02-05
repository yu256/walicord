use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
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
    members: &'a [&'a str],
    statements: Vec<Statement<'a>>,
}

pub struct BalanceAccumulator<'a> {
    balances: HashMap<&'a str, Money>,
    resolver: MemberSetResolver<'a>,
}

pub struct StatementWithLine<'a> {
    pub line: usize,
    pub statement: Statement<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProgramBuildError<'a> {
    MissingMembersDeclaration,
    UndefinedMember { name: &'a str, line: usize },
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MemberSetOp<'a> {
    Push(&'a str),
    Union,
    Intersection,
    Difference,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MemberSetExpr<'a> {
    ops: Vec<MemberSetOp<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemberSet<'a> {
    members: Vec<&'a str>,
}

impl<'a> MemberSet<'a> {
    pub fn new(members: Vec<&'a str>) -> Self {
        Self { members }
    }

    pub fn members(&self) -> &[&'a str] {
        &self.members
    }

    pub fn iter(&self) -> impl Iterator<Item = &'a str> + '_ {
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

    pub fn evaluate<'b, F>(&self, resolver: &F) -> Option<Cow<'b, HashSet<&'a str>>>
    where
        'a: 'b,
        F: Fn(&str) -> Option<&'b HashSet<&'a str>>,
    {
        let mut stack: Vec<Cow<'b, HashSet<&'a str>>> = Vec::with_capacity(self.ops.len());

        for op in &self.ops {
            match op {
                MemberSetOp::Push(name) => {
                    let set = resolver(name)?;
                    stack.push(Cow::Borrowed(set));
                }
                MemberSetOp::Union => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(Cow::Owned(a.union(&b).copied().collect()));
                }
                MemberSetOp::Intersection => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(Cow::Owned(a.intersection(&b).copied().collect()));
                }
                MemberSetOp::Difference => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(Cow::Owned(a.difference(&b).copied().collect()));
                }
            }
        }

        if stack.len() == 1 { stack.pop() } else { None }
    }

    pub fn referenced_names(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.ops.iter().filter_map(|op| match op {
            MemberSetOp::Push(name) => Some(*name),
            _ => None,
        })
    }
}

impl<'a> Program<'a> {
    pub fn try_new(
        members: &'a [&'a str],
        statements: Vec<StatementWithLine<'a>>,
    ) -> Result<Self, ProgramBuildError<'a>> {
        if members.is_empty() {
            return Err(ProgramBuildError::MissingMembersDeclaration);
        }

        let mut resolver = MemberSetResolver::new(members);
        let mut validated_statements = Vec::with_capacity(statements.len());

        for StatementWithLine { line, statement } in statements {
            match &statement {
                Statement::Declaration(decl) => {
                    for name in decl.expression.referenced_names() {
                        if !resolver.is_defined(name) {
                            return Err(ProgramBuildError::UndefinedMember { name, line });
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
                    for name in payment
                        .payer
                        .referenced_names()
                        .chain(payment.payee.referenced_names())
                    {
                        if !resolver.is_defined(name) {
                            return Err(ProgramBuildError::UndefinedMember { name, line });
                        }
                    }
                }
            }

            validated_statements.push(statement);
        }

        Ok(Self {
            members,
            statements: validated_statements,
        })
    }

    pub fn members(&self) -> &'a [&'a str] {
        self.members
    }

    pub fn statements(&self) -> &[Statement<'a>] {
        &self.statements
    }

    pub fn calculate_balances(&self) -> HashMap<&'a str, Money> {
        let mut accumulator = BalanceAccumulator::new(self.members);
        for stmt in &self.statements {
            accumulator.apply(stmt);
        }
        accumulator.into_balances()
    }
}

impl<'a> BalanceAccumulator<'a> {
    pub fn new(members: &'a [&'a str]) -> Self {
        let balances: HashMap<&'a str, Money> = members
            .iter()
            .copied()
            .map(|member| (member, Money::zero()))
            .collect();
        let resolver = MemberSetResolver::new(members);

        Self { balances, resolver }
    }

    pub fn apply(&mut self, statement: &Statement<'a>) {
        match statement {
            Statement::Declaration(decl) => {
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

    pub fn balances(&self) -> &HashMap<&'a str, Money> {
        &self.balances
    }

    pub fn into_balances(self) -> HashMap<&'a str, Money> {
        self.balances
    }

    pub fn set_balances(&mut self, balances: HashMap<&'a str, Money>) {
        self.balances = balances;
    }

    pub fn evaluate_members(&self, expr: &MemberSetExpr<'a>) -> Option<MemberSet<'a>> {
        self.resolver.evaluate_members(expr)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Transfer<'a> {
    pub from: &'a str,
    pub to: &'a str,
    pub amount: Money,
}

#[derive(Debug, PartialEq)]
pub struct Settlement<'a> {
    pub new_balances: HashMap<&'a str, Money>,
    pub transfers: Vec<Transfer<'a>>,
}

pub fn distribute_balances<'a>(
    balances: &mut HashMap<&'a str, Money>,
    members: &MemberSet<'a>,
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
