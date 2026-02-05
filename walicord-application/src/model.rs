use walicord_domain::{Declaration, MemberSetExpr, Money, Transfer};

#[derive(Debug, Clone, PartialEq)]
pub enum Command<'a> {
    Variables,
    Evaluate,
    SettleUp(MemberSetExpr<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AmountExpr {
    Literal(Money),
    ReceiptRef { index: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Payment<'a> {
    pub amount: AmountExpr,
    pub payer: MemberSetExpr<'a>,
    pub payee: MemberSetExpr<'a>,
}

pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Payment(Payment<'a>),
}

pub enum ScriptStatement<'a> {
    Domain(Statement<'a>),
    Command(Command<'a>),
}

pub struct ScriptStatementWithLine<'a> {
    pub line: usize,
    pub statement: ScriptStatement<'a>,
}

pub struct Script<'a> {
    members: &'a [&'a str],
    statements: Vec<ScriptStatementWithLine<'a>>,
}

impl<'a> Script<'a> {
    pub fn new(members: &'a [&'a str], statements: Vec<ScriptStatementWithLine<'a>>) -> Self {
        Self {
            members,
            statements,
        }
    }

    pub fn members(&self) -> &'a [&'a str] {
        self.members
    }

    pub fn statements(&self) -> &[ScriptStatementWithLine<'a>] {
        &self.statements
    }
}

#[derive(Debug)]
pub struct SettleUpContext<'a> {
    pub settle_members: Vec<&'a str>,
    pub immediate_transfers: Vec<Transfer<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PersonBalance<'a> {
    pub name: &'a str,
    pub balance: Money,
}
