use walicord_domain::{
    Money, Statement, Transfer,
    model::{MemberId, MemberSetExpr},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Command<'a> {
    Variables,
    Evaluate,
    SettleUp(MemberSetExpr<'a>),
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
pub struct SettleUpContext {
    pub settle_members: Vec<MemberId>,
    pub immediate_transfers: Vec<Transfer>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PersonBalance {
    pub id: MemberId,
    pub balance: Money,
}
