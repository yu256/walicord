use walicord_domain::{
    Money, Statement, Transfer,
    model::{MemberId, MemberSetExpr, RoleMembers},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Command<'a> {
    Variables,
    Review,
    MemberAddCash {
        members: MemberSetExpr<'a>,
    },
    SettleUp {
        members: MemberSetExpr<'a>,
        cash_members: Option<MemberSetExpr<'a>>,
    },
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
    members: &'a [MemberId],
    role_members: &'a RoleMembers,
    statements: Vec<ScriptStatementWithLine<'a>>,
}

impl<'a> Script<'a> {
    pub fn new(
        member_ids: &'a [MemberId],
        role_members: &'a RoleMembers,
        statements: Vec<ScriptStatementWithLine<'a>>,
    ) -> Self {
        Self {
            members: member_ids,
            role_members,
            statements,
        }
    }

    pub fn members(&self) -> &'a [MemberId] {
        self.members
    }

    pub fn role_members(&self) -> &'a RoleMembers {
        self.role_members
    }

    pub fn statements(&self) -> &[ScriptStatementWithLine<'a>] {
        &self.statements
    }

    pub fn into_statements(self) -> Vec<ScriptStatementWithLine<'a>> {
        self.statements
    }
}

#[derive(Debug)]
pub struct SettleUpContext {
    pub settle_members: Vec<MemberId>,
    /// Confirmed transfers already applied to `SettlementResult::balances` via
    /// `SettleUpPolicy::settle()`. The presentation layer assumes balances
    /// reflect these transfers (e.g. settle members have zero balance).
    pub immediate_transfers: Vec<Transfer>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PersonBalance {
    pub id: MemberId,
    pub balance: Money,
}
