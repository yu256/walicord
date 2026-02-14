use crate::{
    Script,
    error::{ProgramParseError, SettlementOptimizationError},
    model::PersonBalance,
};
use std::collections::HashMap;
use walicord_domain::{SettlementContext, Transfer, model::MemberId};

pub trait ProgramParser: Send + Sync {
    fn parse<'a>(
        &self,
        member_ids: &'a [MemberId],
        content: &'a str,
        author_id: Option<MemberId>,
    ) -> Result<Script<'a>, ProgramParseError<'a>>;
}

pub trait SettlementOptimizer: Send + Sync {
    fn optimize(
        &self,
        balances: &[PersonBalance],
        settle_members: &[MemberId],
        context: SettlementContext,
    ) -> Result<Vec<Transfer>, SettlementOptimizationError>;
}

pub trait MemberDirectory: Send + Sync {
    fn display_name(&self, member_id: MemberId) -> Option<&str>;
}

impl MemberDirectory for HashMap<MemberId, String> {
    fn display_name(&self, member_id: MemberId) -> Option<&str> {
        self.get(&member_id).map(String::as_str)
    }
}
