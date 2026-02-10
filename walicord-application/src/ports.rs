use crate::{
    Script,
    error::{ProgramParseError, SettlementOptimizationError},
    model::PersonBalance,
};
use std::collections::HashMap;
use walicord_domain::{Transfer, model::MemberId};

pub trait ProgramParser: Send + Sync {
    fn parse<'a>(
        &self,
        members: &'a [&'a str],
        content: &'a str,
    ) -> Result<Script<'a>, ProgramParseError<'a>>;
}

pub trait SettlementOptimizer: Send + Sync {
    fn optimize(
        &self,
        balances: &[PersonBalance],
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
