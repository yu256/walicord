use crate::{
    Script,
    error::{ProgramParseError, SettlementOptimizationError},
    model::PersonBalance,
};
use walicord_domain::Transfer;

pub trait ProgramParser: Send + Sync {
    fn parse<'a>(
        &self,
        members: &'a [&'a str],
        content: &'a str,
    ) -> Result<Script<'a>, ProgramParseError<'a>>;
}

pub trait SettlementOptimizer: Send + Sync {
    fn optimize<'a>(
        &self,
        balances: &[PersonBalance<'a>],
    ) -> Result<Vec<Transfer<'a>>, SettlementOptimizationError>;
}
