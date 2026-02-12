#![warn(clippy::uninlined_format_args)]

pub mod command_syntax;
pub mod error;
pub mod message_processor;
pub mod model;
pub mod ports;

pub use command_syntax::{COMMAND_PREFIXES, is_command_message, is_command_prefix};
pub use error::{ProgramParseError, SettlementOptimizationError};
pub use message_processor::{MessageProcessor, ProcessingOutcome, SettlementResult};
pub use model::{
    Command, PersonBalance, Script, ScriptStatement, ScriptStatementWithLine, SettleUpContext,
};
pub use ports::{MemberDirectory, ProgramParser, SettlementOptimizer};
