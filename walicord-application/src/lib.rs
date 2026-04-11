#![warn(clippy::uninlined_format_args)]

pub mod command_syntax;
pub mod error;
pub mod message_processor;
pub mod model;
pub mod ports;
pub mod role_visibility;

pub use command_syntax::{COMMAND_PREFIXES, is_command_message, is_command_prefix};
pub use error::{
    BalanceCalculationError, ExpectedElement, FailureKind, ProgramParseError,
    SettlementOptimizationError, SyntaxErrorKind,
};
pub use message_processor::{MessageProcessor, ProcessingOutcome, SettlementResult};
pub use model::{
    Command, PersonBalance, Script, ScriptStatement, ScriptStatementWithLine, SettleUpContext,
};
pub use ports::{MemberDirectory, ProgramParser, SettlementOptimizer};
pub use role_visibility::{
    FilteredEmptyRoleParseError, RoleVisibilityDiagnostic, RoleVisibilityDiagnostics,
    RoleVisibilityWarning, filtered_empty_role_parse_error, warnings_for_program_prefix,
};
