#![warn(clippy::uninlined_format_args)]

pub mod command_syntax;
pub mod error;
pub mod ledger;
pub mod message_processor;
pub mod model;
pub mod ports;
pub mod role_visibility;
pub mod settle_up;

pub use command_syntax::{COMMAND_PREFIXES, is_command_message, is_command_prefix};
pub use error::{
    BalanceCalculationError, ExpectedElement, FailureKind, ProgramParseError,
    SettlementOptimizationError, SyntaxErrorKind,
};
pub use ledger::{
    SealThroughTailError, seal_through_latest_unvoided_expense_or_settlement_entry,
    seal_through_tail, seal_through_tail_if_advances,
};
pub use message_processor::{MessageProcessor, ProcessingOutcome, SettlementResult};
pub use model::{
    Command, PersonBalance, Script, ScriptStatement, ScriptStatementWithLine, SettleUpContext,
};
pub use ports::{MemberDirectory, ProgramParser, SettlementPlanner};
pub use role_visibility::{
    FilteredEmptyRoleParseError, RoleVisibilityDiagnostic, RoleVisibilityDiagnostics,
    RoleVisibilityWarning, filtered_empty_role_parse_error, warnings_for_program_prefix,
};
pub use settle_up::{
    PreviewConfirmationBinding, PreviewedSettlement, PreviewedSettlementDigest, SettleUpPolicy,
    SettlementLedgerEventOutcome, ValidatedSettlementPlan,
};
