#![warn(clippy::uninlined_format_args)]

pub mod error;
pub mod message_processor;
pub mod model;
pub mod ports;
pub mod receipt;

pub use error::{
    ProgramParseError, ReceiptOcrError, ReceiptResolveError, SettlementBuildError,
    SettlementOptimizationError,
};
pub use message_processor::{MessageProcessor, ProcessingOutcome, SettlementResult};
pub use model::{
    AmountExpr, Command, Payment, PersonBalance, Script, ScriptStatement, ScriptStatementWithLine,
    SettleUpContext, Statement,
};
pub use ports::{ProgramParser, ReceiptOcr, SettlementOptimizer};
pub use receipt::{ReceiptAttachment, ReceiptContext};
