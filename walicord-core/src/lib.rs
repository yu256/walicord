#![warn(clippy::uninlined_format_args)]

pub mod application;
pub mod domain;
pub mod i18n;
pub mod infrastructure;

pub use application::{MessageProcessor, ProcessingOutcome, SettlementView};
pub use domain::{Declaration, Program, ProgramParseError};
