#![warn(clippy::uninlined_format_args)]

pub mod application;
pub mod domain;
pub mod infrastructure;

pub use application::{MessageProcessor, ProcessingOutcome};
pub use domain::{Declaration, Program, ProgramParseError};
