#![warn(clippy::uninlined_format_args)]

pub mod error_presenter;
pub mod settlement_presenter;
pub mod svg_table;
pub mod variables_presenter;

pub use error_presenter::format_program_parse_error;
pub use settlement_presenter::{SettlementPresenter, SettlementView};
pub use variables_presenter::VariablesPresenter;
