#![warn(clippy::uninlined_format_args)]

pub mod settlement_presenter;
pub mod svg_table;
pub mod variables_presenter;

pub use settlement_presenter::{SettlementPresenter, SettlementView};
pub use variables_presenter::VariablesPresenter;
