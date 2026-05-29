#![warn(clippy::uninlined_format_args)]

pub mod discord_ledger;
pub mod error_presenter;
pub mod settlement_presenter;
pub mod svg_table;
#[cfg(any(test, feature = "test-fixtures"))]
pub mod test_fixtures;
pub mod variables_presenter;

pub use discord_ledger::{
    BusinessDateTime, DiscordLedgerPresenter, RenderBudgetError, SafeLiteralText,
    SurfaceMemberLabels, confirmation_source_disclosure_line, truncate_component_label,
    validate_button_label, validate_component_placeholder, validate_custom_id,
    validate_modal_title, validate_text_input_label, validate_text_input_placeholder,
};
pub use error_presenter::format_program_parse_error;
pub use settlement_presenter::{SettlementPresenter, SettlementView};
pub use variables_presenter::VariablesPresenter;
