pub const DISCORD_MESSAGE_CONTENT_LIMIT: usize = 2_000;
pub const DISCORD_BUTTON_LABEL_LIMIT: usize = 80;
pub const DISCORD_MODAL_TITLE_LIMIT: usize = 45;
pub const DISCORD_TEXT_INPUT_LABEL_LIMIT: usize = 45;
pub const DISCORD_TEXT_INPUT_PLACEHOLDER_LIMIT: usize = 100;
pub const DISCORD_COMPONENT_LABEL_LIMIT: usize = 100;
pub const DISCORD_COMPONENT_DESCRIPTION_LIMIT: usize = 100;
pub const DISCORD_COMPONENT_PLACEHOLDER_LIMIT: usize = 150;
pub const DISCORD_CUSTOM_ID_LIMIT: usize = 100;
pub const DISCORD_ACTION_ROW_LIMIT: usize = 5;
pub const DISCORD_BUTTONS_PER_ROW_LIMIT: usize = 5;
pub const DISCORD_SELECT_OPTION_LIMIT: usize = 25;

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum RenderBudgetError {
    #[error("message content {actual} chars exceeds discord limit {limit}")]
    MessageContentTooLong { actual: usize, limit: usize },
    #[error("component label {actual} chars exceeds discord limit {limit}")]
    ComponentLabelTooLong { actual: usize, limit: usize },
    #[error("custom_id {actual} chars exceeds discord limit {limit}")]
    CustomIdTooLong { actual: usize, limit: usize },
    #[error("action rows {actual} exceeds discord limit {limit}")]
    TooManyActionRows { actual: usize, limit: usize },
    #[error("buttons {actual} in single row exceeds discord limit {limit}")]
    TooManyButtonsInRow { actual: usize, limit: usize },
    #[error("select options {actual} exceeds discord limit {limit}")]
    TooManySelectOptions { actual: usize, limit: usize },
    #[error(
        "invalid select value bounds: min_values={min_values}, max_values={max_values}, option_count={option_count}"
    )]
    InvalidSelectValueBounds {
        min_values: u8,
        max_values: u8,
        option_count: usize,
    },
}

pub fn validate_message_content(content: &str) -> Result<(), RenderBudgetError> {
    let actual = content.chars().count();
    if actual > DISCORD_MESSAGE_CONTENT_LIMIT {
        return Err(RenderBudgetError::MessageContentTooLong {
            actual,
            limit: DISCORD_MESSAGE_CONTENT_LIMIT,
        });
    }
    Ok(())
}

pub fn validate_custom_id(custom_id: &str) -> Result<(), RenderBudgetError> {
    let actual = custom_id.chars().count();
    if actual > DISCORD_CUSTOM_ID_LIMIT {
        return Err(RenderBudgetError::CustomIdTooLong {
            actual,
            limit: DISCORD_CUSTOM_ID_LIMIT,
        });
    }
    Ok(())
}

pub fn validate_button_label(label: &str) -> Result<(), RenderBudgetError> {
    validate_text_limit(label, DISCORD_BUTTON_LABEL_LIMIT)
}

pub fn validate_modal_title(title: &str) -> Result<(), RenderBudgetError> {
    validate_text_limit(title, DISCORD_MODAL_TITLE_LIMIT)
}

pub fn validate_text_input_label(label: &str) -> Result<(), RenderBudgetError> {
    validate_text_limit(label, DISCORD_TEXT_INPUT_LABEL_LIMIT)
}

pub fn validate_text_input_placeholder(placeholder: &str) -> Result<(), RenderBudgetError> {
    validate_text_limit(placeholder, DISCORD_TEXT_INPUT_PLACEHOLDER_LIMIT)
}

pub fn validate_component_label(label: &str) -> Result<(), RenderBudgetError> {
    validate_text_limit(label, DISCORD_COMPONENT_LABEL_LIMIT)
}

pub fn validate_component_description(description: &str) -> Result<(), RenderBudgetError> {
    validate_text_limit(description, DISCORD_COMPONENT_DESCRIPTION_LIMIT)
}

pub fn validate_component_placeholder(placeholder: &str) -> Result<(), RenderBudgetError> {
    validate_text_limit(placeholder, DISCORD_COMPONENT_PLACEHOLDER_LIMIT)
}

pub fn validate_action_rows(row_count: usize) -> Result<(), RenderBudgetError> {
    if row_count > DISCORD_ACTION_ROW_LIMIT {
        return Err(RenderBudgetError::TooManyActionRows {
            actual: row_count,
            limit: DISCORD_ACTION_ROW_LIMIT,
        });
    }
    Ok(())
}

pub fn validate_buttons_in_row(button_count: usize) -> Result<(), RenderBudgetError> {
    if button_count > DISCORD_BUTTONS_PER_ROW_LIMIT {
        return Err(RenderBudgetError::TooManyButtonsInRow {
            actual: button_count,
            limit: DISCORD_BUTTONS_PER_ROW_LIMIT,
        });
    }
    Ok(())
}

pub fn validate_select_options(option_count: usize) -> Result<(), RenderBudgetError> {
    if option_count > DISCORD_SELECT_OPTION_LIMIT {
        return Err(RenderBudgetError::TooManySelectOptions {
            actual: option_count,
            limit: DISCORD_SELECT_OPTION_LIMIT,
        });
    }
    Ok(())
}

pub fn validate_select_value_bounds(
    min_values: u8,
    max_values: u8,
    option_count: usize,
) -> Result<(), RenderBudgetError> {
    if min_values > max_values
        || usize::from(max_values) > option_count
        || usize::from(min_values) > option_count
    {
        return Err(RenderBudgetError::InvalidSelectValueBounds {
            min_values,
            max_values,
            option_count,
        });
    }
    Ok(())
}

fn validate_text_limit(text: &str, limit: usize) -> Result<(), RenderBudgetError> {
    let actual = text.chars().count();
    if actual > limit {
        return Err(RenderBudgetError::ComponentLabelTooLong { actual, limit });
    }
    Ok(())
}

pub fn truncate_component_label(label: &str) -> String {
    truncate_after_identifying_prefix(label, DISCORD_COMPONENT_LABEL_LIMIT)
}

fn truncate_after_identifying_prefix(text: &str, limit: usize) -> String {
    let actual = text.chars().count();
    if actual <= limit {
        return text.to_owned();
    }

    let keep = limit.saturating_sub(1);
    let prefix: String = text.chars().take(keep).collect();
    format!("{prefix}…")
}

#[cfg(test)]
mod tests {
    use super::{
        RenderBudgetError, truncate_component_label, validate_action_rows, validate_button_label,
        validate_buttons_in_row, validate_component_description, validate_component_label,
        validate_component_placeholder, validate_custom_id, validate_message_content,
        validate_modal_title, validate_select_options, validate_select_value_bounds,
        validate_text_input_label, validate_text_input_placeholder,
    };

    #[test]
    fn component_label_truncation_keeps_the_identifying_prefix() {
        let label = format!(
            "{}{}",
            "田中 太郎 / 開発部 / 夜ごはん会",
            " / 追加情報".repeat(20)
        );

        let actual = truncate_component_label(&label);

        assert!(actual.starts_with("田中 太郎 / 開発部 / 夜ごはん会"));
        assert!(actual.ends_with('…'));
        assert!(actual.chars().count() <= 100);
    }

    #[test]
    fn custom_ids_fail_closed_when_they_exceed_the_discord_limit() {
        let actual = validate_custom_id(&"x".repeat(101));

        assert_eq!(
            actual,
            Err(RenderBudgetError::CustomIdTooLong {
                actual: 101,
                limit: 100
            })
        );
    }

    #[test]
    fn button_labels_fail_closed_when_they_exceed_discords_limit() {
        let actual = validate_button_label(&"x".repeat(81));

        assert_eq!(
            actual,
            Err(RenderBudgetError::ComponentLabelTooLong {
                actual: 81,
                limit: 80
            })
        );
    }

    #[test]
    fn modal_titles_and_text_input_fields_fail_closed_when_they_exceed_discord_limits() {
        let title = validate_modal_title(&"x".repeat(46));
        let label = validate_text_input_label(&"x".repeat(46));
        let placeholder = validate_text_input_placeholder(&"x".repeat(101));

        assert_eq!(
            title,
            Err(RenderBudgetError::ComponentLabelTooLong {
                actual: 46,
                limit: 45
            })
        );
        assert_eq!(
            label,
            Err(RenderBudgetError::ComponentLabelTooLong {
                actual: 46,
                limit: 45
            })
        );
        assert_eq!(
            placeholder,
            Err(RenderBudgetError::ComponentLabelTooLong {
                actual: 101,
                limit: 100
            })
        );
    }

    #[test]
    fn component_placeholders_and_descriptions_fail_closed_when_they_exceed_limits() {
        let placeholder = validate_component_placeholder(&"x".repeat(151));
        let description = validate_component_description(&"x".repeat(101));

        assert_eq!(
            placeholder,
            Err(RenderBudgetError::ComponentLabelTooLong {
                actual: 151,
                limit: 150
            })
        );
        assert_eq!(
            description,
            Err(RenderBudgetError::ComponentLabelTooLong {
                actual: 101,
                limit: 100
            })
        );
    }

    #[test]
    fn component_labels_fail_closed_when_they_exceed_the_select_option_limit() {
        let actual = validate_component_label(&"x".repeat(101));

        assert_eq!(
            actual,
            Err(RenderBudgetError::ComponentLabelTooLong {
                actual: 101,
                limit: 100
            })
        );
    }

    #[test]
    fn action_rows_fail_closed_when_the_surface_exceeds_discords_row_budget() {
        let actual = validate_action_rows(6);

        assert_eq!(
            actual,
            Err(RenderBudgetError::TooManyActionRows {
                actual: 6,
                limit: 5
            })
        );
    }

    #[test]
    fn button_rows_fail_closed_when_a_single_row_exceeds_discords_button_budget() {
        let actual = validate_buttons_in_row(6);

        assert_eq!(
            actual,
            Err(RenderBudgetError::TooManyButtonsInRow {
                actual: 6,
                limit: 5
            })
        );
    }

    #[test]
    fn select_menus_fail_closed_when_a_page_exceeds_discords_option_budget() {
        let actual = validate_select_options(26);

        assert_eq!(
            actual,
            Err(RenderBudgetError::TooManySelectOptions {
                actual: 26,
                limit: 25
            })
        );
    }

    #[test]
    fn select_menus_fail_closed_when_value_bounds_do_not_match_the_option_set() {
        let actual = validate_select_value_bounds(1, 3, 2);

        assert_eq!(
            actual,
            Err(RenderBudgetError::InvalidSelectValueBounds {
                min_values: 1,
                max_values: 3,
                option_count: 2,
            })
        );
    }

    #[test]
    fn message_content_fails_closed_when_a_surface_exceeds_discords_budget() {
        let actual = validate_message_content(&"x".repeat(2_001));

        assert_eq!(
            actual,
            Err(RenderBudgetError::MessageContentTooLong {
                actual: 2_001,
                limit: 2_000,
            })
        );
    }
}
