use walicord_application::{FilteredEmptyRoleParseError, RoleVisibilityWarning};

pub fn format_warning_lines(warnings: &[RoleVisibilityWarning]) -> Vec<String> {
    warnings
        .iter()
        .map(|warning| {
            walicord_i18n::role_members_filtered_by_channel_visibility(
                warning.role_id.0,
                warning.visible_members,
                warning.excluded_members,
            )
            .to_string()
        })
        .collect()
}

pub fn format_filtered_empty_role_parse_error(
    error: FilteredEmptyRoleParseError,
    mention: impl std::fmt::Display,
) -> String {
    format!(
        "{mention} {} (line {})",
        walicord_i18n::role_has_no_visible_members_in_channel(
            error.role_id.0,
            error.excluded_members
        ),
        error.line
    )
}
