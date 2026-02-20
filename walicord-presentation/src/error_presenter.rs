use walicord_application::ProgramParseError;

pub fn format_program_parse_error(
    error: ProgramParseError<'_>,
    mention: impl std::fmt::Display,
) -> String {
    match error {
        ProgramParseError::FailedToEvaluateGroup { name, line } => {
            format!(
                "{mention} {} (line {line})",
                walicord_i18n::failed_to_evaluate_group(&name)
            )
        }
        ProgramParseError::UndefinedGroup { name, line } => {
            format!(
                "{mention} {} (line {line})",
                walicord_i18n::undefined_group(&name)
            )
        }
        ProgramParseError::UndefinedMember { id, line } => {
            format!(
                "{mention} {} (line {line})",
                walicord_i18n::undefined_member(id)
            )
        }
        ProgramParseError::SyntaxError { line, detail } => {
            format!("{mention} {}", walicord_i18n::syntax_error(line, detail))
        }
        ProgramParseError::MissingContextForImplicitAuthor { line } => {
            format!("{mention} {}", walicord_i18n::implicit_payer_missing(line))
        }
        ProgramParseError::InvalidAmountExpression { line, detail } => {
            format!(
                "{mention} {}",
                walicord_i18n::invalid_amount_expression(line, detail)
            )
        }
        ProgramParseError::AllZeroWeights { line } => {
            format!(
                "{mention} {} (line {line})",
                walicord_i18n::all_zero_weights()
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use std::borrow::Cow;

    fn get_line_number(error: &ProgramParseError<'_>) -> usize {
        match error {
            ProgramParseError::FailedToEvaluateGroup { line, .. } => *line,
            ProgramParseError::UndefinedGroup { line, .. } => *line,
            ProgramParseError::UndefinedMember { line, .. } => *line,
            ProgramParseError::SyntaxError { line, .. } => *line,
            ProgramParseError::MissingContextForImplicitAuthor { line } => *line,
            ProgramParseError::InvalidAmountExpression { line, .. } => *line,
            ProgramParseError::AllZeroWeights { line } => *line,
        }
    }

    #[rstest]
    #[case::failed_to_evaluate_group(
        ProgramParseError::FailedToEvaluateGroup {
            name: Cow::Borrowed("group1"),
            line: 5,
        },
        "@user"
    )]
    #[case::undefined_group(
        ProgramParseError::UndefinedGroup {
            name: Cow::Borrowed("unknown"),
            line: 3,
        },
        "@user"
    )]
    #[case::undefined_member(
        ProgramParseError::UndefinedMember { id: 12345, line: 2 },
        "@user"
    )]
    #[case::syntax_error(
        ProgramParseError::SyntaxError {
            line: 1,
            detail: "unexpected token".to_string(),
        },
        "@user"
    )]
    #[case::missing_context(
        ProgramParseError::MissingContextForImplicitAuthor { line: 4 },
        "@user"
    )]
    #[case::invalid_amount(
        ProgramParseError::InvalidAmountExpression {
            line: 6,
            detail: "division by zero".to_string(),
        },
        "@user"
    )]
    fn format_program_parse_error_includes_mention_and_line(
        #[case] error: ProgramParseError<'static>,
        #[case] mention: &str,
    ) {
        let expected_line = get_line_number(&error);
        let result = format_program_parse_error(error, mention);
        assert!(result.starts_with(mention));
        assert!(result.contains(&expected_line.to_string()));
    }
}
