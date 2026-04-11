use walicord_application::{ExpectedElement, ProgramParseError, SyntaxErrorKind};

fn expected_element_label(element: &ExpectedElement) -> &'static str {
    match element {
        ExpectedElement::Amount => walicord_i18n::EXPECTED_AMOUNT,
        ExpectedElement::MemberOrGroup => walicord_i18n::EXPECTED_MEMBER_OR_GROUP,
        ExpectedElement::Unknown => walicord_i18n::EXPECTED_INPUT,
    }
}

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
        ProgramParseError::UndefinedRole { id, line } => {
            format!(
                "{mention} {} (line {line})",
                walicord_i18n::undefined_role(id)
            )
        }
        ProgramParseError::UndefinedMember { id, line } => {
            format!(
                "{mention} {} (line {line})",
                walicord_i18n::undefined_member(id)
            )
        }
        ProgramParseError::SyntaxError { line, kind } => match &kind {
            SyntaxErrorKind::ParseFailure {
                attempted_form: Some(form),
                expected,
                near,
            } => {
                format!(
                    "{mention} {}",
                    walicord_i18n::syntax_error_with_form(
                        line,
                        form,
                        expected_element_label(expected),
                        near,
                    )
                )
            }
            SyntaxErrorKind::ParseFailure {
                attempted_form: None,
                near,
                ..
            } => {
                format!(
                    "{mention} {}",
                    walicord_i18n::syntax_error_unknown(line, near)
                )
            }
            SyntaxErrorKind::TrailingInput { text } => {
                format!(
                    "{mention} {}",
                    walicord_i18n::syntax_error_trailing(line, text)
                )
            }
        },
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
        ProgramParseError::WeightedReferenceOutsidePayee { line } => {
            format!(
                "{mention} {}",
                walicord_i18n::weighted_reference_outside_payee(line)
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
            ProgramParseError::FailedToEvaluateGroup { line, .. }
            | ProgramParseError::UndefinedGroup { line, .. }
            | ProgramParseError::UndefinedRole { line, .. }
            | ProgramParseError::UndefinedMember { line, .. }
            | ProgramParseError::SyntaxError { line, .. }
            | ProgramParseError::MissingContextForImplicitAuthor { line }
            | ProgramParseError::InvalidAmountExpression { line, .. }
            | ProgramParseError::AllZeroWeights { line }
            | ProgramParseError::WeightedReferenceOutsidePayee { line } => *line,
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
    #[case::undefined_role(
        ProgramParseError::UndefinedRole { id: 10, line: 4 },
        "@user"
    )]
    #[case::undefined_member(
        ProgramParseError::UndefinedMember { id: 12345, line: 2 },
        "@user"
    )]
    #[case::syntax_error(
        ProgramParseError::SyntaxError {
            line: 1,
            kind: SyntaxErrorKind::ParseFailure {
                attempted_form: Some("<PAYER> paid <AMOUNT> to <PAYEE>"), expected: ExpectedElement::Unknown,
                near: "unexpected".to_string(),
            },
        },
        "@user"
    )]
    #[case::syntax_error_trailing(
        ProgramParseError::SyntaxError {
            line: 2,
            kind: SyntaxErrorKind::TrailingInput {
                text: "xyz".to_string(),
            },
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
