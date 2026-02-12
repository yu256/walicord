pub const COMMAND_PREFIXES: &[&str] = &["!variables", "!review", "!清算確認", "!settleup", "!確定"];

pub fn is_command_prefix(content: &str, cmd: &str) -> bool {
    let is_prefix = if cmd.is_ascii() {
        content
            .get(..cmd.len())
            .is_some_and(|head| head.eq_ignore_ascii_case(cmd))
    } else {
        content.starts_with(cmd)
    };

    if !is_prefix {
        return false;
    }

    content
        .get(cmd.len()..)
        .and_then(|rest| rest.chars().next())
        .is_none_or(char::is_whitespace)
}

pub fn is_command_message(content: &str) -> bool {
    COMMAND_PREFIXES
        .iter()
        .any(|&cmd| is_command_prefix(content, cmd))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("!review", true)]
    #[case("!REVIEW", true)]
    #[case("!reviewing", false)]
    #[case("!review now", true)]
    #[case("!清算確認", true)]
    #[case("!清算確認中", false)]
    #[case("!settleup <@1>", true)]
    #[case("!variablesXYZ", false)]
    fn test_is_command_message(#[case] input: &str, #[case] expected: bool) {
        assert_eq!(is_command_message(input), expected);
    }
}
