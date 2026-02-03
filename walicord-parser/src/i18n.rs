#[cfg(all(feature = "ja", feature = "en"))]
compile_error!("Cannot enable both 'ja' and 'en' features at the same time");

#[cfg(feature = "ja")]
pub fn line_syntax_error(line: usize, error: impl std::fmt::Display) -> String {
    format!("行 {line}: 構文エラー - {error}")
}

#[cfg(feature = "ja")]
pub fn line_syntax_error_unparsed(line: usize, input: impl std::fmt::Display) -> String {
    format!("行 {line}: 構文エラー - 解析されていない入力: {input}")
}

#[cfg(feature = "en")]
pub fn line_syntax_error(line: usize, error: impl std::fmt::Display) -> String {
    format!("Line {}: Syntax error - {}", line, error)
}

#[cfg(feature = "en")]
pub fn line_syntax_error_unparsed(line: usize, input: impl std::fmt::Display) -> String {
    format!("Line {}: Syntax error - Unparsed input: {}", line, input)
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn line_syntax_error(line: usize, error: impl std::fmt::Display) -> String {
    format!("Line {}: Syntax error - {}", line, error)
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn line_syntax_error_unparsed(line: usize, input: impl std::fmt::Display) -> String {
    format!("Line {}: Syntax error - Unparsed input: {}", line, input)
}
