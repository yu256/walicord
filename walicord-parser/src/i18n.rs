#[cfg(all(feature = "ja", feature = "en"))]
compile_error!("Cannot enable both 'ja' and 'en' features at the same time");

#[cfg(feature = "ja")]
pub fn syntax_error_detail(error: impl std::fmt::Display) -> String {
    format!("{error}")
}

#[cfg(feature = "ja")]
pub fn syntax_error_unparsed_detail(input: impl std::fmt::Display) -> String {
    format!("解析されていない入力: {input}")
}

#[cfg(feature = "en")]
pub fn syntax_error_detail(error: impl std::fmt::Display) -> String {
    format!("{error}")
}

#[cfg(feature = "en")]
pub fn syntax_error_unparsed_detail(input: impl std::fmt::Display) -> String {
    format!("Unparsed input: {input}")
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn syntax_error_detail(error: impl std::fmt::Display) -> String {
    format!("{error}")
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn syntax_error_unparsed_detail(input: impl std::fmt::Display) -> String {
    format!("Unparsed input: {input}")
}
