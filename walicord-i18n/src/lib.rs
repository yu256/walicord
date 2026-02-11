#[cfg(all(feature = "ja", feature = "en"))]
compile_error!("Cannot enable both 'ja' and 'en' features at the same time");

#[cfg(feature = "ja")]
pub mod strings {
    pub const MEMBER: &str = "メンバー";
    pub const BALANCE: &str = "収支";
    pub const FROM: &str = "支払人";
    pub const TO: &str = "受取人";
    pub const AMOUNT: &str = "金額";
    pub const CATEGORY: &str = "カテゴリ";
    pub const SETTLEMENT_PAYMENT: &str = "確定者の支払い";
    pub const PAYMENT_TO_SETTLOR: &str = "確定者への支払い";
    pub const PENDING: &str = "保留";
    pub const SETTLEMENT_CALCULATION_FAILED: &str = "清算の計算に失敗しました";
    pub const MISSING_MEMBERS_DECLARATION: &str =
        "チャンネルのtopicに `MEMBERS := ...` の宣言が見つかりません。";
}

#[cfg(feature = "en")]
pub mod strings {
    pub const MEMBER: &str = "Member";
    pub const BALANCE: &str = "Balance";
    pub const FROM: &str = "From";
    pub const TO: &str = "To";
    pub const AMOUNT: &str = "Amount";
    pub const CATEGORY: &str = "Category";
    pub const SETTLEMENT_PAYMENT: &str = "Settlement Payment";
    pub const PAYMENT_TO_SETTLOR: &str = "Payment to Settlor";
    pub const PENDING: &str = "Pending";
    pub const SETTLEMENT_CALCULATION_FAILED: &str = "Settlement calculation failed";
    pub const MISSING_MEMBERS_DECLARATION: &str =
        "Could not find `MEMBERS := ...` declaration in the channel topic.";
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub mod strings {
    pub const MEMBER: &str = "Member";
    pub const BALANCE: &str = "Balance";
    pub const FROM: &str = "From";
    pub const TO: &str = "To";
    pub const AMOUNT: &str = "Amount";
    pub const CATEGORY: &str = "Category";
    pub const SETTLEMENT_PAYMENT: &str = "Settlement Payment";
    pub const PAYMENT_TO_SETTLOR: &str = "Payment to Settlor";
    pub const PENDING: &str = "Pending";
    pub const SETTLEMENT_CALCULATION_FAILED: &str = "Settlement calculation failed";
    pub const MISSING_MEMBERS_DECLARATION: &str =
        "Could not find `MEMBERS := ...` declaration in the channel topic.";
}

pub use strings::*;

#[cfg(feature = "ja")]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> String {
    format!("グループ '{}' の評価に失敗しました", name)
}

#[cfg(feature = "ja")]
pub fn undefined_group(name: impl std::fmt::Display) -> String {
    format!("未定義のグループ '{}' です", name)
}

#[cfg(feature = "ja")]
pub fn undefined_member(id: u64) -> String {
    format!("未定義のメンバー <@{id}> です")
}

#[cfg(feature = "en")]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> String {
    format!("Failed to evaluate group '{}'", name)
}

#[cfg(feature = "en")]
pub fn undefined_group(name: impl std::fmt::Display) -> String {
    format!("Undefined group '{}'", name)
}

#[cfg(feature = "en")]
pub fn undefined_member(id: u64) -> String {
    format!("Undefined member <@{id}>")
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> String {
    format!("Failed to evaluate group '{}'", name)
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn undefined_group(name: impl std::fmt::Display) -> String {
    format!("Undefined group '{}'", name)
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn undefined_member(id: u64) -> String {
    format!("Undefined member <@{id}>")
}

pub struct SyntaxErrorMessage {
    line: usize,
    detail: String,
}

pub struct ImplicitPayerMissingMessage {
    line: usize,
}

pub fn syntax_error(line: usize, detail: String) -> SyntaxErrorMessage {
    SyntaxErrorMessage { line, detail }
}

pub fn implicit_payer_missing(line: usize) -> ImplicitPayerMissingMessage {
    ImplicitPayerMissingMessage { line }
}

#[cfg(feature = "ja")]
impl std::fmt::Display for SyntaxErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "構文エラー (行 {}): {}", self.line, self.detail)
    }
}

#[cfg(feature = "ja")]
impl std::fmt::Display for ImplicitPayerMissingMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "支払者が省略されています (行 {}). `A が B に 1000 貸した` のように支払者を明示してください。",
            self.line
        )
    }
}

#[cfg(feature = "en")]
impl std::fmt::Display for SyntaxErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Syntax error at line {}: {}", self.line, self.detail)
    }
}

#[cfg(feature = "en")]
impl std::fmt::Display for ImplicitPayerMissingMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Payer is missing at line {}. Use explicit payer syntax, for example `Alice lent 1000 to Bob`.",
            self.line
        )
    }
}

#[cfg(not(any(feature = "ja", feature = "en")))]
impl std::fmt::Display for SyntaxErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Syntax error at line {}: {}", self.line, self.detail)
    }
}

#[cfg(not(any(feature = "ja", feature = "en")))]
impl std::fmt::Display for ImplicitPayerMissingMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Payer is missing at line {}. Use explicit payer syntax, for example `Alice lent 1000 to Bob`.",
            self.line
        )
    }
}
