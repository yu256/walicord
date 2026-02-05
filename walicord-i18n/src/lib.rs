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
    pub const RECEIPT_OCR_UNAVAILABLE: &str = "レシートOCRが利用できません。";
    pub const RECEIPT_ATTACHMENT_MISSING: &str = "レシート画像が見つかりません。";
    pub const RECEIPT_OCR_FAILED: &str = "レシートのOCRに失敗しました。";
    pub const RECEIPT_TOTAL_NOT_FOUND: &str = "レシートの合計金額が見つかりません。";
    pub const RECEIPT_TOTAL_AMBIGUOUS: &str = "レシートの合計金額が特定できません。";
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
    pub const RECEIPT_OCR_UNAVAILABLE: &str = "Receipt OCR is unavailable.";
    pub const RECEIPT_ATTACHMENT_MISSING: &str = "Receipt image is missing.";
    pub const RECEIPT_OCR_FAILED: &str = "Receipt OCR failed.";
    pub const RECEIPT_TOTAL_NOT_FOUND: &str = "Receipt total not found.";
    pub const RECEIPT_TOTAL_AMBIGUOUS: &str = "Receipt total is ambiguous.";
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
    pub const RECEIPT_OCR_UNAVAILABLE: &str = "Receipt OCR is unavailable.";
    pub const RECEIPT_ATTACHMENT_MISSING: &str = "Receipt image is missing.";
    pub const RECEIPT_OCR_FAILED: &str = "Receipt OCR failed.";
    pub const RECEIPT_TOTAL_NOT_FOUND: &str = "Receipt total not found.";
    pub const RECEIPT_TOTAL_AMBIGUOUS: &str = "Receipt total is ambiguous.";
}

pub use strings::*;

#[cfg(feature = "ja")]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> String {
    format!("グループ '{}' の評価に失敗しました", name)
}

#[cfg(feature = "en")]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> String {
    format!("Failed to evaluate group '{}'", name)
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> String {
    format!("Failed to evaluate group '{}'", name)
}
