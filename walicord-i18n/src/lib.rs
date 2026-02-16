#![warn(clippy::uninlined_format_args)]

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
    pub const STATUS: &str = "状態";
    pub const SETTLEMENT_PAYMENT: &str = "確定者の支払い";
    pub const PAYMENT_TO_SETTLOR: &str = "確定者への支払い";
    pub const PENDING: &str = "保留";
    pub const SETTLED_MEMBER: &str = "確定対象";
    pub const UNSETTLED_MEMBER: &str = "未確定";
    pub const SETTLED_TRANSFER: &str = "確定済み";
    pub const PLANNED_TRANSFER: &str = "参考";
    pub const SETTLEMENT_CALCULATION_FAILED: &str = "清算の計算に失敗しました";
    pub const SETTLEMENT_QUANTIZATION_FAILED: &str = "清算の丸めに失敗しました";
    pub const SETTLEMENT_QUANTIZATION_INVALID_ADJUSTMENT: &str =
        "清算の丸めに失敗しました (調整回数が不正です)";
    pub const SETTLEMENT_QUANTIZATION_INSUFFICIENT_CANDIDATES: &str =
        "清算の丸めに失敗しました (調整候補が不足しています)";
    pub const SETTLEMENT_QUANTIZATION_ZERO_SUM_INVARIANT: &str =
        "清算の丸めに失敗しました (ゼロサム整合性を満たせませんでした)";
    pub const SETTLEMENT_QUANTIZATION_NON_INTEGRAL: &str =
        "清算の丸めに失敗しました (整数化できません)";
}

#[cfg(feature = "en")]
pub mod strings {
    pub const MEMBER: &str = "Member";
    pub const BALANCE: &str = "Balance";
    pub const FROM: &str = "From";
    pub const TO: &str = "To";
    pub const AMOUNT: &str = "Amount";
    pub const CATEGORY: &str = "Category";
    pub const STATUS: &str = "Status";
    pub const SETTLEMENT_PAYMENT: &str = "Settlement Payment";
    pub const PAYMENT_TO_SETTLOR: &str = "Payment to Settlor";
    pub const PENDING: &str = "Pending";
    pub const SETTLED_MEMBER: &str = "Settled";
    pub const UNSETTLED_MEMBER: &str = "Not settled";
    pub const SETTLED_TRANSFER: &str = "Confirmed";
    pub const PLANNED_TRANSFER: &str = "Plan";
    pub const SETTLEMENT_CALCULATION_FAILED: &str = "Settlement calculation failed";
    pub const SETTLEMENT_QUANTIZATION_FAILED: &str = "Settlement quantization failed";
    pub const SETTLEMENT_QUANTIZATION_INVALID_ADJUSTMENT: &str =
        "Settlement quantization failed (invalid adjustment count)";
    pub const SETTLEMENT_QUANTIZATION_INSUFFICIENT_CANDIDATES: &str =
        "Settlement quantization failed (insufficient candidates)";
    pub const SETTLEMENT_QUANTIZATION_ZERO_SUM_INVARIANT: &str =
        "Settlement quantization failed (zero-sum invariant violation)";
    pub const SETTLEMENT_QUANTIZATION_NON_INTEGRAL: &str =
        "Settlement quantization failed (non-integral amount)";
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub mod strings {
    pub const MEMBER: &str = "Member";
    pub const BALANCE: &str = "Balance";
    pub const FROM: &str = "From";
    pub const TO: &str = "To";
    pub const AMOUNT: &str = "Amount";
    pub const CATEGORY: &str = "Category";
    pub const STATUS: &str = "Status";
    pub const SETTLEMENT_PAYMENT: &str = "Settlement Payment";
    pub const PAYMENT_TO_SETTLOR: &str = "Payment to Settlor";
    pub const PENDING: &str = "Pending";
    pub const SETTLED_MEMBER: &str = "Settled";
    pub const UNSETTLED_MEMBER: &str = "Not settled";
    pub const SETTLED_TRANSFER: &str = "Confirmed";
    pub const PLANNED_TRANSFER: &str = "Plan";
    pub const SETTLEMENT_CALCULATION_FAILED: &str = "Settlement calculation failed";
    pub const SETTLEMENT_QUANTIZATION_FAILED: &str = "Settlement quantization failed";
    pub const SETTLEMENT_QUANTIZATION_INVALID_ADJUSTMENT: &str =
        "Settlement quantization failed (invalid adjustment count)";
    pub const SETTLEMENT_QUANTIZATION_INSUFFICIENT_CANDIDATES: &str =
        "Settlement quantization failed (insufficient candidates)";
    pub const SETTLEMENT_QUANTIZATION_ZERO_SUM_INVARIANT: &str =
        "Settlement quantization failed (zero-sum invariant violation)";
    pub const SETTLEMENT_QUANTIZATION_NON_INTEGRAL: &str =
        "Settlement quantization failed (non-integral amount)";
}

pub use strings::*;

#[cfg(feature = "ja")]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "グループ '{name}' の評価に失敗しました"))
}

#[cfg(feature = "ja")]
pub fn undefined_group(name: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "未定義のグループ '{name}' です"))
}

#[cfg(feature = "ja")]
pub fn undefined_member(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "未定義のメンバー <@{id}> です"))
}

#[cfg(feature = "en")]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Failed to evaluate group '{}'", name))
}

#[cfg(feature = "en")]
pub fn undefined_group(name: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Undefined group '{}'", name))
}

#[cfg(feature = "en")]
pub fn undefined_member(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Undefined member <@{id}>"))
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Failed to evaluate group '{}'", name))
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn undefined_group(name: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Undefined group '{}'", name))
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn undefined_member(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Undefined member <@{id}>"))
}

pub struct SyntaxErrorMessage {
    line: usize,
    detail: String,
}

pub struct ImplicitPayerMissingMessage {
    line: usize,
}

pub struct AmountExpressionErrorMessage {
    line: usize,
    detail: String,
}

pub fn syntax_error(line: usize, detail: String) -> SyntaxErrorMessage {
    SyntaxErrorMessage { line, detail }
}

pub fn implicit_payer_missing(line: usize) -> ImplicitPayerMissingMessage {
    ImplicitPayerMissingMessage { line }
}

pub fn invalid_amount_expression(line: usize, detail: String) -> AmountExpressionErrorMessage {
    AmountExpressionErrorMessage { line, detail }
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
            "支払者が省略されています (行 {}). `A が B に 1000 立て替えた` のように支払者を明示してください。",
            self.line
        )
    }
}

#[cfg(feature = "ja")]
impl std::fmt::Display for AmountExpressionErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "金額の式が不正です (行 {}): {}", self.line, self.detail)
    }
}

#[cfg(feature = "en")]
impl std::fmt::Display for SyntaxErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Syntax error at line {}: {}", self.line, self.detail)
    }
}

#[cfg(feature = "en")]
impl std::fmt::Display for AmountExpressionErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Invalid amount expression at line {}: {}",
            self.line, self.detail
        )
    }
}

#[cfg(feature = "en")]
impl std::fmt::Display for ImplicitPayerMissingMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Payer is missing at line {}. Use explicit payer syntax, for example `Alice paid 1000 to Bob`.",
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
            "Payer is missing at line {}. Use explicit payer syntax, for example `Alice paid 1000 to Bob`.",
            self.line
        )
    }
}

#[cfg(not(any(feature = "ja", feature = "en")))]
impl std::fmt::Display for AmountExpressionErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Invalid amount expression at line {}: {}",
            self.line, self.detail
        )
    }
}

pub fn settlement_quantization_imbalanced(total: impl std::fmt::Display) -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| write!(f, "清算の丸めに失敗しました (合計: {total})"));
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        write!(f, "Settlement quantization failed (total: {})", total)
    });
}

pub fn settlement_quantization_unsupported_scale(
    scale: u32,
    max_supported: u32,
) -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "清算の丸めに失敗しました (scale={scale}, max={max_supported})"
        )
    });
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "Settlement quantization failed (scale={}, max={})",
            scale, max_supported
        )
    });
}

pub fn settlement_imbalanced_total(total: impl std::fmt::Display) -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| write!(f, "清算の計算に失敗しました (total: {total})"));
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        write!(f, "Settlement calculation failed (total: {})", total)
    });
}

pub fn settlement_invalid_grid(g1: i64, g2: i64) -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "清算の計算に失敗しました (invalid grid: g1={g1}, g2={g2})"
        )
    });
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "Settlement calculation failed (invalid grid: g1={}, g2={})",
            g1, g2
        )
    });
}

pub fn settlement_model_too_large(edge_count: usize, max_edges: usize) -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "清算の計算に失敗しました (model too large: edges={edge_count}, max={max_edges})"
        )
    });
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "Settlement calculation failed (model too large: edges={}, max={})",
            edge_count, max_edges
        )
    });
}
