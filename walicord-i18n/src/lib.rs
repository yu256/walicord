#![warn(clippy::uninlined_format_args)]

#[cfg(all(feature = "ja", feature = "en"))]
compile_error!("Cannot enable both 'ja' and 'en' features at the same time");

#[cfg(feature = "ja")]
pub mod strings {
    pub const MEMBER: &str = "メンバー";
    pub const BALANCE: &str = "収支";
    pub const FROM: &str = "支払者";
    pub const TO: &str = "受取人";
    pub const AMOUNT: &str = "金額";
    pub const STATUS: &str = "状態";
    pub const SETTLED_MEMBER: &str = "確定対象";
    pub const UNSETTLED_MEMBER: &str = "未確定";
    pub const SETTLED_TRANSFER: &str = "確定済み";
    pub const PLANNED_TRANSFER: &str = "参考";
    pub const SETTLEMENT_CALCULATION_FAILED: &str =
        "清算計算に失敗しました。入力内容を確認してください。";
    pub const SETTLEMENT_QUANTIZATION_FAILED: &str =
        "清算計算に失敗しました。入力内容を確認してください。";
    pub const SETTLEMENT_QUANTIZATION_INVALID_ADJUSTMENT: &str =
        "清算計算に失敗しました。入力内容を確認してください。";
    pub const SETTLEMENT_QUANTIZATION_INSUFFICIENT_CANDIDATES: &str =
        "清算計算に失敗しました。入力内容を確認してください。";
    pub const SETTLEMENT_QUANTIZATION_ZERO_SUM_INVARIANT: &str =
        "清算計算に失敗しました。入力内容を確認してください。";
    pub const SETTLEMENT_QUANTIZATION_NON_INTEGRAL: &str =
        "清算計算に失敗しました。入力内容を確認してください。";
    pub const WEIGHT_OVERFLOW: &str = "重みの合計がオーバーフローしました";
    pub const ZERO_TOTAL_WEIGHT: &str = "重みの合計が0です";
    pub const RECEIVER_BALANCE: &str = "受取後残高";
    pub const EXPECTED_AMOUNT: &str = "金額";
    pub const EXPECTED_MEMBER_OR_GROUP: &str = "メンバーまたはグループ";
    pub const EXPECTED_INPUT: &str = "入力";
    pub const CHANNEL_NOT_TRACKED: &str = "このチャンネルは walicord で追跡されていません。チャンネルトピックに `#walicord` を追加してください。";
    pub const SLASH_REVIEW_DESCRIPTION: &str =
        "清算プランを確認します（台帳スレッド。親チャンネルでは従来の review）。";
    pub const SLASH_VARIABLES_DESCRIPTION: &str = "変数一覧 - 定義されたグループ変数を表示";
    pub const SLASH_CACHE_LOAD_FAILED: &str = "チャンネル履歴の読み込みに失敗しました。";
    pub const SLASH_PENDING_NOT_CLEARED: &str =
        "メッセージの処理が完了していません。しばらく待ってから再度お試しください。";
    pub const SLASH_ROSTER_LOAD_FAILED: &str = "メンバー情報の読み込みに失敗しました。";
    pub const SLASH_NO_VARIABLES: &str = "変数は定義されていません。";
    pub const SLASH_RENDER_FAILED: &str = "清算結果の画像を生成できませんでした。";
    pub const RECOVERY_REFERENCE_PREFIX: &str = "復旧用の参照: ";
    pub const PUBLIC_TRUNCATION_OMITTED: &str = "... (省略されました)";
    pub const PUBLIC_TRUNCATION_LEDGER_GUIDANCE: &str = "残高は /ledger で確認できます。";
}

#[cfg(feature = "en")]
pub mod strings {
    pub const MEMBER: &str = "Member";
    pub const BALANCE: &str = "Balance";
    pub const FROM: &str = "From";
    pub const TO: &str = "To";
    pub const AMOUNT: &str = "Amount";
    pub const STATUS: &str = "Status";
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
    pub const WEIGHT_OVERFLOW: &str = "Weight sum overflow";
    pub const ZERO_TOTAL_WEIGHT: &str = "Total weight is zero";
    pub const RECEIVER_BALANCE: &str = "Balance After";
    pub const EXPECTED_AMOUNT: &str = "amount";
    pub const EXPECTED_MEMBER_OR_GROUP: &str = "member or group";
    pub const EXPECTED_INPUT: &str = "input";
    pub const CHANNEL_NOT_TRACKED: &str =
        "This channel is not tracked by walicord. Add `#walicord` to the channel topic.";
    pub const SLASH_REVIEW_DESCRIPTION: &str =
        "Review the settlement plan (ledger thread; parent channel uses legacy review).";
    pub const SLASH_VARIABLES_DESCRIPTION: &str = "Show defined group variables";
    pub const SLASH_CACHE_LOAD_FAILED: &str = "Failed to load channel history.";
    pub const SLASH_PENDING_NOT_CLEARED: &str =
        "Messages are still being processed. Please try again shortly.";
    pub const SLASH_ROSTER_LOAD_FAILED: &str = "Failed to load member roster.";
    pub const SLASH_NO_VARIABLES: &str = "No variables defined.";
    pub const SLASH_RENDER_FAILED: &str = "Failed to render settlement image.";
    pub const RECOVERY_REFERENCE_PREFIX: &str = "Recovery reference: ";
    pub const PUBLIC_TRUNCATION_OMITTED: &str = "... (truncated)";
    pub const PUBLIC_TRUNCATION_LEDGER_GUIDANCE: &str = "Use /ledger to check balances.";
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub mod strings {
    pub const MEMBER: &str = "Member";
    pub const BALANCE: &str = "Balance";
    pub const FROM: &str = "From";
    pub const TO: &str = "To";
    pub const AMOUNT: &str = "Amount";
    pub const STATUS: &str = "Status";
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
    pub const WEIGHT_OVERFLOW: &str = "Weight sum overflow";
    pub const ZERO_TOTAL_WEIGHT: &str = "Total weight is zero";
    pub const RECEIVER_BALANCE: &str = "Balance After";
    pub const EXPECTED_AMOUNT: &str = "amount";
    pub const EXPECTED_MEMBER_OR_GROUP: &str = "member or group";
    pub const EXPECTED_INPUT: &str = "input";
    pub const CHANNEL_NOT_TRACKED: &str =
        "This channel is not tracked by walicord. Add `#walicord` to the channel topic.";
    pub const SLASH_REVIEW_DESCRIPTION: &str =
        "Review the settlement plan (ledger thread; parent channel uses legacy review).";
    pub const SLASH_VARIABLES_DESCRIPTION: &str = "Show defined group variables";
    pub const SLASH_CACHE_LOAD_FAILED: &str = "Failed to load channel history.";
    pub const SLASH_PENDING_NOT_CLEARED: &str =
        "Messages are still being processed. Please try again shortly.";
    pub const SLASH_ROSTER_LOAD_FAILED: &str = "Failed to load member roster.";
    pub const SLASH_NO_VARIABLES: &str = "No variables defined.";
    pub const SLASH_RENDER_FAILED: &str = "Failed to render settlement image.";
    pub const RECOVERY_REFERENCE_PREFIX: &str = "Recovery reference: ";
    pub const PUBLIC_TRUNCATION_OMITTED: &str = "... (truncated)";
    pub const PUBLIC_TRUNCATION_LEDGER_GUIDANCE: &str = "Use /ledger to check balances.";
}

pub use strings::*;

pub const SLASH_PANEL_DESCRIPTION: &str = if cfg!(feature = "ja") {
    "このチャンネルに台帳パネルを投稿します。"
} else {
    "Post the ledger panel in this channel."
};

pub const SLASH_EXPENSE_DESCRIPTION: &str = if cfg!(feature = "ja") {
    "このチャンネルで支出を記録します。"
} else {
    "Record an expense in this channel."
};

pub const SLASH_SETTLE_DESCRIPTION: &str = if cfg!(feature = "ja") {
    "表示中の清算プランを確定します（台帳スレッド）。"
} else {
    "Confirm the displayed settlement plan (ledger thread)."
};

pub const SETTLEMENT_THREAD_ONLY_MESSAGE: &str = if cfg!(feature = "ja") {
    "/settle は台帳スレッドで実行してください。"
} else {
    "Run /settle in the ledger thread."
};

pub const SLASH_VOID_DESCRIPTION: &str = if cfg!(feature = "ja") {
    "このチャンネルで最近の記録を取り消します。"
} else {
    "Void recent records in this channel."
};

pub const SLASH_LEDGER_DESCRIPTION: &str = if cfg!(feature = "ja") {
    "このチャンネルの台帳を表示します。"
} else {
    "Show this channel's ledger."
};

pub const SLASH_LEDGER_REFRESH_DESCRIPTION: &str = if cfg!(feature = "ja") {
    "この親チャンネルの台帳スレッド状態を管理者が再確認します。"
} else {
    "Admins re-check the ledger-thread state for this parent channel."
};

#[cfg(feature = "ja")]
pub fn failed_to_evaluate_group(name: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "グループ '{name}' の評価に失敗しました"))
}

#[cfg(feature = "ja")]
pub fn undefined_group(name: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "未定義のグループ '{name}' です"))
}

#[cfg(feature = "ja")]
pub fn undefined_role(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "未定義のロール <@&{id}> です"))
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
pub fn undefined_role(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Undefined role <@&{id}>"))
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
pub fn undefined_role(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Undefined role <@&{id}>"))
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn undefined_member(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Undefined member <@{id}>"))
}

#[cfg(feature = "ja")]
pub fn unknown_user_label(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "不明なユーザー (ID: {id})"))
}

#[cfg(feature = "ja")]
pub const UNKNOWN_DISPLAY_LABEL: &str = "不明";

#[cfg(feature = "ja")]
pub fn unknown_role_label(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "不明なロール (ID: {id})"))
}

#[cfg(feature = "ja")]
pub fn disambiguated_visible_label(
    label: impl std::fmt::Display,
    id: u64,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "{label} (ID: {id})"))
}

#[cfg(feature = "en")]
pub fn unknown_user_label(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Unknown user (ID: {id})"))
}

#[cfg(feature = "en")]
pub const UNKNOWN_DISPLAY_LABEL: &str = "Unknown";

#[cfg(feature = "en")]
pub fn unknown_role_label(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Unknown role (ID: {id})"))
}

#[cfg(feature = "en")]
pub fn disambiguated_visible_label(
    label: impl std::fmt::Display,
    id: u64,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "{label} (ID: {id})"))
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn unknown_user_label(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Unknown user (ID: {id})"))
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub const UNKNOWN_DISPLAY_LABEL: &str = "Unknown";

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn unknown_role_label(id: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "Unknown role (ID: {id})"))
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn disambiguated_visible_label(
    label: impl std::fmt::Display,
    id: u64,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "{label} (ID: {id})"))
}

#[cfg(feature = "ja")]
pub fn selected_name_summary_omitted(
    visible_names: impl std::fmt::Display,
    omitted_count: usize,
    total_count: usize,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        write!(
            f,
            "{visible_names}... 他 {omitted_count}名 (合計 {total_count}名)"
        )
    })
}

#[cfg(feature = "en")]
pub fn selected_name_summary_omitted(
    visible_names: impl std::fmt::Display,
    omitted_count: usize,
    total_count: usize,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        write!(
            f,
            "{visible_names}... and {omitted_count} more ({total_count} total)"
        )
    })
}

#[cfg(not(any(feature = "ja", feature = "en")))]
pub fn selected_name_summary_omitted(
    visible_names: impl std::fmt::Display,
    omitted_count: usize,
    total_count: usize,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        write!(
            f,
            "{visible_names}... and {omitted_count} more ({total_count} total)"
        )
    })
}

pub fn role_members_filtered_by_channel_visibility(
    id: u64,
    visible_members: usize,
    excluded_members: usize,
) -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "警告: ロール <@&{id}> はこのチャンネルで見えないメンバー {excluded_members} 人を除外しました（対象 {visible_members} 人）"
        )
    });
    #[cfg(feature = "en")]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "Warning: Excluded {excluded_members} members from role <@&{id}> due to channel visibility ({visible_members} members remain)"
        )
    });
    #[cfg(not(any(feature = "ja", feature = "en")))]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "Warning: Excluded {excluded_members} members from role <@&{id}> due to channel visibility ({visible_members} members remain)"
        )
    });
}

pub fn role_has_no_visible_members_in_channel(
    id: u64,
    excluded_members: usize,
) -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "ロール <@&{id}> は存在しますが、このチャンネルで見えるメンバーがいません（{excluded_members} 人を除外）"
        )
    });
    #[cfg(feature = "en")]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "Role <@&{id}> exists, but no members are visible in this channel ({excluded_members} excluded)"
        )
    });
    #[cfg(not(any(feature = "ja", feature = "en")))]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "Role <@&{id}> exists, but no members are visible in this channel ({excluded_members} excluded)"
        )
    });
}

pub struct ImplicitPayerMissingMessage {
    line: usize,
}

pub struct AmountExpressionErrorMessage {
    line: usize,
    detail: String,
}

pub fn implicit_payer_missing(line: usize) -> ImplicitPayerMissingMessage {
    ImplicitPayerMissingMessage { line }
}

pub fn invalid_amount_expression(line: usize, detail: String) -> AmountExpressionErrorMessage {
    AmountExpressionErrorMessage { line, detail }
}

pub fn syntax_error_with_form<'a>(
    line: usize,
    form: &'a str,
    expected: &'a str,
    near: &'a str,
) -> impl std::fmt::Display + use<'a> {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        if near.is_empty() {
            write!(
                f,
                "「{form}」として解析中にエラー (行 {line}): {expected}が必要ですが、文が途中で終わっています"
            )
        } else {
            write!(
                f,
                "「{form}」として解析中にエラー (行 {line}): {expected}が必要ですが `{near}` が見つかりました"
            )
        }
    });
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        if near.is_empty() {
            write!(
                f,
                "Expected {expected} while parsing \"{form}\" at line {line}, but the line ended before {expected} was provided"
            )
        } else {
            write!(
                f,
                "Expected {expected} while parsing \"{form}\" at line {line}, but found `{near}`"
            )
        }
    });
}

pub fn syntax_error_unknown<'a>(line: usize, near: &'a str) -> impl std::fmt::Display + use<'a> {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        if near.is_empty() {
            write!(f, "入力を認識できません (行 {line})")
        } else {
            write!(f, "`{near}` を認識できません (行 {line})")
        }
    });
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        if near.is_empty() {
            write!(f, "Unrecognized input at line {line}")
        } else {
            write!(f, "Unrecognized input `{near}` at line {line}")
        }
    });
}

pub fn syntax_error_trailing<'a>(line: usize, text: &'a str) -> impl std::fmt::Display + use<'a> {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "文の末尾に不要なテキストがあります (行 {line}): `{text}`"
        )
    });
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "Unexpected text after end of statement at line {line}: `{text}`"
        )
    });
}

pub fn weighted_reference_outside_payee(line: usize) -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        write!(f, "重み付き参照は支払先でのみ使用できます (行 {line})")
    });
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "Weighted references are only allowed in payment payee (line {line})"
        )
    });
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
    return std::fmt::from_fn(move |f| {
        let _ = &total;
        write!(f, "清算計算に失敗しました。入力内容を確認してください。")
    });
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
        let _ = (scale, max_supported);
        write!(f, "清算計算に失敗しました。入力内容を確認してください。")
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
    return std::fmt::from_fn(move |f| {
        let _ = &total;
        write!(f, "清算計算に失敗しました。入力内容を確認してください。")
    });
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        write!(f, "Settlement calculation failed (total: {})", total)
    });
}

pub fn settlement_invalid_grid(g1: i64, g2: i64) -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        let _ = (g1, g2);
        write!(f, "清算計算に失敗しました。入力内容を確認してください。")
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
        let _ = (edge_count, max_edges);
        write!(f, "清算計算に失敗しました。入力内容を確認してください。")
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

pub fn all_zero_weights() -> impl std::fmt::Display {
    #[cfg(feature = "ja")]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "すべての重みが0です。少なくとも1人の重みを0より大きくしてください"
        )
    });
    #[cfg(not(feature = "ja"))]
    return std::fmt::from_fn(move |f| {
        write!(
            f,
            "All weights are zero. At least one member must have a weight greater than 0"
        )
    });
}

pub fn public_expense_header(entry_id: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "支出 [#{entry_id}]")
        } else {
            write!(f, "Expense [#{entry_id}]")
        }
    })
}

pub fn public_settlement_header(entry_id: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "清算 [#{entry_id}]")
        } else {
            write!(f, "Settlement [#{entry_id}]")
        }
    })
}

pub fn public_seal_header(entry_id: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "確認 [#{entry_id}]")
        } else {
            write!(f, "Seal [#{entry_id}]")
        }
    })
}

pub fn public_balance_adjustment_header(
    entry_id: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "残高補正 [#{entry_id}]")
        } else {
            write!(f, "Balance Adjustment [#{entry_id}]")
        }
    })
}

pub fn public_void_line(
    entry_id: impl std::fmt::Display,
    voider: impl std::fmt::Display,
    voided_at: impl std::fmt::Display,
    original_summary: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(
                f,
                "[#{entry_id}] {voider} が {voided_at} に取り消し: {original_summary}"
            )
        } else {
            write!(
                f,
                "[#{entry_id}] {voider} voided at {voided_at}: {original_summary}"
            )
        }
    })
}

pub fn public_date_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "日付: {value}")
        } else {
            write!(f, "Date: {value}")
        }
    })
}

pub fn public_payer_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "支払者: {value}")
        } else {
            write!(f, "Payer: {value}")
        }
    })
}

pub fn public_amount_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "金額: {value}円")
        } else {
            write!(f, "Amount: {value} JPY")
        }
    })
}

pub const PUBLIC_PARTICIPANTS_HEADING: &str = if cfg!(feature = "ja") {
    "参加者:"
} else {
    "Participants:"
};

pub fn public_note_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "メモ: {value}")
        } else {
            write!(f, "Note: {value}")
        }
    })
}

pub fn public_recorded_by_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "記録者: {value}")
        } else {
            write!(f, "Recorded by: {value}")
        }
    })
}

pub fn public_recorded_at_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "記録日時: {value}")
        } else {
            write!(f, "Recorded at: {value}")
        }
    })
}

pub const PUBLIC_TRANSFER_HEADING: &str = if cfg!(feature = "ja") {
    "送金予定:"
} else {
    "Planned transfers:"
};

pub fn public_confirmed_by_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "確定者: {value}")
        } else {
            write!(f, "Confirmed by: {value}")
        }
    })
}

pub const PUBLIC_VOID_PRESERVED_LINE: &str = if cfg!(feature = "ja") {
    "元の記録は 取り消し済み として残ります。"
} else {
    "The original record remains visible as voided."
};

pub fn public_seal_line(
    actor: impl std::fmt::Display,
    through_entry_id: impl std::fmt::Display,
    through_summary: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(
                f,
                "確認 {actor}: [#{through_entry_id}] {through_summary} まで確定"
            )
        } else {
            write!(
                f,
                "Confirmed by {actor}: [#{through_entry_id}] through {through_summary}"
            )
        }
    })
}

pub fn public_balance_adjustment_line(
    actor: impl std::fmt::Display,
    reason: impl std::fmt::Display,
    impact_summary: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "残高補正 {actor}: {reason} ({impact_summary})")
        } else {
            write!(f, "Balance adjustment {actor}: {reason} ({impact_summary})")
        }
    })
}

pub fn balance_row_receive(
    label: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "- {label}: 受け取り {amount}円")
        } else {
            write!(f, "- {label}: receive {amount} JPY")
        }
    })
}

pub fn balance_row_pay(
    label: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "- {label}: 支払い {amount}円")
        } else {
            write!(f, "- {label}: pay {amount} JPY")
        }
    })
}

pub fn participant_share_row(
    label: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "- {label}: {amount}円")
        } else {
            write!(f, "- {label}: {amount} JPY")
        }
    })
}

pub fn settlement_transfer_row(
    from: impl std::fmt::Display,
    to: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "- {from} -> {to} {amount}円")
        } else {
            write!(f, "- {from} -> {to} {amount} JPY")
        }
    })
}

pub fn void_candidate_expense_summary(
    date: impl std::fmt::Display,
    payer: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{date} {payer} の支払い {amount}円")
        } else {
            write!(f, "{date} expense by {payer} {amount} JPY")
        }
    })
}

pub fn void_candidate_settlement_summary(
    date: impl std::fmt::Display,
    from: impl std::fmt::Display,
    to: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{date} 清算 {from}->{to} {amount}円")
        } else {
            write!(f, "{date} settlement {from}->{to} {amount} JPY")
        }
    })
}

pub fn additional_items(count: usize) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "ほか{count}件")
        } else {
            write!(f, "{count} more")
        }
    })
}

pub fn void_candidate_note_excerpt(note: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "メモ: {note}")
        } else {
            write!(f, "Note: {note}")
        }
    })
}

pub fn void_candidate_void_summary(
    date: impl std::fmt::Display,
    voider: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{date} 取り消し {voider}")
        } else {
            write!(f, "{date} void by {voider}")
        }
    })
}

pub fn void_candidate_seal_summary(
    date: impl std::fmt::Display,
    actor: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{date} 確認 {actor}")
        } else {
            write!(f, "{date} seal by {actor}")
        }
    })
}

pub fn void_candidate_adjustment_summary(
    date: impl std::fmt::Display,
    impact_summary: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{date} 残高補正 {impact_summary}")
        } else {
            write!(f, "{date} balance adjustment {impact_summary}")
        }
    })
}

pub const SEALED_RANGE_NONE: &str = if cfg!(feature = "ja") {
    "確認済み: なし"
} else {
    "Confirmed: none"
};

pub fn sealed_range_line(
    through_entry_id: impl std::fmt::Display,
    effective_date: impl std::fmt::Display,
    summary: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(
                f,
                "確認済み: [#{through_entry_id}] {effective_date} {summary} まで"
            )
        } else {
            write!(
                f,
                "Confirmed: [#{through_entry_id}] through {effective_date} {summary}"
            )
        }
    })
}

pub fn sealed_expense_summary(
    payer: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{payer} の支払い {amount}円")
        } else {
            write!(f, "expense by {payer} {amount} JPY")
        }
    })
}

pub fn sealed_settlement_summary(
    from: impl std::fmt::Display,
    to: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "清算 {from} -> {to}")
        } else {
            write!(f, "settlement {from} -> {to}")
        }
    })
}

pub const SEALED_VOID_SUMMARY: &str = if cfg!(feature = "ja") {
    "取り消し"
} else {
    "void"
};

pub fn sealed_prior_seal_summary(actor: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "確認 {actor} (これより前が確定)")
        } else {
            write!(f, "seal {actor} (earlier history is confirmed)")
        }
    })
}

pub fn sealed_adjustment_summary(actor: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "残高補正 {actor} (管理上の調整)")
        } else {
            write!(f, "balance adjustment {actor} (administrative correction)")
        }
    })
}

pub fn balance_adjustment_row(
    actor: impl std::fmt::Display,
    reason: impl std::fmt::Display,
    impact_summary: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "残高補正 {actor}: {reason} ({impact_summary})")
        } else {
            write!(f, "Balance adjustment {actor}: {reason} ({impact_summary})")
        }
    })
}

pub fn impact_summary_receive(
    label: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{label} 受け取り {amount}円")
        } else {
            write!(f, "{label} receive {amount} JPY")
        }
    })
}

pub fn impact_summary_pay(
    label: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{label} 支払い {amount}円")
        } else {
            write!(f, "{label} pay {amount} JPY")
        }
    })
}

pub const READ_UNCERTAIN_WRITE_ADVISORY: &str = if cfg!(feature = "ja") {
    "ℹ️ この台帳は現在書き込み確認中です。記録・取り消し・清算は一時的に制限されています。"
} else {
    "ℹ️ This ledger is currently verifying a write. Recording, voiding, and settlement are temporarily restricted."
};

pub const UNKNOWN_LEDGER_FORMAT_MESSAGE: &str = if cfg!(feature = "ja") {
    "この台帳は新しい形式で記録されています。Bot を更新してください。"
} else {
    "This ledger was recorded in a newer format. Please update the bot."
};

pub const LEDGER_RETRYABLE_LOAD_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳を読み込めませんでした。通信やDiscord側の一時的な問題の可能性があります。もう一度試してください。続く場合は管理者に連絡してください。"
} else {
    "Failed to load the ledger. This may be a temporary network or Discord-side issue. Please try again. If it continues, contact an administrator."
};

pub const LEDGER_LOAD_TIMEOUT_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳の読み込みがタイムアウトしました。少し待ってからもう一度試してください。続く場合は管理者に連絡してください。"
} else {
    "Loading the ledger timed out. Please wait a moment and try again. If it continues, contact an administrator."
};

pub const LEDGER_PERMISSION_FAILED_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳を確認する権限または設定を確認できませんでした。管理者に連絡してください。"
} else {
    "The bot could not verify the permissions or configuration needed to open this ledger. Please contact an administrator."
};

pub const LEDGER_ATTACHMENT_DECODE_FAILED_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳の一部が破損している可能性があります (添付ファイルが見つからないか、このボットで読めない形式です)。運用担当者に連絡してください。"
} else {
    "Part of the ledger may be corrupted (the attachment is missing or in a format this bot cannot read). Please contact an operator."
};

pub const LEDGER_INTEGRITY_FAILED_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳の整合性検証に失敗しました (履歴のつながりが壊れているか、このボットで安全に扱えない状態です)。運用担当者に連絡してください。"
} else {
    "Ledger integrity verification failed (the history chain is broken or is in a state this bot cannot safely handle). Please contact an operator."
};

pub const LEDGER_THREAD_PREPARE_FAILED_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳スレッドを準備できませんでした。少し待ってからもう一度試してください。解決しない場合は管理者に連絡してください。"
} else {
    "Failed to prepare the ledger thread. Please wait a moment and try again. If it does not recover, contact an administrator."
};

pub const BALANCES_HEADING: &str = if cfg!(feature = "ja") {
    "残高"
} else {
    "Balances"
};

pub const PARTICIPANTS_HEADING: &str = if cfg!(feature = "ja") {
    "参加者"
} else {
    "Participants"
};

pub const RECENT_ENTRIES_HEADING: &str = if cfg!(feature = "ja") {
    "最近の記録"
} else {
    "Recent entries"
};

pub const RECENT_ENTRIES_NONE: &str = if cfg!(feature = "ja") {
    "記録なし"
} else {
    "No entries"
};

pub fn recent_entry_line(
    entry_id: impl std::fmt::Display,
    summary: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| write!(f, "[#{entry_id}] {summary}"))
}

pub const VOIDED_HEADING: &str = if cfg!(feature = "ja") {
    "取り消し済み"
} else {
    "Voided"
};

pub const CONFIRMED_HEADING: &str = if cfg!(feature = "ja") {
    "確認済み"
} else {
    "Confirmed"
};

pub const SETTLEMENT_PLAN_HEADING: &str = if cfg!(feature = "ja") {
    "送金予定"
} else {
    "Planned transfers"
};

pub const BALANCES_EXPLAINER: &str = if cfg!(feature = "ja") {
    "確認済み履歴と残高補正を含む現在差額"
} else {
    "Current balance including confirmed history and balance adjustments"
};

pub const PARTICIPANTS_EXPLAINER: &str = if cfg!(feature = "ja") {
    "これまで記録に出た人"
} else {
    "Members who have appeared in recorded history"
};

pub const REVIEW_ZERO_BALANCES: &str = if cfg!(feature = "ja") {
    "残高なし"
} else {
    "No balances"
};

pub const LEDGER_ZERO_BALANCES: &str = if cfg!(feature = "ja") {
    "残高なし (新しい支出は 記録する ボタン (/expense コマンド) で記録できます。)"
} else {
    "No balances (You can record a new expense with the Record button or /expense.)"
};

pub const VOIDED_NONE: &str = if cfg!(feature = "ja") {
    "取り消し済みなし"
} else {
    "No voided records"
};

pub const LEDGER_EMPTY_STATE: &str = if cfg!(feature = "ja") {
    "まだ記録がありません。/expense または 記録する ボタンで最初の支出を記録してみましょう。"
} else {
    "No records exist yet. Try recording the first expense with /expense or the Record button."
};

pub const REVIEW_PARENT_EMPTY_STATE: &str = if cfg!(feature = "ja") {
    "まだ記録がありません。/expense または 記録する ボタンで最初の支出を記録してみましょう。"
} else {
    "No records exist yet. Try recording the first expense with /expense or the Record button."
};

pub const REVIEW_THREAD_EMPTY_STATE: &str = if cfg!(feature = "ja") {
    "まだ記録がありません。親チャンネルで /expense または 記録する ボタンから最初の支出を記録してみましょう。"
} else {
    "No records exist yet. Try recording the first expense from the parent channel with /expense or the Record button."
};

pub const VOID_CONFIRM_APPEND_ONLY_LINE: &str = if cfg!(feature = "ja") {
    "-# 元の記録は取り消し済みとして残ります。"
} else {
    "-# The original record remains visible as voided."
};

pub fn void_confirm_date_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "日付: {value}")
        } else {
            write!(f, "Date: {value}")
        }
    })
}

pub fn void_confirm_target_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "対象: {value}")
        } else {
            write!(f, "Target: {value}")
        }
    })
}

pub fn void_confirm_amount_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "金額: {value}円")
        } else {
            write!(f, "Amount: {value} JPY")
        }
    })
}

pub fn void_confirm_note_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "メモ: {value}")
        } else {
            write!(f, "Note: {value}")
        }
    })
}

pub fn void_confirm_expense_target(payer: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{payer} の支払い")
        } else {
            write!(f, "expense by {payer}")
        }
    })
}

pub fn void_confirm_settlement_target(
    from: impl std::fmt::Display,
    to: impl std::fmt::Display,
    amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "清算 {from}->{to} {amount}円")
        } else {
            write!(f, "settlement {from}->{to} {amount} JPY")
        }
    })
}

pub const VOID_MISSING_SELECTION_ERROR: &str = if cfg!(feature = "ja") {
    "対象を選択してください。"
} else {
    "Select a target."
};

pub const VOID_STALE_TARGET_REASON_VOIDED: &str = if cfg!(feature = "ja") {
    "他のユーザーが取り消しました"
} else {
    "another user already voided it"
};

pub const VOID_STALE_TARGET_REASON_SEALED: &str = if cfg!(feature = "ja") {
    "確認済み範囲に入りました"
} else {
    "it entered the confirmed range"
};

pub const VOID_STALE_TARGET_REASON_EXCLUDED: &str = if cfg!(feature = "ja") {
    "候補から外れました"
} else {
    "it left the candidate set"
};

pub fn void_stale_target_error(reason: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(
                f,
                "対象がもう取り消せません ({reason})。もう一度選んでください。"
            )
        } else {
            write!(
                f,
                "That target can no longer be voided ({reason}). Choose again."
            )
        }
    })
}

pub const VOID_SUCCESS_ACK: &str = if cfg!(feature = "ja") {
    "取り消しました。"
} else {
    "Voided."
};

pub const VOID_SUCCESS_EFFECT: &str = if cfg!(feature = "ja") {
    "この取り消しで元の記録の影響は打ち消されます。正しい内容が必要なら記録し直してください。"
} else {
    "This void cancels the original record's effect. Re-record the correct contents if needed."
};

pub fn void_success_thread_line(thread_mention: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "台帳スレッドで確認できます: {thread_mention}")
        } else {
            write!(f, "Check it in the ledger thread: {thread_mention}")
        }
    })
}

pub const VOID_SUCCESS_THREAD_LINE_WITHOUT_MENTION: &str = if cfg!(feature = "ja") {
    "台帳スレッドで確認できます。"
} else {
    "Check it in the ledger thread."
};

pub const VOID_EMPTY_STATE: &str = if cfg!(feature = "ja") {
    "まだ記録がありません。/expense または 記録する ボタンで最初の支出を記録してみましょう。"
} else {
    "No records exist yet. Try recording the first expense with /expense or the Record button."
};

pub const VOID_NO_CANDIDATE_STATE: &str = if cfg!(feature = "ja") {
    "取り消せる対象がありません。確認済みや既に取り消した記録は対象外です。/ledger または 台帳 で状態を確認してください。"
} else {
    "No voidable targets exist. Confirmed or already voided records are excluded. Check /ledger or the ledger panel."
};

pub const VOID_WINDOW_EMPTY_STATE: &str = if cfg!(feature = "ja") {
    "対象がありません (このUIで選べるのは新しい25件までです。古い記録はこのUIから取り消せません。運用担当者に連絡してください)。"
} else {
    "No target is available (this UI can only choose from the latest 25 entries; older records require operator help)."
};

pub fn void_window_handoff_line(reference_hint: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(
                f,
                "連絡時は対象の公開台帳メッセージのリンク、または控えている {reference_hint} と、わかる範囲の日時・支払者・金額・メモ抜粋を伝えてください。"
            )
        } else {
            write!(
                f,
                "When contacting an operator, share the public ledger message link or saved {reference_hint}, plus the known date, payer, amount, and note excerpt."
            )
        }
    })
}

pub const REVIEW_PREVIEW_INSTRUCTION: &str = if cfg!(feature = "ja") {
    "-# これはプレビューです。10分以内に、作成した本人だけが確定できます。あとから同じ台帳でプレビューを作り直すと、最新のプレビューだけが確定対象になります。"
} else {
    "-# This is a preview. Within 10 minutes, only the creator can confirm. If a newer preview is created for the same ledger, only the newest remains confirmable."
};

pub const REVIEW_SETTLE_BUTTON_LABEL: &str = if cfg!(feature = "ja") {
    "確定する"
} else {
    "Confirm"
};

pub const REVIEW_PREVIEW_BLOCKED_INSTRUCTION: &str = if cfg!(feature = "ja") {
    "この台帳は書き込み状態を確認中です。台帳スレッドを確認し、復旧後に /settle を実行してください。"
} else {
    "This ledger is currently verifying a write state. Check the ledger thread, then run /settle after recovery."
};

pub const REVIEW_PREVIEW_REQUIRED_MESSAGE: &str = if cfg!(feature = "ja") {
    "先に清算確認でプレビューを作成してください。"
} else {
    "Create a settlement preview first."
};

pub const REVIEW_RENDER_FAILED_MESSAGE: &str = if cfg!(feature = "ja") {
    "清算確認を表示できませんでした。"
} else {
    "Failed to display the settlement preview."
};

pub const STALE_SETTLEMENT_PREVIEW_MESSAGE: &str = if cfg!(feature = "ja") {
    "プレビューの有効期限が切れました。清算確認からやり直してください。"
} else {
    "The preview expired. Start from the settlement review again."
};

pub const SETTLEMENT_PREVIEW_REPLACED_MESSAGE: &str = if cfg!(feature = "ja") {
    "前の清算プレビューを新しい内容で置き換えました。"
} else {
    "The previous settlement preview was replaced with the new one."
};

pub const SETTLEMENT_ALREADY_NOT_NEEDED_MESSAGE: &str = if cfg!(feature = "ja") {
    "すでに清算は不要です。"
} else {
    "Settlement is already unnecessary."
};

pub const SETTLEMENT_PREVIEW_NOT_SAVED_MESSAGE: &str = if cfg!(feature = "ja") {
    "プレビューは保存されていません。/settle は不要です。"
} else {
    "The preview was not saved. /settle is unnecessary."
};

pub const SETTLEMENT_CONFIRMATION_FAILED_MESSAGE: &str = if cfg!(feature = "ja") {
    "確認に失敗しました。清算確認からやり直してください。"
} else {
    "Confirmation failed. Start from the settlement review again."
};

pub const SETTLEMENT_PREVIEW_NOT_DELIVERED_MESSAGE: &str = if cfg!(feature = "ja") {
    "プレビューが届かなかったようです。清算確認からやり直してください。"
} else {
    "The preview was not delivered. Start from the settlement review again."
};

pub fn panel_thread_cue_known(thread_mention: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "台帳スレッド: {thread_mention}")
        } else {
            write!(f, "Ledger thread: {thread_mention}")
        }
    })
}

pub const UNCERTAIN_WRITE_BLOCK_MESSAGE: &str = if cfg!(feature = "ja") {
    "この台帳の書き込み状態が確認できません。しばらく待ってからもう一度お試しください。"
} else {
    "The write state of this ledger could not be confirmed. Please wait a bit and try again."
};

pub const UNCERTAIN_WRITE_INPUT_PRESERVED_MESSAGE: &str = if cfg!(feature = "ja") {
    "入力中の内容は10分間保持されます。"
} else {
    "Your in-progress input will be kept for 10 minutes."
};

pub const UNCERTAIN_WRITE_PREVIEW_PRESERVED_MESSAGE: &str = if cfg!(feature = "ja") {
    "保留中のプレビューは作成から10分間有効です。"
} else {
    "The pending preview stays valid for 10 minutes from creation."
};

pub fn abandoned_uncertain_write_message(
    last_known_summary: impl std::fmt::Display,
) -> impl std::fmt::Display {
    struct Message<T>(T);

    impl<T: std::fmt::Display> std::fmt::Display for Message<T> {
        fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if cfg!(feature = "ja") {
                write!(
                    formatter,
                    "前回の送信結果を自動確認できませんでした。Discord の台帳スレッドで重複がないことを確認してから解除してください。\n前回の内容: {}",
                    self.0
                )
            } else {
                write!(
                    formatter,
                    "The previous send result could not be verified automatically. Check the Discord ledger thread for duplicates before clearing the block.\nPrevious content: {}",
                    self.0
                )
            }
        }
    }

    Message(last_known_summary)
}

pub const ABANDONED_UNCERTAIN_WRITE_ACKNOWLEDGE_LABEL: &str = if cfg!(feature = "ja") {
    "確認したのでやり直す"
} else {
    "Checked, try again"
};

pub const ABANDONED_UNCERTAIN_WRITE_ACKNOWLEDGED_MESSAGE: &str = if cfg!(feature = "ja") {
    "未確定の送信を確認済みとして解除しました。元の操作からもう一度実行してください。"
} else {
    "The uncertain send block was cleared after acknowledgement. Run the original action again."
};

pub const PANEL_RECORD_BUTTON_LABEL: &str = if cfg!(feature = "ja") {
    "記録する"
} else {
    "Record"
};

pub const PANEL_REVIEW_BUTTON_LABEL: &str = if cfg!(feature = "ja") {
    "清算確認"
} else {
    "Settlement Preview"
};

pub const PANEL_LEDGER_BUTTON_LABEL: &str = if cfg!(feature = "ja") {
    "台帳"
} else {
    "Ledger"
};

pub const PANEL_VOID_BUTTON_LABEL: &str = if cfg!(feature = "ja") {
    "取り消し"
} else {
    "Void"
};

pub const PANEL_POST_RETRY_MESSAGE: &str = if cfg!(feature = "ja") {
    "パネルを投稿できませんでした。少し待ってからもう一度試してください。続く場合は管理者に連絡してください。"
} else {
    "The panel could not be posted. Please wait a moment and try again. If it keeps happening, contact an administrator."
};

pub const PANEL_RENDER_RETRY_MESSAGE: &str = if cfg!(feature = "ja") {
    "パネルを表示できませんでした。少し待ってからもう一度試してください。続く場合は管理者に連絡してください。"
} else {
    "The panel could not be rendered. Please wait a moment and try again. If it keeps happening, contact an administrator."
};

pub const BOT_CANNOT_OPERATE_PARENT_CHANNEL_MESSAGE: &str = if cfg!(feature = "ja") {
    "ボットが親チャンネルを操作できません。チャンネルまたはロールの権限を管理できる管理者にBotの必要な権限を確認してもらってください。"
} else {
    "The bot cannot operate on the parent channel. Ask an administrator who can manage channel or role permissions to verify the bot's required permissions."
};

pub const DUPLICATE_THREAD_BLOCKED_MESSAGE_AUTHORITATIVE: &str = if cfg!(feature = "ja") {
    "台帳スレッドが複数見つかりました。スレッド管理権限を持つ管理者に重複する台帳スレッドを削除してもらってください (1つだけ残してください)。"
} else {
    "Multiple ledger threads were found. Ask an administrator with thread management permission to delete the duplicates and leave only one."
};

pub const DUPLICATE_THREAD_BLOCKED_MESSAGE_UNRESOLVED: &str = if cfg!(feature = "ja") {
    "台帳スレッドが複数見つかりました。どれを残すべきか自動では判断できません。スレッド管理権限を持つ運用担当者に重複する台帳スレッドと上記の復旧用の参照を確認してもらってください。"
} else {
    "Multiple ledger threads were found and walicord cannot determine which one to keep automatically. Ask an operator with thread management permission to review the duplicate threads and recovery references above."
};

pub const DUPLICATE_THREAD_RECOVERY_GUIDANCE_AUTHORITATIVE: &str = if cfg!(feature = "ja") {
    "管理者には残すべき台帳スレッドと上記の復旧用の参照を伝えてください。"
} else {
    "Tell the administrator which ledger thread should be kept and share the recovery references above."
};

pub const DUPLICATE_THREAD_RECOVERY_GUIDANCE_UNRESOLVED: &str = if cfg!(feature = "ja") {
    "運用担当者には対象のチャンネルまたは台帳スレッドと上記の復旧用の参照を伝えてください。"
} else {
    "Tell the operator which channel or ledger thread is affected and share the recovery references above."
};

pub const DAMAGED_CANDIDATE_BLOCKED_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳スレッドが破損している可能性があります。運用担当者が置き換えてください。"
} else {
    "The ledger thread may be damaged. Ask an operator to replace it."
};

pub const DAMAGED_CANDIDATE_RECOVERY_GUIDANCE: &str = if cfg!(feature = "ja") {
    "運用担当者には対象のチャンネルまたは台帳スレッド、発生時刻、上記の復旧用の参照を伝えてください。"
} else {
    "Tell the operator which channel or ledger thread is affected, when it happened, and share the recovery reference above."
};

pub fn outside_tracked_channel_message_with_hint(
    tracked_parent_mention: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(
                f,
                "このチャンネルは台帳の対象ではありません。{tracked_parent_mention} で実行してください。"
            )
        } else {
            write!(
                f,
                "This channel is not tracked. Run the command in {tracked_parent_mention}."
            )
        }
    })
}

pub const OUTSIDE_TRACKED_CHANNEL_MESSAGE_GENERIC: &str = if cfg!(feature = "ja") {
    "このチャンネルは台帳の対象ではありません。記録用チャンネルで実行してください。わからない場合は管理者に確認してください。"
} else {
    "This channel is not tracked. Run the command in a ledger parent channel. Ask an administrator if you are not sure which channel to use."
};

pub const ARCHIVED_THREAD_RECOVERY_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳スレッドがアーカイブされています。スレッド管理権限を持つ管理者に Discord でスレッドを開いてアーカイブを解除してもらってください。"
} else {
    "The ledger thread is archived. Ask an administrator with thread-management permission to open it in Discord and remove the archive state."
};

pub const OPEN_PARENT_CHANNEL_LABEL: &str = if cfg!(feature = "ja") {
    "親チャンネルを開く"
} else {
    "Open Parent Channel"
};

pub const OPEN_LEDGER_THREAD_LABEL: &str = if cfg!(feature = "ja") {
    "台帳スレッドを開く"
} else {
    "Open Ledger Thread"
};

pub const LEDGER_REFRESH_ADMIN_ONLY_MESSAGE: &str = if cfg!(feature = "ja") {
    "このコマンドは管理者のみ実行できます。必要な場合は管理者に /ledger-refresh の実行を依頼してください。"
} else {
    "Only administrators can run this command. Ask an administrator to run /ledger-refresh if needed."
};

pub const LEDGER_REFRESH_NO_THREAD_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳スレッドはまだありません。最初の記録時に作成されます。"
} else {
    "There is no ledger thread yet. It will be created when the first record is written."
};

pub const LEDGER_REFRESH_READY_MESSAGE: &str = if cfg!(feature = "ja") {
    "台帳スレッドの状態を更新しました。利用できます。"
} else {
    "The ledger thread state has been refreshed and is ready to use."
};

pub const LEDGER_REFRESH_UNCERTAIN_WRITE_MESSAGE: &str = if cfg!(feature = "ja") {
    "前回の書き込み結果を確認中です。新しい書き込みはまだ受け付けられません。しばらく待ってからもう一度確認してください。"
} else {
    "The previous write result is still being checked. New writes cannot be accepted yet. Please wait a moment and check again."
};

pub const GUILD_ONLY_COMMAND_MESSAGE: &str = if cfg!(feature = "ja") {
    "このコマンドはサーバー内でのみ使えます。"
} else {
    "This command can only be used inside a server."
};

pub const EXPENSE_SESSION_EXPIRED_MESSAGE: &str = if cfg!(feature = "ja") {
    "セッションの有効期限が切れました (10分間操作がない場合に切れます)。/expense からやり直してください。パネルを使う場合は 記録する ボタンから始められます。"
} else {
    "The session has expired (it expires after 10 minutes of inactivity). Start again from /expense. If you use the panel, begin with the Record button."
};

pub const EXPENSE_SESSION_WRONG_ACTOR_MESSAGE: &str = if cfg!(feature = "ja") {
    "この入力は他のユーザーのものです。自分で始めるには /expense または 記録する を使ってください。"
} else {
    "This expense draft belongs to another user. Start your own with /expense or Record."
};

pub const EXPENSE_SESSION_REPLACED_MESSAGE: &str = if cfg!(feature = "ja") {
    "前の入力を新しい入力で置き換えました。"
} else {
    "The previous expense draft was replaced with the new input."
};

pub const STALE_INTERACTION_MESSAGE: &str = if cfg!(feature = "ja") {
    "この操作は期限切れです。もう一度やり直してください。"
} else {
    "This interaction has expired. Please try again."
};

pub fn page_indicator(current: usize, total: usize) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "ページ {current}/{total}")
        } else {
            write!(f, "Page {current}/{total}")
        }
    })
}

pub fn page_range_indicator(start: usize, end: usize, total: usize) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "{start}-{end} / {total}人")
        } else {
            write!(f, "{start}-{end} / {total}")
        }
    })
}

pub const SNAPSHOT_NOTICE: &str = if cfg!(feature = "ja") {
    "-# この表示は固定スナップショットです。更新するには開き直してください。"
} else {
    "-# This view is a fixed snapshot. Reopen it to refresh."
};

pub const STALE_LEDGER_PAGE_MESSAGE: &str = if cfg!(feature = "ja") {
    "この表示は期限切れです。/ledger または 台帳 で開き直してください。"
} else {
    "This view has expired. Reopen it with /ledger or the ledger panel."
};

pub const STALE_REVIEW_PAGE_MESSAGE: &str = if cfg!(feature = "ja") {
    "この表示は期限切れです。台帳スレッドの /review または親チャンネルの 清算確認 で開き直してください。"
} else {
    "This view has expired. Reopen it from /review in the ledger thread or the parent channel's settlement button."
};

pub const STALE_VOID_PAGE_MESSAGE: &str = if cfg!(feature = "ja") {
    "この表示は期限切れです。/void または 取り消し で開き直してください。"
} else {
    "This view has expired. Reopen it with /void or the Void button."
};

pub const VOID_SESSION_WRONG_ACTOR_MESSAGE: &str = if cfg!(feature = "ja") {
    "この取り消し操作は他のユーザーのものです。自分で始めるには /void または 取り消し を使ってください。"
} else {
    "This void operation belongs to another user. Start your own with /void or Void."
};

pub const VOID_SESSION_REPLACED_MESSAGE: &str = if cfg!(feature = "ja") {
    "前の取り消し操作を新しい入力で置き換えました。"
} else {
    "The previous void operation was replaced with the new input."
};

pub const NO_LEDGER_THREAD_YET_NOTE: &str = if cfg!(feature = "ja") {
    "-# まだ台帳スレッドはありません。最初の記録後に全件確認できます。"
} else {
    "-# There is no ledger thread yet. You can review everything after the first record is written."
};

pub const EXPENSE_STEP_TITLE_PARTICIPANTS: &str = if cfg!(feature = "ja") {
    "参加者"
} else {
    "Participants"
};

pub const EXPENSE_STEP_TITLE_CONFIRM: &str = if cfg!(feature = "ja") {
    "最終確認"
} else {
    "Final Confirmation"
};

pub const EXPENSE_TO_CONFIRM_LABEL: &str = if cfg!(feature = "ja") {
    "確認へ"
} else {
    "To Confirmation"
};

pub const EXPENSE_RECORD_LABEL: &str = if cfg!(feature = "ja") {
    "記録する"
} else {
    "Record"
};

pub const EXPENSE_MODAL_TITLE: &str = if cfg!(feature = "ja") {
    "支出を記録"
} else {
    "Record Expense"
};

pub const EXPENSE_MODAL_AMOUNT_LABEL: &str = if cfg!(feature = "ja") {
    "金額"
} else {
    "Amount"
};

pub const EXPENSE_MODAL_NOTE_LABEL: &str = if cfg!(feature = "ja") {
    "メモ"
} else {
    "Note"
};

pub const EXPENSE_MODAL_DATE_LABEL: &str = if cfg!(feature = "ja") {
    "日付"
} else {
    "Date"
};

pub const EXPENSE_PAYER_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "支払者を選択"
} else {
    "Select payer"
};

pub const EXPENSE_BASIC_INFO_EDIT_LABEL: &str = if cfg!(feature = "ja") {
    "基本情報修正"
} else {
    "Edit Info"
};

pub const EXPENSE_WEIGHT_EDIT_LABEL: &str = if cfg!(feature = "ja") {
    "重みを編集"
} else {
    "Edit Weights"
};

pub const EXPENSE_REVISE_LABEL: &str = if cfg!(feature = "ja") {
    "選び直す"
} else {
    "Reselect"
};

pub const EXPENSE_CANCEL_LABEL: &str = if cfg!(feature = "ja") {
    "キャンセル"
} else {
    "Cancel"
};

pub const EXPENSE_CANCELLED_MESSAGE: &str = if cfg!(feature = "ja") {
    "操作をキャンセルしました。"
} else {
    "The operation was cancelled."
};

pub const EXPENSE_MODAL_AMOUNT_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "例: 1500"
} else {
    "Example: 1500"
};

pub const EXPENSE_MODAL_NOTE_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "任意・200文字以内 (メンションや装飾は文字として表示されます)"
} else {
    "Optional, up to 200 characters (mentions and formatting are shown literally)"
};

pub const EXPENSE_MODAL_DATE_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "例: 2026-05-20 / 5/20 / 今日 / 昨日"
} else {
    "Example: 2026-05-20 / 5/20 / today / yesterday"
};

pub const PARTICIPANT_SOURCE_INDIVIDUAL_LABEL: &str = if cfg!(feature = "ja") {
    "個別選択"
} else {
    "Choose Individuals"
};

pub const PARTICIPANT_SOURCE_ROLE_LABEL: &str = if cfg!(feature = "ja") {
    "ロール選択"
} else {
    "Choose Roles"
};

pub const EXPENSE_SWITCH_TO_PAYER_LABEL: &str = if cfg!(feature = "ja") {
    "支払者変更"
} else {
    "Payer"
};

pub const EXPENSE_SUB_VIEW_PAYER_TITLE: &str = if cfg!(feature = "ja") {
    "支払者"
} else {
    "Payer"
};

pub const PARTICIPANT_SOURCE_INDIVIDUAL_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "参加者を選択"
} else {
    "Select participants"
};

pub const PARTICIPANT_SOURCE_ROLE_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "ロールを選択"
} else {
    "Select roles"
};

pub const MEMBER_SEARCH_MODAL_TITLE: &str = if cfg!(feature = "ja") {
    "参加者を検索"
} else {
    "Search Participants"
};

pub const MEMBER_SEARCH_INPUT_LABEL: &str = if cfg!(feature = "ja") {
    "表示名で検索"
} else {
    "Search by display name"
};

pub const MEMBER_SEARCH_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "例: 田中"
} else {
    "Example: Alice"
};

pub const ROLE_SEARCH_MODAL_TITLE: &str = if cfg!(feature = "ja") {
    "ロールを検索"
} else {
    "Search Roles"
};

pub const ROLE_SEARCH_INPUT_LABEL: &str = if cfg!(feature = "ja") {
    "ロール名で検索"
} else {
    "Search by role name"
};

pub const ROLE_SEARCH_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "例: 開発"
} else {
    "Example: Dev"
};

pub const PARTICIPANT_SOURCE_MEMBERS_LABEL: &str = if cfg!(feature = "ja") {
    "全員を追加"
} else {
    "Add All Members"
};

pub const PARTICIPANT_SOURCE_CLEAR_MEMBERS_LABEL: &str = if cfg!(feature = "ja") {
    "全員を外す"
} else {
    "Remove All Members"
};

pub const PICKER_PREVIOUS_PAGE_LABEL: &str = if cfg!(feature = "ja") {
    "前のページ"
} else {
    "Previous Page"
};

pub const PICKER_NEXT_PAGE_LABEL: &str = if cfg!(feature = "ja") {
    "次のページ"
} else {
    "Next Page"
};

pub const PICKER_SEARCH_LABEL: &str = if cfg!(feature = "ja") {
    "検索"
} else {
    "Search"
};

pub const PICKER_CLEAR_LABEL: &str = if cfg!(feature = "ja") {
    "クリア"
} else {
    "Clear"
};

pub const SEARCH_BLANK_ERROR: &str = if cfg!(feature = "ja") {
    "検索語を入力してください。"
} else {
    "Enter a search term."
};

pub const MEMBER_SEARCH_NOT_FOUND_ERROR: &str = if cfg!(feature = "ja") {
    "見つかりませんでした。"
} else {
    "No match found."
};

pub const ROLE_SEARCH_NOT_FOUND_ERROR: &str = if cfg!(feature = "ja") {
    "見つかりませんでした。"
} else {
    "No match found."
};

pub const WEIGHT_EDITOR_MODAL_TITLE: &str = if cfg!(feature = "ja") {
    "重みを編集"
} else {
    "Edit Weights"
};

pub const WEIGHT_EDITOR_INPUT_LABEL: &str = if cfg!(feature = "ja") {
    "ユーザー名 = 重み"
} else {
    "Username = Weight"
};

pub const WEIGHT_EDITOR_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "例: alice = 2"
} else {
    "Example: alice = 2"
};

pub const WEIGHT_EDITOR_TOO_MANY_MESSAGE: &str = if cfg!(feature = "ja") {
    "重み編集は40人までです。参加者を減らすか均等割りで記録してください。"
} else {
    "Weight editing supports up to 40 participants. Reduce the participants or record with equal weights."
};

pub const WEIGHT_EDITOR_PARSE_ERROR: &str = if cfg!(feature = "ja") {
    "ユーザー名 = 重み の形式で入力してください。例: alice = 2"
} else {
    "Enter as username = weight. Example: alice = 2"
};

pub const WEIGHT_EDITOR_RESET_LABEL: &str = if cfg!(feature = "ja") {
    "均等割りに戻す"
} else {
    "Reset to Equal Weights"
};

pub const EXPENSE_NOTE_NONE: &str = if cfg!(feature = "ja") {
    "なし"
} else {
    "none"
};

pub fn expense_search_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "検索: {value}")
        } else {
            write!(f, "Search: {value}")
        }
    })
}

pub const EXPENSE_INVALID_AMOUNT_MESSAGE: &str = if cfg!(feature = "ja") {
    "金額は 1 円以上の整数で入力してください。"
} else {
    "Enter an integer amount of at least 1 JPY."
};

pub const EXPENSE_INVALID_DATE_MESSAGE: &str = if cfg!(feature = "ja") {
    "日付の形式が正しくありません。例: 今日、昨日、5/1、2026-05-01"
} else {
    "The date format is invalid. Example: today, yesterday, 5/1, 2026-05-01"
};

pub const EXPENSE_NOTE_TOO_LONG_MESSAGE: &str = if cfg!(feature = "ja") {
    "メモは 200 文字以内で入力してください。"
} else {
    "Enter a note of at most 200 characters."
};

pub const EXPENSE_MODAL_RETRY_BUTTON_LABEL: &str = if cfg!(feature = "ja") {
    "入力を修正する"
} else {
    "Edit input"
};

pub const EXPENSE_RECORDED_MESSAGE: &str = if cfg!(feature = "ja") {
    "支出を記録しました。"
} else {
    "Recorded the expense."
};

pub fn expense_recorded_summary(
    amount: impl std::fmt::Display,
    date: impl std::fmt::Display,
    participant_count: usize,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(
                f,
                "金額: {amount}円 / 日付: {date} / 参加者: {participant_count}人"
            )
        } else {
            write!(
                f,
                "Amount: {amount} JPY / Date: {date} / Participants: {participant_count}"
            )
        }
    })
}

pub const EXPENSE_PARTICIPANTS_DRIFTED_CUE: &str = if cfg!(feature = "ja") {
    "対象者が更新されたため確認内容を更新しました。もう一度「記録する」を押してください。"
} else {
    "Participants changed since you confirmed; the page has been refreshed. Press 記録する again to record."
};

pub const SETTLEMENT_RECORDED_MESSAGE: &str = if cfg!(feature = "ja") {
    "清算を記録しました。"
} else {
    "Recorded the settlement."
};

pub const SETTLEMENT_PREVIEW_EXPIRED_OR_CONFIRMED_MESSAGE: &str = if cfg!(feature = "ja") {
    "プレビューの期限切れ・確定済み・作り直しにより無効です。必要であれば清算確認からやり直してください。"
} else {
    "The preview is no longer valid (expired, already confirmed, or replaced). Start from the settlement review if needed."
};

pub const SETTLEMENT_NO_TRANSFER_MESSAGE: &str = if cfg!(feature = "ja") {
    "この清算プランでは、送金を記録する必要がありません。"
} else {
    "This settlement plan does not require any transfer record."
};

pub fn expense_draft_amount_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "金額: {value}円")
        } else {
            write!(f, "Amount: {value} JPY")
        }
    })
}

pub fn expense_draft_date_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "日付: {value}")
        } else {
            write!(f, "Date: {value}")
        }
    })
}

pub fn expense_draft_note_line(value: impl std::fmt::Display) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "メモ: {value}")
        } else {
            write!(f, "Note: {value}")
        }
    })
}

pub fn individual_selection_prefix(count: usize) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "個別選択 {count}人")
        } else {
            write!(f, "Individual Selection ({count})")
        }
    })
}

pub fn expense_confirmation_share_row(
    display_name: impl std::fmt::Display,
    share_amount: impl std::fmt::Display,
    weight: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "- {display_name}: {share_amount}円 (×{weight})")
        } else {
            write!(f, "- {display_name}: {share_amount} JPY (x{weight})")
        }
    })
}

pub fn expense_confirmation_equal_row(
    display_name: impl std::fmt::Display,
    share_amount: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "- {display_name}: {share_amount}円")
        } else {
            write!(f, "- {display_name}: {share_amount} JPY")
        }
    })
}

pub fn expense_confirmation_zero_weight_row(
    display_name: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "- {display_name}: 0円 (×0)")
        } else {
            write!(f, "- {display_name}: 0 JPY (x0)")
        }
    })
}

pub fn expense_confirmation_rounded_zero_row(
    display_name: impl std::fmt::Display,
    weight: impl std::fmt::Display,
) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| {
        if cfg!(feature = "ja") {
            write!(f, "- {display_name} ×{weight} (端数で0)")
        } else {
            write!(f, "- {display_name} x{weight} (rounded to zero)")
        }
    })
}

pub const VOID_RESELECT_LABEL: &str = if cfg!(feature = "ja") {
    "選び直す"
} else {
    "Choose Again"
};

pub const VOID_NEXT_LABEL: &str = if cfg!(feature = "ja") {
    "確認へ"
} else {
    "To Confirmation"
};

pub const VOID_CANCEL_LABEL: &str = EXPENSE_CANCEL_LABEL;

pub const VOID_CONFIRM_LABEL: &str = if cfg!(feature = "ja") {
    "取り消す"
} else {
    "Void"
};

pub const VOID_CANCELLED_MESSAGE: &str = EXPENSE_CANCELLED_MESSAGE;

pub const VOID_IN_FLIGHT_COPY: &str = if cfg!(feature = "ja") {
    "取り消し中..."
} else {
    "Voiding..."
};

pub const VOID_WRONG_STAGE_COPY: &str = if cfg!(feature = "ja") {
    "取り消しボタンは確認画面から押してください。"
} else {
    "Press the void button from the confirmation screen."
};

pub const VOID_CONFIRMATION_TITLE: &str = if cfg!(feature = "ja") {
    "取り消し確認"
} else {
    "Void Confirmation"
};

pub const VOID_SELECT_PLACEHOLDER: &str = if cfg!(feature = "ja") {
    "取り消し対象を選ぶ"
} else {
    "Select a record to void"
};

pub const VOID_SUCCESS_TITLE: &str = if cfg!(feature = "ja") {
    "取り消し完了"
} else {
    "Void Complete"
};

pub const VOID_TARGET_UPDATED_MESSAGE: &str = if cfg!(feature = "ja") {
    "取り消し対象が更新されました。最新の表示でもう一度「取り消す」を押してください。"
} else {
    "The void target changed. Press Void again from the latest view."
};
