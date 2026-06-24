use super::{
    sanitizer::SafeLiteralText,
    surfaces::{SurfaceActionRow, SurfaceSelectMenu},
};
use walicord_application::ledger::LedgerEffectiveDate;
use walicord_i18n as i18n;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PickerSurfaceModel {
    pub title: String,
    pub page_indicator: Option<String>,
    pub range_indicator: Option<String>,
    pub snapshot_notice: Option<String>,
    pub helper_lines: Vec<String>,
    pub selected_summary: Option<String>,
    pub select_menu: Option<SurfaceSelectMenu>,
    pub action_rows: Vec<SurfaceActionRow>,
    pub ephemeral: bool,
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpenseStepTitle {
    Payer,
    Participants,
    Weight,
    Confirm,
}

#[cfg(test)]
impl ExpenseStepTitle {
    pub fn render(self) -> &'static str {
        match self {
            Self::Payer => i18n::EXPENSE_STEP_TITLE_PAYER,
            Self::Participants => i18n::EXPENSE_STEP_TITLE_PARTICIPANTS,
            Self::Weight => i18n::EXPENSE_STEP_TITLE_WEIGHT,
            Self::Confirm => i18n::EXPENSE_STEP_TITLE_CONFIRM,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseDraftSummary {
    pub amount: String,
    pub effective_date: LedgerEffectiveDate,
    pub note: Option<SafeLiteralText>,
}

impl ExpenseDraftSummary {
    pub fn render_lines(&self) -> [String; 3] {
        [
            i18n::expense_draft_amount_line(&self.amount).to_string(),
            i18n::expense_draft_date_line(self.effective_date).to_string(),
            i18n::expense_draft_note_line(
                self.note
                    .as_ref()
                    .map(SafeLiteralText::as_str)
                    .unwrap_or(i18n::EXPENSE_NOTE_NONE),
            )
            .to_string(),
        ]
    }
}

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PickerPageChrome {
    pub current_page: usize,
    pub total_pages: usize,
    pub range_start: usize,
    pub range_end: usize,
    pub total_items: usize,
}

#[cfg(test)]
impl PickerPageChrome {
    pub fn page_indicator(&self) -> String {
        i18n::page_indicator(self.current_page, self.total_pages).to_string()
    }

    pub fn range_indicator(&self) -> String {
        i18n::page_range_indicator(self.range_start, self.range_end, self.total_items).to_string()
    }

    pub fn snapshot_notice(&self) -> String {
        i18n::SNAPSHOT_NOTICE.to_owned()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseConfirmationParticipantRow {
    pub display_name: SafeLiteralText,
    pub share_amount: Option<String>,
    pub weight: u64,
}

impl ExpenseConfirmationParticipantRow {
    pub fn render(&self, show_weight: bool) -> String {
        if show_weight {
            match (self.weight, self.share_amount.as_deref()) {
                (0, _) => {
                    i18n::expense_confirmation_zero_weight_row(&self.display_name).to_string()
                }
                (_, Some("0")) | (_, None) => {
                    i18n::expense_confirmation_rounded_zero_row(&self.display_name, self.weight)
                        .to_string()
                }
                (_, Some(share_amount)) => i18n::expense_confirmation_share_row(
                    &self.display_name,
                    share_amount,
                    self.weight,
                )
                .to_string(),
            }
        } else {
            match self.share_amount.as_deref() {
                Some(share_amount) => {
                    i18n::expense_confirmation_equal_row(&self.display_name, share_amount)
                        .to_string()
                }
                None => {
                    i18n::expense_confirmation_rounded_zero_row(&self.display_name, self.weight)
                        .to_string()
                }
            }
        }
    }
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpenseForwardAction {
    Payer,
    Participants,
    Weight,
    Confirm,
}

#[cfg(test)]
impl ExpenseForwardAction {
    pub fn render(self) -> &'static str {
        match self {
            Self::Payer => i18n::EXPENSE_NEXT_LABEL,
            Self::Participants => i18n::EXPENSE_TO_WEIGHTS_LABEL,
            Self::Weight => i18n::EXPENSE_TO_CONFIRM_LABEL,
            Self::Confirm => i18n::EXPENSE_RECORD_LABEL,
        }
    }
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModalValidationFailure {
    Amount,
    NoteLength,
    Date,
}

#[cfg(test)]
pub fn first_invalid_modal_field(
    amount_valid: bool,
    note_valid: bool,
    date_valid: bool,
) -> Option<ModalValidationFailure> {
    if !amount_valid {
        return Some(ModalValidationFailure::Amount);
    }
    if !note_valid {
        return Some(ModalValidationFailure::NoteLength);
    }
    if !date_valid {
        return Some(ModalValidationFailure::Date);
    }
    None
}

pub fn individual_selection_title(count: usize, names: &[SafeLiteralText]) -> String {
    let base = i18n::individual_selection_prefix(count).to_string();
    summarize_selected_names(names)
        .map(|summary| format!("{base}: {summary}"))
        .unwrap_or(base)
}

#[cfg(test)]
pub fn payer_picker_utility_labels() -> [&'static str; 4] {
    [
        i18n::PICKER_PREVIOUS_PAGE_LABEL,
        i18n::PICKER_NEXT_PAGE_LABEL,
        i18n::PICKER_SEARCH_LABEL,
        i18n::PAYER_CLEAR_LABEL,
    ]
}

#[cfg(test)]
pub fn individual_picker_utility_labels() -> [&'static str; 4] {
    [
        i18n::PICKER_PREVIOUS_PAGE_LABEL,
        i18n::PICKER_NEXT_PAGE_LABEL,
        i18n::PICKER_SEARCH_LABEL,
        i18n::INDIVIDUAL_CLEAR_LABEL,
    ]
}

#[cfg(test)]
pub fn role_picker_utility_labels() -> [&'static str; 4] {
    [
        i18n::PICKER_PREVIOUS_PAGE_LABEL,
        i18n::PICKER_NEXT_PAGE_LABEL,
        i18n::PICKER_SEARCH_LABEL,
        i18n::ROLE_CLEAR_LABEL,
    ]
}

#[cfg(test)]
pub fn participant_source_entry_labels() -> [&'static str; 3] {
    [
        i18n::PARTICIPANT_SOURCE_INDIVIDUAL_LABEL,
        i18n::PARTICIPANT_SOURCE_ROLE_LABEL,
        i18n::PARTICIPANT_SOURCE_MEMBERS_LABEL,
    ]
}

#[cfg(test)]
pub fn participant_source_clear_labels() -> [&'static str; 2] {
    [
        i18n::PARTICIPANT_SOURCE_CLEAR_ROLES_LABEL,
        i18n::PARTICIPANT_SOURCE_CLEAR_MEMBERS_LABEL,
    ]
}

const SELECTED_SUMMARY_LABEL_LIMIT: usize = 32;

fn summarize_label(label: &SafeLiteralText) -> String {
    let actual = label.as_str().chars().count();
    if actual <= SELECTED_SUMMARY_LABEL_LIMIT {
        return label.as_str().to_owned();
    }

    let prefix: String = label
        .as_str()
        .chars()
        .take(SELECTED_SUMMARY_LABEL_LIMIT.saturating_sub(1))
        .collect();
    format!("{prefix}…")
}

pub fn summarize_selected_names(names: &[SafeLiteralText]) -> Option<String> {
    match names.len() {
        0 => None,
        1..=3 => Some(
            names
                .iter()
                .map(summarize_label)
                .collect::<Vec<_>>()
                .join(", "),
        ),
        _ => {
            let visible_names = names
                .iter()
                .take(3)
                .map(summarize_label)
                .collect::<Vec<_>>()
                .join(", ");
            Some(
                i18n::selected_name_summary_omitted(visible_names, names.len() - 3, names.len())
                    .to_string(),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        ExpenseConfirmationParticipantRow, ExpenseDraftSummary, ExpenseForwardAction,
        ExpenseStepTitle, ModalValidationFailure, PickerPageChrome, first_invalid_modal_field,
        individual_picker_utility_labels, individual_selection_title,
        participant_source_clear_labels, participant_source_entry_labels,
        payer_picker_utility_labels, role_picker_utility_labels, summarize_selected_names,
    };
    use crate::discord_ledger::{
        budgets::{validate_button_label, validate_component_placeholder},
        sanitizer::SafeLiteralText,
    };
    use walicord_application::ledger::LedgerEffectiveDate;
    use walicord_i18n as i18n;

    fn name(value: &str) -> SafeLiteralText {
        SafeLiteralText::from_roster_label(value).expect("name should remain valid")
    }

    fn date(value: &str) -> LedgerEffectiveDate {
        LedgerEffectiveDate::new(value).expect("date should remain valid")
    }

    #[test]
    fn selected_name_summary_omits_the_list_when_empty() {
        let actual = summarize_selected_names(&[]);

        assert_eq!(actual, None);
    }

    #[test]
    fn selected_name_summary_lists_up_to_three_names_without_an_omission_cue() {
        let actual = summarize_selected_names(&[name("Alice"), name("Bob"), name("Carol")]);

        assert_eq!(actual, Some("Alice, Bob, Carol".to_owned()));
    }

    #[test]
    fn selected_name_summary_uses_the_fixed_omission_cue_for_four_or_more_names() {
        let actual = summarize_selected_names(&[
            name("Alice"),
            name("Bob"),
            name("Carol"),
            name("Dave"),
            name("Erin"),
        ]);

        assert_eq!(
            actual,
            Some("Alice, Bob, Carol... 他 2名 (合計 5名)".to_owned())
        );
    }

    #[test]
    fn selected_name_summary_truncates_overlong_names_before_joining() {
        let actual = summarize_selected_names(&[name(&"a".repeat(40))]);

        assert_eq!(actual, Some(format!("{}…", "a".repeat(31))));
    }

    #[test]
    fn picker_page_chrome_uses_the_fixed_page_and_snapshot_copy() {
        let chrome = PickerPageChrome {
            current_page: 2,
            total_pages: 4,
            range_start: 26,
            range_end: 50,
            total_items: 73,
        };

        assert_eq!(chrome.page_indicator(), "ページ 2/4");
        assert_eq!(chrome.range_indicator(), "26-50 / 73人");
        assert_eq!(
            chrome.snapshot_notice(),
            "-# この表示は固定スナップショットです。更新するには開き直してください。"
        );
    }

    #[test]
    fn expense_step_titles_use_the_fixed_stage_framing() {
        assert_eq!(ExpenseStepTitle::Payer.render(), "1/4 支払者");
        assert_eq!(ExpenseStepTitle::Participants.render(), "2/4 参加者");
        assert_eq!(ExpenseStepTitle::Weight.render(), "3/4 重み");
        assert_eq!(ExpenseStepTitle::Confirm.render(), "4/4 確認");
    }

    #[test]
    fn expense_draft_summary_uses_the_fixed_three_line_layout() {
        let summary = ExpenseDraftSummary {
            amount: "1500".to_owned(),
            effective_date: date("2026-05-20"),
            note: Some(SafeLiteralText::from_note("ランチ").expect("note should remain non-blank")),
        };

        assert_eq!(
            summary.render_lines(),
            [
                "金額: 1500円".to_owned(),
                "日付: 2026-05-20".to_owned(),
                "メモ: ランチ".to_owned()
            ]
        );
    }

    #[test]
    fn individual_selection_title_appends_the_selected_name_summary() {
        let actual = individual_selection_title(4, &[name("Alice"), name("Bob"), name("Carol")]);

        assert_eq!(actual, "個別選択 4人: Alice, Bob, Carol");
    }

    #[test]
    fn confirmation_row_hides_weight_when_equal() {
        let row = ExpenseConfirmationParticipantRow {
            display_name: name("Alice"),
            share_amount: Some("500".to_owned()),
            weight: 1,
        };
        assert_eq!(row.render(false), "- Alice: 500円");
    }

    #[test]
    fn confirmation_row_shows_weight_when_requested() {
        let row = ExpenseConfirmationParticipantRow {
            display_name: name("Alice"),
            share_amount: Some("750".to_owned()),
            weight: 2,
        };
        assert_eq!(row.render(true), "- Alice: 750円 (×2)");
    }

    #[test]
    fn confirmation_row_shows_zero_weight() {
        let row = ExpenseConfirmationParticipantRow {
            display_name: name("Carol"),
            share_amount: None,
            weight: 0,
        };
        assert_eq!(row.render(true), "- Carol: 0円 (×0)");
    }

    #[test]
    fn modal_validation_precedence_is_amount_then_note_then_date() {
        assert_eq!(
            first_invalid_modal_field(false, false, false),
            Some(ModalValidationFailure::Amount)
        );
        assert_eq!(
            first_invalid_modal_field(true, false, false),
            Some(ModalValidationFailure::NoteLength)
        );
        assert_eq!(
            first_invalid_modal_field(true, true, false),
            Some(ModalValidationFailure::Date)
        );
        assert_eq!(first_invalid_modal_field(true, true, true), None);
    }

    #[test]
    fn picker_utility_rows_use_the_fixed_button_order() {
        assert_eq!(
            payer_picker_utility_labels(),
            ["前のページ", "次のページ", "検索", "支払者をクリア"]
        );
        assert_eq!(
            individual_picker_utility_labels(),
            ["前のページ", "次のページ", "検索", "個別選択をクリア"]
        );
        assert_eq!(
            role_picker_utility_labels(),
            ["前のページ", "次のページ", "検索", "ロール選択をクリア"]
        );
    }

    #[test]
    fn participant_source_controls_and_forward_labels_use_the_fixed_order() {
        assert_eq!(
            participant_source_entry_labels(),
            ["個別選択", "ロール選択", "全員を追加"]
        );
        assert_eq!(
            participant_source_clear_labels(),
            ["ロール選択をクリア", "全員を外す"]
        );
        assert_eq!(ExpenseForwardAction::Payer.render(), "次へ");
        assert_eq!(ExpenseForwardAction::Participants.render(), "重みへ");
        assert_eq!(ExpenseForwardAction::Weight.render(), "確認へ");
        assert_eq!(ExpenseForwardAction::Confirm.render(), "記録する");
    }

    #[test]
    fn fixed_picker_copy_routes_through_i18n() {
        assert_eq!(i18n::WEIGHT_EDITOR_RESET_LABEL, "均等割りに戻す");
        assert_eq!(i18n::SEARCH_BLANK_ERROR, "検索語を入力してください。");
        assert_eq!(
            i18n::MEMBER_SEARCH_NOT_FOUND_ERROR,
            "見つかりませんでした。"
        );
        assert_eq!(i18n::ROLE_SEARCH_NOT_FOUND_ERROR, "見つかりませんでした。");
    }

    #[test]
    fn bounded_picker_locale_fields_fit_within_discord_limits() {
        for label in [
            i18n::PICKER_PREVIOUS_PAGE_LABEL,
            i18n::PICKER_NEXT_PAGE_LABEL,
            i18n::PICKER_SEARCH_LABEL,
            i18n::PAYER_CLEAR_LABEL,
            i18n::INDIVIDUAL_CLEAR_LABEL,
            i18n::ROLE_CLEAR_LABEL,
            i18n::PARTICIPANT_SOURCE_INDIVIDUAL_LABEL,
            i18n::PARTICIPANT_SOURCE_ROLE_LABEL,
            i18n::PARTICIPANT_SOURCE_MEMBERS_LABEL,
            i18n::PARTICIPANT_SOURCE_CLEAR_ROLES_LABEL,
            i18n::PARTICIPANT_SOURCE_CLEAR_MEMBERS_LABEL,
            i18n::EXPENSE_NEXT_LABEL,
            i18n::EXPENSE_TO_WEIGHTS_LABEL,
            i18n::EXPENSE_TO_CONFIRM_LABEL,
            i18n::EXPENSE_RECORD_LABEL,
        ] {
            validate_button_label(label).expect("button label should fit Discord limits");
        }

        for placeholder in [
            i18n::EXPENSE_MODAL_AMOUNT_PLACEHOLDER,
            i18n::EXPENSE_MODAL_NOTE_PLACEHOLDER,
            i18n::EXPENSE_MODAL_DATE_PLACEHOLDER,
        ] {
            validate_component_placeholder(placeholder)
                .expect("placeholder should fit Discord limits");
        }
    }
}
