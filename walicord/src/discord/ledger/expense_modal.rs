use chrono::{Datelike, NaiveDate};
use walicord_application::{
    Clock,
    ledger::{ExpenseNote, LedgerEffectiveDate},
};
use walicord_domain::Money;

/// Maximum note length enforced at modal validation per criterion 204. The cap counts
/// Unicode scalar values (`char`s), not bytes or grapheme clusters, matching the
/// existing user-facing contract.
pub const NOTE_MAX_UNICODE_SCALARS: usize = 200;

/// Raw modal submission as Discord delivers it: three free-text fields. Validation
/// converts these into the typed values the rest of the expense flow consumes; failure
/// produces a [`ExpenseModalValidationError`] which the adapter pairs with a
/// [`super::sessions::ModalRetryBinding`] so the actor can re-open the modal with the
/// preserved raw values (criteria 124-125, 145, 205).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawExpenseModalSubmission {
    pub raw_amount: String,
    pub raw_note: String,
    pub raw_date: String,
}

/// Modal validation outcome on success. Note is `None` when normalisation collapses it
/// to blank (criterion 127); effective_date may come from the modal text or from the
/// `Clock` port default when the field is blank (criterion 126).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidatedExpenseModalSubmission {
    pub amount: Money,
    pub note: Option<ExpenseNote>,
    pub effective_date: LedgerEffectiveDate,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ExpenseModalValidationError {
    /// Criterion 124: blank amount input. Adapter renders `i18n::expense_amount_blank_message`
    /// (or equivalent) plus the retry CTA.
    #[error("expense amount is blank")]
    AmountBlank,
    /// Criterion 168: amount has decimal point / fractional part.
    #[error("expense amount must be an integer (no fractional part)")]
    AmountNonInteger,
    /// Criterion 124 + 90: zero or negative amount.
    #[error("expense amount must be positive")]
    AmountNonPositive,
    /// Criterion 124: malformed amount that is neither integer nor decimal.
    #[error("expense amount is malformed")]
    AmountMalformed,
    /// Criterion 204: note exceeds the unicode-scalar cap.
    #[error("expense note length {length} exceeds maximum {max}")]
    NoteTooLong { length: usize, max: usize },
    /// Criterion 125: malformed date that no supported format matches.
    #[error("expense effective date is malformed (no supported format matches)")]
    DateMalformed,
}

/// Criterion 265: modal validation precedence runs in visual order (amount → note →
/// date). The first failing field returns; later fields are not validated until the
/// earlier issue is fixed.
pub fn validate_expense_modal_submission(
    raw: &RawExpenseModalSubmission,
    clock: &dyn Clock,
) -> Result<ValidatedExpenseModalSubmission, ExpenseModalValidationError> {
    let amount = parse_amount(&raw.raw_amount)?;
    let note = parse_note(&raw.raw_note)?;
    let effective_date = parse_or_default_date(&raw.raw_date, clock)?;
    Ok(ValidatedExpenseModalSubmission {
        amount,
        note,
        effective_date,
    })
}

fn parse_amount(raw: &str) -> Result<Money, ExpenseModalValidationError> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return Err(ExpenseModalValidationError::AmountBlank);
    }
    let normalized: String = trimmed
        .chars()
        .filter(|character| !character.is_whitespace() && *character != ',')
        .collect();
    if normalized.contains('.') {
        return Err(ExpenseModalValidationError::AmountNonInteger);
    }
    let parsed: i64 = normalized
        .parse()
        .map_err(|_| ExpenseModalValidationError::AmountMalformed)?;
    if parsed <= 0 {
        return Err(ExpenseModalValidationError::AmountNonPositive);
    }
    Ok(Money::from_i64(parsed))
}

fn parse_note(raw: &str) -> Result<Option<ExpenseNote>, ExpenseModalValidationError> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return Ok(None);
    }
    let length = trimmed.chars().count();
    if length > NOTE_MAX_UNICODE_SCALARS {
        return Err(ExpenseModalValidationError::NoteTooLong {
            length,
            max: NOTE_MAX_UNICODE_SCALARS,
        });
    }
    Ok(ExpenseNote::new(trimmed).ok())
}

fn parse_or_default_date(
    raw: &str,
    clock: &dyn Clock,
) -> Result<LedgerEffectiveDate, ExpenseModalValidationError> {
    if raw.trim().is_empty() {
        return Ok(clock.today_business_date());
    }
    let today = parse_clock_date(clock)?;
    normalize_date_input(raw, today)
        .and_then(|date| LedgerEffectiveDate::new(date.format("%Y-%m-%d").to_string()).ok())
        .ok_or(ExpenseModalValidationError::DateMalformed)
}

fn parse_clock_date(clock: &dyn Clock) -> Result<NaiveDate, ExpenseModalValidationError> {
    NaiveDate::parse_from_str(clock.today_business_date().as_str(), "%Y-%m-%d")
        .map_err(|_| ExpenseModalValidationError::DateMalformed)
}

fn normalize_date_input(raw: &str, today: NaiveDate) -> Option<NaiveDate> {
    let trimmed = raw.trim();
    match trimmed {
        "今日" | "today" | "きょう" => return Some(today),
        "昨日" | "yesterday" | "きのう" => return today.pred_opt(),
        _ => {}
    }
    let bytes = trimmed.as_bytes();
    if bytes.len() == 10 && bytes.get(4) == Some(&b'-') && bytes.get(7) == Some(&b'-') {
        return NaiveDate::parse_from_str(trimmed, "%Y-%m-%d").ok();
    }
    if bytes.len() == 10 && bytes.get(4) == Some(&b'/') && bytes.get(7) == Some(&b'/') {
        return NaiveDate::parse_from_str(trimmed, "%Y/%m/%d").ok();
    }
    if let Some(slash) = trimmed.find('/') {
        let (month_str, rest) = trimmed.split_at(slash);
        let day_str = &rest[1..];
        let all_digits = |segment: &str| {
            !segment.is_empty() && segment.bytes().all(|byte| byte.is_ascii_digit())
        };
        if all_digits(month_str) && all_digits(day_str) {
            let month: u32 = month_str.parse().ok()?;
            let day: u32 = day_str.parse().ok()?;
            return NaiveDate::from_ymd_opt(today.year(), month, day);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use std::time::SystemTime;

    struct FixedClock {
        today: &'static str,
    }

    impl Clock for FixedClock {
        fn now(&self) -> SystemTime {
            SystemTime::UNIX_EPOCH
        }
        fn today_business_date(&self) -> LedgerEffectiveDate {
            LedgerEffectiveDate::new(self.today).expect("test clock today should parse")
        }
    }

    fn clock() -> FixedClock {
        FixedClock {
            today: "2026-05-29",
        }
    }

    fn submission(amount: &str, note: &str, date: &str) -> RawExpenseModalSubmission {
        RawExpenseModalSubmission {
            raw_amount: amount.to_owned(),
            raw_note: note.to_owned(),
            raw_date: date.to_owned(),
        }
    }

    #[rstest]
    #[case::blank_amount(
        submission("", "ランチ", "2026-05-01"),
        Err(ExpenseModalValidationError::AmountBlank)
    )]
    #[case::decimal_amount(
        submission("1000.5", "ランチ", "2026-05-01"),
        Err(ExpenseModalValidationError::AmountNonInteger)
    )]
    #[case::zero_amount(
        submission("0", "ランチ", "2026-05-01"),
        Err(ExpenseModalValidationError::AmountNonPositive)
    )]
    #[case::negative_amount(
        submission("-100", "ランチ", "2026-05-01"),
        Err(ExpenseModalValidationError::AmountNonPositive)
    )]
    #[case::malformed_amount(
        submission("abc", "ランチ", "2026-05-01"),
        Err(ExpenseModalValidationError::AmountMalformed)
    )]
    #[case::iso_date(
        submission("1000", "ランチ", "2026-05-01"),
        Ok(ValidatedExpenseModalSubmission {
            amount: Money::from_i64(1000),
            note: Some(ExpenseNote::new("ランチ").unwrap()),
            effective_date: LedgerEffectiveDate::new("2026-05-01").unwrap(),
        }),
    )]
    #[case::comma_separators_in_amount(
        submission("1,500", "ランチ", "2026-05-01"),
        Ok(ValidatedExpenseModalSubmission {
            amount: Money::from_i64(1500),
            note: Some(ExpenseNote::new("ランチ").unwrap()),
            effective_date: LedgerEffectiveDate::new("2026-05-01").unwrap(),
        }),
    )]
    #[case::blank_note_is_normalized_to_none(
        submission("1000", "   ", "2026-05-01"),
        Ok(ValidatedExpenseModalSubmission {
            amount: Money::from_i64(1000),
            note: None,
            effective_date: LedgerEffectiveDate::new("2026-05-01").unwrap(),
        }),
    )]
    #[case::blank_date_defaults_to_clock_today(
        submission("1000", "ランチ", ""),
        Ok(ValidatedExpenseModalSubmission {
            amount: Money::from_i64(1000),
            note: Some(ExpenseNote::new("ランチ").unwrap()),
            effective_date: LedgerEffectiveDate::new("2026-05-29").unwrap(),
        }),
    )]
    #[case::slash_separator_date(
        submission("1000", "ランチ", "2026/05/01"),
        Ok(ValidatedExpenseModalSubmission {
            amount: Money::from_i64(1000),
            note: Some(ExpenseNote::new("ランチ").unwrap()),
            effective_date: LedgerEffectiveDate::new("2026-05-01").unwrap(),
        }),
    )]
    #[case::today_keyword_japanese(
        submission("1000", "ランチ", "今日"),
        Ok(ValidatedExpenseModalSubmission {
            amount: Money::from_i64(1000),
            note: Some(ExpenseNote::new("ランチ").unwrap()),
            effective_date: LedgerEffectiveDate::new("2026-05-29").unwrap(),
        }),
    )]
    #[case::yesterday_keyword_english(
        submission("1000", "ランチ", "yesterday"),
        Ok(ValidatedExpenseModalSubmission {
            amount: Money::from_i64(1000),
            note: Some(ExpenseNote::new("ランチ").unwrap()),
            effective_date: LedgerEffectiveDate::new("2026-05-28").unwrap(),
        }),
    )]
    #[case::month_day_assumes_clock_year(
        submission("1000", "ランチ", "5/1"),
        Ok(ValidatedExpenseModalSubmission {
            amount: Money::from_i64(1000),
            note: Some(ExpenseNote::new("ランチ").unwrap()),
            effective_date: LedgerEffectiveDate::new("2026-05-01").unwrap(),
        }),
    )]
    #[case::malformed_date(
        submission("1000", "ランチ", "tomorrow"),
        Err(ExpenseModalValidationError::DateMalformed)
    )]
    fn validate_expense_modal_submission_cases(
        #[case] raw: RawExpenseModalSubmission,
        #[case] expected: Result<ValidatedExpenseModalSubmission, ExpenseModalValidationError>,
    ) {
        let actual = validate_expense_modal_submission(&raw, &clock());
        assert_eq!(actual, expected);
    }

    #[test]
    fn precedence_returns_amount_error_when_all_three_fields_invalid() {
        let raw = submission("abc", &"x".repeat(NOTE_MAX_UNICODE_SCALARS + 1), "garbage");
        let actual = validate_expense_modal_submission(&raw, &clock());
        assert_eq!(actual, Err(ExpenseModalValidationError::AmountMalformed));
    }

    #[test]
    fn precedence_returns_note_error_when_amount_valid_but_note_too_long_and_date_invalid() {
        let long_note = "x".repeat(NOTE_MAX_UNICODE_SCALARS + 1);
        let raw = submission("1000", &long_note, "garbage");
        let actual = validate_expense_modal_submission(&raw, &clock());
        assert_eq!(
            actual,
            Err(ExpenseModalValidationError::NoteTooLong {
                length: NOTE_MAX_UNICODE_SCALARS + 1,
                max: NOTE_MAX_UNICODE_SCALARS,
            })
        );
    }

    #[test]
    fn note_length_uses_unicode_scalar_count_not_bytes() {
        let multibyte_note: String = std::iter::repeat_n('あ', NOTE_MAX_UNICODE_SCALARS).collect();
        let raw = submission("1000", &multibyte_note, "2026-05-01");
        let actual = validate_expense_modal_submission(&raw, &clock());
        assert!(actual.is_ok(), "200 multibyte scalars should fit the limit");
    }

    #[test]
    fn note_just_over_limit_is_rejected() {
        let too_long: String = std::iter::repeat_n('a', NOTE_MAX_UNICODE_SCALARS + 1).collect();
        let raw = submission("1000", &too_long, "2026-05-01");
        let actual = validate_expense_modal_submission(&raw, &clock());
        assert_eq!(
            actual,
            Err(ExpenseModalValidationError::NoteTooLong {
                length: NOTE_MAX_UNICODE_SCALARS + 1,
                max: NOTE_MAX_UNICODE_SCALARS,
            })
        );
    }
}
