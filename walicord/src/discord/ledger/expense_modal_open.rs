use serenity::{
    all::{
        ActionRowComponent, CreateActionRow, CreateInputText, CreateModal, InputTextStyle,
        ModalInteraction,
    },
    builder::CreateInteractionResponse,
};
use walicord_application::{Clock, InteractionNonce};
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    RenderBudgetError, truncate_component_label, validate_custom_id, validate_modal_title,
    validate_text_input_label, validate_text_input_placeholder,
};

use std::{collections::BTreeMap, fmt::Write as _};
use walicord_application::ledger::expense_modal::RawExpenseModalSubmission;
use walicord_domain::model::{MemberId, Weight};

pub const EXPENSE_MODAL_CUSTOM_ID_PREFIX: &str = "ledger:expense:new:";
pub const EXPENSE_WEIGHT_MODAL_CUSTOM_ID_PREFIX: &str = "ledger:expense:weights:";
const AMOUNT_FIELD: &str = "amount";
const NOTE_FIELD: &str = "note";
const DATE_FIELD: &str = "date";
const WEIGHTS_FIELD: &str = "weights";

/// Optional prefill state. When the modal is being re-opened after a validation
/// failure (criterion 124-125 / 205), the caller passes the raw values from the
/// preserved `ModalRetryBinding` here so the actor sees their last submission.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ExpenseModalPrefill {
    pub raw_amount: Option<String>,
    pub raw_note: Option<String>,
    pub raw_date: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ExpenseModalBuildError {
    #[error("modal exceeded a Discord render budget: {0}")]
    Budget(#[from] RenderBudgetError),
}

/// Result of inspecting a `ModalInteraction.custom_id` to see if it belongs to the
/// expense-new modal family. Returns the carried interaction nonce when the prefix
/// matches; the router uses this to confirm the modal is the one we opened and to
/// detect stale-nonce re-submits per criterion 167.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpenseModalCustomIdMatch {
    Match { nonce: InteractionNonce },
    Stale,
    NoMatch,
}

pub fn parse_expense_modal_custom_id(custom_id: &str) -> ExpenseModalCustomIdMatch {
    parse_modal_custom_id(custom_id, EXPENSE_MODAL_CUSTOM_ID_PREFIX)
}

pub fn parse_expense_weight_modal_custom_id(custom_id: &str) -> ExpenseModalCustomIdMatch {
    parse_modal_custom_id(custom_id, EXPENSE_WEIGHT_MODAL_CUSTOM_ID_PREFIX)
}

fn parse_modal_custom_id(custom_id: &str, prefix: &str) -> ExpenseModalCustomIdMatch {
    let Some(remainder) = custom_id.strip_prefix(prefix) else {
        return ExpenseModalCustomIdMatch::NoMatch;
    };
    let Ok(parsed) = remainder.parse::<u64>() else {
        return ExpenseModalCustomIdMatch::Stale;
    };
    match InteractionNonce::new(parsed) {
        Ok(nonce) => ExpenseModalCustomIdMatch::Match { nonce },
        Err(_) => ExpenseModalCustomIdMatch::Stale,
    }
}

pub struct WeightEditorParticipant<'a> {
    pub username: &'a str,
    pub weight: Weight,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ExpenseWeightModalParseError {
    #[error("weight line is malformed")]
    InvalidLine,
    #[error("unknown username in weight editor")]
    UnknownUsername,
    #[error("weight editor contains duplicate username")]
    DuplicateUsername,
}

pub fn parse_expense_weight_modal_submission(
    modal: &ModalInteraction,
    username_to_member: &BTreeMap<&str, MemberId>,
) -> Result<BTreeMap<MemberId, Weight>, ExpenseWeightModalParseError> {
    let mut raw = None;
    for row in &modal.data.components {
        for component in &row.components {
            if let ActionRowComponent::InputText(input) = component
                && input.custom_id == WEIGHTS_FIELD
            {
                raw = input.value.as_deref();
            }
        }
    }
    parse_weight_overrides(raw.unwrap_or_default(), username_to_member)
}

fn parse_weight_overrides(
    raw: &str,
    username_to_member: &BTreeMap<&str, MemberId>,
) -> Result<BTreeMap<MemberId, Weight>, ExpenseWeightModalParseError> {
    let mut weights = BTreeMap::new();
    for line in raw.lines().map(str::trim).filter(|line| !line.is_empty()) {
        let (username, weight_str) = line
            .split_once('=')
            .ok_or(ExpenseWeightModalParseError::InvalidLine)?;
        let username = username.trim();
        let member_id = username_to_member
            .get(username)
            .copied()
            .ok_or(ExpenseWeightModalParseError::UnknownUsername)?;
        let weight = Weight(
            weight_str
                .trim()
                .parse()
                .map_err(|_| ExpenseWeightModalParseError::InvalidLine)?,
        );
        if weights.insert(member_id, weight).is_some() {
            return Err(ExpenseWeightModalParseError::DuplicateUsername);
        }
    }
    Ok(weights)
}

/// Extract the raw amount / note / date strings from a Discord modal submission so
/// the application-layer validator can run. Returns `None` only if the modal
/// somehow shipped without the expected fields, which is a Discord-side malformation;
/// the router maps that to a generic internal error.
pub fn extract_raw_expense_modal_submission(
    modal: &ModalInteraction,
) -> Option<RawExpenseModalSubmission> {
    let mut amount = None;
    let mut note = None;
    let mut date = None;
    for row in &modal.data.components {
        for component in &row.components {
            if let ActionRowComponent::InputText(input) = component {
                let value = input.value.clone().unwrap_or_default();
                match input.custom_id.as_str() {
                    AMOUNT_FIELD => amount = Some(value),
                    NOTE_FIELD => note = Some(value),
                    DATE_FIELD => date = Some(value),
                    _ => {}
                }
            }
        }
    }
    Some(RawExpenseModalSubmission {
        raw_amount: amount?,
        raw_note: note.unwrap_or_default(),
        raw_date: date.unwrap_or_default(),
    })
}

/// Build the expense modal `CreateInteractionResponse` for `/expense` and the panel
/// `記録する` launcher. Today's date (criterion 147) is sourced from the `Clock` port
/// so tests are deterministic, the custom_id carries the `InteractionNonce` per
/// criterion 193, and all chrome (title / labels / placeholders) is routed through
/// `walicord-i18n` per criterion 191. The function is pure — the caller invokes
/// `create_response` separately.
pub fn build_expense_modal_response(
    clock: &dyn Clock,
    nonce: InteractionNonce,
    prefill: &ExpenseModalPrefill,
) -> Result<CreateInteractionResponse, ExpenseModalBuildError> {
    let custom_id = format!("{EXPENSE_MODAL_CUSTOM_ID_PREFIX}{nonce}");
    validate_custom_id(&custom_id).map_err(ExpenseModalBuildError::Budget)?;

    let title_text = truncate_component_label(i18n::expense_modal_title());
    validate_modal_title(&title_text).map_err(ExpenseModalBuildError::Budget)?;

    let amount_label = truncate_component_label(i18n::expense_modal_amount_label());
    validate_text_input_label(&amount_label).map_err(ExpenseModalBuildError::Budget)?;
    let amount_placeholder = truncate_component_label(i18n::expense_modal_amount_placeholder());
    validate_text_input_placeholder(&amount_placeholder).map_err(ExpenseModalBuildError::Budget)?;

    let note_label = truncate_component_label(i18n::expense_modal_note_label());
    validate_text_input_label(&note_label).map_err(ExpenseModalBuildError::Budget)?;
    let note_placeholder = truncate_component_label(i18n::expense_modal_note_placeholder());
    validate_text_input_placeholder(&note_placeholder).map_err(ExpenseModalBuildError::Budget)?;

    let date_label = truncate_component_label(i18n::expense_modal_date_label());
    validate_text_input_label(&date_label).map_err(ExpenseModalBuildError::Budget)?;
    let date_placeholder = truncate_component_label(i18n::expense_modal_date_placeholder());
    validate_text_input_placeholder(&date_placeholder).map_err(ExpenseModalBuildError::Budget)?;

    let today_value = clock.today_business_date().to_string();
    let date_value = prefill.raw_date.clone().unwrap_or(today_value);

    let mut amount_input = CreateInputText::new(InputTextStyle::Short, amount_label, AMOUNT_FIELD)
        .placeholder(amount_placeholder)
        .required(true);
    if let Some(value) = prefill.raw_amount.as_ref() {
        amount_input = amount_input.value(value.clone());
    }

    let mut note_input = CreateInputText::new(InputTextStyle::Paragraph, note_label, NOTE_FIELD)
        .placeholder(note_placeholder)
        .required(false);
    if let Some(value) = prefill.raw_note.as_ref() {
        note_input = note_input.value(value.clone());
    }

    let date_input = CreateInputText::new(InputTextStyle::Short, date_label, DATE_FIELD)
        .placeholder(date_placeholder)
        .value(date_value)
        .required(false);

    let modal = CreateModal::new(custom_id, title_text).components(vec![
        CreateActionRow::InputText(amount_input),
        CreateActionRow::InputText(note_input),
        CreateActionRow::InputText(date_input),
    ]);
    Ok(CreateInteractionResponse::Modal(modal))
}

pub fn build_expense_weight_modal_response(
    nonce: InteractionNonce,
    participants: &[WeightEditorParticipant<'_>],
) -> Result<CreateInteractionResponse, ExpenseModalBuildError> {
    let custom_id = format!("{EXPENSE_WEIGHT_MODAL_CUSTOM_ID_PREFIX}{nonce}");
    validate_custom_id(&custom_id).map_err(ExpenseModalBuildError::Budget)?;
    let title = truncate_component_label(i18n::weight_editor_modal_title());
    validate_modal_title(&title).map_err(ExpenseModalBuildError::Budget)?;
    let label = truncate_component_label(i18n::weight_editor_input_label());
    validate_text_input_label(&label).map_err(ExpenseModalBuildError::Budget)?;
    let placeholder = truncate_component_label(i18n::weight_editor_placeholder());
    validate_text_input_placeholder(&placeholder).map_err(ExpenseModalBuildError::Budget)?;
    let mut value = String::new();
    for participant in participants {
        let _ = writeln!(value, "{} = {}", participant.username, participant.weight);
    }
    let input = CreateInputText::new(InputTextStyle::Paragraph, label, WEIGHTS_FIELD)
        .placeholder(placeholder)
        .value(value)
        .required(false);
    Ok(CreateInteractionResponse::Modal(
        CreateModal::new(custom_id, title).components(vec![CreateActionRow::InputText(input)]),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use std::time::SystemTime;
    use walicord_application::ledger::LedgerEffectiveDate;

    struct FixedClock {
        today: &'static str,
    }
    impl Clock for FixedClock {
        fn now(&self) -> SystemTime {
            SystemTime::UNIX_EPOCH
        }
        fn today_business_date(&self) -> LedgerEffectiveDate {
            LedgerEffectiveDate::new(self.today).expect("date")
        }
    }

    fn nonce(value: u64) -> InteractionNonce {
        InteractionNonce::new(value).expect("nonce")
    }

    fn username_map() -> BTreeMap<&'static str, MemberId> {
        BTreeMap::from([("alice", MemberId(42)), ("bob", MemberId(7))])
    }

    #[rstest]
    #[case::empty("", Ok(BTreeMap::new()))]
    #[case::weights(
        "alice = 1\nbob = 0\n",
        Ok(BTreeMap::from([(MemberId(7), Weight(0)), (MemberId(42), Weight(1))]))
    )]
    #[case::invalid("alice: 1", Err(ExpenseWeightModalParseError::InvalidLine))]
    #[case::unknown_username("charlie = 1", Err(ExpenseWeightModalParseError::UnknownUsername))]
    #[case::duplicate(
        "alice = 1\nalice = 2",
        Err(ExpenseWeightModalParseError::DuplicateUsername)
    )]
    fn weight_override_parser_is_fail_closed(
        #[case] raw: &str,
        #[case] expected: Result<BTreeMap<MemberId, Weight>, ExpenseWeightModalParseError>,
    ) {
        assert_eq!(parse_weight_overrides(raw, &username_map()), expected);
    }

    #[test]
    fn build_response_succeeds_with_clock_today_when_prefill_has_no_date() {
        let actual = build_expense_modal_response(
            &FixedClock {
                today: "2026-05-29",
            },
            nonce(7),
            &ExpenseModalPrefill::default(),
        );
        assert!(actual.is_ok());
    }

    #[test]
    fn build_response_keeps_prefill_date_when_supplied() {
        // The prefill date is preserved verbatim; modal validation later rejects
        // malformed values (criterion 125) so the modal trusts whatever the actor
        // typed previously.
        let actual = build_expense_modal_response(
            &FixedClock {
                today: "2026-05-29",
            },
            nonce(7),
            &ExpenseModalPrefill {
                raw_date: Some("2026/05/01".to_owned()),
                ..Default::default()
            },
        );
        assert!(actual.is_ok());
    }

    #[test]
    fn build_response_succeeds_with_full_prefill() {
        let actual = build_expense_modal_response(
            &FixedClock {
                today: "2026-05-29",
            },
            nonce(7),
            &ExpenseModalPrefill {
                raw_amount: Some("1500".to_owned()),
                raw_note: Some("ランチ".to_owned()),
                raw_date: Some("2026-05-01".to_owned()),
            },
        );
        assert!(actual.is_ok());
    }

    #[test]
    fn parse_custom_id_recovers_nonce_when_prefix_matches() {
        let actual =
            parse_expense_modal_custom_id(&format!("{EXPENSE_MODAL_CUSTOM_ID_PREFIX}{}", 42));
        assert_eq!(
            actual,
            ExpenseModalCustomIdMatch::Match { nonce: nonce(42) }
        );
    }

    #[test]
    fn parse_custom_id_returns_no_match_when_prefix_differs() {
        let actual = parse_expense_modal_custom_id("settle:something:1");
        assert_eq!(actual, ExpenseModalCustomIdMatch::NoMatch);
    }

    #[test]
    fn parse_custom_id_returns_stale_for_non_numeric_remainder() {
        let actual = parse_expense_modal_custom_id(&format!("{EXPENSE_MODAL_CUSTOM_ID_PREFIX}abc"));
        assert_eq!(actual, ExpenseModalCustomIdMatch::Stale);
    }

    #[test]
    fn parse_custom_id_returns_stale_for_zero_nonce() {
        let actual = parse_expense_modal_custom_id(&format!("{EXPENSE_MODAL_CUSTOM_ID_PREFIX}0"));
        assert_eq!(actual, ExpenseModalCustomIdMatch::Stale);
    }
}
