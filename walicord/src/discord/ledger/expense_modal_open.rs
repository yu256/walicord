use serenity::{
    all::{CreateActionRow, CreateInputText, CreateModal, InputTextStyle},
    builder::CreateInteractionResponse,
};
use walicord_application::{Clock, InteractionNonce};
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    RenderBudgetError, truncate_component_label, validate_custom_id, validate_modal_title,
    validate_text_input_label, validate_text_input_placeholder,
};

pub const EXPENSE_MODAL_CUSTOM_ID_PREFIX: &str = "ledger:expense:new:";
const AMOUNT_FIELD: &str = "amount";
const NOTE_FIELD: &str = "note";
const DATE_FIELD: &str = "date";

/// Optional prefill state. When the modal is being re-opened after a validation
/// failure (criterion 124-125 / 205), the caller passes the raw values from the
/// preserved `ModalRetryBinding` here so the actor sees their last submission.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ExpenseModalPrefill {
    pub raw_amount: Option<String>,
    pub raw_note: Option<String>,
    pub raw_date: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpenseModalBuildError {
    Budget(RenderBudgetError),
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
    let custom_id = format!("{EXPENSE_MODAL_CUSTOM_ID_PREFIX}{}", nonce.get());
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

    let today_value = clock.today_business_date().as_str().to_owned();
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

#[cfg(test)]
mod tests {
    use super::*;
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
}
