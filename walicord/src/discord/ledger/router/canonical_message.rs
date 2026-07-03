//! Adapter-side composers that turn a freshly recorded `LedgerEntry` plus the live
//! Discord roster into a budget-validated `RenderedCanonicalMessage`. Each composer
//! lives behind a `Recordable*Entry` newtype so the caller never has to discriminate
//! between event variants; the budget validation comes from
//! `DiscordLedgerPresenter::render_public_entry`.

use std::collections::HashMap;

use walicord_application::ledger::{
    LedgerEntry, LedgerId, MemberAmount,
    expense_flow::ConfirmationBuildError,
    expense_write::RecordableExpenseEntry,
    projection::ExpenseOrSettlementView,
    record_expense::{ExpenseEntryRenderer, ExpenseRenderError},
    settle_execute::{SettlementEntryRenderer, SettlementRenderError},
    settle_flow::RecordableSettlementEntry,
    void_execute::{VoidEntryRenderer, VoidRenderError},
};
use walicord_domain::model::MemberId;
use walicord_presentation::discord_ledger::{
    BusinessDateTime, DiscordLedgerPresenter, ParticipantShareRow, PublicCanonicalMessageModel,
    PublicExpenseMessageModel, PublicSettlementMessageModel, PublicVoidMessageModel,
    RecoveryReference, RenderedCanonicalMessage, SafeLiteralText, SurfaceMemberLabels, TransferRow,
    expense_or_settlement_summary,
};

use super::{InternalLedgerRouteError, LedgerRouteError, format_money_for_modal};

/// Per-request adapter that translates the application
/// [`ExpenseEntryRenderer`] port into the existing presentation-layer
/// expense-message renderer. The display names map is bound per interaction so
/// the application use case calls a single `render_public_body(&entry, ledger_id)`.
pub(crate) struct DiscordExpenseEntryRenderer<'a> {
    pub(crate) display_names: &'a HashMap<MemberId, smol_str::SmolStr>,
}

impl ExpenseEntryRenderer for DiscordExpenseEntryRenderer<'_> {
    fn render_public_body(
        &self,
        recordable: &RecordableExpenseEntry,
        ledger_id: LedgerId,
    ) -> Result<String, ExpenseRenderError> {
        let rendered = render_public_expense_message(recordable, ledger_id, self.display_names)
            .map_err(|error| {
                tracing::error!(
                    ledger_id = ?ledger_id,
                    entry_id = ?recordable.id(),
                    error = %error,
                    "expense canonical body render failed",
                );
                ExpenseRenderError::with_source(error)
            })?;
        Ok(rendered.body().to_owned())
    }
}

/// Per-request adapter that translates the application
/// [`SettlementEntryRenderer`] port into the existing presentation-layer
/// settlement-message renderer.
pub(crate) struct DiscordSettlementEntryRenderer<'a> {
    pub(crate) display_names: &'a HashMap<MemberId, smol_str::SmolStr>,
}

impl SettlementEntryRenderer for DiscordSettlementEntryRenderer<'_> {
    fn render_public_body(
        &self,
        recordable: &RecordableSettlementEntry,
        ledger_id: LedgerId,
    ) -> Result<String, SettlementRenderError> {
        let rendered = render_public_settlement_message(recordable, ledger_id, self.display_names)
            .map_err(|error| {
                tracing::error!(
                    ledger_id = ?ledger_id,
                    entry_id = ?recordable.id(),
                    error = %error,
                    "settlement canonical body render failed",
                );
                SettlementRenderError::with_source(error)
            })?;
        Ok(rendered.body().to_owned())
    }
}

/// Per-request adapter that translates the application [`VoidEntryRenderer`]
/// port into the existing presentation-layer void-message renderer. The
/// adapter binds the roster-derived labels per interaction so the application
/// use case calls a single `render_public_body(&entry, &target, ledger_id)`.
pub(crate) struct DiscordVoidEntryRenderer<'a> {
    pub(crate) labels: &'a SurfaceMemberLabels,
}

impl VoidEntryRenderer for DiscordVoidEntryRenderer<'_> {
    fn render_public_body(
        &self,
        entry: &LedgerEntry,
        target: &ExpenseOrSettlementView,
        ledger_id: LedgerId,
    ) -> Result<String, VoidRenderError> {
        let rendered =
            render_public_void_message(entry, target, ledger_id, self.labels).map_err(|error| {
                tracing::error!(
                    ledger_id = ?ledger_id,
                    entry_id = ?entry.id,
                    error = %error,
                    "void canonical body render failed",
                );
                VoidRenderError::with_source(error)
            })?;
        Ok(rendered.body().to_owned())
    }
}

/// Render the public canonical message for a freshly composed expense entry. Returns
/// the budget-validated `RenderedCanonicalMessage` newtype so the canonical recovery
/// shape and surface budget stay enforced through the write boundary (criterion 275 /
/// AC25); the caller passes it to `append_authoritative` unchanged.
#[allow(clippy::result_large_err)] // LedgerRouteError is the project's standard error envelope.
pub(super) fn render_public_expense_message(
    recordable: &RecordableExpenseEntry,
    ledger_id: LedgerId,
    display_names: &HashMap<MemberId, smol_str::SmolStr>,
) -> Result<RenderedCanonicalMessage, LedgerRouteError> {
    let entry = recordable.entry();
    let event = recordable.event();

    let paid_by: &[MemberAmount] = event.paid_by();
    let payer_member_id = paid_by
        .first()
        .map(|amount| amount.member_id)
        .ok_or_else(|| {
            LedgerRouteError::Internal(InternalLedgerRouteError::ConfirmationBuild(
                ConfirmationBuildError::PayerNotSelected,
            ))
        })?;
    let total_amount: walicord_domain::Money = paid_by.iter().map(|amount| amount.amount).sum();

    let labels = SurfaceMemberLabels::from_member_names(
        std::iter::once((
            payer_member_id,
            display_names.get(&payer_member_id).map(|s| s.as_str()),
        ))
        .chain(event.owed_by().iter().map(|amount| {
            (
                amount.member_id,
                display_names.get(&amount.member_id).map(|s| s.as_str()),
            )
        })),
    );

    let payer_display_name = labels.safe_member_label(payer_member_id);

    let participant_rows: Vec<ParticipantShareRow> = event
        .owed_by()
        .iter()
        .map(|amount| ParticipantShareRow {
            display_name: labels.safe_member_label(amount.member_id),
            share_amount: format_money_for_modal(amount.amount),
        })
        .collect();

    let note = event
        .note()
        .map(|note| SafeLiteralText::parse_note(note.as_str()))
        .transpose()?;

    let actor_member_id = entry
        .metadata
        .recorded_by
        .expect("composed entry always records `recorded_by`");
    let actor_labels = SurfaceMemberLabels::from_member_names(std::iter::once((
        actor_member_id,
        display_names.get(&actor_member_id).map(|s| s.as_str()),
    )));
    let actor_display_name = actor_labels.safe_member_label(actor_member_id);

    let recorded_at = entry
        .metadata
        .recorded_at
        .expect("composed entry always records `recorded_at`");
    let effective_date = entry
        .metadata
        .effective_date
        .expect("composed expense entry always records `effective_date`");

    let model = PublicCanonicalMessageModel::Expense(PublicExpenseMessageModel {
        entry_id: entry.id,
        effective_date,
        payer_display_name,
        amount: format_money_for_modal(total_amount),
        participant_rows,
        note,
        actor_display_name,
        recorded_at: BusinessDateTime::from_system_time(recorded_at),
        // The permalink only exists after the canonical message has been posted.
        recovery_reference: RecoveryReference {
            ledger_id_short: format!("{ledger_id:x}"),
            entry_id: entry.id,
            message_link: None,
        },
    });

    DiscordLedgerPresenter::render_public_entry(&model)
        .map_err(|error| LedgerRouteError::Internal(InternalLedgerRouteError::PanelRender(error)))
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the project's standard error envelope.
pub(super) fn render_public_settlement_message(
    recordable: &RecordableSettlementEntry,
    ledger_id: LedgerId,
    display_names: &HashMap<MemberId, smol_str::SmolStr>,
) -> Result<RenderedCanonicalMessage, LedgerRouteError> {
    let entry = recordable.entry();
    let event = recordable.event();

    let labels =
        SurfaceMemberLabels::from_member_names(event.transfers().iter().flat_map(|transfer| {
            [
                (
                    transfer.from,
                    display_names.get(&transfer.from).map(|s| s.as_str()),
                ),
                (
                    transfer.to,
                    display_names.get(&transfer.to).map(|s| s.as_str()),
                ),
            ]
        }));
    let transfers = event
        .transfers()
        .iter()
        .map(|transfer| TransferRow {
            from_display_name: labels.safe_member_label(transfer.from),
            to_display_name: labels.safe_member_label(transfer.to),
            amount: format_money_for_modal(transfer.amount),
        })
        .collect();

    let actor_member_id = entry
        .metadata
        .recorded_by
        .expect("composed settlement entry always records `recorded_by`");
    let actor_labels = SurfaceMemberLabels::from_member_names(std::iter::once((
        actor_member_id,
        display_names.get(&actor_member_id).map(|s| s.as_str()),
    )));
    let actor_display_name = actor_labels.safe_member_label(actor_member_id);
    let recorded_at = entry
        .metadata
        .recorded_at
        .expect("composed settlement entry always records `recorded_at`");

    let model = PublicCanonicalMessageModel::Settlement(PublicSettlementMessageModel {
        entry_id: entry.id,
        recorded_date: walicord_application::ledger::LedgerEffectiveDate::from_system_time(
            recorded_at,
        ),
        transfers,
        actor_display_name,
        recorded_at: BusinessDateTime::from_system_time(recorded_at),
        recovery_reference: RecoveryReference {
            ledger_id_short: format!("{ledger_id:08x}"),
            entry_id: entry.id,
            message_link: None,
        },
    });

    DiscordLedgerPresenter::render_public_entry(&model)
        .map_err(|error| LedgerRouteError::Internal(InternalLedgerRouteError::PanelRender(error)))
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
pub(super) fn render_public_void_message(
    entry: &LedgerEntry,
    target: &ExpenseOrSettlementView,
    ledger_id: LedgerId,
    labels: &SurfaceMemberLabels,
) -> Result<RenderedCanonicalMessage, LedgerRouteError> {
    let actor_member_id = entry
        .metadata
        .recorded_by
        .expect("composed void entry always records `recorded_by`");
    let recorded_at = entry
        .metadata
        .recorded_at
        .expect("composed void entry always records `recorded_at`");
    let model = PublicCanonicalMessageModel::Void(PublicVoidMessageModel {
        entry_id: entry.id,
        voider_display_name: labels.safe_member_label(actor_member_id),
        voided_at: BusinessDateTime::from_system_time(recorded_at),
        original_summary: expense_or_settlement_summary(target, labels)?,
        recorded_at: BusinessDateTime::from_system_time(recorded_at),
        recovery_reference: RecoveryReference {
            ledger_id_short: format!("{ledger_id:08x}"),
            entry_id: entry.id,
            message_link: None,
        },
    });
    DiscordLedgerPresenter::render_public_entry(&model)
        .map_err(|error| LedgerRouteError::Internal(InternalLedgerRouteError::PanelRender(error)))
}
