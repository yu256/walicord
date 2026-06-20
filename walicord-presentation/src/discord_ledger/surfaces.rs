use super::{
    budgets::{
        RenderBudgetError, truncate_component_label, validate_action_rows, validate_button_label,
        validate_buttons_in_row, validate_component_description, validate_component_label,
        validate_component_placeholder, validate_custom_id, validate_message_content,
        validate_select_options, validate_select_value_bounds,
    },
    pickers::PickerSurfaceModel,
    sanitizer::{BusinessDateTime, SafeLiteralText},
    void_surfaces::{VoidConfirmationRecap, VoidSurfaceModel},
};
use walicord_application::ledger::{LedgerEffectiveDate, LedgerEntryId};
use walicord_i18n as i18n;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SurfaceInteractiveButtonStyle {
    Primary,
    Secondary,
    Danger,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SurfaceButton {
    Link {
        label: String,
        url: String,
        disabled: bool,
    },
    Interactive {
        label: String,
        custom_id: String,
        style: SurfaceInteractiveButtonStyle,
        disabled: bool,
    },
}

impl SurfaceButton {
    pub fn label(&self) -> &str {
        match self {
            Self::Link { label, .. } | Self::Interactive { label, .. } => label,
        }
    }

    pub fn custom_id(&self) -> Option<&str> {
        match self {
            Self::Interactive { custom_id, .. } => Some(custom_id),
            Self::Link { .. } => None,
        }
    }

    pub fn url(&self) -> Option<&str> {
        match self {
            Self::Link { url, .. } => Some(url),
            Self::Interactive { .. } => None,
        }
    }

    pub fn disabled(&self) -> bool {
        match self {
            Self::Link { disabled, .. } | Self::Interactive { disabled, .. } => *disabled,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceSelectOption {
    pub value: String,
    pub label: SafeLiteralText,
    pub description: Option<String>,
    pub selected: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceSelectMenu {
    pub custom_id: String,
    pub placeholder: Option<String>,
    pub options: Vec<SurfaceSelectOption>,
    pub min_values: u8,
    pub max_values: u8,
    pub disabled: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SurfaceActionRow {
    Buttons(Vec<SurfaceButton>),
    Select(SurfaceSelectMenu),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ExpenseSurfaceModel {
    pub title: String,
    pub summary_lines: Vec<String>,
    pub detail_lines: Vec<String>,
    pub validation_message: Option<String>,
    pub action_rows: Vec<SurfaceActionRow>,
    pub ephemeral: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ExpenseSuccessSurfaceModel {
    pub body_lines: Vec<String>,
    pub action_rows: Vec<SurfaceActionRow>,
    pub ephemeral: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct PanelButtonStates {
    pub record_disabled: bool,
    pub review_disabled: bool,
    pub ledger_disabled: bool,
    pub void_disabled: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PanelSurfaceModel {
    pub thread_cue: String,
    pub status_line: Option<String>,
    pub button_states: PanelButtonStates,
    pub ephemeral: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BalanceDirection {
    #[default]
    Receive,
    Pay,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BalanceRow {
    pub display_name: SafeLiteralText,
    pub amount: String,
    pub direction: BalanceDirection,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParticipantShareRow {
    pub display_name: SafeLiteralText,
    pub share_amount: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TransferRow {
    pub from_display_name: SafeLiteralText,
    pub to_display_name: SafeLiteralText,
    pub amount: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BalanceImpactRow {
    pub display_name: SafeLiteralText,
    pub amount: String,
    pub direction: BalanceDirection,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerSurfaceSummary {
    Expense {
        date: LedgerEffectiveDate,
        payer_display_name: SafeLiteralText,
        amount: String,
        note: Option<SafeLiteralText>,
    },
    Settlement {
        date: LedgerEffectiveDate,
        from_display_name: SafeLiteralText,
        to_display_name: SafeLiteralText,
        amount: String,
        additional_transfers: usize,
    },
    Void {
        date: LedgerEffectiveDate,
        voider_display_name: SafeLiteralText,
        recorded_at: BusinessDateTime,
    },
    Sealed {
        date: LedgerEffectiveDate,
        actor_display_name: SafeLiteralText,
    },
    BalanceAdjustment {
        date: LedgerEffectiveDate,
        actor_display_name: SafeLiteralText,
        impact_summary: SafeLiteralText,
    },
}

impl LedgerSurfaceSummary {
    fn date(&self) -> &LedgerEffectiveDate {
        match self {
            Self::Expense { date, .. }
            | Self::Settlement { date, .. }
            | Self::Void { date, .. }
            | Self::Sealed { date, .. }
            | Self::BalanceAdjustment { date, .. } => date,
        }
    }

    fn render_void_candidate_summary(&self) -> String {
        match self {
            Self::Expense {
                date,
                payer_display_name,
                amount,
                note,
            } => {
                let mut summary =
                    i18n::void_candidate_expense_summary(date, payer_display_name, amount)
                        .to_string();
                if let Some(note) = note {
                    summary.push(' ');
                    summary.push_str(
                        &i18n::void_candidate_note_excerpt(excerpt_with_ascii_ellipsis(
                            note.as_str(),
                            30,
                        ))
                        .to_string(),
                    );
                }
                summary
            }
            Self::Settlement {
                date,
                from_display_name,
                to_display_name,
                amount,
                additional_transfers,
            } => {
                let mut summary = i18n::void_candidate_settlement_summary(
                    date,
                    from_display_name,
                    to_display_name,
                    amount,
                )
                .to_string();
                if *additional_transfers > 0 {
                    summary.push(' ');
                    summary.push_str(&i18n::additional_items(*additional_transfers).to_string());
                }
                summary
            }
            Self::Void {
                date,
                voider_display_name,
                ..
            } => i18n::void_candidate_void_summary(date, voider_display_name).to_string(),
            Self::Sealed {
                date,
                actor_display_name,
            } => i18n::void_candidate_seal_summary(date, actor_display_name).to_string(),
            Self::BalanceAdjustment {
                date,
                impact_summary,
                ..
            } => i18n::void_candidate_adjustment_summary(date, impact_summary).to_string(),
        }
    }

    fn render_public_void_compact_summary(&self) -> String {
        match self {
            Self::Expense {
                date,
                payer_display_name,
                amount,
                note,
            } => {
                let mut summary =
                    i18n::void_candidate_expense_summary(date, payer_display_name, amount)
                        .to_string();
                if let Some(note) = note {
                    summary.push(' ');
                    summary.push_str(
                        &i18n::void_candidate_note_excerpt(excerpt_with_ascii_ellipsis(
                            note.as_str(),
                            30,
                        ))
                        .to_string(),
                    );
                }
                summary
            }
            Self::Settlement {
                date,
                from_display_name,
                to_display_name,
                amount,
                additional_transfers,
            } => {
                let mut summary = i18n::void_candidate_settlement_summary(
                    date,
                    from_display_name,
                    to_display_name,
                    amount,
                )
                .to_string();
                if *additional_transfers > 0 {
                    summary.push(' ');
                    summary.push_str(&i18n::additional_items(*additional_transfers).to_string());
                }
                summary
            }
            Self::Void {
                date,
                voider_display_name,
                ..
            } => i18n::void_candidate_void_summary(date, voider_display_name).to_string(),
            Self::Sealed {
                date,
                actor_display_name,
            } => i18n::void_candidate_seal_summary(date, actor_display_name).to_string(),
            Self::BalanceAdjustment {
                date,
                impact_summary,
                ..
            } => i18n::void_candidate_adjustment_summary(
                date,
                excerpt_with_ellipsis(impact_summary.as_str(), PUBLIC_COMPACT_IMPACT_LIMIT),
            )
            .to_string(),
        }
    }

    fn render_sealed_summary(&self) -> String {
        match self {
            Self::Expense {
                payer_display_name,
                amount,
                ..
            } => i18n::sealed_expense_summary(payer_display_name, amount).to_string(),
            Self::Settlement {
                from_display_name,
                to_display_name,
                additional_transfers,
                ..
            } => {
                let mut summary =
                    i18n::sealed_settlement_summary(from_display_name, to_display_name).to_string();
                if *additional_transfers > 0 {
                    summary.push(' ');
                    summary.push_str(&i18n::additional_items(*additional_transfers).to_string());
                }
                summary
            }
            Self::Void { .. } => i18n::sealed_void_summary().to_owned(),
            Self::Sealed {
                actor_display_name, ..
            } => i18n::sealed_prior_seal_summary(actor_display_name).to_string(),
            Self::BalanceAdjustment {
                actor_display_name, ..
            } => i18n::sealed_adjustment_summary(actor_display_name).to_string(),
        }
    }

    fn render_sealed_through_summary(&self) -> String {
        format!("{} {}", self.date(), self.render_sealed_summary())
    }

    fn render_confirmation_target(&self) -> String {
        match self {
            Self::Expense {
                payer_display_name, ..
            } => i18n::void_confirm_expense_target(payer_display_name).to_string(),
            Self::Settlement {
                from_display_name,
                to_display_name,
                amount,
                additional_transfers,
                ..
            } => {
                let mut target = i18n::void_confirm_settlement_target(
                    from_display_name,
                    to_display_name,
                    amount,
                )
                .to_string();
                if *additional_transfers > 0 {
                    target.push(' ');
                    target.push_str(&i18n::additional_items(*additional_transfers).to_string());
                }
                target
            }
            _ => self.render_void_candidate_summary(),
        }
    }

    fn confirmation_note_excerpt(&self) -> Option<String> {
        match self {
            Self::Expense { note, .. } => note
                .as_ref()
                .map(|note| excerpt_with_ascii_ellipsis(note.as_str(), 30)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecoveryReference {
    pub ledger_id_short: String,
    pub entry_id: LedgerEntryId,
    pub message_link: Option<String>,
}

impl RecoveryReference {
    pub fn render_line(&self) -> String {
        let mut line = format!(
            "{}ledger:{}/entry:{}",
            i18n::RECOVERY_REFERENCE_PREFIX,
            self.ledger_id_short,
            self.entry_id.0
        );
        if let Some(message_link) = &self.message_link {
            line.push_str(" | <");
            line.push_str(message_link);
            line.push('>');
        }
        line
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublicExpenseMessageModel {
    pub entry_id: LedgerEntryId,
    pub effective_date: LedgerEffectiveDate,
    pub payer_display_name: SafeLiteralText,
    pub amount: String,
    pub participant_rows: Vec<ParticipantShareRow>,
    pub note: Option<SafeLiteralText>,
    pub actor_display_name: SafeLiteralText,
    pub recorded_at: BusinessDateTime,
    pub recovery_reference: RecoveryReference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublicSettlementMessageModel {
    pub entry_id: LedgerEntryId,
    pub recorded_date: LedgerEffectiveDate,
    pub transfers: Vec<TransferRow>,
    pub actor_display_name: SafeLiteralText,
    pub recorded_at: BusinessDateTime,
    pub recovery_reference: RecoveryReference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublicVoidMessageModel {
    pub entry_id: LedgerEntryId,
    pub voider_display_name: SafeLiteralText,
    pub voided_at: BusinessDateTime,
    pub original_summary: LedgerSurfaceSummary,
    pub recorded_at: BusinessDateTime,
    pub recovery_reference: RecoveryReference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublicSealMessageModel {
    pub entry_id: LedgerEntryId,
    pub actor_display_name: SafeLiteralText,
    pub through_entry_id: LedgerEntryId,
    pub through_summary: LedgerSurfaceSummary,
    pub recorded_at: BusinessDateTime,
    pub recovery_reference: RecoveryReference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublicBalanceAdjustmentMessageModel {
    pub entry_id: LedgerEntryId,
    pub actor_display_name: SafeLiteralText,
    pub reason: SafeLiteralText,
    pub impacts: Vec<BalanceImpactRow>,
    pub recorded_at: BusinessDateTime,
    pub recovery_reference: RecoveryReference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PublicCanonicalMessageModel {
    Expense(PublicExpenseMessageModel),
    Settlement(PublicSettlementMessageModel),
    Void(PublicVoidMessageModel),
    Seal(PublicSealMessageModel),
    BalanceAdjustment(PublicBalanceAdjustmentMessageModel),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ReadViewKind {
    #[default]
    Review,
    Ledger,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ReadViewRoute {
    #[default]
    ReviewThread,
    ReviewParent,
    LedgerCommand,
    LedgerPanel,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VoidedEntryRow {
    pub void_entry_id: LedgerEntryId,
    pub voider_display_name: SafeLiteralText,
    pub voided_at: BusinessDateTime,
    pub original_summary: LedgerSurfaceSummary,
    pub recovery_reference: RecoveryReference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SealedRangeSummary {
    pub through_entry_id: LedgerEntryId,
    pub through_summary: LedgerSurfaceSummary,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BalanceAdjustmentSummary {
    pub actor_display_name: SafeLiteralText,
    pub reason: SafeLiteralText,
    pub impacts: Vec<BalanceImpactRow>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ReadViewSectionVisibility {
    pub balances: bool,
    pub transfers: bool,
    pub participants: bool,
    pub voided_entries: bool,
    pub confirmed: bool,
    pub route_guidance: bool,
    pub footer: bool,
}

impl Default for ReadViewSectionVisibility {
    fn default() -> Self {
        Self {
            balances: true,
            transfers: true,
            participants: true,
            voided_entries: true,
            confirmed: true,
            route_guidance: true,
            footer: true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ReadViewPageModel {
    pub kind: ReadViewKind,
    pub route: ReadViewRoute,
    pub title: std::borrow::Cow<'static, str>,
    pub uncertain_write: bool,
    pub stale_page: bool,
    pub page_indicator: Option<String>,
    pub snapshot_notice: Option<String>,
    pub route_guidance_lines: Vec<String>,
    pub recovery_cta: RecoveryCta,
    pub recovery_url: Option<String>,
    pub missing_thread_note: bool,
    pub balances: Vec<BalanceRow>,
    pub transfers: Vec<TransferRow>,
    pub participants: Vec<SafeLiteralText>,
    pub voided_entries: Vec<VoidedEntryRow>,
    pub sealed_range: Option<SealedRangeSummary>,
    pub balance_adjustments: Vec<BalanceAdjustmentSummary>,
    pub footer_lines: Vec<String>,
    pub visible_sections: ReadViewSectionVisibility,
    pub empty_state: Option<std::borrow::Cow<'static, str>>,
    pub action_rows: Vec<SurfaceActionRow>,
    pub ephemeral: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct WriteRejectionSurfaceModel {
    pub title: String,
    pub body_lines: Vec<String>,
    pub action_rows: Vec<SurfaceActionRow>,
    pub ephemeral: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct UncertainWriteSurfaceModel {
    pub title: String,
    pub status_lines: Vec<String>,
    pub recovery_reference: Option<String>,
    pub action_rows: Vec<SurfaceActionRow>,
    pub ephemeral: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderedCanonicalMessage {
    body: String,
}

const PUBLIC_COMPACT_SUMMARY_LIMIT: usize = 140;
const PUBLIC_COMPACT_REASON_LIMIT: usize = 48;
const PUBLIC_COMPACT_IMPACT_LIMIT: usize = 96;

impl RenderedCanonicalMessage {
    pub fn new(body: impl Into<String>) -> Result<Self, RenderBudgetError> {
        let body = body.into();
        validate_message_content(&body)?;
        validate_canonical_message_recovery_shape(&body)?;
        Ok(Self { body })
    }

    pub fn body(&self) -> &str {
        &self.body
    }
}

fn validate_canonical_message_recovery_shape(body: &str) -> Result<(), RenderBudgetError> {
    let lines: Vec<&str> = body.lines().collect();
    let recovery_reference_indices = lines
        .iter()
        .enumerate()
        .filter(|(_, line)| line.starts_with(i18n::RECOVERY_REFERENCE_PREFIX))
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    let recovery_reference_count = recovery_reference_indices.len();

    match recovery_reference_count {
        0 => return Err(RenderBudgetError::MissingRecoveryReference),
        1 => {}
        actual => return Err(RenderBudgetError::MultipleRecoveryReferences { actual }),
    }

    if recovery_reference_indices[0] + 1 != lines.len() {
        return Err(RenderBudgetError::MissingRecoveryReference);
    }

    let truncation_marker_count = lines
        .iter()
        .filter(|line| **line == i18n::PUBLIC_TRUNCATION_OMITTED)
        .count();
    let attachment_guidance_count = lines
        .iter()
        .filter(|line| **line == i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE)
        .count();
    if truncation_marker_count != attachment_guidance_count || truncation_marker_count > 1 {
        return Err(RenderBudgetError::InvalidPublicTruncationCue);
    }

    if truncation_marker_count == 1
        && (lines.len() < 3
            || lines[lines.len() - 3] != i18n::PUBLIC_TRUNCATION_OMITTED
            || lines[lines.len() - 2] != i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE)
    {
        return Err(RenderBudgetError::InvalidPublicTruncationCue);
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderedSurface {
    pub body: String,
    pub action_rows: Vec<SurfaceActionRow>,
    pub ephemeral: bool,
}

impl RenderedSurface {
    pub fn new(
        body: impl Into<String>,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Result<Self, RenderBudgetError> {
        let body = body.into();
        validate_message_content(&body)?;
        let action_rows = normalize_action_rows(action_rows)?;
        Ok(Self {
            body,
            action_rows,
            ephemeral,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RecoveryCta {
    ThreadLink,
    ParentLink,
    RecoveryReferenceOnly,
    #[default]
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RecoveryContext {
    pub canonical_thread_known: bool,
    pub canonical_thread_accessible: bool,
    pub tracked_parent_known: bool,
    pub tracked_parent_accessible: bool,
    pub recovery_reference_available: bool,
}

pub struct DiscordLedgerPresenter;

impl DiscordLedgerPresenter {
    pub fn render_expense_step(
        model: &ExpenseSurfaceModel,
    ) -> Result<RenderedSurface, RenderBudgetError> {
        RenderedSurface::new(
            render_sections([
                Some(model.title.as_ref()),
                model.validation_message.as_deref(),
                non_empty_join(&model.summary_lines).as_deref(),
                non_empty_join(&model.detail_lines).as_deref(),
            ]),
            model.action_rows.clone(),
            model.ephemeral,
        )
    }

    pub fn render_expense_success(
        model: &ExpenseSuccessSurfaceModel,
    ) -> Result<RenderedSurface, RenderBudgetError> {
        RenderedSurface::new(
            render_sections([non_empty_join(&model.body_lines).as_deref()]),
            model.action_rows.clone(),
            model.ephemeral,
        )
    }

    pub fn render_panel(model: &PanelSurfaceModel) -> Result<RenderedSurface, RenderBudgetError> {
        RenderedSurface::new(
            render_sections([
                Some(model.thread_cue.as_str()),
                model.status_line.as_deref(),
            ]),
            vec![SurfaceActionRow::Buttons(vec![
                SurfaceButton::Interactive {
                    label: i18n::panel_record_button_label().to_owned(),
                    custom_id: "ledger:panel:expense".to_owned(),
                    style: SurfaceInteractiveButtonStyle::Primary,
                    disabled: model.button_states.record_disabled,
                },
                SurfaceButton::Interactive {
                    label: i18n::panel_review_button_label().to_owned(),
                    custom_id: "ledger:panel:review".to_owned(),
                    style: SurfaceInteractiveButtonStyle::Secondary,
                    disabled: model.button_states.review_disabled,
                },
                SurfaceButton::Interactive {
                    label: i18n::panel_ledger_button_label().to_owned(),
                    custom_id: "ledger:panel:ledger".to_owned(),
                    style: SurfaceInteractiveButtonStyle::Secondary,
                    disabled: model.button_states.ledger_disabled,
                },
                SurfaceButton::Interactive {
                    label: i18n::panel_void_button_label().to_owned(),
                    custom_id: "ledger:panel:void".to_owned(),
                    style: SurfaceInteractiveButtonStyle::Secondary,
                    disabled: model.button_states.void_disabled,
                },
            ])],
            model.ephemeral,
        )
    }

    pub fn render_member_picker(
        model: &PickerSurfaceModel,
    ) -> Result<RenderedSurface, RenderBudgetError> {
        let mut action_rows = Vec::new();
        if let Some(select_menu) = model.select_menu.clone() {
            action_rows.push(SurfaceActionRow::Select(select_menu));
        }
        action_rows.extend(model.action_rows.clone());

        RenderedSurface::new(
            render_sections([
                Some(model.title.as_ref()),
                model.selected_summary.as_deref(),
                model.page_indicator.as_deref(),
                model.range_indicator.as_deref(),
                model.snapshot_notice.as_deref(),
                non_empty_join(&model.helper_lines).as_deref(),
            ]),
            action_rows,
            model.ephemeral,
        )
    }

    pub fn render_public_entry(
        model: &PublicCanonicalMessageModel,
    ) -> Result<RenderedCanonicalMessage, RenderBudgetError> {
        match model {
            PublicCanonicalMessageModel::Expense(model) => render_compacted_canonical_message(
                render_public_expense_full(model),
                render_public_expense_compact(model),
            ),
            PublicCanonicalMessageModel::Settlement(model) => render_compacted_canonical_message(
                render_public_settlement_full(model),
                render_public_settlement_compact(model),
            ),
            PublicCanonicalMessageModel::Void(model) => render_compacted_canonical_message(
                render_public_void_full(model),
                render_public_void_compact(model),
            ),
            PublicCanonicalMessageModel::Seal(model) => render_compacted_canonical_message(
                render_public_seal_full(model),
                render_public_seal_compact(model),
            ),
            PublicCanonicalMessageModel::BalanceAdjustment(model) => {
                render_compacted_canonical_message(
                    render_public_balance_adjustment_full(model),
                    render_public_balance_adjustment_compact(model),
                )
            }
        }
    }

    pub fn render_read_view_page(
        model: &ReadViewPageModel,
    ) -> Result<RenderedSurface, RenderBudgetError> {
        let advisory = model
            .uncertain_write
            .then(|| i18n::read_uncertain_write_advisory().to_owned());
        let mut action_rows = model.action_rows.clone();
        append_recovery_cta_row(
            &mut action_rows,
            model.recovery_cta,
            model.recovery_url.as_deref(),
        );

        let body = if model.stale_page {
            render_sections([
                Some(model.title.as_ref()),
                Some(read_view_stale_message(model.kind)),
                model
                    .missing_thread_note
                    .then_some(i18n::no_ledger_thread_yet_note()),
            ])
        } else if let Some(empty_state) = &model.empty_state {
            render_sections([
                Some(model.title.as_ref()),
                advisory.as_deref(),
                Some(empty_state.as_ref()),
                model
                    .missing_thread_note
                    .then_some(i18n::no_ledger_thread_yet_note()),
            ])
        } else {
            match model.kind {
                ReadViewKind::Review => {
                    let balances_section = render_review_balances_section(model);
                    let transfers_section = render_review_transfers_section(model);
                    let route_guidance: Vec<&str> = model
                        .route_guidance_lines
                        .iter()
                        .map(String::as_str)
                        .collect();
                    let review_instruction = if model.uncertain_write {
                        i18n::review_preview_blocked_instruction()
                    } else {
                        match model.route {
                            ReadViewRoute::ReviewParent => {
                                i18n::review_parent_preview_instruction()
                            }
                            _ => i18n::review_preview_instruction(),
                        }
                    };
                    render_sections([
                        Some(model.title.as_ref()),
                        advisory.as_deref(),
                        model.page_indicator.as_deref(),
                        model.snapshot_notice.as_deref(),
                        model
                            .visible_sections
                            .balances
                            .then_some(balances_section.as_str()),
                        model
                            .visible_sections
                            .transfers
                            .then_some(transfers_section.as_str()),
                        Some(review_instruction),
                        model
                            .visible_sections
                            .route_guidance
                            .then(|| non_empty_join(&route_guidance))
                            .flatten()
                            .as_deref(),
                        model
                            .missing_thread_note
                            .then_some(i18n::no_ledger_thread_yet_note()),
                    ])
                }
                ReadViewKind::Ledger => {
                    let balances_section = render_ledger_balances_section(model);
                    let participants_section = render_ledger_participants_section(model);
                    let voided_section = render_ledger_voided_section(model);
                    let confirmed_section = render_ledger_confirmed_section(model);
                    let footer = render_ledger_footer(model);
                    render_sections([
                        Some(model.title.as_ref()),
                        advisory.as_deref(),
                        model.page_indicator.as_deref(),
                        model.snapshot_notice.as_deref(),
                        model
                            .visible_sections
                            .balances
                            .then_some(balances_section.as_str()),
                        model
                            .visible_sections
                            .participants
                            .then_some(participants_section.as_str()),
                        model
                            .visible_sections
                            .voided_entries
                            .then_some(voided_section.as_str()),
                        model
                            .visible_sections
                            .confirmed
                            .then_some(confirmed_section.as_str()),
                        model
                            .visible_sections
                            .footer
                            .then_some(footer.as_deref())
                            .flatten(),
                        model
                            .missing_thread_note
                            .then_some(i18n::no_ledger_thread_yet_note()),
                    ])
                }
            }
        };

        RenderedSurface::new(body, action_rows, model.ephemeral)
    }

    pub fn render_void_flow(
        model: &VoidSurfaceModel,
    ) -> Result<RenderedSurface, RenderBudgetError> {
        let mut action_rows = model.action_rows.clone();
        append_recovery_cta_row(
            &mut action_rows,
            model.recovery_cta,
            model.recovery_url.as_deref(),
        );

        let body = if model.stale_page {
            render_sections([
                Some(model.title.as_ref()),
                Some(i18n::stale_void_page_message()),
                model
                    .missing_thread_note
                    .then_some(i18n::no_ledger_thread_yet_note()),
            ])
        } else {
            let confirmation_lines = model
                .confirmation
                .as_ref()
                .map(render_void_confirmation_lines)
                .unwrap_or_default();
            let candidate_lines = model
                .candidates
                .iter()
                .map(render_void_candidate_line)
                .collect::<Vec<_>>();

            render_sections([
                Some(model.title.as_ref()),
                model.page_indicator.as_deref(),
                model.snapshot_notice.as_deref(),
                non_empty_join(&model.phase_copy).as_deref(),
                non_empty_join(&model.warning_lines).as_deref(),
                non_empty_join(&confirmation_lines).as_deref(),
                non_empty_join(&candidate_lines).as_deref(),
                model
                    .missing_thread_note
                    .then_some(i18n::no_ledger_thread_yet_note()),
            ])
        };

        RenderedSurface::new(body, action_rows, model.ephemeral)
    }

    pub fn render_write_rejection(
        model: &WriteRejectionSurfaceModel,
    ) -> Result<RenderedSurface, RenderBudgetError> {
        RenderedSurface::new(
            render_sections([
                Some(model.title.as_ref()),
                non_empty_join(&model.body_lines).as_deref(),
            ]),
            model.action_rows.clone(),
            model.ephemeral,
        )
    }

    pub fn render_uncertain_write_block(
        model: &UncertainWriteSurfaceModel,
    ) -> Result<RenderedSurface, RenderBudgetError> {
        RenderedSurface::new(
            render_sections([
                Some(model.title.as_ref()),
                non_empty_join(&model.status_lines).as_deref(),
                model.recovery_reference.as_deref(),
            ]),
            model.action_rows.clone(),
            model.ephemeral,
        )
    }

    pub fn select_recovery_cta(context: RecoveryContext) -> RecoveryCta {
        if context.canonical_thread_known && context.canonical_thread_accessible {
            return RecoveryCta::ThreadLink;
        }

        if context.tracked_parent_known && context.tracked_parent_accessible {
            return RecoveryCta::ParentLink;
        }

        if context.recovery_reference_available {
            return RecoveryCta::RecoveryReferenceOnly;
        }

        RecoveryCta::None
    }
}

fn render_public_expense_full(model: &PublicExpenseMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_expense_header(model.entry_id.0).to_string(),
        i18n::public_date_line(model.effective_date).to_string(),
        i18n::public_payer_line(&model.payer_display_name).to_string(),
        i18n::public_amount_line(&model.amount).to_string(),
        i18n::public_participants_heading().to_owned(),
    ];
    lines.extend(
        model
            .participant_rows
            .iter()
            .map(render_participant_share_row),
    );
    if let Some(note) = &model.note {
        lines.push(i18n::public_note_line(note.as_str()).to_string());
    }
    lines.push(i18n::public_recorded_by_line(&model.actor_display_name).to_string());
    lines.push(i18n::public_recorded_at_line(&model.recorded_at).to_string());
    lines.push(model.recovery_reference.render_line());
    lines
}

fn render_public_expense_compact(model: &PublicExpenseMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_expense_header(model.entry_id.0).to_string(),
        i18n::public_date_line(model.effective_date).to_string(),
        i18n::public_payer_line(&model.payer_display_name).to_string(),
        i18n::public_amount_line(&model.amount).to_string(),
        i18n::public_participants_heading().to_owned(),
    ];
    lines.extend(
        model
            .participant_rows
            .iter()
            .take(10)
            .map(render_compact_participant_share_row),
    );
    lines.push(i18n::public_recorded_by_line(&model.actor_display_name).to_string());
    lines.push(i18n::public_recorded_at_line(&model.recorded_at).to_string());
    lines.extend(public_truncation_lines(&model.recovery_reference));
    lines
}

fn render_public_settlement_full(model: &PublicSettlementMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_settlement_header(model.entry_id.0).to_string(),
        i18n::public_date_line(model.recorded_date).to_string(),
        i18n::public_transfer_heading().to_owned(),
    ];
    lines.extend(model.transfers.iter().map(render_transfer_row));
    lines.push(i18n::public_confirmed_by_line(&model.actor_display_name).to_string());
    lines.push(i18n::public_recorded_at_line(&model.recorded_at).to_string());
    lines.push(model.recovery_reference.render_line());
    lines
}

fn render_public_settlement_compact(model: &PublicSettlementMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_settlement_header(model.entry_id.0).to_string(),
        i18n::public_date_line(model.recorded_date).to_string(),
        i18n::public_transfer_heading().to_owned(),
    ];
    lines.extend(
        model
            .transfers
            .iter()
            .take(10)
            .map(render_compact_transfer_row),
    );
    lines.push(i18n::public_confirmed_by_line(&model.actor_display_name).to_string());
    lines.push(i18n::public_recorded_at_line(&model.recorded_at).to_string());
    lines.extend(public_truncation_lines(&model.recovery_reference));
    lines
}

fn render_public_void_full(model: &PublicVoidMessageModel) -> Vec<String> {
    vec![
        i18n::public_void_line(
            model.entry_id.0,
            &model.voider_display_name,
            &model.voided_at,
            model.original_summary.render_void_candidate_summary(),
        )
        .to_string(),
        i18n::public_void_preserved_line().to_owned(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
        model.recovery_reference.render_line(),
    ]
}

fn render_public_void_compact(model: &PublicVoidMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_void_line(
            model.entry_id.0,
            &model.voider_display_name,
            &model.voided_at,
            model.original_summary.render_public_void_compact_summary(),
        )
        .to_string(),
        i18n::public_void_preserved_line().to_owned(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
    ];
    lines.extend(public_truncation_lines(&model.recovery_reference));
    lines
}

fn render_public_seal_full(model: &PublicSealMessageModel) -> Vec<String> {
    vec![
        i18n::public_seal_header(model.entry_id.0).to_string(),
        i18n::public_seal_line(
            &model.actor_display_name,
            model.through_entry_id.0,
            model.through_summary.render_sealed_through_summary(),
        )
        .to_string(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
        model.recovery_reference.render_line(),
    ]
}

fn render_public_seal_compact(model: &PublicSealMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_seal_line(
            &model.actor_display_name,
            model.through_entry_id.0,
            excerpt_with_ellipsis(
                &model.through_summary.render_sealed_through_summary(),
                PUBLIC_COMPACT_SUMMARY_LIMIT,
            ),
        )
        .to_string(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
    ];
    lines.extend(public_truncation_lines(&model.recovery_reference));
    lines
}

fn render_public_balance_adjustment_full(
    model: &PublicBalanceAdjustmentMessageModel,
) -> Vec<String> {
    vec![
        i18n::public_balance_adjustment_header(model.entry_id.0).to_string(),
        i18n::public_balance_adjustment_line(
            &model.actor_display_name,
            model.reason.as_str(),
            render_impact_summary(&model.impacts),
        )
        .to_string(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
        model.recovery_reference.render_line(),
    ]
}

fn render_public_balance_adjustment_compact(
    model: &PublicBalanceAdjustmentMessageModel,
) -> Vec<String> {
    let mut lines = vec![
        i18n::public_balance_adjustment_line(
            &model.actor_display_name,
            excerpt_with_ellipsis(model.reason.as_str(), PUBLIC_COMPACT_REASON_LIMIT),
            excerpt_with_ellipsis(
                &render_impact_summary(&model.impacts),
                PUBLIC_COMPACT_IMPACT_LIMIT,
            ),
        )
        .to_string(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
    ];
    lines.extend(public_truncation_lines(&model.recovery_reference));
    lines
}

fn render_compacted_canonical_message(
    full_lines: Vec<String>,
    compact_lines: Vec<String>,
) -> Result<RenderedCanonicalMessage, RenderBudgetError> {
    let full = full_lines.join("\n");
    if validate_message_content(&full).is_ok() {
        return RenderedCanonicalMessage::new(full);
    }

    RenderedCanonicalMessage::new(compact_lines.join("\n"))
}

fn public_truncation_lines(recovery_reference: &RecoveryReference) -> Vec<String> {
    vec![
        i18n::PUBLIC_TRUNCATION_OMITTED.to_owned(),
        i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE.to_owned(),
        recovery_reference.render_line(),
    ]
}

fn render_review_balances_section(model: &ReadViewPageModel) -> String {
    let mut lines = vec![i18n::balances_explainer().to_owned()];
    if model.balances.is_empty() {
        lines.push(i18n::review_zero_balances().to_owned());
    } else {
        lines.extend(model.balances.iter().map(render_balance_row));
    }
    render_section(i18n::balances_heading(), lines)
}

fn render_review_transfers_section(model: &ReadViewPageModel) -> String {
    let lines = model
        .transfers
        .iter()
        .map(render_transfer_row)
        .collect::<Vec<_>>();
    render_section(i18n::settlement_plan_heading(), lines)
}

fn render_ledger_balances_section(model: &ReadViewPageModel) -> String {
    let mut lines = vec![i18n::balances_explainer().to_owned()];
    if model.balances.is_empty() {
        lines.push(i18n::ledger_zero_balances().to_owned());
    } else {
        lines.extend(model.balances.iter().map(render_balance_row));
    }
    render_section(i18n::balances_heading(), lines)
}

fn render_ledger_participants_section(model: &ReadViewPageModel) -> String {
    let mut lines = vec![i18n::participants_explainer().to_owned()];
    lines.extend(
        model
            .participants
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>(),
    );
    render_section(i18n::participants_heading(), lines)
}

fn render_ledger_voided_section(model: &ReadViewPageModel) -> String {
    let lines = if model.voided_entries.is_empty() {
        vec![i18n::voided_none().to_owned()]
    } else {
        model.voided_entries.iter().map(render_voided_row).collect()
    };
    render_section(i18n::voided_heading(), lines)
}

fn render_ledger_confirmed_section(model: &ReadViewPageModel) -> String {
    let mut lines = vec![render_sealed_range(model.sealed_range.as_ref())];
    lines.extend(
        model
            .balance_adjustments
            .iter()
            .map(render_balance_adjustment_summary),
    );
    render_section(i18n::confirmed_heading(), lines)
}

fn render_section(heading: &str, lines: Vec<String>) -> String {
    if lines.is_empty() {
        return heading.to_owned();
    }

    format!("{heading}\n{}", lines.join("\n"))
}

fn normalize_action_rows(
    action_rows: Vec<SurfaceActionRow>,
) -> Result<Vec<SurfaceActionRow>, RenderBudgetError> {
    validate_action_rows(action_rows.len())?;

    action_rows
        .into_iter()
        .map(|row| match row {
            SurfaceActionRow::Buttons(buttons) => {
                validate_buttons_in_row(buttons.len())?;
                let buttons = buttons
                    .into_iter()
                    .map(|button| {
                        validate_button_label(button.label())?;
                        if let Some(custom_id) = button.custom_id() {
                            validate_custom_id(custom_id)?;
                        }

                        Ok(button)
                    })
                    .collect::<Result<Vec<_>, RenderBudgetError>>()?;
                Ok(SurfaceActionRow::Buttons(buttons))
            }
            SurfaceActionRow::Select(select_menu) => {
                validate_custom_id(&select_menu.custom_id)?;
                validate_select_options(select_menu.options.len())?;
                validate_select_value_bounds(
                    select_menu.min_values,
                    select_menu.max_values,
                    select_menu.options.len(),
                )?;
                if let Some(placeholder) = &select_menu.placeholder {
                    validate_component_placeholder(placeholder)?;
                }
                let options = select_menu
                    .options
                    .into_iter()
                    .map(|option| {
                        validate_custom_id(&option.value)?;
                        let label = truncate_component_label(option.label.as_str());
                        validate_component_label(&label)?;
                        if let Some(description) = &option.description {
                            validate_component_description(description)?;
                        }
                        Ok(SurfaceSelectOption {
                            value: option.value,
                            label: SafeLiteralText::from_roster_label(&label)
                                .expect("validated component label should sanitize"),
                            description: option.description,
                            selected: option.selected,
                        })
                    })
                    .collect::<Result<Vec<_>, RenderBudgetError>>()?;

                Ok(SurfaceActionRow::Select(SurfaceSelectMenu {
                    custom_id: select_menu.custom_id,
                    placeholder: select_menu.placeholder,
                    options,
                    min_values: select_menu.min_values,
                    max_values: select_menu.max_values,
                    disabled: select_menu.disabled,
                }))
            }
        })
        .collect()
}

fn render_balance_row(row: &BalanceRow) -> String {
    match row.direction {
        BalanceDirection::Receive => {
            i18n::balance_row_receive(&row.display_name, &row.amount).to_string()
        }
        BalanceDirection::Pay => i18n::balance_row_pay(&row.display_name, &row.amount).to_string(),
    }
}

fn render_participant_share_row(row: &ParticipantShareRow) -> String {
    i18n::participant_share_row(&row.display_name, &row.share_amount).to_string()
}

fn render_compact_participant_share_row(row: &ParticipantShareRow) -> String {
    i18n::participant_share_row(&row.display_name, &row.share_amount).to_string()
}

fn render_transfer_row(row: &TransferRow) -> String {
    i18n::settlement_transfer_row(&row.from_display_name, &row.to_display_name, &row.amount)
        .to_string()
}

fn render_compact_transfer_row(row: &TransferRow) -> String {
    i18n::settlement_transfer_row(&row.from_display_name, &row.to_display_name, &row.amount)
        .to_string()
}

fn render_voided_row(row: &VoidedEntryRow) -> String {
    format!(
        "{} | {}",
        i18n::public_void_line(
            row.void_entry_id.0,
            &row.voider_display_name,
            &row.voided_at,
            row.original_summary.render_void_candidate_summary(),
        ),
        row.recovery_reference.render_line()
    )
}

fn render_ledger_footer(model: &ReadViewPageModel) -> Option<String> {
    non_empty_join(&model.footer_lines)
}

fn render_sealed_range(sealed_range: Option<&SealedRangeSummary>) -> String {
    sealed_range.map_or_else(
        || i18n::sealed_range_none().to_owned(),
        |sealed_range| {
            i18n::sealed_range_line(
                sealed_range.through_entry_id.0,
                sealed_range.through_summary.date(),
                sealed_range.through_summary.render_sealed_summary(),
            )
            .to_string()
        },
    )
}

fn render_balance_adjustment_summary(summary: &BalanceAdjustmentSummary) -> String {
    i18n::balance_adjustment_row(
        &summary.actor_display_name,
        summary.reason.as_str(),
        render_impact_summary(&summary.impacts),
    )
    .to_string()
}

fn render_impact_summary(rows: &[BalanceImpactRow]) -> String {
    rows.iter()
        .map(|row| match row.direction {
            BalanceDirection::Receive => {
                i18n::impact_summary_receive(&row.display_name, &row.amount).to_string()
            }
            BalanceDirection::Pay => {
                i18n::impact_summary_pay(&row.display_name, &row.amount).to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_void_candidate_line(candidate: &super::void_surfaces::VoidCandidateRow) -> String {
    format!(
        "{} | {}",
        candidate.summary.render_void_candidate_summary(),
        candidate.recovery_reference.render_line()
    )
}

fn render_void_confirmation_lines(recap: &VoidConfirmationRecap) -> Vec<String> {
    let mut lines = vec![
        i18n::void_confirm_date_line(recap.summary.date()).to_string(),
        i18n::void_confirm_target_line(recap.summary.render_confirmation_target()).to_string(),
        i18n::void_confirm_amount_line(&recap.total_amount).to_string(),
    ];
    if let Some(note_excerpt) = recap.summary.confirmation_note_excerpt() {
        lines.push(i18n::void_confirm_note_line(note_excerpt).to_string());
    }
    lines.push(recap.recovery_reference.render_line());
    lines
}

fn read_view_stale_message(kind: ReadViewKind) -> &'static str {
    match kind {
        ReadViewKind::Review => i18n::stale_review_page_message(),
        ReadViewKind::Ledger => i18n::stale_ledger_page_message(),
    }
}

fn append_recovery_cta_row(
    action_rows: &mut Vec<SurfaceActionRow>,
    recovery_cta: RecoveryCta,
    recovery_url: Option<&str>,
) {
    let Some((label, url)) = recovery_cta_button(recovery_cta, recovery_url) else {
        return;
    };
    action_rows.push(SurfaceActionRow::Buttons(vec![SurfaceButton::Link {
        label,
        url,
        disabled: false,
    }]));
}

fn recovery_cta_button(
    recovery_cta: RecoveryCta,
    recovery_url: Option<&str>,
) -> Option<(String, String)> {
    let url = recovery_url?.to_owned();
    let label = match recovery_cta {
        RecoveryCta::ThreadLink => i18n::open_ledger_thread_label(),
        RecoveryCta::ParentLink => i18n::open_parent_channel_label(),
        RecoveryCta::RecoveryReferenceOnly | RecoveryCta::None => return None,
    };
    Some((label.to_owned(), url))
}

fn render_sections<'a>(sections: impl IntoIterator<Item = Option<&'a str>>) -> String {
    sections
        .into_iter()
        .flatten()
        .filter(|section| !section.is_empty())
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn non_empty_join(lines: &[impl AsRef<str>]) -> Option<String> {
    let joined = lines
        .iter()
        .map(AsRef::as_ref)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");
    (!joined.is_empty()).then_some(joined)
}

fn excerpt_with_ellipsis(text: &str, limit: usize) -> String {
    let actual = text.chars().count();
    if actual <= limit {
        return text.to_owned();
    }

    let prefix: String = text.chars().take(limit).collect();
    format!("{prefix}…")
}

fn excerpt_with_ascii_ellipsis(text: &str, limit: usize) -> String {
    let actual = text.chars().count();
    if actual <= limit {
        return text.to_owned();
    }

    let prefix: String = text.chars().take(limit).collect();
    format!("{prefix}...")
}

#[cfg(test)]
mod tests {
    use super::{
        BalanceAdjustmentSummary, BalanceDirection, BalanceImpactRow, BalanceRow, BusinessDateTime,
        DiscordLedgerPresenter, ExpenseSurfaceModel, LedgerSurfaceSummary, PanelButtonStates,
        PanelSurfaceModel, PublicBalanceAdjustmentMessageModel, PublicCanonicalMessageModel,
        PublicExpenseMessageModel, PublicSealMessageModel, PublicSettlementMessageModel,
        PublicVoidMessageModel, ReadViewKind, ReadViewPageModel, ReadViewRoute,
        ReadViewSectionVisibility, RecoveryContext, RecoveryCta, RecoveryReference,
        RenderBudgetError, RenderedCanonicalMessage, SafeLiteralText, SealedRangeSummary,
        SurfaceActionRow, SurfaceButton, SurfaceInteractiveButtonStyle, SurfaceSelectMenu,
        SurfaceSelectOption, TransferRow, UncertainWriteSurfaceModel, VoidedEntryRow,
        render_public_balance_adjustment_compact, render_public_seal_compact,
        render_public_void_compact,
    };
    use crate::discord_ledger::{
        budgets::{validate_button_label, validate_component_placeholder},
        pickers::PickerSurfaceModel,
        void_surfaces::{
            VoidCandidateRow, VoidConfirmationRecap, VoidRetargetReason, VoidSurfaceModel,
        },
    };
    use walicord_application::ledger::{LedgerEffectiveDate, LedgerEntryId};
    use walicord_i18n as i18n;

    #[test]
    fn recovery_cta_prefers_the_thread_link_when_it_is_accessible() {
        let actual = DiscordLedgerPresenter::select_recovery_cta(RecoveryContext {
            canonical_thread_known: true,
            canonical_thread_accessible: true,
            tracked_parent_known: true,
            tracked_parent_accessible: true,
            recovery_reference_available: true,
        });

        assert_eq!(actual, RecoveryCta::ThreadLink);
    }

    #[test]
    fn recovery_cta_falls_back_to_parent_then_reference_only_then_none() {
        let parent = DiscordLedgerPresenter::select_recovery_cta(RecoveryContext {
            canonical_thread_known: true,
            canonical_thread_accessible: false,
            tracked_parent_known: true,
            tracked_parent_accessible: true,
            recovery_reference_available: true,
        });
        let reference_only = DiscordLedgerPresenter::select_recovery_cta(RecoveryContext {
            canonical_thread_known: false,
            canonical_thread_accessible: false,
            tracked_parent_known: false,
            tracked_parent_accessible: false,
            recovery_reference_available: true,
        });
        let none = DiscordLedgerPresenter::select_recovery_cta(RecoveryContext::default());

        assert_eq!(parent, RecoveryCta::ParentLink);
        assert_eq!(reference_only, RecoveryCta::RecoveryReferenceOnly);
        assert_eq!(none, RecoveryCta::None);
    }

    #[test]
    fn recovery_reference_only_and_none_do_not_emit_dead_cta_buttons() {
        for recovery_cta in [RecoveryCta::RecoveryReferenceOnly, RecoveryCta::None] {
            let actual = DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel {
                kind: ReadViewKind::Review,
                route: ReadViewRoute::ReviewParent,
                title: std::borrow::Cow::Borrowed("清算確認"),
                uncertain_write: false,
                stale_page: false,
                page_indicator: None,
                snapshot_notice: None,
                route_guidance_lines: Vec::new(),
                recovery_cta,
                recovery_url: Some("https://discord.com/channels/1/2".to_owned()),
                missing_thread_note: false,
                balances: Vec::new(),
                transfers: Vec::new(),
                participants: Vec::new(),
                voided_entries: Vec::new(),
                sealed_range: None,
                balance_adjustments: Vec::new(),
                footer_lines: Vec::new(),
                visible_sections: ReadViewSectionVisibility::default(),
                empty_state: Some(std::borrow::Cow::Borrowed("empty")),
                action_rows: Vec::new(),
                ephemeral: true,
            })
            .expect("surface should render");

            assert!(actual.action_rows.is_empty());
            assert!(!actual.body.contains("https://discord.com/channels/1/2"));
        }
    }

    #[test]
    fn expense_step_layout_keeps_validation_before_summary_and_detail_sections() {
        let actual = DiscordLedgerPresenter::render_expense_step(&ExpenseSurfaceModel {
            title: "4/4 確認".to_owned(),
            summary_lines: vec!["金額: 1500円".to_owned(), "日付: 2026-05-10".to_owned()],
            detail_lines: vec!["支払者: Alice".to_owned()],
            validation_message: Some("参加者を1人以上選んでください。".to_owned()),
            action_rows: Vec::new(),
            ephemeral: true,
        })
        .expect("surface should render");

        assert_eq!(
            actual.body,
            "4/4 確認\n\n参加者を1人以上選んでください。\n\n金額: 1500円\n日付: 2026-05-10\n\n支払者: Alice"
        );
    }

    #[test]
    fn public_expense_message_uses_the_fixed_full_template() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Expense(PublicExpenseMessageModel {
                entry_id: LedgerEntryId(7),
                effective_date: date("2026-05-25"),
                payer_display_name: name("Alice"),
                amount: "1500".to_owned(),
                participant_rows: vec![
                    participant_share("Alice", "0"),
                    participant_share("Bob", "750"),
                    participant_share("Carol", "750"),
                ],
                note: Some(note("ランチ")),
                actor_display_name: name("Alice"),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(7),
            }),
        )
        .expect("canonical message should render");

        assert_eq!(
            actual.body(),
            "支出 [#7]\n日付: 2026-05-25\n支払者: Alice\n金額: 1500円\n参加者:\n- Alice: 0円\n- Bob: 750円\n- Carol: 750円\nメモ: ランチ\n記録者: Alice\n記録日時: 2026-05-25 18:55\n復旧用の参照: ledger:abcd1234/entry:7 | <https://discord.com/channels/1/2/7>"
        );
    }

    #[test]
    fn public_expense_message_compacts_to_the_first_ten_participants() {
        let participants = (1..=15)
            .map(|index| participant_share(&format!("参加者{index:02}{}", "あ".repeat(150)), "100"))
            .collect::<Vec<_>>();

        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Expense(PublicExpenseMessageModel {
                entry_id: LedgerEntryId(7),
                effective_date: date("2026-05-25"),
                payer_display_name: name("Alice"),
                amount: "1500".to_owned(),
                participant_rows: participants,
                note: Some(note("省略されるメモ")),
                actor_display_name: name("Alice"),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(7),
            }),
        )
        .expect("canonical message should compact");

        assert!(
            actual
                .body()
                .contains(&format!("- 参加者10{}: 100円", "あ".repeat(150)))
        );
        assert!(!actual.body().contains("省略されるメモ"));
        assert!(actual.body().contains(i18n::PUBLIC_TRUNCATION_OMITTED));
        assert!(!actual.body().contains("参加者11"));
    }

    #[test]
    fn public_expense_compact_renderer_preserves_full_labels_within_budget() {
        let payer = format!("支払者{}", "い".repeat(80));
        let actor = format!("記録者{}", "う".repeat(80));
        let participant = format!("参加者10{}", "あ".repeat(80));
        let participants = (1..=10)
            .map(|index| participant_share(&format!("参加者{index:02}{}", "あ".repeat(80)), "100"))
            .collect::<Vec<_>>();

        let actual = RenderedCanonicalMessage::new(
            super::render_public_expense_compact(&PublicExpenseMessageModel {
                entry_id: LedgerEntryId(7),
                effective_date: date("2026-05-25"),
                payer_display_name: name(&payer),
                amount: "1500".to_owned(),
                participant_rows: participants,
                note: None,
                actor_display_name: name(&actor),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(7),
            })
            .join("\n"),
        )
        .expect("compact message should preserve full labels");

        assert!(actual.body().contains(&payer));
        assert!(actual.body().contains(&actor));
        assert!(actual.body().contains(&participant));
    }

    #[test]
    fn public_expense_compact_renderer_returns_budget_error_for_extreme_display_names() {
        let participants = (1..=10)
            .map(|index| participant_share(&format!("参加者{index:02}{}", "あ".repeat(400)), "100"))
            .collect::<Vec<_>>();

        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Expense(PublicExpenseMessageModel {
                entry_id: LedgerEntryId(7),
                effective_date: date("2026-05-25"),
                payer_display_name: name(&format!("支払者{}", "い".repeat(400))),
                amount: "1500".to_owned(),
                participant_rows: participants,
                note: Some(note("省略されるメモ")),
                actor_display_name: name(&format!("記録者{}", "う".repeat(400))),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(7),
            }),
        );

        assert!(actual.is_err());
    }

    #[test]
    fn public_settlement_message_uses_the_fixed_full_template() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Settlement(PublicSettlementMessageModel {
                entry_id: LedgerEntryId(8),
                recorded_date: date("2026-05-25"),
                transfers: vec![
                    transfer("Alice", "Bob", "300"),
                    transfer("Alice", "Carol", "200"),
                ],
                actor_display_name: name("Alice"),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(8),
            }),
        )
        .expect("canonical message should render");

        assert_eq!(
            actual.body(),
            "清算 [#8]\n日付: 2026-05-25\n送金予定:\n- Alice -> Bob 300円\n- Alice -> Carol 200円\n確定者: Alice\n記録日時: 2026-05-25 18:55\n復旧用の参照: ledger:abcd1234/entry:8 | <https://discord.com/channels/1/2/8>"
        );
    }

    #[test]
    fn public_settlement_message_compacts_to_the_first_ten_transfers() {
        let transfers = (1..=15)
            .map(|index| {
                transfer(
                    &format!("Alice{}", "あ".repeat(60)),
                    &format!("Bob{index:02}{}", "い".repeat(60)),
                    "300",
                )
            })
            .collect::<Vec<_>>();

        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Settlement(PublicSettlementMessageModel {
                entry_id: LedgerEntryId(8),
                recorded_date: date("2026-05-25"),
                transfers,
                actor_display_name: name("Alice"),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(8),
            }),
        )
        .expect("canonical message should compact");

        assert!(actual.body().contains(&format!("Bob10{}", "い".repeat(60))));
        assert!(!actual.body().contains("Bob11"));
        assert!(
            actual
                .body()
                .contains(i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE)
        );
    }

    #[test]
    fn public_settlement_compact_renderer_preserves_full_labels_within_budget() {
        let actor = format!("確定者{}", "う".repeat(60));
        let sender = format!("送金者10{}", "あ".repeat(60));
        let receiver = format!("受取者10{}", "い".repeat(60));
        let transfers = (1..=10)
            .map(|index| {
                transfer(
                    &format!("送金者{index:02}{}", "あ".repeat(60)),
                    &format!("受取者{index:02}{}", "い".repeat(60)),
                    "300",
                )
            })
            .collect::<Vec<_>>();

        let actual = RenderedCanonicalMessage::new(
            super::render_public_settlement_compact(&PublicSettlementMessageModel {
                entry_id: LedgerEntryId(8),
                recorded_date: date("2026-05-25"),
                transfers,
                actor_display_name: name(&actor),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(8),
            })
            .join("\n"),
        )
        .expect("compact message should preserve full labels");

        assert!(actual.body().contains(&actor));
        assert!(actual.body().contains(&sender));
        assert!(actual.body().contains(&receiver));
    }

    #[test]
    fn public_settlement_compact_renderer_returns_budget_error_for_extreme_transfer_labels() {
        let transfers = (1..=10)
            .map(|index| {
                transfer(
                    &format!("送金者{index:02}{}", "あ".repeat(400)),
                    &format!("受取者{index:02}{}", "い".repeat(400)),
                    "300",
                )
            })
            .collect::<Vec<_>>();

        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Settlement(PublicSettlementMessageModel {
                entry_id: LedgerEntryId(8),
                recorded_date: date("2026-05-25"),
                transfers,
                actor_display_name: name(&format!("確定者{}", "う".repeat(400))),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(8),
            }),
        );

        assert!(actual.is_err());
    }

    #[test]
    fn public_void_message_uses_the_fixed_full_template() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Void(PublicVoidMessageModel {
                entry_id: LedgerEntryId(9),
                voider_display_name: name("Alice"),
                voided_at: timestamp("2026-05-25 18:55"),
                original_summary: LedgerSurfaceSummary::Expense {
                    date: date("2026-05-24"),
                    payer_display_name: name("Bob"),
                    amount: "1200".to_owned(),
                    note: Some(note("ランチ")),
                },
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(9),
            }),
        )
        .expect("canonical message should render");

        assert_eq!(
            actual.body(),
            "[#9] Alice が 2026-05-25 18:55 に取り消し: 2026-05-24 Bob の支払い 1200円 メモ: ランチ\n元の記録は 取り消し済み として残ります。\n記録日時: 2026-05-25 18:55\n復旧用の参照: ledger:abcd1234/entry:9 | <https://discord.com/channels/1/2/9>"
        );
    }

    #[test]
    fn public_void_message_uses_ascii_ellipsis_for_note_excerpts() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Void(PublicVoidMessageModel {
                entry_id: LedgerEntryId(9),
                voider_display_name: name("Alice"),
                voided_at: timestamp("2026-05-25 18:55"),
                original_summary: LedgerSurfaceSummary::Expense {
                    date: date("2026-05-24"),
                    payer_display_name: name("Bob"),
                    amount: "1200".to_owned(),
                    note: Some(note("123456789012345678901234567890X")),
                },
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(9),
            }),
        )
        .expect("canonical message should render");

        assert!(
            actual
                .body()
                .contains("メモ: 123456789012345678901234567890...")
        );
    }

    #[test]
    fn public_seal_message_reuses_the_sealed_range_summary() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Seal(PublicSealMessageModel {
                entry_id: LedgerEntryId(10),
                actor_display_name: name("Operator"),
                through_entry_id: LedgerEntryId(4),
                through_summary: LedgerSurfaceSummary::Settlement {
                    date: date("2026-05-20"),
                    from_display_name: name("Alice"),
                    to_display_name: name("Bob"),
                    amount: "400".to_owned(),
                    additional_transfers: 2,
                },
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(10),
            }),
        )
        .expect("canonical message should render");

        assert_eq!(
            actual.body(),
            "確認 [#10]\n確認 Operator: [#4] 2026-05-20 清算 Alice -> Bob ほか2件 まで確定\n記録日時: 2026-05-25 18:55\n復旧用の参照: ledger:abcd1234/entry:10 | <https://discord.com/channels/1/2/10>"
        );
    }

    #[test]
    fn public_balance_adjustment_message_uses_the_fixed_plain_language_summary() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::BalanceAdjustment(PublicBalanceAdjustmentMessageModel {
                entry_id: LedgerEntryId(11),
                actor_display_name: name("Operator"),
                reason: note("差額補正"),
                impacts: vec![
                    impact("Alice", "300", BalanceDirection::Receive),
                    impact("Bob", "300", BalanceDirection::Pay),
                ],
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(11),
            }),
        )
        .expect("canonical message should render");

        assert_eq!(
            actual.body(),
            "残高補正 [#11]\n残高補正 Operator: 差額補正 (Alice 受け取り 300円, Bob 支払い 300円)\n記録日時: 2026-05-25 18:55\n復旧用の参照: ledger:abcd1234/entry:11 | <https://discord.com/channels/1/2/11>"
        );
    }

    #[test]
    fn public_void_compact_branch_keeps_the_truncation_cue_and_recovery_reference() {
        let actual = RenderedCanonicalMessage::new(
            render_public_void_compact(&PublicVoidMessageModel {
                entry_id: LedgerEntryId(9),
                voider_display_name: name("Alice"),
                voided_at: timestamp("2026-05-25 18:55"),
                original_summary: LedgerSurfaceSummary::Expense {
                    date: date("2026-05-24"),
                    payer_display_name: name("Bob"),
                    amount: "1200".to_owned(),
                    note: Some(note("ランチ")),
                },
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(9),
            })
            .join("\n"),
        )
        .expect("compact message should satisfy Discord limits");

        assert!(
            actual
                .body()
                .contains(i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE)
        );
        assert!(
            actual
                .body()
                .contains("復旧用の参照: ledger:abcd1234/entry:9")
        );
    }

    #[test]
    fn public_void_compact_branch_truncates_extreme_original_summaries_and_still_renders() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Void(PublicVoidMessageModel {
                entry_id: LedgerEntryId(9),
                voider_display_name: name(&format!("取消者{}", "あ".repeat(1500))),
                voided_at: timestamp("2026-05-25 18:55"),
                original_summary: LedgerSurfaceSummary::Expense {
                    date: date("2026-05-24"),
                    payer_display_name: name(&format!("支払者{}", "い".repeat(1500))),
                    amount: "1200".to_owned(),
                    note: Some(note(&format!("メモ{}", "う".repeat(400)))),
                },
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(9),
            }),
        );

        assert!(actual.is_err());
    }

    #[test]
    fn public_void_compact_branch_preserves_amount_and_status_cues() {
        let voider = format!("取消者{}", "あ".repeat(80));
        let payer = format!("支払者{}", "い".repeat(80));
        let actual = RenderedCanonicalMessage::new(
            render_public_void_compact(&PublicVoidMessageModel {
                entry_id: LedgerEntryId(9),
                voider_display_name: name(&voider),
                voided_at: timestamp("2026-05-25 18:55"),
                original_summary: LedgerSurfaceSummary::Expense {
                    date: date("2026-05-24"),
                    payer_display_name: name(&payer),
                    amount: "1200".to_owned(),
                    note: Some(note("ランチ")),
                },
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(9),
            })
            .join("\n"),
        )
        .expect("compact message should preserve full labels");

        assert!(actual.body().contains("支払い 1200円"));
        assert!(actual.body().contains(&voider));
        assert!(actual.body().contains(&payer));
        assert!(
            actual
                .body()
                .contains(i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE)
        );
    }

    #[test]
    fn public_seal_compact_branch_keeps_the_truncation_cue_and_drops_the_header() {
        let actual = RenderedCanonicalMessage::new(
            render_public_seal_compact(&PublicSealMessageModel {
                entry_id: LedgerEntryId(10),
                actor_display_name: name("Operator"),
                through_entry_id: LedgerEntryId(4),
                through_summary: LedgerSurfaceSummary::Settlement {
                    date: date("2026-05-20"),
                    from_display_name: name("Alice"),
                    to_display_name: name("Bob"),
                    amount: "400".to_owned(),
                    additional_transfers: 2,
                },
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(10),
            })
            .join("\n"),
        )
        .expect("compact message should satisfy Discord limits");

        assert!(
            actual
                .body()
                .contains(i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE)
        );
        assert!(!actual.body().contains("確認 [#10]"));
    }

    #[test]
    fn public_balance_adjustment_compact_branch_keeps_the_truncation_cue_and_drops_header() {
        let actual = RenderedCanonicalMessage::new(
            render_public_balance_adjustment_compact(&PublicBalanceAdjustmentMessageModel {
                entry_id: LedgerEntryId(11),
                actor_display_name: name("Operator"),
                reason: note("差額補正"),
                impacts: vec![
                    impact("Alice", "300", BalanceDirection::Receive),
                    impact("Bob", "300", BalanceDirection::Pay),
                ],
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(11),
            })
            .join("\n"),
        )
        .expect("compact message should satisfy Discord limits");

        assert!(
            actual
                .body()
                .contains(i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE)
        );
        assert!(!actual.body().contains("残高補正 [#11]"));
    }

    #[test]
    fn review_view_renders_balances_then_transfers_and_uncertain_write_copy() {
        let actual = DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel {
            kind: ReadViewKind::Review,
            route: ReadViewRoute::ReviewThread,
            title: std::borrow::Cow::Borrowed("清算確認"),
            uncertain_write: true,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::ParentLink,
            recovery_url: Some("https://discord.com/channels/1/2/3".to_owned()),
            missing_thread_note: false,
            balances: vec![
                balance("Alice", "300", BalanceDirection::Receive),
                balance("Bob", "300", BalanceDirection::Pay),
            ],
            transfers: vec![transfer("Bob", "Alice", "300")],
            participants: Vec::new(),
            voided_entries: Vec::new(),
            sealed_range: None,
            balance_adjustments: Vec::new(),
            footer_lines: Vec::new(),
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        })
        .expect("surface should render");

        assert_eq!(
            actual.body,
            "清算確認\n\nℹ️ この台帳は現在書き込み確認中です。記録・取り消し・清算は一時的に制限されています。\n\n残高\n確認済み履歴と残高補正を含む現在差額\n- Alice: 受け取り 300円\n- Bob: 支払い 300円\n\n送金予定\n- Bob -> Alice 300円\n\nこの台帳は書き込み状態を確認中です。台帳スレッドを確認し、復旧後に /settle を実行してください。"
        );
        let SurfaceActionRow::Buttons(buttons) = &actual.action_rows[0] else {
            panic!("expected recovery button row");
        };
        assert_eq!(buttons[0].label(), "親チャンネルを開く");
    }

    #[test]
    fn member_picker_surface_renders_summary_before_paging_and_keeps_the_select_menu() {
        let actual = DiscordLedgerPresenter::render_member_picker(&PickerSurfaceModel {
            title: "1/4 支払者".to_owned(),
            page_indicator: Some(i18n::page_indicator(1, 2).to_string()),
            range_indicator: Some("1-25 / 30".to_owned()),
            snapshot_notice: Some("この表示は固定スナップショットです。".to_owned()),
            helper_lines: vec!["候補から 1 人を選んでください。".to_owned()],
            selected_summary: Some("選択中: Alice".to_owned()),
            select_menu: Some(SurfaceSelectMenu {
                custom_id: "picker:members:page1".to_owned(),
                placeholder: Some("支払者を選択".to_owned()),
                options: vec![SurfaceSelectOption {
                    value: "member:1".to_owned(),
                    label: SafeLiteralText::from_roster_label("Alice")
                        .expect("label should sanitize"),
                    description: None,
                    selected: true,
                }],
                min_values: 1,
                max_values: 1,
                disabled: false,
            }),
            action_rows: Vec::new(),
            ephemeral: true,
        })
        .expect("picker should render");

        assert_eq!(
            actual.body,
            "1/4 支払者\n\n選択中: Alice\n\nページ 1/2\n\n1-25 / 30\n\nこの表示は固定スナップショットです。\n\n候補から 1 人を選んでください。"
        );
        let SurfaceActionRow::Select(select_menu) = &actual.action_rows[0] else {
            panic!("expected select menu row");
        };
        assert_eq!(select_menu.placeholder.as_deref(), Some("支払者を選択"));
        assert_eq!(select_menu.options[0].label.as_str(), "Alice");
    }

    #[test]
    fn member_picker_truncates_overlong_option_labels_before_budget_validation() {
        let actual = DiscordLedgerPresenter::render_member_picker(&PickerSurfaceModel {
            title: "1/4 支払者".to_owned(),
            page_indicator: None,
            range_indicator: None,
            snapshot_notice: None,
            helper_lines: Vec::new(),
            selected_summary: None,
            select_menu: Some(SurfaceSelectMenu {
                custom_id: "picker:members:page1".to_owned(),
                placeholder: Some("支払者を選択".to_owned()),
                options: vec![SurfaceSelectOption {
                    value: "member:1".to_owned(),
                    label: SafeLiteralText::from_roster_label(&format!(
                        "{}{}",
                        "田中 太郎 / 開発部",
                        " / 追加情報".repeat(20)
                    ))
                    .expect("label should sanitize"),
                    description: None,
                    selected: false,
                }],
                min_values: 1,
                max_values: 1,
                disabled: false,
            }),
            action_rows: Vec::new(),
            ephemeral: true,
        })
        .expect("picker should render");

        let SurfaceActionRow::Select(select_menu) = &actual.action_rows[0] else {
            panic!("expected select menu row");
        };
        assert!(
            select_menu.options[0]
                .label
                .as_str()
                .starts_with("田中 太郎 / 開発部")
        );
        assert!(select_menu.options[0].label.as_str().ends_with('…'));
        assert!(select_menu.options[0].label.as_str().chars().count() <= 100);
    }

    #[test]
    fn ledger_view_renders_fixed_section_order_with_confirmed_and_voided_rows() {
        let actual = DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel {
            kind: ReadViewKind::Ledger,
            route: ReadViewRoute::LedgerCommand,
            title: std::borrow::Cow::Borrowed("台帳"),
            uncertain_write: true,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            balances: vec![
                balance("Alice", "300", BalanceDirection::Receive),
                balance("Bob", "300", BalanceDirection::Pay),
            ],
            transfers: Vec::new(),
            participants: vec![name("Alice"), name("Bob")],
            voided_entries: vec![VoidedEntryRow {
                void_entry_id: LedgerEntryId(9),
                voider_display_name: name("Alice"),
                voided_at: timestamp("2026-05-25 18:55"),
                original_summary: LedgerSurfaceSummary::Expense {
                    date: date("2026-05-24"),
                    payer_display_name: name("Bob"),
                    amount: "1200".to_owned(),
                    note: Some(note("ランチ")),
                },
                recovery_reference: recovery_reference(9),
            }],
            sealed_range: Some(SealedRangeSummary {
                through_entry_id: LedgerEntryId(4),
                through_summary: LedgerSurfaceSummary::Settlement {
                    date: date("2026-05-20"),
                    from_display_name: name("Alice"),
                    to_display_name: name("Bob"),
                    amount: "400".to_owned(),
                    additional_transfers: 1,
                },
            }),
            balance_adjustments: vec![BalanceAdjustmentSummary {
                actor_display_name: name("Operator"),
                reason: note("差額補正"),
                impacts: vec![impact("Alice", "300", BalanceDirection::Receive)],
            }],
            footer_lines: vec!["表示範囲: 最新の検証済み台帳".to_owned()],
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        })
        .expect("surface should render");

        assert_eq!(
            actual.body,
            "台帳\n\nℹ️ この台帳は現在書き込み確認中です。記録・取り消し・清算は一時的に制限されています。\n\n残高\n確認済み履歴と残高補正を含む現在差額\n- Alice: 受け取り 300円\n- Bob: 支払い 300円\n\n参加者\nこれまで記録に出た人\nAlice\nBob\n\n取り消し済み\n[#9] Alice が 2026-05-25 18:55 に取り消し: 2026-05-24 Bob の支払い 1200円 メモ: ランチ | 復旧用の参照: ledger:abcd1234/entry:9 | <https://discord.com/channels/1/2/9>\n\n確認済み\n確認済み: [#4] 2026-05-20 清算 Alice -> Bob ほか1件 まで\n残高補正 Operator: 差額補正 (Alice 受け取り 300円)\n\n表示範囲: 最新の検証済み台帳"
        );
    }

    #[test]
    fn ledger_view_uses_the_inviting_empty_state_without_scope_footer() {
        let actual = DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel {
            kind: ReadViewKind::Ledger,
            route: ReadViewRoute::LedgerPanel,
            title: std::borrow::Cow::Borrowed("台帳"),
            uncertain_write: false,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            balances: Vec::new(),
            transfers: Vec::new(),
            participants: Vec::new(),
            voided_entries: Vec::new(),
            sealed_range: None,
            balance_adjustments: Vec::new(),
            footer_lines: vec!["表示範囲: これは出てはいけない".to_owned()],
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: Some(std::borrow::Cow::Borrowed(i18n::ledger_empty_state())),
            action_rows: Vec::new(),
            ephemeral: true,
        })
        .expect("surface should render");

        assert_eq!(
            actual.body,
            "台帳\n\nまだ記録がありません。/expense または 記録する ボタンで最初の経費を記録してみましょう。"
        );
    }

    #[test]
    fn parent_review_route_uses_the_thread_handoff_instruction_and_cta_label() {
        let actual = DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel {
            kind: ReadViewKind::Review,
            route: ReadViewRoute::ReviewParent,
            title: std::borrow::Cow::Borrowed("清算確認"),
            uncertain_write: false,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::ThreadLink,
            recovery_url: Some("https://discord.com/channels/1/2/4".to_owned()),
            missing_thread_note: false,
            balances: vec![balance("Alice", "300", BalanceDirection::Receive)],
            transfers: vec![transfer("Bob", "Alice", "300")],
            participants: Vec::new(),
            voided_entries: Vec::new(),
            sealed_range: None,
            balance_adjustments: Vec::new(),
            footer_lines: Vec::new(),
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        })
        .expect("surface should render");

        assert!(
            actual
                .body
                .contains(i18n::review_parent_preview_instruction())
        );
        let SurfaceActionRow::Buttons(buttons) = &actual.action_rows[0] else {
            panic!("expected thread handoff button");
        };
        assert_eq!(buttons[0].label(), "台帳スレッドを開く");
    }

    #[test]
    fn stale_read_and_void_pages_use_fixed_reopen_copy_and_cta_contracts() {
        let stale_read = DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel {
            kind: ReadViewKind::Ledger,
            route: ReadViewRoute::LedgerPanel,
            title: std::borrow::Cow::Borrowed("台帳"),
            uncertain_write: false,
            stale_page: true,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: true,
            balances: Vec::new(),
            transfers: Vec::new(),
            participants: Vec::new(),
            voided_entries: Vec::new(),
            sealed_range: None,
            balance_adjustments: Vec::new(),
            footer_lines: Vec::new(),
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        })
        .expect("stale ledger view should render");
        let stale_void = DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::stale_page(
            "取り消し",
            RecoveryCta::ParentLink,
            Some("https://discord.com/channels/1/2/3".to_owned()),
            false,
            Vec::new(),
            true,
        ))
        .expect("stale void view should render");

        assert_eq!(
            stale_read.body,
            "台帳\n\nこの表示は期限切れです。/ledger または 台帳 で開き直してください。\n\n-# まだ台帳スレッドはありません。最初の記録後に全件確認できます。"
        );
        assert_eq!(
            stale_void.body,
            "取り消し\n\nこの表示は期限切れです。/void または 取り消し で開き直してください。"
        );
        let SurfaceActionRow::Buttons(buttons) = &stale_void.action_rows[0] else {
            panic!("expected parent reopen button");
        };
        assert_eq!(buttons[0].label(), "親チャンネルを開く");
    }

    #[test]
    fn ledger_view_can_hide_sections_for_snapshot_bound_paging() {
        let actual = DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel {
            kind: ReadViewKind::Ledger,
            route: ReadViewRoute::LedgerCommand,
            title: std::borrow::Cow::Borrowed("台帳"),
            uncertain_write: false,
            stale_page: false,
            page_indicator: Some(i18n::page_indicator(1, 2).to_string()),
            snapshot_notice: Some(i18n::snapshot_notice().to_owned()),
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            balances: vec![balance("Alice", "300", BalanceDirection::Receive)],
            transfers: Vec::new(),
            participants: vec![name("Alice"), name("Bob")],
            voided_entries: vec![VoidedEntryRow {
                void_entry_id: LedgerEntryId(9),
                voider_display_name: name("Alice"),
                voided_at: timestamp("2026-05-25 18:55"),
                original_summary: LedgerSurfaceSummary::Expense {
                    date: date("2026-05-24"),
                    payer_display_name: name("Bob"),
                    amount: "1200".to_owned(),
                    note: Some(note("ランチ")),
                },
                recovery_reference: recovery_reference(9),
            }],
            sealed_range: Some(SealedRangeSummary {
                through_entry_id: LedgerEntryId(4),
                through_summary: LedgerSurfaceSummary::Settlement {
                    date: date("2026-05-20"),
                    from_display_name: name("Alice"),
                    to_display_name: name("Bob"),
                    amount: "400".to_owned(),
                    additional_transfers: 1,
                },
            }),
            balance_adjustments: Vec::new(),
            footer_lines: vec!["表示範囲: 最新の検証済み台帳".to_owned()],
            visible_sections: ReadViewSectionVisibility {
                balances: false,
                transfers: false,
                participants: true,
                voided_entries: true,
                confirmed: false,
                route_guidance: false,
                footer: false,
            },
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        })
        .expect("paged ledger view should render");

        assert_eq!(
            actual.body,
            "台帳\n\nページ 1/2\n\n-# この表示は固定スナップショットです。更新するには開き直してください。\n\n参加者\nこれまで記録に出た人\nAlice\nBob\n\n取り消し済み\n[#9] Alice が 2026-05-25 18:55 に取り消し: 2026-05-24 Bob の支払い 1200円 メモ: ランチ | 復旧用の参照: ledger:abcd1234/entry:9 | <https://discord.com/channels/1/2/9>"
        );
    }

    #[test]
    fn panel_surface_has_no_body_text_and_uses_fixed_launcher_order() {
        let actual = DiscordLedgerPresenter::render_panel(&PanelSurfaceModel {
            thread_cue: String::new(),
            status_line: None,
            button_states: PanelButtonStates::default(),
            ephemeral: false,
        })
        .expect("surface should render");

        assert!(actual.body.is_empty());
        let SurfaceActionRow::Buttons(buttons) = &actual.action_rows[0] else {
            panic!("expected button row");
        };
        assert_eq!(
            buttons
                .iter()
                .map(|button| button.label())
                .collect::<Vec<_>>(),
            vec!["記録する", "清算確認", "台帳", "取り消し"]
        );
        assert_eq!(buttons[0].custom_id(), Some("ledger:panel:expense"));
        assert!(matches!(
            buttons[0],
            SurfaceButton::Interactive {
                style: SurfaceInteractiveButtonStyle::Primary,
                ..
            }
        ));
        assert_eq!(buttons[1].custom_id(), Some("ledger:panel:review"));
        assert_eq!(buttons[2].custom_id(), Some("ledger:panel:ledger"));
        assert_eq!(buttons[3].custom_id(), Some("ledger:panel:void"));
    }

    #[test]
    fn panel_and_recovery_locale_fields_fit_discord_component_limits() {
        for label in [
            i18n::panel_record_button_label(),
            i18n::panel_review_button_label(),
            i18n::panel_ledger_button_label(),
            i18n::panel_void_button_label(),
            i18n::open_parent_channel_label(),
            i18n::open_ledger_thread_label(),
        ] {
            validate_button_label(label).expect("button label should fit Discord limits");
        }

        validate_component_placeholder(i18n::expense_modal_note_placeholder())
            .expect("placeholder should fit Discord limits");
    }

    #[test]
    fn void_selection_surface_renders_candidate_summaries_and_recovery_refs() {
        let model = VoidSurfaceModel::selection(
            "取り消し",
            vec![
                VoidCandidateRow {
                    summary: LedgerSurfaceSummary::Expense {
                        date: date("2026-05-24"),
                        payer_display_name: name("Bob"),
                        amount: "1200".to_owned(),
                        note: Some(note("ランチの会計")),
                    },
                    recovery_reference: recovery_reference(7),
                },
                VoidCandidateRow {
                    summary: LedgerSurfaceSummary::Settlement {
                        date: date("2026-05-25"),
                        from_display_name: name("Alice"),
                        to_display_name: name("Bob"),
                        amount: "300".to_owned(),
                        additional_transfers: 2,
                    },
                    recovery_reference: recovery_reference(8),
                },
            ],
            Vec::new(),
            true,
        );
        let actual =
            DiscordLedgerPresenter::render_void_flow(&model).expect("surface should render");

        assert_eq!(
            actual.body,
            "取り消し\n\n2026-05-24 Bob の支払い 1200円 メモ: ランチの会計 | 復旧用の参照: ledger:abcd1234/entry:7 | <https://discord.com/channels/1/2/7>\n2026-05-25 清算 Alice->Bob 300円 ほか2件 | 復旧用の参照: ledger:abcd1234/entry:8 | <https://discord.com/channels/1/2/8>"
        );
        assert_eq!(model.primary_action_label.as_deref(), Some("確認へ"));
    }

    #[test]
    fn void_confirmation_surface_repeats_the_full_target_recap() {
        let model = VoidSurfaceModel::confirmation(
            "取り消し確認",
            VoidConfirmationRecap {
                summary: LedgerSurfaceSummary::Expense {
                    date: date("2026-05-24"),
                    payer_display_name: name("Bob"),
                    amount: "1200".to_owned(),
                    note: Some(note("ランチの会計")),
                },
                total_amount: "1200".to_owned(),
                recovery_reference: recovery_reference(7),
            },
            Vec::new(),
            true,
        );
        let actual =
            DiscordLedgerPresenter::render_void_flow(&model).expect("surface should render");

        assert_eq!(
            actual.body,
            "取り消し確認\n\n-# 元の記録は取り消し済みとして残ります。\n\n日付: 2026-05-24\n対象: Bob の支払い\n金額: 1200円\nメモ: ランチの会計\n復旧用の参照: ledger:abcd1234/entry:7 | <https://discord.com/channels/1/2/7>"
        );
        assert_eq!(model.primary_action_label.as_deref(), Some("取り消す"));
        assert_eq!(model.secondary_action_label.as_deref(), Some("選び直す"));
        assert_eq!(model.in_flight_copy.as_deref(), Some("取り消し中..."));
        assert_eq!(
            model.wrong_stage_copy.as_deref(),
            Some("取り消しボタンは確認画面から押してください。")
        );

        let settlement_recap = VoidConfirmationRecap {
            summary: LedgerSurfaceSummary::Settlement {
                date: date("2026-05-25"),
                from_display_name: name("Alice"),
                to_display_name: name("Bob"),
                amount: "300".to_owned(),
                additional_transfers: 2,
            },
            total_amount: "900".to_owned(),
            recovery_reference: recovery_reference(8),
        };
        let settlement_lines = super::render_void_confirmation_lines(&settlement_recap);
        assert_eq!(settlement_lines[1], "対象: 清算 Alice->Bob 300円 ほか2件");
    }

    #[test]
    fn void_confirmation_lines_use_ascii_ellipsis_for_note_excerpts() {
        let lines = super::render_void_confirmation_lines(&VoidConfirmationRecap {
            summary: LedgerSurfaceSummary::Expense {
                date: date("2026-05-24"),
                payer_display_name: name("Bob"),
                amount: "1200".to_owned(),
                note: Some(note("123456789012345678901234567890X")),
            },
            total_amount: "1200".to_owned(),
            recovery_reference: recovery_reference(7),
        });

        assert_eq!(lines[3], "メモ: 123456789012345678901234567890...");
    }

    #[test]
    fn void_candidate_lines_truncate_overlong_note_excerpts_with_ellipsis() {
        let actual = super::render_void_candidate_line(&VoidCandidateRow {
            summary: LedgerSurfaceSummary::Expense {
                date: date("2026-05-24"),
                payer_display_name: name("Bob"),
                amount: "1200".to_owned(),
                note: Some(note("123456789012345678901234567890X")),
            },
            recovery_reference: recovery_reference(7),
        });

        assert_eq!(
            actual,
            "2026-05-24 Bob の支払い 1200円 メモ: 123456789012345678901234567890... | 復旧用の参照: ledger:abcd1234/entry:7 | <https://discord.com/channels/1/2/7>"
        );
    }

    #[test]
    fn void_selection_surface_can_render_missing_selection_and_refreshed_candidates() {
        let actual =
            DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::missing_selection(
                "取り消し",
                vec![VoidCandidateRow {
                    summary: LedgerSurfaceSummary::Expense {
                        date: date("2026-05-24"),
                        payer_display_name: name("Bob"),
                        amount: "1200".to_owned(),
                        note: None,
                    },
                    recovery_reference: recovery_reference(7),
                }],
                Vec::new(),
                true,
            ))
            .expect("surface should render");

        assert!(
            actual
                .body
                .starts_with("取り消し\n\n対象を選択してください。")
        );
        assert!(actual.body.contains("2026-05-24 Bob の支払い 1200円"));
    }

    #[test]
    fn void_selection_surface_can_render_a_stale_target_reason() {
        let actual = DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::stale_target(
            "取り消し",
            VoidRetargetReason::EnteredSealedRange,
            vec![VoidCandidateRow {
                summary: LedgerSurfaceSummary::Settlement {
                    date: date("2026-05-25"),
                    from_display_name: name("Alice"),
                    to_display_name: name("Bob"),
                    amount: "300".to_owned(),
                    additional_transfers: 0,
                },
                recovery_reference: recovery_reference(8),
            }],
            Vec::new(),
            true,
        ))
        .expect("surface should render");

        assert!(actual.body.contains(
            "対象がもう取り消せません (確認済み範囲に入りました)。もう一度選んでください。"
        ));
    }

    #[test]
    fn void_success_surface_uses_the_fixed_followup_copy() {
        let actual = DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::success(
            "取り消し完了",
            "<#1234>",
            Vec::new(),
            true,
        ))
        .expect("surface should render");

        assert_eq!(
            actual.body,
            "取り消し完了\n\n取り消しました。\nこの取り消しで元の記録の影響は打ち消されます。正しい内容が必要なら記録し直してください。\n台帳スレッドで確認できます: <#1234>\n修正して再記録する場合は /expense または 記録する を使ってください。"
        );
    }

    #[test]
    fn void_success_surface_omits_dead_thread_mentions_and_uses_parent_cta() {
        let actual =
            DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::success_with_recovery(
                "取り消し完了",
                None,
                RecoveryCta::ParentLink,
                Some("https://discord.com/channels/1/2".to_owned()),
                Vec::new(),
                true,
            ))
            .expect("surface should render");

        assert!(actual.body.contains("台帳スレッドで確認できます。"));
        assert!(!actual.body.contains("<#"));
        let SurfaceActionRow::Buttons(buttons) = &actual.action_rows[0] else {
            panic!("expected parent-channel button");
        };
        assert_eq!(buttons[0].label(), "親チャンネルを開く");
    }

    #[test]
    fn void_terminal_surfaces_cover_empty_and_non_candidate_outcomes() {
        let empty = DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::empty(
            "取り消し",
            Vec::new(),
            true,
        ))
        .expect("surface should render");
        let no_candidates = DiscordLedgerPresenter::render_void_flow(
            &VoidSurfaceModel::no_candidates("取り消し", Vec::new(), true),
        )
        .expect("surface should render");

        assert_eq!(
            empty.body,
            "取り消し\n\nまだ記録がありません。/expense または 記録する ボタンで最初の経費を記録してみましょう。"
        );
        assert_eq!(
            no_candidates.body,
            "取り消し\n\n取り消せる対象がありません。確認済みや既に取り消した記録は対象外です。/ledger または 台帳 で状態を確認してください。"
        );
    }

    #[test]
    fn void_operator_handoff_surface_covers_the_window_limit() {
        let actual =
            DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::older_than_window(
                "取り消し",
                "復旧用の参照: ledger:abcd1234/entry:7 | <https://discord.com/channels/1/2/7>",
                Vec::new(),
                true,
            ))
            .expect("surface should render");

        assert_eq!(
            actual.body,
            "取り消し\n\n対象がありません (このUIで選べるのは新しい25件までです。古い記録はこのUIから取り消せません。運用担当者に連絡してください)。\n連絡時は対象の公開台帳メッセージのリンク、または控えている 復旧用の参照: ledger:abcd1234/entry:7 | <https://discord.com/channels/1/2/7> と、わかる範囲の日時・支払者・金額・メモ抜粋を伝えてください。"
        );
    }

    #[test]
    fn uncertain_write_layout_keeps_status_lines_and_recovery_reference_together() {
        let actual =
            DiscordLedgerPresenter::render_uncertain_write_block(&UncertainWriteSurfaceModel {
                title: "書き込み確認中".to_owned(),
                status_lines: vec!["前回の書き込み結果を確認中です。".to_owned()],
                recovery_reference: Some("復旧用の参照: ledger:abcd1234/entry:7".to_owned()),
                action_rows: vec![SurfaceActionRow::Buttons(vec![SurfaceButton::Link {
                    label: "台帳スレッドを開く".to_owned(),
                    url: "https://discord.com/channels/1/2/3".to_owned(),
                    disabled: false,
                }])],
                ephemeral: true,
            })
            .expect("surface should render");

        assert_eq!(
            actual.body,
            "書き込み確認中\n\n前回の書き込み結果を確認中です。\n\n復旧用の参照: ledger:abcd1234/entry:7"
        );
        assert_eq!(actual.action_rows.len(), 1);
    }

    #[test]
    fn canonical_message_accepts_a_single_recovery_reference_line() {
        let actual = RenderedCanonicalMessage::new(
            "支出 [#7]\n日付: 2026-05-25\n記録者: Alice\n記録日時: 2026-05-25 18:55\n復旧用の参照: ledger:abcd1234/entry:7",
        );

        assert!(actual.is_ok());
    }

    #[test]
    fn canonical_message_accepts_the_standard_public_truncation_cue() {
        let actual = RenderedCanonicalMessage::new(format!(
            "清算 [#8]\n日付: 2026-05-25\n記録日時: 2026-05-25 18:55\n{}\n{}\n復旧用の参照: ledger:abcd1234/entry:8 | <https://discord.com/channels/1/2/8>",
            i18n::PUBLIC_TRUNCATION_OMITTED,
            i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE
        ));

        assert!(actual.is_ok());
    }

    #[test]
    fn canonical_message_requires_one_recovery_reference_line() {
        let actual = RenderedCanonicalMessage::new("支出 [#7]\n日付: 2026-05-25");

        assert_eq!(actual, Err(RenderBudgetError::MissingRecoveryReference));
    }

    #[test]
    fn canonical_message_rejects_multiple_recovery_reference_lines() {
        let actual = RenderedCanonicalMessage::new(
            "支出 [#7]\n復旧用の参照: ledger:abcd1234/entry:7\n復旧用の参照: ledger:abcd1234/entry:7 | <https://discord.com/channels/1/2/7>",
        );

        assert_eq!(
            actual,
            Err(RenderBudgetError::MultipleRecoveryReferences { actual: 2 })
        );
    }

    #[test]
    fn canonical_message_rejects_partial_truncation_cue() {
        let actual = RenderedCanonicalMessage::new(format!(
            "清算 [#8]\n{}\n復旧用の参照: ledger:abcd1234/entry:8",
            i18n::PUBLIC_TRUNCATION_OMITTED
        ));

        assert_eq!(actual, Err(RenderBudgetError::InvalidPublicTruncationCue));
    }

    #[test]
    fn canonical_message_requires_the_recovery_reference_to_be_the_final_line() {
        let actual = RenderedCanonicalMessage::new(
            "支出 [#7]\n復旧用の参照: ledger:abcd1234/entry:7\n記録日時: 2026-05-25 18:55",
        );

        assert_eq!(actual, Err(RenderBudgetError::MissingRecoveryReference));
    }

    #[test]
    fn canonical_message_requires_the_truncation_cue_to_stay_in_the_final_tail() {
        let actual = RenderedCanonicalMessage::new(format!(
            "清算 [#8]\n{}\n記録日時: 2026-05-25 18:55\n{}\n復旧用の参照: ledger:abcd1234/entry:8 | <https://discord.com/channels/1/2/8>",
            i18n::PUBLIC_TRUNCATION_OMITTED,
            i18n::PUBLIC_TRUNCATION_ATTACHMENT_GUIDANCE
        ));

        assert_eq!(actual, Err(RenderBudgetError::InvalidPublicTruncationCue));
    }

    fn note(value: &str) -> SafeLiteralText {
        SafeLiteralText::from_note(value).expect("note should sanitize")
    }

    fn name(value: &str) -> SafeLiteralText {
        SafeLiteralText::from_roster_label(value).expect("name should sanitize")
    }

    fn date(value: &str) -> LedgerEffectiveDate {
        LedgerEffectiveDate::new(value).expect("date should stay valid")
    }

    fn timestamp(value: &str) -> BusinessDateTime {
        BusinessDateTime::parse(value).expect("timestamp should stay valid")
    }

    fn recovery_reference(entry_id: u64) -> RecoveryReference {
        RecoveryReference {
            ledger_id_short: "abcd1234".to_owned(),
            entry_id: LedgerEntryId(entry_id),
            message_link: Some(format!("https://discord.com/channels/1/2/{entry_id}")),
        }
    }

    fn participant_share(name: &str, amount: &str) -> super::ParticipantShareRow {
        super::ParticipantShareRow {
            display_name: self::name(name),
            share_amount: amount.to_owned(),
        }
    }

    fn transfer(from: &str, to: &str, amount: &str) -> TransferRow {
        TransferRow {
            from_display_name: self::name(from),
            to_display_name: self::name(to),
            amount: amount.to_owned(),
        }
    }

    fn balance(name: &str, amount: &str, direction: BalanceDirection) -> BalanceRow {
        BalanceRow {
            display_name: self::name(name),
            amount: amount.to_owned(),
            direction,
        }
    }

    fn impact(name: &str, amount: &str, direction: BalanceDirection) -> BalanceImpactRow {
        BalanceImpactRow {
            display_name: self::name(name),
            amount: amount.to_owned(),
            direction,
        }
    }
}
