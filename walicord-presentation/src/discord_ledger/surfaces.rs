use super::{
    budgets::{
        DISCORD_MESSAGE_CONTENT_LIMIT, RenderBudgetError, validate_action_rows,
        validate_button_label, validate_buttons_in_row, validate_component_description,
        validate_component_label, validate_component_placeholder, validate_custom_id,
        validate_message_content, validate_select_options, validate_select_value_bounds,
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

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum DiscordLinkUrlError {
    #[error("invalid Discord link URL")]
    Invalid,
    #[error("unsupported Discord link URL scheme: {0}")]
    UnsupportedScheme(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiscordLinkUrl(String);

impl DiscordLinkUrl {
    pub fn parse(raw: impl Into<String>) -> Result<Self, DiscordLinkUrlError> {
        let raw = raw.into();
        let parsed = url::Url::parse(&raw).map_err(|_| DiscordLinkUrlError::Invalid)?;
        match parsed.scheme() {
            "http" | "https" => Ok(Self(raw)),
            scheme => Err(DiscordLinkUrlError::UnsupportedScheme(scheme.to_owned())),
        }
    }

    pub fn discord_channel(guild_id: u64, channel_id: u64) -> Self {
        Self(format!(
            "https://discord.com/channels/{guild_id}/{channel_id}"
        ))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SurfaceButton {
    Link {
        label: String,
        url: DiscordLinkUrl,
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
            Self::Link { url, .. } => Some(url.as_str()),
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
pub enum ExpenseOrSettlementSummary {
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
}

impl ExpenseOrSettlementSummary {
    pub fn date(&self) -> &LedgerEffectiveDate {
        match self {
            Self::Expense { date, .. } | Self::Settlement { date, .. } => date,
        }
    }

    pub fn render_candidate_summary(&self) -> String {
        format!("{} {}", self.date(), self.render_recent_entry_summary())
    }

    pub fn render_recent_entry_summary(&self) -> String {
        match self {
            Self::Expense {
                payer_display_name,
                amount,
                note,
                ..
            } => {
                let mut summary =
                    i18n::sealed_expense_summary(payer_display_name, amount).to_string();
                if let Some(note) = note {
                    summary.push(' ');
                    summary.push_str(
                        &i18n::void_candidate_note_excerpt(append_ascii_ellipsis_after_prefix(
                            note.as_str(),
                            30,
                        ))
                        .to_string(),
                    );
                }
                summary
            }
            Self::Settlement {
                from_display_name,
                to_display_name,
                amount,
                additional_transfers,
                ..
            } => {
                let mut summary = i18n::sealed_settlement_summary_with_amount(
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
        }
    }

    pub fn render_confirmation_target(&self) -> String {
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
        }
    }

    pub fn confirmation_note_excerpt(&self) -> Option<String> {
        match self {
            Self::Expense { note, .. } => note
                .as_ref()
                .map(|note| append_ascii_ellipsis_after_prefix(note.as_str(), 30)),
            Self::Settlement { .. } => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerSurfaceSummary {
    ExpenseOrSettlement(ExpenseOrSettlementSummary),
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
            Self::ExpenseOrSettlement(inner) => inner.date(),
            Self::Void { date, .. }
            | Self::Sealed { date, .. }
            | Self::BalanceAdjustment { date, .. } => date,
        }
    }

    fn render_sealed_summary(&self) -> String {
        match self {
            Self::ExpenseOrSettlement(inner) => inner.render_sealed_summary(),
            Self::Void { .. } => i18n::SEALED_VOID_SUMMARY.to_owned(),
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
            self.entry_id
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
    pub original_summary: ExpenseOrSettlementSummary,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LedgerRoute {
    Command,
    Panel,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReviewRoute {
    Thread,
    Parent,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum ReviewSettleAction {
    Enabled {
        custom_id: String,
    },
    #[default]
    Hidden,
}

impl ReviewSettleAction {
    pub fn enabled(custom_id: impl Into<String>) -> Self {
        Self::Enabled {
            custom_id: custom_id.into(),
        }
    }

    fn into_action_rows(self) -> Vec<SurfaceActionRow> {
        match self {
            Self::Enabled { custom_id } => {
                vec![SurfaceActionRow::Buttons(vec![
                    SurfaceButton::Interactive {
                        label: i18n::REVIEW_SETTLE_BUTTON_LABEL.to_owned(),
                        custom_id,
                        style: SurfaceInteractiveButtonStyle::Primary,
                        disabled: false,
                    },
                ])]
            }
            Self::Hidden => Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentRoute {
    Ledger(LedgerRoute),
    Review(ReviewRoute),
}

impl From<LedgerRoute> for DocumentRoute {
    fn from(r: LedgerRoute) -> Self {
        Self::Ledger(r)
    }
}

impl From<ReviewRoute> for DocumentRoute {
    fn from(r: ReviewRoute) -> Self {
        Self::Review(r)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecentEntryRow {
    pub entry_id: LedgerEntryId,
    pub summary: ExpenseOrSettlementSummary,
    pub recovery_reference: RecoveryReference,
}

// --- Builder types (pre-pagination) ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Section<T> {
    Empty,
    Rows(walicord_domain::NonEmptyVec<T>),
}

impl<T> Section<T> {
    pub fn from_rows(rows: Vec<T>) -> Self {
        match walicord_domain::NonEmptyVec::new(rows) {
            Ok(rows) => Self::Rows(rows),
            Err(_) => Self::Empty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReadViewData {
    Ledger {
        balances: Section<BalanceRow>,
        recent_entries: Section<RecentEntryRow>,
    },
    Review {
        balances: Section<BalanceRow>,
        // Populated Review always has transfers; no-transfer state is ReadViewDocumentState::Empty
        transfers: walicord_domain::NonEmptyVec<TransferRow>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReadViewDocumentState {
    Empty {
        empty_state: std::borrow::Cow<'static, str>,
    },
    Populated(ReadViewData),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadViewDocument {
    route: DocumentRoute,
    title: std::borrow::Cow<'static, str>,
    uncertain_write: bool,
    route_guidance_lines: Vec<String>,
    recovery_action: RecoveryAction,
    footer_lines: Vec<String>,
    action_rows: Vec<SurfaceActionRow>,
    ephemeral: bool,
    state: ReadViewDocumentState,
}

impl ReadViewDocument {
    pub fn route(&self) -> DocumentRoute {
        self.route
    }
    pub fn title(&self) -> &str {
        &self.title
    }
    pub fn uncertain_write(&self) -> bool {
        self.uncertain_write
    }
    pub fn route_guidance_lines(&self) -> &[String] {
        &self.route_guidance_lines
    }
    pub fn recovery_action(&self) -> &RecoveryAction {
        &self.recovery_action
    }
    pub fn footer_lines(&self) -> &[String] {
        &self.footer_lines
    }
    pub fn action_rows(&self) -> &[SurfaceActionRow] {
        &self.action_rows
    }
    pub fn ephemeral(&self) -> bool {
        self.ephemeral
    }
    pub fn state(&self) -> &ReadViewDocumentState {
        &self.state
    }
    pub fn into_state(self) -> ReadViewDocumentState {
        self.state
    }

    pub fn with_route_guidance(mut self, lines: Vec<String>) -> Self {
        self.route_guidance_lines = lines;
        self
    }

    pub fn ledger(
        route: LedgerRoute,
        balances: Section<BalanceRow>,
        recent_entries: Section<RecentEntryRow>,
        uncertain_write: bool,
    ) -> Self {
        let state = if matches!(
            (&balances, &recent_entries),
            (Section::Empty, Section::Empty)
        ) {
            ReadViewDocumentState::Empty {
                empty_state: std::borrow::Cow::Borrowed(i18n::LEDGER_EMPTY_STATE),
            }
        } else {
            ReadViewDocumentState::Populated(ReadViewData::Ledger {
                balances,
                recent_entries,
            })
        };
        Self {
            route: route.into(),
            title: std::borrow::Cow::Borrowed(i18n::PANEL_LEDGER_BUTTON_LABEL),
            uncertain_write,
            route_guidance_lines: Vec::new(),
            recovery_action: RecoveryAction::None,
            footer_lines: Vec::new(),
            action_rows: Vec::new(),
            ephemeral: true,
            state,
        }
    }

    pub fn ledger_empty(route: LedgerRoute, uncertain_write: bool) -> Self {
        Self {
            route: route.into(),
            title: std::borrow::Cow::Borrowed(i18n::PANEL_LEDGER_BUTTON_LABEL),
            uncertain_write,
            route_guidance_lines: Vec::new(),
            recovery_action: RecoveryAction::None,
            footer_lines: Vec::new(),
            action_rows: Vec::new(),
            ephemeral: true,
            state: ReadViewDocumentState::Empty {
                empty_state: std::borrow::Cow::Borrowed(i18n::LEDGER_EMPTY_STATE),
            },
        }
    }

    pub fn review(
        route: ReviewRoute,
        balances: Section<BalanceRow>,
        transfers: walicord_domain::NonEmptyVec<TransferRow>,
        uncertain_write: bool,
        recovery_action: RecoveryAction,
        settle_action: ReviewSettleAction,
    ) -> Self {
        Self {
            route: route.into(),
            title: std::borrow::Cow::Borrowed(i18n::PANEL_REVIEW_BUTTON_LABEL),
            uncertain_write,
            route_guidance_lines: Vec::new(),
            recovery_action,
            footer_lines: Vec::new(),
            action_rows: settle_action.into_action_rows(),
            ephemeral: true,
            state: ReadViewDocumentState::Populated(ReadViewData::Review {
                balances,
                transfers,
            }),
        }
    }

    pub fn review_empty(
        route: ReviewRoute,
        uncertain_write: bool,
        recovery_action: RecoveryAction,
    ) -> Self {
        let (empty_state, recovery_action) = match route {
            ReviewRoute::Parent => (
                std::borrow::Cow::Borrowed(i18n::REVIEW_PARENT_EMPTY_STATE),
                RecoveryAction::None,
            ),
            ReviewRoute::Thread => (
                std::borrow::Cow::Borrowed(i18n::REVIEW_THREAD_EMPTY_STATE),
                recovery_action,
            ),
        };
        Self {
            route: route.into(),
            title: std::borrow::Cow::Borrowed(i18n::PANEL_REVIEW_BUTTON_LABEL),
            uncertain_write,
            route_guidance_lines: Vec::new(),
            recovery_action,
            footer_lines: Vec::new(),
            action_rows: Vec::new(),
            ephemeral: true,
            state: ReadViewDocumentState::Empty { empty_state },
        }
    }

    pub fn review_no_transfers(
        route: ReviewRoute,
        uncertain_write: bool,
        recovery_action: RecoveryAction,
    ) -> Self {
        use std::fmt::Write as _;
        let mut body = String::new();
        let _ = write!(
            body,
            "{}\n{}",
            i18n::SETTLEMENT_ALREADY_NOT_NEEDED_MESSAGE,
            i18n::SETTLEMENT_PREVIEW_NOT_SAVED_MESSAGE,
        );
        Self {
            route: route.into(),
            title: std::borrow::Cow::Borrowed(i18n::PANEL_REVIEW_BUTTON_LABEL),
            uncertain_write,
            route_guidance_lines: Vec::new(),
            recovery_action,
            footer_lines: Vec::new(),
            action_rows: Vec::new(),
            ephemeral: true,
            state: ReadViewDocumentState::Empty {
                empty_state: std::borrow::Cow::Owned(body),
            },
        }
    }
}

// --- Page types (post-pagination) ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PageSection<T> {
    Hidden,
    Empty,
    Rows(walicord_domain::NonEmptyVec<T>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RequiredPageSection<T> {
    Hidden,
    Rows(walicord_domain::NonEmptyVec<T>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReadViewPageData {
    Ledger {
        balances: PageSection<BalanceRow>,
        recent_entries: PageSection<RecentEntryRow>,
    },
    Review {
        balances: PageSection<BalanceRow>,
        transfers: RequiredPageSection<TransferRow>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReadViewPageState {
    Stale {
        missing_thread_note: bool,
    },
    Empty {
        empty_state: std::borrow::Cow<'static, str>,
    },
    Populated(ReadViewPageData),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadViewPageModel {
    route: DocumentRoute,
    title: std::borrow::Cow<'static, str>,
    uncertain_write: bool,
    page_indicator: Option<String>,
    snapshot_notice: Option<String>,
    route_guidance_lines: Vec<String>,
    recovery_action: RecoveryAction,
    footer_lines: Vec<String>,
    action_rows: Vec<SurfaceActionRow>,
    ephemeral: bool,
    state: ReadViewPageState,
}

impl ReadViewPageModel {
    fn from_doc(
        doc: &ReadViewDocument,
        state: ReadViewPageState,
        page_indicator: Option<String>,
        snapshot_notice: Option<String>,
    ) -> Self {
        Self {
            route: doc.route,
            title: doc.title.clone(),
            uncertain_write: doc.uncertain_write,
            page_indicator,
            snapshot_notice,
            route_guidance_lines: doc.route_guidance_lines.clone(),
            recovery_action: doc.recovery_action.clone(),
            footer_lines: doc.footer_lines.clone(),
            action_rows: doc.action_rows.clone(),
            ephemeral: doc.ephemeral,
            state,
        }
    }

    pub fn stale_ledger(route: LedgerRoute, missing_thread_note: bool) -> Self {
        Self {
            route: route.into(),
            title: std::borrow::Cow::Borrowed(i18n::PANEL_LEDGER_BUTTON_LABEL),
            uncertain_write: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_action: RecoveryAction::None,
            footer_lines: Vec::new(),
            action_rows: Vec::new(),
            ephemeral: true,
            state: ReadViewPageState::Stale {
                missing_thread_note,
            },
        }
    }

    pub(crate) fn empty(
        doc: &ReadViewDocument,
        empty_state: std::borrow::Cow<'static, str>,
    ) -> Self {
        Self::from_doc(doc, ReadViewPageState::Empty { empty_state }, None, None)
    }

    pub(crate) fn ledger_page(
        doc: &ReadViewDocument,
        balances: PageSection<BalanceRow>,
        recent_entries: PageSection<RecentEntryRow>,
        page_indicator: Option<String>,
        snapshot_notice: Option<String>,
    ) -> Self {
        Self::from_doc(
            doc,
            ReadViewPageState::Populated(ReadViewPageData::Ledger {
                balances,
                recent_entries,
            }),
            page_indicator,
            snapshot_notice,
        )
    }

    pub(crate) fn review_page(
        doc: &ReadViewDocument,
        balances: PageSection<BalanceRow>,
        transfers: RequiredPageSection<TransferRow>,
        page_indicator: Option<String>,
        snapshot_notice: Option<String>,
    ) -> Self {
        Self::from_doc(
            doc,
            ReadViewPageState::Populated(ReadViewPageData::Review {
                balances,
                transfers,
            }),
            page_indicator,
            snapshot_notice,
        )
    }

    pub fn route(&self) -> DocumentRoute {
        self.route
    }
    pub fn title(&self) -> &str {
        &self.title
    }
    pub fn uncertain_write(&self) -> bool {
        self.uncertain_write
    }
    pub fn page_indicator(&self) -> Option<&str> {
        self.page_indicator.as_deref()
    }
    pub fn snapshot_notice(&self) -> Option<&str> {
        self.snapshot_notice.as_deref()
    }
    pub fn route_guidance_lines(&self) -> &[String] {
        &self.route_guidance_lines
    }
    pub fn recovery_action(&self) -> &RecoveryAction {
        &self.recovery_action
    }
    pub fn footer_lines(&self) -> &[String] {
        &self.footer_lines
    }
    pub fn action_rows(&self) -> &[SurfaceActionRow] {
        &self.action_rows
    }
    pub fn ephemeral(&self) -> bool {
        self.ephemeral
    }
    pub fn state(&self) -> &ReadViewPageState {
        &self.state
    }

    pub fn with_route_guidance(mut self, lines: Vec<String>) -> Self {
        self.route_guidance_lines = lines;
        self
    }

    pub fn clear_action_rows(&mut self) {
        self.action_rows.clear();
    }

    #[cfg(test)]
    pub(crate) fn test_fixture(
        route: DocumentRoute,
        title: &'static str,
        uncertain_write: bool,
        recovery_action: RecoveryAction,
        footer_lines: Vec<String>,
        state: ReadViewPageState,
    ) -> Self {
        Self {
            route,
            title: std::borrow::Cow::Borrowed(title),
            uncertain_write,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_action,
            footer_lines,
            action_rows: Vec::new(),
            ephemeral: true,
            state,
        }
    }

    #[cfg(test)]
    pub(crate) fn test_fixture_paged(
        route: DocumentRoute,
        title: &'static str,
        page_indicator: Option<String>,
        snapshot_notice: Option<String>,
        state: ReadViewPageState,
    ) -> Self {
        Self {
            route,
            title: std::borrow::Cow::Borrowed(title),
            uncertain_write: false,
            page_indicator,
            snapshot_notice,
            route_guidance_lines: Vec::new(),
            recovery_action: RecoveryAction::None,
            footer_lines: Vec::new(),
            action_rows: Vec::new(),
            ephemeral: true,
            state,
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CanonicalMessageKind {
    Full,
    Truncated,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderedCanonicalMessage {
    body: String,
}

const PUBLIC_COMPACT_SUMMARY_LIMIT: usize = 140;
const PUBLIC_COMPACT_REASON_LIMIT: usize = 48;
const PUBLIC_COMPACT_IMPACT_LIMIT: usize = 96;

impl RenderedCanonicalMessage {
    pub(crate) fn new(
        content_lines: &[String],
        recovery_reference: &RecoveryReference,
        kind: CanonicalMessageKind,
    ) -> Result<Self, RenderBudgetError> {
        use std::fmt::Write;
        let mut body = content_lines.join("\n");
        if kind == CanonicalMessageKind::Truncated {
            write!(
                body,
                "\n{}\n{}",
                i18n::PUBLIC_TRUNCATION_OMITTED,
                i18n::PUBLIC_TRUNCATION_LEDGER_GUIDANCE
            )
            .unwrap();
        }
        write!(body, "\n{}", recovery_reference.render_line()).unwrap();
        validate_message_content(&body)?;
        Ok(Self { body })
    }

    pub fn body(&self) -> &str {
        &self.body
    }
}

// Discord-specific presentation type: enforces Discord's 2000-char content limit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageContent(String);

impl MessageContent {
    pub fn parse(raw: impl Into<String>) -> Result<Self, RenderBudgetError> {
        let s = raw.into();
        validate_message_content(&s)?;
        Ok(Self(s))
    }

    pub fn fit_to_discord_limit(raw: impl Into<String>) -> Self {
        let s = raw.into();
        Self(fit_with_ascii_ellipsis(&s, DISCORD_MESSAGE_CONTENT_LIMIT))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn into_builder(self) -> MessageContentBuilder {
        MessageContentBuilder { raw: self.0 }
    }

    pub fn into_string(self) -> String {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageContentBuilder {
    raw: String,
}

impl MessageContentBuilder {
    pub fn push_line(&mut self, line: &str) {
        if !self.raw.is_empty() {
            self.raw.push('\n');
        }
        self.raw.push_str(line);
    }

    pub fn build(self) -> Result<MessageContent, RenderBudgetError> {
        MessageContent::parse(self.raw)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PresentationSurfaceBody {
    Text(MessageContent),
    ImageBacked {
        compact_text: MessageContent,
        fallback_text: MessageContent,
        image: crate::svg_table::RenderedSvg,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextRenderedSurface {
    body: MessageContent,
    action_rows: Vec<SurfaceActionRow>,
    ephemeral: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextRenderedSurfaceParts {
    pub body: MessageContent,
    pub action_rows: Vec<SurfaceActionRow>,
}

impl TextRenderedSurface {
    pub fn new(
        body: impl Into<String>,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Result<Self, RenderBudgetError> {
        let content = MessageContent::parse(body)?;
        let action_rows = normalize_action_rows(action_rows)?;
        Ok(Self {
            body: content,
            action_rows,
            ephemeral,
        })
    }

    pub fn text_body(&self) -> &str {
        self.body.as_str()
    }

    pub fn action_rows(&self) -> &[SurfaceActionRow] {
        &self.action_rows
    }

    pub fn into_text_message_parts(self) -> TextRenderedSurfaceParts {
        TextRenderedSurfaceParts {
            body: self.body,
            action_rows: self.action_rows,
        }
    }
}

impl From<TextRenderedSurface> for RenderedSurface {
    fn from(text: TextRenderedSurface) -> Self {
        Self {
            body: PresentationSurfaceBody::Text(text.body),
            action_rows: text.action_rows,
            ephemeral: text.ephemeral,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderedSurface {
    body: PresentationSurfaceBody,
    action_rows: Vec<SurfaceActionRow>,
    ephemeral: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderedSurfaceParts {
    pub body: PresentationSurfaceBody,
    pub action_rows: Vec<SurfaceActionRow>,
    pub ephemeral: bool,
}

impl RenderedSurface {
    fn new_text(
        body: impl Into<String>,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Result<Self, RenderBudgetError> {
        Ok(TextRenderedSurface::new(body, action_rows, ephemeral)?.into())
    }

    pub fn new_image_backed(
        compact_text: impl Into<String>,
        fallback_text: impl Into<String>,
        image: crate::svg_table::RenderedSvg,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Result<Self, RenderBudgetError> {
        let compact = MessageContent::parse(compact_text)?;
        let fallback_raw: String = fallback_text.into();
        let fallback = MessageContent::fit_to_discord_limit(fallback_raw);

        let action_rows = normalize_action_rows(action_rows)?;
        Ok(Self {
            body: PresentationSurfaceBody::ImageBacked {
                compact_text: compact,
                fallback_text: fallback,
                image,
            },
            action_rows,
            ephemeral,
        })
    }

    pub fn body(&self) -> &PresentationSurfaceBody {
        &self.body
    }

    pub fn into_parts(self) -> RenderedSurfaceParts {
        RenderedSurfaceParts {
            body: self.body,
            action_rows: self.action_rows,
            ephemeral: self.ephemeral,
        }
    }

    pub fn action_rows(&self) -> &[SurfaceActionRow] {
        &self.action_rows
    }

    pub fn ephemeral(&self) -> bool {
        self.ephemeral
    }

    pub fn text_body(&self) -> &str {
        match &self.body {
            PresentationSurfaceBody::Text(content) => content.as_str(),
            PresentationSurfaceBody::ImageBacked { fallback_text, .. } => fallback_text.as_str(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum RecoveryAction {
    ThreadLink {
        url: DiscordLinkUrl,
    },
    ParentLink {
        url: DiscordLinkUrl,
    },
    RecoveryReferenceOnly,
    #[default]
    None,
}

impl RecoveryAction {
    pub fn thread_link(url: DiscordLinkUrl) -> Self {
        Self::ThreadLink { url }
    }

    pub fn parent_link(url: DiscordLinkUrl) -> Self {
        Self::ParentLink { url }
    }
}

pub struct DiscordLedgerPresenter;

impl DiscordLedgerPresenter {
    pub fn render_expense_step(
        model: &ExpenseSurfaceModel,
    ) -> Result<TextRenderedSurface, RenderBudgetError> {
        TextRenderedSurface::new(
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
    ) -> Result<TextRenderedSurface, RenderBudgetError> {
        TextRenderedSurface::new(
            render_sections([non_empty_join(&model.body_lines).as_deref()]),
            model.action_rows.clone(),
            model.ephemeral,
        )
    }

    pub fn render_panel(
        model: &PanelSurfaceModel,
    ) -> Result<TextRenderedSurface, RenderBudgetError> {
        TextRenderedSurface::new(
            render_sections([
                Some(model.thread_cue.as_str()),
                model.status_line.as_deref(),
            ]),
            vec![SurfaceActionRow::Buttons(vec![
                SurfaceButton::Interactive {
                    label: i18n::PANEL_RECORD_BUTTON_LABEL.to_owned(),
                    custom_id: "ledger:panel:expense".to_owned(),
                    style: SurfaceInteractiveButtonStyle::Primary,
                    disabled: model.button_states.record_disabled,
                },
                SurfaceButton::Interactive {
                    label: i18n::PANEL_REVIEW_BUTTON_LABEL.to_owned(),
                    custom_id: "ledger:panel:review".to_owned(),
                    style: SurfaceInteractiveButtonStyle::Secondary,
                    disabled: model.button_states.review_disabled,
                },
                SurfaceButton::Interactive {
                    label: i18n::PANEL_LEDGER_BUTTON_LABEL.to_owned(),
                    custom_id: "ledger:panel:ledger".to_owned(),
                    style: SurfaceInteractiveButtonStyle::Secondary,
                    disabled: model.button_states.ledger_disabled,
                },
                SurfaceButton::Interactive {
                    label: i18n::PANEL_VOID_BUTTON_LABEL.to_owned(),
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
    ) -> Result<TextRenderedSurface, RenderBudgetError> {
        let mut action_rows = Vec::new();
        if let Some(select_menu) = model.select_menu.clone() {
            action_rows.push(SurfaceActionRow::Select(select_menu));
        }
        action_rows.extend(model.action_rows.clone());

        TextRenderedSurface::new(
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
                &render_public_expense_full(model),
                &render_public_expense_compact(model),
                &model.recovery_reference,
            ),
            PublicCanonicalMessageModel::Settlement(model) => render_compacted_canonical_message(
                &render_public_settlement_full(model),
                &render_public_settlement_compact(model),
                &model.recovery_reference,
            ),
            PublicCanonicalMessageModel::Void(model) => render_compacted_canonical_message(
                &render_public_void_full(model),
                &render_public_void_compact(model),
                &model.recovery_reference,
            ),
            PublicCanonicalMessageModel::Seal(model) => render_compacted_canonical_message(
                &render_public_seal_full(model),
                &render_public_seal_compact(model),
                &model.recovery_reference,
            ),
            PublicCanonicalMessageModel::BalanceAdjustment(model) => {
                render_compacted_canonical_message(
                    &render_public_balance_adjustment_full(model),
                    &render_public_balance_adjustment_compact(model),
                    &model.recovery_reference,
                )
            }
        }
    }

    pub fn render_read_view_page(
        model: &ReadViewPageModel,
    ) -> Result<RenderedSurface, RenderBudgetError> {
        let advisory = model
            .uncertain_write
            .then(|| i18n::READ_UNCERTAIN_WRITE_ADVISORY.to_owned());
        let mut action_rows = model.action_rows.clone();
        append_recovery_cta_row(&mut action_rows, &model.recovery_action);

        let body = match &model.state {
            ReadViewPageState::Stale {
                missing_thread_note,
            } => {
                let stale_message = match model.route {
                    DocumentRoute::Review(_) => i18n::STALE_REVIEW_PAGE_MESSAGE,
                    DocumentRoute::Ledger(_) => i18n::STALE_LEDGER_PAGE_MESSAGE,
                };
                render_sections([
                    Some(model.title.as_ref()),
                    Some(stale_message),
                    missing_thread_note.then_some(i18n::NO_LEDGER_THREAD_YET_NOTE),
                ])
            }
            ReadViewPageState::Empty { empty_state } => render_sections([
                Some(model.title.as_ref()),
                advisory.as_deref(),
                Some(empty_state.as_ref()),
            ]),
            ReadViewPageState::Populated(data) => {
                use crate::svg_table::combine_svgs_vertically;
                use walicord_domain::NonEmptyVec;

                let review_instruction = match data {
                    ReadViewPageData::Review { .. } => Some(if model.uncertain_write {
                        i18n::REVIEW_PREVIEW_BLOCKED_INSTRUCTION
                    } else {
                        i18n::REVIEW_PREVIEW_INSTRUCTION
                    }),
                    ReadViewPageData::Ledger { .. } => None,
                };
                let route_guidance = non_empty_join(
                    &model
                        .route_guidance_lines
                        .iter()
                        .map(String::as_str)
                        .collect::<Vec<_>>(),
                );
                let footer = non_empty_join(model.footer_lines());

                let fallback_text = match data {
                    ReadViewPageData::Review {
                        balances,
                        transfers,
                    } => {
                        let balances_section =
                            render_page_section(balances, render_review_balances_section);
                        let transfers_section = render_required_page_section(
                            transfers,
                            render_review_transfers_section,
                        );
                        render_sections([
                            Some(model.title.as_ref()),
                            advisory.as_deref(),
                            model.page_indicator.as_deref(),
                            model.snapshot_notice.as_deref(),
                            balances_section.as_deref(),
                            transfers_section.as_deref(),
                            review_instruction,
                            route_guidance.as_deref(),
                            footer.as_deref(),
                        ])
                    }
                    ReadViewPageData::Ledger {
                        balances,
                        recent_entries,
                    } => {
                        let balances_section =
                            render_page_section(balances, render_ledger_balances_section);
                        let recent_entries_section = render_page_section(
                            recent_entries,
                            render_ledger_recent_entries_section,
                        );
                        render_sections([
                            Some(model.title.as_ref()),
                            advisory.as_deref(),
                            model.page_indicator.as_deref(),
                            model.snapshot_notice.as_deref(),
                            balances_section.as_deref(),
                            recent_entries_section.as_deref(),
                            route_guidance.as_deref(),
                            footer.as_deref(),
                        ])
                    }
                };

                let svg_parts: Vec<crate::svg_table::RenderedSvg> = match data {
                    ReadViewPageData::Ledger {
                        balances,
                        recent_entries,
                    } => [balances.to_svg(), recent_entries.to_svg()]
                        .into_iter()
                        .flatten()
                        .collect(),
                    ReadViewPageData::Review {
                        balances,
                        transfers,
                    } => [balances.to_svg(), transfers.to_svg()]
                        .into_iter()
                        .flatten()
                        .collect(),
                };

                let compact_text = render_sections([
                    Some(model.title.as_ref()),
                    advisory.as_deref(),
                    model.page_indicator.as_deref(),
                    model.snapshot_notice.as_deref(),
                    review_instruction,
                    route_guidance.as_deref(),
                    footer.as_deref(),
                ]);

                return match NonEmptyVec::new(svg_parts) {
                    Ok(svgs) => {
                        let image = combine_svgs_vertically(&svgs);
                        RenderedSurface::new_image_backed(
                            compact_text,
                            fallback_text,
                            image,
                            action_rows,
                            model.ephemeral,
                        )
                    }
                    Err(_) => {
                        RenderedSurface::new_text(fallback_text, action_rows, model.ephemeral)
                    }
                };
            }
        };

        RenderedSurface::new_text(body, action_rows, model.ephemeral)
    }

    pub fn render_void_flow(
        model: &VoidSurfaceModel,
    ) -> Result<TextRenderedSurface, RenderBudgetError> {
        let mut action_rows = model.action_rows.clone();
        append_recovery_cta_row(&mut action_rows, &model.recovery_action);

        let body = if model.stale_page {
            render_sections([
                Some(model.title.as_ref()),
                Some(i18n::STALE_VOID_PAGE_MESSAGE),
                model
                    .missing_thread_note
                    .then_some(i18n::NO_LEDGER_THREAD_YET_NOTE),
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
                    .then_some(i18n::NO_LEDGER_THREAD_YET_NOTE),
            ])
        };

        TextRenderedSurface::new(body, action_rows, model.ephemeral)
    }

    pub fn render_write_rejection(
        model: &WriteRejectionSurfaceModel,
    ) -> Result<TextRenderedSurface, RenderBudgetError> {
        TextRenderedSurface::new(
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
    ) -> Result<TextRenderedSurface, RenderBudgetError> {
        TextRenderedSurface::new(
            render_sections([
                Some(model.title.as_ref()),
                non_empty_join(&model.status_lines).as_deref(),
                model.recovery_reference.as_deref(),
            ]),
            model.action_rows.clone(),
            model.ephemeral,
        )
    }
}

fn render_public_expense_full(model: &PublicExpenseMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_expense_header(model.entry_id).to_string(),
        i18n::public_date_line(model.effective_date).to_string(),
        i18n::public_payer_line(&model.payer_display_name).to_string(),
        i18n::public_amount_line(&model.amount).to_string(),
        i18n::PUBLIC_PARTICIPANTS_HEADING.to_owned(),
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
    lines
}

fn render_public_expense_compact(model: &PublicExpenseMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_expense_header(model.entry_id).to_string(),
        i18n::public_date_line(model.effective_date).to_string(),
        i18n::public_payer_line(&model.payer_display_name).to_string(),
        i18n::public_amount_line(&model.amount).to_string(),
        i18n::PUBLIC_PARTICIPANTS_HEADING.to_owned(),
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
    lines
}

fn render_public_settlement_full(model: &PublicSettlementMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_settlement_header(model.entry_id).to_string(),
        i18n::public_date_line(model.recorded_date).to_string(),
        i18n::PUBLIC_TRANSFER_HEADING.to_owned(),
    ];
    lines.extend(model.transfers.iter().map(render_transfer_row));
    lines.push(i18n::public_confirmed_by_line(&model.actor_display_name).to_string());
    lines.push(i18n::public_recorded_at_line(&model.recorded_at).to_string());
    lines
}

fn render_public_settlement_compact(model: &PublicSettlementMessageModel) -> Vec<String> {
    let mut lines = vec![
        i18n::public_settlement_header(model.entry_id).to_string(),
        i18n::public_date_line(model.recorded_date).to_string(),
        i18n::PUBLIC_TRANSFER_HEADING.to_owned(),
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
    lines
}

fn render_public_void_full(model: &PublicVoidMessageModel) -> Vec<String> {
    vec![
        i18n::public_void_line(
            model.entry_id,
            &model.voider_display_name,
            &model.voided_at,
            model.original_summary.render_candidate_summary(),
        )
        .to_string(),
        i18n::PUBLIC_VOID_PRESERVED_LINE.to_owned(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
    ]
}

fn render_public_void_compact(model: &PublicVoidMessageModel) -> Vec<String> {
    vec![
        i18n::public_void_line(
            model.entry_id,
            &model.voider_display_name,
            &model.voided_at,
            model.original_summary.render_candidate_summary(),
        )
        .to_string(),
        i18n::PUBLIC_VOID_PRESERVED_LINE.to_owned(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
    ]
}

fn render_public_seal_full(model: &PublicSealMessageModel) -> Vec<String> {
    vec![
        i18n::public_seal_header(model.entry_id).to_string(),
        i18n::public_seal_line(
            &model.actor_display_name,
            model.through_entry_id,
            model.through_summary.render_sealed_through_summary(),
        )
        .to_string(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
    ]
}

fn render_public_seal_compact(model: &PublicSealMessageModel) -> Vec<String> {
    vec![
        i18n::public_seal_line(
            &model.actor_display_name,
            model.through_entry_id,
            excerpt_with_ellipsis(
                &model.through_summary.render_sealed_through_summary(),
                PUBLIC_COMPACT_SUMMARY_LIMIT,
            ),
        )
        .to_string(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
    ]
}

fn render_public_balance_adjustment_full(
    model: &PublicBalanceAdjustmentMessageModel,
) -> Vec<String> {
    vec![
        i18n::public_balance_adjustment_header(model.entry_id).to_string(),
        i18n::public_balance_adjustment_line(
            &model.actor_display_name,
            model.reason.as_str(),
            render_impact_summary(&model.impacts),
        )
        .to_string(),
        i18n::public_recorded_at_line(&model.recorded_at).to_string(),
    ]
}

fn render_public_balance_adjustment_compact(
    model: &PublicBalanceAdjustmentMessageModel,
) -> Vec<String> {
    vec![
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
    ]
}

fn render_compacted_canonical_message(
    full_content: &[String],
    compact_content: &[String],
    recovery_reference: &RecoveryReference,
) -> Result<RenderedCanonicalMessage, RenderBudgetError> {
    match RenderedCanonicalMessage::new(
        full_content,
        recovery_reference,
        CanonicalMessageKind::Full,
    ) {
        Err(RenderBudgetError::MessageContentTooLong { .. }) => RenderedCanonicalMessage::new(
            compact_content,
            recovery_reference,
            CanonicalMessageKind::Truncated,
        ),
        result => result,
    }
}

fn render_review_balances_section(balances: &[BalanceRow]) -> String {
    let mut lines = vec![i18n::BALANCES_EXPLAINER.to_owned()];
    if balances.is_empty() {
        lines.push(i18n::REVIEW_ZERO_BALANCES.to_owned());
    } else {
        lines.extend(balances.iter().map(render_balance_row));
    }
    render_section(i18n::BALANCES_HEADING, lines)
}

fn render_review_transfers_section(transfers: &[TransferRow]) -> String {
    let lines = transfers
        .iter()
        .map(render_transfer_row)
        .collect::<Vec<_>>();
    render_section(i18n::SETTLEMENT_PLAN_HEADING, lines)
}

fn render_ledger_recent_entries_section(entries: &[RecentEntryRow]) -> String {
    let lines = if entries.is_empty() {
        vec![i18n::RECENT_ENTRIES_NONE.to_owned()]
    } else {
        entries.iter().map(render_recent_entry_row).collect()
    };
    render_section(i18n::RECENT_ENTRIES_HEADING, lines)
}

fn render_recent_entry_row(row: &RecentEntryRow) -> String {
    format!(
        "{} | {}",
        i18n::recent_entry_line(row.entry_id, row.summary.render_candidate_summary()),
        row.recovery_reference.render_line()
    )
}

fn render_ledger_balances_section(balances: &[BalanceRow]) -> String {
    let mut lines = vec![i18n::BALANCES_EXPLAINER.to_owned()];
    if balances.is_empty() {
        lines.push(i18n::LEDGER_ZERO_BALANCES.to_owned());
    } else {
        lines.extend(balances.iter().map(render_balance_row));
    }
    render_section(i18n::BALANCES_HEADING, lines)
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
                        let label = option.label.truncate_for_component_label();
                        validate_component_label(label.as_str())?;
                        if let Some(description) = &option.description {
                            validate_component_description(description)?;
                        }
                        Ok(SurfaceSelectOption {
                            value: option.value,
                            label,
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

fn render_page_section<T>(
    section: &PageSection<T>,
    render_fn: impl FnOnce(&[T]) -> String,
) -> Option<String> {
    match section {
        PageSection::Hidden => None,
        PageSection::Empty => Some(render_fn(&[])),
        PageSection::Rows(rows) => Some(render_fn(rows)),
    }
}

fn render_required_page_section<T>(
    section: &RequiredPageSection<T>,
    render_fn: impl FnOnce(&[T]) -> String,
) -> Option<String> {
    match section {
        RequiredPageSection::Hidden => None,
        RequiredPageSection::Rows(rows) => Some(render_fn(rows)),
    }
}

fn render_void_candidate_line(candidate: &super::void_surfaces::VoidCandidateRow) -> String {
    format!(
        "{} | {}",
        candidate.summary.render_candidate_summary(),
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

fn append_recovery_cta_row(
    action_rows: &mut Vec<SurfaceActionRow>,
    recovery_action: &RecoveryAction,
) {
    match RecoveryCtaButtonRender::from(recovery_action) {
        RecoveryCtaButtonRender::Button(button) => {
            action_rows.push(SurfaceActionRow::Buttons(vec![
                button.into_surface_button(),
            ]));
        }
        RecoveryCtaButtonRender::Omit => {}
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RecoveryCtaButtonRender {
    Button(RecoveryCtaButton),
    Omit,
}

impl From<&RecoveryAction> for RecoveryCtaButtonRender {
    fn from(recovery_action: &RecoveryAction) -> Self {
        match recovery_action {
            RecoveryAction::ThreadLink { url } => {
                Self::Button(RecoveryCtaButton::ThreadLink { url: url.clone() })
            }
            RecoveryAction::ParentLink { url } => {
                Self::Button(RecoveryCtaButton::ParentLink { url: url.clone() })
            }
            RecoveryAction::RecoveryReferenceOnly | RecoveryAction::None => Self::Omit,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RecoveryCtaButton {
    ThreadLink { url: DiscordLinkUrl },
    ParentLink { url: DiscordLinkUrl },
}

impl RecoveryCtaButton {
    fn into_surface_button(self) -> SurfaceButton {
        match self {
            Self::ThreadLink { url } => SurfaceButton::Link {
                label: i18n::OPEN_LEDGER_THREAD_LABEL.to_owned(),
                url,
                disabled: false,
            },
            Self::ParentLink { url } => SurfaceButton::Link {
                label: i18n::OPEN_PARENT_CHANNEL_LABEL.to_owned(),
                url,
                disabled: false,
            },
        }
    }
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

fn append_ascii_ellipsis_after_prefix(text: &str, prefix_chars: usize) -> String {
    let actual = text.chars().count();
    if actual <= prefix_chars {
        return text.to_owned();
    }

    let prefix: String = text.chars().take(prefix_chars).collect();
    format!("{prefix}...")
}

fn fit_with_ascii_ellipsis(text: &str, max_chars: usize) -> String {
    let actual = text.chars().count();
    if actual <= max_chars {
        return text.to_owned();
    }

    let prefix: String = text.chars().take(max_chars.saturating_sub(3)).collect();
    format!("{prefix}...")
}

#[cfg(test)]
mod tests {
    use super::{
        BalanceDirection, BalanceImpactRow, BalanceRow, BusinessDateTime, DiscordLedgerPresenter,
        DiscordLinkUrl, DiscordLinkUrlError, DocumentRoute, ExpenseOrSettlementSummary,
        ExpenseSurfaceModel, LedgerRoute, LedgerSurfaceSummary, PageSection, PanelButtonStates,
        PanelSurfaceModel, PublicBalanceAdjustmentMessageModel, PublicCanonicalMessageModel,
        PublicExpenseMessageModel, PublicSealMessageModel, PublicSettlementMessageModel,
        PublicVoidMessageModel, ReadViewDocument, ReadViewPageData, ReadViewPageModel,
        ReadViewPageState, RecentEntryRow, RecoveryAction, RecoveryReference, RequiredPageSection,
        ReviewRoute, ReviewSettleAction, SafeLiteralText, Section, SurfaceActionRow, SurfaceButton,
        SurfaceInteractiveButtonStyle, SurfaceSelectMenu, SurfaceSelectOption, TransferRow,
        UncertainWriteSurfaceModel,
    };
    use crate::discord_ledger::{
        budgets::{validate_button_label, validate_component_placeholder},
        pickers::PickerSurfaceModel,
        void_surfaces::{
            VoidCandidateRow, VoidConfirmationRecap, VoidRetargetReason, VoidSurfaceModel,
        },
    };
    use walicord_application::ledger::{LedgerEffectiveDate, LedgerEntryId};
    use walicord_domain::NonEmptyVec;
    use walicord_i18n as i18n;

    #[test]
    fn recovery_reference_only_and_none_do_not_emit_dead_cta_buttons() {
        for recovery_action in [RecoveryAction::RecoveryReferenceOnly, RecoveryAction::None] {
            let actual =
                DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel::test_fixture(
                    DocumentRoute::Review(ReviewRoute::Parent),
                    "清算確認",
                    false,
                    recovery_action,
                    Vec::new(),
                    ReadViewPageState::Empty {
                        empty_state: std::borrow::Cow::Borrowed("empty"),
                    },
                ))
                .expect("surface should render");

            assert!(actual.action_rows().is_empty());
        }
    }

    #[test]
    fn discord_link_url_rejects_non_http_schemes() {
        let actual = DiscordLinkUrl::parse("ftp://discord.example/channels/1/2");

        assert_eq!(
            actual,
            Err(DiscordLinkUrlError::UnsupportedScheme("ftp".to_owned()))
        );
    }

    #[test]
    fn review_document_with_settle_action_contains_settle_button() {
        let doc = ReadViewDocument::review(
            ReviewRoute::Thread,
            Section::Empty,
            NonEmptyVec::new(vec![transfer("Bob", "Alice", "300")]).expect("non-empty"),
            false,
            RecoveryAction::None,
            ReviewSettleAction::enabled("ledger:review:settle:1"),
        );

        assert_eq!(doc.action_rows().len(), 1);
    }

    #[test]
    fn review_document_with_hidden_settle_action_has_no_settle_button() {
        let doc = ReadViewDocument::review(
            ReviewRoute::Thread,
            Section::Empty,
            NonEmptyVec::new(vec![transfer("Bob", "Alice", "300")]).expect("non-empty"),
            false,
            RecoveryAction::None,
            ReviewSettleAction::Hidden,
        );

        assert!(doc.action_rows().is_empty());
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
            actual.text_body(),
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
        assert!(
            actual
                .body()
                .contains(i18n::PUBLIC_TRUNCATION_LEDGER_GUIDANCE)
        );
        assert!(!actual.body().contains("参加者11"));
    }

    #[test]
    fn public_expense_compact_renderer_preserves_full_labels_within_budget() {
        let payer = format!("支払者{}", "い".repeat(80));
        let actor = format!("記録者{}", "う".repeat(80));
        let participant = format!("参加者10{}", "あ".repeat(80));
        let participants = (1..=20)
            .map(|index| participant_share(&format!("参加者{index:02}{}", "あ".repeat(80)), "100"))
            .collect::<Vec<_>>();

        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Expense(PublicExpenseMessageModel {
                entry_id: LedgerEntryId(7),
                effective_date: date("2026-05-25"),
                payer_display_name: name(&payer),
                amount: "1500".to_owned(),
                participant_rows: participants,
                note: None,
                actor_display_name: name(&actor),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(7),
            }),
        )
        .expect("compact message should preserve full labels");

        assert!(actual.body().contains(&payer));
        assert!(actual.body().contains(&actor));
        assert!(actual.body().contains(&participant));
        assert!(actual.body().contains(i18n::PUBLIC_TRUNCATION_OMITTED));
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
        assert!(actual.body().contains(i18n::PUBLIC_TRUNCATION_OMITTED));
    }

    #[test]
    fn public_settlement_compact_renderer_preserves_full_labels_within_budget() {
        let actor = format!("確定者{}", "う".repeat(60));
        let sender = format!("送金者10{}", "あ".repeat(60));
        let receiver = format!("受取者10{}", "い".repeat(60));
        let transfers = (1..=15)
            .map(|index| {
                transfer(
                    &format!("送金者{index:02}{}", "あ".repeat(60)),
                    &format!("受取者{index:02}{}", "い".repeat(60)),
                    "300",
                )
            })
            .collect::<Vec<_>>();

        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Settlement(PublicSettlementMessageModel {
                entry_id: LedgerEntryId(8),
                recorded_date: date("2026-05-25"),
                transfers,
                actor_display_name: name(&actor),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(8),
            }),
        )
        .expect("compact message should preserve full labels");

        assert!(actual.body().contains(&actor));
        assert!(actual.body().contains(&sender));
        assert!(actual.body().contains(&receiver));
        assert!(actual.body().contains(i18n::PUBLIC_TRUNCATION_OMITTED));
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
                original_summary: ExpenseOrSettlementSummary::Expense {
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
                original_summary: ExpenseOrSettlementSummary::Expense {
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
                through_summary: LedgerSurfaceSummary::ExpenseOrSettlement(
                    ExpenseOrSettlementSummary::Settlement {
                        date: date("2026-05-20"),
                        from_display_name: name("Alice"),
                        to_display_name: name("Bob"),
                        amount: "400".to_owned(),
                        additional_transfers: 2,
                    },
                ),
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
    fn public_void_compact_branch_truncates_extreme_original_summaries_and_still_renders() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Void(PublicVoidMessageModel {
                entry_id: LedgerEntryId(9),
                voider_display_name: name(&format!("取消者{}", "あ".repeat(1500))),
                voided_at: timestamp("2026-05-25 18:55"),
                original_summary: ExpenseOrSettlementSummary::Expense {
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
    fn public_seal_compact_branch_keeps_the_truncation_cue_and_drops_the_header() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::Seal(PublicSealMessageModel {
                entry_id: LedgerEntryId(10),
                actor_display_name: name(&format!("Operator{}", "あ".repeat(1200))),
                through_entry_id: LedgerEntryId(4),
                through_summary: LedgerSurfaceSummary::ExpenseOrSettlement(
                    ExpenseOrSettlementSummary::Settlement {
                        date: date("2026-05-20"),
                        from_display_name: name(&format!("Alice{}", "い".repeat(1200))),
                        to_display_name: name("Bob"),
                        amount: "400".to_owned(),
                        additional_transfers: 2,
                    },
                ),
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(10),
            }),
        )
        .expect("compact message should satisfy Discord limits");

        assert!(actual.body().contains(i18n::PUBLIC_TRUNCATION_OMITTED));
        assert!(!actual.body().contains("確認 [#10]"));
    }

    #[test]
    fn public_balance_adjustment_compact_branch_keeps_the_truncation_cue_and_drops_header() {
        let actual = DiscordLedgerPresenter::render_public_entry(
            &PublicCanonicalMessageModel::BalanceAdjustment(PublicBalanceAdjustmentMessageModel {
                entry_id: LedgerEntryId(11),
                actor_display_name: name(&format!("Operator{}", "あ".repeat(1200))),
                reason: note(&format!("差額補正{}", "う".repeat(1200))),
                impacts: vec![
                    impact("Alice", "300", BalanceDirection::Receive),
                    impact("Bob", "300", BalanceDirection::Pay),
                ],
                recorded_at: timestamp("2026-05-25 18:55"),
                recovery_reference: recovery_reference(11),
            }),
        )
        .expect("compact message should satisfy Discord limits");

        assert!(actual.body().contains(i18n::PUBLIC_TRUNCATION_OMITTED));
        assert!(!actual.body().contains("残高補正 [#11]"));
    }

    #[test]
    fn review_view_renders_balances_then_transfers_and_uncertain_write_copy() {
        let page = ReadViewPageModel::test_fixture(
            DocumentRoute::Review(ReviewRoute::Thread),
            "清算確認",
            true,
            RecoveryAction::parent_link(link_url("https://discord.com/channels/1/2/3")),
            vec!["清算完了".to_owned()],
            ReadViewPageState::Populated(ReadViewPageData::Review {
                balances: PageSection::Rows(
                    NonEmptyVec::new(vec![
                        balance("Alice", "300", BalanceDirection::Receive),
                        balance("Bob", "300", BalanceDirection::Pay),
                    ])
                    .expect("non-empty"),
                ),
                transfers: RequiredPageSection::Rows(
                    NonEmptyVec::new(vec![transfer("Bob", "Alice", "300")]).expect("non-empty"),
                ),
            }),
        )
        .with_route_guidance(vec!["/ledger を開き直してください".to_owned()]);
        let actual =
            DiscordLedgerPresenter::render_read_view_page(&page).expect("surface should render");

        assert_eq!(
            actual.text_body(),
            "清算確認\n\nℹ️ この台帳は現在書き込み確認中です。記録・取り消し・清算は一時的に制限されています。\n\n残高\n確認済み履歴と残高補正を含む現在差額\n- Alice: 受け取り 300円\n- Bob: 支払い 300円\n\n送金予定\n- Bob -> Alice 300円\n\nこの台帳は書き込み状態を確認中です。台帳スレッドを確認し、復旧後に /settle を実行してください。\n\n/ledger を開き直してください\n\n清算完了"
        );
        let SurfaceActionRow::Buttons(buttons) = &actual.action_rows()[0] else {
            panic!("expected recovery button row");
        };
        assert_eq!(buttons[0].label(), "親チャンネルを開く");
    }

    #[test]
    fn populated_review_page_renders_image_backed_with_table_free_compact_text() {
        let actual =
            DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel::test_fixture(
                DocumentRoute::Review(ReviewRoute::Thread),
                "清算確認",
                false,
                RecoveryAction::None,
                Vec::new(),
                ReadViewPageState::Populated(ReadViewPageData::Review {
                    balances: PageSection::Rows(
                        NonEmptyVec::new(vec![balance("Alice", "300", BalanceDirection::Receive)])
                            .expect("non-empty"),
                    ),
                    transfers: RequiredPageSection::Rows(
                        NonEmptyVec::new(vec![transfer("Bob", "Alice", "300")]).expect("non-empty"),
                    ),
                }),
            ))
            .expect("surface should render");

        let super::PresentationSurfaceBody::ImageBacked {
            compact_text,
            fallback_text,
            image,
        } = actual.body()
        else {
            panic!("expected ImageBacked body");
        };
        assert!(compact_text.as_str().contains("清算確認"));
        assert!(
            compact_text
                .as_str()
                .contains(i18n::REVIEW_PREVIEW_INSTRUCTION)
        );
        assert!(!compact_text.as_str().contains("Alice"));
        assert!(fallback_text.as_str().contains("Alice"));
        let xml = image.to_svg_string();
        assert!(xml.contains("Alice"));
        assert!(xml.contains("Bob"));
    }

    #[test]
    fn stale_and_empty_pages_render_text_only_bodies() {
        let stale =
            DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel::test_fixture(
                DocumentRoute::Ledger(LedgerRoute::Panel),
                "台帳",
                false,
                RecoveryAction::None,
                Vec::new(),
                ReadViewPageState::Stale {
                    missing_thread_note: false,
                },
            ))
            .expect("stale should render");
        assert!(matches!(
            stale.body(),
            super::PresentationSurfaceBody::Text(_)
        ));

        let empty =
            DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel::test_fixture(
                DocumentRoute::Ledger(LedgerRoute::Command),
                "台帳",
                false,
                RecoveryAction::None,
                Vec::new(),
                ReadViewPageState::Empty {
                    empty_state: std::borrow::Cow::Borrowed("empty"),
                },
            ))
            .expect("empty should render");
        assert!(matches!(
            empty.body(),
            super::PresentationSurfaceBody::Text(_)
        ));
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
            actual.text_body(),
            "1/4 支払者\n\n選択中: Alice\n\nページ 1/2\n\n1-25 / 30\n\nこの表示は固定スナップショットです。\n\n候補から 1 人を選んでください。"
        );
        let SurfaceActionRow::Select(select_menu) = &actual.action_rows()[0] else {
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

        let SurfaceActionRow::Select(select_menu) = &actual.action_rows()[0] else {
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
    fn ledger_view_renders_balances_and_recent_entries() {
        let actual =
            DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel::test_fixture(
                DocumentRoute::Ledger(LedgerRoute::Command),
                "台帳",
                true,
                RecoveryAction::None,
                vec!["表示範囲: 最新の検証済み台帳".to_owned()],
                ReadViewPageState::Populated(ReadViewPageData::Ledger {
                    balances: PageSection::Rows(
                        NonEmptyVec::new(vec![
                            balance("Alice", "300", BalanceDirection::Receive),
                            balance("Bob", "300", BalanceDirection::Pay),
                        ])
                        .expect("non-empty"),
                    ),
                    recent_entries: PageSection::Rows(
                        NonEmptyVec::new(vec![
                            RecentEntryRow {
                                entry_id: LedgerEntryId(5),
                                summary: ExpenseOrSettlementSummary::Expense {
                                    date: date("2026-05-26"),
                                    payer_display_name: name("Alice"),
                                    amount: "1500".to_owned(),
                                    note: Some(note("ランチ")),
                                },
                                recovery_reference: recovery_reference(5),
                            },
                            RecentEntryRow {
                                entry_id: LedgerEntryId(3),
                                summary: ExpenseOrSettlementSummary::Settlement {
                                    date: date("2026-05-25"),
                                    from_display_name: name("Bob"),
                                    to_display_name: name("Alice"),
                                    amount: "300".to_owned(),
                                    additional_transfers: 0,
                                },
                                recovery_reference: recovery_reference(3),
                            },
                        ])
                        .expect("non-empty"),
                    ),
                }),
            ))
            .expect("surface should render");

        assert!(actual.text_body().contains("残高\n"));
        assert!(actual.text_body().contains("- Alice: 受け取り 300円"));
        assert!(actual.text_body().contains("最近の記録\n"));
        assert!(
            actual
                .text_body()
                .contains("[#5] 2026-05-26 Alice の支払い 1500円")
        );
        assert!(
            actual
                .text_body()
                .contains("[#3] 2026-05-25 清算 Bob -> Alice 300円")
        );
        assert!(actual.text_body().contains("表示範囲: 最新の検証済み台帳"));
    }

    #[test]
    fn ledger_view_uses_the_inviting_empty_state_without_scope_footer() {
        let actual =
            DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel::test_fixture(
                DocumentRoute::Ledger(LedgerRoute::Panel),
                "台帳",
                false,
                RecoveryAction::None,
                vec!["表示範囲: これは出てはいけない".to_owned()],
                ReadViewPageState::Empty {
                    empty_state: std::borrow::Cow::Borrowed(i18n::LEDGER_EMPTY_STATE),
                },
            ))
            .expect("surface should render");

        assert_eq!(
            actual.text_body(),
            "台帳\n\nまだ記録がありません。/expense または 記録する ボタンで最初の支出を記録してみましょう。"
        );
    }

    #[test]
    fn parent_review_route_uses_the_thread_handoff_instruction_and_cta_label() {
        let actual =
            DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel::test_fixture(
                DocumentRoute::Review(ReviewRoute::Parent),
                "清算確認",
                false,
                RecoveryAction::thread_link(link_url("https://discord.com/channels/1/2/4")),
                Vec::new(),
                ReadViewPageState::Populated(ReadViewPageData::Review {
                    balances: PageSection::Rows(
                        NonEmptyVec::new(vec![balance("Alice", "300", BalanceDirection::Receive)])
                            .expect("non-empty"),
                    ),
                    transfers: RequiredPageSection::Rows(
                        NonEmptyVec::new(vec![transfer("Bob", "Alice", "300")]).expect("non-empty"),
                    ),
                }),
            ))
            .expect("surface should render");

        assert!(
            actual
                .text_body()
                .contains(i18n::REVIEW_PREVIEW_INSTRUCTION)
        );
        let SurfaceActionRow::Buttons(buttons) = &actual.action_rows()[0] else {
            panic!("expected thread handoff button");
        };
        assert_eq!(buttons[0].label(), "台帳スレッドを開く");
    }

    #[test]
    fn stale_read_and_void_pages_use_fixed_reopen_copy_and_cta_contracts() {
        let stale_read =
            DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel::test_fixture(
                DocumentRoute::Ledger(LedgerRoute::Panel),
                "台帳",
                false,
                RecoveryAction::None,
                Vec::new(),
                ReadViewPageState::Stale {
                    missing_thread_note: true,
                },
            ))
            .expect("stale ledger view should render");
        let stale_void = DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::stale_page(
            "取り消し",
            RecoveryAction::parent_link(link_url("https://discord.com/channels/1/2/3")),
            false,
            Vec::new(),
            true,
        ))
        .expect("stale void view should render");

        assert_eq!(
            stale_read.text_body(),
            "台帳\n\nこの表示は期限切れです。/ledger または 台帳 で開き直してください。\n\n-# まだ台帳スレッドはありません。最初の記録後に全件確認できます。"
        );
        assert_eq!(
            stale_void.text_body(),
            "取り消し\n\nこの表示は期限切れです。/void または 取り消し で開き直してください。"
        );
        let SurfaceActionRow::Buttons(buttons) = &stale_void.action_rows()[0] else {
            panic!("expected parent reopen button");
        };
        assert_eq!(buttons[0].label(), "親チャンネルを開く");
    }

    #[test]
    fn ledger_view_paged_shows_only_entries_when_balances_empty() {
        let actual =
            DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel::test_fixture_paged(
                DocumentRoute::Ledger(LedgerRoute::Command),
                "台帳",
                Some(i18n::page_indicator(2, 2).to_string()),
                Some(i18n::SNAPSHOT_NOTICE.to_owned()),
                ReadViewPageState::Populated(ReadViewPageData::Ledger {
                    balances: PageSection::Hidden,
                    recent_entries: PageSection::Rows(
                        NonEmptyVec::new(vec![RecentEntryRow {
                            entry_id: LedgerEntryId(1),
                            summary: ExpenseOrSettlementSummary::Expense {
                                date: date("2026-05-20"),
                                payer_display_name: name("Alice"),
                                amount: "800".to_owned(),
                                note: None,
                            },
                            recovery_reference: recovery_reference(1),
                        }])
                        .expect("non-empty"),
                    ),
                }),
            ))
            .expect("paged ledger view should render");

        assert!(actual.text_body().contains("ページ 2/2"));
        assert!(
            actual
                .text_body()
                .contains("[#1] 2026-05-20 Alice の支払い 800円")
        );
        assert!(!actual.text_body().contains(i18n::LEDGER_ZERO_BALANCES));
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

        assert!(actual.text_body().is_empty());
        let SurfaceActionRow::Buttons(buttons) = &actual.action_rows()[0] else {
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
            i18n::PANEL_RECORD_BUTTON_LABEL,
            i18n::PANEL_REVIEW_BUTTON_LABEL,
            i18n::PANEL_LEDGER_BUTTON_LABEL,
            i18n::PANEL_VOID_BUTTON_LABEL,
            i18n::OPEN_PARENT_CHANNEL_LABEL,
            i18n::OPEN_LEDGER_THREAD_LABEL,
        ] {
            validate_button_label(label).expect("button label should fit Discord limits");
        }

        validate_component_placeholder(i18n::EXPENSE_MODAL_NOTE_PLACEHOLDER)
            .expect("placeholder should fit Discord limits");
    }

    #[test]
    fn void_selection_surface_renders_candidate_summaries_and_recovery_refs() {
        let model = VoidSurfaceModel::selection(
            "取り消し",
            vec![
                VoidCandidateRow {
                    summary: ExpenseOrSettlementSummary::Expense {
                        date: date("2026-05-24"),
                        payer_display_name: name("Bob"),
                        amount: "1200".to_owned(),
                        note: Some(note("ランチの会計")),
                    },
                    recovery_reference: recovery_reference(7),
                },
                VoidCandidateRow {
                    summary: ExpenseOrSettlementSummary::Settlement {
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
            actual.text_body(),
            "取り消し\n\n2026-05-24 Bob の支払い 1200円 メモ: ランチの会計 | 復旧用の参照: ledger:abcd1234/entry:7 | <https://discord.com/channels/1/2/7>\n2026-05-25 清算 Alice -> Bob 300円 ほか2件 | 復旧用の参照: ledger:abcd1234/entry:8 | <https://discord.com/channels/1/2/8>"
        );
        assert_eq!(model.primary_action_label.as_deref(), Some("確認へ"));
    }

    #[test]
    fn void_confirmation_surface_repeats_the_full_target_recap() {
        let model = VoidSurfaceModel::confirmation(
            "取り消し確認",
            VoidConfirmationRecap {
                summary: ExpenseOrSettlementSummary::Expense {
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
            actual.text_body(),
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
            summary: ExpenseOrSettlementSummary::Settlement {
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
        assert_eq!(settlement_lines[1], "対象: 清算 Alice -> Bob 300円 ほか2件");
    }

    #[test]
    fn void_confirmation_lines_use_ascii_ellipsis_for_note_excerpts() {
        let lines = super::render_void_confirmation_lines(&VoidConfirmationRecap {
            summary: ExpenseOrSettlementSummary::Expense {
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
            summary: ExpenseOrSettlementSummary::Expense {
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
                    summary: ExpenseOrSettlementSummary::Expense {
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
                .text_body()
                .starts_with("取り消し\n\n対象を選択してください。")
        );
        assert!(
            actual
                .text_body()
                .contains("2026-05-24 Bob の支払い 1200円")
        );
    }

    #[test]
    fn void_selection_surface_can_render_a_stale_target_reason() {
        let actual = DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::stale_target(
            "取り消し",
            VoidRetargetReason::EnteredSealedRange,
            vec![VoidCandidateRow {
                summary: ExpenseOrSettlementSummary::Settlement {
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

        assert!(actual.text_body().contains(
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
            actual.text_body(),
            "取り消し完了\n\n取り消しました。\nこの取り消しで元の記録の影響は打ち消されます。正しい内容が必要なら記録し直してください。\n台帳スレッドで確認できます: <#1234>"
        );
    }

    #[test]
    fn void_success_surface_omits_dead_thread_mentions_and_uses_parent_cta() {
        let actual =
            DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::success_with_recovery(
                "取り消し完了",
                None,
                RecoveryAction::parent_link(link_url("https://discord.com/channels/1/2")),
                Vec::new(),
                true,
            ))
            .expect("surface should render");

        assert!(actual.text_body().contains("台帳スレッドで確認できます。"));
        assert!(!actual.text_body().contains("<#"));
        let SurfaceActionRow::Buttons(buttons) = &actual.action_rows()[0] else {
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
            empty.text_body(),
            "取り消し\n\nまだ記録がありません。/expense または 記録する ボタンで最初の支出を記録してみましょう。"
        );
        assert_eq!(
            no_candidates.text_body(),
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
            actual.text_body(),
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
                    url: link_url("https://discord.com/channels/1/2/3"),
                    disabled: false,
                }])],
                ephemeral: true,
            })
            .expect("surface should render");

        assert_eq!(
            actual.text_body(),
            "書き込み確認中\n\n前回の書き込み結果を確認中です。\n\n復旧用の参照: ledger:abcd1234/entry:7"
        );
        assert_eq!(actual.action_rows().len(), 1);
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

    fn link_url(raw: &str) -> DiscordLinkUrl {
        DiscordLinkUrl::parse(raw).expect("url should parse")
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
