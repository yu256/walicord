use chrono::{Datelike, FixedOffset, NaiveDate, TimeZone, Utc};
use fs2::FileExt;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use serenity::{
    all::{
        ActionRowComponent, ButtonStyle, ChannelId, CommandInteraction, ComponentInteraction,
        ComponentInteractionDataKind, CreateActionRow, CreateAttachment, CreateButton,
        CreateInputText, CreateInteractionResponse, CreateModal, CreateSelectMenu,
        CreateSelectMenuKind, CreateSelectMenuOption, GuildId, InputTextStyle, Member, Message,
        MessageId, ModalInteraction, Permissions, RoleId as SerenityRoleId, UserId,
    },
    model::channel::ChannelType,
    prelude::*,
};
use smol_str::SmolStr;
use std::{
    collections::{HashMap, HashSet},
    env,
    fs::{File, OpenOptions},
    path::PathBuf,
    str::FromStr,
    sync::{
        Arc, Mutex as StdMutex,
        atomic::{AtomicU64, Ordering},
    },
    time::{SystemTime, UNIX_EPOCH},
};
use tokio::sync::Mutex;
use walicord_application::{
    Clock, InteractionNonce, NonceProvider, PreviewConfirmationBinding, PreviewInstanceId,
    PreviewedSettlement, SettleUpPolicy, SettlementOptimizationError, SettlementPlanner,
    ledger::{
        AdjustmentReason, AllocationSnapshot, BalanceAdjusted, BalanceAdjustment,
        BalanceAdjustmentSource, DiscordLedgerEntryError, DiscordLedgerSourceDescriptor, EntryHash,
        EntryVoided, ExpenseAuthoringError, ExpenseNote, ExpenseRecorded, LedgerEffectiveDate,
        LedgerEntry, LedgerEntryId, LedgerEntryMetadata, LedgerEvent, LedgerHashSuite,
        LedgerHistorySealed, LedgerId, LedgerSourceCanonical, LedgerSourceCanonicalKind,
        LedgerState, MemberAmount, MemberWeight, NormalizedSettlementPlanRecorded,
        PreviewedSettlementOutcome, ProjectedEntryKind, ProjectedLedger,
        RecordableExpenseAuthoring, ResolvedExpenseAuthoringInput, SchemaVersion,
        SettlementRecordError, UnverifiedLedgerStoreEnvelope, VerifiedLedgerSnapshot,
        VerifiedLedgerStoreEnvelope, build_discord_expense_entry, build_discord_void_entry,
        external_correction_source_for_transport_decode, ledger_chain_genesis_sha256_v1,
        make_unverified_envelope_sha256_v1, preview_settlement_from_snapshot,
        record_previewed_plan_matching, replay_entries,
    },
};
use walicord_domain::{
    Money, SettlementContext, Transfer,
    model::{MemberId, RoleId, Weight},
};
use walicord_i18n as i18n;
use walicord_infrastructure::HighsSettlementPlanner;
#[cfg(test)]
use walicord_presentation::discord_ledger::{BalanceRow, PanelButtonStates, PanelSurfaceModel};
use walicord_presentation::{
    BusinessDateTime, DiscordLedgerPresenter, SafeLiteralText, SurfaceMemberLabels,
    confirmation_source_disclosure_line,
    discord_ledger::{
        BalanceAdjustmentSummary, BalanceDirection, BalanceImpactRow,
        ExpenseConfirmationParticipantRow, ExpenseDraftSummary, ExpenseParticipantSourceBadge,
        LedgerSurfaceSummary, PublicBalanceAdjustmentMessageModel, PublicCanonicalMessageModel,
        PublicExpenseMessageModel, PublicSealMessageModel, PublicSettlementMessageModel,
        PublicVoidMessageModel, ReadViewKind, ReadViewPageModel, ReadViewRoute,
        ReadViewSectionVisibility, RecoveryContext, RecoveryCta, RecoveryReference,
        RenderedSurface, SealedRangeSummary, TransferRow, VoidCandidateRow, VoidConfirmationRecap,
        VoidRetargetReason, VoidSurfaceModel, VoidedEntryRow, balance_adjustment_rows,
        balance_rows_for_state, individual_selection_title, participant_names_for_state,
        participant_source_help_line, preview_transfer_rows, public_participant_rows,
        unknown_member_label, validate_message_content,
    },
    truncate_component_label, validate_button_label, validate_component_placeholder,
    validate_custom_id, validate_modal_title, validate_text_input_label,
    validate_text_input_placeholder,
};

#[allow(dead_code)]
#[path = "ledger/adapters.rs"]
mod adapters;
#[allow(dead_code)]
#[path = "ledger/expense_modal_open.rs"]
mod expense_modal_open;
#[allow(dead_code)]
#[path = "ledger/locator.rs"]
mod locator;
#[allow(dead_code)]
#[path = "ledger/observability.rs"]
mod observability;
#[allow(dead_code)]
#[path = "ledger/panel.rs"]
mod panel;
#[allow(dead_code)]
#[path = "ledger/permissions.rs"]
mod permissions;
#[allow(dead_code)]
#[path = "ledger/projection.rs"]
mod projection;
#[allow(dead_code)]
#[path = "ledger/response_writer.rs"]
mod response_writer;
#[allow(dead_code)]
#[path = "ledger/route_guard.rs"]
mod route_guard;
#[allow(dead_code)]
#[path = "ledger/router.rs"]
mod router;
#[allow(dead_code)]
#[path = "ledger/store.rs"]
mod store;

#[cfg(test)]
pub(crate) use self::{
    locator::resolved_void_thread_channel_id, response_writer::suppressed_allowed_mentions,
    route_guard::startup_channel_is_track_target,
};
pub(crate) use self::{
    locator::{
        CANONICAL_LEDGER_THREAD_NAME, canonical_thread_candidate_ids,
        first_verified_canonical_thread_id_for_panel,
    },
    panel::{
        LEDGER_PANEL_EXPENSE_ID, LEDGER_PANEL_LEDGER_ID, LEDGER_PANEL_REVIEW_ID,
        LEDGER_PANEL_VOID_ID, is_ledger_panel_component_id, render_panel_post_message,
    },
    permissions::{StartupReadinessFailure, required_gateway_intents, validate_startup_readiness},
    response_writer::{
        rendered_surface_to_message, safe_create_message, safe_edit_interaction_response,
        safe_edit_message, safe_interaction_response_message,
    },
    route_guard::{
        ChannelFlagAction, SlashScopeError, channel_flag_action, slash_scope_channel_id,
        startup_track_targets,
    },
};

pub(crate) use self::{
    adapters::{DiscordLedgerThreadLoader, DiscordRouterRosterFetcher},
    observability::TracingLedgerObservability,
    router::{InteractionDispatch, LedgerRouter, LedgerRouterDependencies},
    store::{DiscordCanonicalLedgerStore, WriterLineagePolicy},
};
pub(crate) use walicord_application::ledger::{
    expense_session::{ExpenseSessionStore, ModalRetryBindingStore, VoidSessionStore},
    preview_store::PreviewStore,
    write_coordinator::{UncertainWriteRegistry, WriteCoordinator},
};
pub(crate) use walicord_infrastructure::{ProcessNonceProvider, SystemClock};

pub const LEDGER_ATTACHMENT_FILENAME: &str = "walicord-ledger-entry.json";
#[cfg_attr(not(test), allow(dead_code))]
pub const EXPENSE_SOURCE_CANONICAL: &str = "expense/slash-modal/v1";
#[cfg_attr(not(test), allow(dead_code))]
pub const SETTLE_SOURCE_CANONICAL: &str = "settle/slash/v1";
#[cfg_attr(not(test), allow(dead_code))]
pub const VOID_SOURCE_CANONICAL: &str = "void/slash/v1";
pub const PREVIEW_EXPIRY_SECS: u64 = 10 * 60;
const INTERACTION_STATE_TTL_SECS: u64 = 10 * 60;
#[allow(dead_code)]
const ATTACHMENT_SCHEMA_VERSION: u32 = 1;
const BUSINESS_TIMEZONE_OFFSET_SECONDS: i32 = 9 * 60 * 60;
const PICKER_PAGE_SIZE: usize = 25;
const SURFACE_PAGE_SIZE: usize = 20;
const WRITER_LINEAGE_ALLOWLIST_ENV: &str = "WALICORD_LEDGER_WRITER_LINEAGE_ALLOWLIST";
const EXPENSE_MODAL_PREFIX: &str = "expense:new:";
const EXPENSE_WEIGHTS_MODAL_PREFIX: &str = "expense:weights:";
const EXPENSE_PAYER_PREFIX: &str = "expense:payer:";
const EXPENSE_PARTICIPANTS_PREFIX: &str = "expense:participants:";
const EXPENSE_ROLES_PREFIX: &str = "expense:roles:";
const EXPENSE_PICKER_PAYER_PREFIX: &str = "expense:picker-payer:";
const EXPENSE_PICKER_PARTICIPANTS_PREFIX: &str = "expense:picker-participants:";
const EXPENSE_PICKER_ROLES_PREFIX: &str = "expense:picker-roles:";
const EXPENSE_PICKER_PREVIOUS_PREFIX: &str = "expense:picker-prev:";
const EXPENSE_PICKER_NEXT_PREFIX: &str = "expense:picker-next:";
const EXPENSE_PICKER_SEARCH_PREFIX: &str = "expense:picker-search:";
const EXPENSE_CLEAR_PAYER_PREFIX: &str = "expense:clear-payer:";
const EXPENSE_CLEAR_PARTICIPANTS_PREFIX: &str = "expense:clear-participants:";
const EXPENSE_CLEAR_ROLES_PREFIX: &str = "expense:clear-roles:";
const EXPENSE_TOGGLE_MEMBERS_PREFIX: &str = "expense:toggle-members:";
const EXPENSE_OPEN_WEIGHTS_PREFIX: &str = "expense:open-weights:";
const EXPENSE_RESET_WEIGHTS_PREFIX: &str = "expense:reset-weights:";
const EXPENSE_CONFIRM_WEIGHTS_PREFIX: &str = "expense:confirm-weights:";
const EXPENSE_CONFIRM_RESET_WEIGHTS_PREFIX: &str = "expense:confirm-reset-weights:";
const EXPENSE_CONFIRM_PREFIX: &str = "expense:confirm:";
const EXPENSE_CONFIRM_PREVIOUS_PREFIX: &str = "expense:confirm-prev:";
const EXPENSE_CONFIRM_NEXT_PREFIX: &str = "expense:confirm-next:";
const EXPENSE_CONFIRM_BACK_PREFIX: &str = "expense:confirm-back:";
const EXPENSE_REVISE_PREFIX: &str = "expense:revise:";
const EXPENSE_NEXT_PREFIX: &str = "expense:next:";
const EXPENSE_BACK_PREFIX: &str = "expense:back:";
const EXPENSE_CANCEL_PREFIX: &str = "expense:cancel:";
const EXPENSE_EDIT_PREFIX: &str = "expense:edit:";
const EXPENSE_RECORD_PREFIX: &str = "expense:record:";
const EXPENSE_PAYER_SEARCH_MODAL_PREFIX: &str = "expense:payer-search:";
const EXPENSE_MEMBER_SEARCH_MODAL_PREFIX: &str = "expense:member-search:";
const EXPENSE_ROLE_SEARCH_MODAL_PREFIX: &str = "expense:role-search:";
const VOID_SELECT_PREFIX: &str = "void:select:";
const VOID_PREVIOUS_PAGE_PREFIX: &str = "void:page-prev:";
const VOID_NEXT_PAGE_PREFIX: &str = "void:page-next:";
const VOID_NEXT_PREFIX: &str = "void:next:";
const VOID_RESELECT_PREFIX: &str = "void:reselect:";
const VOID_CANCEL_PREFIX: &str = "void:cancel:";
const VOID_CONFIRM_PREFIX: &str = "void:confirm:";
const READ_VIEW_PREVIOUS_PREFIX: &str = "read:view-prev:";
const READ_VIEW_NEXT_PREFIX: &str = "read:view-next:";
const EXPENSE_AMOUNT_FIELD: &str = "expense_amount";
const EXPENSE_NOTE_FIELD: &str = "expense_note";
const EXPENSE_DATE_FIELD: &str = "expense_date";
const EXPENSE_WEIGHTS_FIELD: &str = "expense_weights";
const EXPENSE_SEARCH_FIELD: &str = "expense_search";
pub struct DiscordLedgerPoc {
    interaction_nonce: u64,
    nonce_provider: PocNonceProvider,
    settlement_planner: Arc<dyn SettlementPlanner + Send + Sync>,
    next_session_id: AtomicU64,
    expense_drafts: dashmap::DashMap<u64, ExpenseDraft>,
    expense_defaulted_weight_members: dashmap::DashMap<u64, HashSet<MemberId>>,
    expense_weight_return_to_confirmation: dashmap::DashMap<u64, bool>,
    expense_confirmation_sessions: dashmap::DashMap<u64, ReadViewPageSession>,
    void_drafts: dashmap::DashMap<u64, PendingVoidDraft>,
    settlement_previews: dashmap::DashMap<(u64, u64), PendingSettlementPreview>,
    read_view_sessions: dashmap::DashMap<u64, ReadViewPageSession>,
    runtime_lock: StdMutex<Option<CrossProcessFileLock>>,
    append_locks: dashmap::DashMap<ChannelId, Arc<Mutex<()>>>,
    uncertain_write_ledgers: dashmap::DashMap<ChannelId, SystemTime>,
    panel_state_cache: dashmap::DashMap<ChannelId, bool>,
}

#[derive(Debug, Clone)]
struct ExpenseDraft {
    actor_id: UserId,
    guild_id: GuildId,
    parent_channel_id: ChannelId,
    expires_at: SystemTime,
    amount: Money,
    note: Option<String>,
    effective_date: Option<LedgerEffectiveDate>,
    payer: Option<MemberId>,
    explicit_members: Vec<MemberId>,
    selected_roles: Vec<RoleId>,
    include_members_group: bool,
    active_picker: ExpensePickerKind,
    picker_query: Option<String>,
    picker_page: usize,
    weight_overrides: HashMap<MemberId, Weight>,
    frozen_participants: Option<Vec<ExpenseParticipantSelection>>,
    recording: bool,
}

#[derive(Debug, Clone)]
struct PendingVoidDraft {
    actor_id: UserId,
    guild_id: GuildId,
    ledger_channel_id: ChannelId,
    parent_channel_id: ChannelId,
    success_thread_channel_id: Option<ChannelId>,
    expires_at: SystemTime,
    stage: VoidStage,
    selected_target: Option<LedgerEntryId>,
    current_page: usize,
    selection_snapshot: Vec<VoidSelectionSnapshotRow>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VoidStage {
    Select,
    Confirm,
}

#[derive(Debug, Clone)]
struct VoidSelectionSnapshotRow {
    entry_id: LedgerEntryId,
    select_label: String,
    candidate_row: VoidCandidateRow,
}

#[derive(Debug, Clone)]
struct PendingSettlementPreview {
    ledger_id: LedgerId,
    ledger_channel_id: ChannelId,
    binding: PreviewConfirmationBinding,
    previewed: PreviewedSettlement,
}

#[derive(Debug, Clone)]
struct ReadViewPageSession {
    actor_id: UserId,
    expires_at: SystemTime,
    stale_message: String,
    current_page: usize,
    pages: Vec<ReadViewRenderedPage>,
    bound_expense_session_id: Option<u64>,
}

#[derive(Debug, Clone)]
struct ReadViewRenderedPage {
    body: String,
    base_components: Vec<CreateActionRow>,
}

#[derive(Debug)]
struct PocSystemClock;

impl Clock for PocSystemClock {
    fn now(&self) -> SystemTime {
        SystemTime::now()
    }

    fn today_business_date(&self) -> LedgerEffectiveDate {
        LedgerEffectiveDate::new(today_date().format("%Y-%m-%d").to_string())
            .expect("system business date should be valid")
    }
}

#[derive(Debug)]
struct PocNonceProvider {
    interaction_nonce: InteractionNonce,
    next_preview_instance_id: AtomicU64,
}

impl PocNonceProvider {
    fn new(interaction_nonce: InteractionNonce) -> Self {
        Self {
            interaction_nonce,
            next_preview_instance_id: AtomicU64::new(1),
        }
    }
}

impl NonceProvider for PocNonceProvider {
    fn next_interaction_nonce(&self) -> InteractionNonce {
        self.interaction_nonce
    }

    fn next_preview_instance_id(&self) -> PreviewInstanceId {
        PreviewInstanceId::new(
            self.next_preview_instance_id
                .fetch_add(1, Ordering::Relaxed),
        )
        .expect("preview instance ids should stay non-zero")
    }
}

struct LoadedLedgerThread {
    ledger_id: LedgerId,
    channel_id: ChannelId,
    snapshot: VerifiedLedgerSnapshot,
    verified: Vec<VerifiedLedgerStoreEnvelope<MessageId>>,
    projected: ProjectedLedger,
    entries: Vec<LedgerEntry>,
    transport_entries: HashMap<LedgerEntryId, LoadedTransportEntry>,
}

#[derive(Debug, Clone)]
struct LoadedTransportEntry {
    message_link: String,
    recorded_at: SystemTime,
}

#[derive(Debug, Clone, Default)]
struct ReadViewRenderState {
    uncertain_write: bool,
    stale_page: bool,
    page_indicator: Option<String>,
    snapshot_notice: Option<String>,
}

struct CrossProcessFileLock {
    _file: File,
}

impl Drop for CrossProcessFileLock {
    fn drop(&mut self) {
        let _ = self._file.unlock();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpenseRecordingClaimError {
    Missing,
    Forbidden,
    AlreadyRecording,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AppendEntryFailure {
    Message(String),
    UncertainWrite,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SessionIdMatch {
    Match(u64),
    Stale,
    NoMatch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VoidConfirmMatch {
    Match {
        session_id: u64,
        target: LedgerEntryId,
    },
    Stale,
    NoMatch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpensePickerKind {
    Payer,
    Participants,
    Roles,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpenseEditorStage {
    Payer,
    Participants,
    Weight,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ConfirmedExpenseSelectionsError {
    Missing,
    Drifted(Vec<ExpenseParticipantSelection>),
}

impl DiscordLedgerPoc {
    #[cfg(test)]
    pub fn new() -> Self {
        Self::new_with_planner(Arc::new(HighsSettlementPlanner))
    }

    pub fn new_with_planner(settlement_planner: Arc<dyn SettlementPlanner + Send + Sync>) -> Self {
        let interaction_nonce = InteractionNonce::new(new_interaction_nonce().max(1))
            .expect("interaction nonce should be non-zero");
        Self {
            interaction_nonce: interaction_nonce.get(),
            nonce_provider: PocNonceProvider::new(interaction_nonce),
            settlement_planner,
            next_session_id: AtomicU64::new(1),
            expense_drafts: dashmap::DashMap::new(),
            expense_defaulted_weight_members: dashmap::DashMap::new(),
            expense_weight_return_to_confirmation: dashmap::DashMap::new(),
            expense_confirmation_sessions: dashmap::DashMap::new(),
            void_drafts: dashmap::DashMap::new(),
            settlement_previews: dashmap::DashMap::new(),
            read_view_sessions: dashmap::DashMap::new(),
            runtime_lock: StdMutex::new(None),
            append_locks: dashmap::DashMap::new(),
            uncertain_write_ledgers: dashmap::DashMap::new(),
            panel_state_cache: dashmap::DashMap::new(),
        }
    }

    pub async fn ensure_single_process_runtime(&self) -> Result<(), String> {
        {
            let guard = self
                .runtime_lock
                .lock()
                .map_err(|_| "runtime lock state が壊れています。".to_string())?;
            if guard.is_some() {
                return Ok(());
            }
        }
        let runtime_lock = acquire_cross_process_file_lock(instance_lock_path()).await?;
        let mut guard = self
            .runtime_lock
            .lock()
            .map_err(|_| "runtime lock state が壊れています。".to_string())?;
        if guard.is_none() {
            *guard = Some(runtime_lock);
        }
        Ok(())
    }

    fn prune_stale_state(&self) {
        let now = SystemTime::now();
        let stale_expense_sessions: Vec<u64> = self
            .expense_drafts
            .iter()
            .filter_map(|draft| (draft.expires_at <= now).then_some(*draft.key()))
            .collect();
        for session_id in stale_expense_sessions {
            self.expense_drafts.remove(&session_id);
            self.expense_defaulted_weight_members.remove(&session_id);
            self.expense_weight_return_to_confirmation
                .remove(&session_id);
        }

        let stale_expense_confirmation_sessions: Vec<u64> = self
            .expense_confirmation_sessions
            .iter()
            .filter_map(|session| (session.expires_at <= now).then_some(*session.key()))
            .collect();
        for session_id in stale_expense_confirmation_sessions {
            self.expense_confirmation_sessions.remove(&session_id);
        }

        let stale_void_sessions: Vec<u64> = self
            .void_drafts
            .iter()
            .filter_map(|draft| (draft.expires_at <= now).then_some(*draft.key()))
            .collect();
        for session_id in stale_void_sessions {
            self.void_drafts.remove(&session_id);
        }

        let stale_preview_keys: Vec<(u64, u64)> = self
            .settlement_previews
            .iter()
            .filter_map(|preview| (preview.binding.expires_at() <= now).then_some(*preview.key()))
            .collect();
        for key in stale_preview_keys {
            self.settlement_previews.remove(&key);
        }

        let stale_read_view_sessions: Vec<u64> = self
            .read_view_sessions
            .iter()
            .filter_map(|session| (session.expires_at <= now).then_some(*session.key()))
            .collect();
        for session_id in stale_read_view_sessions {
            self.read_view_sessions.remove(&session_id);
        }

        let stale_uncertain_ledgers: Vec<ChannelId> = self
            .uncertain_write_ledgers
            .iter()
            .filter_map(|entry| (*entry.value() <= now).then_some(*entry.key()))
            .collect();
        for channel_id in stale_uncertain_ledgers {
            self.uncertain_write_ledgers.remove(&channel_id);
        }
    }

    fn is_uncertain_write(&self, channel_id: ChannelId) -> bool {
        self.uncertain_write_ledgers
            .get(&channel_id)
            .is_some_and(|expires_at| *expires_at > SystemTime::now())
    }

    fn mark_uncertain_write(&self, channel_id: ChannelId) {
        self.uncertain_write_ledgers
            .insert(channel_id, interaction_state_expires_at());
    }

    fn preview_instance_id_for(
        &self,
        channel_id: ChannelId,
        actor_id: UserId,
    ) -> Option<PreviewInstanceId> {
        self.settlement_previews
            .get(&(channel_id.get(), actor_id.get()))
            .map(|preview| preview.binding.preview_instance_id())
    }

    fn clear_preview_if_current(
        &self,
        channel_id: ChannelId,
        actor_id: UserId,
        expected_preview_instance_id: Option<PreviewInstanceId>,
    ) {
        let Some(expected_preview_instance_id) = expected_preview_instance_id else {
            return;
        };
        let key = (channel_id.get(), actor_id.get());
        let _ = self.settlement_previews.remove_if(&key, |_, preview| {
            preview.binding.preview_instance_id() == expected_preview_instance_id
        });
    }

    fn uncertain_write_block_message(
        &self,
        preserve_input: bool,
        preserve_preview: bool,
    ) -> String {
        let mut lines = vec![i18n::uncertain_write_block_message().to_owned()];
        if preserve_input {
            lines.push(i18n::uncertain_write_input_preserved_message().to_owned());
        }
        if preserve_preview {
            lines.push(i18n::uncertain_write_preview_preserved_message().to_owned());
        }
        lines.join("\n")
    }

    fn create_read_view_session(
        &self,
        actor_id: UserId,
        stale_message: &str,
        pages: Vec<ReadViewRenderedPage>,
    ) -> Option<u64> {
        if pages.len() <= 1 {
            return None;
        }
        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        self.read_view_sessions.insert(
            session_id,
            ReadViewPageSession {
                actor_id,
                expires_at: interaction_state_expires_at(),
                stale_message: stale_message.to_owned(),
                current_page: 0,
                pages,
                bound_expense_session_id: None,
            },
        );
        Some(session_id)
    }

    fn initial_read_view_page(
        &self,
        actor_id: UserId,
        stale_message: &str,
        pages: Vec<ReadViewRenderedPage>,
    ) -> (String, Vec<CreateActionRow>) {
        if let Some(session_id) =
            self.create_read_view_session(actor_id, stale_message, pages.clone())
            && let Some(session) = self.read_view_sessions.get(&session_id)
        {
            return self.read_view_page_message(session_id, &session);
        }
        let page = pages
            .into_iter()
            .next()
            .expect("read views should render at least one page");
        (page.body, page.base_components)
    }

    fn read_view_page_message(
        &self,
        session_id: u64,
        session: &ReadViewPageSession,
    ) -> (String, Vec<CreateActionRow>) {
        let page = &session.pages[session.current_page];
        let mut components = page.base_components.clone();
        if session.pages.len() > 1 {
            components.push(self.read_view_navigation_row(
                session_id,
                session.current_page,
                session.pages.len(),
            ));
        }
        (page.body.clone(), components)
    }

    fn read_view_navigation_row(
        &self,
        session_id: u64,
        current_page: usize,
        total_pages: usize,
    ) -> CreateActionRow {
        CreateActionRow::Buttons(vec![
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                self.interaction_nonce,
                READ_VIEW_PREVIOUS_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(
                i18n::picker_previous_page_label(),
            ))
            .style(ButtonStyle::Secondary)
            .disabled(current_page == 0),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                self.interaction_nonce,
                READ_VIEW_NEXT_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::picker_next_page_label()))
            .style(ButtonStyle::Secondary)
            .disabled(current_page + 1 >= total_pages),
        ])
    }

    fn create_expense_confirmation_session(
        &self,
        expense_session_id: u64,
        actor_id: UserId,
        pages: Vec<ReadViewRenderedPage>,
    ) -> Option<u64> {
        if pages.len() <= 1 {
            return None;
        }
        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        self.expense_confirmation_sessions.insert(
            session_id,
            ReadViewPageSession {
                actor_id,
                expires_at: interaction_state_expires_at(),
                stale_message: i18n::expense_session_expired_message().to_owned(),
                current_page: 0,
                pages,
                bound_expense_session_id: Some(expense_session_id),
            },
        );
        Some(session_id)
    }

    fn initial_expense_confirmation_page(
        &self,
        expense_session_id: u64,
        actor_id: UserId,
        pages: Vec<ReadViewRenderedPage>,
    ) -> (String, Vec<CreateActionRow>) {
        if let Some(session_id) =
            self.create_expense_confirmation_session(expense_session_id, actor_id, pages.clone())
            && let Some(session) = self.expense_confirmation_sessions.get(&session_id)
        {
            return self.expense_confirmation_page_message(session_id, &session);
        }
        let page = pages
            .into_iter()
            .next()
            .expect("expense confirmation should render at least one page");
        (page.body, page.base_components)
    }

    fn expense_confirmation_page_message(
        &self,
        session_id: u64,
        session: &ReadViewPageSession,
    ) -> (String, Vec<CreateActionRow>) {
        let page = &session.pages[session.current_page];
        let mut components = page.base_components.clone();
        if session.pages.len() > 1 {
            components.push(self.expense_confirmation_navigation_row(
                session_id,
                session.current_page,
                session.pages.len(),
            ));
        }
        (page.body.clone(), components)
    }

    fn clear_expense_confirmation_sessions_for_draft(&self, expense_session_id: u64) {
        let stale_session_ids = self
            .expense_confirmation_sessions
            .iter()
            .filter(|entry| entry.value().bound_expense_session_id == Some(expense_session_id))
            .map(|entry| *entry.key())
            .collect::<Vec<_>>();
        for session_id in stale_session_ids {
            self.expense_confirmation_sessions.remove(&session_id);
        }
    }

    fn expense_confirmation_navigation_row(
        &self,
        session_id: u64,
        current_page: usize,
        total_pages: usize,
    ) -> CreateActionRow {
        CreateActionRow::Buttons(vec![
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                self.interaction_nonce,
                EXPENSE_CONFIRM_PREVIOUS_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(
                i18n::picker_previous_page_label(),
            ))
            .style(ButtonStyle::Secondary)
            .disabled(current_page == 0),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                self.interaction_nonce,
                EXPENSE_CONFIRM_NEXT_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::picker_next_page_label()))
            .style(ButtonStyle::Secondary)
            .disabled(current_page + 1 >= total_pages),
        ])
    }

    pub async fn handle_command<RP>(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        roster_provider: &RP,
    ) -> bool
    where
        RP: crate::discord::ports::RosterProvider,
    {
        self.prune_stale_state();
        match command.data.name.as_str() {
            "panel" => {
                self.post_panel(ctx, command).await;
                true
            }
            "expense" => {
                self.start_expense(ctx, command).await;
                true
            }
            "ledger" => {
                self.show_ledger(ctx, command, roster_provider).await;
                true
            }
            "settle" => {
                self.confirm_settlement(ctx, command, roster_provider).await;
                true
            }
            "void" => {
                self.start_void(ctx, command, roster_provider).await;
                true
            }
            _ => false,
        }
    }

    pub async fn handle_review_command<RP>(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        roster_provider: &RP,
    ) -> bool
    where
        RP: crate::discord::ports::RosterProvider,
    {
        self.prune_stale_state();
        let Some(guild_id) = command.guild_id else {
            return false;
        };
        let channel_id = match self.lookup_parent_channel_id(ctx, command.channel_id).await {
            Ok(channel_id) => channel_id,
            Err(_) => return false,
        };
        let prior_preview_instance_id = self.preview_instance_id_for(channel_id, command.user.id);
        let render_state = ReadViewRenderState {
            uncertain_write: self.is_uncertain_write(channel_id),
            ..ReadViewRenderState::default()
        };
        let _ = command.defer_ephemeral(&ctx.http).await;
        let loaded = match self
            .load_ledger(ctx, channel_id, projection::CanonicalLoadRoute::Preview)
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.clear_preview_if_current(
                    channel_id,
                    command.user.id,
                    prior_preview_instance_id,
                );
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return true;
            }
        };
        if loaded.entries.is_empty() {
            self.clear_preview_if_current(channel_id, command.user.id, prior_preview_instance_id);
            let (body, components) = render_review_empty_state(
                ReadViewRoute::ReviewThread,
                channel_link(guild_id, channel_id),
                &render_state,
            )
            .unwrap_or_else(|message| (message, Vec::new()));
            self.edit_command_message(ctx, command, body, components)
                .await;
            return true;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let previewed = match preview_settlement_from_snapshot(
            &loaded.snapshot,
            MemberId(command.user.id.get()),
            self.settlement_planner.as_ref(),
            &PocSystemClock,
        ) {
            Ok(PreviewedSettlementOutcome::RecordablePreview { previewed, .. }) => previewed,
            Ok(PreviewedSettlementOutcome::NoTransfersNeeded) => {
                self.clear_preview_if_current(
                    channel_id,
                    command.user.id,
                    prior_preview_instance_id,
                );
                let (body, components) = render_review_no_transfer_state(&render_state)
                    .unwrap_or_else(|message| (message, Vec::new()));
                self.edit_command_message(ctx, command, body, components)
                    .await;
                return true;
            }
            Err(_) => {
                self.clear_preview_if_current(
                    channel_id,
                    command.user.id,
                    prior_preview_instance_id,
                );
                self.edit_command_message(
                    ctx,
                    command,
                    i18n::review_render_failed_message(),
                    Vec::new(),
                )
                .await;
                return true;
            }
        };
        let binding = create_preview_binding(
            loaded.ledger_id,
            current_head_hash(loaded.ledger_id, &loaded.verified),
            MemberId(command.user.id.get()),
            &previewed,
            &PocSystemClock,
            &self.nonce_provider,
        );
        let append_lock = self.append_lock(loaded.channel_id);
        let _append_guard = append_lock.lock().await;
        self.settlement_previews.insert(
            (loaded.channel_id.get(), command.user.id.get()),
            PendingSettlementPreview {
                ledger_id: loaded.ledger_id,
                ledger_channel_id: loaded.channel_id,
                binding: binding.clone(),
                previewed: previewed.clone(),
            },
        );
        let pages = match render_review_surface_pages(
            &loaded,
            &previewed,
            &member_names,
            ReadViewRoute::ReviewThread,
            RecoveryCta::ParentLink,
            Some(channel_link(guild_id, channel_id)),
            &render_state,
        ) {
            Ok(rendered) => rendered,
            Err(message) => {
                self.clear_preview_if_current(
                    loaded.channel_id,
                    command.user.id,
                    Some(binding.preview_instance_id()),
                );
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return true;
            }
        };
        let (body, components) =
            self.initial_read_view_page(command.user.id, i18n::stale_review_page_message(), pages);
        if self
            .edit_command_message(ctx, command, body, components)
            .await
            && let Some(mut preview) = self
                .settlement_previews
                .get_mut(&(loaded.channel_id.get(), command.user.id.get()))
        {
            preview.binding = delivered_preview_binding(&binding);
        }
        true
    }

    pub async fn handle_component<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        roster_provider: &RP,
    ) -> bool
    where
        RP: crate::discord::ports::RosterProvider,
    {
        self.prune_stale_state();
        let custom_id = component.data.custom_id.as_str();

        if custom_id == LEDGER_PANEL_EXPENSE_ID {
            self.start_expense_from_component(ctx, component).await;
            return true;
        }
        if custom_id == LEDGER_PANEL_REVIEW_ID {
            self.show_review_from_component(ctx, component, roster_provider)
                .await;
            return true;
        }
        if custom_id == LEDGER_PANEL_LEDGER_ID {
            self.show_ledger_from_component(ctx, component, roster_provider)
                .await;
            return true;
        }
        if custom_id == LEDGER_PANEL_VOID_ID {
            self.start_void_from_component(ctx, component, roster_provider)
                .await;
            return true;
        }
        match self
            .component_session_id(ctx, component, custom_id, READ_VIEW_PREVIOUS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.navigate_read_view_page(ctx, component, session_id, false)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, READ_VIEW_NEXT_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.navigate_read_view_page(ctx, component, session_id, true)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }

        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_PICKER_PAYER_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.switch_expense_picker(
                    ctx,
                    component,
                    session_id,
                    ExpensePickerKind::Payer,
                    roster_provider,
                )
                .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(
                ctx,
                component,
                custom_id,
                EXPENSE_PICKER_PARTICIPANTS_PREFIX,
            )
            .await
        {
            Ok(Some(session_id)) => {
                self.switch_expense_picker(
                    ctx,
                    component,
                    session_id,
                    ExpensePickerKind::Participants,
                    roster_provider,
                )
                .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_PICKER_ROLES_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.switch_expense_picker(
                    ctx,
                    component,
                    session_id,
                    ExpensePickerKind::Roles,
                    roster_provider,
                )
                .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_PICKER_PREVIOUS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.move_expense_picker_page(ctx, component, session_id, false, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_PICKER_NEXT_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.move_expense_picker_page(ctx, component, session_id, true, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_PICKER_SEARCH_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.open_expense_search_modal(ctx, component, session_id)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_CLEAR_PAYER_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.clear_expense_payer(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_CLEAR_PARTICIPANTS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.clear_expense_participants(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }

        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_PAYER_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.update_expense_payer(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_PARTICIPANTS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.update_expense_participants(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_ROLES_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.update_expense_roles(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_CLEAR_ROLES_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.clear_expense_roles(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_TOGGLE_MEMBERS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.toggle_expense_members_group(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_OPEN_WEIGHTS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.open_weights_modal(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_RESET_WEIGHTS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.reset_expense_weights_from_editor(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_CONFIRM_WEIGHTS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.open_weights_modal_from_confirmation(
                    ctx,
                    component,
                    session_id,
                    roster_provider,
                )
                .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(
                ctx,
                component,
                custom_id,
                EXPENSE_CONFIRM_RESET_WEIGHTS_PREFIX,
            )
            .await
        {
            Ok(Some(session_id)) => {
                self.reset_expense_weights_from_confirmation(
                    ctx,
                    component,
                    session_id,
                    roster_provider,
                )
                .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_CONFIRM_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.show_expense_confirmation(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_CONFIRM_BACK_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.return_to_expense_weight_stage(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_CONFIRM_PREVIOUS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.navigate_expense_confirmation_page(ctx, component, session_id, false)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_CONFIRM_NEXT_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.navigate_expense_confirmation_page(ctx, component, session_id, true)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_REVISE_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.return_to_expense_editor(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_NEXT_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.advance_expense_to_participants(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_BACK_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.return_to_previous_expense_stage(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_CANCEL_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.cancel_expense_flow(ctx, component, session_id).await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_EDIT_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.open_expense_basic_info_modal(ctx, component, session_id)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, EXPENSE_RECORD_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.record_expense(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, VOID_SELECT_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.update_void_selection(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, VOID_CANCEL_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.cancel_void_flow(ctx, component, session_id).await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, VOID_PREVIOUS_PAGE_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.navigate_void_selection_page(ctx, component, session_id, false)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, VOID_NEXT_PAGE_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.navigate_void_selection_page(ctx, component, session_id, true)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, VOID_NEXT_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.show_void_confirmation(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .component_session_id(ctx, component, custom_id, VOID_RESELECT_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.return_to_void_selection(ctx, component, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self.void_confirm_payload(ctx, component, custom_id).await {
            Ok(Some((session_id, target))) => {
                self.confirm_void(ctx, component, session_id, target, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }

        false
    }

    pub async fn handle_modal<RP>(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        roster_provider: &RP,
    ) -> bool
    where
        RP: crate::discord::ports::RosterProvider,
    {
        self.prune_stale_state();
        let custom_id = modal.data.custom_id.as_str();
        match self
            .modal_session_id(ctx, modal, custom_id, EXPENSE_MODAL_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.complete_expense_modal(ctx, modal, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .modal_session_id(ctx, modal, custom_id, EXPENSE_WEIGHTS_MODAL_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.complete_weights_modal(ctx, modal, session_id, roster_provider)
                    .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .modal_session_id(ctx, modal, custom_id, EXPENSE_PAYER_SEARCH_MODAL_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.complete_expense_search_modal(
                    ctx,
                    modal,
                    session_id,
                    ExpensePickerKind::Payer,
                    roster_provider,
                )
                .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .modal_session_id(ctx, modal, custom_id, EXPENSE_MEMBER_SEARCH_MODAL_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.complete_expense_search_modal(
                    ctx,
                    modal,
                    session_id,
                    ExpensePickerKind::Participants,
                    roster_provider,
                )
                .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        match self
            .modal_session_id(ctx, modal, custom_id, EXPENSE_ROLE_SEARCH_MODAL_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.complete_expense_search_modal(
                    ctx,
                    modal,
                    session_id,
                    ExpensePickerKind::Roles,
                    roster_provider,
                )
                .await;
                return true;
            }
            Err(()) => return true,
            Ok(None) => {}
        }
        false
    }

    async fn start_expense(&self, ctx: &Context, command: &CommandInteraction) {
        let Some(guild_id) = command.guild_id else {
            let _ = command
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::guild_only_channel_message()),
                    ),
                )
                .await;
            return;
        };
        let channel_id = match self.lookup_parent_channel_id(ctx, command.channel_id).await {
            Ok(channel_id) => channel_id,
            Err(message) => {
                let _ = command
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::Message(
                            safe_interaction_response_message()
                                .ephemeral(true)
                                .content(message),
                        ),
                    )
                    .await;
                return;
            }
        };
        if self.is_uncertain_write(channel_id) {
            let _ = command
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(self.uncertain_write_block_message(false, false)),
                    ),
                )
                .await;
            return;
        }

        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        let modal = expense_modal(self.interaction_nonce, session_id, None);

        let _ = guild_id; // kept for symmetry with later draft creation
        let _ = command
            .create_response(&ctx.http, CreateInteractionResponse::Modal(modal))
            .await;
    }

    async fn start_expense_from_component(&self, ctx: &Context, component: &ComponentInteraction) {
        if component.guild_id.is_none() {
            self.reply_component_error(ctx, component, i18n::guild_only_channel_message())
                .await;
            return;
        }
        let channel_id = match self
            .lookup_parent_channel_id(ctx, component.channel_id)
            .await
        {
            Ok(channel_id) => channel_id,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };
        if self.is_uncertain_write(channel_id) {
            self.reply_component_error(
                ctx,
                component,
                &self.uncertain_write_block_message(false, false),
            )
            .await;
            return;
        }

        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        let modal = expense_modal(self.interaction_nonce, session_id, None);
        let _ = component
            .create_response(&ctx.http, CreateInteractionResponse::Modal(modal))
            .await;
    }

    async fn post_panel(&self, ctx: &Context, command: &CommandInteraction) {
        let Some(guild_id) = command.guild_id else {
            let _ = command
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::guild_only_channel_message()),
                    ),
                )
                .await;
            return;
        };

        let _ = command.defer_ephemeral(&ctx.http).await;
        let channel_id = match self.lookup_parent_channel_id(ctx, command.channel_id).await {
            Ok(channel_id) => channel_id,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };
        let canonical_thread_id = self
            .verified_canonical_thread_id_for_panel(ctx, guild_id, channel_id)
            .await;
        let (body, components) = match render_panel_post_message(
            canonical_thread_id,
            self.is_uncertain_write(channel_id),
        ) {
            Ok(message) => message,
            Err(message) => {
                let (body, components) = message.into_parts();
                self.edit_command_message(ctx, command, body, components)
                    .await;
                return;
            }
        };

        match channel_id
            .send_message(
                &ctx.http,
                safe_create_message().content(body).components(components),
            )
            .await
        {
            Ok(_) => {
                self.edit_command_message(ctx, command, i18n::panel_posted_message(), Vec::new())
                    .await;
            }
            Err(error) => {
                self.edit_command_message(
                    ctx,
                    command,
                    format!("操作パネルを投稿できませんでした: {error:?}"),
                    Vec::new(),
                )
                .await;
            }
        }
    }

    async fn show_review_from_component<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(guild_id) = component.guild_id else {
            self.reply_component_error(ctx, component, "このボタンはサーバー内でのみ使えます。")
                .await;
            return;
        };
        self.defer_ephemeral_component(ctx, component).await;
        let channel_id = match self
            .lookup_parent_channel_id(ctx, component.channel_id)
            .await
        {
            Ok(channel_id) => channel_id,
            Err(message) => {
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let prior_preview_instance_id = self.preview_instance_id_for(channel_id, component.user.id);
        let render_state = ReadViewRenderState {
            uncertain_write: self.is_uncertain_write(channel_id),
            ..ReadViewRenderState::default()
        };
        let loaded = match self
            .load_ledger(ctx, channel_id, projection::CanonicalLoadRoute::Preview)
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.clear_preview_if_current(
                    channel_id,
                    component.user.id,
                    prior_preview_instance_id,
                );
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        if loaded.entries.is_empty() {
            self.clear_preview_if_current(channel_id, component.user.id, prior_preview_instance_id);
            let (body, components) = render_review_empty_state(
                ReadViewRoute::ReviewParent,
                channel_link(guild_id, loaded.channel_id),
                &render_state,
            )
            .unwrap_or_else(|message| (message, Vec::new()));
            self.edit_component_message(ctx, component, body, components)
                .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let previewed = match preview_settlement_from_snapshot(
            &loaded.snapshot,
            MemberId(component.user.id.get()),
            self.settlement_planner.as_ref(),
            &PocSystemClock,
        ) {
            Ok(PreviewedSettlementOutcome::RecordablePreview { previewed, .. }) => previewed,
            Ok(PreviewedSettlementOutcome::NoTransfersNeeded) => {
                self.clear_preview_if_current(
                    channel_id,
                    component.user.id,
                    prior_preview_instance_id,
                );
                let (body, components) = render_review_no_transfer_state(&render_state)
                    .unwrap_or_else(|message| (message, Vec::new()));
                self.edit_component_message(ctx, component, body, components)
                    .await;
                return;
            }
            Err(_) => {
                self.clear_preview_if_current(
                    channel_id,
                    component.user.id,
                    prior_preview_instance_id,
                );
                self.edit_component_message(
                    ctx,
                    component,
                    i18n::review_render_failed_message(),
                    Vec::new(),
                )
                .await;
                return;
            }
        };
        let binding = create_preview_binding(
            loaded.ledger_id,
            current_head_hash(loaded.ledger_id, &loaded.verified),
            MemberId(component.user.id.get()),
            &previewed,
            &PocSystemClock,
            &self.nonce_provider,
        );
        let append_lock = self.append_lock(loaded.channel_id);
        let _append_guard = append_lock.lock().await;
        self.settlement_previews.insert(
            (loaded.channel_id.get(), component.user.id.get()),
            PendingSettlementPreview {
                ledger_id: loaded.ledger_id,
                ledger_channel_id: loaded.channel_id,
                binding: binding.clone(),
                previewed: previewed.clone(),
            },
        );
        let (recovery_cta, recovery_url) = self
            .review_parent_recovery_cta_and_url(
                ctx,
                guild_id,
                channel_id,
                loaded.channel_id,
                component.member.as_ref(),
            )
            .await;
        let pages = match render_review_surface_pages(
            &loaded,
            &previewed,
            &member_names,
            ReadViewRoute::ReviewParent,
            recovery_cta,
            recovery_url,
            &render_state,
        ) {
            Ok(rendered) => rendered,
            Err(message) => {
                self.clear_preview_if_current(
                    loaded.channel_id,
                    component.user.id,
                    Some(binding.preview_instance_id()),
                );
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let (body, components) = self.initial_read_view_page(
            component.user.id,
            i18n::stale_review_page_message(),
            pages,
        );
        if self
            .edit_component_message(ctx, component, body, components)
            .await
            && let Some(mut preview) = self
                .settlement_previews
                .get_mut(&(loaded.channel_id.get(), component.user.id.get()))
        {
            preview.binding = delivered_preview_binding(&binding);
        }
    }

    async fn show_ledger_from_component<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(guild_id) = component.guild_id else {
            self.reply_component_error(ctx, component, "このボタンはサーバー内でのみ使えます。")
                .await;
            return;
        };
        self.defer_ephemeral_component(ctx, component).await;
        let channel_id = match self
            .lookup_parent_channel_id(ctx, component.channel_id)
            .await
        {
            Ok(channel_id) => channel_id,
            Err(message) => {
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let loaded = match self
            .load_ledger(ctx, channel_id, projection::CanonicalLoadRoute::Read)
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let render_state = ReadViewRenderState {
            uncertain_write: self.is_uncertain_write(channel_id),
            ..ReadViewRenderState::default()
        };
        if loaded.entries.is_empty() {
            self.edit_component_message(
                ctx,
                component,
                render_ledger_empty_state(ReadViewRoute::LedgerPanel, &render_state)
                    .unwrap_or_else(|message| message),
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let pages = match render_ledger_surface_pages(
            &loaded,
            &member_names,
            ReadViewRoute::LedgerPanel,
            &render_state,
        ) {
            Ok(rendered) => rendered,
            Err(message) => {
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let (body, components) = self.initial_read_view_page(
            component.user.id,
            i18n::stale_ledger_page_message(),
            pages,
        );
        self.edit_component_message(ctx, component, body, components)
            .await;
    }

    async fn navigate_read_view_page(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        forward: bool,
    ) {
        let fallback_stale_message = stale_read_view_message_for_body(&component.message.content);
        let Some(mut session) = self.read_view_sessions.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, fallback_stale_message)
                .await;
            return;
        };
        if session.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if session.expires_at <= SystemTime::now() {
            let stale_message = session.stale_message.clone();
            drop(session);
            self.read_view_sessions.remove(&session_id);
            self.reply_component_error(ctx, component, &stale_message)
                .await;
            return;
        }

        if forward {
            session.current_page = (session.current_page + 1).min(session.pages.len() - 1);
        } else {
            session.current_page = session.current_page.saturating_sub(1);
        }
        let (body, components) = self.read_view_page_message(session_id, &session);
        drop(session);
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await;
    }

    async fn navigate_expense_confirmation_page(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        forward: bool,
    ) {
        let Some(mut session) = self.expense_confirmation_sessions.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, i18n::expense_session_expired_message())
                .await;
            return;
        };
        if session.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if session.expires_at <= SystemTime::now() {
            drop(session);
            self.expense_confirmation_sessions.remove(&session_id);
            self.reply_component_error(ctx, component, i18n::expense_session_expired_message())
                .await;
            return;
        }
        if let Some(expense_session_id) = session.bound_expense_session_id
            && !self.expense_drafts.contains_key(&expense_session_id)
        {
            drop(session);
            self.expense_confirmation_sessions.remove(&session_id);
            self.reply_component_error(ctx, component, i18n::expense_session_expired_message())
                .await;
            return;
        }

        if forward {
            session.current_page = (session.current_page + 1).min(session.pages.len() - 1);
        } else {
            session.current_page = session.current_page.saturating_sub(1);
        }
        let (body, components) = self.expense_confirmation_page_message(session_id, &session);
        drop(session);
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await;
    }

    async fn navigate_void_selection_page(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        forward: bool,
    ) {
        let Some(mut draft) = self.void_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, i18n::stale_void_page_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if draft.expires_at <= SystemTime::now() {
            drop(draft);
            self.void_drafts.remove(&session_id);
            self.reply_component_error(ctx, component, i18n::stale_void_page_message())
                .await;
            return;
        }
        if draft.stage != VoidStage::Select {
            self.reply_component_error(ctx, component, i18n::stale_void_page_message())
                .await;
            return;
        }
        let total_pages = match void_selection_page_ranges(&draft.selection_snapshot) {
            Ok(page_ranges) => page_ranges.len(),
            Err(message) => {
                drop(draft);
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };
        if forward {
            draft.current_page = (draft.current_page + 1).min(total_pages.saturating_sub(1));
        } else {
            draft.current_page = draft.current_page.saturating_sub(1);
        }
        let render_draft = draft.clone();
        drop(draft);

        let (body, components) = match render_void_selection_surface(
            &render_draft,
            session_id,
            self.interaction_nonce,
            VoidSelectionBodyKind::Normal,
        ) {
            Ok(surface) => surface,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await;
    }

    async fn start_void_from_component<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(guild_id) = component.guild_id else {
            self.reply_component_error(ctx, component, "このボタンはサーバー内でのみ使えます。")
                .await;
            return;
        };
        self.defer_ephemeral_component(ctx, component).await;
        let channel_id = match self
            .lookup_parent_channel_id(ctx, component.channel_id)
            .await
        {
            Ok(channel_id) => channel_id,
            Err(message) => {
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let success_thread_channel_id = self
            .canonical_thread_id_for_void_flow(ctx, guild_id, component.channel_id, channel_id)
            .await;
        if self.is_uncertain_write(channel_id) {
            self.edit_component_message(
                ctx,
                component,
                self.uncertain_write_block_message(false, false),
                Vec::new(),
            )
            .await;
            return;
        }
        let loaded = match self
            .load_ledger(
                ctx,
                channel_id,
                projection::CanonicalLoadRoute::WritePrelude,
            )
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        if loaded.entries.is_empty() {
            self.edit_component_message(
                ctx,
                component,
                render_void_surface_model(&VoidSurfaceModel::empty(
                    i18n::panel_void_button_label(),
                    Vec::new(),
                    true,
                ))
                .unwrap_or_else(|message| message),
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let all_candidates = candidate_entries_for_void(&loaded.entries, &loaded.projected);
        let candidates = void_window_entries(&loaded.entries, &loaded.projected);
        if candidates.is_empty() {
            self.edit_component_message(
                ctx,
                component,
                if let Some(target) = all_candidates.first().map(|entry| entry.id) {
                    render_void_older_than_window_body(&loaded, target)
                } else {
                    render_void_surface_model(&VoidSurfaceModel::no_candidates(
                        i18n::panel_void_button_label(),
                        Vec::new(),
                        true,
                    ))
                }
                .unwrap_or_else(|message| message),
                Vec::new(),
            )
            .await;
            return;
        }
        let selection_snapshot =
            match void_selection_snapshot_rows(&loaded, &candidates, &member_names) {
                Ok(snapshot) => snapshot,
                Err(message) => {
                    self.edit_component_message(ctx, component, message, Vec::new())
                        .await;
                    return;
                }
            };
        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        self.void_drafts.insert(
            session_id,
            PendingVoidDraft {
                actor_id: component.user.id,
                guild_id,
                ledger_channel_id: channel_id,
                parent_channel_id: channel_id,
                success_thread_channel_id,
                expires_at: interaction_state_expires_at(),
                stage: VoidStage::Select,
                selected_target: None,
                current_page: 0,
                selection_snapshot,
            },
        );
        let draft = self
            .void_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
            .expect("void draft should remain available");
        let (body, components) = match render_void_selection_surface(
            &draft,
            session_id,
            self.interaction_nonce,
            VoidSelectionBodyKind::Normal,
        ) {
            Ok(surface) => surface,
            Err(message) => {
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        self.edit_component_message(ctx, component, body, components)
            .await;
    }

    async fn complete_expense_modal<RP>(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(guild_id) = modal.guild_id else {
            return;
        };
        let values = modal_values(modal);
        let Some(amount_text) = values.get(EXPENSE_AMOUNT_FIELD) else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::expense_missing_amount_message()),
                    ),
                )
                .await;
            return;
        };

        let Ok(amount) = parse_amount(amount_text) else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::expense_invalid_amount_message()),
                    ),
                )
                .await;
            return;
        };

        let effective_date = match values.get(EXPENSE_DATE_FIELD).map(String::as_str) {
            Some("") | None => None,
            Some(raw) => {
                let normalized = normalize_date_input(raw, today_date())
                    .and_then(|d| LedgerEffectiveDate::new(d.format("%Y-%m-%d").to_string()).ok());
                match normalized {
                    Some(date) => Some(date),
                    None => {
                        let _ = modal
                            .create_response(
                                &ctx.http,
                                CreateInteractionResponse::Message(
                                    safe_interaction_response_message()
                                        .ephemeral(true)
                                        .content(i18n::expense_invalid_date_message()),
                                ),
                            )
                            .await;
                        return;
                    }
                }
            }
        };
        let parent_channel_id = match self.lookup_parent_channel_id(ctx, modal.channel_id).await {
            Ok(parent_channel_id) => parent_channel_id,
            Err(message) => {
                let _ = modal
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::Message(
                            safe_interaction_response_message()
                                .ephemeral(true)
                                .content(message),
                        ),
                    )
                    .await;
                return;
            }
        };
        if let Some(existing) = self.expense_drafts.get(&session_id)
            && existing.parent_channel_id != parent_channel_id
        {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::stale_interaction_message()),
                    ),
                )
                .await;
            return;
        }

        let roster = match roster_provider
            .roster_for_channel(ctx, parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                let _ = modal
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::Message(
                            safe_interaction_response_message()
                                .ephemeral(true)
                                .content(i18n::member_roster_load_failed_message()),
                        ),
                    )
                    .await;
                return;
            }
        };

        let draft = if let Some(existing) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        {
            ExpenseDraft {
                actor_id: existing.actor_id,
                guild_id,
                parent_channel_id: existing.parent_channel_id,
                expires_at: interaction_state_expires_at(),
                amount,
                note: values
                    .get(EXPENSE_NOTE_FIELD)
                    .and_then(|value| non_empty_trimmed(value)),
                effective_date,
                payer: existing.payer,
                explicit_members: existing.explicit_members,
                selected_roles: existing.selected_roles,
                include_members_group: existing.include_members_group,
                active_picker: existing.active_picker,
                picker_query: existing.picker_query,
                picker_page: existing.picker_page,
                weight_overrides: existing.weight_overrides,
                frozen_participants: None,
                recording: false,
            }
        } else {
            ExpenseDraft {
                actor_id: modal.user.id,
                guild_id,
                parent_channel_id,
                expires_at: interaction_state_expires_at(),
                amount,
                note: values
                    .get(EXPENSE_NOTE_FIELD)
                    .and_then(|value| non_empty_trimmed(value)),
                effective_date,
                payer: Some(MemberId(modal.user.id.get())),
                explicit_members: vec![MemberId(modal.user.id.get())],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Payer,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: None,
                recording: false,
            }
        };
        self.expense_drafts.insert(session_id, draft.clone());

        let member_names =
            roster_provider.display_names_for_guild(guild_id, roster.member_ids.iter().copied());
        let role_labels = roster_role_labels(ctx, guild_id, &roster);
        let response = match self.validated_expense_editor_content(
            session_id,
            &roster,
            &member_names,
            &role_labels,
        ) {
            Ok(content) => safe_interaction_response_message()
                .ephemeral(true)
                .content(content)
                .components(expense_editor_components(
                    session_id,
                    self.interaction_nonce,
                    &draft,
                    &roster,
                    &member_names,
                    &role_labels,
                )),
            Err(message) => safe_interaction_response_message()
                .ephemeral(true)
                .content(message),
        };
        let _ = modal
            .create_response(&ctx.http, CreateInteractionResponse::Message(response))
            .await;
    }

    async fn update_expense_payer<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if let ComponentInteractionDataKind::StringSelect { values } = &component.data.kind {
            draft.payer = values
                .first()
                .and_then(|value| value.parse::<u64>().ok())
                .map(MemberId);
        }
        draft.frozen_participants = None;
        drop(draft);
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn update_expense_participants<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if let ComponentInteractionDataKind::StringSelect { values } = &component.data.kind {
            draft.explicit_members = values
                .iter()
                .filter_map(|value| value.parse::<u64>().ok())
                .map(MemberId)
                .collect();
            let explicit_members = draft.explicit_members.clone();
            for member_id in explicit_members {
                draft.weight_overrides.entry(member_id).or_insert(Weight(1));
            }
        }
        draft.frozen_participants = None;
        drop(draft);
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn update_expense_roles<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if let ComponentInteractionDataKind::StringSelect { values } = &component.data.kind {
            draft.selected_roles = values
                .iter()
                .filter_map(|value| value.parse::<u64>().ok())
                .map(RoleId)
                .collect();
        }
        draft.frozen_participants = None;
        drop(draft);
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn clear_expense_roles<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        draft.selected_roles.clear();
        draft.frozen_participants = None;
        drop(draft);
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn clear_expense_payer<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.payer = None;
            draft.frozen_participants = None;
        }
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn clear_expense_participants<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.explicit_members.clear();
            draft.frozen_participants = None;
        }
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn switch_expense_picker<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        picker_kind: ExpensePickerKind,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.active_picker = picker_kind;
            draft.picker_query = None;
            draft.picker_page = 0;
            draft.frozen_participants = None;
        }
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn advance_expense_to_participants<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.active_picker = ExpensePickerKind::Participants;
            draft.picker_query = None;
            draft.picker_page = 0;
            draft.frozen_participants = None;
        }
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn return_to_previous_expense_stage<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        match expense_editor_stage(&draft) {
            ExpenseEditorStage::Payer => {
                self.open_expense_basic_info_modal(ctx, component, session_id)
                    .await;
            }
            ExpenseEditorStage::Participants => {
                if let Some(mut stored) = self.expense_drafts.get_mut(&session_id) {
                    stored.active_picker = ExpensePickerKind::Payer;
                    stored.picker_query = None;
                    stored.picker_page = 0;
                    stored.frozen_participants = None;
                }
                self.update_expense_editor_message(ctx, component, session_id, roster_provider)
                    .await;
            }
            ExpenseEditorStage::Weight => {
                if let Some(mut stored) = self.expense_drafts.get_mut(&session_id) {
                    stored.frozen_participants = None;
                }
                self.update_expense_editor_message(ctx, component, session_id, roster_provider)
                    .await;
            }
        }
    }

    async fn return_to_expense_weight_stage<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if draft.frozen_participants.is_none() {
            let roster = match roster_provider
                .roster_for_channel(ctx, draft.parent_channel_id)
                .await
            {
                Ok(roster) => roster,
                Err(_) => {
                    self.reply_component_error(
                        ctx,
                        component,
                        i18n::member_roster_load_failed_message(),
                    )
                    .await;
                    return;
                }
            };
            if let Some(mut stored) = self.expense_drafts.get_mut(&session_id) {
                stored.active_picker = ExpensePickerKind::Participants;
                stored.frozen_participants = Some(resolve_expense_selections(&stored, &roster));
            }
        }
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn move_expense_picker_page<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        forward: bool,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            if forward {
                draft.picker_page = draft.picker_page.saturating_add(1);
            } else {
                draft.picker_page = draft.picker_page.saturating_sub(1);
            }
        }
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn open_expense_search_modal(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
    ) {
        let Some(draft) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let modal = expense_search_modal(
            self.interaction_nonce,
            session_id,
            draft.active_picker,
            draft.picker_query.as_deref(),
        );
        let _ = component
            .create_response(&ctx.http, CreateInteractionResponse::Modal(modal))
            .await;
    }

    async fn complete_expense_search_modal<RP>(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        session_id: u64,
        picker_kind: ExpensePickerKind,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let values = modal_values(modal);
        let Some(existing) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::expense_draft_missing_message()),
                    ),
                )
                .await;
            return;
        };
        if existing.actor_id != modal.user.id {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::initiator_only_continuation_message()),
                    ),
                )
                .await;
            return;
        }
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.active_picker = picker_kind;
        }
        let draft = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
            .expect("draft should remain available");
        let roster = match roster_provider
            .roster_for_channel(ctx, draft.parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                let _ = modal
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::Message(
                            safe_interaction_response_message()
                                .ephemeral(true)
                                .content(i18n::member_roster_load_failed_message()),
                        ),
                    )
                    .await;
                return;
            }
        };
        let member_names = roster_provider
            .display_names_for_guild(draft.guild_id, roster.member_ids.iter().copied());
        let role_labels = roster_role_labels(ctx, draft.guild_id, &roster);
        let Some(raw_query) = values.get(EXPENSE_SEARCH_FIELD) else {
            return;
        };
        let Some(query) = non_empty_trimmed(raw_query) else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::search_blank_error()),
                    ),
                )
                .await;
            return;
        };
        let matched_page =
            picker_search_target_page(picker_kind, &roster, &member_names, &role_labels, &query);
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.active_picker = picker_kind;
            draft.picker_query = Some(query.clone());
            if let Some(page) = matched_page {
                draft.picker_page = page;
            }
            draft.frozen_participants = None;
        }
        let draft = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
            .expect("draft should remain available");
        let response = match self.validated_expense_editor_content(
            session_id,
            &roster,
            &member_names,
            &role_labels,
        ) {
            Ok(content) => safe_interaction_response_message()
                .ephemeral(true)
                .content(content)
                .components(expense_editor_components(
                    session_id,
                    self.interaction_nonce,
                    &draft,
                    &roster,
                    &member_names,
                    &role_labels,
                )),
            Err(message) => safe_interaction_response_message()
                .ephemeral(true)
                .content(message),
        };
        let _ = modal
            .create_response(&ctx.http, CreateInteractionResponse::Message(response))
            .await;
    }

    async fn open_expense_basic_info_modal(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
    ) {
        let Some(draft) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let modal = expense_modal(self.interaction_nonce, session_id, Some(&draft));
        let _ = component
            .create_response(&ctx.http, CreateInteractionResponse::Modal(modal))
            .await;
    }

    async fn cancel_expense_flow(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
    ) {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        self.expense_drafts.remove(&session_id);
        self.clear_expense_confirmation_sessions_for_draft(session_id);
        self.expense_defaulted_weight_members.remove(&session_id);
        self.expense_weight_return_to_confirmation
            .remove(&session_id);
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(i18n::expense_cancelled_message())
                        .components(Vec::new()),
                ),
            )
            .await;
    }

    async fn cancel_void_flow(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
    ) {
        let Some(actor_id) = self
            .void_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::void_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        self.void_drafts.remove(&session_id);
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(i18n::void_cancelled_message())
                        .components(Vec::new()),
                ),
            )
            .await;
    }

    async fn toggle_expense_members_group<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        draft.include_members_group = !draft.include_members_group;
        draft.frozen_participants = None;
        drop(draft);
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn open_weights_modal<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if expense_editor_stage(&draft) == ExpenseEditorStage::Participants {
            let roster = match roster_provider
                .roster_for_channel(ctx, draft.parent_channel_id)
                .await
            {
                Ok(roster) => roster,
                Err(_) => {
                    self.reply_component_error(
                        ctx,
                        component,
                        i18n::member_roster_load_failed_message(),
                    )
                    .await;
                    return;
                }
            };
            if let Some(mut stored) = self.expense_drafts.get_mut(&session_id) {
                stored.frozen_participants = Some(resolve_expense_selections(&stored, &roster));
            }
            self.update_expense_editor_message(ctx, component, session_id, roster_provider)
                .await;
            return;
        }
        self.open_weights_modal_with_return(ctx, component, session_id, roster_provider, false)
            .await;
    }

    async fn open_weights_modal_from_confirmation<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        self.open_weights_modal_with_return(ctx, component, session_id, roster_provider, true)
            .await;
    }

    async fn open_weights_modal_with_return<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
        return_to_confirmation: bool,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let roster = match roster_provider
            .roster_for_channel(ctx, draft.parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                self.reply_component_error(
                    ctx,
                    component,
                    i18n::member_roster_load_failed_message(),
                )
                .await;
                return;
            }
        };
        let selections = resolve_expense_selections(&draft, &roster);
        if selections.len() > 40 {
            self.reply_component_error_with_components(
                ctx,
                component,
                i18n::weight_editor_too_many_message(),
                weight_reset_only_components(
                    session_id,
                    self.interaction_nonce,
                    return_to_confirmation,
                ),
            )
            .await;
            return;
        }
        let member_names = roster_provider
            .display_names_for_guild(draft.guild_id, roster.member_ids.iter().copied());
        let body = expense_weight_modal_body(&draft, &roster, &member_names);
        self.expense_weight_return_to_confirmation
            .insert(session_id, return_to_confirmation);
        let modal = expense_weight_modal(self.interaction_nonce, session_id, body);
        let _ = component
            .create_response(&ctx.http, CreateInteractionResponse::Modal(modal))
            .await;
    }

    async fn reset_expense_weights_from_editor<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(actor_id) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.actor_id)
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.weight_overrides.clear();
            draft.frozen_participants = None;
        }
        self.expense_defaulted_weight_members.remove(&session_id);
        self.expense_weight_return_to_confirmation
            .remove(&session_id);
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn reset_expense_weights_from_confirmation<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let roster = match roster_provider
            .roster_for_channel(ctx, draft.parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                self.reply_component_error(
                    ctx,
                    component,
                    i18n::member_roster_load_failed_message(),
                )
                .await;
                return;
            }
        };
        if let Some(mut stored) = self.expense_drafts.get_mut(&session_id) {
            stored.weight_overrides.clear();
            stored.frozen_participants = Some(resolve_expense_selections(&stored, &roster));
        }
        self.expense_defaulted_weight_members.remove(&session_id);
        self.expense_weight_return_to_confirmation
            .remove(&session_id);
        let member_names = roster_provider
            .display_names_for_guild(draft.guild_id, roster.member_ids.iter().copied());
        let (body, components) = match self.render_expense_confirmation_surface(
            component.user.id,
            session_id,
            &roster,
            &member_names,
        ) {
            Ok(surface) => surface,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await;
    }

    async fn complete_weights_modal<RP>(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let values = modal_values(modal);
        let Some(weights_text) = values.get(EXPENSE_WEIGHTS_FIELD) else {
            return;
        };
        let Some(existing) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::expense_draft_missing_message()),
                    ),
                )
                .await;
            return;
        };
        if existing.actor_id != modal.user.id {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::initiator_only_continuation_message()),
                    ),
                )
                .await;
            return;
        }
        let roster = match roster_provider
            .roster_for_channel(ctx, existing.parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                let _ = modal
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::Message(
                            safe_interaction_response_message()
                                .ephemeral(true)
                                .content(i18n::member_roster_load_failed_message()),
                        ),
                    )
                    .await;
                return;
            }
        };
        let member_names = roster_provider
            .display_names_for_guild(existing.guild_id, roster.member_ids.iter().copied());
        let role_labels = roster_role_labels(ctx, existing.guild_id, &roster);
        let Ok(weight_overrides) = parse_weight_overrides(
            weights_text,
            &expense_weight_label_lookup(&existing, &roster, &member_names),
        ) else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::weight_editor_parse_error()),
                    ),
                )
                .await;
            return;
        };
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_interaction_response_message()
                            .ephemeral(true)
                            .content(i18n::expense_draft_missing_message()),
                    ),
                )
                .await;
            return;
        };
        draft.weight_overrides = weight_overrides;
        drop(draft);
        self.expense_defaulted_weight_members.remove(&session_id);
        let return_to_confirmation = self
            .expense_weight_return_to_confirmation
            .remove(&session_id)
            .map(|(_, value)| value)
            .unwrap_or(false);
        let updated_draft = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
            .expect("draft should stay available");
        let response = if return_to_confirmation {
            match self.render_expense_confirmation_surface(
                modal.user.id,
                session_id,
                &roster,
                &member_names,
            ) {
                Ok((body, components)) => safe_interaction_response_message()
                    .ephemeral(true)
                    .content(body)
                    .components(components),
                Err(message) => {
                    let content = match self.validated_expense_editor_notice(
                        session_id,
                        &roster,
                        &member_names,
                        &role_labels,
                        &message,
                    ) {
                        Ok(content) => content,
                        Err(content_error) => content_error,
                    };
                    safe_interaction_response_message()
                        .ephemeral(true)
                        .content(content)
                        .components(expense_editor_components(
                            session_id,
                            self.interaction_nonce,
                            &updated_draft,
                            &roster,
                            &member_names,
                            &role_labels,
                        ))
                }
            }
        } else {
            match self.validated_expense_editor_content(
                session_id,
                &roster,
                &member_names,
                &role_labels,
            ) {
                Ok(content) => safe_interaction_response_message()
                    .ephemeral(true)
                    .content(content)
                    .components(expense_editor_components(
                        session_id,
                        self.interaction_nonce,
                        &updated_draft,
                        &roster,
                        &member_names,
                        &role_labels,
                    )),
                Err(message) => safe_interaction_response_message()
                    .ephemeral(true)
                    .content(message),
            }
        };
        let _ = modal
            .create_response(&ctx.http, CreateInteractionResponse::Message(response))
            .await;
    }

    async fn show_expense_confirmation<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        let roster = match roster_provider
            .roster_for_channel(ctx, draft.parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                self.reply_component_error(
                    ctx,
                    component,
                    i18n::member_roster_load_failed_message(),
                )
                .await;
                return;
            }
        };
        let member_names = roster_provider
            .display_names_for_guild(draft.guild_id, roster.member_ids.iter().copied());
        let selections = resolve_expense_selections(&draft, &roster);
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.frozen_participants = Some(selections);
        }
        self.expense_defaulted_weight_members.remove(&session_id);
        let (body, components) = match self.render_expense_confirmation_surface(
            component.user.id,
            session_id,
            &roster,
            &member_names,
        ) {
            Ok(surface) => surface,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };

        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await;
    }

    async fn return_to_expense_editor<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.frozen_participants = None;
            draft.active_picker = ExpensePickerKind::Participants;
            draft.picker_query = None;
            draft.picker_page = 0;
        }
        self.expense_defaulted_weight_members.remove(&session_id);
        self.expense_weight_return_to_confirmation
            .remove(&session_id);
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn record_expense<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let draft = match self.claim_expense_recording(session_id, component.user.id) {
            Ok(draft) => draft,
            Err(ExpenseRecordingClaimError::Missing) => {
                self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                    .await;
                return;
            }
            Err(ExpenseRecordingClaimError::Forbidden) => {
                self.reply_component_error(
                    ctx,
                    component,
                    i18n::initiator_only_continuation_message(),
                )
                .await;
                return;
            }
            Err(ExpenseRecordingClaimError::AlreadyRecording) => {
                self.reply_component_error(
                    ctx,
                    component,
                    i18n::expense_already_recording_message(),
                )
                .await;
                return;
            }
        };
        let _ = component.defer(&ctx.http).await;
        if self.is_uncertain_write(draft.parent_channel_id) {
            self.release_expense_recording(session_id);
            self.edit_component_message(
                ctx,
                component,
                self.uncertain_write_block_message(true, false),
                Vec::new(),
            )
            .await;
            return;
        }
        let roster = match roster_provider
            .roster_for_channel(ctx, draft.parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                self.release_expense_recording(session_id);
                self.edit_component_message(
                    ctx,
                    component,
                    i18n::member_roster_load_failed_message(),
                    Vec::new(),
                )
                .await;
                return;
            }
        };
        let member_names = roster_provider
            .display_names_for_guild(draft.guild_id, roster.member_ids.iter().copied());
        let role_labels = roster_role_labels(ctx, draft.guild_id, &roster);
        let selections = match confirmed_expense_selections(&draft, &roster) {
            Ok(selections) => selections,
            Err(ConfirmedExpenseSelectionsError::Missing) => {
                self.release_expense_recording(session_id);
                self.edit_component_message(
                    ctx,
                    component,
                    "先に確認画面を開いてください。",
                    Vec::new(),
                )
                .await;
                return;
            }
            Err(ConfirmedExpenseSelectionsError::Drifted(current)) => {
                let defaulted_members = current
                    .iter()
                    .filter(|selection| {
                        selection.weight == Weight(1)
                            && !draft.weight_overrides.contains_key(&selection.member_id)
                    })
                    .map(|selection| selection.member_id)
                    .collect::<HashSet<_>>();
                if let Some(mut stored) = self.expense_drafts.get_mut(&session_id) {
                    stored.frozen_participants = (!current.is_empty()).then_some(current.clone());
                }
                self.expense_defaulted_weight_members
                    .insert(session_id, defaulted_members);
                self.release_expense_recording(session_id);
                if current.is_empty() {
                    let content = match self.validated_expense_editor_notice(
                        session_id,
                        &roster,
                        &member_names,
                        &role_labels,
                        "対象者が更新されたため入力内容を編集画面に戻しました。対象者を選び直してください。",
                    ) {
                        Ok(content) => content,
                        Err(message) => message,
                    };
                    self.edit_component_message(
                        ctx,
                        component,
                        content,
                        expense_editor_components(
                            session_id,
                            self.interaction_nonce,
                            &draft,
                            &roster,
                            &member_names,
                            &role_labels,
                        ),
                    )
                    .await;
                    return;
                }
                let (confirmation_body, confirmation_components) = match self
                    .render_expense_confirmation_surface(
                        component.user.id,
                        session_id,
                        &roster,
                        &member_names,
                    ) {
                    Ok(surface) => surface,
                    Err(message) => {
                        let content = match self.validated_expense_editor_notice(
                            session_id,
                            &roster,
                            &member_names,
                            &role_labels,
                            &message,
                        ) {
                            Ok(content) => content,
                            Err(content_error) => content_error,
                        };
                        self.edit_component_message(
                            ctx,
                            component,
                            content,
                            expense_editor_components(
                                session_id,
                                self.interaction_nonce,
                                &draft,
                                &roster,
                                &member_names,
                                &role_labels,
                            ),
                        )
                        .await;
                        return;
                    }
                };
                self.edit_component_message(
                    ctx,
                    component,
                    format!(
                        "対象者が更新されたため確認内容を更新しました。もう一度「記録する」を押してください。\n\n{confirmation_body}"
                    ),
                    confirmation_components,
                )
                .await;
                return;
            }
        };
        let payer = match draft.payer {
            Some(payer) => payer,
            None => {
                self.release_expense_recording(session_id);
                self.edit_component_message(
                    ctx,
                    component,
                    i18n::expense_select_payer_message(),
                    Vec::new(),
                )
                .await;
                return;
            }
        };
        if selections.is_empty() {
            self.release_expense_recording(session_id);
            self.edit_component_message(
                ctx,
                component,
                i18n::expense_select_participants_message(),
                Vec::new(),
            )
            .await;
            return;
        }
        let input = RecordExpenseInput {
            payer,
            amount: draft.amount,
            participants: selections,
            note: draft.note.clone(),
            effective_date: draft.effective_date.clone(),
            recorded_by: MemberId(component.user.id.get()),
        };
        let channel_id = draft.parent_channel_id;
        let append_lock = self.append_lock(channel_id);
        let _append_guard = append_lock.lock().await;
        let _cross_process_append_guard =
            match self.acquire_cross_process_append_lock(channel_id).await {
                Ok(guard) => guard,
                Err(message) => {
                    self.release_expense_recording(session_id);
                    self.edit_component_message(ctx, component, message, Vec::new())
                        .await;
                    return;
                }
            };
        if self.is_uncertain_write(channel_id) {
            self.release_expense_recording(session_id);
            self.edit_component_message(
                ctx,
                component,
                self.uncertain_write_block_message(true, false),
                Vec::new(),
            )
            .await;
            return;
        }
        let loaded = match self
            .load_ledger(
                ctx,
                channel_id,
                projection::CanonicalLoadRoute::WritePrelude,
            )
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.release_expense_recording(session_id);
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let entry = match build_live_expense_entry(next_entry_id(&loaded.verified), &input) {
            Ok(entry) => entry,
            Err(error) => {
                self.release_expense_recording(session_id);
                let content = match self.validated_expense_editor_notice(
                    session_id,
                    &roster,
                    &member_names,
                    &role_labels,
                    expense_entry_build_error_message(&error),
                ) {
                    Ok(content) => content,
                    Err(message) => message,
                };
                self.edit_component_message(
                    ctx,
                    component,
                    content,
                    expense_editor_components(
                        session_id,
                        self.interaction_nonce,
                        &draft,
                        &roster,
                        &member_names,
                        &role_labels,
                    ),
                )
                .await;
                return;
            }
        };
        if let Err(error) = self
            .append_entry(ctx, &loaded, channel_id, entry, &member_names)
            .await
        {
            self.release_expense_recording(session_id);
            let message = match error {
                AppendEntryFailure::Message(message) => message,
                AppendEntryFailure::UncertainWrite => {
                    self.uncertain_write_block_message(true, false)
                }
            };
            self.edit_component_message(ctx, component, message, Vec::new())
                .await;
            return;
        }
        self.expense_drafts.remove(&session_id);
        self.expense_defaulted_weight_members.remove(&session_id);
        self.expense_weight_return_to_confirmation
            .remove(&session_id);
        self.edit_component_message(ctx, component, i18n::expense_recorded_message(), Vec::new())
            .await;
    }

    async fn confirm_settlement<RP>(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(guild_id) = command.guild_id else {
            self.reply_command_error(ctx, command, i18n::guild_only_command_message())
                .await;
            return;
        };
        let _ = command.defer_ephemeral(&ctx.http).await;
        let channel_id = match self.lookup_parent_channel_id(ctx, command.channel_id).await {
            Ok(channel_id) => channel_id,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };
        let Some(requested_preview) = self
            .settlement_previews
            .get(&(channel_id.get(), command.user.id.get()))
            .map(|preview| preview.clone())
        else {
            self.edit_command_message(
                ctx,
                command,
                i18n::review_preview_required_message(),
                Vec::new(),
            )
            .await;
            return;
        };
        if self.is_uncertain_write(channel_id) {
            self.edit_command_message(
                ctx,
                command,
                self.uncertain_write_block_message(false, true),
                Vec::new(),
            )
            .await;
            return;
        }
        let append_lock = self.append_lock(requested_preview.ledger_channel_id);
        let _append_guard = append_lock.lock().await;
        let _cross_process_append_guard = match self
            .acquire_cross_process_append_lock(requested_preview.ledger_channel_id)
            .await
        {
            Ok(guard) => guard,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };
        let Some(preview) = self
            .settlement_previews
            .get(&(channel_id.get(), command.user.id.get()))
            .map(|preview| preview.clone())
        else {
            self.edit_command_message(
                ctx,
                command,
                i18n::review_preview_required_message(),
                Vec::new(),
            )
            .await;
            return;
        };
        if preview.binding.preview_instance_id() != requested_preview.binding.preview_instance_id()
        {
            self.edit_command_message(
                ctx,
                command,
                i18n::stale_settlement_preview_message(),
                Vec::new(),
            )
            .await;
            return;
        }
        if self.is_uncertain_write(preview.ledger_channel_id) {
            self.edit_command_message(
                ctx,
                command,
                self.uncertain_write_block_message(false, true),
                Vec::new(),
            )
            .await;
            return;
        }

        let loaded = match self
            .load_ledger(
                ctx,
                preview.ledger_channel_id,
                projection::CanonicalLoadRoute::WritePrelude,
            )
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };

        if loaded.ledger_id != preview.ledger_id
            || current_head_hash(loaded.ledger_id, &loaded.verified)
                != preview.binding.ledger_head_hash()
            || SystemTime::now() > preview.binding.expires_at()
        {
            self.clear_preview_if_current(
                preview.ledger_channel_id,
                command.user.id,
                Some(preview.binding.preview_instance_id()),
            );
            self.edit_command_message(
                ctx,
                command,
                i18n::stale_settlement_preview_message(),
                Vec::new(),
            )
            .await;
            return;
        }

        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        match build_live_settlement_entry(
            next_entry_id(&loaded.verified),
            MemberId(command.user.id.get()),
            &preview.previewed,
            &preview.binding,
        ) {
            Ok(Some(entry)) => {
                if let Err(error) = self
                    .append_entry(
                        ctx,
                        &loaded,
                        preview.ledger_channel_id,
                        entry,
                        &member_names,
                    )
                    .await
                {
                    let message = match error {
                        AppendEntryFailure::Message(message) => message,
                        AppendEntryFailure::UncertainWrite => {
                            self.uncertain_write_block_message(false, true)
                        }
                    };
                    self.edit_command_message(ctx, command, message, Vec::new())
                        .await;
                    return;
                }
                self.clear_preview_if_current(
                    preview.ledger_channel_id,
                    command.user.id,
                    Some(preview.binding.preview_instance_id()),
                );
                self.edit_command_message(
                    ctx,
                    command,
                    i18n::settlement_recorded_message(),
                    Vec::new(),
                )
                .await;
            }
            Ok(None) => {
                self.clear_preview_if_current(
                    preview.ledger_channel_id,
                    command.user.id,
                    Some(preview.binding.preview_instance_id()),
                );
                self.edit_command_message(
                    ctx,
                    command,
                    i18n::settlement_confirmation_failed_message(),
                    Vec::new(),
                )
                .await;
            }
            Err(SettlementRecordError::PreviewNotDelivered) => {
                self.clear_preview_if_current(
                    preview.ledger_channel_id,
                    command.user.id,
                    Some(preview.binding.preview_instance_id()),
                );
                self.edit_command_message(
                    ctx,
                    command,
                    i18n::settlement_preview_not_delivered_message(),
                    Vec::new(),
                )
                .await;
            }
            Err(_) => {
                self.clear_preview_if_current(
                    preview.ledger_channel_id,
                    command.user.id,
                    Some(preview.binding.preview_instance_id()),
                );
                self.edit_command_message(
                    ctx,
                    command,
                    i18n::settlement_confirmation_failed_message(),
                    Vec::new(),
                )
                .await;
            }
        }
    }

    async fn show_ledger<RP>(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(guild_id) = command.guild_id else {
            self.reply_command_error(ctx, command, i18n::guild_only_command_message())
                .await;
            return;
        };
        let _ = command.defer_ephemeral(&ctx.http).await;
        let channel_id = match self.lookup_parent_channel_id(ctx, command.channel_id).await {
            Ok(channel_id) => channel_id,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };
        let loaded = match self
            .load_ledger(ctx, channel_id, projection::CanonicalLoadRoute::Read)
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };
        let render_state = ReadViewRenderState {
            uncertain_write: self.is_uncertain_write(channel_id),
            ..ReadViewRenderState::default()
        };
        if loaded.entries.is_empty() {
            self.edit_command_message(
                ctx,
                command,
                render_ledger_empty_state(ReadViewRoute::LedgerCommand, &render_state)
                    .unwrap_or_else(|message| message),
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let pages = match render_ledger_surface_pages(
            &loaded,
            &member_names,
            ReadViewRoute::LedgerCommand,
            &render_state,
        ) {
            Ok(rendered) => rendered,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };
        let (body, components) =
            self.initial_read_view_page(command.user.id, i18n::stale_ledger_page_message(), pages);
        self.edit_command_message(ctx, command, body, components)
            .await;
    }

    async fn start_void<RP>(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(guild_id) = command.guild_id else {
            self.reply_command_error(ctx, command, i18n::guild_only_command_message())
                .await;
            return;
        };
        let _ = command.defer_ephemeral(&ctx.http).await;
        let channel_id = match self.lookup_parent_channel_id(ctx, command.channel_id).await {
            Ok(channel_id) => channel_id,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };
        let success_thread_channel_id = self
            .canonical_thread_id_for_void_flow(ctx, guild_id, command.channel_id, channel_id)
            .await;
        if self.is_uncertain_write(channel_id) {
            self.edit_command_message(
                ctx,
                command,
                self.uncertain_write_block_message(false, false),
                Vec::new(),
            )
            .await;
            return;
        }
        let loaded = match self
            .load_ledger(
                ctx,
                channel_id,
                projection::CanonicalLoadRoute::WritePrelude,
            )
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };
        if loaded.entries.is_empty() {
            self.edit_command_message(
                ctx,
                command,
                render_void_surface_model(&VoidSurfaceModel::empty(
                    i18n::panel_void_button_label(),
                    Vec::new(),
                    true,
                ))
                .unwrap_or_else(|message| message),
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let all_candidates = candidate_entries_for_void(&loaded.entries, &loaded.projected);
        let candidates = void_window_entries(&loaded.entries, &loaded.projected);
        if candidates.is_empty() {
            self.edit_command_message(
                ctx,
                command,
                if let Some(target) = all_candidates.first().map(|entry| entry.id) {
                    render_void_older_than_window_body(&loaded, target)
                } else {
                    render_void_surface_model(&VoidSurfaceModel::no_candidates(
                        i18n::panel_void_button_label(),
                        Vec::new(),
                        true,
                    ))
                }
                .unwrap_or_else(|message| message),
                Vec::new(),
            )
            .await;
            return;
        }
        let selection_snapshot =
            match void_selection_snapshot_rows(&loaded, &candidates, &member_names) {
                Ok(snapshot) => snapshot,
                Err(message) => {
                    self.edit_command_message(ctx, command, message, Vec::new())
                        .await;
                    return;
                }
            };
        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        self.void_drafts.insert(
            session_id,
            PendingVoidDraft {
                actor_id: command.user.id,
                guild_id,
                ledger_channel_id: channel_id,
                parent_channel_id: channel_id,
                success_thread_channel_id,
                expires_at: interaction_state_expires_at(),
                stage: VoidStage::Select,
                selected_target: None,
                current_page: 0,
                selection_snapshot,
            },
        );
        let draft = self
            .void_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
            .expect("void draft should remain available");
        let (body, components) = match render_void_selection_surface(
            &draft,
            session_id,
            self.interaction_nonce,
            VoidSelectionBodyKind::Normal,
        ) {
            Ok(surface) => surface,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };
        self.edit_command_message(ctx, command, body, components)
            .await;
    }

    async fn update_void_selection<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        _roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self.void_drafts.get(&session_id).map(|draft| draft.clone()) else {
            self.reply_component_error(ctx, component, i18n::void_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let selected_target = match &component.data.kind {
            ComponentInteractionDataKind::StringSelect { values } => values
                .first()
                .and_then(|value| value.parse::<u64>().ok())
                .map(LedgerEntryId),
            _ => draft.selected_target,
        };
        if draft.selection_snapshot.is_empty() {
            self.void_drafts.remove(&session_id);
            self.reply_component_error(ctx, component, i18n::no_voidable_entries_message())
                .await;
            return;
        }
        let Some(mut draft_guard) = self.void_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, i18n::void_draft_missing_message())
                .await;
            return;
        };
        let selected_target = selected_target.filter(|target| {
            draft_guard
                .selection_snapshot
                .iter()
                .any(|row| row.entry_id == *target)
        });
        draft_guard.selected_target = selected_target;
        draft_guard.stage = VoidStage::Select;
        let current_page = draft_guard.current_page;
        let snapshot = draft_guard.selection_snapshot.clone();
        drop(draft_guard);
        let (body, components) = match render_void_selection_surface(
            &PendingVoidDraft {
                selected_target,
                current_page,
                selection_snapshot: snapshot,
                ..draft
            },
            session_id,
            self.interaction_nonce,
            VoidSelectionBodyKind::Normal,
        ) {
            Ok(surface) => surface,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await;
    }

    async fn show_void_confirmation<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self.void_drafts.get(&session_id).map(|draft| draft.clone()) else {
            self.reply_component_error(ctx, component, i18n::void_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let loaded = match self
            .load_ledger(
                ctx,
                draft.ledger_channel_id,
                projection::CanonicalLoadRoute::WritePrelude,
            )
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };
        let member_names = roster_provider.display_names_for_guild(
            draft.guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let all_candidates = candidate_entries_for_void(&loaded.entries, &loaded.projected);
        let candidate_window = void_window_entries(&loaded.entries, &loaded.projected);
        if candidate_window.is_empty() {
            self.void_drafts.remove(&session_id);
            let _ =
                component
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::UpdateMessage(
                            safe_interaction_response_message()
                                .content(
                                    if let Some(target) =
                                        all_candidates.first().map(|entry| entry.id)
                                    {
                                        render_void_older_than_window_body(&loaded, target)
                                    } else {
                                        render_void_surface_model(&VoidSurfaceModel::no_candidates(
                                            i18n::panel_void_button_label(),
                                            Vec::new(),
                                            true,
                                        ))
                                    }
                                    .unwrap_or_else(|message| message),
                                )
                                .components(Vec::new()),
                        ),
                    )
                    .await;
            return;
        }
        let Some(target) = draft.selected_target else {
            let selection_snapshot =
                match void_selection_snapshot_rows(&loaded, &candidate_window, &member_names) {
                    Ok(snapshot) => snapshot,
                    Err(message) => {
                        self.reply_component_error(ctx, component, &message).await;
                        return;
                    }
                };
            if let Some(mut draft_guard) = self.void_drafts.get_mut(&session_id) {
                draft_guard.stage = VoidStage::Select;
                draft_guard.selected_target = None;
                draft_guard.current_page = draft.current_page;
                draft_guard.selection_snapshot = selection_snapshot;
            }
            let render_draft = self
                .void_drafts
                .get(&session_id)
                .map(|draft| draft.clone())
                .expect("void draft should remain available");
            let (body, components) = match render_void_selection_surface(
                &render_draft,
                session_id,
                self.interaction_nonce,
                VoidSelectionBodyKind::MissingSelection,
            ) {
                Ok(surface) => surface,
                Err(message) => {
                    self.reply_component_error(ctx, component, &message).await;
                    return;
                }
            };
            let _ = component
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::UpdateMessage(
                        safe_interaction_response_message()
                            .content(body)
                            .components(components),
                    ),
                )
                .await;
            return;
        };
        if !all_candidates.iter().any(|entry| entry.id == target) {
            let selection_snapshot =
                match void_selection_snapshot_rows(&loaded, &candidate_window, &member_names) {
                    Ok(snapshot) => snapshot,
                    Err(message) => {
                        self.reply_component_error(ctx, component, &message).await;
                        return;
                    }
                };
            if let Some(mut draft_guard) = self.void_drafts.get_mut(&session_id) {
                draft_guard.stage = VoidStage::Select;
                draft_guard.selected_target = None;
                draft_guard.current_page = draft.current_page;
                draft_guard.selection_snapshot = selection_snapshot;
            }
            let render_draft = self
                .void_drafts
                .get(&session_id)
                .map(|draft| draft.clone())
                .expect("void draft should remain available");
            let (body, components) = match render_void_selection_surface(
                &render_draft,
                session_id,
                self.interaction_nonce,
                VoidSelectionBodyKind::StaleTarget(void_retarget_reason(&loaded, target)),
            ) {
                Ok(surface) => surface,
                Err(message) => {
                    self.reply_component_error(ctx, component, &message).await;
                    return;
                }
            };
            let _ = component
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::UpdateMessage(
                        safe_interaction_response_message()
                            .content(body)
                            .components(components),
                    ),
                )
                .await;
            return;
        }
        if !candidate_window.iter().any(|entry| entry.id == target) {
            self.void_drafts.remove(&session_id);
            let _ = component
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::UpdateMessage(
                        safe_interaction_response_message()
                            .content(
                                render_void_older_than_window_body(&loaded, target)
                                    .unwrap_or_else(|message| message),
                            )
                            .components(Vec::new()),
                    ),
                )
                .await;
            return;
        }
        if let Some(mut draft_guard) = self.void_drafts.get_mut(&session_id) {
            draft_guard.stage = VoidStage::Confirm;
        }
        let target_entry = candidate_window
            .iter()
            .find(|entry| entry.id == target)
            .expect("candidate window should contain confirmed target");
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(
                            render_void_confirmation_body(&loaded, target_entry, &member_names)
                                .unwrap_or_else(|message| message),
                        )
                        .components(void_confirmation_components(
                            session_id,
                            target,
                            self.interaction_nonce,
                        )),
                ),
            )
            .await;
    }

    async fn return_to_void_selection<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self.void_drafts.get(&session_id).map(|draft| draft.clone()) else {
            self.reply_component_error(ctx, component, i18n::void_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        let loaded = match self
            .load_ledger(
                ctx,
                draft.ledger_channel_id,
                projection::CanonicalLoadRoute::WritePrelude,
            )
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };
        let member_names = roster_provider.display_names_for_guild(
            draft.guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let all_candidates = candidate_entries_for_void(&loaded.entries, &loaded.projected);
        let candidates = void_window_entries(&loaded.entries, &loaded.projected);
        if candidates.is_empty() {
            self.void_drafts.remove(&session_id);
            self.edit_component_message(
                ctx,
                component,
                if let Some(target) = draft
                    .selected_target
                    .or_else(|| all_candidates.first().map(|entry| entry.id))
                {
                    render_void_older_than_window_body(&loaded, target)
                } else {
                    render_void_surface_model(&VoidSurfaceModel::no_candidates(
                        i18n::panel_void_button_label(),
                        Vec::new(),
                        true,
                    ))
                }
                .unwrap_or_else(|message| message),
                Vec::new(),
            )
            .await;
            return;
        }
        let selection_snapshot =
            match void_selection_snapshot_rows(&loaded, &candidates, &member_names) {
                Ok(snapshot) => snapshot,
                Err(message) => {
                    self.reply_component_error(ctx, component, &message).await;
                    return;
                }
            };
        if let Some(mut draft_guard) = self.void_drafts.get_mut(&session_id) {
            draft_guard.stage = VoidStage::Select;
            draft_guard.selected_target = None;
            draft_guard.current_page = 0;
            draft_guard.selection_snapshot = selection_snapshot;
        }
        let render_draft = self
            .void_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
            .expect("void draft should remain available");
        let (body, components) = match render_void_selection_surface(
            &render_draft,
            session_id,
            self.interaction_nonce,
            VoidSelectionBodyKind::Normal,
        ) {
            Ok(surface) => surface,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await;
    }

    async fn confirm_void<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        confirmed_target: LedgerEntryId,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self.void_drafts.get(&session_id).map(|draft| draft.clone()) else {
            self.reply_component_error(ctx, component, i18n::void_draft_missing_message())
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(ctx, component, i18n::initiator_only_continuation_message())
                .await;
            return;
        }
        if draft.stage != VoidStage::Confirm {
            self.reply_component_error(ctx, component, i18n::void_wrong_stage_copy())
                .await;
            return;
        }
        if draft.selected_target != Some(confirmed_target) {
            self.reply_component_error(ctx, component, i18n::void_target_updated_message())
                .await;
            return;
        }
        let Some(target) = draft.selected_target else {
            self.reply_component_error(ctx, component, i18n::void_select_target_message())
                .await;
            return;
        };
        let _ = component.defer(&ctx.http).await;
        let channel_id = draft.ledger_channel_id;
        if self.is_uncertain_write(channel_id) {
            self.edit_component_message(
                ctx,
                component,
                self.uncertain_write_block_message(true, false),
                Vec::new(),
            )
            .await;
            return;
        }
        let append_lock = self.append_lock(channel_id);
        let _append_guard = append_lock.lock().await;
        let _cross_process_append_guard =
            match self.acquire_cross_process_append_lock(channel_id).await {
                Ok(guard) => guard,
                Err(message) => {
                    self.edit_component_message(ctx, component, message, Vec::new())
                        .await;
                    return;
                }
            };
        if self.is_uncertain_write(channel_id) {
            self.edit_component_message(
                ctx,
                component,
                self.uncertain_write_block_message(true, false),
                Vec::new(),
            )
            .await;
            return;
        }
        let loaded = match self
            .load_ledger(
                ctx,
                channel_id,
                projection::CanonicalLoadRoute::WritePrelude,
            )
            .await
        {
            Ok(loaded) => loaded,
            Err(message) => {
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let current_candidates = candidate_entries_for_void(&loaded.entries, &loaded.projected);
        let candidate_window = void_window_entries(&loaded.entries, &loaded.projected);
        if !current_candidates.iter().any(|entry| entry.id == target) {
            let member_names = roster_provider.display_names_for_guild(
                draft.guild_id,
                loaded.projected.state().participants().iter().copied(),
            );
            if candidate_window.is_empty() {
                self.void_drafts.remove(&session_id);
                self.edit_component_message(
                    ctx,
                    component,
                    if let Some(older_target) = current_candidates.first().map(|entry| entry.id) {
                        render_void_older_than_window_body(&loaded, older_target)
                    } else {
                        render_void_surface_model(&VoidSurfaceModel::no_candidates(
                            i18n::panel_void_button_label(),
                            Vec::new(),
                            true,
                        ))
                    }
                    .unwrap_or_else(|message| message),
                    Vec::new(),
                )
                .await;
                return;
            }
            let selection_snapshot =
                match void_selection_snapshot_rows(&loaded, &candidate_window, &member_names) {
                    Ok(snapshot) => snapshot,
                    Err(message) => {
                        self.edit_component_message(ctx, component, message, Vec::new())
                            .await;
                        return;
                    }
                };
            if let Some(mut draft_guard) = self.void_drafts.get_mut(&session_id) {
                draft_guard.stage = VoidStage::Select;
                draft_guard.selected_target = None;
                draft_guard.current_page = 0;
                draft_guard.selection_snapshot = selection_snapshot;
            }
            let render_draft = self
                .void_drafts
                .get(&session_id)
                .map(|draft| draft.clone())
                .expect("void draft should remain available");
            let (body, components) = match render_void_selection_surface(
                &render_draft,
                session_id,
                self.interaction_nonce,
                VoidSelectionBodyKind::StaleTarget(void_retarget_reason(&loaded, target)),
            ) {
                Ok(surface) => surface,
                Err(message) => {
                    self.edit_component_message(ctx, component, message, Vec::new())
                        .await;
                    return;
                }
            };
            self.edit_component_message(ctx, component, body, components)
                .await;
            return;
        }
        if !candidate_window.iter().any(|entry| entry.id == target) {
            self.void_drafts.remove(&session_id);
            self.edit_component_message(
                ctx,
                component,
                render_void_older_than_window_body(&loaded, target)
                    .unwrap_or_else(|message| message),
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            draft.guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let entry = match build_live_void_entry(
            next_entry_id(&loaded.verified),
            MemberId(component.user.id.get()),
            target,
        ) {
            Ok(entry) => entry,
            Err(_) => {
                self.edit_component_message(
                    ctx,
                    component,
                    i18n::void_build_failed_message(),
                    Vec::new(),
                )
                .await;
                return;
            }
        };
        if let Err(error) = self
            .append_entry(ctx, &loaded, channel_id, entry, &member_names)
            .await
        {
            let message = match error {
                AppendEntryFailure::Message(message) => message,
                AppendEntryFailure::UncertainWrite => {
                    self.uncertain_write_block_message(true, false)
                }
            };
            self.edit_component_message(ctx, component, message, Vec::new())
                .await;
            return;
        }
        self.void_drafts.remove(&session_id);
        let (recovery_cta, recovery_url) = self
            .void_success_recovery_cta_and_url(
                ctx,
                draft.guild_id,
                draft.parent_channel_id,
                draft.success_thread_channel_id,
                component.member.as_ref(),
            )
            .await;
        let (body, components) = render_void_success_surface(
            draft.success_thread_channel_id,
            recovery_cta,
            recovery_url,
        )
        .unwrap_or_else(|message| (message, Vec::new()));
        self.edit_component_message(ctx, component, body, components)
            .await;
    }

    async fn update_expense_editor_message<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| draft.clone())
        else {
            self.reply_component_error(ctx, component, i18n::expense_draft_missing_message())
                .await;
            return;
        };
        let roster = match roster_provider
            .roster_for_channel(ctx, draft.parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                self.reply_component_error(
                    ctx,
                    component,
                    i18n::member_roster_load_failed_message(),
                )
                .await;
                return;
            }
        };
        let member_names = roster_provider
            .display_names_for_guild(draft.guild_id, roster.member_ids.iter().copied());
        let role_labels = roster_role_labels(ctx, draft.guild_id, &roster);
        self.expense_defaulted_weight_members.remove(&session_id);
        let response = match self.validated_expense_editor_content(
            session_id,
            &roster,
            &member_names,
            &role_labels,
        ) {
            Ok(content) => safe_interaction_response_message()
                .content(content)
                .components(expense_editor_components(
                    session_id,
                    self.interaction_nonce,
                    &draft,
                    &roster,
                    &member_names,
                    &role_labels,
                )),
            Err(message) => safe_interaction_response_message().content(message),
        };
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(response),
            )
            .await;
    }

    fn render_expense_editor(
        &self,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
        role_labels: &HashMap<RoleId, SafeLiteralText>,
    ) -> String {
        let Some(draft) = self.expense_drafts.get(&session_id) else {
            return i18n::expense_draft_missing_message().into();
        };
        let labels = surface_member_labels(member_names);
        let stage = expense_editor_stage(&draft);
        let picker_page =
            (stage != ExpenseEditorStage::Weight).then(|| match draft.active_picker {
                ExpensePickerKind::Payer => member_picker_page(
                    &draft,
                    roster,
                    member_names,
                    &draft.payer.iter().copied().collect::<Vec<_>>(),
                ),
                ExpensePickerKind::Participants => {
                    member_picker_page(&draft, roster, member_names, &draft.explicit_members)
                }
                ExpensePickerKind::Roles => {
                    role_picker_page(&draft, role_labels, &draft.selected_roles)
                }
            });
        let mut selections = draft
            .frozen_participants
            .clone()
            .unwrap_or_else(|| resolve_expense_selections(&draft, roster));
        sort_expense_selections(&labels, &mut selections);
        let title = expense_editor_stage_title(stage);
        let summary_lines = self.expense_summary_lines(&draft);
        let payer_line = i18n::expense_payer_line(
            draft
                .payer
                .map(|member_id| safe_display_name(&labels, member_id))
                .unwrap_or_else(|| i18n::expense_unselected_label().to_owned()),
        )
        .to_string();
        let mut out = format!("{title}\n\n");
        for line in &summary_lines {
            out.push_str(line);
            out.push('\n');
        }
        out.push_str(&payer_line);
        out.push('\n');
        if let Some(picker_page) = picker_page.as_ref() {
            if stage == ExpenseEditorStage::Participants {
                out.push_str(
                    &i18n::expense_current_selection_line(expense_picker_kind_label(
                        draft.active_picker,
                    ))
                    .to_string(),
                );
                out.push('\n');
            }
            if let Some((page_indicator, range_indicator)) = picker_page_lines(picker_page) {
                out.push_str(&format!("{page_indicator}\n{range_indicator}\n"));
            }
            if let Some(query) = draft.picker_query.as_deref() {
                out.push_str(&i18n::expense_search_line(query).to_string());
                out.push('\n');
                if picker_search_target_page(
                    draft.active_picker,
                    roster,
                    member_names,
                    role_labels,
                    query,
                )
                .is_none()
                {
                    out.push_str(match draft.active_picker {
                        ExpensePickerKind::Payer | ExpensePickerKind::Participants => {
                            i18n::member_search_not_found_error()
                        }
                        ExpensePickerKind::Roles => i18n::role_search_not_found_error(),
                    });
                    out.push('\n');
                }
            }
        }
        out.push_str(&format!(
            "\n{}\n",
            i18n::expense_current_participants_heading()
        ));
        if selections.is_empty() {
            out.push_str(i18n::expense_current_participants_empty_line());
            out.push('\n');
        } else {
            for selection in selections {
                out.push_str(&format!(
                    "- {} ({}) x{}\n",
                    safe_display_name(&labels, selection.member_id),
                    selection.member_id.0,
                    selection.weight.0
                ));
            }
        }
        let mut selected_members = draft.explicit_members.clone();
        selected_members.sort_by(|lhs, rhs| labels.compare_members(*lhs, *rhs));
        let selected_names = selected_members
            .into_iter()
            .map(|member_id| labels.safe_member_label(member_id))
            .collect::<Vec<_>>();
        out.push('\n');
        out.push_str(&individual_selection_title(
            selected_names.len(),
            &selected_names,
        ));
        out.push('\n');
        if draft.include_members_group {
            out.push('\n');
            out.push_str(i18n::expense_members_group_line());
            out.push('\n');
        }
        if !draft.selected_roles.is_empty() {
            let selected_roles = draft
                .selected_roles
                .iter()
                .map(|role_id| {
                    role_labels
                        .get(role_id)
                        .cloned()
                        .unwrap_or_else(|| fallback_role_label(*role_id))
                })
                .collect::<Vec<_>>();
            if let Some(summary) = summarize_selected_role_labels(&selected_roles) {
                out.push_str(i18n::expense_selected_roles_prefix());
                out.push_str(": ");
                out.push_str(&summary);
                out.push('\n');
            }
        }
        out.push('\n');
        match stage {
            ExpenseEditorStage::Payer => {}
            ExpenseEditorStage::Participants => {
                out.push_str(match draft.active_picker {
                    ExpensePickerKind::Roles => i18n::role_picker_help(),
                    ExpensePickerKind::Payer | ExpensePickerKind::Participants => {
                        i18n::member_picker_help()
                    }
                });
                out.push('\n');
                out.push_str(participant_source_help_line());
            }
            ExpenseEditorStage::Weight => out.push_str(i18n::weight_editor_help()),
        }
        out
    }

    fn render_expense_editor_notice(
        &self,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
        role_labels: &HashMap<RoleId, SafeLiteralText>,
        notice: &str,
    ) -> String {
        format!(
            "{notice}\n\n{}",
            self.render_expense_editor(session_id, roster, member_names, role_labels)
        )
    }

    fn validated_expense_editor_content(
        &self,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
        role_labels: &HashMap<RoleId, SafeLiteralText>,
    ) -> Result<String, String> {
        let content = self.render_expense_editor(session_id, roster, member_names, role_labels);
        validate_message_content(&content)
            .map_err(|error| format!("経費入力画面を表示できませんでした: {error:?}"))?;
        Ok(content)
    }

    fn validated_expense_editor_notice(
        &self,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
        role_labels: &HashMap<RoleId, SafeLiteralText>,
        notice: &str,
    ) -> Result<String, String> {
        let content = self.render_expense_editor_notice(
            session_id,
            roster,
            member_names,
            role_labels,
            notice,
        );
        validate_message_content(&content)
            .map_err(|error| format!("経費入力画面を表示できませんでした: {error:?}"))?;
        Ok(content)
    }

    fn render_expense_confirmation_surface(
        &self,
        actor_id: UserId,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
    ) -> Result<(String, Vec<CreateActionRow>), String> {
        let pages = self.expense_confirmation_pages(session_id, roster, member_names)?;
        Ok(self.initial_expense_confirmation_page(session_id, actor_id, pages))
    }

    fn expense_confirmation_pages(
        &self,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
    ) -> Result<Vec<ReadViewRenderedPage>, String> {
        let Some(draft) = self.expense_drafts.get(&session_id) else {
            return Err(i18n::expense_draft_missing_message().into());
        };
        let payer = draft
            .payer
            .ok_or_else(|| i18n::expense_select_payer_message().to_string())?;
        let labels = surface_member_labels(member_names);
        let mut selections = draft
            .frozen_participants
            .clone()
            .unwrap_or_else(|| resolve_expense_selections(&draft, roster));
        sort_expense_selections(&labels, &mut selections);
        if selections.is_empty() {
            return Err(i18n::expense_select_participants_message().into());
        }
        validate_expense_weight_configuration(&selections)
            .map_err(|error| expense_entry_build_error_message(&error).to_owned())?;
        let share_amounts = self.confirmation_share_amounts(&draft, &selections)?;
        let defaulted_members = self
            .expense_defaulted_weight_members
            .get(&session_id)
            .map(|members| members.clone())
            .unwrap_or_default();
        let summary_lines = self.expense_summary_lines(&draft);
        let mut participant_rows = Vec::with_capacity(selections.len());
        for selection in selections {
            let row = ExpenseConfirmationParticipantRow {
                display_name: labels.safe_member_label(selection.member_id),
                share_amount: share_amounts
                    .get(&selection.member_id)
                    .map(ToString::to_string),
                weight: selection.weight.0,
                badges: confirmation_badges(&draft, roster, selection.member_id),
                defaulted_weight: defaulted_members.contains(&selection.member_id),
            };
            participant_rows.push(row.render());
        }
        let payer_line = i18n::expense_payer_line(safe_display_name(&labels, payer)).to_string();
        let base_components = expense_confirmation_components(session_id, self.interaction_nonce);
        let render_page_body = |rows: &[String], current_page: usize, total_pages: usize| {
            let mut out = format!("{}\n\n", i18n::expense_step_title_confirm());
            for line in &summary_lines {
                out.push_str(line);
                out.push('\n');
            }
            if total_pages > 1 {
                out.push('\n');
                out.push_str(&i18n::page_indicator(current_page, total_pages).to_string());
                out.push('\n');
                out.push_str(i18n::snapshot_notice());
                out.push('\n');
            }
            out.push_str(&format!(
                "\n{}\n",
                i18n::expense_confirmation_participants_heading()
            ));
            out.push_str(&payer_line);
            out.push('\n');
            for row in rows {
                out.push_str(row);
                out.push('\n');
            }
            out.push('\n');
            out.push_str(i18n::weight_editor_help());
            out.push('\n');
            out.push('\n');
            out.push_str(confirmation_source_disclosure_line());
            out
        };

        let single_page = render_page_body(&participant_rows, 1, 1);
        if participant_rows.len() <= SURFACE_PAGE_SIZE
            && validate_message_content(&single_page).is_ok()
        {
            return Ok(vec![ReadViewRenderedPage {
                body: single_page,
                base_components,
            }]);
        }

        let max_possible_pages = participant_rows.len().max(1);
        let mut page_ranges = Vec::new();
        let mut start = 0;
        while start < participant_rows.len() {
            let mut end = start;
            let mut next_start = start;
            while end < participant_rows.len() && end - start < SURFACE_PAGE_SIZE {
                let candidate = render_page_body(
                    &participant_rows[start..=end],
                    max_possible_pages,
                    max_possible_pages,
                );
                if validate_message_content(&candidate).is_ok() {
                    next_start = end + 1;
                    end += 1;
                    continue;
                }
                break;
            }
            if next_start == start {
                let error = validate_message_content(&render_page_body(
                    &participant_rows[start..start + 1],
                    max_possible_pages,
                    max_possible_pages,
                ))
                .expect_err("single confirmation row should report the budget error");
                return Err(format!("確認画面を表示できませんでした: {error:?}"));
            }
            page_ranges.push((start, next_start));
            start = next_start;
        }

        let total_pages = page_ranges.len();
        Ok(page_ranges
            .into_iter()
            .enumerate()
            .map(|(page_index, (start, end))| ReadViewRenderedPage {
                body: render_page_body(&participant_rows[start..end], page_index + 1, total_pages),
                base_components: base_components.clone(),
            })
            .collect())
    }

    #[cfg_attr(not(test), allow(dead_code))]
    fn render_expense_confirmation(
        &self,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
    ) -> Result<String, String> {
        Ok(self
            .expense_confirmation_pages(session_id, roster, member_names)?
            .into_iter()
            .next()
            .expect("expense confirmation should render at least one page")
            .body)
    }

    fn expense_summary_lines(&self, draft: &ExpenseDraft) -> [String; 3] {
        ExpenseDraftSummary {
            amount: draft.amount.to_string(),
            effective_date: draft
                .effective_date
                .clone()
                .unwrap_or_else(|| PocSystemClock.today_business_date()),
            note: draft.note.as_deref().and_then(sanitized_note_text),
        }
        .render_lines()
    }

    fn expense_record_input(
        &self,
        draft: &ExpenseDraft,
        selections: Vec<ExpenseParticipantSelection>,
    ) -> Result<RecordExpenseInput, String> {
        Ok(RecordExpenseInput {
            payer: draft
                .payer
                .ok_or_else(|| i18n::expense_select_payer_message().to_owned())?,
            amount: draft.amount,
            participants: selections,
            note: draft.note.clone(),
            effective_date: draft.effective_date.clone(),
            recorded_by: MemberId(draft.actor_id.get()),
        })
    }

    fn confirmation_share_amounts(
        &self,
        draft: &ExpenseDraft,
        selections: &[ExpenseParticipantSelection],
    ) -> Result<HashMap<MemberId, Money>, String> {
        let input = self.expense_record_input(draft, selections.to_vec())?;
        let entry = build_live_expense_entry(LedgerEntryId(0), &input)
            .map_err(|error| expense_entry_build_error_message(&error).to_owned())?;
        let LedgerEvent::ExpenseRecorded(event) = entry.event else {
            unreachable!("build_expense_entry should always return an expense")
        };
        Ok(event
            .owed_by()
            .iter()
            .map(|owed| (owed.member_id, owed.amount))
            .collect())
    }

    fn claim_expense_recording(
        &self,
        session_id: u64,
        actor_id: UserId,
    ) -> Result<ExpenseDraft, ExpenseRecordingClaimError> {
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            return Err(ExpenseRecordingClaimError::Missing);
        };
        if draft.actor_id != actor_id {
            return Err(ExpenseRecordingClaimError::Forbidden);
        }
        if draft.recording {
            return Err(ExpenseRecordingClaimError::AlreadyRecording);
        }
        draft.recording = true;
        Ok(draft.clone())
    }

    fn release_expense_recording(&self, session_id: u64) {
        if let Some(mut draft) = self.expense_drafts.get_mut(&session_id) {
            draft.recording = false;
        }
    }

    async fn component_session_id(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        custom_id: &str,
        prefix: &str,
    ) -> Result<Option<u64>, ()> {
        match parse_session_id(custom_id, prefix, self.interaction_nonce) {
            SessionIdMatch::Match(session_id) => {
                if let Err(message) = self
                    .ensure_session_scope_matches(ctx, component.channel_id, session_id)
                    .await
                {
                    self.reply_component_error(ctx, component, &message).await;
                    return Err(());
                }
                Ok(Some(session_id))
            }
            SessionIdMatch::Stale => {
                self.reply_component_error(ctx, component, i18n::stale_interaction_message())
                    .await;
                Err(())
            }
            SessionIdMatch::NoMatch => Ok(None),
        }
    }

    async fn void_confirm_payload(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        custom_id: &str,
    ) -> Result<Option<(u64, LedgerEntryId)>, ()> {
        match parse_void_confirm_id(custom_id, self.interaction_nonce) {
            VoidConfirmMatch::Match { session_id, target } => {
                if let Err(message) = self
                    .ensure_session_scope_matches(ctx, component.channel_id, session_id)
                    .await
                {
                    self.reply_component_error(ctx, component, &message).await;
                    return Err(());
                }
                Ok(Some((session_id, target)))
            }
            VoidConfirmMatch::Stale => {
                self.reply_component_error(ctx, component, i18n::stale_interaction_message())
                    .await;
                Err(())
            }
            VoidConfirmMatch::NoMatch => Ok(None),
        }
    }

    async fn modal_session_id(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        custom_id: &str,
        prefix: &str,
    ) -> Result<Option<u64>, ()> {
        match parse_session_id(custom_id, prefix, self.interaction_nonce) {
            SessionIdMatch::Match(session_id) => {
                if let Err(message) = self
                    .ensure_session_scope_matches(ctx, modal.channel_id, session_id)
                    .await
                {
                    let _ = modal
                        .create_response(
                            &ctx.http,
                            CreateInteractionResponse::Message(
                                safe_interaction_response_message()
                                    .ephemeral(true)
                                    .content(message),
                            ),
                        )
                        .await;
                    return Err(());
                }
                Ok(Some(session_id))
            }
            SessionIdMatch::Stale => {
                let _ = modal
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::Message(
                            safe_interaction_response_message()
                                .ephemeral(true)
                                .content(i18n::stale_interaction_message()),
                        ),
                    )
                    .await;
                Err(())
            }
            SessionIdMatch::NoMatch => Ok(None),
        }
    }

    fn expected_session_parent_channel_id(&self, session_id: u64) -> Option<ChannelId> {
        self.expense_drafts
            .get(&session_id)
            .map(|draft| draft.parent_channel_id)
            .or_else(|| {
                self.void_drafts
                    .get(&session_id)
                    .map(|draft| draft.ledger_channel_id)
            })
    }

    async fn acquire_cross_process_append_lock(
        &self,
        channel_id: ChannelId,
    ) -> Result<CrossProcessFileLock, String> {
        acquire_cross_process_file_lock(append_lock_path(channel_id)).await
    }

    async fn lookup_parent_channel_id(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> Result<ChannelId, String> {
        let channel = channel_id
            .to_channel(&ctx.http)
            .await
            .map_err(|error| format!("現在のチャンネルを確認できませんでした: {error:?}"))?;
        let Some(channel) = channel.guild() else {
            return Ok(channel_id);
        };
        let is_thread = matches!(
            channel.kind,
            ChannelType::PublicThread | ChannelType::PrivateThread | ChannelType::NewsThread
        );
        if is_thread && let Some(parent_id) = channel.parent_id {
            return Ok(parent_id);
        }
        Ok(channel.id)
    }

    async fn canonical_thread_candidates_for_parent(
        &self,
        ctx: &Context,
        guild_id: GuildId,
        parent_channel_id: ChannelId,
    ) -> Vec<ChannelId> {
        let mut seen = HashSet::new();
        let mut candidates = Vec::new();
        if let Ok(active_threads) = guild_id.get_active_threads(&ctx.http).await {
            for thread_id in canonical_thread_candidate_ids(
                parent_channel_id,
                active_threads
                    .threads
                    .into_iter()
                    .map(|thread| (thread.id, thread.kind, thread.name, thread.parent_id)),
            ) {
                if seen.insert(thread_id) {
                    candidates.push(thread_id);
                }
            }
        }

        let mut before = None;
        loop {
            let Ok(archived_threads) = parent_channel_id
                .get_archived_public_threads(&ctx.http, before, Some(100))
                .await
            else {
                break;
            };
            let next_before = archived_threads
                .threads
                .last()
                .map(|thread| thread.id.get());
            for thread_id in canonical_thread_candidate_ids(
                parent_channel_id,
                archived_threads
                    .threads
                    .into_iter()
                    .map(|thread| (thread.id, thread.kind, thread.name, thread.parent_id)),
            ) {
                if seen.insert(thread_id) {
                    candidates.push(thread_id);
                }
            }
            if !archived_threads.has_more || next_before.is_none() {
                break;
            }
            before = next_before;
        }

        candidates
    }

    async fn verified_canonical_thread_id_for_panel(
        &self,
        ctx: &Context,
        guild_id: GuildId,
        parent_channel_id: ChannelId,
    ) -> Option<ChannelId> {
        let mut results = Vec::new();
        for thread_id in self
            .canonical_thread_candidates_for_parent(ctx, guild_id, parent_channel_id)
            .await
        {
            results.push((
                thread_id,
                self.load_ledger(ctx, thread_id, projection::CanonicalLoadRoute::Read)
                    .await
                    .is_ok_and(|loaded| !loaded.verified.is_empty()),
            ));
        }
        first_verified_canonical_thread_id_for_panel(results)
    }

    async fn review_parent_recovery_cta_and_url(
        &self,
        ctx: &Context,
        guild_id: GuildId,
        parent_channel_id: ChannelId,
        thread_channel_id: ChannelId,
        member: Option<&Member>,
    ) -> (RecoveryCta, Option<String>) {
        let (canonical_thread_accessible, tracked_parent_accessible) = match member {
            Some(member) => (
                self.member_can_access_channel(ctx, guild_id, member, thread_channel_id, true)
                    .await,
                self.member_can_access_channel(ctx, guild_id, member, parent_channel_id, false)
                    .await,
            ),
            None => (false, true),
        };
        let recovery_cta = DiscordLedgerPresenter::select_recovery_cta(RecoveryContext {
            canonical_thread_known: true,
            canonical_thread_accessible,
            tracked_parent_known: true,
            tracked_parent_accessible,
            recovery_reference_available: false,
        });
        let recovery_url = match recovery_cta {
            RecoveryCta::ThreadLink => Some(channel_link(guild_id, thread_channel_id)),
            RecoveryCta::ParentLink => Some(channel_link(guild_id, parent_channel_id)),
            _ => None,
        };
        (recovery_cta, recovery_url)
    }

    async fn member_can_access_channel(
        &self,
        ctx: &Context,
        guild_id: GuildId,
        member: &Member,
        channel_id: ChannelId,
        require_application_commands: bool,
    ) -> bool {
        self.member_permissions_in_channel(ctx, guild_id, member, channel_id)
            .await
            .is_some_and(|permissions| {
                permissions.view_channel()
                    && (!require_application_commands || permissions.use_application_commands())
            })
    }

    async fn member_permissions_in_channel(
        &self,
        ctx: &Context,
        guild_id: GuildId,
        member: &Member,
        channel_id: ChannelId,
    ) -> Option<Permissions> {
        let channel = channel_id.to_channel(&ctx.http).await.ok()?;
        let guild_channel = channel.guild()?;
        let guild = guild_id.to_partial_guild(&ctx.http).await.ok()?;
        Some(guild.user_permissions_in(&guild_channel, member))
    }

    async fn canonical_thread_id_for_void_flow(
        &self,
        ctx: &Context,
        guild_id: GuildId,
        interaction_channel_id: ChannelId,
        parent_channel_id: ChannelId,
    ) -> Option<ChannelId> {
        if interaction_channel_id != parent_channel_id
            && self
                .is_verified_canonical_thread(ctx, interaction_channel_id, parent_channel_id)
                .await
        {
            return Some(interaction_channel_id);
        }

        self.verified_canonical_thread_id_for_panel(ctx, guild_id, parent_channel_id)
            .await
    }

    async fn is_verified_canonical_thread(
        &self,
        ctx: &Context,
        thread_channel_id: ChannelId,
        parent_channel_id: ChannelId,
    ) -> bool {
        let Ok(channel) = thread_channel_id.to_channel(&ctx.http).await else {
            return false;
        };
        let Some(channel) = channel.guild() else {
            return false;
        };
        if channel.name != CANONICAL_LEDGER_THREAD_NAME
            || channel.parent_id != Some(parent_channel_id)
        {
            return false;
        }

        self.load_ledger(ctx, thread_channel_id, projection::CanonicalLoadRoute::Read)
            .await
            .is_ok_and(|loaded| !loaded.verified.is_empty())
    }

    async fn void_success_recovery_cta_and_url(
        &self,
        ctx: &Context,
        guild_id: GuildId,
        parent_channel_id: ChannelId,
        thread_channel_id: Option<ChannelId>,
        member: Option<&Member>,
    ) -> (RecoveryCta, Option<String>) {
        let canonical_thread_accessible = match (member, thread_channel_id) {
            (Some(member), Some(thread_channel_id)) => {
                self.member_can_access_channel(ctx, guild_id, member, thread_channel_id, true)
                    .await
            }
            (None, Some(_)) => false,
            (_, None) => false,
        };
        let tracked_parent_accessible = match member {
            Some(member) => {
                self.member_can_access_channel(ctx, guild_id, member, parent_channel_id, false)
                    .await
            }
            None => true,
        };
        let recovery_cta = DiscordLedgerPresenter::select_recovery_cta(RecoveryContext {
            canonical_thread_known: thread_channel_id.is_some(),
            canonical_thread_accessible,
            tracked_parent_known: true,
            tracked_parent_accessible,
            recovery_reference_available: false,
        });
        let recovery_url = match recovery_cta {
            RecoveryCta::ThreadLink => {
                thread_channel_id.map(|thread_id| channel_link(guild_id, thread_id))
            }
            RecoveryCta::ParentLink => Some(channel_link(guild_id, parent_channel_id)),
            _ => None,
        };
        (recovery_cta, recovery_url)
    }

    async fn ensure_session_scope_matches(
        &self,
        ctx: &Context,
        interaction_channel_id: ChannelId,
        session_id: u64,
    ) -> Result<(), String> {
        let Some(expected_parent_channel_id) = self.expected_session_parent_channel_id(session_id)
        else {
            return Ok(());
        };
        let actual_parent_channel_id = self
            .lookup_parent_channel_id(ctx, interaction_channel_id)
            .await?;
        if actual_parent_channel_id != expected_parent_channel_id {
            return Err(i18n::stale_interaction_message().to_owned());
        }
        Ok(())
    }

    async fn load_ledger(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
        route: projection::CanonicalLoadRoute,
    ) -> Result<LoadedLedgerThread, String> {
        let ledger_id = LedgerId(channel_id.get());
        let bot_id = ctx.cache.current_user().id;
        let writer_lineage = writer_lineage_policy_from_env(bot_id).map_err(|error| {
            let failure = projection::CanonicalLoadFailure::WriterLineage { route };
            tracing::warn!(
                event = failure
                    .observability_event()
                    .unwrap_or("ledger_load_failed_closed"),
                ledger_id = ledger_id.0,
                route = failure.route().label(),
                error = %error,
                "writer lineage policy failed closed"
            );
            failure
                .user_message()
                .unwrap_or(i18n::ledger_integrity_failed_message())
                .to_owned()
        })?;
        let store = store::DiscordCanonicalLedgerStore::new(
            writer_lineage,
            Arc::new(observability::TracingLedgerObservability),
        );
        let load = store
            .load_verified_thread(ctx, channel_id, ledger_id, route.label())
            .await
            .map_err(|error| {
                let failure = projection::CanonicalLoadFailure::from_store_error(route, &error);
                tracing::warn!(
                    event = failure
                        .observability_event()
                        .unwrap_or("ledger_load_failed_closed"),
                    ledger_id = ledger_id.0,
                    route = failure.route().label(),
                    failing_entry_id = failure.failing_entry_id().map(|entry_id| entry_id.0),
                    error = ?error,
                    "canonical ledger load failed closed"
                );
                failure
                    .user_message()
                    .map(str::to_owned)
                    .unwrap_or_else(|| format!("台帳を読み込めませんでした: {error:?}"))
            })?;
        let snapshot = load.snapshot().clone();
        let verified = load.verified().to_vec();
        self.panel_state_cache
            .insert(channel_id, !verified.is_empty());
        let projected = snapshot.projected().clone();
        let entries = verified
            .iter()
            .map(|envelope| envelope.payload().entry.clone())
            .collect::<Vec<_>>();
        let transport_entries = load
            .transport_index()
            .iter()
            .map(|(entry_id, transport)| {
                (
                    entry_id,
                    LoadedTransportEntry {
                        message_link: transport.message_link().to_owned(),
                        recorded_at: transport.recorded_at(),
                    },
                )
            })
            .collect();
        Ok(LoadedLedgerThread {
            ledger_id,
            channel_id,
            snapshot,
            verified,
            projected,
            entries,
            transport_entries,
        })
    }

    async fn append_entry(
        &self,
        ctx: &Context,
        loaded: &LoadedLedgerThread,
        channel_id: ChannelId,
        entry: LedgerEntry,
        member_names: &HashMap<MemberId, SmolStr>,
    ) -> Result<(), AppendEntryFailure> {
        let envelope = make_unverified_envelope_sha256_v1(
            loaded.ledger_id,
            current_head_hash(loaded.ledger_id, &loaded.verified),
            (),
            entry.clone(),
        )
        .map_err(|error| {
            AppendEntryFailure::Message(format!(
                "ハッシュチェーン用のデータを構築できませんでした: {error:?}"
            ))
        })?;
        let mut entries = loaded.entries.clone();
        entries.push(entry.clone());
        replay_entries(entries).map_err(|error| {
            AppendEntryFailure::Message(format!("台帳記録の事前再生に失敗しました: {error:?}"))
        })?;
        let public_message = render_public_entry_message(loaded, &entry, member_names, None)
            .map_err(AppendEntryFailure::Message)?;
        let attachment_bytes = encode_discord_canonical_attachment(
            &envelope,
            Some(&public_message),
        )
        .map_err(|error| {
            AppendEntryFailure::Message(format!("正規データ添付を作成できませんでした: {error:?}"))
        })?;

        let mut sent_message = channel_id
            .send_message(
                &ctx.http,
                safe_create_message()
                    .content(&public_message)
                    .add_file(CreateAttachment::bytes(
                        attachment_bytes,
                        LEDGER_ATTACHMENT_FILENAME,
                    )),
            )
            .await
            .map_err(|error| {
                AppendEntryFailure::Message(format!("台帳記録を送信できませんでした: {error:?}"))
            })?;
        self.panel_state_cache.insert(channel_id, true);
        let finalized_message = match render_public_entry_message(
            loaded,
            &entry,
            member_names,
            Some(sent_message.link()),
        ) {
            Ok(message) => message,
            Err(_) => {
                self.mark_uncertain_write(channel_id);
                return Err(AppendEntryFailure::UncertainWrite);
            }
        };
        if finalized_message != public_message
            && sent_message
                .edit(&ctx.http, safe_edit_message().content(finalized_message))
                .await
                .is_err()
        {
            self.mark_uncertain_write(channel_id);
            return Err(AppendEntryFailure::UncertainWrite);
        }
        Ok(())
    }

    fn append_lock(&self, channel_id: ChannelId) -> Arc<Mutex<()>> {
        self.append_locks
            .entry(channel_id)
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone()
    }

    async fn edit_command_message(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        content: impl Into<String>,
        components: Vec<CreateActionRow>,
    ) -> bool {
        command
            .edit_response(
                &ctx.http,
                safe_edit_interaction_response()
                    .content(content)
                    .components(components),
            )
            .await
            .is_ok()
    }

    async fn edit_component_message(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        content: impl Into<String>,
        components: Vec<CreateActionRow>,
    ) -> bool {
        component
            .edit_response(
                &ctx.http,
                safe_edit_interaction_response()
                    .content(content)
                    .components(components),
            )
            .await
            .is_ok()
    }

    async fn defer_ephemeral_component(&self, ctx: &Context, component: &ComponentInteraction) {
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Defer(
                    safe_interaction_response_message().ephemeral(true),
                ),
            )
            .await;
    }

    async fn reply_component_error(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        message: &str,
    ) {
        self.reply_component_error_with_components(ctx, component, message, Vec::new())
            .await;
    }

    async fn reply_component_error_with_components(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        message: &str,
        components: Vec<CreateActionRow>,
    ) {
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    safe_interaction_response_message()
                        .ephemeral(true)
                        .content(message)
                        .components(components),
                ),
            )
            .await;
    }

    async fn reply_command_error(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        message: &str,
    ) {
        let _ = command
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    safe_interaction_response_message()
                        .ephemeral(true)
                        .content(message),
                ),
            )
            .await;
    }
}

fn new_interaction_nonce() -> u64 {
    let started_at = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64;
    started_at ^ u64::from(std::process::id())
}

fn ledger_lock_dir() -> PathBuf {
    std::env::temp_dir().join("walicord-ledger-locks")
}

fn append_lock_path(thread_id: ChannelId) -> PathBuf {
    ledger_lock_dir().join(format!("append-{}.lock", thread_id.get()))
}

fn instance_lock_path() -> PathBuf {
    ledger_lock_dir().join("runtime-instance.lock")
}

async fn acquire_cross_process_file_lock(path: PathBuf) -> Result<CrossProcessFileLock, String> {
    tokio::task::spawn_blocking(move || -> Result<CrossProcessFileLock, String> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|error| format!("ロック用ディレクトリを作成できませんでした: {error}"))?;
        }
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&path)
            .map_err(|error| format!("ロックファイルを開けませんでした: {error}"))?;
        file.lock_exclusive()
            .map_err(|error| format!("プロセス間ロックを取得できませんでした: {error}"))?;
        Ok(CrossProcessFileLock { _file: file })
    })
    .await
    .map_err(|error| format!("プロセス間ロックの取得処理に失敗しました: {error}"))?
}

fn parse_session_id(custom_id: &str, prefix: &str, interaction_nonce: u64) -> SessionIdMatch {
    let Some(value) = custom_id.strip_prefix(prefix) else {
        return SessionIdMatch::NoMatch;
    };
    let Some((nonce, session_id)) = value.split_once(':') else {
        return SessionIdMatch::Stale;
    };
    if nonce != interaction_nonce.to_string() {
        return SessionIdMatch::Stale;
    }
    session_id
        .parse()
        .map(SessionIdMatch::Match)
        .unwrap_or(SessionIdMatch::Stale)
}

fn parse_void_confirm_id(custom_id: &str, interaction_nonce: u64) -> VoidConfirmMatch {
    let Some(value) = custom_id.strip_prefix(VOID_CONFIRM_PREFIX) else {
        return VoidConfirmMatch::NoMatch;
    };
    let mut parts = value.split(':');
    let (Some(nonce), Some(session_id), Some(target), None) =
        (parts.next(), parts.next(), parts.next(), parts.next())
    else {
        return VoidConfirmMatch::Stale;
    };
    if nonce != interaction_nonce.to_string() {
        return VoidConfirmMatch::Stale;
    }
    let (Ok(session_id), Ok(target)) = (session_id.parse::<u64>(), target.parse::<u64>()) else {
        return VoidConfirmMatch::Stale;
    };
    VoidConfirmMatch::Match {
        session_id,
        target: LedgerEntryId(target),
    }
}

fn session_custom_id(interaction_nonce: u64, prefix: &str, session_id: u64) -> String {
    format!("{prefix}{interaction_nonce}:{session_id}")
}

fn validated_custom_id_owned(custom_id: String) -> String {
    validate_custom_id(&custom_id).expect("generated custom ids should fit Discord limits");
    custom_id
}

fn validated_button_label_text(label: &'static str) -> &'static str {
    validate_button_label(label).expect("button labels should fit Discord limits");
    label
}

fn validated_modal_title_text(title: &'static str) -> &'static str {
    validate_modal_title(title).expect("modal titles should fit Discord limits");
    title
}

fn validated_text_input_label_text(label: &'static str) -> &'static str {
    validate_text_input_label(label).expect("text input labels should fit Discord limits");
    label
}

fn validated_text_input_placeholder_text(placeholder: &'static str) -> &'static str {
    validate_text_input_placeholder(placeholder)
        .expect("text input placeholders should fit Discord limits");
    placeholder
}

fn validated_component_placeholder_text(placeholder: &'static str) -> &'static str {
    validate_component_placeholder(placeholder).expect("placeholders should fit Discord limits");
    placeholder
}

fn void_confirm_custom_id(
    interaction_nonce: u64,
    session_id: u64,
    target: LedgerEntryId,
) -> String {
    format!(
        "{VOID_CONFIRM_PREFIX}{interaction_nonce}:{session_id}:{}",
        target.0
    )
}

pub(crate) fn is_ledger_component_id(custom_id: &str) -> bool {
    is_ledger_panel_component_id(custom_id)
        || custom_id.starts_with("expense:")
        || custom_id.starts_with("void:")
}

pub(crate) fn is_ledger_modal_id(custom_id: &str) -> bool {
    custom_id.starts_with("expense:")
}

fn expense_modal(
    interaction_nonce: u64,
    session_id: u64,
    draft: Option<&ExpenseDraft>,
) -> CreateModal {
    let mut amount_input = CreateInputText::new(
        InputTextStyle::Short,
        validated_text_input_label_text(i18n::expense_modal_amount_label()),
        EXPENSE_AMOUNT_FIELD,
    )
    .placeholder(validated_text_input_placeholder_text(
        i18n::expense_modal_amount_placeholder(),
    ))
    .required(true);
    if let Some(draft) = draft {
        amount_input = amount_input.value(draft.amount.to_string());
    }

    let mut note_input = CreateInputText::new(
        InputTextStyle::Paragraph,
        validated_text_input_label_text(i18n::expense_modal_note_label()),
        EXPENSE_NOTE_FIELD,
    )
    .placeholder(validated_text_input_placeholder_text(
        i18n::expense_modal_note_placeholder(),
    ))
    .required(false);
    if let Some(note) = draft.and_then(|draft| draft.note.as_ref()) {
        note_input = note_input.value(note.clone());
    }

    let date_value = draft
        .and_then(|draft| {
            draft
                .effective_date
                .as_ref()
                .map(|date| date.as_str().to_owned())
        })
        .unwrap_or_else(|| today_date().format("%Y-%m-%d").to_string());

    CreateModal::new(
        validated_custom_id_owned(session_custom_id(
            interaction_nonce,
            EXPENSE_MODAL_PREFIX,
            session_id,
        )),
        validated_modal_title_text(i18n::expense_modal_title()),
    )
    .components(vec![
        CreateActionRow::InputText(amount_input),
        CreateActionRow::InputText(note_input),
        CreateActionRow::InputText(
            CreateInputText::new(
                InputTextStyle::Short,
                validated_text_input_label_text(i18n::expense_modal_date_label()),
                EXPENSE_DATE_FIELD,
            )
            .placeholder(validated_text_input_placeholder_text(
                i18n::expense_modal_date_placeholder(),
            ))
            .value(date_value)
            .required(false),
        ),
    ])
}

fn expense_weight_modal(interaction_nonce: u64, session_id: u64, body: String) -> CreateModal {
    CreateModal::new(
        validated_custom_id_owned(session_custom_id(
            interaction_nonce,
            EXPENSE_WEIGHTS_MODAL_PREFIX,
            session_id,
        )),
        validated_modal_title_text(i18n::weight_editor_modal_title()),
    )
    .components(vec![CreateActionRow::InputText(
        CreateInputText::new(
            InputTextStyle::Paragraph,
            validated_text_input_label_text(i18n::weight_editor_input_label()),
            EXPENSE_WEIGHTS_FIELD,
        )
        .placeholder(validated_text_input_placeholder_text(
            i18n::weight_editor_placeholder(),
        ))
        .value(body)
        .required(true),
    )])
}

fn expense_search_modal(
    interaction_nonce: u64,
    session_id: u64,
    picker_kind: ExpensePickerKind,
    current_query: Option<&str>,
) -> CreateModal {
    let (prefix, title, label, placeholder) = match picker_kind {
        ExpensePickerKind::Payer | ExpensePickerKind::Participants => (
            if matches!(picker_kind, ExpensePickerKind::Payer) {
                EXPENSE_PAYER_SEARCH_MODAL_PREFIX
            } else {
                EXPENSE_MEMBER_SEARCH_MODAL_PREFIX
            },
            i18n::member_search_modal_title(),
            i18n::member_search_input_label(),
            i18n::member_search_placeholder(),
        ),
        ExpensePickerKind::Roles => (
            EXPENSE_ROLE_SEARCH_MODAL_PREFIX,
            i18n::role_search_modal_title(),
            i18n::role_search_input_label(),
            i18n::role_search_placeholder(),
        ),
    };
    let mut input = CreateInputText::new(
        InputTextStyle::Short,
        validated_text_input_label_text(label),
        EXPENSE_SEARCH_FIELD,
    )
    .placeholder(validated_text_input_placeholder_text(placeholder))
    .required(false);
    if let Some(query) = current_query.filter(|query| !query.is_empty()) {
        input = input.value(query.to_owned());
    }
    CreateModal::new(
        validated_custom_id_owned(session_custom_id(interaction_nonce, prefix, session_id)),
        validated_modal_title_text(title),
    )
    .components(vec![CreateActionRow::InputText(input)])
}

#[derive(Debug, Clone)]
struct PickerPage<T> {
    items: Vec<T>,
    current_page: usize,
    total_pages: usize,
    range_start: usize,
    range_end: usize,
    total_items: usize,
}

fn picker_search_page_index<'a>(
    labels: impl IntoIterator<Item = &'a str>,
    query: &str,
) -> Option<usize> {
    let normalized_query = query.trim().to_lowercase();
    (!normalized_query.is_empty()).then_some(())?;
    labels
        .into_iter()
        .position(|label| label.to_lowercase().contains(&normalized_query))
        .map(|index| index / PICKER_PAGE_SIZE)
}

fn paginate_entries<T>(entries: Vec<T>, requested_page: usize) -> PickerPage<T> {
    let total_items = entries.len();
    let total_pages = total_items.max(1).div_ceil(PICKER_PAGE_SIZE);
    let current_page = requested_page.min(total_pages.saturating_sub(1));
    let start = current_page * PICKER_PAGE_SIZE;
    let end = (start + PICKER_PAGE_SIZE).min(total_items);
    let items = entries
        .into_iter()
        .skip(start)
        .take(PICKER_PAGE_SIZE)
        .collect::<Vec<_>>();
    PickerPage {
        items,
        current_page,
        total_pages,
        range_start: if total_items == 0 { 0 } else { start + 1 },
        range_end: end,
        total_items,
    }
}

fn current_picker_page<T>(draft: &ExpenseDraft, items: Vec<T>) -> PickerPage<T> {
    paginate_entries(items, draft.picker_page)
}

fn expense_editor_stage(draft: &ExpenseDraft) -> ExpenseEditorStage {
    if draft.active_picker == ExpensePickerKind::Payer {
        ExpenseEditorStage::Payer
    } else if draft.frozen_participants.is_some() {
        ExpenseEditorStage::Weight
    } else {
        ExpenseEditorStage::Participants
    }
}

fn expense_editor_stage_title(stage: ExpenseEditorStage) -> &'static str {
    match stage {
        ExpenseEditorStage::Payer => i18n::expense_step_title_payer(),
        ExpenseEditorStage::Participants => i18n::expense_step_title_participants(),
        ExpenseEditorStage::Weight => i18n::expense_step_title_weight(),
    }
}

fn sorted_member_picker_entries(
    roster: &crate::discord::ports::RosterSnapshot,
    member_names: &HashMap<MemberId, SmolStr>,
) -> Vec<(MemberId, String)> {
    let labels = surface_member_labels(member_names);
    let mut entries = roster.member_ids.clone();
    entries.sort_by(|lhs, rhs| labels.compare_members(*lhs, *rhs));
    entries
        .into_iter()
        .map(|member_id| (member_id, safe_display_name(&labels, member_id)))
        .collect()
}

fn sorted_role_picker_entries(
    role_labels: &HashMap<RoleId, SafeLiteralText>,
) -> Vec<(RoleId, String)> {
    let mut entries = role_labels
        .iter()
        .map(|(role_id, label)| (*role_id, label.as_str().to_owned()))
        .collect::<Vec<_>>();
    entries.sort_by(|(lhs_id, lhs_label), (rhs_id, rhs_label)| {
        SurfaceMemberLabels::compare_display_text(lhs_label, rhs_label).then(lhs_id.cmp(rhs_id))
    });
    entries
}

fn picker_search_target_page(
    picker_kind: ExpensePickerKind,
    roster: &crate::discord::ports::RosterSnapshot,
    member_names: &HashMap<MemberId, SmolStr>,
    role_labels: &HashMap<RoleId, SafeLiteralText>,
    query: &str,
) -> Option<usize> {
    match picker_kind {
        ExpensePickerKind::Payer | ExpensePickerKind::Participants => picker_search_page_index(
            sorted_member_picker_entries(roster, member_names)
                .iter()
                .map(|(_, label)| label.as_str()),
            query,
        ),
        ExpensePickerKind::Roles => picker_search_page_index(
            sorted_role_picker_entries(role_labels)
                .iter()
                .map(|(_, label)| label.as_str()),
            query,
        ),
    }
}

fn member_picker_page(
    draft: &ExpenseDraft,
    roster: &crate::discord::ports::RosterSnapshot,
    member_names: &HashMap<MemberId, SmolStr>,
    selected_members: &[MemberId],
) -> PickerPage<CreateSelectMenuOption> {
    let selected_members = selected_members.iter().copied().collect::<HashSet<_>>();
    let entries = sorted_member_picker_entries(roster, member_names)
        .into_iter()
        .map(|(member_id, label)| {
            let label = truncate_component_label(&label);
            CreateSelectMenuOption::new(label, member_id.0.to_string())
                .default_selection(selected_members.contains(&member_id))
        })
        .collect::<Vec<_>>();
    current_picker_page(draft, entries)
}

fn role_picker_page(
    draft: &ExpenseDraft,
    role_labels: &HashMap<RoleId, SafeLiteralText>,
    selected_roles: &[RoleId],
) -> PickerPage<CreateSelectMenuOption> {
    let selected_roles = selected_roles.iter().copied().collect::<HashSet<_>>();
    let entries = sorted_role_picker_entries(role_labels)
        .into_iter()
        .map(|(role_id, label)| {
            let label = truncate_component_label(&label);
            CreateSelectMenuOption::new(label, role_id.0.to_string())
                .default_selection(selected_roles.contains(&role_id))
        })
        .collect::<Vec<_>>();
    current_picker_page(draft, entries)
}

fn expense_picker_kind_label(kind: ExpensePickerKind) -> &'static str {
    match kind {
        ExpensePickerKind::Payer => i18n::FROM,
        ExpensePickerKind::Participants => i18n::participant_source_individual_label(),
        ExpensePickerKind::Roles => i18n::participant_source_role_label(),
    }
}

fn picker_page_lines(page: &PickerPage<CreateSelectMenuOption>) -> Option<(String, String)> {
    (page.total_items > 0).then(|| {
        (
            i18n::page_indicator(page.current_page + 1, page.total_pages).to_string(),
            i18n::page_range_indicator(page.range_start, page.range_end, page.total_items)
                .to_string(),
        )
    })
}

fn expense_editor_components(
    session_id: u64,
    interaction_nonce: u64,
    draft: &ExpenseDraft,
    roster: &crate::discord::ports::RosterSnapshot,
    member_names: &HashMap<MemberId, SmolStr>,
    role_labels: &HashMap<RoleId, SafeLiteralText>,
) -> Vec<CreateActionRow> {
    let stage = expense_editor_stage(draft);
    let payer_page = member_picker_page(
        draft,
        roster,
        member_names,
        &draft.payer.iter().copied().collect::<Vec<_>>(),
    );
    let participants_page =
        member_picker_page(draft, roster, member_names, &draft.explicit_members);
    let roles_page = role_picker_page(draft, role_labels, &draft.selected_roles);
    let picker_select_row = |prefix: &str,
                             placeholder: &'static str,
                             min_values: u8,
                             max_values: u8,
                             page: PickerPage<CreateSelectMenuOption>| {
        let PickerPage {
            items,
            current_page,
            total_pages,
            total_items,
            ..
        } = page;
        let empty_label = match draft.active_picker {
            ExpensePickerKind::Payer | ExpensePickerKind::Participants => {
                i18n::member_search_not_found_error()
            }
            ExpensePickerKind::Roles => i18n::role_search_not_found_error(),
        };
        let options = if items.is_empty() {
            vec![CreateSelectMenuOption::new(
                truncate_component_label(empty_label),
                "empty",
            )]
        } else {
            items
        };
        let option_count =
            u8::try_from(options.len()).expect("picker option count should fit in u8");
        let max_values = max_values.min(option_count);
        let min_values = min_values.min(max_values);
        (
            CreateActionRow::SelectMenu(
                CreateSelectMenu::new(
                    validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        prefix,
                        session_id,
                    )),
                    CreateSelectMenuKind::String { options },
                )
                .placeholder(validated_component_placeholder_text(placeholder))
                .min_values(min_values)
                .max_values(max_values)
                .disabled(total_items == 0),
            ),
            current_page,
            total_pages,
            total_items,
        )
    };
    let picker_utility_row = |clear_prefix: &str,
                              clear_label: &'static str,
                              clear_disabled: bool,
                              current_page: usize,
                              total_pages: usize,
                              total_items: usize| {
        CreateActionRow::Buttons(vec![
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_PICKER_PREVIOUS_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(
                i18n::picker_previous_page_label(),
            ))
            .style(ButtonStyle::Secondary)
            .disabled(current_page == 0 || total_items == 0),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_PICKER_NEXT_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::picker_next_page_label()))
            .style(ButtonStyle::Secondary)
            .disabled(current_page + 1 >= total_pages || total_items == 0),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_PICKER_SEARCH_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::picker_search_label()))
            .style(ButtonStyle::Secondary),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                clear_prefix,
                session_id,
            )))
            .label(validated_button_label_text(clear_label))
            .style(ButtonStyle::Secondary)
            .disabled(clear_disabled),
        ])
    };

    match stage {
        ExpenseEditorStage::Payer => {
            let (select_row, current_page, total_pages, total_items) = picker_select_row(
                EXPENSE_PAYER_PREFIX,
                i18n::expense_payer_placeholder(),
                1,
                1,
                payer_page,
            );
            vec![
                select_row,
                picker_utility_row(
                    EXPENSE_CLEAR_PAYER_PREFIX,
                    i18n::payer_clear_label(),
                    draft.payer.is_none(),
                    current_page,
                    total_pages,
                    total_items,
                ),
                CreateActionRow::Buttons(vec![
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_BACK_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(i18n::expense_back_label()))
                    .style(ButtonStyle::Secondary),
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_CANCEL_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(i18n::expense_cancel_label()))
                    .style(ButtonStyle::Danger),
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_NEXT_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(i18n::expense_next_label()))
                    .style(ButtonStyle::Primary)
                    .disabled(draft.payer.is_none()),
                ]),
            ]
        }
        ExpenseEditorStage::Participants => {
            let (
                prefix,
                placeholder,
                min_values,
                max_values,
                page,
                clear_prefix,
                clear_label,
                clear_disabled,
            ) = match draft.active_picker {
                ExpensePickerKind::Payer => unreachable!("payer picker belongs to the payer stage"),
                ExpensePickerKind::Participants => (
                    EXPENSE_PARTICIPANTS_PREFIX,
                    i18n::participant_source_individual_placeholder(),
                    0,
                    25,
                    participants_page,
                    EXPENSE_CLEAR_PARTICIPANTS_PREFIX,
                    i18n::individual_clear_label(),
                    draft.explicit_members.is_empty(),
                ),
                ExpensePickerKind::Roles => (
                    EXPENSE_ROLES_PREFIX,
                    i18n::participant_source_role_placeholder(),
                    0,
                    25,
                    roles_page,
                    EXPENSE_CLEAR_ROLES_PREFIX,
                    i18n::role_clear_label(),
                    draft.selected_roles.is_empty(),
                ),
            };
            let (select_row, current_page, total_pages, total_items) =
                picker_select_row(prefix, placeholder, min_values, max_values, page);
            vec![
                CreateActionRow::Buttons(vec![
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_PICKER_PARTICIPANTS_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(
                        i18n::participant_source_individual_label(),
                    ))
                    .style(
                        if draft.active_picker == ExpensePickerKind::Participants {
                            ButtonStyle::Primary
                        } else {
                            ButtonStyle::Secondary
                        },
                    ),
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_PICKER_ROLES_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(
                        i18n::participant_source_role_label(),
                    ))
                    .style(
                        if draft.active_picker == ExpensePickerKind::Roles {
                            ButtonStyle::Primary
                        } else {
                            ButtonStyle::Secondary
                        },
                    ),
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_TOGGLE_MEMBERS_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(
                        i18n::participant_source_members_label(),
                    ))
                    .style(ButtonStyle::Secondary)
                    .disabled(draft.include_members_group),
                ]),
                select_row,
                picker_utility_row(
                    clear_prefix,
                    clear_label,
                    clear_disabled,
                    current_page,
                    total_pages,
                    total_items,
                ),
                CreateActionRow::Buttons(vec![
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_CLEAR_ROLES_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(
                        i18n::participant_source_clear_roles_label(),
                    ))
                    .style(ButtonStyle::Secondary)
                    .disabled(draft.selected_roles.is_empty()),
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_TOGGLE_MEMBERS_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(
                        i18n::participant_source_clear_members_label(),
                    ))
                    .style(ButtonStyle::Secondary)
                    .disabled(!draft.include_members_group),
                ]),
                CreateActionRow::Buttons(vec![
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_BACK_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(i18n::expense_back_label()))
                    .style(ButtonStyle::Secondary),
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_CANCEL_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(i18n::expense_cancel_label()))
                    .style(ButtonStyle::Danger),
                    CreateButton::new(validated_custom_id_owned(session_custom_id(
                        interaction_nonce,
                        EXPENSE_OPEN_WEIGHTS_PREFIX,
                        session_id,
                    )))
                    .label(validated_button_label_text(i18n::expense_to_weights_label()))
                    .style(ButtonStyle::Primary)
                    .disabled(
                        draft.explicit_members.is_empty()
                            && draft.selected_roles.is_empty()
                            && !draft.include_members_group,
                    ),
                ]),
            ]
        }
        ExpenseEditorStage::Weight => vec![
            CreateActionRow::Buttons(vec![
                CreateButton::new(validated_custom_id_owned(session_custom_id(
                    interaction_nonce,
                    EXPENSE_OPEN_WEIGHTS_PREFIX,
                    session_id,
                )))
                .label(validated_button_label_text(
                    i18n::expense_weight_edit_label(),
                ))
                .style(ButtonStyle::Secondary),
                CreateButton::new(validated_custom_id_owned(session_custom_id(
                    interaction_nonce,
                    EXPENSE_RESET_WEIGHTS_PREFIX,
                    session_id,
                )))
                .label(validated_button_label_text(
                    i18n::weight_editor_reset_label(),
                ))
                .style(ButtonStyle::Secondary),
            ]),
            CreateActionRow::Buttons(vec![
                CreateButton::new(validated_custom_id_owned(session_custom_id(
                    interaction_nonce,
                    EXPENSE_BACK_PREFIX,
                    session_id,
                )))
                .label(validated_button_label_text(i18n::expense_back_label()))
                .style(ButtonStyle::Secondary),
                CreateButton::new(validated_custom_id_owned(session_custom_id(
                    interaction_nonce,
                    EXPENSE_CANCEL_PREFIX,
                    session_id,
                )))
                .label(validated_button_label_text(i18n::expense_cancel_label()))
                .style(ButtonStyle::Danger),
                CreateButton::new(validated_custom_id_owned(session_custom_id(
                    interaction_nonce,
                    EXPENSE_CONFIRM_PREFIX,
                    session_id,
                )))
                .label(validated_button_label_text(i18n::expense_to_confirm_label()))
                .style(ButtonStyle::Success)
                .disabled(
                    draft
                        .frozen_participants
                        .as_ref()
                        .is_none_or(|selections| selections.is_empty()),
                ),
            ]),
        ],
    }
}

fn expense_confirmation_components(
    session_id: u64,
    interaction_nonce: u64,
) -> Vec<CreateActionRow> {
    vec![
        CreateActionRow::Buttons(vec![
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_CONFIRM_BACK_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::expense_back_label()))
            .style(ButtonStyle::Secondary),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_CONFIRM_WEIGHTS_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(
                i18n::expense_weight_edit_label(),
            ))
            .style(ButtonStyle::Secondary),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_CONFIRM_RESET_WEIGHTS_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(
                i18n::weight_editor_reset_label(),
            ))
            .style(ButtonStyle::Secondary),
        ]),
        CreateActionRow::Buttons(vec![
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_REVISE_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::expense_revise_label()))
            .style(ButtonStyle::Secondary),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_EDIT_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(
                i18n::expense_basic_info_edit_label(),
            ))
            .style(ButtonStyle::Secondary),
        ]),
        CreateActionRow::Buttons(vec![
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_CANCEL_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::expense_cancel_label()))
            .style(ButtonStyle::Danger),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                EXPENSE_RECORD_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::expense_record_label()))
            .style(ButtonStyle::Success),
        ]),
    ]
}

fn weight_reset_only_components(
    session_id: u64,
    interaction_nonce: u64,
    return_to_confirmation: bool,
) -> Vec<CreateActionRow> {
    let prefix = if return_to_confirmation {
        EXPENSE_CONFIRM_RESET_WEIGHTS_PREFIX
    } else {
        EXPENSE_RESET_WEIGHTS_PREFIX
    };
    vec![CreateActionRow::Buttons(vec![
        CreateButton::new(validated_custom_id_owned(session_custom_id(
            interaction_nonce,
            prefix,
            session_id,
        )))
        .label(validated_button_label_text(
            i18n::weight_editor_reset_label(),
        ))
        .style(ButtonStyle::Secondary),
    ])]
}

fn render_void_surface_model(model: &VoidSurfaceModel) -> Result<String, String> {
    DiscordLedgerPresenter::render_void_flow(model)
        .map(|rendered| rendered.body)
        .map_err(|error| format!("取り消し画面を表示できませんでした: {error:?}"))
}

#[derive(Debug, Clone, Copy)]
enum VoidSelectionBodyKind {
    Normal,
    MissingSelection,
    StaleTarget(VoidRetargetReason),
}

fn void_selection_snapshot_rows(
    loaded: &LoadedLedgerThread,
    candidates: &[LedgerEntry],
    member_names: &HashMap<MemberId, SmolStr>,
) -> Result<Vec<VoidSelectionSnapshotRow>, String> {
    let labels = surface_member_labels(member_names);
    candidates
        .iter()
        .map(|entry| {
            Ok(VoidSelectionSnapshotRow {
                entry_id: entry.id,
                select_label: summarize_entry_for_choice_with_labels(entry, &labels),
                candidate_row: VoidCandidateRow {
                    summary: surface_summary_for_entry(loaded, entry, &labels)?,
                    recovery_reference: recovery_reference_for(loaded, entry.id, true)?,
                },
            })
        })
        .collect()
}

fn void_selection_model(
    body_kind: VoidSelectionBodyKind,
    rows: &[VoidSelectionSnapshotRow],
    current_page: usize,
    total_pages: usize,
) -> VoidSurfaceModel {
    let candidate_rows = rows
        .iter()
        .map(|row| row.candidate_row.clone())
        .collect::<Vec<_>>();
    let mut model = match body_kind {
        VoidSelectionBodyKind::Normal => VoidSurfaceModel::selection(
            i18n::panel_void_button_label(),
            candidate_rows,
            Vec::new(),
            true,
        ),
        VoidSelectionBodyKind::MissingSelection => VoidSurfaceModel::missing_selection(
            i18n::panel_void_button_label(),
            candidate_rows,
            Vec::new(),
            true,
        ),
        VoidSelectionBodyKind::StaleTarget(reason) => VoidSurfaceModel::stale_target(
            i18n::panel_void_button_label(),
            reason,
            candidate_rows,
            Vec::new(),
            true,
        ),
    };
    if total_pages > 1 {
        model.page_indicator =
            Some(i18n::page_indicator(current_page + 1, total_pages).to_string());
        model.snapshot_notice = Some(i18n::snapshot_notice().to_owned());
    }
    model
}

fn render_void_selection_body_for_rows(
    body_kind: VoidSelectionBodyKind,
    rows: &[VoidSelectionSnapshotRow],
    current_page: usize,
    total_pages: usize,
) -> Result<String, String> {
    render_void_surface_model(&void_selection_model(
        body_kind,
        rows,
        current_page,
        total_pages,
    ))
}

fn void_selection_page_ranges(
    snapshot: &[VoidSelectionSnapshotRow],
) -> Result<Vec<(usize, usize)>, String> {
    if snapshot.is_empty() {
        return Err(i18n::no_voidable_entries_message().to_owned());
    }
    if render_void_selection_body_for_rows(VoidSelectionBodyKind::Normal, snapshot, 0, 1).is_ok() {
        return Ok(vec![(0, snapshot.len())]);
    }

    let mut pages = Vec::new();
    let mut start = 0;
    while start < snapshot.len() {
        let mut end = start;
        while end < snapshot.len() && end - start < SURFACE_PAGE_SIZE {
            let candidate_end = end + 1;
            if render_void_selection_body_for_rows(
                VoidSelectionBodyKind::Normal,
                &snapshot[start..candidate_end],
                19,
                20,
            )
            .is_ok()
            {
                end = candidate_end;
            } else {
                break;
            }
        }
        if end == start {
            return render_void_selection_body_for_rows(
                VoidSelectionBodyKind::Normal,
                &snapshot[start..start + 1],
                19,
                20,
            )
            .map(|_| Vec::new());
        }
        pages.push((start, end));
        start = end;
    }
    Ok(pages)
}

fn void_selection_current_page(
    snapshot: &[VoidSelectionSnapshotRow],
    requested_page: usize,
) -> Result<(usize, Vec<(usize, usize)>), String> {
    let page_ranges = void_selection_page_ranges(snapshot)?;
    let current_page = requested_page.min(page_ranges.len().saturating_sub(1));
    Ok((current_page, page_ranges))
}

fn void_selection_components_page(
    session_id: u64,
    rows: &[VoidSelectionSnapshotRow],
    selected_target: Option<LedgerEntryId>,
    interaction_nonce: u64,
    current_page: usize,
    total_pages: usize,
) -> Result<Vec<CreateActionRow>, String> {
    if rows.is_empty() {
        return Err(i18n::no_voidable_entries_message().to_owned());
    }
    let options = rows
        .iter()
        .map(|row| {
            Ok(CreateSelectMenuOption::new(
                truncate_component_label(&row.select_label),
                row.entry_id.0.to_string(),
            )
            .default_selection(selected_target == Some(row.entry_id)))
        })
        .collect::<Result<Vec<_>, String>>()?;
    let mut components = vec![
        CreateActionRow::SelectMenu(
            CreateSelectMenu::new(
                validated_custom_id_owned(session_custom_id(
                    interaction_nonce,
                    VOID_SELECT_PREFIX,
                    session_id,
                )),
                CreateSelectMenuKind::String { options },
            )
            .placeholder(validated_component_placeholder_text(
                i18n::void_select_placeholder(),
            ))
            .min_values(1)
            .max_values(1),
        ),
        CreateActionRow::Buttons(vec![
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                VOID_CANCEL_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::void_cancel_label()))
            .style(ButtonStyle::Danger),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                VOID_NEXT_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::void_next_label()))
            .style(ButtonStyle::Primary)
            .disabled(selected_target.is_none()),
        ]),
    ];
    if total_pages > 1 {
        components.push(CreateActionRow::Buttons(vec![
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                VOID_PREVIOUS_PAGE_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(
                i18n::picker_previous_page_label(),
            ))
            .style(ButtonStyle::Secondary)
            .disabled(current_page == 0),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                VOID_NEXT_PAGE_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::picker_next_page_label()))
            .style(ButtonStyle::Secondary)
            .disabled(current_page + 1 >= total_pages),
        ]));
    }
    Ok(components)
}

fn render_void_selection_surface(
    draft: &PendingVoidDraft,
    session_id: u64,
    interaction_nonce: u64,
    body_kind: VoidSelectionBodyKind,
) -> Result<(String, Vec<CreateActionRow>), String> {
    let (current_page, page_ranges) =
        void_selection_current_page(&draft.selection_snapshot, draft.current_page)?;
    let total_pages = page_ranges.len();
    let (start, end) = page_ranges[current_page];
    let page_rows = &draft.selection_snapshot[start..end];
    Ok((
        render_void_selection_body_for_rows(body_kind, page_rows, current_page, total_pages)?,
        void_selection_components_page(
            session_id,
            page_rows,
            draft.selected_target,
            interaction_nonce,
            current_page,
            total_pages,
        )?,
    ))
}

fn void_confirmation_total_amount(entry: &LedgerEntry) -> Result<String, String> {
    match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => Ok(event
            .paid_by()
            .iter()
            .map(|paid| paid.amount)
            .sum::<Money>()
            .to_string()),
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => Ok(event
            .transfers()
            .iter()
            .map(|transfer| transfer.amount)
            .sum::<Money>()
            .to_string()),
        other => Err(format!(
            "entry #{} cannot be void-confirmed from {other:?}",
            entry.id.0
        )),
    }
}

fn void_confirmation_recap_for_entry(
    loaded: &LoadedLedgerThread,
    entry: &LedgerEntry,
    member_names: &HashMap<MemberId, SmolStr>,
) -> Result<VoidConfirmationRecap, String> {
    let labels = surface_member_labels(member_names);
    Ok(VoidConfirmationRecap {
        summary: surface_summary_for_entry(loaded, entry, &labels)?,
        total_amount: void_confirmation_total_amount(entry)?,
        recovery_reference: recovery_reference_for(loaded, entry.id, true)?,
    })
}

fn void_retarget_reason(
    loaded: &LoadedLedgerThread,
    entry_id: LedgerEntryId,
) -> VoidRetargetReason {
    match loaded.projected.entry(entry_id) {
        Some(info) if info.voided => VoidRetargetReason::VoidedByAnotherUser,
        Some(info) if info.sealed => VoidRetargetReason::EnteredSealedRange,
        _ => VoidRetargetReason::ExcludedFromCandidates,
    }
}

#[cfg_attr(not(test), allow(dead_code))]
fn render_void_selection_body(
    loaded: &LoadedLedgerThread,
    candidates: &[LedgerEntry],
    member_names: &HashMap<MemberId, SmolStr>,
) -> Result<String, String> {
    let snapshot = void_selection_snapshot_rows(loaded, candidates, member_names)?;
    let (_, page_ranges) = void_selection_current_page(&snapshot, 0)?;
    let total_pages = page_ranges.len();
    let (start, end) = page_ranges[0];
    render_void_selection_body_for_rows(
        VoidSelectionBodyKind::Normal,
        &snapshot[start..end],
        0,
        total_pages,
    )
}

fn render_void_confirmation_body(
    loaded: &LoadedLedgerThread,
    target: &LedgerEntry,
    member_names: &HashMap<MemberId, SmolStr>,
) -> Result<String, String> {
    render_void_surface_model(&VoidSurfaceModel::confirmation(
        i18n::void_confirmation_title(),
        void_confirmation_recap_for_entry(loaded, target, member_names)?,
        Vec::new(),
        true,
    ))
}

fn render_void_older_than_window_body(
    loaded: &LoadedLedgerThread,
    target: LedgerEntryId,
) -> Result<String, String> {
    render_void_surface_model(&VoidSurfaceModel::older_than_window(
        i18n::panel_void_button_label(),
        recovery_reference_for(loaded, target, true)?.render_line(),
        Vec::new(),
        true,
    ))
}

fn render_void_success_surface(
    canonical_thread_channel_id: Option<ChannelId>,
    recovery_cta: RecoveryCta,
    recovery_url: Option<String>,
) -> Result<(String, Vec<CreateActionRow>), String> {
    DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::success_with_recovery(
        i18n::void_success_title(),
        canonical_thread_channel_id
            .filter(|_| matches!(recovery_cta, RecoveryCta::ThreadLink))
            .map(|channel_id| format!("<#{}>", channel_id.get())),
        recovery_cta,
        recovery_url,
        Vec::new(),
        true,
    ))
    .map(rendered_surface_to_message)
    .map_err(|error| format!("取り消し画面を表示できませんでした: {error:?}"))
}

#[cfg_attr(not(test), allow(dead_code))]
fn void_selection_components(
    session_id: u64,
    candidates: &[LedgerEntry],
    member_names: &HashMap<MemberId, SmolStr>,
    selected_target: Option<LedgerEntryId>,
    interaction_nonce: u64,
) -> Result<Vec<CreateActionRow>, String> {
    if candidates.is_empty() {
        return Err(i18n::no_voidable_entries_message().to_owned());
    }
    let options = candidates
        .iter()
        .take(25)
        .map(|entry| {
            let label = summarize_entry_for_choice(entry, member_names);
            Ok(CreateSelectMenuOption::new(
                truncate_component_label(&label),
                entry.id.0.to_string(),
            )
            .default_selection(selected_target == Some(entry.id)))
        })
        .collect::<Result<Vec<_>, String>>()?;
    Ok(vec![
        CreateActionRow::SelectMenu(
            CreateSelectMenu::new(
                validated_custom_id_owned(session_custom_id(
                    interaction_nonce,
                    VOID_SELECT_PREFIX,
                    session_id,
                )),
                CreateSelectMenuKind::String { options },
            )
            .placeholder(validated_component_placeholder_text(
                i18n::void_select_placeholder(),
            ))
            .min_values(1)
            .max_values(1),
        ),
        CreateActionRow::Buttons(vec![
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                VOID_CANCEL_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::void_cancel_label()))
            .style(ButtonStyle::Danger),
            CreateButton::new(validated_custom_id_owned(session_custom_id(
                interaction_nonce,
                VOID_NEXT_PREFIX,
                session_id,
            )))
            .label(validated_button_label_text(i18n::void_next_label()))
            .style(ButtonStyle::Primary)
            .disabled(selected_target.is_none()),
        ]),
    ])
}

fn void_confirmation_components(
    session_id: u64,
    target: LedgerEntryId,
    interaction_nonce: u64,
) -> Vec<CreateActionRow> {
    vec![CreateActionRow::Buttons(vec![
        CreateButton::new(validated_custom_id_owned(session_custom_id(
            interaction_nonce,
            VOID_RESELECT_PREFIX,
            session_id,
        )))
        .label(validated_button_label_text(i18n::void_reselect_label()))
        .style(ButtonStyle::Secondary),
        CreateButton::new(validated_custom_id_owned(session_custom_id(
            interaction_nonce,
            VOID_CANCEL_PREFIX,
            session_id,
        )))
        .label(validated_button_label_text(i18n::void_cancel_label()))
        .style(ButtonStyle::Danger),
        CreateButton::new(validated_custom_id_owned(void_confirm_custom_id(
            interaction_nonce,
            session_id,
            target,
        )))
        .label(validated_button_label_text(i18n::void_confirm_label()))
        .style(ButtonStyle::Danger),
    ])]
}

fn modal_values(modal: &ModalInteraction) -> HashMap<String, String> {
    let mut values = HashMap::new();
    for row in &modal.data.components {
        for component in &row.components {
            if let ActionRowComponent::InputText(input) = component
                && let Some(value) = input.value.clone()
            {
                values.insert(input.custom_id.clone(), value);
            }
        }
    }
    values
}

fn parse_amount(text: &str) -> Result<Money, ()> {
    let decimal = Decimal::from_str(text.trim()).map_err(|_| ())?;
    if decimal.fract() != Decimal::ZERO || decimal <= Decimal::ZERO {
        return Err(());
    }
    Ok(Money::from_decimal(decimal))
}

fn validate_expense_weight_configuration(
    participants: &[ExpenseParticipantSelection],
) -> Result<(), ExpenseEntryBuildError> {
    participants
        .iter()
        .any(|participant| participant.weight > Weight::ZERO)
        .then_some(())
        .ok_or(ExpenseEntryBuildError::InvalidWeightConfiguration)
}

fn non_empty_trimmed(value: &str) -> Option<String> {
    let trimmed = value.trim();
    (!trimmed.is_empty()).then_some(trimmed.to_owned())
}

fn resolve_expense_selections(
    draft: &ExpenseDraft,
    roster: &crate::discord::ports::RosterSnapshot,
) -> Vec<ExpenseParticipantSelection> {
    resolved_member_ids_from_draft(draft, Some(roster))
        .into_iter()
        .map(|member_id| ExpenseParticipantSelection {
            member_id,
            weight: draft
                .weight_overrides
                .get(&member_id)
                .copied()
                .unwrap_or(Weight(1)),
        })
        .collect()
}

fn confirmed_expense_selections(
    draft: &ExpenseDraft,
    roster: &crate::discord::ports::RosterSnapshot,
) -> Result<Vec<ExpenseParticipantSelection>, ConfirmedExpenseSelectionsError> {
    let current = resolve_expense_selections(draft, roster);
    match draft.frozen_participants.clone() {
        Some(confirmed) if confirmed == current => Ok(current),
        Some(_) => Err(ConfirmedExpenseSelectionsError::Drifted(current)),
        None => Err(ConfirmedExpenseSelectionsError::Missing),
    }
}

fn resolved_member_ids_from_draft(
    draft: &ExpenseDraft,
    roster: Option<&crate::discord::ports::RosterSnapshot>,
) -> Vec<MemberId> {
    let mut members: HashSet<MemberId> = draft.explicit_members.iter().copied().collect();
    if let Some(roster) = roster {
        for role_id in &draft.selected_roles {
            if let Some(role_members) = roster.role_members.get(role_id) {
                members.extend(role_members.iter().copied());
            }
        }
        if draft.include_members_group {
            members.extend(roster.member_ids.iter().copied());
        }
    }
    let mut ordered: Vec<MemberId> = members.into_iter().collect();
    ordered.sort_unstable();
    ordered
}

fn expense_weight_entries(
    draft: &ExpenseDraft,
    roster: &crate::discord::ports::RosterSnapshot,
    member_names: &HashMap<MemberId, SmolStr>,
) -> Vec<(MemberId, String, Weight)> {
    let labels = surface_member_labels(member_names);
    let mut selections = resolve_expense_selections(draft, roster);
    sort_expense_selections(&labels, &mut selections);
    selections
        .into_iter()
        .map(|selection| {
            (
                selection.member_id,
                safe_display_name(&labels, selection.member_id),
                selection.weight,
            )
        })
        .collect()
}

fn expense_weight_label_lookup(
    draft: &ExpenseDraft,
    roster: &crate::discord::ports::RosterSnapshot,
    member_names: &HashMap<MemberId, SmolStr>,
) -> HashMap<String, MemberId> {
    expense_weight_entries(draft, roster, member_names)
        .into_iter()
        .map(|(member_id, label, _)| (label, member_id))
        .collect()
}

fn expense_weight_modal_body(
    draft: &ExpenseDraft,
    roster: &crate::discord::ports::RosterSnapshot,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    expense_weight_entries(draft, roster, member_names)
        .into_iter()
        .map(|(_, label, weight)| format!("{label} = {}", weight.0))
        .collect::<Vec<_>>()
        .join("\n")
}

fn legacy_weight_member_id(label: &str) -> Option<MemberId> {
    let member_id = label
        .trim()
        .trim_end_matches(')')
        .rsplit_once('(')
        .map(|(_, member_id)| member_id)
        .unwrap_or(label)
        .trim()
        .parse::<u64>()
        .ok()?;
    Some(MemberId(member_id))
}

fn parse_weight_overrides(
    text: &str,
    valid_labels: &HashMap<String, MemberId>,
) -> Result<HashMap<MemberId, Weight>, ()> {
    let mut weights = HashMap::new();
    for line in text.lines().map(str::trim).filter(|line| !line.is_empty()) {
        let Some((left, right)) = line.rsplit_once('=') else {
            return Err(());
        };
        let member_id = valid_labels
            .get(left.trim())
            .copied()
            .or_else(|| legacy_weight_member_id(left))
            .ok_or(())?;
        let weight = right.trim().parse::<u64>().map_err(|_| ())?;
        if weights.insert(member_id, Weight(weight)).is_some() {
            return Err(());
        }
    }
    Ok(weights)
}

async fn fetch_all_channel_messages(
    ctx: &Context,
    channel_id: ChannelId,
) -> serenity::Result<Vec<Message>> {
    use serenity::builder::GetMessages;

    let mut all_messages = Vec::new();
    let mut last_message_id = None;
    loop {
        let mut builder = GetMessages::new().limit(100);
        if let Some(before) = last_message_id {
            builder = builder.before(before);
        }
        let messages = channel_id.messages(&ctx.http, builder).await?;
        if messages.is_empty() {
            break;
        }
        last_message_id = messages.last().map(|message| message.id);
        all_messages.extend(messages);
    }
    all_messages.reverse();
    Ok(all_messages)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseParticipantSelection {
    pub member_id: MemberId,
    pub weight: Weight,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordExpenseInput {
    pub payer: MemberId,
    pub amount: Money,
    pub participants: Vec<ExpenseParticipantSelection>,
    pub note: Option<String>,
    pub effective_date: Option<LedgerEffectiveDate>,
    pub recorded_by: MemberId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpenseEntryBuildError {
    EmptyParticipants,
    InvalidAmount,
    InvalidNote,
    InvalidSource,
    InvalidWeightConfiguration,
    LedgerConstruction,
}

fn expense_entry_build_error_message(error: &ExpenseEntryBuildError) -> &'static str {
    match error {
        ExpenseEntryBuildError::EmptyParticipants => i18n::expense_select_participants_message(),
        ExpenseEntryBuildError::InvalidAmount => i18n::expense_invalid_amount_message(),
        ExpenseEntryBuildError::InvalidWeightConfiguration => {
            i18n::expense_invalid_weight_configuration_message()
        }
        ExpenseEntryBuildError::InvalidNote
        | ExpenseEntryBuildError::InvalidSource
        | ExpenseEntryBuildError::LedgerConstruction => i18n::expense_build_failed_message(),
    }
}

fn map_expense_authoring_error(error: ExpenseAuthoringError) -> ExpenseEntryBuildError {
    match error {
        ExpenseAuthoringError::EmptyParticipants => ExpenseEntryBuildError::EmptyParticipants,
        ExpenseAuthoringError::InvalidAmount => ExpenseEntryBuildError::InvalidAmount,
        ExpenseAuthoringError::InvalidNote | ExpenseAuthoringError::NoteTooLong => {
            ExpenseEntryBuildError::InvalidNote
        }
        ExpenseAuthoringError::InvalidWeightConfiguration => {
            ExpenseEntryBuildError::InvalidWeightConfiguration
        }
        ExpenseAuthoringError::DuplicateParticipant { .. }
        | ExpenseAuthoringError::TooManyParticipants => ExpenseEntryBuildError::LedgerConstruction,
    }
}

fn map_discord_ledger_entry_error(error: DiscordLedgerEntryError) -> ExpenseEntryBuildError {
    match error {
        DiscordLedgerEntryError::InvalidSource(_) => ExpenseEntryBuildError::InvalidSource,
        DiscordLedgerEntryError::LedgerConstruction
        | DiscordLedgerEntryError::WrongSourceDescriptor => {
            ExpenseEntryBuildError::LedgerConstruction
        }
    }
}

fn build_live_expense_entry(
    entry_id: LedgerEntryId,
    input: &RecordExpenseInput,
) -> Result<LedgerEntry, ExpenseEntryBuildError> {
    let clock = PocSystemClock;
    let effective_date = input
        .effective_date
        .clone()
        .unwrap_or_else(|| clock.today_business_date());
    let resolved = ResolvedExpenseAuthoringInput::new(
        input.payer,
        input.amount,
        input.participants.iter().map(|selection| MemberWeight {
            member_id: selection.member_id,
            weight: selection.weight,
        }),
        input.note.clone(),
        effective_date,
        input.recorded_by,
    )
    .map_err(map_expense_authoring_error)?;
    let authored =
        RecordableExpenseAuthoring::new(resolved).map_err(map_expense_authoring_error)?;
    build_discord_expense_entry(
        entry_id,
        authored,
        DiscordLedgerSourceDescriptor::expense_slash_modal_v1(),
        &clock,
    )
    .map_err(map_discord_ledger_entry_error)
}

fn build_live_settlement_entry(
    entry_id: LedgerEntryId,
    recorded_by: MemberId,
    previewed: &PreviewedSettlement,
    binding: &PreviewConfirmationBinding,
) -> Result<Option<LedgerEntry>, SettlementRecordError> {
    let clock = PocSystemClock;
    record_previewed_plan_matching(
        entry_id,
        recorded_by,
        previewed.clone(),
        binding.clone(),
        DiscordLedgerSourceDescriptor::settle_thread_v1(),
        &clock,
    )
}

fn build_live_void_entry(
    entry_id: LedgerEntryId,
    recorded_by: MemberId,
    target: LedgerEntryId,
) -> Result<LedgerEntry, DiscordLedgerEntryError> {
    build_discord_void_entry(
        entry_id,
        recorded_by,
        target,
        DiscordLedgerSourceDescriptor::void_parent_v1(),
        &PocSystemClock,
    )
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn build_expense_entry(
    entry_id: LedgerEntryId,
    input: &RecordExpenseInput,
) -> Result<LedgerEntry, ExpenseEntryBuildError> {
    if input.participants.is_empty() {
        return Err(ExpenseEntryBuildError::EmptyParticipants);
    }

    let mut canonical_weights = input.participants.clone();
    canonical_weights.sort_by_key(|participant| participant.member_id);
    canonical_weights.dedup_by_key(|participant| participant.member_id);

    validate_expense_weight_configuration(&canonical_weights)?;
    let allocation_snapshot = allocation_snapshot_for(&canonical_weights)?;
    let owed_by = distribute_owed_amounts(&canonical_weights, input.amount)?;
    let note = input
        .note
        .as_deref()
        .map(ExpenseNote::new)
        .transpose()
        .map_err(|_| ExpenseEntryBuildError::InvalidNote)?;

    let event = ExpenseRecorded::new(
        vec![MemberAmount {
            member_id: input.payer,
            amount: input.amount,
        }],
        owed_by,
        note,
    )
    .map_err(|_| ExpenseEntryBuildError::LedgerConstruction)?;

    let mut entry = LedgerEntry::expense(entry_id, event, allocation_snapshot)
        .map_err(|_| ExpenseEntryBuildError::LedgerConstruction)?;
    entry.metadata.recorded_by = Some(input.recorded_by);
    entry.metadata.source = Some(
        LedgerSourceCanonical::discord_ui(EXPENSE_SOURCE_CANONICAL)
            .map_err(|_| ExpenseEntryBuildError::InvalidSource)?,
    );
    entry.metadata.effective_date = input.effective_date.clone();
    Ok(entry)
}

#[cfg_attr(not(test), allow(dead_code))]
fn preview_settlement_from_ledger(
    projected: &ProjectedLedger,
) -> Result<PreviewedSettlement, SettlementOptimizationError> {
    let settle_members: Vec<MemberId> = projected.state().participants().iter().copied().collect();
    SettleUpPolicy::preview(
        &HighsSettlementPlanner,
        projected.state().balances(),
        &settle_members,
        std::iter::empty(),
        SettlementContext::jpy_default(),
    )
    .map_err(Into::into)
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn build_settlement_entry(
    entry_id: LedgerEntryId,
    recorded_by: MemberId,
    previewed: &PreviewedSettlement,
) -> Result<Option<LedgerEntry>, SettlementOptimizationError> {
    let Some(event) = previewed.recordable_event().cloned() else {
        return Ok(None);
    };

    let mut entry = LedgerEntry::non_expense(entry_id, event);
    entry.metadata.recorded_by = Some(recorded_by);
    entry.metadata.source = Some(
        LedgerSourceCanonical::discord_ui(SETTLE_SOURCE_CANONICAL)
            .expect("static settle source should be valid"),
    );
    Ok(Some(entry))
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn build_void_entry(
    entry_id: LedgerEntryId,
    recorded_by: MemberId,
    target: LedgerEntryId,
) -> LedgerEntry {
    let mut entry = LedgerEntry::non_expense(entry_id, EntryVoided::new(target));
    entry.metadata.recorded_by = Some(recorded_by);
    entry.metadata.source = Some(
        LedgerSourceCanonical::discord_ui(VOID_SOURCE_CANONICAL)
            .expect("static void source should be valid"),
    );
    entry
}

pub fn next_entry_id<ExternalId>(
    verified: &[VerifiedLedgerStoreEnvelope<ExternalId>],
) -> LedgerEntryId {
    verified
        .last()
        .map(|envelope| LedgerEntryId(envelope.payload().entry.id.0 + 1))
        .unwrap_or(LedgerEntryId(1))
}

fn current_head_hash<ExternalId>(
    ledger_id: LedgerId,
    verified: &[VerifiedLedgerStoreEnvelope<ExternalId>],
) -> EntryHash {
    verified
        .last()
        .map(VerifiedLedgerStoreEnvelope::entry_hash)
        .unwrap_or_else(|| ledger_chain_genesis_sha256_v1(ledger_id))
}

fn interaction_state_expires_at() -> SystemTime {
    SystemTime::now() + std::time::Duration::from_secs(INTERACTION_STATE_TTL_SECS)
}

pub fn create_preview_binding(
    ledger_id: LedgerId,
    head_hash: EntryHash,
    actor_id: MemberId,
    previewed: &PreviewedSettlement,
    clock: &dyn Clock,
    nonce_provider: &dyn NonceProvider,
) -> PreviewConfirmationBinding {
    let created_at = clock.now();
    let expires_at = created_at + std::time::Duration::from_secs(PREVIEW_EXPIRY_SECS);
    PreviewConfirmationBinding::capture(
        nonce_provider.next_preview_instance_id(),
        ledger_id,
        head_hash,
        actor_id,
        created_at,
        expires_at,
        previewed,
    )
    .expect("system preview binding inputs should always be valid")
}

fn delivered_preview_binding(binding: &PreviewConfirmationBinding) -> PreviewConfirmationBinding {
    binding
        .clone()
        .mark_delivered(binding.preview_instance_id())
        .expect("fresh preview bindings should accept first delivery")
}

pub fn encode_discord_canonical_attachment(
    envelope: &UnverifiedLedgerStoreEnvelope<()>,
    pre_self_link_content: Option<&str>,
) -> Result<Vec<u8>, LedgerAttachmentError> {
    walicord_application::ledger::canonical_attachment::CanonicalAttachmentCodec::encode_with_pre_self_link_content(
        envelope,
        pre_self_link_content,
    )
    .map_err(|error| match error {
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::Clock => LedgerAttachmentError::Clock,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::JsonEncode(error) => LedgerAttachmentError::JsonEncode(error),
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::UnsupportedTransportVersion(version) => {
            LedgerAttachmentError::UnsupportedVersion(version)
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidHashLength => LedgerAttachmentError::InvalidHashLength,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidHashHex => LedgerAttachmentError::InvalidHashHex,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidMoney => LedgerAttachmentError::InvalidMoney,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidSourceKind(kind) => {
            LedgerAttachmentError::InvalidSourceKind(kind)
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidSource => LedgerAttachmentError::InvalidSource,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidEffectiveDate => {
            LedgerAttachmentError::InvalidEffectiveDate
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidRecordedAt => LedgerAttachmentError::InvalidRecordedAt,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidAllocationSnapshot => {
            LedgerAttachmentError::InvalidAllocationSnapshot
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidNote => LedgerAttachmentError::InvalidNote,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidExpenseEvent => {
            LedgerAttachmentError::InvalidExpenseEvent
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidSettlementEvent => {
            LedgerAttachmentError::InvalidSettlementEvent
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidAdjustmentReason => {
            LedgerAttachmentError::InvalidAdjustmentReason
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidAdjustmentEvent => {
            LedgerAttachmentError::InvalidAdjustmentEvent
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidHashSuite(suite) => {
            LedgerAttachmentError::InvalidHashSuite(suite)
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::JsonDecode(_)
        | walicord_application::ledger::canonical_attachment::AttachmentCodecError::UnreadableAttachment(_)
        | walicord_application::ledger::canonical_attachment::AttachmentCodecError::UnknownSchemaVersion { .. }
        | walicord_application::ledger::canonical_attachment::AttachmentCodecError::UnknownEventVariant { .. } => {
            unreachable!("attachment encoding should not emit decode-only errors")
        }
    })
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn decode_discord_canonical_attachment<ExternalId>(
    bytes: &[u8],
    external_id: ExternalId,
) -> Result<UnverifiedLedgerStoreEnvelope<ExternalId>, LedgerAttachmentError> {
    walicord_application::ledger::canonical_attachment::CanonicalAttachmentCodec::decode(bytes, external_id).map_err(|error| match error {
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::Clock => LedgerAttachmentError::Clock,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::JsonEncode(error) => LedgerAttachmentError::JsonEncode(error),
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::JsonDecode(error) => LedgerAttachmentError::JsonDecode(error),
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::UnsupportedTransportVersion(version)
        | walicord_application::ledger::canonical_attachment::AttachmentCodecError::UnknownSchemaVersion { version, .. } => {
            LedgerAttachmentError::UnsupportedVersion(version)
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidHashLength => LedgerAttachmentError::InvalidHashLength,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidHashHex => LedgerAttachmentError::InvalidHashHex,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidMoney => LedgerAttachmentError::InvalidMoney,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidSourceKind(kind) => {
            LedgerAttachmentError::InvalidSourceKind(kind)
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidSource => LedgerAttachmentError::InvalidSource,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidEffectiveDate => {
            LedgerAttachmentError::InvalidEffectiveDate
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidRecordedAt => LedgerAttachmentError::InvalidRecordedAt,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidAllocationSnapshot => {
            LedgerAttachmentError::InvalidAllocationSnapshot
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidNote => LedgerAttachmentError::InvalidNote,
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidExpenseEvent => {
            LedgerAttachmentError::InvalidExpenseEvent
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidSettlementEvent => {
            LedgerAttachmentError::InvalidSettlementEvent
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidAdjustmentReason => {
            LedgerAttachmentError::InvalidAdjustmentReason
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidAdjustmentEvent => {
            LedgerAttachmentError::InvalidAdjustmentEvent
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::InvalidHashSuite(suite) => {
            LedgerAttachmentError::InvalidHashSuite(suite)
        }
        walicord_application::ledger::canonical_attachment::AttachmentCodecError::UnreadableAttachment(_)
        | walicord_application::ledger::canonical_attachment::AttachmentCodecError::UnknownEventVariant { .. } => {
            unreachable!("in-memory attachment decode should not emit transport-read errors")
        }
    })
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn render_entry_message(
    entry: &LedgerEntry,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    let labels = surface_member_labels(member_names);
    match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => render_expense_message(entry, event, &labels),
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
            render_settlement_message(entry, event, &labels)
        }
        LedgerEvent::EntryVoided(event) => render_void_message(entry, event, member_names),
        LedgerEvent::LedgerHistorySealed(event) => render_seal_message(entry, event),
        LedgerEvent::BalanceAdjusted(event) => render_adjustment_message(entry, event, &labels),
    }
}

#[allow(dead_code)]
pub fn render_review_message(
    projected: &ProjectedLedger,
    previewed: &PreviewedSettlement,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    let labels = surface_member_labels(member_names);
    let mut out = String::from("📊 現在の清算プラン\n\n残高\n");
    out.push_str(&render_balances(projected.state(), &labels));
    out.push_str("\n\n送金リスト\n");
    if previewed.plan().transfers.is_empty() {
        out.push_str("清算対象はありません。");
    } else {
        for transfer in &previewed.plan().transfers {
            out.push_str(&format!(
                "- {} -> {}: {}円\n",
                safe_display_name(&labels, transfer.from),
                safe_display_name(&labels, transfer.to),
                transfer.amount
            ));
        }
    }
    out
}

#[allow(dead_code)]
pub fn render_ledger_summary(
    projected: &ProjectedLedger,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    let labels = surface_member_labels(member_names);
    let mut out = String::from("📚 台帳\n\n参加者\n");
    for member_id in projected.state().participants() {
        out.push_str(&format!("- {}\n", safe_display_name(&labels, *member_id)));
    }

    out.push_str("\n残高\n");
    out.push_str(&render_balances(projected.state(), &labels));

    out.push_str("\n\n取り消し済み記録\n");
    if projected.state().voided_entry_ids().is_empty() {
        out.push_str("なし");
    } else {
        for entry_id in projected.state().voided_entry_ids() {
            out.push_str(&format!("- #{}\n", entry_id.0));
        }
    }

    out.push_str("\n\n確認済み\n");
    match projected.state().sealed_through() {
        Some(entry_id) => out.push_str(&format!("#{}", entry_id.0)),
        None => out.push_str("なし"),
    }

    out
}

pub fn candidate_entries_for_void(
    entries: &[LedgerEntry],
    projected: &ProjectedLedger,
) -> Vec<LedgerEntry> {
    entries
        .iter()
        .rev()
        .filter(|entry| is_voidable_entry(entry, projected))
        .cloned()
        .collect()
}

fn void_window_entries(entries: &[LedgerEntry], projected: &ProjectedLedger) -> Vec<LedgerEntry> {
    entries
        .iter()
        .rev()
        .take(20)
        .filter(|entry| is_voidable_entry(entry, projected))
        .cloned()
        .collect()
}

fn is_voidable_entry(entry: &LedgerEntry, projected: &ProjectedLedger) -> bool {
    projected.entry(entry.id).is_some_and(|info| {
        matches!(
            info.kind,
            ProjectedEntryKind::Expense | ProjectedEntryKind::SettlementTransfer
        ) && !info.voided
            && !info.sealed
    })
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn summarize_entry_for_choice(
    entry: &LedgerEntry,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    let labels = surface_member_labels(member_names);
    summarize_entry_for_choice_with_labels(entry, &labels)
}

fn summarize_entry_for_choice_with_labels(
    entry: &LedgerEntry,
    labels: &SurfaceMemberLabels,
) -> String {
    match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => {
            let payer = event
                .paid_by()
                .first()
                .map(|paid| safe_display_name(labels, paid.member_id))
                .unwrap_or_else(unknown_display_name);
            format!(
                "#{} 経費 {}円 {payer}",
                entry.id.0,
                event
                    .paid_by()
                    .iter()
                    .map(|paid| paid.amount)
                    .sum::<Money>()
            )
        }
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
            let transfers = event
                .transfers()
                .iter()
                .map(|transfer| {
                    format!(
                        "{}→{} {}円",
                        safe_display_name(labels, transfer.from),
                        safe_display_name(labels, transfer.to),
                        transfer.amount
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("#{} 清算 {transfers}", entry.id.0)
        }
        _ => format!("#{}", entry.id.0),
    }
}

#[cfg_attr(not(test), allow(dead_code))]
fn render_expense_message(
    entry: &LedgerEntry,
    event: &ExpenseRecorded,
    labels: &SurfaceMemberLabels,
) -> String {
    let payer = event
        .paid_by()
        .first()
        .map(|paid| safe_display_name(labels, paid.member_id))
        .unwrap_or_else(unknown_display_name);
    let total = event
        .paid_by()
        .iter()
        .map(|paid| paid.amount)
        .sum::<Money>();
    let mut out = format!(
        "🧾 経費を記録しました\n\n#{}\n支払者: {payer}\n金額: {total}円\n",
        entry.id.0
    );

    if let Some(effective_date) = entry.metadata.effective_date.as_ref() {
        out.push_str(&format!("日付: {}\n", effective_date.as_str()));
    }

    out.push_str("対象:\n");
    match entry.metadata.allocation_snapshot.as_ref() {
        Some(AllocationSnapshot::Even) => {
            for owed in event.owed_by() {
                out.push_str(&format!(
                    "- {} x1\n",
                    safe_display_name(labels, owed.member_id)
                ));
            }
        }
        Some(AllocationSnapshot::Weighted { resolved_weights }) => {
            for MemberWeight { member_id, weight } in resolved_weights {
                out.push_str(&format!(
                    "- {} x{}\n",
                    safe_display_name(labels, *member_id),
                    weight.0
                ));
            }
        }
        Some(AllocationSnapshot::LegacyUnknown) | None => {
            for owed in event.owed_by() {
                out.push_str(&format!(
                    "- {}\n",
                    safe_display_name(labels, owed.member_id)
                ));
            }
        }
    }

    if let Some(note) = event
        .note()
        .and_then(|note| sanitized_note_text(note.as_str()))
    {
        out.push_str(&format!("\n{}", i18n::public_note_line(note.as_str())));
    }

    out
}

#[cfg_attr(not(test), allow(dead_code))]
fn render_settlement_message(
    entry: &LedgerEntry,
    event: &NormalizedSettlementPlanRecorded,
    labels: &SurfaceMemberLabels,
) -> String {
    let mut out = format!("💸 清算を記録しました\n\n記録: #{}\n", entry.id.0);
    for transfer in event.transfers() {
        out.push_str(&format!(
            "- {} -> {}: {}円\n",
            safe_display_name(labels, transfer.from),
            safe_display_name(labels, transfer.to),
            transfer.amount
        ));
    }
    out
}

#[cfg_attr(not(test), allow(dead_code))]
fn render_void_message(
    entry: &LedgerEntry,
    event: &EntryVoided,
    _member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    format!(
        "🪫 記録を取り消しました\n\n記録: #{}\n対象: #{}",
        entry.id.0,
        event.target().0
    )
}

#[cfg_attr(not(test), allow(dead_code))]
fn render_seal_message(entry: &LedgerEntry, event: &LedgerHistorySealed) -> String {
    format!(
        "🔒 履歴を確認済みにしました\n\n記録: #{}\n確認済み: #{} まで",
        entry.id.0,
        event.through().0
    )
}

#[cfg_attr(not(test), allow(dead_code))]
fn render_adjustment_message(
    entry: &LedgerEntry,
    event: &BalanceAdjusted,
    labels: &SurfaceMemberLabels,
) -> String {
    let mut out = format!(
        "🩹 残高補正を記録しました\n\n記録: #{}\n理由: {}\n",
        entry.id.0,
        sanitized_reason_text(event.reason().as_str())
    );
    for adjustment in event.adjustments() {
        out.push_str(&format!(
            "- {}: {}円\n",
            safe_display_name(labels, adjustment.member_id),
            adjustment.amount
        ));
    }
    out
}

fn render_balances(state: &LedgerState, labels: &SurfaceMemberLabels) -> String {
    if state.balances().is_empty() {
        return "全員の残高は 0 です。".into();
    }

    state
        .balances()
        .iter()
        .map(|(member_id, balance)| {
            format!("- {}: {balance}円", safe_display_name(labels, *member_id))
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn surface_member_labels(member_names: &HashMap<MemberId, SmolStr>) -> SurfaceMemberLabels {
    SurfaceMemberLabels::from_member_names(
        member_names
            .iter()
            .map(|(member_id, display_name)| (*member_id, Some(display_name.as_str()))),
    )
}

fn safe_display_name(labels: &SurfaceMemberLabels, member_id: MemberId) -> String {
    labels
        .member(member_id)
        .map(|label| label.visible().as_str().to_owned())
        .unwrap_or_else(|| {
            SafeLiteralText::from_roster_label(&i18n::unknown_user_label(member_id.0).to_string())
                .expect("fallback user label should sanitize")
                .into_inner()
        })
}

const EXPENSE_SUMMARY_LABEL_LIMIT: usize = 32;
const EXPENSE_SUMMARY_VISIBLE_LABELS: usize = 3;

fn truncate_expense_summary_label(label: &str) -> String {
    let actual = label.chars().count();
    if actual <= EXPENSE_SUMMARY_LABEL_LIMIT {
        return label.to_owned();
    }

    let prefix: String = label
        .chars()
        .take(EXPENSE_SUMMARY_LABEL_LIMIT.saturating_sub(1))
        .collect();
    format!("{prefix}…")
}

fn summarize_selected_role_labels(labels: &[SafeLiteralText]) -> Option<String> {
    if labels.is_empty() {
        return None;
    }

    let visible = labels
        .iter()
        .take(EXPENSE_SUMMARY_VISIBLE_LABELS)
        .map(|label| truncate_expense_summary_label(label.as_str()))
        .collect::<Vec<_>>();
    let mut summary = visible.join(", ");
    if labels.len() > EXPENSE_SUMMARY_VISIBLE_LABELS {
        summary.push_str(", ");
        summary.push_str(
            &i18n::additional_items(labels.len() - EXPENSE_SUMMARY_VISIBLE_LABELS).to_string(),
        );
    }
    Some(summary)
}

fn unknown_display_name() -> String {
    i18n::unknown_display_label().to_owned()
}

fn sanitized_note_text(raw: &str) -> Option<SafeLiteralText> {
    SafeLiteralText::from_note(raw)
}

#[cfg_attr(not(test), allow(dead_code))]
fn sanitized_reason_text(raw: &str) -> String {
    sanitized_note_text(raw)
        .map(SafeLiteralText::into_inner)
        .unwrap_or_else(|| i18n::expense_note_none().to_owned())
}

fn sort_expense_selections(
    labels: &SurfaceMemberLabels,
    selections: &mut [ExpenseParticipantSelection],
) {
    selections.sort_by(|lhs, rhs| labels.compare_members(lhs.member_id, rhs.member_id));
}

fn fallback_role_label(role_id: RoleId) -> SafeLiteralText {
    SafeLiteralText::from_roster_label(&i18n::unknown_role_label(role_id.0).to_string())
        .expect("fallback role label should sanitize")
}

fn roster_role_labels(
    ctx: &Context,
    guild_id: GuildId,
    roster: &crate::discord::ports::RosterSnapshot,
) -> HashMap<RoleId, SafeLiteralText> {
    let cached_guild = guild_id.to_guild_cached(&ctx.cache);
    roster
        .role_members
        .keys()
        .copied()
        .map(|role_id| {
            let label = cached_guild
                .as_ref()
                .and_then(|guild| guild.roles.get(&SerenityRoleId::new(role_id.0)))
                .and_then(|role| SafeLiteralText::from_roster_label(&role.name))
                .unwrap_or_else(|| fallback_role_label(role_id));
            (role_id, label)
        })
        .collect()
}

fn business_datetime_from_system_time(recorded_at: SystemTime) -> BusinessDateTime {
    BusinessDateTime::from_system_time(recorded_at)
}

fn effective_date_from_recorded_at(recorded_at: SystemTime) -> LedgerEffectiveDate {
    let recorded_at = chrono::DateTime::<Utc>::from(recorded_at);
    LedgerEffectiveDate::new(
        business_timezone()
            .timestamp_opt(
                recorded_at.timestamp(),
                recorded_at.timestamp_subsec_nanos(),
            )
            .single()
            .expect("business date should resolve")
            .format("%Y-%m-%d")
            .to_string(),
    )
    .expect("formatted effective date should stay valid")
}

fn business_timezone() -> FixedOffset {
    FixedOffset::east_opt(BUSINESS_TIMEZONE_OFFSET_SECONDS)
        .expect("business timezone offset should stay valid")
}

fn ledger_id_short(ledger_id: LedgerId) -> String {
    format!("{:08x}", ledger_id.0)
}

fn loaded_transport(
    loaded: &LoadedLedgerThread,
    entry_id: LedgerEntryId,
) -> Result<&LoadedTransportEntry, String> {
    loaded
        .transport_entries
        .get(&entry_id)
        .ok_or_else(|| format!("entry #{} is missing transport metadata", entry_id.0))
}

fn resolved_recorded_at(
    loaded: &LoadedLedgerThread,
    entry: &LedgerEntry,
) -> Result<SystemTime, String> {
    Ok(entry
        .metadata
        .recorded_at
        .unwrap_or(loaded_transport(loaded, entry.id)?.recorded_at))
}

fn recovery_reference_for(
    loaded: &LoadedLedgerThread,
    entry_id: LedgerEntryId,
    include_message_link: bool,
) -> Result<RecoveryReference, String> {
    Ok(RecoveryReference {
        ledger_id_short: ledger_id_short(loaded.ledger_id),
        entry_id,
        message_link: include_message_link
            .then(|| {
                loaded_transport(loaded, entry_id).map(|transport| transport.message_link.clone())
            })
            .transpose()?,
    })
}

fn confirmation_badges(
    draft: &ExpenseDraft,
    roster: &crate::discord::ports::RosterSnapshot,
    member_id: MemberId,
) -> Vec<ExpenseParticipantSourceBadge> {
    let mut badges = Vec::new();
    if draft.explicit_members.contains(&member_id) {
        badges.push(ExpenseParticipantSourceBadge::DirectSelection);
    }
    if draft.selected_roles.iter().any(|role_id| {
        roster
            .role_members
            .get(role_id)
            .is_some_and(|members| members.contains(&member_id))
    }) {
        badges.push(ExpenseParticipantSourceBadge::RoleExpansion);
    }
    if draft.include_members_group && roster.member_ids.contains(&member_id) {
        badges.push(ExpenseParticipantSourceBadge::AllMembers);
    }
    badges
}

fn entry_effective_date_for_surface(
    loaded: &LoadedLedgerThread,
    entry: &LedgerEntry,
) -> Result<LedgerEffectiveDate, String> {
    Ok(entry
        .metadata
        .effective_date
        .clone()
        .unwrap_or(effective_date_from_recorded_at(resolved_recorded_at(
            loaded, entry,
        )?)))
}

fn impact_summary_text(rows: &[BalanceImpactRow]) -> String {
    rows.iter()
        .map(|row| match row.direction {
            BalanceDirection::Receive => {
                i18n::impact_summary_receive(row.display_name.as_str(), &row.amount).to_string()
            }
            BalanceDirection::Pay => {
                i18n::impact_summary_pay(row.display_name.as_str(), &row.amount).to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn surface_summary_for_entry(
    loaded: &LoadedLedgerThread,
    entry: &LedgerEntry,
    labels: &SurfaceMemberLabels,
) -> Result<LedgerSurfaceSummary, String> {
    match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => Ok(LedgerSurfaceSummary::Expense {
            date: entry_effective_date_for_surface(loaded, entry)?,
            payer_display_name: event
                .paid_by()
                .first()
                .map(|paid| labels.safe_member_label(paid.member_id))
                .unwrap_or_else(unknown_member_label),
            amount: event
                .paid_by()
                .iter()
                .map(|paid| paid.amount)
                .sum::<Money>()
                .to_string(),
            note: event
                .note()
                .and_then(|note| sanitized_note_text(note.as_str())),
        }),
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
            let Some(first) = event.transfers().first() else {
                return Err(format!("settlement entry #{} has no transfers", entry.id.0));
            };
            Ok(LedgerSurfaceSummary::Settlement {
                date: effective_date_from_recorded_at(resolved_recorded_at(loaded, entry)?),
                from_display_name: labels.safe_member_label(first.from),
                to_display_name: labels.safe_member_label(first.to),
                amount: first.amount.to_string(),
                additional_transfers: event.transfers().len().saturating_sub(1),
            })
        }
        LedgerEvent::EntryVoided(_) => {
            let recorded_at = resolved_recorded_at(loaded, entry)?;
            Ok(LedgerSurfaceSummary::Void {
                date: effective_date_from_recorded_at(recorded_at),
                voider_display_name: labels.safe_actor_label(entry),
                recorded_at: business_datetime_from_system_time(recorded_at),
            })
        }
        LedgerEvent::LedgerHistorySealed(_) => Ok(LedgerSurfaceSummary::Sealed {
            date: effective_date_from_recorded_at(resolved_recorded_at(loaded, entry)?),
            actor_display_name: labels.safe_actor_label(entry),
        }),
        LedgerEvent::BalanceAdjusted(event) => {
            let impacts = balance_adjustment_rows(event, labels);
            Ok(LedgerSurfaceSummary::BalanceAdjustment {
                date: effective_date_from_recorded_at(resolved_recorded_at(loaded, entry)?),
                actor_display_name: labels.safe_actor_label(entry),
                impact_summary: SafeLiteralText::from_note(&impact_summary_text(&impacts))
                    .expect("impact summary should sanitize"),
            })
        }
    }
}

fn voided_rows_for_loaded(
    loaded: &LoadedLedgerThread,
    labels: &SurfaceMemberLabels,
) -> Result<Vec<VoidedEntryRow>, String> {
    loaded
        .entries
        .iter()
        .filter(|entry| matches!(entry.event, LedgerEvent::EntryVoided(_)))
        .map(|entry| {
            let LedgerEvent::EntryVoided(event) = &entry.event else {
                unreachable!("filtered to void entries")
            };
            let target = loaded
                .entries
                .iter()
                .find(|candidate| candidate.id == event.target())
                .ok_or_else(|| format!("void target #{} is missing", event.target().0))?;
            Ok(VoidedEntryRow {
                void_entry_id: entry.id,
                voider_display_name: labels.safe_actor_label(entry),
                voided_at: business_datetime_from_system_time(resolved_recorded_at(loaded, entry)?),
                original_summary: surface_summary_for_entry(loaded, target, labels)?,
                recovery_reference: recovery_reference_for(loaded, entry.id, true)?,
            })
        })
        .collect()
}

fn sealed_range_summary_for_loaded(
    loaded: &LoadedLedgerThread,
    labels: &SurfaceMemberLabels,
) -> Result<Option<SealedRangeSummary>, String> {
    let Some(through_entry_id) = loaded.projected.state().sealed_through() else {
        return Ok(None);
    };
    let through_entry = loaded
        .entries
        .iter()
        .find(|entry| entry.id == through_entry_id)
        .ok_or_else(|| format!("sealed-through entry #{} is missing", through_entry_id.0))?;
    Ok(Some(SealedRangeSummary {
        through_entry_id,
        through_summary: surface_summary_for_entry(loaded, through_entry, labels)?,
    }))
}

fn adjustment_summaries_for_loaded(
    loaded: &LoadedLedgerThread,
    labels: &SurfaceMemberLabels,
) -> Vec<BalanceAdjustmentSummary> {
    loaded
        .entries
        .iter()
        .filter_map(|entry| match &entry.event {
            LedgerEvent::BalanceAdjusted(event) => Some(BalanceAdjustmentSummary {
                actor_display_name: labels.safe_actor_label(entry),
                reason: sanitized_note_text(event.reason().as_str()).unwrap_or_else(|| {
                    SafeLiteralText::from_note(i18n::expense_note_none())
                        .expect("fallback reason should sanitize")
                }),
                impacts: balance_adjustment_rows(event, labels),
            }),
            _ => None,
        })
        .collect()
}

fn render_public_entry_message(
    loaded: &LoadedLedgerThread,
    entry: &LedgerEntry,
    member_names: &HashMap<MemberId, SmolStr>,
    message_link: Option<String>,
) -> Result<String, String> {
    let labels = surface_member_labels(member_names);
    let recorded_at = business_datetime_from_system_time(resolved_recorded_at(loaded, entry)?);
    let model = match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => {
            PublicCanonicalMessageModel::Expense(PublicExpenseMessageModel {
                entry_id: entry.id,
                effective_date: entry_effective_date_for_surface(loaded, entry)?,
                payer_display_name: event
                    .paid_by()
                    .first()
                    .map(|paid| labels.safe_member_label(paid.member_id))
                    .unwrap_or_else(unknown_member_label),
                amount: event
                    .paid_by()
                    .iter()
                    .map(|paid| paid.amount)
                    .sum::<Money>()
                    .to_string(),
                participant_rows: public_participant_rows(entry, &labels),
                note: event
                    .note()
                    .and_then(|note| sanitized_note_text(note.as_str())),
                actor_display_name: labels.safe_actor_label(entry),
                recorded_at,
                recovery_reference: RecoveryReference {
                    ledger_id_short: ledger_id_short(loaded.ledger_id),
                    entry_id: entry.id,
                    message_link: message_link.clone(),
                },
            })
        }
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
            PublicCanonicalMessageModel::Settlement(PublicSettlementMessageModel {
                entry_id: entry.id,
                recorded_date: effective_date_from_recorded_at(resolved_recorded_at(
                    loaded, entry,
                )?),
                transfers: event
                    .transfers()
                    .iter()
                    .map(|transfer| TransferRow {
                        from_display_name: labels.safe_member_label(transfer.from),
                        to_display_name: labels.safe_member_label(transfer.to),
                        amount: transfer.amount.to_string(),
                    })
                    .collect(),
                actor_display_name: labels.safe_actor_label(entry),
                recorded_at,
                recovery_reference: RecoveryReference {
                    ledger_id_short: ledger_id_short(loaded.ledger_id),
                    entry_id: entry.id,
                    message_link: message_link.clone(),
                },
            })
        }
        LedgerEvent::EntryVoided(event) => {
            let target = loaded
                .entries
                .iter()
                .find(|candidate| candidate.id == event.target())
                .ok_or_else(|| format!("void target #{} is missing", event.target().0))?;
            PublicCanonicalMessageModel::Void(PublicVoidMessageModel {
                entry_id: entry.id,
                voider_display_name: labels.safe_actor_label(entry),
                voided_at: recorded_at,
                original_summary: surface_summary_for_entry(loaded, target, &labels)?,
                recorded_at,
                recovery_reference: RecoveryReference {
                    ledger_id_short: ledger_id_short(loaded.ledger_id),
                    entry_id: entry.id,
                    message_link,
                },
            })
        }
        LedgerEvent::LedgerHistorySealed(event) => {
            let through_entry = loaded
                .entries
                .iter()
                .find(|candidate| candidate.id == event.through())
                .ok_or_else(|| format!("sealed-through entry #{} is missing", event.through().0))?;
            PublicCanonicalMessageModel::Seal(PublicSealMessageModel {
                entry_id: entry.id,
                through_entry_id: event.through(),
                through_summary: surface_summary_for_entry(loaded, through_entry, &labels)?,
                actor_display_name: labels.safe_actor_label(entry),
                recorded_at,
                recovery_reference: RecoveryReference {
                    ledger_id_short: ledger_id_short(loaded.ledger_id),
                    entry_id: entry.id,
                    message_link,
                },
            })
        }
        LedgerEvent::BalanceAdjusted(event) => {
            PublicCanonicalMessageModel::BalanceAdjustment(PublicBalanceAdjustmentMessageModel {
                entry_id: entry.id,
                actor_display_name: labels.safe_actor_label(entry),
                reason: sanitized_note_text(event.reason().as_str()).unwrap_or_else(|| {
                    SafeLiteralText::from_note(i18n::expense_note_none())
                        .expect("fallback reason should sanitize")
                }),
                impacts: balance_adjustment_rows(event, &labels),
                recorded_at,
                recovery_reference: RecoveryReference {
                    ledger_id_short: ledger_id_short(loaded.ledger_id),
                    entry_id: entry.id,
                    message_link,
                },
            })
        }
    };

    DiscordLedgerPresenter::render_public_entry(&model)
        .map(|rendered| rendered.body().to_owned())
        .map_err(|error| format!("台帳記録を表示できませんでした: {error:?}"))
}

#[cfg_attr(not(test), allow(dead_code))]
fn render_review_surface(
    loaded: &LoadedLedgerThread,
    previewed: &PreviewedSettlement,
    member_names: &HashMap<MemberId, SmolStr>,
    route: ReadViewRoute,
    recovery_cta: RecoveryCta,
    recovery_url: Option<String>,
    render_state: &ReadViewRenderState,
) -> Result<(String, Vec<CreateActionRow>), String> {
    let mut pages = render_review_surface_pages(
        loaded,
        previewed,
        member_names,
        route,
        recovery_cta,
        recovery_url,
        render_state,
    )?;
    pages
        .drain(..)
        .next()
        .map(|page| (page.body, page.base_components))
        .ok_or_else(|| "清算確認を表示できませんでした: ページがありません".to_owned())
}

fn render_read_view_page_models(
    page_models: Vec<ReadViewPageModel>,
) -> Result<Vec<ReadViewRenderedPage>, String> {
    page_models
        .into_iter()
        .map(|page_model| {
            DiscordLedgerPresenter::render_read_view_page(&page_model)
                .map(rendered_surface_to_message)
                .map(|(body, base_components)| ReadViewRenderedPage {
                    body,
                    base_components,
                })
                .map_err(|error| match page_model.kind {
                    ReadViewKind::Review => {
                        format!("清算確認を表示できませんでした: {error:?}")
                    }
                    ReadViewKind::Ledger => format!("台帳を表示できませんでした: {error:?}"),
                })
        })
        .collect()
}

fn render_review_surface_pages(
    loaded: &LoadedLedgerThread,
    previewed: &PreviewedSettlement,
    member_names: &HashMap<MemberId, SmolStr>,
    route: ReadViewRoute,
    recovery_cta: RecoveryCta,
    recovery_url: Option<String>,
    render_state: &ReadViewRenderState,
) -> Result<Vec<ReadViewRenderedPage>, String> {
    let labels = surface_member_labels(member_names);
    render_read_view_page_models(
        walicord_presentation::discord_ledger::paginate_read_view_model(ReadViewPageModel {
            kind: ReadViewKind::Review,
            route,
            title: i18n::panel_review_button_label().to_owned(),
            uncertain_write: render_state.uncertain_write,
            stale_page: render_state.stale_page,
            page_indicator: render_state.page_indicator.clone(),
            snapshot_notice: render_state.snapshot_notice.clone(),
            route_guidance_lines: Vec::new(),
            recovery_cta,
            recovery_url,
            missing_thread_note: false,
            balances: balance_rows_for_state(loaded.projected.state(), &labels),
            transfers: preview_transfer_rows(previewed, &labels),
            participants: Vec::new(),
            voided_entries: Vec::new(),
            sealed_range: None,
            balance_adjustments: Vec::new(),
            footer_lines: Vec::new(),
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        }),
    )
}

fn render_review_empty_state(
    route: ReadViewRoute,
    recovery_url: String,
    render_state: &ReadViewRenderState,
) -> Result<(String, Vec<CreateActionRow>), String> {
    let (empty_state, recovery_cta, recovery_url) = match route {
        ReadViewRoute::ReviewParent => (
            i18n::review_parent_empty_state().to_owned(),
            RecoveryCta::None,
            None,
        ),
        _ => (
            i18n::review_thread_empty_state().to_owned(),
            RecoveryCta::ParentLink,
            Some(recovery_url),
        ),
    };
    DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel {
        kind: ReadViewKind::Review,
        route,
        title: i18n::panel_review_button_label().to_owned(),
        uncertain_write: render_state.uncertain_write,
        stale_page: render_state.stale_page,
        page_indicator: render_state.page_indicator.clone(),
        snapshot_notice: render_state.snapshot_notice.clone(),
        route_guidance_lines: Vec::new(),
        recovery_cta,
        recovery_url,
        missing_thread_note: false,
        balances: Vec::new(),
        transfers: Vec::new(),
        participants: Vec::new(),
        voided_entries: Vec::new(),
        sealed_range: None,
        balance_adjustments: Vec::new(),
        footer_lines: Vec::new(),
        visible_sections: ReadViewSectionVisibility::default(),
        empty_state: Some(empty_state),
        action_rows: Vec::new(),
        ephemeral: true,
    })
    .map(rendered_surface_to_message)
    .map_err(|error| format!("清算確認を表示できませんでした: {error:?}"))
}

fn render_review_no_transfer_state(
    render_state: &ReadViewRenderState,
) -> Result<(String, Vec<CreateActionRow>), String> {
    let mut body = vec![i18n::panel_review_button_label().to_owned()];
    if render_state.uncertain_write {
        body.push(String::new());
        body.push(i18n::read_uncertain_write_advisory().to_owned());
    }
    body.push(String::new());
    body.push(i18n::settlement_already_not_needed_message().to_owned());
    body.push(i18n::settlement_preview_not_saved_message().to_owned());
    body.push(String::new());
    body.push(i18n::balances_heading().to_owned());
    body.push(i18n::review_zero_balances().to_owned());
    RenderedSurface::new(body.join("\n"), Vec::new(), true)
        .map(rendered_surface_to_message)
        .map_err(|error| format!("清算確認を表示できませんでした: {error:?}"))
}

#[cfg_attr(not(test), allow(dead_code))]
fn render_ledger_surface(
    loaded: &LoadedLedgerThread,
    member_names: &HashMap<MemberId, SmolStr>,
    route: ReadViewRoute,
    render_state: &ReadViewRenderState,
) -> Result<String, String> {
    let mut pages = render_ledger_surface_pages(loaded, member_names, route, render_state)?;
    pages
        .drain(..)
        .next()
        .map(|page| page.body)
        .ok_or_else(|| "台帳を表示できませんでした: ページがありません".to_owned())
}

fn render_ledger_surface_pages(
    loaded: &LoadedLedgerThread,
    member_names: &HashMap<MemberId, SmolStr>,
    route: ReadViewRoute,
    render_state: &ReadViewRenderState,
) -> Result<Vec<ReadViewRenderedPage>, String> {
    let labels = surface_member_labels(member_names);
    render_read_view_page_models(
        walicord_presentation::discord_ledger::paginate_read_view_model(ReadViewPageModel {
            kind: ReadViewKind::Ledger,
            route,
            title: i18n::panel_ledger_button_label().to_owned(),
            uncertain_write: render_state.uncertain_write,
            stale_page: render_state.stale_page,
            page_indicator: render_state.page_indicator.clone(),
            snapshot_notice: render_state.snapshot_notice.clone(),
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            balances: balance_rows_for_state(loaded.projected.state(), &labels),
            transfers: Vec::new(),
            participants: participant_names_for_state(loaded.projected.state(), &labels),
            voided_entries: voided_rows_for_loaded(loaded, &labels)?,
            sealed_range: sealed_range_summary_for_loaded(loaded, &labels)?,
            balance_adjustments: adjustment_summaries_for_loaded(loaded, &labels),
            footer_lines: Vec::new(),
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        }),
    )
}

fn render_ledger_empty_state(
    route: ReadViewRoute,
    render_state: &ReadViewRenderState,
) -> Result<String, String> {
    DiscordLedgerPresenter::render_read_view_page(&ReadViewPageModel {
        kind: ReadViewKind::Ledger,
        route,
        title: i18n::panel_ledger_button_label().to_owned(),
        uncertain_write: render_state.uncertain_write,
        stale_page: render_state.stale_page,
        page_indicator: render_state.page_indicator.clone(),
        snapshot_notice: render_state.snapshot_notice.clone(),
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
        footer_lines: Vec::new(),
        visible_sections: ReadViewSectionVisibility::default(),
        empty_state: Some(i18n::ledger_empty_state().to_owned()),
        action_rows: Vec::new(),
        ephemeral: true,
    })
    .map(|rendered| rendered.body)
    .map_err(|error| format!("台帳を表示できませんでした: {error:?}"))
}

fn channel_link(guild_id: GuildId, channel_id: ChannelId) -> String {
    format!(
        "https://discord.com/channels/{}/{}",
        guild_id.get(),
        channel_id.get()
    )
}

fn writer_lineage_policy_from_env(
    active_writer: UserId,
) -> Result<store::WriterLineagePolicy, String> {
    let raw_allowlist = env::var(WRITER_LINEAGE_ALLOWLIST_ENV).map_err(|error| match error {
        env::VarError::NotPresent => {
            format!("{WRITER_LINEAGE_ALLOWLIST_ENV} is not set")
        }
        env::VarError::NotUnicode(_) => {
            format!("{WRITER_LINEAGE_ALLOWLIST_ENV} must be valid UTF-8")
        }
    })?;
    let approved_writers = raw_allowlist
        .split(',')
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(|value| {
            value.parse::<u64>().map(UserId::new).map_err(|_| {
                format!("invalid bot user id in {WRITER_LINEAGE_ALLOWLIST_ENV}: {value}")
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    store::WriterLineagePolicy::load(Some(active_writer), Some(approved_writers))
        .map_err(|error| format!("writer lineage policy を初期化できませんでした: {error}"))
}

fn stale_read_view_message_for_body(body: &str) -> &'static str {
    if body.starts_with(i18n::panel_ledger_button_label()) {
        i18n::stale_ledger_page_message()
    } else {
        i18n::stale_review_page_message()
    }
}

#[cfg_attr(not(test), allow(dead_code))]
fn allocation_snapshot_for(
    participants: &[ExpenseParticipantSelection],
) -> Result<AllocationSnapshot, ExpenseEntryBuildError> {
    if participants
        .iter()
        .all(|participant| participant.weight == Weight(1))
    {
        return Ok(AllocationSnapshot::Even);
    }

    AllocationSnapshot::weighted(participants.iter().map(|participant| MemberWeight {
        member_id: participant.member_id,
        weight: participant.weight,
    }))
    .map_err(|_| ExpenseEntryBuildError::InvalidWeightConfiguration)
}

#[cfg_attr(not(test), allow(dead_code))]
fn distribute_owed_amounts(
    participants: &[ExpenseParticipantSelection],
    amount: Money,
) -> Result<Vec<MemberAmount>, ExpenseEntryBuildError> {
    let total_units = SettlementContext::jpy_default()
        .to_atomic_units_i64(amount)
        .map_err(|_| ExpenseEntryBuildError::InvalidAmount)?;
    let total_units =
        u64::try_from(total_units).map_err(|_| ExpenseEntryBuildError::InvalidAmount)?;

    let mut units_per_member = vec![0_u64; participants.len()];
    let positive_indices: Vec<usize> = participants
        .iter()
        .enumerate()
        .filter_map(|(index, participant)| (participant.weight > Weight::ZERO).then_some(index))
        .collect();
    if positive_indices.is_empty() {
        return Err(ExpenseEntryBuildError::InvalidWeightConfiguration);
    }

    let total_weight = positive_indices
        .iter()
        .map(|index| u128::from(participants[*index].weight.0))
        .sum::<u128>();
    if total_weight == 0 {
        return Err(ExpenseEntryBuildError::InvalidWeightConfiguration);
    }

    let total_units_u128 = u128::from(total_units);
    let mut assigned = 0_u64;
    for index in &positive_indices {
        let share = (total_units_u128 * u128::from(participants[*index].weight.0)) / total_weight;
        let share = u64::try_from(share).map_err(|_| ExpenseEntryBuildError::InvalidAmount)?;
        units_per_member[*index] = share;
        assigned += share;
    }

    let remainder = total_units.saturating_sub(assigned);
    for index in positive_indices
        .into_iter()
        .take(usize::try_from(remainder).unwrap_or(0))
    {
        units_per_member[index] += 1;
    }

    Ok(participants
        .iter()
        .zip(units_per_member)
        .filter_map(|(participant, units)| {
            (units > 0).then_some(MemberAmount {
                member_id: participant.member_id,
                amount: Money::from_i64(i64::try_from(units).expect("u64 units should fit in i64")),
            })
        })
        .collect())
}

#[derive(Debug, Serialize, Deserialize)]
#[allow(dead_code)]
struct DiscordLedgerAttachmentDto {
    version: u32,
    transport: TransportMetadataDto,
    envelope: EnvelopeDto,
}

#[derive(Debug, Serialize, Deserialize)]
#[allow(dead_code)]
struct TransportMetadataDto {
    recorded_at_unix_ms: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pre_self_link_content_sha256: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct EnvelopeDto {
    previous_hash: String,
    entry_hash: String,
    payload: PayloadDto,
}

#[allow(dead_code)]
impl EnvelopeDto {
    fn from_unverified(
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
    ) -> Result<Self, LedgerAttachmentError> {
        Ok(Self {
            previous_hash: encode_hash(envelope.previous_hash),
            entry_hash: encode_hash(envelope.entry_hash),
            payload: PayloadDto::from_payload(&envelope.payload)?,
        })
    }

    fn into_unverified<ExternalId>(
        self,
        external_id: ExternalId,
    ) -> Result<UnverifiedLedgerStoreEnvelope<ExternalId>, LedgerAttachmentError> {
        Ok(UnverifiedLedgerStoreEnvelope {
            previous_hash: decode_hash(&self.previous_hash)?,
            entry_hash: decode_hash(&self.entry_hash)?,
            external_id,
            payload: self.payload.into_payload()?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct PayloadDto {
    ledger_id: u64,
    schema_version: u32,
    hash_suite: String,
    entry: EntryDto,
}

#[allow(dead_code)]
impl PayloadDto {
    fn from_payload(
        payload: &walicord_application::ledger::HashedLedgerPayload,
    ) -> Result<Self, LedgerAttachmentError> {
        Ok(Self {
            ledger_id: payload.ledger_id.0,
            schema_version: payload.schema_version.0,
            hash_suite: hash_suite_name(payload.hash_suite).into(),
            entry: EntryDto::from_entry(&payload.entry)?,
        })
    }

    fn into_payload(
        self,
    ) -> Result<walicord_application::ledger::HashedLedgerPayload, LedgerAttachmentError> {
        Ok(walicord_application::ledger::HashedLedgerPayload {
            ledger_id: LedgerId(self.ledger_id),
            schema_version: SchemaVersion(self.schema_version),
            hash_suite: parse_hash_suite(&self.hash_suite)?,
            entry: self.entry.into_entry()?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct EntryDto {
    id: u64,
    metadata: MetadataDto,
    event: EventDto,
}

#[allow(dead_code)]
impl EntryDto {
    fn from_entry(entry: &LedgerEntry) -> Result<Self, LedgerAttachmentError> {
        Ok(Self {
            id: entry.id.0,
            metadata: MetadataDto::from_metadata(&entry.metadata)?,
            event: EventDto::from_event(&entry.event),
        })
    }

    fn into_entry(self) -> Result<LedgerEntry, LedgerAttachmentError> {
        Ok(LedgerEntry {
            id: LedgerEntryId(self.id),
            metadata: self.metadata.into_metadata()?,
            event: self.event.into_event()?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct MetadataDto {
    recorded_by: Option<u64>,
    source: Option<SourceDto>,
    effective_date: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    recorded_at_unix_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    allocation_snapshot: Option<AllocationSnapshotDto>,
}

#[allow(dead_code)]
impl MetadataDto {
    fn from_metadata(metadata: &LedgerEntryMetadata) -> Result<Self, LedgerAttachmentError> {
        Ok(Self {
            recorded_by: metadata.recorded_by.map(|member_id| member_id.0),
            source: metadata.source.as_ref().map(SourceDto::from_source),
            effective_date: metadata
                .effective_date
                .as_ref()
                .map(|effective_date| effective_date.as_str().to_owned()),
            recorded_at_unix_ms: metadata
                .recorded_at
                .map(system_time_to_unix_ms)
                .transpose()?,
            allocation_snapshot: metadata
                .allocation_snapshot
                .as_ref()
                .map(AllocationSnapshotDto::from_snapshot),
        })
    }

    fn into_metadata(self) -> Result<LedgerEntryMetadata, LedgerAttachmentError> {
        Ok(LedgerEntryMetadata {
            recorded_by: self.recorded_by.map(MemberId),
            source: self.source.map(SourceDto::into_source).transpose()?,
            effective_date: self
                .effective_date
                .map(LedgerEffectiveDate::new)
                .transpose()
                .map_err(|_| LedgerAttachmentError::InvalidEffectiveDate)?,
            recorded_at: self
                .recorded_at_unix_ms
                .map(unix_ms_to_system_time)
                .transpose()?,
            allocation_snapshot: self
                .allocation_snapshot
                .map(AllocationSnapshotDto::into_snapshot)
                .transpose()?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct SourceDto {
    kind: String,
    canonical: String,
}

#[allow(dead_code)]
impl SourceDto {
    fn from_source(source: &LedgerSourceCanonical) -> Self {
        Self {
            kind: source_kind_name(source.kind()).into(),
            canonical: source.canonical_text().to_owned(),
        }
    }

    fn into_source(self) -> Result<LedgerSourceCanonical, LedgerAttachmentError> {
        match self.kind.as_str() {
            "legacy_dsl" => LedgerSourceCanonical::legacy_dsl(self.canonical),
            "discord_ui" => LedgerSourceCanonical::discord_ui(self.canonical),
            _ => return Err(LedgerAttachmentError::InvalidSourceKind(self.kind)),
        }
        .map_err(|_| LedgerAttachmentError::InvalidSource)
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum AllocationSnapshotDto {
    Even,
    Weighted {
        resolved_weights: Vec<MemberWeightDto>,
    },
    LegacyUnknown,
}

#[allow(dead_code)]
impl AllocationSnapshotDto {
    fn from_snapshot(snapshot: &AllocationSnapshot) -> Self {
        match snapshot {
            AllocationSnapshot::Even => Self::Even,
            AllocationSnapshot::Weighted { resolved_weights } => Self::Weighted {
                resolved_weights: resolved_weights
                    .iter()
                    .map(|weight| MemberWeightDto {
                        member_id: weight.member_id.0,
                        weight: weight.weight.0,
                    })
                    .collect(),
            },
            AllocationSnapshot::LegacyUnknown => Self::LegacyUnknown,
        }
    }

    fn into_snapshot(self) -> Result<AllocationSnapshot, LedgerAttachmentError> {
        match self {
            Self::Even => Ok(AllocationSnapshot::Even),
            Self::Weighted { resolved_weights } => {
                AllocationSnapshot::weighted(resolved_weights.into_iter().map(|weight| {
                    MemberWeight {
                        member_id: MemberId(weight.member_id),
                        weight: Weight(weight.weight),
                    }
                }))
                .map_err(|_| LedgerAttachmentError::InvalidAllocationSnapshot)
            }
            Self::LegacyUnknown => Ok(AllocationSnapshot::LegacyUnknown),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct MemberWeightDto {
    member_id: u64,
    weight: u64,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum EventDto {
    ExpenseRecorded {
        paid_by: Vec<MemberAmountDto>,
        owed_by: Vec<MemberAmountDto>,
        note: Option<String>,
    },
    NormalizedSettlementPlanRecorded {
        transfers: Vec<TransferDto>,
    },
    LedgerHistorySealed {
        through: u64,
    },
    EntryVoided {
        target: u64,
    },
    BalanceAdjusted {
        adjustments: Vec<BalanceAdjustmentDto>,
        reason: String,
        source: BalanceAdjustmentSourceDto,
    },
}

#[allow(dead_code)]
impl EventDto {
    fn from_event(event: &LedgerEvent) -> Self {
        match event {
            LedgerEvent::ExpenseRecorded(event) => Self::ExpenseRecorded {
                paid_by: event
                    .paid_by()
                    .iter()
                    .map(MemberAmountDto::from_amount)
                    .collect(),
                owed_by: event
                    .owed_by()
                    .iter()
                    .map(MemberAmountDto::from_amount)
                    .collect(),
                note: event.note().map(|note| note.as_str().to_owned()),
            },
            LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
                Self::NormalizedSettlementPlanRecorded {
                    transfers: event
                        .transfers()
                        .iter()
                        .map(TransferDto::from_transfer)
                        .collect(),
                }
            }
            LedgerEvent::LedgerHistorySealed(event) => Self::LedgerHistorySealed {
                through: event.through().0,
            },
            LedgerEvent::EntryVoided(event) => Self::EntryVoided {
                target: event.target().0,
            },
            LedgerEvent::BalanceAdjusted(event) => Self::BalanceAdjusted {
                adjustments: event
                    .adjustments()
                    .iter()
                    .map(BalanceAdjustmentDto::from_adjustment)
                    .collect(),
                reason: event.reason().as_str().to_owned(),
                source: BalanceAdjustmentSourceDto::from_source(event.source()),
            },
        }
    }

    fn into_event(self) -> Result<LedgerEvent, LedgerAttachmentError> {
        match self {
            Self::ExpenseRecorded {
                paid_by,
                owed_by,
                note,
            } => Ok(LedgerEvent::ExpenseRecorded(
                ExpenseRecorded::new(
                    paid_by
                        .into_iter()
                        .map(MemberAmountDto::into_amount)
                        .collect::<Result<_, _>>()?,
                    owed_by
                        .into_iter()
                        .map(MemberAmountDto::into_amount)
                        .collect::<Result<_, _>>()?,
                    note.map(ExpenseNote::new)
                        .transpose()
                        .map_err(|_| LedgerAttachmentError::InvalidNote)?,
                )
                .map_err(|_| LedgerAttachmentError::InvalidExpenseEvent)?,
            )),
            Self::NormalizedSettlementPlanRecorded { transfers } => {
                Ok(LedgerEvent::NormalizedSettlementPlanRecorded(
                    NormalizedSettlementPlanRecorded::new(
                        transfers
                            .into_iter()
                            .map(TransferDto::into_transfer)
                            .collect::<Result<_, _>>()?,
                    )
                    .map_err(|_| LedgerAttachmentError::InvalidSettlementEvent)?,
                ))
            }
            Self::LedgerHistorySealed { through } => Ok(LedgerEvent::LedgerHistorySealed(
                LedgerHistorySealed::new(LedgerEntryId(through)),
            )),
            Self::EntryVoided { target } => Ok(LedgerEvent::EntryVoided(EntryVoided::new(
                LedgerEntryId(target),
            ))),
            Self::BalanceAdjusted {
                adjustments,
                reason,
                source,
            } => Ok(LedgerEvent::BalanceAdjusted(
                BalanceAdjusted::new(
                    adjustments
                        .into_iter()
                        .map(BalanceAdjustmentDto::into_adjustment)
                        .collect::<Result<_, _>>()?,
                    AdjustmentReason::new(reason)
                        .map_err(|_| LedgerAttachmentError::InvalidAdjustmentReason)?,
                    source.into_source()?,
                )
                .map_err(|_| LedgerAttachmentError::InvalidAdjustmentEvent)?,
            )),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct MemberAmountDto {
    member_id: u64,
    amount: String,
}

#[allow(dead_code)]
impl MemberAmountDto {
    fn from_amount(amount: &MemberAmount) -> Self {
        Self {
            member_id: amount.member_id.0,
            amount: encode_money(amount.amount),
        }
    }

    fn into_amount(self) -> Result<MemberAmount, LedgerAttachmentError> {
        Ok(MemberAmount {
            member_id: MemberId(self.member_id),
            amount: parse_money(&self.amount)?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct TransferDto {
    from: u64,
    to: u64,
    amount: String,
}

#[allow(dead_code)]
impl TransferDto {
    fn from_transfer(transfer: &Transfer) -> Self {
        Self {
            from: transfer.from.0,
            to: transfer.to.0,
            amount: encode_money(transfer.amount),
        }
    }

    fn into_transfer(self) -> Result<Transfer, LedgerAttachmentError> {
        Ok(Transfer {
            from: MemberId(self.from),
            to: MemberId(self.to),
            amount: parse_money(&self.amount)?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct BalanceAdjustmentDto {
    member_id: u64,
    amount: String,
}

#[allow(dead_code)]
impl BalanceAdjustmentDto {
    fn from_adjustment(adjustment: &BalanceAdjustment) -> Self {
        Self {
            member_id: adjustment.member_id.0,
            amount: encode_money(adjustment.amount),
        }
    }

    fn into_adjustment(self) -> Result<BalanceAdjustment, LedgerAttachmentError> {
        Ok(BalanceAdjustment {
            member_id: MemberId(self.member_id),
            amount: parse_money(&self.amount)?,
        })
    }
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum BalanceAdjustmentSourceDto {
    SealedEntryCorrection { entry_id: u64 },
    PriorAdjustmentCorrection { entry_id: u64 },
    ExternalCorrection,
}

#[allow(dead_code)]
impl BalanceAdjustmentSourceDto {
    fn from_source(source: &BalanceAdjustmentSource) -> Self {
        match source {
            BalanceAdjustmentSource::SealedEntryCorrection(entry_id) => {
                Self::SealedEntryCorrection {
                    entry_id: entry_id.0,
                }
            }
            BalanceAdjustmentSource::PriorAdjustmentCorrection(entry_id) => {
                Self::PriorAdjustmentCorrection {
                    entry_id: entry_id.0,
                }
            }
            BalanceAdjustmentSource::ExternalCorrection(_) => Self::ExternalCorrection,
        }
    }

    fn into_source(self) -> Result<BalanceAdjustmentSource, LedgerAttachmentError> {
        match self {
            Self::SealedEntryCorrection { entry_id } => Ok(
                BalanceAdjustmentSource::SealedEntryCorrection(LedgerEntryId(entry_id)),
            ),
            Self::PriorAdjustmentCorrection { entry_id } => Ok(
                BalanceAdjustmentSource::PriorAdjustmentCorrection(LedgerEntryId(entry_id)),
            ),
            Self::ExternalCorrection => Ok(external_correction_source_for_transport_decode()),
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum LedgerAttachmentError {
    Clock,
    JsonEncode(serde_json::Error),
    JsonDecode(serde_json::Error),
    UnsupportedVersion(u32),
    InvalidHashLength,
    InvalidHashHex,
    InvalidMoney,
    InvalidSourceKind(String),
    InvalidSource,
    InvalidEffectiveDate,
    InvalidRecordedAt,
    InvalidAllocationSnapshot,
    InvalidNote,
    InvalidExpenseEvent,
    InvalidSettlementEvent,
    InvalidAdjustmentReason,
    InvalidAdjustmentEvent,
    InvalidHashSuite(String),
}

impl std::fmt::Display for LedgerAttachmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Clock => write!(f, "system clock error"),
            Self::JsonEncode(error) => write!(f, "failed to encode canonical attachment: {error}"),
            Self::JsonDecode(error) => write!(f, "failed to decode canonical attachment: {error}"),
            Self::UnsupportedVersion(version) => {
                write!(f, "unsupported canonical attachment version: {version}")
            }
            Self::InvalidHashLength => write!(f, "invalid hash length"),
            Self::InvalidHashHex => write!(f, "invalid hash hex"),
            Self::InvalidMoney => write!(f, "invalid money value"),
            Self::InvalidSourceKind(kind) => write!(f, "invalid source kind: {kind}"),
            Self::InvalidSource => write!(f, "invalid source"),
            Self::InvalidEffectiveDate => write!(f, "invalid effective date"),
            Self::InvalidRecordedAt => write!(f, "invalid recorded_at timestamp"),
            Self::InvalidAllocationSnapshot => write!(f, "invalid allocation snapshot"),
            Self::InvalidNote => write!(f, "invalid note"),
            Self::InvalidExpenseEvent => write!(f, "invalid expense event"),
            Self::InvalidSettlementEvent => write!(f, "invalid settlement event"),
            Self::InvalidAdjustmentReason => write!(f, "invalid adjustment reason"),
            Self::InvalidAdjustmentEvent => write!(f, "invalid adjustment event"),
            Self::InvalidHashSuite(suite) => write!(f, "invalid hash suite: {suite}"),
        }
    }
}

impl std::error::Error for LedgerAttachmentError {}

fn encode_hash(hash: EntryHash) -> String {
    hash.0.iter().map(|byte| format!("{byte:02x}")).collect()
}

#[allow(dead_code)]
fn decode_hash(hex: &str) -> Result<EntryHash, LedgerAttachmentError> {
    if hex.len() != 64 {
        return Err(LedgerAttachmentError::InvalidHashLength);
    }

    let mut out = [0_u8; 32];
    for (index, chunk) in hex.as_bytes().chunks_exact(2).enumerate() {
        let text = std::str::from_utf8(chunk).map_err(|_| LedgerAttachmentError::InvalidHashHex)?;
        out[index] =
            u8::from_str_radix(text, 16).map_err(|_| LedgerAttachmentError::InvalidHashHex)?;
    }
    Ok(EntryHash(out))
}

#[allow(dead_code)]
fn system_time_to_unix_ms(value: SystemTime) -> Result<u64, LedgerAttachmentError> {
    value
        .duration_since(UNIX_EPOCH)
        .map_err(|_| LedgerAttachmentError::InvalidRecordedAt)?
        .as_millis()
        .try_into()
        .map_err(|_| LedgerAttachmentError::InvalidRecordedAt)
}

#[allow(dead_code)]
fn unix_ms_to_system_time(value: u64) -> Result<SystemTime, LedgerAttachmentError> {
    UNIX_EPOCH
        .checked_add(std::time::Duration::from_millis(value))
        .ok_or(LedgerAttachmentError::InvalidRecordedAt)
}

#[allow(dead_code)]
fn encode_money(money: Money) -> String {
    money.as_decimal().normalize().to_string()
}

#[allow(dead_code)]
fn parse_money(text: &str) -> Result<Money, LedgerAttachmentError> {
    let decimal = Decimal::from_str(text).map_err(|_| LedgerAttachmentError::InvalidMoney)?;
    Ok(Money::from_decimal(decimal))
}

#[allow(dead_code)]
fn hash_suite_name(hash_suite: LedgerHashSuite) -> &'static str {
    match hash_suite {
        LedgerHashSuite::Sha256V1 => "sha256_v1",
    }
}

#[allow(dead_code)]
fn parse_hash_suite(name: &str) -> Result<LedgerHashSuite, LedgerAttachmentError> {
    match name {
        "sha256_v1" => Ok(LedgerHashSuite::Sha256V1),
        other => Err(LedgerAttachmentError::InvalidHashSuite(other.to_owned())),
    }
}

#[allow(dead_code)]
fn source_kind_name(kind: LedgerSourceCanonicalKind) -> &'static str {
    match kind {
        LedgerSourceCanonicalKind::LegacyDsl => "legacy_dsl",
        LedgerSourceCanonicalKind::DiscordUi => "discord_ui",
    }
}

fn today_date() -> NaiveDate {
    business_timezone()
        .timestamp_opt(Utc::now().timestamp(), Utc::now().timestamp_subsec_nanos())
        .single()
        .expect("today should resolve in the business timezone")
        .date_naive()
}

/// Accepts human-friendly date strings and parses them into a `NaiveDate`.
///
/// Supported forms (in addition to `YYYY-MM-DD` pass-through):
/// - `今日` / `today` / `きょう` → today
/// - `昨日` / `yesterday` / `きのう` → yesterday
/// - `YYYY/MM/DD` → separator normalisation only
/// - `M/D` / `MM/DD` → current year is assumed
fn normalize_date_input(raw: &str, today: NaiveDate) -> Option<NaiveDate> {
    let s = raw.trim();
    match s {
        "今日" | "today" | "きょう" => return Some(today),
        "昨日" | "yesterday" | "きのう" => return today.pred_opt(),
        _ => {}
    }
    if s.len() == 10 && s.as_bytes().get(4) == Some(&b'-') && s.as_bytes().get(7) == Some(&b'-') {
        return NaiveDate::parse_from_str(s, "%Y-%m-%d").ok();
    }
    if s.len() == 10 && s.as_bytes().get(4) == Some(&b'/') && s.as_bytes().get(7) == Some(&b'/') {
        return NaiveDate::parse_from_str(s, "%Y/%m/%d").ok();
    }
    if let Some(slash_pos) = s.find('/') {
        let m_str = &s[..slash_pos];
        let d_str = &s[slash_pos + 1..];
        let all_digits = |t: &str| !t.is_empty() && t.bytes().all(|b| b.is_ascii_digit());
        if all_digits(m_str) && all_digits(d_str) {
            let m: u32 = m_str.parse().ok()?;
            let d: u32 = d_str.parse().ok()?;
            return NaiveDate::from_ymd_opt(today.year(), m, d);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use walicord_application::ledger::{load_and_replay_verified_sha256_v1, replay_entries};

    fn expense_input() -> RecordExpenseInput {
        RecordExpenseInput {
            payer: MemberId(1),
            amount: Money::from_i64(10_000),
            participants: vec![
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(2),
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(3),
                    weight: Weight(1),
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(4),
                    weight: Weight::ZERO,
                },
            ],
            note: Some("ランチ".into()),
            effective_date: Some(
                LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
            ),
            recorded_by: MemberId(9),
        }
    }

    fn business_system_time(timestamp: &str) -> SystemTime {
        use chrono::NaiveDateTime;

        let naive = NaiveDateTime::parse_from_str(timestamp, "%Y-%m-%d %H:%M")
            .expect("timestamp should parse");
        business_timezone()
            .from_local_datetime(&naive)
            .single()
            .expect("timestamp should resolve in the business timezone")
            .into()
    }

    fn utc_system_time(timestamp: &str) -> SystemTime {
        chrono::DateTime::parse_from_rfc3339(timestamp)
            .expect("timestamp should parse")
            .with_timezone(&Utc)
            .into()
    }

    fn sample_member_names() -> HashMap<MemberId, SmolStr> {
        HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
            (MemberId(3), SmolStr::new("次郎")),
            (MemberId(4), SmolStr::new("三郎")),
            (MemberId(9), SmolStr::new("記録者")),
        ])
    }

    fn loaded_thread_for_rendering(
        entries: Vec<LedgerEntry>,
        transports: Vec<(LedgerEntryId, u64, &str)>,
    ) -> LoadedLedgerThread {
        let ledger_id = LedgerId(0xabcd1234);
        let channel_id = ChannelId::new(2);
        let mut previous_hash = ledger_chain_genesis_sha256_v1(ledger_id);
        let unverified = entries
            .iter()
            .cloned()
            .enumerate()
            .map(|(index, entry)| {
                let envelope = make_unverified_envelope_sha256_v1(
                    ledger_id,
                    previous_hash,
                    MessageId::new(u64::try_from(index + 1).expect("message id should fit")),
                    entry,
                )
                .expect("fixture envelope should encode");
                previous_hash = envelope.entry_hash;
                envelope
            })
            .collect::<Vec<_>>();
        let (verified, projected) =
            walicord_application::ledger::load_and_replay_verified_sha256_v1(unverified, ledger_id)
                .expect("fixture thread should verify");
        let snapshot = walicord_application::ledger::replay_verified_snapshot(&verified)
            .expect("fixture snapshot should replay");
        let transport_entries = transports
            .into_iter()
            .map(|(entry_id, message_id, recorded_at)| {
                (
                    entry_id,
                    LoadedTransportEntry {
                        message_link: format!(
                            "https://discord.com/channels/{}/{}/{message_id}",
                            GuildId::new(1).get(),
                            channel_id.get()
                        ),
                        recorded_at: business_system_time(recorded_at),
                    },
                )
            })
            .collect();

        LoadedLedgerThread {
            ledger_id,
            channel_id,
            snapshot,
            verified,
            projected,
            entries,
            transport_entries,
        }
    }

    #[test]
    fn ledger_panel_component_ids_are_fixed_launchers() {
        assert!(is_ledger_panel_component_id(LEDGER_PANEL_EXPENSE_ID));
        assert!(is_ledger_panel_component_id(LEDGER_PANEL_REVIEW_ID));
        assert!(is_ledger_panel_component_id(LEDGER_PANEL_LEDGER_ID));
        assert!(is_ledger_panel_component_id(LEDGER_PANEL_VOID_ID));
        assert!(!is_ledger_panel_component_id(&session_custom_id(
            10,
            EXPENSE_RECORD_PREFIX,
            99
        )));
    }

    #[test]
    fn expense_builder_creates_weighted_entry_and_replays_balances() {
        let entry =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");

        assert!(matches!(
            entry.metadata.source.as_ref().map(|source| source.kind()),
            Some(LedgerSourceCanonicalKind::DiscordUi)
        ));
        assert_eq!(entry.metadata.recorded_by, Some(MemberId(9)));
        assert_eq!(
            entry.metadata.effective_date,
            Some(LedgerEffectiveDate::new("2026-05-01").expect("date should parse"))
        );
        assert_eq!(
            entry.metadata.allocation_snapshot,
            Some(AllocationSnapshot::Weighted {
                resolved_weights: vec![
                    MemberWeight {
                        member_id: MemberId(2),
                        weight: Weight(2),
                    },
                    MemberWeight {
                        member_id: MemberId(3),
                        weight: Weight(1),
                    },
                    MemberWeight {
                        member_id: MemberId(4),
                        weight: Weight::ZERO,
                    },
                ],
            })
        );

        let projected = replay_entries(vec![entry.clone()]).expect("entry should replay");
        assert_eq!(
            projected.state().balances().get(&MemberId(1)),
            Some(&Money::from_i64(10_000))
        );
        assert_eq!(
            projected.state().balances().get(&MemberId(2)),
            Some(&Money::from_i64(-6_667))
        );
        assert_eq!(
            projected.state().balances().get(&MemberId(3)),
            Some(&Money::from_i64(-3_333))
        );
        assert!(!projected.state().balances().contains_key(&MemberId(4)));

        assert_eq!(
            entry.event,
            LedgerEvent::ExpenseRecorded(
                ExpenseRecorded::new(
                    vec![MemberAmount {
                        member_id: MemberId(1),
                        amount: Money::from_i64(10_000),
                    }],
                    vec![
                        MemberAmount {
                            member_id: MemberId(2),
                            amount: Money::from_i64(6_667),
                        },
                        MemberAmount {
                            member_id: MemberId(3),
                            amount: Money::from_i64(3_333),
                        },
                    ],
                    Some(ExpenseNote::new("ランチ").expect("note should be valid")),
                )
                .expect("expense event should be valid")
            )
        );
    }

    #[test]
    fn canonical_attachment_round_trips_event_and_metadata() {
        let entry =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let envelope = make_unverified_envelope_sha256_v1(
            LedgerId(77),
            ledger_chain_genesis_sha256_v1(LedgerId(77)),
            (),
            entry.clone(),
        )
        .expect("envelope should build");

        let encoded =
            encode_discord_canonical_attachment(&envelope, None).expect("attachment should encode");
        let decoded = decode_discord_canonical_attachment(&encoded, 123_u64)
            .expect("attachment should decode");

        assert_eq!(decoded.previous_hash, envelope.previous_hash);
        assert_eq!(decoded.entry_hash, envelope.entry_hash);
        assert_eq!(decoded.payload.ledger_id, envelope.payload.ledger_id);
        assert_eq!(decoded.payload.entry, entry);
    }

    #[test]
    fn canonical_attachment_encode_keeps_the_legacy_metadata_shape_when_recorded_at_is_absent() {
        let entry =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let envelope = make_unverified_envelope_sha256_v1(
            LedgerId(77),
            ledger_chain_genesis_sha256_v1(LedgerId(77)),
            (),
            entry,
        )
        .expect("envelope should build");

        let encoded =
            encode_discord_canonical_attachment(&envelope, None).expect("attachment should encode");
        let value: serde_json::Value = serde_json::from_slice(&encoded).expect("json should parse");

        assert!(
            value["envelope"]["payload"]["entry"]["metadata"]
                .as_object()
                .expect("metadata should be an object")
                .get("recorded_at_unix_ms")
                .is_none()
        );
    }

    #[test]
    fn weight_modal_body_shows_name_id_and_zero_weight() {
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: Some("ランチ".into()),
            effective_date: Some(
                LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
            ),
            payer: Some(MemberId(1)),
            explicit_members: vec![MemberId(2), MemberId(3), MemberId(4)],
            selected_roles: Vec::new(),
            include_members_group: false,
            active_picker: ExpensePickerKind::Participants,
            picker_query: None,
            picker_page: 0,
            weight_overrides: HashMap::from([
                (MemberId(2), Weight(2)),
                (MemberId(3), Weight(1)),
                (MemberId(4), Weight::ZERO),
            ]),
            frozen_participants: None,
            recording: false,
        };
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3), MemberId(4)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
            (MemberId(3), SmolStr::new("次郎")),
            (MemberId(4), SmolStr::new("三郎")),
        ]);

        let body = expense_weight_modal_body(&draft, &roster, &member_names);

        assert!(body.contains("花子 = 2"));
        assert!(body.contains("次郎 = 1"));
        assert!(body.contains("三郎 = 0"));
        assert_eq!(
            parse_weight_overrides(
                &body,
                &expense_weight_label_lookup(&draft, &roster, &member_names)
            )
            .expect("weight body should parse back"),
            HashMap::from([
                (MemberId(2), Weight(2)),
                (MemberId(3), Weight(1)),
                (MemberId(4), Weight::ZERO),
            ])
        );
    }

    #[test]
    fn expense_weight_modal_body_disambiguates_duplicate_names_for_parsing() {
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: None,
            effective_date: None,
            payer: Some(MemberId(1)),
            explicit_members: vec![MemberId(2), MemberId(3)],
            selected_roles: Vec::new(),
            include_members_group: false,
            active_picker: ExpensePickerKind::Participants,
            picker_query: None,
            picker_page: 0,
            weight_overrides: HashMap::new(),
            frozen_participants: None,
            recording: false,
        };
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("payer")),
            (MemberId(2), SmolStr::new("Alice")),
            (MemberId(3), SmolStr::new("Alice")),
        ]);

        let body = expense_weight_modal_body(&draft, &roster, &member_names);

        assert!(body.contains("Alice (ID: 2) = 1"));
        assert!(body.contains("Alice (ID: 3) = 1"));
        assert_eq!(
            parse_weight_overrides(
                &body,
                &expense_weight_label_lookup(&draft, &roster, &member_names)
            )
            .expect("duplicate names should still parse"),
            HashMap::from([(MemberId(2), Weight(1)), (MemberId(3), Weight(1))])
        );
    }

    #[test]
    fn expense_weight_modal_body_sanitizes_member_labels() {
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: None,
            effective_date: None,
            payer: Some(MemberId(1)),
            explicit_members: vec![MemberId(2)],
            selected_roles: Vec::new(),
            include_members_group: false,
            active_picker: ExpensePickerKind::Participants,
            picker_query: None,
            picker_page: 0,
            weight_overrides: HashMap::new(),
            frozen_participants: None,
            recording: false,
        };
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("payer")),
            (MemberId(2), SmolStr::new("@everyone")),
        ]);

        let body = expense_weight_modal_body(&draft, &roster, &member_names);

        assert!(body.contains("＠everyone = 1"));
        assert!(!body.contains("@everyone = 1"));
    }

    #[test]
    fn parse_amount_rejects_zero_yen() {
        assert!(parse_amount("0").is_err());
    }

    #[test]
    fn parse_amount_rejects_negative_yen() {
        assert!(parse_amount("-1").is_err());
    }

    #[test]
    fn confirmation_renders_human_friendly_weights() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".into()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                ),
                payer: Some(MemberId(1)),
                explicit_members: vec![MemberId(2), MemberId(3), MemberId(4)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: Some(vec![
                    ExpenseParticipantSelection {
                        member_id: MemberId(2),
                        weight: Weight(2),
                    },
                    ExpenseParticipantSelection {
                        member_id: MemberId(3),
                        weight: Weight(1),
                    },
                    ExpenseParticipantSelection {
                        member_id: MemberId(4),
                        weight: Weight::ZERO,
                    },
                ]),
                recording: false,
            },
        );
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3), MemberId(4)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
            (MemberId(3), SmolStr::new("次郎")),
            (MemberId(4), SmolStr::new("三郎")),
        ]);

        let confirmation = poc
            .render_expense_confirmation(session_id, &roster, &member_names)
            .expect("confirmation should render");

        assert!(confirmation.contains("支払者: 太郎"));
        assert!(confirmation.contains("- 花子: 6667円 (×2) [直接選択]"));
        assert!(confirmation.contains("- 次郎: 3333円 (×1) [直接選択]"));
        assert!(confirmation.contains("- 三郎 ×0 (取り分なし) [直接選択]"));
        assert!(confirmation.contains("ロールと 全メンバー (MEMBERS) は記録時に再評価されます。"));
    }

    #[test]
    fn confirmation_sanitizes_member_labels_and_note_text() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("@everyone <@123> https://example.com".into()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                ),
                payer: Some(MemberId(1)),
                explicit_members: vec![MemberId(2)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: Some(vec![ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(1),
                }]),
                recording: false,
            },
        );
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("@everyone")),
            (MemberId(2), SmolStr::new("<@123>")),
        ]);

        let confirmation = poc
            .render_expense_confirmation(session_id, &roster, &member_names)
            .expect("confirmation should render");

        assert!(confirmation.contains("支払者: ＠everyone"));
        assert!(confirmation.contains("- ＜＠123＞: 10000円 (×1) [直接選択]"));
        assert!(confirmation.contains("メモ: ＠everyone ＜＠123＞ https：／／example．com"));
    }

    #[test]
    fn expense_confirmation_surface_paginates_large_confirmation_blocks() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        let participant_ids = (2..=22).map(MemberId).collect::<Vec<_>>();
        let frozen_participants = participant_ids
            .iter()
            .copied()
            .map(|member_id| ExpenseParticipantSelection {
                member_id,
                weight: Weight(1),
            })
            .collect::<Vec<_>>();
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(21_000),
                note: Some("送別会".into()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                ),
                payer: Some(MemberId(1)),
                explicit_members: participant_ids.clone(),
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: Some(frozen_participants),
                recording: false,
            },
        );
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: (1..=22).map(MemberId).collect(),
            ..Default::default()
        };
        let member_names = (1..=22)
            .map(|index| (MemberId(index), SmolStr::new(format!("参加者{index:02}"))))
            .collect::<HashMap<_, _>>();

        let (first_page, first_components) = poc
            .render_expense_confirmation_surface(UserId::new(9), session_id, &roster, &member_names)
            .expect("confirmation should render");

        assert!(first_page.contains("ページ 1/2"));
        assert!(first_page.contains(i18n::snapshot_notice()));
        assert!(first_page.contains("参加者02"));
        assert!(first_page.contains("参加者21"));
        assert!(!first_page.contains("参加者22"));
        let first_components = serde_json::to_value(first_components)
            .expect("confirmation components should serialize");
        assert_eq!(first_components.as_array().map(Vec::len), Some(4));
        assert_eq!(
            first_components[3]["components"][0]["disabled"],
            serde_json::Value::Bool(true)
        );
        assert_eq!(
            first_components[3]["components"][1]["disabled"],
            serde_json::Value::Bool(false)
        );

        let page_session_id = *poc
            .expense_confirmation_sessions
            .iter()
            .next()
            .expect("paged confirmation session should exist")
            .key();
        let mut page_session = poc
            .expense_confirmation_sessions
            .get_mut(&page_session_id)
            .expect("page session should stay available");
        page_session.current_page = 1;
        let (second_page, second_components) =
            poc.expense_confirmation_page_message(page_session_id, &page_session);
        drop(page_session);

        assert!(second_page.contains("ページ 2/2"));
        assert!(!second_page.contains("参加者02"));
        assert!(second_page.contains("参加者22"));
        let second_components = serde_json::to_value(second_components)
            .expect("confirmation components should serialize");
        assert_eq!(
            second_components[3]["components"][0]["disabled"],
            serde_json::Value::Bool(false)
        );
        assert_eq!(
            second_components[3]["components"][1]["disabled"],
            serde_json::Value::Bool(true)
        );
    }

    #[test]
    fn confirmation_rejects_zero_weight_only_participants() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".into()),
                effective_date: None,
                payer: Some(MemberId(1)),
                explicit_members: vec![MemberId(2), MemberId(3)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::from([
                    (MemberId(2), Weight::ZERO),
                    (MemberId(3), Weight::ZERO),
                ]),
                frozen_participants: Some(vec![
                    ExpenseParticipantSelection {
                        member_id: MemberId(2),
                        weight: Weight::ZERO,
                    },
                    ExpenseParticipantSelection {
                        member_id: MemberId(3),
                        weight: Weight::ZERO,
                    },
                ]),
                recording: false,
            },
        );
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
            (MemberId(3), SmolStr::new("次郎")),
        ]);

        assert_eq!(
            poc.render_expense_confirmation(session_id, &roster, &member_names),
            Err("対象者の重みは 1 人以上を 1 以上にしてください。".into())
        );
    }

    #[test]
    fn confirmed_expense_selections_accept_matching_confirmation() {
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: Some("ランチ".into()),
            effective_date: None,
            payer: Some(MemberId(1)),
            explicit_members: vec![MemberId(2), MemberId(3)],
            selected_roles: Vec::new(),
            include_members_group: false,
            active_picker: ExpensePickerKind::Participants,
            picker_query: None,
            picker_page: 0,
            weight_overrides: HashMap::from([(MemberId(2), Weight(2)), (MemberId(3), Weight(1))]),
            frozen_participants: Some(vec![
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(2),
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(3),
                    weight: Weight(1),
                },
            ]),
            recording: false,
        };
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3)],
            ..Default::default()
        };

        assert_eq!(
            confirmed_expense_selections(&draft, &roster),
            Ok(vec![
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(2),
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(3),
                    weight: Weight(1),
                },
            ])
        );
    }

    #[test]
    fn confirmed_expense_selections_reports_drifted_membership() {
        let mut role_members = walicord_domain::model::RoleMembers::default();
        role_members.insert(
            RoleId(7),
            vec![MemberId(2), MemberId(3)].into_iter().collect(),
        );
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: Some("ランチ".into()),
            effective_date: None,
            payer: Some(MemberId(1)),
            explicit_members: Vec::new(),
            selected_roles: vec![RoleId(7)],
            include_members_group: false,
            active_picker: ExpensePickerKind::Participants,
            picker_query: None,
            picker_page: 0,
            weight_overrides: HashMap::from([(MemberId(2), Weight(2)), (MemberId(3), Weight(1))]),
            frozen_participants: Some(vec![ExpenseParticipantSelection {
                member_id: MemberId(2),
                weight: Weight(2),
            }]),
            recording: false,
        };
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3)],
            role_members,
            ..Default::default()
        };

        assert_eq!(
            confirmed_expense_selections(&draft, &roster),
            Err(ConfirmedExpenseSelectionsError::Drifted(vec![
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(2),
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(3),
                    weight: Weight(1),
                },
            ]))
        );
    }

    #[test]
    fn claim_expense_recording_blocks_duplicate_recording_until_released() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".into()),
                effective_date: None,
                payer: Some(MemberId(1)),
                explicit_members: vec![MemberId(1), MemberId(2)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: Some(vec![ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(1),
                }]),
                recording: false,
            },
        );

        let first_claim = poc
            .claim_expense_recording(session_id, UserId::new(9))
            .expect("first claim should succeed");

        assert!(first_claim.recording);
        assert!(matches!(
            poc.claim_expense_recording(session_id, UserId::new(9)),
            Err(ExpenseRecordingClaimError::AlreadyRecording)
        ));

        poc.release_expense_recording(session_id);

        assert!(
            poc.claim_expense_recording(session_id, UserId::new(9))
                .is_ok()
        );
    }

    #[test]
    fn ledger_component_and_modal_ids_cover_mutating_continuations() {
        assert!(is_ledger_component_id(&session_custom_id(
            10,
            EXPENSE_RECORD_PREFIX,
            99
        )));
        assert!(is_ledger_component_id(&session_custom_id(
            10,
            VOID_NEXT_PREFIX,
            99
        )));
        assert!(is_ledger_modal_id(&session_custom_id(
            10,
            EXPENSE_MODAL_PREFIX,
            99
        )));
        assert!(is_ledger_modal_id(&session_custom_id(
            10,
            EXPENSE_WEIGHTS_MODAL_PREFIX,
            99
        )));
        assert!(!is_ledger_component_id("review:legacy"));
        assert!(!is_ledger_modal_id("review:legacy"));
    }

    #[test]
    fn session_custom_id_rejects_stale_nonce() {
        let custom_id = session_custom_id(10, EXPENSE_RECORD_PREFIX, 99);

        assert_eq!(
            parse_session_id(&custom_id, EXPENSE_RECORD_PREFIX, 10),
            SessionIdMatch::Match(99)
        );
        assert_eq!(
            parse_session_id(&custom_id, EXPENSE_RECORD_PREFIX, 11),
            SessionIdMatch::Stale
        );
        assert_eq!(
            parse_session_id(&custom_id, VOID_CONFIRM_PREFIX, 10),
            SessionIdMatch::NoMatch
        );
    }

    #[test]
    fn void_confirm_custom_id_binds_target_and_rejects_stale_nonce() {
        let custom_id = void_confirm_custom_id(10, 99, LedgerEntryId(7));

        assert_eq!(
            parse_void_confirm_id(&custom_id, 10),
            VoidConfirmMatch::Match {
                session_id: 99,
                target: LedgerEntryId(7),
            }
        );
        assert_eq!(
            parse_void_confirm_id(&custom_id, 11),
            VoidConfirmMatch::Stale
        );
        assert_eq!(
            parse_void_confirm_id(&session_custom_id(10, VOID_CONFIRM_PREFIX, 99), 10),
            VoidConfirmMatch::Stale
        );
    }

    #[test]
    fn void_window_entries_ignore_older_voidable_records_outside_the_recent_append_window() {
        let mut entries = vec![
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build"),
        ];
        for offset in 0..10 {
            let expense_entry_id = LedgerEntryId(2 + offset * 2);
            let void_entry_id = LedgerEntryId(3 + offset * 2);
            entries.push(
                build_expense_entry(expense_entry_id, &expense_input())
                    .expect("entry should build"),
            );
            entries.push(build_void_entry(
                void_entry_id,
                MemberId(9),
                expense_entry_id,
            ));
        }
        let projected = replay_entries(entries.clone()).expect("ledger should replay");

        let global_candidates = candidate_entries_for_void(&entries, &projected);
        let window_candidates = void_window_entries(&entries, &projected);

        assert_eq!(
            global_candidates
                .iter()
                .map(|entry| entry.id)
                .collect::<Vec<_>>(),
            vec![LedgerEntryId(1)]
        );
        assert!(window_candidates.is_empty());
    }

    #[test]
    fn prune_stale_state_removes_expired_sessions() {
        let poc = DiscordLedgerPoc::new();
        let stale_expense_session = 1;
        let fresh_expense_session = 2;
        let stale_void_session = 3;
        let stale_preview_key = (4, 9);
        let ledger_id = LedgerId(77);
        let thread_id = ChannelId::new(10);
        let previewed = preview_settlement_from_ledger(
            &replay_entries(vec![
                build_expense_entry(LedgerEntryId(1), &expense_input())
                    .expect("entry should build"),
            ])
            .expect("ledger should replay"),
        )
        .expect("preview should build");

        poc.expense_drafts.insert(
            stale_expense_session,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(11),
                expires_at: UNIX_EPOCH,
                amount: Money::from_i64(10_000),
                note: None,
                effective_date: None,
                payer: Some(MemberId(1)),
                explicit_members: vec![MemberId(1)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: None,
                recording: false,
            },
        );
        poc.expense_drafts.insert(
            fresh_expense_session,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(12),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: None,
                effective_date: None,
                payer: Some(MemberId(1)),
                explicit_members: vec![MemberId(1)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: None,
                recording: false,
            },
        );
        poc.void_drafts.insert(
            stale_void_session,
            PendingVoidDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                ledger_channel_id: thread_id,
                parent_channel_id: ChannelId::new(10),
                success_thread_channel_id: Some(thread_id),
                expires_at: UNIX_EPOCH,
                stage: VoidStage::Select,
                selected_target: Some(LedgerEntryId(1)),
                current_page: 0,
                selection_snapshot: Vec::new(),
            },
        );
        poc.settlement_previews.insert(
            stale_preview_key,
            PendingSettlementPreview {
                ledger_id,
                ledger_channel_id: thread_id,
                binding: PreviewConfirmationBinding::capture(
                    PreviewInstanceId::new(1).expect("preview instance id should be valid"),
                    ledger_id,
                    ledger_chain_genesis_sha256_v1(ledger_id),
                    MemberId(9),
                    UNIX_EPOCH,
                    UNIX_EPOCH + std::time::Duration::from_secs(1),
                    &previewed,
                )
                .expect("stale preview binding should capture"),
                previewed,
            },
        );

        poc.prune_stale_state();

        assert!(!poc.expense_drafts.contains_key(&stale_expense_session));
        assert!(poc.expense_drafts.contains_key(&fresh_expense_session));
        assert!(!poc.void_drafts.contains_key(&stale_void_session));
        assert!(!poc.settlement_previews.contains_key(&stale_preview_key));
    }

    #[test]
    fn clearing_expense_confirmation_sessions_for_a_draft_removes_only_bound_pages() {
        let poc = DiscordLedgerPoc::new();
        let draft_session_id = 7;
        let other_draft_session_id = 8;
        let actor_id = UserId::new(9);
        let pages = vec![
            ReadViewRenderedPage {
                body: "page-1".to_owned(),
                base_components: Vec::new(),
            },
            ReadViewRenderedPage {
                body: "page-2".to_owned(),
                base_components: Vec::new(),
            },
        ];

        let stale_session_id = poc
            .create_expense_confirmation_session(draft_session_id, actor_id, pages.clone())
            .expect("session should be created");
        let retained_session_id = poc
            .create_expense_confirmation_session(other_draft_session_id, actor_id, pages)
            .expect("session should be created");

        poc.clear_expense_confirmation_sessions_for_draft(draft_session_id);

        assert!(
            !poc.expense_confirmation_sessions
                .contains_key(&stale_session_id)
        );
        assert!(
            poc.expense_confirmation_sessions
                .contains_key(&retained_session_id)
        );
    }

    #[test]
    fn canonical_attachments_replay_through_v1_load_path() {
        let ledger_id = LedgerId(77);
        let entry =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let envelope = make_unverified_envelope_sha256_v1(
            ledger_id,
            ledger_chain_genesis_sha256_v1(ledger_id),
            (),
            entry.clone(),
        )
        .expect("legacy envelope should build");

        let encoded =
            encode_discord_canonical_attachment(&envelope, None).expect("attachment should encode");
        let decoded = decode_discord_canonical_attachment(&encoded, 123_u64)
            .expect("attachment should decode");
        let (verified, projected) = load_and_replay_verified_sha256_v1(vec![decoded], ledger_id)
            .expect("attachment should replay");

        assert_eq!(verified[0].payload().entry, entry);
        assert_eq!(
            projected.state().balances().get(&MemberId(1)),
            Some(&Money::from_i64(10_000))
        );
    }

    #[test]
    fn expense_builder_reports_invalid_amount_for_negative_amount() {
        let mut input = expense_input();
        input.amount = Money::from_i64(-1);

        assert_eq!(
            build_expense_entry(LedgerEntryId(1), &input),
            Err(ExpenseEntryBuildError::InvalidAmount)
        );
    }

    #[test]
    fn build_settlement_entry_tags_discord_ui_source() {
        let expense =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let projected = replay_entries(vec![expense]).expect("ledger should replay");
        let previewed = preview_settlement_from_ledger(&projected).expect("preview should build");

        let settlement = build_settlement_entry(LedgerEntryId(2), MemberId(9), &previewed)
            .expect("settlement should build")
            .expect("preview should require a settlement event");

        assert_eq!(
            settlement
                .metadata
                .source
                .as_ref()
                .map(|source| source.kind()),
            Some(LedgerSourceCanonicalKind::DiscordUi)
        );
    }

    #[test]
    fn build_settlement_entry_records_preview_transfers() {
        let expense =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let projected = replay_entries(vec![expense]).expect("ledger should replay");
        let previewed = preview_settlement_from_ledger(&projected).expect("preview should build");

        let settlement = build_settlement_entry(LedgerEntryId(2), MemberId(9), &previewed)
            .expect("settlement should build")
            .expect("preview should require a settlement event");

        assert_eq!(
            settlement.event,
            LedgerEvent::NormalizedSettlementPlanRecorded(
                NormalizedSettlementPlanRecorded::new(previewed.plan().transfers.clone())
                    .expect("settlement event should be valid")
            )
        );
    }

    #[test]
    fn build_live_settlement_entry_requires_delivered_preview_binding() {
        let expense =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let projected = replay_entries(vec![expense]).expect("ledger should replay");
        let previewed = preview_settlement_from_ledger(&projected).expect("preview should build");
        let binding = PreviewConfirmationBinding::capture(
            PreviewInstanceId::new(1).expect("preview instance id should be valid"),
            LedgerId(77),
            ledger_chain_genesis_sha256_v1(LedgerId(77)),
            MemberId(9),
            UNIX_EPOCH,
            UNIX_EPOCH + std::time::Duration::from_secs(600),
            &previewed,
        )
        .expect("binding should capture");

        let actual =
            build_live_settlement_entry(LedgerEntryId(2), MemberId(9), &previewed, &binding);

        assert_eq!(actual, Err(SettlementRecordError::PreviewNotDelivered));
    }

    #[test]
    fn void_selection_components_truncate_candidate_labels_that_exceed_budget() {
        let entry =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let over_budget_name = "A".repeat(128);
        let member_names = HashMap::from([(MemberId(1), SmolStr::new(&over_budget_name))]);

        let actual = serde_json::to_value(
            void_selection_components(7, &[entry], &member_names, Some(LedgerEntryId(1)), 99)
                .expect("selection components should render"),
        )
        .expect("selection components should serialize");

        let option_label = actual[0]["components"][0]["options"][0]["label"]
            .as_str()
            .expect("option label should exist");
        assert!(option_label.ends_with('…'));
        assert!(option_label.chars().count() <= 100);
        assert_eq!(
            actual[1]["components"][0]["label"],
            i18n::void_cancel_label()
        );
        assert_eq!(actual[1]["components"][1]["label"], i18n::void_next_label());
    }

    #[test]
    fn void_selection_surface_paginates_snapshot_bound_candidate_pages() {
        let entries = (1..=20)
            .map(|entry_id| {
                let mut input = expense_input();
                input.note = Some(format!(
                    "候補{entry_id:02}のメモを長めにしてページングを確認します"
                ));
                build_expense_entry(LedgerEntryId(entry_id), &input).expect("entry should build")
            })
            .collect::<Vec<_>>();
        let loaded = loaded_thread_for_rendering(
            entries.clone(),
            (1..=20)
                .map(|entry_id| (LedgerEntryId(entry_id), entry_id, "2026-05-25 18:55"))
                .collect(),
        );
        let member_names = sample_member_names();
        let snapshot =
            void_selection_snapshot_rows(&loaded, &entries, &member_names).expect("snapshot");
        let (_, page_ranges) =
            void_selection_current_page(&snapshot, 0).expect("pages should build");
        assert!(page_ranges.len() > 1);

        let first_draft = PendingVoidDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            ledger_channel_id: ChannelId::new(10),
            parent_channel_id: ChannelId::new(10),
            success_thread_channel_id: Some(ChannelId::new(10)),
            expires_at: interaction_state_expires_at(),
            stage: VoidStage::Select,
            selected_target: Some(LedgerEntryId(20)),
            current_page: 0,
            selection_snapshot: snapshot.clone(),
        };
        let (first_body, first_components) =
            render_void_selection_surface(&first_draft, 7, 99, VoidSelectionBodyKind::Normal)
                .expect("first page should render");

        assert!(first_body.contains(&i18n::page_indicator(1, page_ranges.len()).to_string()));
        assert!(first_body.contains(i18n::snapshot_notice()));
        assert!(first_body.contains(i18n::void_selection_window_guidance()));
        let first_components =
            serde_json::to_value(first_components).expect("components should serialize");
        assert_eq!(first_components.as_array().map(Vec::len), Some(3));
        assert_eq!(
            first_components[1]["components"][0]["label"],
            i18n::void_cancel_label()
        );
        assert_eq!(
            first_components[1]["components"][1]["label"],
            i18n::void_next_label()
        );
        assert_eq!(
            first_components[2]["components"][0]["disabled"],
            serde_json::Value::Bool(true)
        );
        assert_eq!(
            first_components[2]["components"][1]["disabled"],
            serde_json::Value::Bool(false)
        );

        let last_page = page_ranges.len() - 1;
        let last_draft = PendingVoidDraft {
            current_page: last_page,
            ..first_draft
        };
        let (last_body, last_components) =
            render_void_selection_surface(&last_draft, 7, 99, VoidSelectionBodyKind::Normal)
                .expect("last page should render");

        assert!(
            last_body
                .contains(&i18n::page_indicator(page_ranges.len(), page_ranges.len()).to_string())
        );
        let last_components =
            serde_json::to_value(last_components).expect("components should serialize");
        assert_eq!(
            last_components[2]["components"][0]["disabled"],
            serde_json::Value::Bool(false)
        );
        assert_eq!(
            last_components[2]["components"][1]["disabled"],
            serde_json::Value::Bool(true)
        );
        assert_eq!(
            last_components[1]["components"][0]["label"],
            i18n::void_cancel_label()
        );
        assert_eq!(
            last_components[1]["components"][1]["label"],
            i18n::void_next_label()
        );
        let option_values = last_components[0]["components"][0]["options"]
            .as_array()
            .expect("options should serialize")
            .iter()
            .map(|option| option["value"].as_str().expect("value should exist"))
            .collect::<Vec<_>>();
        assert!(option_values.contains(&"20"));
        assert!(
            last_components[0]["components"][0]["options"]
                .as_array()
                .expect("options should serialize")
                .iter()
                .any(|option| option["value"] == "20"
                    && option["default"] == serde_json::Value::Bool(true))
        );
    }

    #[test]
    fn void_flow_keeps_selection_private_until_the_confirmation_stage() {
        let entry =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let member_names = HashMap::from([(MemberId(1), SmolStr::new("太郎"))]);
        let selection_components = serde_json::to_value(
            void_selection_components(
                7,
                std::slice::from_ref(&entry),
                &member_names,
                Some(entry.id),
                99,
            )
            .expect("selection components should render"),
        )
        .expect("selection components should serialize");
        let confirmation_components =
            serde_json::to_value(void_confirmation_components(7, entry.id, 99))
                .expect("confirmation components should serialize");
        let selection_surface =
            DiscordLedgerPresenter::render_void_flow(&VoidSurfaceModel::selection(
                i18n::panel_void_button_label(),
                vec![VoidCandidateRow {
                    summary: LedgerSurfaceSummary::Expense {
                        date: LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                        payer_display_name: SafeLiteralText::from_roster_label("太郎")
                            .expect("label should sanitize"),
                        amount: "1200".to_owned(),
                        note: None,
                    },
                    recovery_reference: RecoveryReference {
                        ledger_id_short: "deadbeef".to_owned(),
                        entry_id: entry.id,
                        message_link: None,
                    },
                }],
                Vec::new(),
                true,
            ))
            .expect("selection surface should render");

        assert!(selection_surface.ephemeral);
        assert_eq!(
            selection_components[1]["components"][0]["label"],
            i18n::void_cancel_label()
        );
        assert_eq!(
            selection_components[1]["components"][1]["label"],
            i18n::void_next_label()
        );
        assert_eq!(
            confirmation_components[0]["components"][1]["label"],
            i18n::void_cancel_label()
        );
        assert_eq!(
            confirmation_components[0]["components"][2]["label"],
            i18n::void_confirm_label()
        );
    }

    #[test]
    fn render_entry_message_sanitizes_public_note_and_adjustment_reason() {
        let entry = build_expense_entry(
            LedgerEntryId(1),
            &RecordExpenseInput {
                note: Some("@everyone <@123> https://example.com".into()),
                ..expense_input()
            },
        )
        .expect("entry should build");
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
            (MemberId(3), SmolStr::new("次郎")),
            (MemberId(4), SmolStr::new("三郎")),
        ]);
        let rendered_expense = render_entry_message(&entry, &member_names);

        assert!(rendered_expense.contains("メモ: ＠everyone ＜＠123＞ https：／／example．com"));

        let adjustment = LedgerEntry::non_expense(
            LedgerEntryId(2),
            BalanceAdjusted::new(
                vec![
                    BalanceAdjustment {
                        member_id: MemberId(1),
                        amount: Money::from_i64(-100),
                    },
                    BalanceAdjustment {
                        member_id: MemberId(2),
                        amount: Money::from_i64(100),
                    },
                ],
                AdjustmentReason::new("@everyone correction").expect("reason should be valid"),
                external_correction_source_for_transport_decode(),
            )
            .expect("adjustment should be valid"),
        );
        let rendered_adjustment = render_entry_message(&adjustment, &member_names);

        assert!(rendered_adjustment.contains("理由: ＠everyone correction"));
    }

    #[test]
    fn void_success_surface_omits_dead_thread_mentions_and_falls_back_to_parent_cta() {
        let (body, components) = render_void_success_surface(
            Some(ChannelId::new(1234)),
            RecoveryCta::ParentLink,
            Some("https://discord.com/channels/1/2".to_owned()),
        )
        .expect("success surface should render");

        assert!(body.contains("台帳スレッドで確認できます。"));
        assert!(!body.contains("<#1234>"));
        let actual = serde_json::to_value(components).expect("components should serialize");
        assert_eq!(actual[0]["components"][0]["label"], "親チャンネルを開く");
    }

    #[test]
    fn render_review_surface_keeps_the_presenter_recovery_cta_row() {
        let entry =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let loaded = loaded_thread_for_rendering(
            vec![entry],
            vec![(LedgerEntryId(1), 1, "2026-05-25 18:55")],
        );
        let previewed =
            preview_settlement_from_ledger(&loaded.projected).expect("preview should build");

        let (_, components) = render_review_surface(
            &loaded,
            &previewed,
            &sample_member_names(),
            ReadViewRoute::ReviewThread,
            RecoveryCta::ParentLink,
            Some("https://discord.com/channels/1/10".to_owned()),
            &ReadViewRenderState::default(),
        )
        .expect("review surface should render");
        let actual = serde_json::to_value(components).expect("components should serialize");

        assert_eq!(
            actual[0]["components"][0]["label"],
            i18n::open_parent_channel_label()
        );
        assert_eq!(
            actual[0]["components"][0]["url"],
            "https://discord.com/channels/1/10"
        );
    }

    #[test]
    fn render_void_success_surface_includes_the_open_thread_button() {
        let (body, components) = render_void_success_surface(
            Some(ChannelId::new(2)),
            RecoveryCta::ThreadLink,
            Some("https://discord.com/channels/1/2".to_owned()),
        )
        .expect("void success surface should render");
        let actual = serde_json::to_value(components).expect("components should serialize");

        assert!(body.contains("<#2>"));
        assert_eq!(
            actual[0]["components"][0]["label"],
            i18n::open_ledger_thread_label()
        );
        assert_eq!(
            actual[0]["components"][0]["url"],
            "https://discord.com/channels/1/2"
        );
        assert_eq!(
            actual[0]["components"][0]["url"],
            "https://discord.com/channels/1/2"
        );
    }

    #[test]
    fn render_public_entry_message_uses_transport_timestamp_for_legacy_entries() {
        let mut input = expense_input();
        input.effective_date = None;
        let entry = build_expense_entry(LedgerEntryId(1), &input).expect("entry should build");
        let loaded = loaded_thread_for_rendering(
            vec![entry.clone()],
            vec![(LedgerEntryId(1), 1, "2026-05-25 18:55")],
        );

        let actual = render_public_entry_message(
            &loaded,
            &entry,
            &sample_member_names(),
            Some("https://discord.com/channels/1/2/1".to_owned()),
        )
        .expect("public message should render");

        assert!(actual.contains("日付: 2026-05-25"));
        assert!(actual.contains("記録日時: 2026-05-25 18:55"));
        assert!(actual.contains("<https://discord.com/channels/1/2/1>"));
    }

    #[test]
    fn render_public_entry_message_uses_business_timezone_for_transport_timestamps() {
        let mut input = expense_input();
        input.effective_date = None;
        let entry = build_expense_entry(LedgerEntryId(1), &input).expect("entry should build");
        let mut loaded = loaded_thread_for_rendering(
            vec![entry.clone()],
            vec![(LedgerEntryId(1), 1, "2026-05-25 18:55")],
        );
        loaded
            .transport_entries
            .get_mut(&LedgerEntryId(1))
            .expect("transport should exist")
            .recorded_at = utc_system_time("2026-05-25T15:30:00Z");

        let actual = render_public_entry_message(&loaded, &entry, &sample_member_names(), None)
            .expect("public message should render");

        assert!(actual.contains("日付: 2026-05-26"));
        assert!(actual.contains("記録日時: 2026-05-26 00:30"));
    }

    #[test]
    fn render_public_entry_message_supports_history_seal_on_live_bridge() {
        let entries = vec![
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build"),
            LedgerEntry::non_expense(LedgerEntryId(2), LedgerHistorySealed::new(LedgerEntryId(1))),
        ];
        let seal_entry = entries[1].clone();
        let loaded = loaded_thread_for_rendering(
            entries,
            vec![
                (LedgerEntryId(1), 1, "2026-05-24 12:00"),
                (LedgerEntryId(2), 2, "2026-05-25 18:55"),
            ],
        );

        let actual =
            render_public_entry_message(&loaded, &seal_entry, &sample_member_names(), None)
                .expect("public message should render");

        assert!(actual.starts_with("確認 [#2]"));
        assert!(actual.contains("[#1] 2026-05-01 太郎 の支払い 10000円"));
    }

    #[test]
    fn render_public_entry_message_preserves_zero_share_participants_from_allocation_snapshot() {
        let entry = build_expense_entry(
            LedgerEntryId(1),
            &RecordExpenseInput {
                payer: MemberId(1),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".to_owned()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should parse"),
                ),
                recorded_by: MemberId(9),
                participants: vec![
                    ExpenseParticipantSelection {
                        member_id: MemberId(2),
                        weight: Weight(2),
                    },
                    ExpenseParticipantSelection {
                        member_id: MemberId(3),
                        weight: Weight(1),
                    },
                    ExpenseParticipantSelection {
                        member_id: MemberId(4),
                        weight: Weight::ZERO,
                    },
                ],
            },
        )
        .expect("entry should build");
        let loaded = loaded_thread_for_rendering(
            vec![entry.clone()],
            vec![(LedgerEntryId(1), 1, "2026-05-25 18:55")],
        );

        let actual = render_public_entry_message(&loaded, &entry, &sample_member_names(), None)
            .expect("public message should render");

        assert!(actual.contains("- 三郎: 0円"));
    }

    #[test]
    fn render_public_entry_message_supports_balance_adjustment_with_member_id_order() {
        let expense =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let seal =
            LedgerEntry::non_expense(LedgerEntryId(2), LedgerHistorySealed::new(LedgerEntryId(1)));
        let entry = LedgerEntry::non_expense(
            LedgerEntryId(3),
            BalanceAdjusted::new(
                vec![
                    BalanceAdjustment {
                        member_id: MemberId(2),
                        amount: Money::from_i64(100),
                    },
                    BalanceAdjustment {
                        member_id: MemberId(1),
                        amount: Money::from_i64(-100),
                    },
                ],
                AdjustmentReason::new("補正").expect("reason should be valid"),
                external_correction_source_for_transport_decode(),
            )
            .expect("adjustment should be valid"),
        );
        let loaded = loaded_thread_for_rendering(
            vec![expense, seal, entry.clone()],
            vec![
                (LedgerEntryId(1), 1, "2026-05-24 12:00"),
                (LedgerEntryId(2), 2, "2026-05-24 12:30"),
                (LedgerEntryId(3), 3, "2026-05-25 18:55"),
            ],
        );
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("Zed")),
            (MemberId(2), SmolStr::new("Alice")),
            (MemberId(9), SmolStr::new("記録者")),
        ]);

        let actual = render_public_entry_message(&loaded, &entry, &member_names, None)
            .expect("public message should render");
        let zed = actual
            .find("Zed 支払い 100円")
            .expect("first impact should exist");
        let alice = actual
            .find("Alice 受け取り 100円")
            .expect("second impact should exist");

        assert!(actual.starts_with("残高補正 [#3]"));
        assert!(zed < alice);
    }

    #[test]
    fn panel_post_message_uses_the_presenter_contract() {
        let actual = render_panel_post_message(None, false).expect("panel should render");
        let expected = rendered_surface_to_message(
            DiscordLedgerPresenter::render_panel(&PanelSurfaceModel {
                thread_cue: i18n::panel_thread_cue_pending().to_owned(),
                status_line: Some(i18n::panel_first_record_prompt().to_owned()),
                button_states: PanelButtonStates::default(),
                ephemeral: false,
            })
            .expect("surface should render"),
        );

        assert_eq!(actual, expected);
    }

    #[test]
    fn panel_post_message_uses_known_thread_cue_when_the_ledger_is_cached() {
        let actual = render_panel_post_message(Some(ChannelId::new(77)), false)
            .expect("panel should render");
        let expected = rendered_surface_to_message(
            DiscordLedgerPresenter::render_panel(&PanelSurfaceModel {
                thread_cue: format!("{}", i18n::panel_thread_cue_known("<#77>")),
                status_line: None,
                button_states: PanelButtonStates::default(),
                ephemeral: false,
            })
            .expect("surface should render"),
        );

        assert_eq!(actual, expected);
    }

    #[test]
    fn panel_thread_discovery_filters_named_threads_and_prefers_the_first_verified_candidate() {
        let parent_channel_id = ChannelId::new(10);
        let candidates = canonical_thread_candidate_ids(
            parent_channel_id,
            vec![
                (
                    ChannelId::new(1),
                    ChannelType::PublicThread,
                    "雑談".to_owned(),
                    Some(parent_channel_id),
                ),
                (
                    ChannelId::new(2),
                    ChannelType::PublicThread,
                    CANONICAL_LEDGER_THREAD_NAME.to_owned(),
                    Some(ChannelId::new(99)),
                ),
                (
                    ChannelId::new(3),
                    ChannelType::PrivateThread,
                    CANONICAL_LEDGER_THREAD_NAME.to_owned(),
                    Some(parent_channel_id),
                ),
                (
                    ChannelId::new(4),
                    ChannelType::PublicThread,
                    CANONICAL_LEDGER_THREAD_NAME.to_owned(),
                    Some(parent_channel_id),
                ),
                (
                    ChannelId::new(4),
                    ChannelType::PublicThread,
                    CANONICAL_LEDGER_THREAD_NAME.to_owned(),
                    Some(parent_channel_id),
                ),
                (
                    ChannelId::new(5),
                    ChannelType::PublicThread,
                    CANONICAL_LEDGER_THREAD_NAME.to_owned(),
                    Some(parent_channel_id),
                ),
            ],
        );

        assert_eq!(candidates, vec![ChannelId::new(4), ChannelId::new(5)]);
        assert_eq!(
            first_verified_canonical_thread_id_for_panel(
                candidates
                    .into_iter()
                    .map(|thread_id| (thread_id, thread_id == ChannelId::new(5))),
            ),
            Some(ChannelId::new(5))
        );
    }

    #[test]
    fn void_flow_uses_the_thread_channel_for_success_targeting() {
        let parent_channel_id = ChannelId::new(10);
        let thread_channel_id = ChannelId::new(20);

        assert_eq!(
            resolved_void_thread_channel_id(thread_channel_id, parent_channel_id, None),
            thread_channel_id
        );
        assert_eq!(
            resolved_void_thread_channel_id(
                parent_channel_id,
                parent_channel_id,
                Some(thread_channel_id)
            ),
            thread_channel_id
        );
        assert_eq!(
            resolved_void_thread_channel_id(parent_channel_id, parent_channel_id, None),
            parent_channel_id
        );
    }

    #[test]
    fn panel_post_message_uses_bootstrap_diagnosis_while_first_write_is_uncertain() {
        let actual = render_panel_post_message(None, true).expect("panel should render");
        let expected = rendered_surface_to_message(
            DiscordLedgerPresenter::render_panel(&PanelSurfaceModel {
                thread_cue: i18n::panel_thread_cue_pending().to_owned(),
                status_line: Some(i18n::panel_bootstrap_diagnosis().to_owned()),
                button_states: PanelButtonStates::default(),
                ephemeral: false,
            })
            .expect("surface should render"),
        );

        assert_eq!(actual, expected);
    }

    #[test]
    fn step_3_adapter_surfaces_route_user_facing_copy_through_i18n() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".into()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                ),
                payer: Some(MemberId(1)),
                explicit_members: vec![MemberId(2)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: Some(vec![ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(1),
                }]),
                recording: false,
            },
        );
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
        ]);

        let panel = render_panel_post_message(None, false).expect("panel should render");
        let editor = poc.render_expense_editor(session_id, &roster, &member_names, &HashMap::new());
        let confirmation = poc
            .render_expense_confirmation(session_id, &roster, &member_names)
            .expect("confirmation should render");
        let void_entry =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let loaded = loaded_thread_for_rendering(
            vec![void_entry.clone()],
            vec![(LedgerEntryId(1), 1, "2026-05-25 18:55")],
        );
        let void_selection = render_void_selection_body(&loaded, &[void_entry], &member_names)
            .expect("void selection should render");

        assert!(panel.0.contains(i18n::panel_thread_cue_pending()));
        assert!(panel.0.contains(i18n::panel_first_record_prompt()));
        assert!(editor.contains(i18n::expense_step_title_weight()));
        assert!(editor.contains(i18n::weight_editor_help()));
        assert!(confirmation.contains(i18n::expense_step_title_confirm()));
        assert!(confirmation.contains(i18n::expense_confirmation_participants_heading()));
        assert!(void_selection.contains(i18n::void_selection_window_guidance()));
    }

    #[test]
    fn render_ledger_surface_uses_transport_metadata_for_void_rows() {
        let target =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let void_entry = build_void_entry(LedgerEntryId(2), MemberId(9), LedgerEntryId(1));
        let loaded = loaded_thread_for_rendering(
            vec![target, void_entry],
            vec![
                (LedgerEntryId(1), 1, "2026-05-24 12:00"),
                (LedgerEntryId(2), 2, "2026-05-25 18:55"),
            ],
        );

        let actual = render_ledger_surface(
            &loaded,
            &sample_member_names(),
            ReadViewRoute::LedgerCommand,
            &ReadViewRenderState::default(),
        )
        .expect("ledger surface should render");

        assert!(actual.contains("2026-05-25 18:55"));
        assert!(actual.contains("https://discord.com/channels/1/2/2"));
    }

    #[test]
    fn render_void_selection_body_uses_transport_metadata_for_legacy_candidates() {
        let mut input = expense_input();
        input.effective_date = None;
        let entry = build_expense_entry(LedgerEntryId(1), &input).expect("entry should build");
        let loaded = loaded_thread_for_rendering(
            vec![entry.clone()],
            vec![(LedgerEntryId(1), 1, "2026-05-25 18:55")],
        );
        let actual = render_void_selection_body(&loaded, &[entry], &sample_member_names())
            .expect("void selection should render");

        assert!(actual.contains("2026-05-25"));
        assert!(actual.contains("https://discord.com/channels/1/2/1"));
    }

    #[test]
    fn replaying_recorded_settlement_zeroes_balances() {
        let expense =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let projected = replay_entries(vec![expense.clone()]).expect("ledger should replay");
        let previewed = preview_settlement_from_ledger(&projected).expect("preview should build");
        let settlement = build_settlement_entry(LedgerEntryId(2), MemberId(9), &previewed)
            .expect("settlement should build")
            .expect("preview should require a settlement event");
        let replayed = replay_entries(vec![expense, settlement]).expect("ledger should replay");

        assert!(
            replayed
                .state()
                .balances()
                .values()
                .all(|balance| *balance == Money::ZERO)
        );
    }

    #[test]
    fn build_void_entry_uses_entry_voided_event() {
        let void = build_void_entry(LedgerEntryId(2), MemberId(9), LedgerEntryId(1));

        assert!(matches!(void.event, LedgerEvent::EntryVoided(_)));
    }

    #[test]
    fn replaying_void_entry_cancels_target_balance_effect() {
        let expense =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let void = build_void_entry(LedgerEntryId(2), MemberId(9), LedgerEntryId(1));
        let replayed = replay_entries(vec![expense, void]).expect("ledger should replay");

        assert!(replayed.state().balances().is_empty());
    }

    #[test]
    fn replaying_void_entry_marks_target_as_voided() {
        let expense =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let void = build_void_entry(LedgerEntryId(2), MemberId(9), LedgerEntryId(1));
        let replayed = replay_entries(vec![expense, void]).expect("ledger should replay");

        assert!(
            replayed
                .state()
                .voided_entry_ids()
                .contains(&LedgerEntryId(1))
        );
    }

    #[test]
    fn void_candidates_exclude_entries_already_sealed() {
        let expense =
            build_expense_entry(LedgerEntryId(1), &expense_input()).expect("entry should build");
        let mut seal =
            LedgerEntry::non_expense(LedgerEntryId(2), LedgerHistorySealed::new(LedgerEntryId(1)));
        seal.metadata.recorded_by = Some(MemberId(9));
        seal.metadata.source = Some(
            LedgerSourceCanonical::discord_ui("seal/slash/v1").expect("source should be valid"),
        );

        let projected = replay_entries(vec![expense.clone(), seal]).expect("ledger should replay");

        assert!(candidate_entries_for_void(&[expense], &projected).is_empty());
    }

    #[test]
    fn hash_hex_round_trip() {
        let text = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
        let decoded = decode_hash(text).expect("hash should decode");
        assert_eq!(encode_hash(decoded), text);
    }

    #[test]
    fn safe_message_builders_disable_all_mentions() {
        let expected = serde_json::json!({
            "parse": [],
            "users": [],
            "roles": [],
            "replied_user": false,
        });

        assert_eq!(
            serde_json::to_value(suppressed_allowed_mentions())
                .expect("allowed mentions should serialize"),
            expected
        );
        assert_eq!(
            serde_json::to_value(safe_interaction_response_message().content("ok"))
                .expect("interaction response should serialize")["allowed_mentions"],
            expected
        );
        assert_eq!(
            serde_json::to_value(safe_edit_interaction_response().content("ok"))
                .expect("interaction edit should serialize")["allowed_mentions"],
            expected
        );
        assert_eq!(
            serde_json::to_value(safe_create_message().content("ok"))
                .expect("message should serialize")["allowed_mentions"],
            expected
        );
        assert_eq!(
            serde_json::to_value(safe_edit_message().content("ok"))
                .expect("message edit should serialize")["allowed_mentions"],
            expected
        );
    }

    #[test]
    fn expense_modal_uses_the_fixed_field_labels_and_hints() {
        let modal =
            serde_json::to_value(expense_modal(7, 9, None)).expect("modal should serialize");
        let inputs = modal["components"]
            .as_array()
            .expect("modal rows should serialize")
            .iter()
            .map(|row| &row["components"][0])
            .collect::<Vec<_>>();

        assert_eq!(modal["title"], i18n::expense_modal_title());
        assert_eq!(inputs[0]["label"], i18n::expense_modal_amount_label());
        assert_eq!(
            inputs[0]["placeholder"],
            i18n::expense_modal_amount_placeholder()
        );
        assert_eq!(inputs[1]["label"], i18n::expense_modal_note_label());
        assert_eq!(
            inputs[1]["placeholder"],
            i18n::expense_modal_note_placeholder()
        );
        assert_eq!(inputs[2]["label"], i18n::expense_modal_date_label());
        assert_eq!(
            inputs[2]["placeholder"],
            i18n::expense_modal_date_placeholder()
        );
    }

    #[test]
    fn expense_weight_modal_uses_the_fixed_chrome() {
        let modal = serde_json::to_value(expense_weight_modal(7, 9, "山田太郎 = 1".to_owned()))
            .expect("modal should serialize");
        let input = &modal["components"][0]["components"][0];

        assert_eq!(modal["title"], i18n::weight_editor_modal_title());
        assert_eq!(input["label"], i18n::weight_editor_input_label());
        assert_eq!(input["placeholder"], i18n::weight_editor_placeholder());
        assert_eq!(input["value"], "山田太郎 = 1");
    }

    #[test]
    fn modal_titles_and_text_inputs_stay_within_discord_budgets() {
        let expense_modal_json =
            serde_json::to_value(expense_modal(7, 9, None)).expect("modal should serialize");
        let expense_inputs = expense_modal_json["components"]
            .as_array()
            .expect("expense modal rows should serialize")
            .iter()
            .map(|row| &row["components"][0])
            .collect::<Vec<_>>();
        let weight_modal_json =
            serde_json::to_value(expense_weight_modal(7, 9, "山田太郎 = 1".to_owned()))
                .expect("modal should serialize");
        let weight_input = &weight_modal_json["components"][0]["components"][0];

        for title in [
            expense_modal_json["title"]
                .as_str()
                .expect("expense modal title should exist"),
            weight_modal_json["title"]
                .as_str()
                .expect("weight modal title should exist"),
        ] {
            validate_modal_title(title).expect("modal title should fit Discord limits");
        }
        for label in expense_inputs
            .iter()
            .map(|input| input["label"].as_str().expect("label should exist"))
            .chain(std::iter::once(
                weight_input["label"]
                    .as_str()
                    .expect("weight modal label should exist"),
            ))
        {
            validate_text_input_label(label).expect("text input label should fit Discord limits");
        }
        for placeholder in expense_inputs
            .iter()
            .map(|input| {
                input["placeholder"]
                    .as_str()
                    .expect("placeholder should exist")
            })
            .chain(std::iter::once(
                weight_input["placeholder"]
                    .as_str()
                    .expect("weight modal placeholder should exist"),
            ))
        {
            validate_text_input_placeholder(placeholder)
                .expect("text input placeholder should fit Discord limits");
        }
    }

    #[test]
    fn expense_search_modals_use_the_fixed_member_and_role_chrome() {
        let member_modal = serde_json::to_value(expense_search_modal(
            7,
            9,
            ExpensePickerKind::Participants,
            None,
        ))
        .expect("modal should serialize");
        let role_modal =
            serde_json::to_value(expense_search_modal(7, 9, ExpensePickerKind::Roles, None))
                .expect("modal should serialize");

        assert_eq!(member_modal["title"], i18n::member_search_modal_title());
        assert_eq!(
            member_modal["components"][0]["components"][0]["label"],
            i18n::member_search_input_label()
        );
        assert_eq!(
            member_modal["components"][0]["components"][0]["placeholder"],
            i18n::member_search_placeholder()
        );
        assert_eq!(role_modal["title"], i18n::role_search_modal_title());
        assert_eq!(
            role_modal["components"][0]["components"][0]["label"],
            i18n::role_search_input_label()
        );
        assert_eq!(
            role_modal["components"][0]["components"][0]["placeholder"],
            i18n::role_search_placeholder()
        );
    }

    #[test]
    fn picker_entries_use_locale_aware_ordering_for_members_and_roles() {
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("Ao")),
            (MemberId(2), SmolStr::new("ao")),
        ]);
        let role_labels = HashMap::from([
            (
                RoleId(1),
                SafeLiteralText::from_roster_label("Ao").expect("label should be valid"),
            ),
            (
                RoleId(2),
                SafeLiteralText::from_roster_label("ao").expect("label should be valid"),
            ),
        ]);

        assert_eq!(
            sorted_member_picker_entries(&roster, &member_names)
                .into_iter()
                .map(|(member_id, _)| member_id)
                .collect::<Vec<_>>(),
            vec![MemberId(1), MemberId(2)]
        );
        assert_eq!(
            sorted_role_picker_entries(&role_labels)
                .into_iter()
                .map(|(role_id, _)| role_id)
                .collect::<Vec<_>>(),
            vec![RoleId(1), RoleId(2)]
        );
    }

    #[test]
    fn picker_search_target_page_handles_blank_miss_and_page_jump_queries() {
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: (1..=30).map(MemberId).collect(),
            ..Default::default()
        };
        let member_names = (1..=30)
            .map(|index| (MemberId(index), SmolStr::new(format!("member-{index:02}"))))
            .collect::<HashMap<_, _>>();
        let role_labels = (1..=30)
            .map(|index| {
                (
                    RoleId(index),
                    SafeLiteralText::from_roster_label(&format!("role-{index:02}"))
                        .expect("label should be valid"),
                )
            })
            .collect::<HashMap<_, _>>();

        assert_eq!(
            picker_search_target_page(
                ExpensePickerKind::Participants,
                &roster,
                &member_names,
                &role_labels,
                "   ",
            ),
            None
        );
        assert_eq!(
            picker_search_target_page(
                ExpensePickerKind::Participants,
                &roster,
                &member_names,
                &role_labels,
                "missing",
            ),
            None
        );
        assert_eq!(
            picker_search_target_page(
                ExpensePickerKind::Participants,
                &roster,
                &member_names,
                &role_labels,
                "BER-30",
            ),
            Some(1)
        );
        assert_eq!(
            picker_search_target_page(
                ExpensePickerKind::Roles,
                &crate::discord::ports::RosterSnapshot::default(),
                &HashMap::new(),
                &role_labels,
                "OLE-30",
            ),
            Some(1)
        );
    }

    #[test]
    fn expense_participant_stage_components_use_fixed_source_and_navigation_controls() {
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: Some("ランチ".into()),
            effective_date: Some(
                LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
            ),
            payer: Some(MemberId(1)),
            explicit_members: vec![MemberId(2)],
            selected_roles: vec![RoleId(5)],
            include_members_group: false,
            active_picker: ExpensePickerKind::Participants,
            picker_query: None,
            picker_page: 0,
            weight_overrides: HashMap::from([(MemberId(2), Weight(2))]),
            frozen_participants: None,
            recording: false,
        };
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
        ]);
        let role_labels = HashMap::from([(
            RoleId(5),
            SafeLiteralText::from_roster_label("開発").expect("label should be valid"),
        )]);

        let components = serde_json::to_value(expense_editor_components(
            7,
            9,
            &draft,
            &roster,
            &member_names,
            &role_labels,
        ))
        .expect("components should serialize");

        let source_labels = components[0]["components"]
            .as_array()
            .expect("source row should serialize")
            .iter()
            .map(|component| {
                component["label"]
                    .as_str()
                    .expect("button label should exist")
            })
            .collect::<Vec<_>>();
        assert_eq!(
            source_labels,
            vec![
                i18n::participant_source_individual_label(),
                i18n::participant_source_role_label(),
                i18n::participant_source_members_label(),
            ]
        );
        assert_eq!(
            components[1]["components"][0]["placeholder"],
            i18n::participant_source_individual_placeholder()
        );
        assert_eq!(
            components[2]["components"][0]["label"],
            i18n::picker_previous_page_label()
        );
        assert_eq!(
            components[2]["components"][1]["label"],
            i18n::picker_next_page_label()
        );
        assert_eq!(
            components[2]["components"][2]["label"],
            i18n::picker_search_label()
        );
        assert_eq!(
            components[2]["components"][3]["label"],
            i18n::individual_clear_label()
        );
        let clear_labels = components[3]["components"]
            .as_array()
            .expect("clear row should serialize")
            .iter()
            .map(|component| {
                component["label"]
                    .as_str()
                    .expect("button label should exist")
            })
            .collect::<Vec<_>>();
        assert_eq!(
            clear_labels,
            vec![
                i18n::participant_source_clear_roles_label(),
                i18n::participant_source_clear_members_label(),
            ]
        );
        let wizard_labels = components[4]["components"]
            .as_array()
            .expect("wizard row should serialize")
            .iter()
            .map(|component| {
                component["label"]
                    .as_str()
                    .expect("button label should exist")
            })
            .collect::<Vec<_>>();
        assert_eq!(
            wizard_labels,
            vec![
                i18n::expense_back_label(),
                i18n::expense_cancel_label(),
                i18n::expense_to_weights_label(),
            ]
        );
    }

    #[test]
    fn expense_editor_picker_placeholders_cover_payer_and_role_chrome() {
        let payer_draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: Some("ランチ".into()),
            effective_date: Some(
                LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
            ),
            payer: Some(MemberId(1)),
            explicit_members: vec![MemberId(2)],
            selected_roles: vec![RoleId(5)],
            include_members_group: false,
            active_picker: ExpensePickerKind::Payer,
            picker_query: None,
            picker_page: 0,
            weight_overrides: HashMap::new(),
            frozen_participants: None,
            recording: false,
        };
        let mut role_draft = payer_draft.clone();
        role_draft.active_picker = ExpensePickerKind::Roles;
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
        ]);
        let role_labels = HashMap::from([(
            RoleId(5),
            SafeLiteralText::from_roster_label("開発").expect("label should be valid"),
        )]);

        let payer_components = serde_json::to_value(expense_editor_components(
            7,
            9,
            &payer_draft,
            &roster,
            &member_names,
            &role_labels,
        ))
        .expect("components should serialize");
        let role_components = serde_json::to_value(expense_editor_components(
            7,
            9,
            &role_draft,
            &roster,
            &member_names,
            &role_labels,
        ))
        .expect("components should serialize");

        assert_eq!(
            payer_components[0]["components"][0]["placeholder"],
            i18n::expense_payer_placeholder()
        );
        assert_eq!(
            role_components[1]["components"][0]["placeholder"],
            i18n::participant_source_role_placeholder()
        );
        assert_eq!(
            payer_components[2]["components"][2]["label"],
            i18n::expense_next_label()
        );
    }

    #[test]
    fn expense_editor_renders_member_picker_paging_helper_and_cross_page_selection_state() {
        let poc = DiscordLedgerPoc::new();
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: Some("ランチ".into()),
            effective_date: Some(
                LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
            ),
            payer: Some(MemberId(1)),
            explicit_members: vec![MemberId(1), MemberId(30)],
            selected_roles: Vec::new(),
            include_members_group: false,
            active_picker: ExpensePickerKind::Participants,
            picker_query: None,
            picker_page: 1,
            weight_overrides: HashMap::new(),
            frozen_participants: None,
            recording: false,
        };
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: (1..=30).map(MemberId).collect(),
            ..Default::default()
        };
        let member_names = (1..=30)
            .map(|index| (MemberId(index), SmolStr::new(format!("member-{index:02}"))))
            .collect::<HashMap<_, _>>();
        poc.expense_drafts.insert(7, draft.clone());

        let body = poc.render_expense_editor(7, &roster, &member_names, &HashMap::new());
        let components = serde_json::to_value(expense_editor_components(
            7,
            9,
            &draft,
            &roster,
            &member_names,
            &HashMap::new(),
        ))
        .expect("components should serialize");

        assert!(body.contains("ページ 2/2"));
        assert!(body.contains("26-30 / 30人"));
        assert!(body.contains(i18n::member_picker_help()));
        assert!(body.contains(participant_source_help_line()));
        assert!(body.contains("member-01"));
        assert!(body.contains("member-30"));
        assert_eq!(
            components[2]["components"][0]["disabled"],
            serde_json::Value::Bool(false)
        );
        assert_eq!(
            components[2]["components"][1]["disabled"],
            serde_json::Value::Bool(true)
        );
        assert_eq!(
            components[1]["components"][0]["options"]
                .as_array()
                .map(Vec::len),
            Some(5)
        );
        assert_eq!(
            components[1]["components"][0]["options"][4]["default"],
            serde_json::Value::Bool(true)
        );
    }

    #[test]
    fn expense_editor_renders_role_picker_paging_helper_and_cross_page_selection_state() {
        let poc = DiscordLedgerPoc::new();
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: Some("ランチ".into()),
            effective_date: Some(
                LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
            ),
            payer: Some(MemberId(1)),
            explicit_members: Vec::new(),
            selected_roles: vec![RoleId(1), RoleId(30)],
            include_members_group: false,
            active_picker: ExpensePickerKind::Roles,
            picker_query: None,
            picker_page: 1,
            weight_overrides: HashMap::new(),
            frozen_participants: None,
            recording: false,
        };
        let role_labels = (1..=30)
            .map(|index| {
                (
                    RoleId(index),
                    SafeLiteralText::from_roster_label(&format!("role-{index:02}"))
                        .expect("label should be valid"),
                )
            })
            .collect::<HashMap<_, _>>();
        poc.expense_drafts.insert(7, draft.clone());

        let body = poc.render_expense_editor(
            7,
            &crate::discord::ports::RosterSnapshot::default(),
            &HashMap::new(),
            &role_labels,
        );
        let components = serde_json::to_value(expense_editor_components(
            7,
            9,
            &draft,
            &crate::discord::ports::RosterSnapshot::default(),
            &HashMap::new(),
            &role_labels,
        ))
        .expect("components should serialize");

        assert!(body.contains("ページ 2/2"));
        assert!(body.contains("26-30 / 30人"));
        assert!(body.contains(i18n::role_picker_help()));
        assert!(body.contains(participant_source_help_line()));
        assert!(body.contains("role-01"));
        assert!(body.contains("role-30"));
        assert_eq!(
            components[2]["components"][0]["disabled"],
            serde_json::Value::Bool(false)
        );
        assert_eq!(
            components[2]["components"][1]["disabled"],
            serde_json::Value::Bool(true)
        );
        assert_eq!(
            components[1]["components"][0]["options"]
                .as_array()
                .map(Vec::len),
            Some(5)
        );
        assert_eq!(
            components[1]["components"][0]["options"][4]["default"],
            serde_json::Value::Bool(true)
        );
    }

    #[test]
    fn confirmation_share_amounts_fail_closed_when_duplicate_participants_are_present() {
        let poc = DiscordLedgerPoc::new();
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: Some("ランチ".into()),
            effective_date: Some(
                LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
            ),
            payer: Some(MemberId(1)),
            explicit_members: vec![MemberId(2)],
            selected_roles: Vec::new(),
            include_members_group: false,
            active_picker: ExpensePickerKind::Participants,
            picker_query: None,
            picker_page: 0,
            weight_overrides: HashMap::new(),
            frozen_participants: None,
            recording: false,
        };

        let actual = poc.confirmation_share_amounts(
            &draft,
            &[
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(1),
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(1),
                },
            ],
        );

        assert_eq!(
            actual,
            Err("経費の台帳記録を作成できませんでした。".to_owned())
        );
    }

    #[test]
    fn expense_weight_stage_uses_the_fixed_title_and_three_line_summary() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".into()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                ),
                payer: Some(MemberId(1)),
                explicit_members: vec![MemberId(2), MemberId(3)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: Some(vec![
                    ExpenseParticipantSelection {
                        member_id: MemberId(2),
                        weight: Weight(1),
                    },
                    ExpenseParticipantSelection {
                        member_id: MemberId(3),
                        weight: Weight(1),
                    },
                ]),
                recording: false,
            },
        );
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
            (MemberId(3), SmolStr::new("次郎")),
        ]);

        let editor = poc.render_expense_editor(session_id, &roster, &member_names, &HashMap::new());

        assert!(editor.starts_with(i18n::expense_step_title_weight()));
        let amount = editor
            .find("金額: 10000円")
            .expect("amount line should exist");
        let date = editor
            .find("日付: 2026-05-01")
            .expect("date line should exist");
        let note = editor.find("メモ: ランチ").expect("note line should exist");
        let payer = editor
            .find("支払者: 太郎")
            .expect("payer line should exist");
        assert!(amount < date);
        assert!(date < note);
        assert!(note < payer);
        assert!(editor.contains("個別選択 2人: 花子, 次郎"));
        assert!(editor.contains(i18n::weight_editor_help()));
    }

    #[test]
    fn expense_editor_selected_member_summary_uses_locale_aware_ordering() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".into()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                ),
                payer: Some(MemberId(3)),
                explicit_members: vec![MemberId(1), MemberId(2)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: None,
                recording: false,
            },
        );
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("Ao")),
            (MemberId(2), SmolStr::new("ao")),
            (MemberId(3), SmolStr::new("payer")),
        ]);

        let editor = poc.render_expense_editor(session_id, &roster, &member_names, &HashMap::new());

        assert!(editor.contains("個別選択 2人: Ao, ao"));
    }

    #[test]
    fn expense_editor_content_bounds_selected_member_and_role_summaries() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".into()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                ),
                payer: Some(MemberId(9)),
                explicit_members: vec![MemberId(1), MemberId(2), MemberId(3)],
                selected_roles: vec![RoleId(11), RoleId(12), RoleId(13), RoleId(14)],
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: None,
                recording: false,
            },
        );
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3), MemberId(9)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (
                MemberId(1),
                SmolStr::new(format!("member-1-{}", "a".repeat(80))),
            ),
            (
                MemberId(2),
                SmolStr::new(format!("member-2-{}", "b".repeat(80))),
            ),
            (
                MemberId(3),
                SmolStr::new(format!("member-3-{}", "c".repeat(80))),
            ),
            (MemberId(9), SmolStr::new("payer")),
        ]);
        let role_labels = HashMap::from([
            (
                RoleId(11),
                SafeLiteralText::from_roster_label(&format!("role-11-{}", "x".repeat(80)))
                    .expect("label should be valid"),
            ),
            (
                RoleId(12),
                SafeLiteralText::from_roster_label(&format!("role-12-{}", "y".repeat(80)))
                    .expect("label should be valid"),
            ),
            (
                RoleId(13),
                SafeLiteralText::from_roster_label(&format!("role-13-{}", "z".repeat(80)))
                    .expect("label should be valid"),
            ),
            (
                RoleId(14),
                SafeLiteralText::from_roster_label(&format!("role-14-{}", "w".repeat(80)))
                    .expect("label should be valid"),
            ),
        ]);

        let content = poc
            .validated_expense_editor_content(session_id, &roster, &member_names, &role_labels)
            .expect("long summaries should stay renderable");

        assert!(content.contains("個別選択 3人: "));
        assert!(content.contains(i18n::expense_selected_roles_prefix()));
        assert!(content.contains('…'));
        assert!(content.contains("ほか1件"));
    }

    #[test]
    fn review_pagination_keeps_instruction_guidance_and_recovery_cta_on_later_pages() {
        let pages = render_read_view_page_models(
            walicord_presentation::discord_ledger::paginate_read_view_model(ReadViewPageModel {
                kind: ReadViewKind::Review,
                route: ReadViewRoute::ReviewThread,
                title: i18n::panel_review_button_label().to_owned(),
                uncertain_write: false,
                stale_page: false,
                page_indicator: None,
                snapshot_notice: None,
                route_guidance_lines: Vec::new(),
                recovery_cta: RecoveryCta::ParentLink,
                recovery_url: Some("https://discord.com/channels/1/10".to_owned()),
                missing_thread_note: false,
                balances: (0..18)
                    .map(|index| BalanceRow {
                        display_name: SafeLiteralText::from_roster_label(&format!(
                            "member-{index:02}"
                        ))
                        .expect("label should be valid"),
                        amount: "300".to_owned(),
                        direction: BalanceDirection::Receive,
                    })
                    .collect(),
                transfers: (0..4)
                    .map(|index| TransferRow {
                        from_display_name: SafeLiteralText::from_roster_label(&format!(
                            "from-{index:02}"
                        ))
                        .expect("label should be valid"),
                        to_display_name: SafeLiteralText::from_roster_label(&format!(
                            "to-{index:02}"
                        ))
                        .expect("label should be valid"),
                        amount: "300".to_owned(),
                    })
                    .collect(),
                participants: Vec::new(),
                voided_entries: Vec::new(),
                sealed_range: None,
                balance_adjustments: Vec::new(),
                footer_lines: Vec::new(),
                visible_sections: ReadViewSectionVisibility::default(),
                empty_state: None,
                action_rows: Vec::new(),
                ephemeral: true,
            }),
        )
        .expect("review pages should render");

        assert_eq!(pages.len(), 2);
        assert!(
            pages[1]
                .body
                .contains(&i18n::page_indicator(2, 2).to_string())
        );
        assert!(pages[1].body.contains(i18n::snapshot_notice()));
        assert!(pages[1].body.contains(i18n::review_explainer()));
        assert!(pages[1].body.contains(i18n::review_preview_instruction()));
        assert!(pages[1].body.contains(i18n::route_task_guidance()));
        let components =
            serde_json::to_value(&pages[1].base_components).expect("components should serialize");
        assert_eq!(
            components[0]["components"][0]["label"],
            i18n::open_parent_channel_label()
        );
    }

    #[test]
    fn ledger_pagination_keeps_required_empty_sections_visible_on_later_pages() {
        let pages = render_read_view_page_models(
            walicord_presentation::discord_ledger::paginate_read_view_model(ReadViewPageModel {
                kind: ReadViewKind::Ledger,
                route: ReadViewRoute::LedgerCommand,
                title: i18n::panel_ledger_button_label().to_owned(),
                uncertain_write: false,
                stale_page: false,
                page_indicator: None,
                snapshot_notice: None,
                route_guidance_lines: Vec::new(),
                recovery_cta: RecoveryCta::None,
                recovery_url: None,
                missing_thread_note: false,
                balances: (0..20)
                    .map(|index| BalanceRow {
                        display_name: SafeLiteralText::from_roster_label(&format!(
                            "member-{index:02}"
                        ))
                        .expect("label should be valid"),
                        amount: "300".to_owned(),
                        direction: BalanceDirection::Receive,
                    })
                    .collect(),
                transfers: Vec::new(),
                participants: vec![
                    SafeLiteralText::from_roster_label("Alice").expect("label should be valid"),
                    SafeLiteralText::from_roster_label("Bob").expect("label should be valid"),
                ],
                voided_entries: Vec::new(),
                sealed_range: None,
                balance_adjustments: Vec::new(),
                footer_lines: vec!["表示範囲: 最新の検証済み台帳".to_owned()],
                visible_sections: ReadViewSectionVisibility::default(),
                empty_state: None,
                action_rows: Vec::new(),
                ephemeral: true,
            }),
        )
        .expect("ledger pages should render");

        assert_eq!(pages.len(), 2);
        assert!(pages[1].body.contains("取り消し済み\n取り消し済みなし"));
        assert!(pages[1].body.contains("確認済み\n確認済み: なし"));
        assert!(pages[1].body.contains("表示範囲: 最新の検証済み台帳"));
    }

    #[test]
    fn paged_non_empty_ledger_with_zero_balances_keeps_zero_balance_guidance() {
        let pages = render_read_view_page_models(
            walicord_presentation::discord_ledger::paginate_read_view_model(ReadViewPageModel {
                kind: ReadViewKind::Ledger,
                route: ReadViewRoute::LedgerCommand,
                title: i18n::panel_ledger_button_label().to_owned(),
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
                participants: (0..25)
                    .map(|index| {
                        SafeLiteralText::from_roster_label(&format!("member-{index:02}"))
                            .expect("label should be valid")
                    })
                    .collect(),
                voided_entries: Vec::new(),
                sealed_range: None,
                balance_adjustments: Vec::new(),
                footer_lines: vec!["表示範囲: 最新の検証済み台帳".to_owned()],
                visible_sections: ReadViewSectionVisibility::default(),
                empty_state: None,
                action_rows: Vec::new(),
                ephemeral: true,
            }),
        )
        .expect("ledger pages should render");

        assert_eq!(pages.len(), 2);
        assert!(
            pages[0]
                .body
                .contains("残高\n確認済み履歴と残高補正を含む現在差額")
        );
        assert!(pages[0].body.contains(i18n::ledger_zero_balances()));
    }

    #[test]
    fn expense_weight_stage_components_use_edit_reset_and_confirm_controls() {
        let draft = ExpenseDraft {
            actor_id: UserId::new(9),
            guild_id: GuildId::new(1),
            parent_channel_id: ChannelId::new(10),
            expires_at: interaction_state_expires_at(),
            amount: Money::from_i64(10_000),
            note: Some("ランチ".into()),
            effective_date: Some(
                LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
            ),
            payer: Some(MemberId(1)),
            explicit_members: vec![MemberId(2)],
            selected_roles: vec![RoleId(5)],
            include_members_group: false,
            active_picker: ExpensePickerKind::Participants,
            picker_query: None,
            picker_page: 0,
            weight_overrides: HashMap::from([(MemberId(2), Weight(2))]),
            frozen_participants: Some(vec![ExpenseParticipantSelection {
                member_id: MemberId(2),
                weight: Weight(2),
            }]),
            recording: false,
        };

        let components = serde_json::to_value(expense_editor_components(
            7,
            9,
            &draft,
            &crate::discord::ports::RosterSnapshot::default(),
            &HashMap::new(),
            &HashMap::new(),
        ))
        .expect("components should serialize");

        let action_labels = components[0]["components"]
            .as_array()
            .expect("action row should serialize")
            .iter()
            .map(|component| {
                component["label"]
                    .as_str()
                    .expect("button label should exist")
            })
            .collect::<Vec<_>>();
        let wizard_labels = components[1]["components"]
            .as_array()
            .expect("wizard row should serialize")
            .iter()
            .map(|component| {
                component["label"]
                    .as_str()
                    .expect("button label should exist")
            })
            .collect::<Vec<_>>();

        assert_eq!(
            action_labels,
            vec![
                i18n::expense_weight_edit_label(),
                i18n::weight_editor_reset_label(),
            ]
        );
        assert_eq!(
            wizard_labels,
            vec![
                i18n::expense_back_label(),
                i18n::expense_cancel_label(),
                i18n::expense_to_confirm_label(),
            ]
        );
    }

    #[test]
    fn expense_confirmation_marks_defaulted_weight_members_after_drift_refresh() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".into()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                ),
                payer: Some(MemberId(1)),
                explicit_members: vec![MemberId(2), MemberId(3)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: Some(vec![
                    ExpenseParticipantSelection {
                        member_id: MemberId(2),
                        weight: Weight(2),
                    },
                    ExpenseParticipantSelection {
                        member_id: MemberId(3),
                        weight: Weight(1),
                    },
                ]),
                recording: false,
            },
        );
        poc.expense_defaulted_weight_members
            .insert(session_id, HashSet::from([MemberId(3)]));
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(3)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("太郎")),
            (MemberId(2), SmolStr::new("花子")),
            (MemberId(3), SmolStr::new("次郎")),
        ]);

        let confirmation = poc
            .render_expense_confirmation(session_id, &roster, &member_names)
            .expect("confirmation should render");

        assert!(confirmation.starts_with(i18n::expense_step_title_confirm()));
        assert!(confirmation.contains("- 次郎: 3333円 (×1) [直接選択] [既定値 1]"));
        assert!(confirmation.contains(i18n::weight_editor_help()));
    }

    #[test]
    fn expense_confirmation_rows_follow_the_locale_aware_member_order() {
        let poc = DiscordLedgerPoc::new();
        let session_id = 1;
        poc.expense_drafts.insert(
            session_id,
            ExpenseDraft {
                actor_id: UserId::new(9),
                guild_id: GuildId::new(1),
                parent_channel_id: ChannelId::new(10),
                expires_at: interaction_state_expires_at(),
                amount: Money::from_i64(10_000),
                note: Some("ランチ".into()),
                effective_date: Some(
                    LedgerEffectiveDate::new("2026-05-01").expect("date should be valid"),
                ),
                payer: Some(MemberId(9)),
                explicit_members: vec![MemberId(1), MemberId(2)],
                selected_roles: Vec::new(),
                include_members_group: false,
                active_picker: ExpensePickerKind::Participants,
                picker_query: None,
                picker_page: 0,
                weight_overrides: HashMap::new(),
                frozen_participants: Some(vec![
                    ExpenseParticipantSelection {
                        member_id: MemberId(1),
                        weight: Weight(1),
                    },
                    ExpenseParticipantSelection {
                        member_id: MemberId(2),
                        weight: Weight(1),
                    },
                ]),
                recording: false,
            },
        );
        let roster = crate::discord::ports::RosterSnapshot {
            member_ids: vec![MemberId(1), MemberId(2), MemberId(9)],
            ..Default::default()
        };
        let member_names = HashMap::from([
            (MemberId(1), SmolStr::new("Ao")),
            (MemberId(2), SmolStr::new("ao")),
            (MemberId(9), SmolStr::new("記録者")),
        ]);

        let confirmation = poc
            .render_expense_confirmation(session_id, &roster, &member_names)
            .expect("confirmation should render");

        let uppercase = confirmation
            .find("- Ao:")
            .expect("uppercase row should exist");
        let lowercase = confirmation
            .find("- ao:")
            .expect("lowercase row should exist");
        assert!(uppercase < lowercase);
    }

    #[test]
    fn confirmation_components_offer_basic_info_and_weight_edits_before_recording() {
        let components = serde_json::to_value(expense_confirmation_components(7, 9))
            .expect("components should serialize");
        let navigation_labels = components[0]["components"]
            .as_array()
            .expect("button row should be serialized")
            .iter()
            .map(|component| {
                component["label"]
                    .as_str()
                    .expect("button label should exist")
            })
            .collect::<Vec<_>>();
        let recovery_labels = components[1]["components"]
            .as_array()
            .expect("button row should be serialized")
            .iter()
            .map(|component| {
                component["label"]
                    .as_str()
                    .expect("button label should exist")
            })
            .collect::<Vec<_>>();
        let primary_labels = components[2]["components"]
            .as_array()
            .expect("button row should be serialized")
            .iter()
            .map(|component| {
                component["label"]
                    .as_str()
                    .expect("button label should exist")
            })
            .collect::<Vec<_>>();

        assert_eq!(
            navigation_labels,
            vec![
                i18n::expense_back_label(),
                i18n::expense_weight_edit_label(),
                i18n::weight_editor_reset_label(),
            ]
        );
        assert_eq!(
            recovery_labels,
            vec![
                i18n::expense_revise_label(),
                i18n::expense_basic_info_edit_label(),
            ]
        );
        assert_eq!(
            primary_labels,
            vec![i18n::expense_cancel_label(), i18n::expense_record_label(),]
        );
    }

    mod normalize_date_input_tests {
        use super::*;
        use rstest::rstest;

        fn fixed_today() -> NaiveDate {
            NaiveDate::from_ymd_opt(2026, 5, 11).unwrap()
        }

        fn d(y: i32, m: u32, day: u32) -> Option<NaiveDate> {
            NaiveDate::from_ymd_opt(y, m, day)
        }

        #[rstest]
        #[case("今日", d(2026, 5, 11))]
        #[case("today", d(2026, 5, 11))]
        #[case("きょう", d(2026, 5, 11))]
        #[case("昨日", d(2026, 5, 10))]
        #[case("yesterday", d(2026, 5, 10))]
        #[case("きのう", d(2026, 5, 10))]
        #[case("2026-03-15", d(2026, 3, 15))]
        #[case(" 2026-03-15 ", d(2026, 3, 15))]
        #[case("2026/03/15", d(2026, 3, 15))]
        #[case("5/1", d(2026, 5, 1))]
        #[case("12/31", d(2026, 12, 31))]
        #[case("05/01/2026", None)]
        #[case("not-a-date", None)]
        #[case("", None)]
        fn parses_to_naive_date(#[case] input: &str, #[case] expected: Option<NaiveDate>) {
            let actual = normalize_date_input(input, fixed_today());
            assert_eq!(actual, expected);
        }
    }
}
