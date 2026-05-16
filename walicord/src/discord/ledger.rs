use chrono::{Datelike, Local, NaiveDate};
use fs2::FileExt;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use serenity::{
    all::{
        ActionRowComponent, ButtonStyle, ChannelId, CommandInteraction, ComponentInteraction,
        ComponentInteractionDataKind, CreateActionRow, CreateAttachment, CreateButton,
        CreateInputText, CreateInteractionResponse, CreateInteractionResponseMessage, CreateModal,
        CreateSelectMenu, CreateSelectMenuKind, CreateSelectMenuOption, EditInteractionResponse,
        GuildId, InputTextStyle, Message, MessageId, ModalInteraction, RoleId as SerenityRoleId,
        UserId,
    },
    builder::CreateMessage,
    model::channel::ChannelType,
    prelude::*,
};
use smol_str::SmolStr;
use std::{
    collections::{HashMap, HashSet},
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
    PreviewConfirmationBinding, PreviewedSettlement, SettleUpPolicy, SettlementOptimizationError,
    ledger::{
        AdjustmentReason, AllocationSnapshot, BalanceAdjusted, BalanceAdjustment,
        BalanceAdjustmentSource, EntryHash, EntryVoided, ExpenseNote, ExpenseRecorded,
        LedgerEffectiveDate, LedgerEntry, LedgerEntryId, LedgerEntryMetadata, LedgerEvent,
        LedgerHashSuite, LedgerHistorySealed, LedgerId, LedgerSourceCanonical,
        LedgerSourceCanonicalKind, LedgerState, MemberAmount, MemberWeight,
        NormalizedSettlementPlanRecorded, ProjectedEntryKind, ProjectedLedger, SchemaVersion,
        UnverifiedLedgerStoreEnvelope, VerifiedLedgerStoreEnvelope,
        external_correction_source_for_transport_decode, ledger_chain_genesis_sha256_v1,
        load_and_replay_verified_sha256_v1, make_unverified_envelope_sha256_v1, replay_entries,
    },
};
use walicord_domain::{
    Money, SettlementContext, Transfer,
    model::{MemberId, RoleId, Weight},
};
use walicord_infrastructure::HighsSettlementPlanner;

pub const LEDGER_ATTACHMENT_FILENAME: &str = "walicord-ledger-entry.json";
pub const EXPENSE_SOURCE_CANONICAL: &str = "expense/slash-modal/v1";
pub const SETTLE_SOURCE_CANONICAL: &str = "settle/slash/v1";
pub const VOID_SOURCE_CANONICAL: &str = "void/slash/v1";
pub const PREVIEW_EXPIRY_SECS: u64 = 15 * 60;
const INTERACTION_STATE_TTL_SECS: u64 = 60 * 60;
const ATTACHMENT_SCHEMA_VERSION: u32 = 1;
const EXPENSE_MODAL_PREFIX: &str = "expense:new:";
const EXPENSE_WEIGHTS_MODAL_PREFIX: &str = "expense:weights:";
const EXPENSE_PAYER_PREFIX: &str = "expense:payer:";
const EXPENSE_PARTICIPANTS_PREFIX: &str = "expense:participants:";
const EXPENSE_ROLES_PREFIX: &str = "expense:roles:";
const EXPENSE_GROUPS_PREFIX: &str = "expense:groups:";
const EXPENSE_OPEN_WEIGHTS_PREFIX: &str = "expense:open-weights:";
const EXPENSE_CONFIRM_PREFIX: &str = "expense:confirm:";
const EXPENSE_EDIT_PREFIX: &str = "expense:edit:";
const EXPENSE_RECORD_PREFIX: &str = "expense:record:";
const VOID_SELECT_PREFIX: &str = "void:select:";
const VOID_CONFIRM_PREFIX: &str = "void:confirm:";
const LEDGER_PANEL_EXPENSE_ID: &str = "ledger:panel:expense";
const LEDGER_PANEL_REVIEW_ID: &str = "ledger:panel:review";
const LEDGER_PANEL_LEDGER_ID: &str = "ledger:panel:ledger";
const LEDGER_PANEL_VOID_ID: &str = "ledger:panel:void";
const EXPENSE_AMOUNT_FIELD: &str = "expense_amount";
const EXPENSE_NOTE_FIELD: &str = "expense_note";
const EXPENSE_DATE_FIELD: &str = "expense_date";
const EXPENSE_WEIGHTS_FIELD: &str = "expense_weights";
const GROUP_MEMBERS_VALUE: &str = "MEMBERS";

pub struct DiscordLedgerPoc {
    interaction_nonce: u64,
    next_session_id: AtomicU64,
    expense_drafts: dashmap::DashMap<u64, ExpenseDraft>,
    void_drafts: dashmap::DashMap<u64, PendingVoidDraft>,
    settlement_previews: dashmap::DashMap<(u64, u64), PendingSettlementPreview>,
    runtime_lock: StdMutex<Option<CrossProcessFileLock>>,
    append_locks: dashmap::DashMap<ChannelId, Arc<Mutex<()>>>,
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
    weight_overrides: HashMap<MemberId, Weight>,
    frozen_participants: Option<Vec<ExpenseParticipantSelection>>,
    recording: bool,
}

#[derive(Debug, Clone)]
struct PendingVoidDraft {
    actor_id: UserId,
    guild_id: GuildId,
    ledger_channel_id: ChannelId,
    expires_at: SystemTime,
    selected_target: Option<LedgerEntryId>,
}

#[derive(Debug, Clone)]
struct PendingSettlementPreview {
    ledger_id: LedgerId,
    ledger_channel_id: ChannelId,
    binding: PreviewConfirmationBinding,
    previewed: PreviewedSettlement,
}

struct LoadedLedgerThread {
    ledger_id: LedgerId,
    channel_id: ChannelId,
    verified: Vec<VerifiedLedgerStoreEnvelope<MessageId>>,
    projected: ProjectedLedger,
    entries: Vec<LedgerEntry>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum ConfirmedExpenseSelectionsError {
    Missing,
    Drifted(Vec<ExpenseParticipantSelection>),
}

impl DiscordLedgerPoc {
    pub fn new() -> Self {
        Self {
            interaction_nonce: new_interaction_nonce(),
            next_session_id: AtomicU64::new(1),
            expense_drafts: dashmap::DashMap::new(),
            void_drafts: dashmap::DashMap::new(),
            settlement_previews: dashmap::DashMap::new(),
            runtime_lock: StdMutex::new(None),
            append_locks: dashmap::DashMap::new(),
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
            .filter_map(|preview| (preview.binding.expires_at <= now).then_some(*preview.key()))
            .collect();
        for key in stale_preview_keys {
            self.settlement_previews.remove(&key);
        }
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
        let _ = command.defer_ephemeral(&ctx.http).await;
        let loaded = match self.load_ledger(ctx, channel_id).await {
            Ok(loaded) => loaded,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return true;
            }
        };
        if loaded.entries.is_empty() {
            self.edit_command_message(
                ctx,
                command,
                "まだ経費が記録されていません。先に /expense を実行してください。",
                Vec::new(),
            )
            .await;
            return true;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let previewed = match preview_settlement_from_ledger(&loaded.projected) {
            Ok(previewed) => previewed,
            Err(_) => {
                self.edit_command_message(
                    ctx,
                    command,
                    "清算案を作成できませんでした。",
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
        );
        self.settlement_previews.insert(
            (loaded.channel_id.get(), command.user.id.get()),
            PendingSettlementPreview {
                ledger_id: loaded.ledger_id,
                ledger_channel_id: loaded.channel_id,
                binding,
                previewed: previewed.clone(),
            },
        );
        self.edit_command_message(
            ctx,
            command,
            render_review_message(&loaded.projected, &previewed, &member_names),
            Vec::new(),
        )
        .await;
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
            .component_session_id(ctx, component, custom_id, EXPENSE_GROUPS_PREFIX)
            .await
        {
            Ok(Some(session_id)) => {
                self.update_expense_groups(ctx, component, session_id, roster_provider)
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
            .component_session_id(ctx, component, custom_id, EXPENSE_EDIT_PREFIX)
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
        false
    }

    async fn start_expense(&self, ctx: &Context, command: &CommandInteraction) {
        let Some(guild_id) = command.guild_id else {
            let _ = command
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        CreateInteractionResponseMessage::new()
                            .ephemeral(true)
                            .content("サーバー内のチャンネルで実行してください。"),
                    ),
                )
                .await;
            return;
        };

        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        let modal = expense_modal(self.interaction_nonce, session_id);

        let _ = guild_id; // kept for symmetry with later draft creation
        let _ = command
            .create_response(&ctx.http, CreateInteractionResponse::Modal(modal))
            .await;
    }

    async fn start_expense_from_component(&self, ctx: &Context, component: &ComponentInteraction) {
        if component.guild_id.is_none() {
            self.reply_component_error(
                ctx,
                component,
                "サーバー内のチャンネルで実行してください。",
            )
            .await;
            return;
        }

        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        let modal = expense_modal(self.interaction_nonce, session_id);
        let _ = component
            .create_response(&ctx.http, CreateInteractionResponse::Modal(modal))
            .await;
    }

    async fn post_panel(&self, ctx: &Context, command: &CommandInteraction) {
        let Some(_) = command.guild_id else {
            let _ = command
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        CreateInteractionResponseMessage::new()
                            .ephemeral(true)
                            .content("サーバー内のチャンネルで実行してください。"),
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

        match channel_id
            .send_message(
                &ctx.http,
                CreateMessage::new()
                    .content("Walicord 操作パネル")
                    .components(ledger_panel_components()),
            )
            .await
        {
            Ok(_) => {
                self.edit_command_message(ctx, command, "操作パネルを投稿しました。", Vec::new())
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
        let loaded = match self.load_ledger(ctx, channel_id).await {
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
                "まだ経費が記録されていません。先に「記録する」から経費を追加してください。",
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let previewed = match preview_settlement_from_ledger(&loaded.projected) {
            Ok(previewed) => previewed,
            Err(_) => {
                self.edit_component_message(
                    ctx,
                    component,
                    "清算案を作成できませんでした。",
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
        );
        self.settlement_previews.insert(
            (loaded.channel_id.get(), component.user.id.get()),
            PendingSettlementPreview {
                ledger_id: loaded.ledger_id,
                ledger_channel_id: loaded.channel_id,
                binding,
                previewed: previewed.clone(),
            },
        );
        self.edit_component_message(
            ctx,
            component,
            render_review_message(&loaded.projected, &previewed, &member_names),
            Vec::new(),
        )
        .await;
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
        let loaded = match self.load_ledger(ctx, channel_id).await {
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
                "まだ経費が記録されていません。先に「記録する」から経費を追加してください。",
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        self.edit_component_message(
            ctx,
            component,
            render_ledger_summary(&loaded.projected, &member_names),
            Vec::new(),
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
        let loaded = match self.load_ledger(ctx, channel_id).await {
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
                "まだ経費が記録されていません。先に「記録する」から経費を追加してください。",
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let candidates = candidate_entries_for_void(&loaded.entries, &loaded.projected);
        if candidates.is_empty() {
            self.edit_component_message(
                ctx,
                component,
                "取り消せる台帳項目がありません。",
                Vec::new(),
            )
            .await;
            return;
        }
        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        self.void_drafts.insert(
            session_id,
            PendingVoidDraft {
                actor_id: component.user.id,
                guild_id,
                ledger_channel_id: channel_id,
                expires_at: interaction_state_expires_at(),
                selected_target: candidates.first().map(|entry| entry.id),
            },
        );
        self.edit_component_message(
            ctx,
            component,
            self.render_void_message(session_id, &candidates, &member_names),
            void_components(
                session_id,
                &candidates,
                &member_names,
                candidates.first().map(|entry| entry.id),
                self.interaction_nonce,
            ),
        )
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
                        CreateInteractionResponseMessage::new()
                            .ephemeral(true)
                            .content("金額が見つかりませんでした。"),
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
                        CreateInteractionResponseMessage::new()
                            .ephemeral(true)
                            .content("金額は 1 円以上の整数で入力してください。"),
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
                                    CreateInteractionResponseMessage::new()
                                        .ephemeral(true)
                                        .content("日付の形式が正しくありません。例: 今日、昨日、5/1、2026-05-01"),
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
                            CreateInteractionResponseMessage::new()
                                .ephemeral(true)
                                .content(message),
                        ),
                    )
                    .await;
                return;
            }
        };

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
                            CreateInteractionResponseMessage::new()
                                .ephemeral(true)
                                .content("メンバー一覧を取得できませんでした。"),
                        ),
                    )
                    .await;
                return;
            }
        };

        let draft = ExpenseDraft {
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
            weight_overrides: HashMap::new(),
            frozen_participants: None,
            recording: false,
        };
        self.expense_drafts.insert(session_id, draft);

        let member_names =
            roster_provider.display_names_for_guild(guild_id, roster.member_ids.iter().copied());
        let _ = modal
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    CreateInteractionResponseMessage::new()
                        .ephemeral(true)
                        .content(self.render_expense_editor(session_id, &roster, &member_names))
                        .components(expense_editor_components(
                            session_id,
                            self.interaction_nonce,
                        )),
                ),
            )
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
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(
                ctx,
                component,
                "この入力は起動した本人だけが続行できます。",
            )
            .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        if let ComponentInteractionDataKind::UserSelect { values } = &component.data.kind {
            draft.payer = values.first().map(|user_id| MemberId(user_id.get()));
        }
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
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(
                ctx,
                component,
                "この入力は起動した本人だけが続行できます。",
            )
            .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        if let ComponentInteractionDataKind::UserSelect { values } = &component.data.kind {
            draft.explicit_members = values
                .iter()
                .map(|user_id| MemberId(user_id.get()))
                .collect();
            let explicit_members = draft.explicit_members.clone();
            for member_id in explicit_members {
                draft.weight_overrides.entry(member_id).or_insert(Weight(1));
            }
        }
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
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(
                ctx,
                component,
                "この入力は起動した本人だけが続行できます。",
            )
            .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        if let ComponentInteractionDataKind::RoleSelect { values } = &component.data.kind {
            draft.selected_roles = values
                .iter()
                .map(|role_id: &SerenityRoleId| RoleId(role_id.get()))
                .collect();
        }
        drop(draft);
        self.update_expense_editor_message(ctx, component, session_id, roster_provider)
            .await;
    }

    async fn update_expense_groups<RP>(
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
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        if actor_id != component.user.id {
            self.reply_component_error(
                ctx,
                component,
                "この入力は起動した本人だけが続行できます。",
            )
            .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        if let ComponentInteractionDataKind::StringSelect { values } = &component.data.kind {
            draft.include_members_group = values.iter().any(|value| value == GROUP_MEMBERS_VALUE);
        }
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
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(
                ctx,
                component,
                "この入力は起動した本人だけが続行できます。",
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
                self.reply_component_error(ctx, component, "メンバー一覧を取得できませんでした。")
                    .await;
                return;
            }
        };
        let member_names = roster_provider
            .display_names_for_guild(draft.guild_id, roster.member_ids.iter().copied());
        let body = expense_weight_modal_body(&draft, &roster, &member_names);
        let modal = CreateModal::new(
            session_custom_id(
                self.interaction_nonce,
                EXPENSE_WEIGHTS_MODAL_PREFIX,
                session_id,
            ),
            "対象者ごとの重み",
        )
        .components(vec![CreateActionRow::InputText(
            CreateInputText::new(
                InputTextStyle::Paragraph,
                "member_id=weight",
                EXPENSE_WEIGHTS_FIELD,
            )
            .value(body)
            .required(true),
        )]);
        let _ = component
            .create_response(&ctx.http, CreateInteractionResponse::Modal(modal))
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
        let Ok(weight_overrides) = parse_weight_overrides(weights_text) else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        CreateInteractionResponseMessage::new()
                            .ephemeral(true)
                            .content("重みは `member_id=weight` 形式で入力してください。"),
                    ),
                )
                .await;
            return;
        };
        let Some((actor_id, guild_id, channel_id)) = self
            .expense_drafts
            .get(&session_id)
            .map(|draft| (draft.actor_id, draft.guild_id, draft.parent_channel_id))
        else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        CreateInteractionResponseMessage::new()
                            .ephemeral(true)
                            .content("経費入力セッションが見つかりません。"),
                    ),
                )
                .await;
            return;
        };
        if actor_id != modal.user.id {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        CreateInteractionResponseMessage::new()
                            .ephemeral(true)
                            .content("この入力は起動した本人だけが続行できます。"),
                    ),
                )
                .await;
            return;
        }
        let Some(mut draft) = self.expense_drafts.get_mut(&session_id) else {
            let _ = modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        CreateInteractionResponseMessage::new()
                            .ephemeral(true)
                            .content("経費入力セッションが見つかりません。"),
                    ),
                )
                .await;
            return;
        };
        draft.weight_overrides = weight_overrides;
        drop(draft);

        let roster = match roster_provider.roster_for_channel(ctx, channel_id).await {
            Ok(roster) => roster,
            Err(_) => {
                let _ = modal
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::Message(
                            CreateInteractionResponseMessage::new()
                                .ephemeral(true)
                                .content("メンバー一覧を取得できませんでした。"),
                        ),
                    )
                    .await;
                return;
            }
        };
        let member_names =
            roster_provider.display_names_for_guild(guild_id, roster.member_ids.iter().copied());
        let _ = modal
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    CreateInteractionResponseMessage::new()
                        .ephemeral(true)
                        .content(self.render_expense_editor(session_id, &roster, &member_names))
                        .components(expense_editor_components(
                            session_id,
                            self.interaction_nonce,
                        )),
                ),
            )
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
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        let roster = match roster_provider
            .roster_for_channel(ctx, draft.parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                self.reply_component_error(ctx, component, "メンバー一覧を取得できませんでした。")
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
        let content = match self.render_expense_confirmation(session_id, &roster, &member_names) {
            Ok(content) => content,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };

        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    CreateInteractionResponseMessage::new()
                        .content(content)
                        .components(expense_confirmation_components(
                            session_id,
                            self.interaction_nonce,
                        )),
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
        }
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
                self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                    .await;
                return;
            }
            Err(ExpenseRecordingClaimError::Forbidden) => {
                self.reply_component_error(
                    ctx,
                    component,
                    "この入力は起動した本人だけが続行できます。",
                )
                .await;
                return;
            }
            Err(ExpenseRecordingClaimError::AlreadyRecording) => {
                self.reply_component_error(ctx, component, "この経費はすでに記録中です。")
                    .await;
                return;
            }
        };
        let _ = component.defer(&ctx.http).await;
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
                    "メンバー一覧を取得できませんでした。",
                    Vec::new(),
                )
                .await;
                return;
            }
        };
        let member_names = roster_provider
            .display_names_for_guild(draft.guild_id, roster.member_ids.iter().copied());
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
                if let Some(mut stored) = self.expense_drafts.get_mut(&session_id) {
                    stored.frozen_participants = (!current.is_empty()).then_some(current.clone());
                }
                self.release_expense_recording(session_id);
                if current.is_empty() {
                    let content = format!(
                        "対象者が更新されたため入力内容を編集画面に戻しました。対象者を選び直してください。\n\n{}",
                        self.render_expense_editor(session_id, &roster, &member_names)
                    );
                    self.edit_component_message(
                        ctx,
                        component,
                        content,
                        expense_editor_components(session_id, self.interaction_nonce),
                    )
                    .await;
                    return;
                }
                let confirmation =
                    match self.render_expense_confirmation(session_id, &roster, &member_names) {
                        Ok(content) => content,
                        Err(message) => {
                            self.edit_component_message(
                                ctx,
                                component,
                                self.render_expense_editor_notice(
                                    session_id,
                                    &roster,
                                    &member_names,
                                    &message,
                                ),
                                expense_editor_components(session_id, self.interaction_nonce),
                            )
                            .await;
                            return;
                        }
                    };
                self.edit_component_message(
                    ctx,
                    component,
                    format!(
                        "対象者が更新されたため確認内容を更新しました。もう一度「記録する」を押してください。\n\n{confirmation}"
                    ),
                    expense_confirmation_components(session_id, self.interaction_nonce),
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
                    "支払者を選択してください。",
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
                "対象者を 1 人以上選択してください。",
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
        let loaded = match self.load_ledger(ctx, channel_id).await {
            Ok(loaded) => loaded,
            Err(message) => {
                self.release_expense_recording(session_id);
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let entry = match build_expense_entry(next_entry_id(&loaded.verified), &input) {
            Ok(entry) => entry,
            Err(error) => {
                self.release_expense_recording(session_id);
                self.edit_component_message(
                    ctx,
                    component,
                    self.render_expense_editor_notice(
                        session_id,
                        &roster,
                        &member_names,
                        expense_entry_build_error_message(&error),
                    ),
                    expense_editor_components(session_id, self.interaction_nonce),
                )
                .await;
                return;
            }
        };
        if let Err(message) = self
            .append_entry(ctx, &loaded, channel_id, entry, &member_names)
            .await
        {
            self.release_expense_recording(session_id);
            self.edit_component_message(ctx, component, message, Vec::new())
                .await;
            return;
        }
        self.expense_drafts.remove(&session_id);
        self.edit_component_message(ctx, component, "経費を記録しました。", Vec::new())
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
            self.reply_command_error(ctx, command, "このコマンドはサーバー内でのみ使えます。")
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
        let Some(preview) = self
            .settlement_previews
            .get(&(channel_id.get(), command.user.id.get()))
            .map(|preview| preview.clone())
        else {
            self.edit_command_message(
                ctx,
                command,
                "先に /review で清算案を確認してください。",
                Vec::new(),
            )
            .await;
            return;
        };
        let append_lock = self.append_lock(preview.ledger_channel_id);
        let _append_guard = append_lock.lock().await;
        let _cross_process_append_guard = match self
            .acquire_cross_process_append_lock(preview.ledger_channel_id)
            .await
        {
            Ok(guard) => guard,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };

        let loaded = match self.load_ledger(ctx, preview.ledger_channel_id).await {
            Ok(loaded) => loaded,
            Err(message) => {
                self.edit_command_message(ctx, command, message, Vec::new())
                    .await;
                return;
            }
        };

        if loaded.ledger_id != preview.ledger_id
            || current_head_hash(loaded.ledger_id, &loaded.verified)
                != preview.binding.ledger_head_hash
            || SystemTime::now() > preview.binding.expires_at
        {
            self.settlement_previews
                .remove(&(preview.ledger_channel_id.get(), command.user.id.get()));
            self.edit_command_message(
                ctx,
                command,
                "清算案が古くなりました。/review で確認し直してください。",
                Vec::new(),
            )
            .await;
            return;
        }

        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        match build_settlement_entry(
            next_entry_id(&loaded.verified),
            MemberId(command.user.id.get()),
            &preview.previewed,
        ) {
            Ok(Some(entry)) => {
                if let Err(message) = self
                    .append_entry(
                        ctx,
                        &loaded,
                        preview.ledger_channel_id,
                        entry,
                        &member_names,
                    )
                    .await
                {
                    self.edit_command_message(ctx, command, message, Vec::new())
                        .await;
                    return;
                }
                self.settlement_previews
                    .remove(&(preview.ledger_channel_id.get(), command.user.id.get()));
                self.edit_command_message(ctx, command, "清算を記録しました。", Vec::new())
                    .await;
            }
            Ok(None) => {
                self.edit_command_message(
                    ctx,
                    command,
                    "この清算案では、送金を記録する必要がありません。",
                    Vec::new(),
                )
                .await;
            }
            Err(_) => {
                self.edit_command_message(
                    ctx,
                    command,
                    "清算用の送金記録を作成できませんでした。",
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
            self.reply_command_error(ctx, command, "このコマンドはサーバー内でのみ使えます。")
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
        let loaded = match self.load_ledger(ctx, channel_id).await {
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
                "まだ経費が記録されていません。先に /expense を実行してください。",
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        self.edit_command_message(
            ctx,
            command,
            render_ledger_summary(&loaded.projected, &member_names),
            Vec::new(),
        )
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
            self.reply_command_error(ctx, command, "このコマンドはサーバー内でのみ使えます。")
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
        let loaded = match self.load_ledger(ctx, channel_id).await {
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
                "まだ経費が記録されていません。先に /expense を実行してください。",
                Vec::new(),
            )
            .await;
            return;
        }
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let candidates = candidate_entries_for_void(&loaded.entries, &loaded.projected);
        if candidates.is_empty() {
            self.edit_command_message(ctx, command, "取り消せる台帳項目がありません。", Vec::new())
                .await;
            return;
        }
        let session_id = self.next_session_id.fetch_add(1, Ordering::Relaxed);
        self.void_drafts.insert(
            session_id,
            PendingVoidDraft {
                actor_id: command.user.id,
                guild_id,
                ledger_channel_id: channel_id,
                expires_at: interaction_state_expires_at(),
                selected_target: candidates.first().map(|entry| entry.id),
            },
        );
        self.edit_command_message(
            ctx,
            command,
            self.render_void_message(session_id, &candidates, &member_names),
            void_components(
                session_id,
                &candidates,
                &member_names,
                candidates.first().map(|entry| entry.id),
                self.interaction_nonce,
            ),
        )
        .await;
    }

    async fn update_void_selection<RP>(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session_id: u64,
        roster_provider: &RP,
    ) where
        RP: crate::discord::ports::RosterProvider,
    {
        let Some(draft) = self.void_drafts.get(&session_id).map(|draft| draft.clone()) else {
            self.reply_component_error(ctx, component, "取り消し操作の入力状態が見つかりません。")
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(
                ctx,
                component,
                "この入力は起動した本人だけが続行できます。",
            )
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
        let Some(mut draft_guard) = self.void_drafts.get_mut(&session_id) else {
            self.reply_component_error(ctx, component, "取り消し操作の入力状態が見つかりません。")
                .await;
            return;
        };
        draft_guard.selected_target = selected_target;
        drop(draft_guard);
        let guild_id = draft.guild_id;
        let channel_id = draft.ledger_channel_id;
        let loaded = match self.load_ledger(ctx, channel_id).await {
            Ok(loaded) => loaded,
            Err(message) => {
                self.reply_component_error(ctx, component, &message).await;
                return;
            }
        };
        let member_names = roster_provider.display_names_for_guild(
            guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let candidates = candidate_entries_for_void(&loaded.entries, &loaded.projected);
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    CreateInteractionResponseMessage::new()
                        .content(self.render_void_message(session_id, &candidates, &member_names))
                        .components(void_components(
                            session_id,
                            &candidates,
                            &member_names,
                            selected_target,
                            self.interaction_nonce,
                        )),
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
            self.reply_component_error(ctx, component, "取り消し操作の入力状態が見つかりません。")
                .await;
            return;
        };
        if draft.actor_id != component.user.id {
            self.reply_component_error(
                ctx,
                component,
                "この入力は起動した本人だけが続行できます。",
            )
            .await;
            return;
        }
        if draft.selected_target != Some(confirmed_target) {
            self.reply_component_error(
                ctx,
                component,
                "取り消し対象が更新されました。最新の表示でもう一度「取り消す」を押してください。",
            )
            .await;
            return;
        }
        let Some(target) = draft.selected_target else {
            self.reply_component_error(ctx, component, "取り消し対象を選択してください。")
                .await;
            return;
        };
        let _ = component.defer(&ctx.http).await;
        let channel_id = draft.ledger_channel_id;
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
        let loaded = match self.load_ledger(ctx, channel_id).await {
            Ok(loaded) => loaded,
            Err(message) => {
                self.edit_component_message(ctx, component, message, Vec::new())
                    .await;
                return;
            }
        };
        let member_names = roster_provider.display_names_for_guild(
            draft.guild_id,
            loaded.projected.state().participants().iter().copied(),
        );
        let entry = build_void_entry(
            next_entry_id(&loaded.verified),
            MemberId(component.user.id.get()),
            target,
        );
        if let Err(message) = self
            .append_entry(ctx, &loaded, channel_id, entry, &member_names)
            .await
        {
            self.edit_component_message(ctx, component, message, Vec::new())
                .await;
            return;
        }
        self.void_drafts.remove(&session_id);
        self.edit_component_message(
            ctx,
            component,
            "対象の台帳項目を追記型で取り消しました。",
            Vec::new(),
        )
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
            self.reply_component_error(ctx, component, "経費入力セッションが見つかりません。")
                .await;
            return;
        };
        let roster = match roster_provider
            .roster_for_channel(ctx, draft.parent_channel_id)
            .await
        {
            Ok(roster) => roster,
            Err(_) => {
                self.reply_component_error(ctx, component, "メンバー一覧を取得できませんでした。")
                    .await;
                return;
            }
        };
        let member_names = roster_provider
            .display_names_for_guild(draft.guild_id, roster.member_ids.iter().copied());
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    CreateInteractionResponseMessage::new()
                        .content(self.render_expense_editor(session_id, &roster, &member_names))
                        .components(expense_editor_components(
                            session_id,
                            self.interaction_nonce,
                        )),
                ),
            )
            .await;
    }

    fn render_expense_editor(
        &self,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
    ) -> String {
        let Some(draft) = self.expense_drafts.get(&session_id) else {
            return "経費入力セッションが見つかりません。".into();
        };
        let selections = resolve_expense_selections(&draft, roster);
        let mut out = format!(
            "🧾 経費入力中\n\n金額: {}円\n支払者: {}\n",
            draft.amount,
            draft
                .payer
                .map(|member_id| display_name(member_names, member_id))
                .unwrap_or("未選択")
        );
        if let Some(date) = draft.effective_date.as_ref() {
            out.push_str(&format!("日付: {}\n", date.as_str()));
        }
        if let Some(note) = draft.note.as_deref() {
            out.push_str(&format!("メモ: {note}\n"));
        }
        out.push_str("\n現在の対象者\n");
        if selections.is_empty() {
            out.push_str("- まだ選択されていません\n");
        } else {
            for selection in selections {
                out.push_str(&format!(
                    "- {} ({}) x{}\n",
                    display_name(member_names, selection.member_id),
                    selection.member_id.0,
                    selection.weight.0
                ));
            }
        }
        if draft.include_members_group {
            out.push_str("\nグループ: MEMBERS\n");
        }
        if !draft.selected_roles.is_empty() {
            out.push_str("ロール追加済み: ");
            out.push_str(
                &draft
                    .selected_roles
                    .iter()
                    .map(|role_id| role_id.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            );
        }
        out
    }

    fn render_expense_editor_notice(
        &self,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
        notice: &str,
    ) -> String {
        format!(
            "{notice}\n\n{}",
            self.render_expense_editor(session_id, roster, member_names)
        )
    }

    fn render_expense_confirmation(
        &self,
        session_id: u64,
        roster: &crate::discord::ports::RosterSnapshot,
        member_names: &HashMap<MemberId, SmolStr>,
    ) -> Result<String, String> {
        let Some(draft) = self.expense_drafts.get(&session_id) else {
            return Err("経費入力セッションが見つかりません。".into());
        };
        let payer = draft
            .payer
            .ok_or_else(|| "支払者を選択してください。".to_string())?;
        let selections = draft
            .frozen_participants
            .clone()
            .unwrap_or_else(|| resolve_expense_selections(&draft, roster));
        if selections.is_empty() {
            return Err("対象者を 1 人以上選択してください。".into());
        }
        validate_expense_weight_configuration(&selections)
            .map_err(|error| expense_entry_build_error_message(&error).to_owned())?;
        let mut out = format!(
            "✅ 内容確認\n\n金額: {}円\n支払者: {}\n",
            draft.amount,
            display_name(member_names, payer)
        );
        if let Some(date) = draft.effective_date.as_ref() {
            out.push_str(&format!("日付: {}\n", date.as_str()));
        }
        if let Some(note) = draft.note.as_deref() {
            out.push_str(&format!("メモ: {note}\n"));
        }
        out.push_str("\n対象者と重み\n");
        for selection in selections {
            out.push_str(&format!(
                "- {} x{}\n",
                display_name(member_names, selection.member_id),
                selection.weight.0
            ));
        }
        Ok(out)
    }

    fn render_void_message(
        &self,
        session_id: u64,
        candidates: &[LedgerEntry],
        member_names: &HashMap<MemberId, SmolStr>,
    ) -> String {
        let selected = self
            .void_drafts
            .get(&session_id)
            .and_then(|draft| draft.selected_target);
        let mut out = String::from("🗑️ 取り消す項目を選んでください\n");
        if let Some(selected) = selected {
            out.push_str(&format!("\n選択中: #{}\n", selected.0));
        }
        out.push('\n');
        for entry in candidates.iter().take(5) {
            out.push_str("- ");
            out.push_str(&summarize_entry_for_choice(entry, member_names));
            out.push('\n');
        }
        out
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
            SessionIdMatch::Match(session_id) => Ok(Some(session_id)),
            SessionIdMatch::Stale => {
                self.reply_component_error(
                    ctx,
                    component,
                    "この操作は古くなりました。もう一度コマンドを実行してください。",
                )
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
            VoidConfirmMatch::Match { session_id, target } => Ok(Some((session_id, target))),
            VoidConfirmMatch::Stale => {
                self.reply_component_error(
                    ctx,
                    component,
                    "この操作は古くなりました。もう一度コマンドを実行してください。",
                )
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
            SessionIdMatch::Match(session_id) => Ok(Some(session_id)),
            SessionIdMatch::Stale => {
                let _ = modal
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::Message(
                            CreateInteractionResponseMessage::new()
                                .ephemeral(true)
                                .content(
                                    "この操作は古くなりました。もう一度コマンドを実行してください。",
                                ),
                        ),
                    )
                    .await;
                Err(())
            }
            SessionIdMatch::NoMatch => Ok(None),
        }
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

    async fn load_ledger(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> Result<LoadedLedgerThread, String> {
        let messages = fetch_all_channel_messages(ctx, channel_id)
            .await
            .map_err(|error| format!("台帳を読み込めませんでした: {error:?}"))?;
        let ledger_id = LedgerId(channel_id.get());
        let mut envelopes = Vec::new();
        let mut entries = Vec::new();
        let bot_id = ctx.cache.current_user().id;
        for message in messages {
            if message.author.id != bot_id {
                continue;
            }
            let Some(attachment) = message
                .attachments
                .iter()
                .find(|attachment| attachment.filename == LEDGER_ATTACHMENT_FILENAME)
            else {
                continue;
            };
            let bytes = attachment
                .download()
                .await
                .map_err(|error| format!("正規データ添付を取得できませんでした: {error:?}"))?;
            let envelope = decode_discord_canonical_attachment(&bytes, message.id)
                .map_err(|error| format!("正規データ添付を復元できませんでした: {error:?}"))?;
            entries.push(envelope.payload.entry.clone());
            envelopes.push(envelope);
        }
        let (verified, projected) = load_and_replay_verified_sha256_v1(envelopes, ledger_id)
            .map_err(|error| format!("ハッシュチェーンの検証と再生に失敗しました: {error:?}"))?;
        Ok(LoadedLedgerThread {
            ledger_id,
            channel_id,
            verified,
            projected,
            entries,
        })
    }

    async fn append_entry(
        &self,
        ctx: &Context,
        loaded: &LoadedLedgerThread,
        channel_id: ChannelId,
        entry: LedgerEntry,
        member_names: &HashMap<MemberId, SmolStr>,
    ) -> Result<(), String> {
        let envelope = make_unverified_envelope_sha256_v1(
            loaded.ledger_id,
            current_head_hash(loaded.ledger_id, &loaded.verified),
            (),
            entry.clone(),
        )
        .map_err(|error| format!("ハッシュチェーン用のデータを構築できませんでした: {error:?}"))?;
        let attachment_bytes = encode_discord_canonical_attachment(&envelope)
            .map_err(|error| format!("正規データ添付を作成できませんでした: {error:?}"))?;
        let mut entries = loaded.entries.clone();
        entries.push(entry.clone());
        replay_entries(entries)
            .map_err(|error| format!("台帳項目の事前再生に失敗しました: {error:?}"))?;

        channel_id
            .send_message(
                &ctx.http,
                CreateMessage::new()
                    .content(render_entry_message(&entry, member_names))
                    .add_file(CreateAttachment::bytes(
                        attachment_bytes,
                        LEDGER_ATTACHMENT_FILENAME,
                    )),
            )
            .await
            .map_err(|error| format!("台帳項目を送信できませんでした: {error:?}"))?;
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
    ) {
        let _ = command
            .edit_response(
                &ctx.http,
                EditInteractionResponse::new()
                    .content(content)
                    .components(components),
            )
            .await;
    }

    async fn edit_component_message(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        content: impl Into<String>,
        components: Vec<CreateActionRow>,
    ) {
        let _ = component
            .edit_response(
                &ctx.http,
                EditInteractionResponse::new()
                    .content(content)
                    .components(components),
            )
            .await;
    }

    async fn defer_ephemeral_component(&self, ctx: &Context, component: &ComponentInteraction) {
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Defer(
                    CreateInteractionResponseMessage::new().ephemeral(true),
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
        let _ = component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    CreateInteractionResponseMessage::new()
                        .ephemeral(true)
                        .content(message),
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
                    CreateInteractionResponseMessage::new()
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

pub fn is_ledger_panel_component_id(custom_id: &str) -> bool {
    matches!(
        custom_id,
        LEDGER_PANEL_EXPENSE_ID
            | LEDGER_PANEL_REVIEW_ID
            | LEDGER_PANEL_LEDGER_ID
            | LEDGER_PANEL_VOID_ID
    )
}

fn expense_modal(interaction_nonce: u64, session_id: u64) -> CreateModal {
    CreateModal::new(
        session_custom_id(interaction_nonce, EXPENSE_MODAL_PREFIX, session_id),
        "経費を記録する",
    )
    .components(vec![
        CreateActionRow::InputText(
            CreateInputText::new(InputTextStyle::Short, "金額", EXPENSE_AMOUNT_FIELD)
                .placeholder("10000")
                .required(true),
        ),
        CreateActionRow::InputText(
            CreateInputText::new(InputTextStyle::Paragraph, "メモ", EXPENSE_NOTE_FIELD)
                .required(false),
        ),
        CreateActionRow::InputText(
            CreateInputText::new(
                InputTextStyle::Short,
                "日付 (今日・昨日・5/1 も可)",
                EXPENSE_DATE_FIELD,
            )
            .value(today_date().format("%Y-%m-%d").to_string())
            .required(false),
        ),
    ])
}

fn ledger_panel_components() -> Vec<CreateActionRow> {
    vec![CreateActionRow::Buttons(vec![
        CreateButton::new(LEDGER_PANEL_EXPENSE_ID)
            .label("記録する")
            .style(ButtonStyle::Success),
        CreateButton::new(LEDGER_PANEL_REVIEW_ID)
            .label("清算確認")
            .style(ButtonStyle::Primary),
        CreateButton::new(LEDGER_PANEL_LEDGER_ID)
            .label("台帳")
            .style(ButtonStyle::Secondary),
        CreateButton::new(LEDGER_PANEL_VOID_ID)
            .label("取り消し")
            .style(ButtonStyle::Danger),
    ])]
}

fn expense_editor_components(session_id: u64, interaction_nonce: u64) -> Vec<CreateActionRow> {
    vec![
        CreateActionRow::SelectMenu(
            CreateSelectMenu::new(
                session_custom_id(interaction_nonce, EXPENSE_PAYER_PREFIX, session_id),
                CreateSelectMenuKind::User {
                    default_users: None,
                },
            )
            .placeholder("支払者を選ぶ")
            .min_values(1)
            .max_values(1),
        ),
        CreateActionRow::SelectMenu(
            CreateSelectMenu::new(
                session_custom_id(interaction_nonce, EXPENSE_PARTICIPANTS_PREFIX, session_id),
                CreateSelectMenuKind::User {
                    default_users: None,
                },
            )
            .placeholder("対象者を選ぶ")
            .min_values(1)
            .max_values(25),
        ),
        CreateActionRow::SelectMenu(
            CreateSelectMenu::new(
                session_custom_id(interaction_nonce, EXPENSE_ROLES_PREFIX, session_id),
                CreateSelectMenuKind::Role {
                    default_roles: None,
                },
            )
            .placeholder("ロールから追加")
            .min_values(0)
            .max_values(25),
        ),
        CreateActionRow::SelectMenu(
            CreateSelectMenu::new(
                session_custom_id(interaction_nonce, EXPENSE_GROUPS_PREFIX, session_id),
                CreateSelectMenuKind::String {
                    options: vec![CreateSelectMenuOption::new("MEMBERS", GROUP_MEMBERS_VALUE)],
                },
            )
            .placeholder("グループから追加")
            .min_values(0)
            .max_values(1),
        ),
        CreateActionRow::Buttons(vec![
            CreateButton::new(session_custom_id(
                interaction_nonce,
                EXPENSE_OPEN_WEIGHTS_PREFIX,
                session_id,
            ))
            .label("重みを編集")
            .style(ButtonStyle::Primary),
            CreateButton::new(session_custom_id(
                interaction_nonce,
                EXPENSE_CONFIRM_PREFIX,
                session_id,
            ))
            .label("確認する")
            .style(ButtonStyle::Success),
        ]),
    ]
}

fn expense_confirmation_components(
    session_id: u64,
    interaction_nonce: u64,
) -> Vec<CreateActionRow> {
    vec![CreateActionRow::Buttons(vec![
        CreateButton::new(session_custom_id(
            interaction_nonce,
            EXPENSE_EDIT_PREFIX,
            session_id,
        ))
        .label("修正する")
        .style(ButtonStyle::Secondary),
        CreateButton::new(session_custom_id(
            interaction_nonce,
            EXPENSE_RECORD_PREFIX,
            session_id,
        ))
        .label("記録する")
        .style(ButtonStyle::Success),
    ])]
}

fn void_components(
    session_id: u64,
    candidates: &[LedgerEntry],
    member_names: &HashMap<MemberId, SmolStr>,
    selected_target: Option<LedgerEntryId>,
    interaction_nonce: u64,
) -> Vec<CreateActionRow> {
    let options = candidates
        .iter()
        .take(25)
        .map(|entry| {
            CreateSelectMenuOption::new(
                summarize_entry_for_choice(entry, member_names),
                entry.id.0.to_string(),
            )
        })
        .collect();
    vec![
        CreateActionRow::SelectMenu(
            CreateSelectMenu::new(
                session_custom_id(interaction_nonce, VOID_SELECT_PREFIX, session_id),
                CreateSelectMenuKind::String { options },
            )
            .placeholder("取り消し対象を選ぶ")
            .min_values(1)
            .max_values(1),
        ),
        CreateActionRow::Buttons(vec![
            CreateButton::new(
                selected_target
                    .map(|target| void_confirm_custom_id(interaction_nonce, session_id, target))
                    .unwrap_or_else(|| {
                        session_custom_id(interaction_nonce, VOID_CONFIRM_PREFIX, session_id)
                    }),
            )
            .label("取り消す")
            .style(ButtonStyle::Danger)
            .disabled(selected_target.is_none()),
        ]),
    ]
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

fn expense_weight_modal_body(
    draft: &ExpenseDraft,
    roster: &crate::discord::ports::RosterSnapshot,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    resolve_expense_selections(draft, roster)
        .into_iter()
        .map(|selection| {
            format!(
                "{} ({})={}",
                display_name(member_names, selection.member_id),
                selection.member_id.0,
                selection.weight.0
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn parse_weight_overrides(text: &str) -> Result<HashMap<MemberId, Weight>, ()> {
    let mut weights = HashMap::new();
    for line in text.lines().map(str::trim).filter(|line| !line.is_empty()) {
        let Some((left, right)) = line.rsplit_once('=') else {
            return Err(());
        };
        let member_id = left
            .trim()
            .trim_end_matches(')')
            .rsplit_once('(')
            .map(|(_, member_id)| member_id)
            .unwrap_or(left)
            .trim()
            .parse::<u64>()
            .map_err(|_| ())?;
        let weight = right.trim().parse::<u64>().map_err(|_| ())?;
        weights.insert(MemberId(member_id), Weight(weight));
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
        ExpenseEntryBuildError::EmptyParticipants => "対象者を 1 人以上選択してください。",
        ExpenseEntryBuildError::InvalidAmount => "金額は 1 円以上の整数で入力してください。",
        ExpenseEntryBuildError::InvalidWeightConfiguration => {
            "対象者の重みは 1 人以上を 1 以上にしてください。"
        }
        ExpenseEntryBuildError::InvalidNote
        | ExpenseEntryBuildError::InvalidSource
        | ExpenseEntryBuildError::LedgerConstruction => "経費の台帳項目を作成できませんでした。",
    }
}

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

pub fn preview_settlement_from_ledger(
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
) -> PreviewConfirmationBinding {
    let expires_at = SystemTime::now() + std::time::Duration::from_secs(PREVIEW_EXPIRY_SECS);
    PreviewConfirmationBinding::capture(ledger_id, head_hash, actor_id, expires_at, previewed)
}

pub fn encode_discord_canonical_attachment(
    envelope: &UnverifiedLedgerStoreEnvelope<()>,
) -> Result<Vec<u8>, LedgerAttachmentError> {
    let dto = DiscordLedgerAttachmentDto {
        version: ATTACHMENT_SCHEMA_VERSION,
        transport: TransportMetadataDto {
            recorded_at_unix_ms: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_err(|_| LedgerAttachmentError::Clock)?
                .as_millis()
                .try_into()
                .map_err(|_| LedgerAttachmentError::Clock)?,
        },
        envelope: EnvelopeDto::from_unverified(envelope),
    };

    serde_json::to_vec_pretty(&dto).map_err(LedgerAttachmentError::JsonEncode)
}

pub fn decode_discord_canonical_attachment<ExternalId>(
    bytes: &[u8],
    external_id: ExternalId,
) -> Result<UnverifiedLedgerStoreEnvelope<ExternalId>, LedgerAttachmentError> {
    let dto: DiscordLedgerAttachmentDto =
        serde_json::from_slice(bytes).map_err(LedgerAttachmentError::JsonDecode)?;
    if dto.version != ATTACHMENT_SCHEMA_VERSION {
        return Err(LedgerAttachmentError::UnsupportedVersion(dto.version));
    }

    dto.envelope.into_unverified(external_id)
}

pub fn render_entry_message(
    entry: &LedgerEntry,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => render_expense_message(entry, event, member_names),
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
            render_settlement_message(entry, event, member_names)
        }
        LedgerEvent::EntryVoided(event) => render_void_message(entry, event, member_names),
        LedgerEvent::LedgerHistorySealed(event) => render_seal_message(entry, event),
        LedgerEvent::BalanceAdjusted(event) => {
            render_adjustment_message(entry, event, member_names)
        }
    }
}

pub fn render_review_message(
    projected: &ProjectedLedger,
    previewed: &PreviewedSettlement,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    let mut out = String::from("📊 現在の清算案\n\n残高\n");
    out.push_str(&render_balances(projected.state(), member_names));
    out.push_str("\n\n清算用の送金\n");
    if previewed.plan().transfers.is_empty() {
        out.push_str("清算対象はありません。");
    } else {
        for transfer in &previewed.plan().transfers {
            out.push_str(&format!(
                "- {} -> {}: {}円\n",
                display_name(member_names, transfer.from),
                display_name(member_names, transfer.to),
                transfer.amount
            ));
        }
    }
    out
}

pub fn render_ledger_summary(
    projected: &ProjectedLedger,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    let mut out = String::from("📚 Ledger 状態\n\n参加者\n");
    for member_id in projected.state().participants() {
        out.push_str(&format!("- {}\n", display_name(member_names, *member_id)));
    }

    out.push_str("\n残高\n");
    out.push_str(&render_balances(projected.state(), member_names));

    out.push_str("\n\n取り消し済みの台帳項目\n");
    if projected.state().voided_entry_ids().is_empty() {
        out.push_str("なし");
    } else {
        for entry_id in projected.state().voided_entry_ids() {
            out.push_str(&format!("- #{}\n", entry_id.0));
        }
    }

    out.push_str("\n\nSealed through\n");
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
    let mut candidates: Vec<LedgerEntry> = entries
        .iter()
        .filter(|entry| {
            let Some(info) = projected.entry(entry.id) else {
                return false;
            };
            matches!(
                info.kind,
                ProjectedEntryKind::Expense | ProjectedEntryKind::SettlementTransfer
            ) && !info.voided
                && !info.sealed
        })
        .cloned()
        .collect();
    candidates.reverse();
    candidates
}

pub fn summarize_entry_for_choice(
    entry: &LedgerEntry,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => {
            let payer = event
                .paid_by()
                .first()
                .map(|paid| display_name(member_names, paid.member_id))
                .unwrap_or("unknown");
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
                        display_name(member_names, transfer.from),
                        display_name(member_names, transfer.to),
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

fn render_expense_message(
    entry: &LedgerEntry,
    event: &ExpenseRecorded,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    let payer = event
        .paid_by()
        .first()
        .map(|paid| display_name(member_names, paid.member_id))
        .unwrap_or("unknown");
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
                    display_name(member_names, owed.member_id)
                ));
            }
        }
        Some(AllocationSnapshot::Weighted { resolved_weights }) => {
            for MemberWeight { member_id, weight } in resolved_weights {
                out.push_str(&format!(
                    "- {} x{}\n",
                    display_name(member_names, *member_id),
                    weight.0
                ));
            }
        }
        Some(AllocationSnapshot::LegacyUnknown) | None => {
            for owed in event.owed_by() {
                out.push_str(&format!(
                    "- {}\n",
                    display_name(member_names, owed.member_id)
                ));
            }
        }
    }

    if let Some(note) = event.note() {
        out.push_str(&format!("\nメモ: {}", note.as_str()));
    }

    out
}

fn render_settlement_message(
    entry: &LedgerEntry,
    event: &NormalizedSettlementPlanRecorded,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    let mut out = format!("💸 清算を記録しました\n\n台帳項目: #{}\n", entry.id.0);
    for transfer in event.transfers() {
        out.push_str(&format!(
            "- {} -> {}: {}円\n",
            display_name(member_names, transfer.from),
            display_name(member_names, transfer.to),
            transfer.amount
        ));
    }
    out
}

fn render_void_message(
    entry: &LedgerEntry,
    event: &EntryVoided,
    _member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    format!(
        "🪫 台帳項目を取り消しました\n\n台帳項目: #{}\n対象: #{}",
        entry.id.0,
        event.target().0
    )
}

fn render_seal_message(entry: &LedgerEntry, event: &LedgerHistorySealed) -> String {
    format!(
        "🔒 履歴を封印しました\n\n台帳項目: #{}\n封印済み範囲: #{} まで",
        entry.id.0,
        event.through().0
    )
}

fn render_adjustment_message(
    entry: &LedgerEntry,
    event: &BalanceAdjusted,
    member_names: &HashMap<MemberId, SmolStr>,
) -> String {
    let mut out = format!(
        "🩹 残高補正を記録しました\n\n台帳項目: #{}\n理由: {}\n",
        entry.id.0,
        event.reason().as_str()
    );
    for adjustment in event.adjustments() {
        out.push_str(&format!(
            "- {}: {}円\n",
            display_name(member_names, adjustment.member_id),
            adjustment.amount
        ));
    }
    out
}

fn render_balances(state: &LedgerState, member_names: &HashMap<MemberId, SmolStr>) -> String {
    if state.balances().is_empty() {
        return "全員の残高は 0 です。".into();
    }

    state
        .balances()
        .iter()
        .map(|(member_id, balance)| {
            format!("- {}: {balance}円", display_name(member_names, *member_id))
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn display_name(member_names: &HashMap<MemberId, SmolStr>, member_id: MemberId) -> &str {
    member_names
        .get(&member_id)
        .map(SmolStr::as_str)
        .unwrap_or("unknown")
}

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
struct DiscordLedgerAttachmentDto {
    version: u32,
    transport: TransportMetadataDto,
    envelope: EnvelopeDto,
}

#[derive(Debug, Serialize, Deserialize)]
struct TransportMetadataDto {
    recorded_at_unix_ms: u64,
}

#[derive(Debug, Serialize, Deserialize)]
struct EnvelopeDto {
    previous_hash: String,
    entry_hash: String,
    payload: PayloadDto,
}

impl EnvelopeDto {
    fn from_unverified(envelope: &UnverifiedLedgerStoreEnvelope<()>) -> Self {
        Self {
            previous_hash: encode_hash(envelope.previous_hash),
            entry_hash: encode_hash(envelope.entry_hash),
            payload: PayloadDto::from_payload(&envelope.payload),
        }
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

impl PayloadDto {
    fn from_payload(payload: &walicord_application::ledger::HashedLedgerPayload) -> Self {
        Self {
            ledger_id: payload.ledger_id.0,
            schema_version: payload.schema_version.0,
            hash_suite: hash_suite_name(payload.hash_suite).into(),
            entry: EntryDto::from_entry(&payload.entry),
        }
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

impl EntryDto {
    fn from_entry(entry: &LedgerEntry) -> Self {
        Self {
            id: entry.id.0,
            metadata: MetadataDto::from_metadata(&entry.metadata),
            event: EventDto::from_event(&entry.event),
        }
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
    allocation_snapshot: Option<AllocationSnapshotDto>,
}

impl MetadataDto {
    fn from_metadata(metadata: &LedgerEntryMetadata) -> Self {
        Self {
            recorded_by: metadata.recorded_by.map(|member_id| member_id.0),
            source: metadata.source.as_ref().map(SourceDto::from_source),
            effective_date: metadata
                .effective_date
                .as_ref()
                .map(|effective_date| effective_date.as_str().to_owned()),
            allocation_snapshot: metadata
                .allocation_snapshot
                .as_ref()
                .map(AllocationSnapshotDto::from_snapshot),
        }
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

fn encode_money(money: Money) -> String {
    money.as_decimal().normalize().to_string()
}

fn parse_money(text: &str) -> Result<Money, LedgerAttachmentError> {
    let decimal = Decimal::from_str(text).map_err(|_| LedgerAttachmentError::InvalidMoney)?;
    Ok(Money::from_decimal(decimal))
}

fn hash_suite_name(hash_suite: LedgerHashSuite) -> &'static str {
    match hash_suite {
        LedgerHashSuite::Sha256V1 => "sha256_v1",
    }
}

fn parse_hash_suite(name: &str) -> Result<LedgerHashSuite, LedgerAttachmentError> {
    match name {
        "sha256_v1" => Ok(LedgerHashSuite::Sha256V1),
        other => Err(LedgerAttachmentError::InvalidHashSuite(other.to_owned())),
    }
}

fn source_kind_name(kind: LedgerSourceCanonicalKind) -> &'static str {
    match kind {
        LedgerSourceCanonicalKind::LegacyDsl => "legacy_dsl",
        LedgerSourceCanonicalKind::DiscordUi => "discord_ui",
    }
}

fn today_date() -> NaiveDate {
    Local::now().date_naive()
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
    use walicord_application::ledger::replay_entries;

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
            encode_discord_canonical_attachment(&envelope).expect("attachment should encode");
        let decoded = decode_discord_canonical_attachment(&encoded, 123_u64)
            .expect("attachment should decode");

        assert_eq!(decoded.previous_hash, envelope.previous_hash);
        assert_eq!(decoded.entry_hash, envelope.entry_hash);
        assert_eq!(decoded.payload.ledger_id, envelope.payload.ledger_id);
        assert_eq!(decoded.payload.entry, entry);
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

        assert!(body.contains("花子 (2)=2"));
        assert!(body.contains("次郎 (3)=1"));
        assert!(body.contains("三郎 (4)=0"));
        assert_eq!(
            parse_weight_overrides(&body).expect("weight body should parse back"),
            HashMap::from([
                (MemberId(2), Weight(2)),
                (MemberId(3), Weight(1)),
                (MemberId(4), Weight::ZERO),
            ])
        );
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
        assert!(confirmation.contains("- 花子 x2"));
        assert!(confirmation.contains("- 次郎 x1"));
        assert!(confirmation.contains("- 三郎 x0"));
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
                expires_at: UNIX_EPOCH,
                selected_target: Some(LedgerEntryId(1)),
            },
        );
        poc.settlement_previews.insert(
            stale_preview_key,
            PendingSettlementPreview {
                ledger_id,
                ledger_channel_id: thread_id,
                binding: PreviewConfirmationBinding {
                    ledger_id,
                    ledger_head_hash: ledger_chain_genesis_sha256_v1(ledger_id),
                    actor_id: MemberId(9),
                    preview_digest: previewed.digest(),
                    expires_at: UNIX_EPOCH,
                },
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
            encode_discord_canonical_attachment(&envelope).expect("attachment should encode");
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
