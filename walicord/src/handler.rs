use crate::{
    channel::{ChannelEvent, ChannelManager, TrackedChannelId},
    discord::ports::{ChannelService, RosterProvider, ServiceError},
    message_cache::{CachedMessage, MessageCache, next_line_offset},
    reaction::{BotReaction, BotReactionState, MessageValidity, ReactionService},
    role_visibility_feedback,
    settlement::{SettlementService, evaluate_program},
};
use arcstr::ArcStr;
use indexmap::IndexMap;
use serenity::{
    all::MessageId,
    async_trait,
    model::{
        channel::{GuildChannel, Message, Reaction, ReactionType},
        event::{GuildMemberUpdateEvent, MessageUpdateEvent},
        gateway::Ready,
        guild::Member,
        id::{ChannelId, GuildId},
        user::User,
    },
    prelude::*,
};
use std::collections::HashMap;
use walicord_application::{
    Command as ProgramCommand, MessageProcessor, ProgramParseError, RoleVisibilityDiagnostics,
    Script, ScriptStatement, filtered_empty_role_parse_error, warnings_for_program_prefix,
};
use walicord_domain::model::{MemberId, RoleId, RoleMembers};
use walicord_presentation::{VariablesPresenter, format_program_parse_error};

/// Result of attempting to load channel cache.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CacheLoadResult {
    /// Cache loaded successfully with at least one message.
    LoadedNonEmpty,
    /// Cache loaded successfully but channel is empty.
    LoadedEmpty,
    /// Channel is not tracked, no cache operation performed.
    NotTracked,
    /// Fetch failed, cache not loaded.
    Failed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ProcessProgramMessageResult {
    Store,
    Skip,
    Deferred,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ProgramCommandEffect {
    ReplyText(String),
    RunSettlement { stmt_index: usize },
}

fn format_program_parse_error_reply(
    error: ProgramParseError<'_>,
    diagnostics: &RoleVisibilityDiagnostics,
    mention: impl std::fmt::Display,
) -> String {
    if let Some(filtered_empty_role_error) = filtered_empty_role_parse_error(&error, diagnostics) {
        role_visibility_feedback::format_filtered_empty_role_parse_error(
            filtered_empty_role_error,
            mention,
        )
    } else {
        format_program_parse_error(error, mention)
    }
}

fn command_warning_lines_for_prefix(
    program: &Script<'_>,
    stmt_index: usize,
    diagnostics: &RoleVisibilityDiagnostics,
) -> Vec<String> {
    let warnings = warnings_for_program_prefix(program, stmt_index, diagnostics);
    role_visibility_feedback::format_warning_lines(&warnings)
}

fn plan_program_command_effects(
    program: &Script<'_>,
    member_ids: &[MemberId],
    diagnostics: &RoleVisibilityDiagnostics,
    next_line_offset: usize,
) -> Vec<ProgramCommandEffect> {
    let mut effects = Vec::new();

    for (stmt_index, stmt) in program.statements().iter().enumerate() {
        if stmt.line <= next_line_offset {
            continue;
        }
        let ScriptStatement::Command(command) = &stmt.statement else {
            continue;
        };

        let warning_lines = command_warning_lines_for_prefix(program, stmt_index, diagnostics);
        match command {
            ProgramCommand::Variables => {
                let mut reply = VariablesPresenter::render_for_prefix_with_members(
                    program, stmt_index, member_ids,
                );
                if !warning_lines.is_empty() {
                    reply.push_str("\n\n");
                    reply.push_str(&warning_lines.join("\n"));
                }
                effects.push(ProgramCommandEffect::ReplyText(reply));
            }
            ProgramCommand::Review | ProgramCommand::SettleUp { .. } => {
                if !warning_lines.is_empty() {
                    effects.push(ProgramCommandEffect::ReplyText(warning_lines.join("\n")));
                }
                effects.push(ProgramCommandEffect::RunSettlement { stmt_index });
            }
            ProgramCommand::MemberAddCash { .. } => {}
        }
    }

    effects
}

/// Discord bot event handler with generic service dependencies
pub struct BotHandler<'a, CS, RP>
where
    CS: ChannelService,
    RP: RosterProvider,
{
    message_cache: MessageCache,
    channel_service: CS,
    roster_provider: RP,
    processor: MessageProcessor<'a>,
    channel_manager: ChannelManager,
}

impl<'a, CS, RP> BotHandler<'a, CS, RP>
where
    CS: ChannelService,
    RP: RosterProvider,
{
    pub fn new(
        message_cache: MessageCache,
        channel_service: CS,
        roster_provider: RP,
        processor: MessageProcessor<'a>,
        channel_manager: ChannelManager,
    ) -> Self {
        Self {
            message_cache,
            channel_service,
            roster_provider,
            processor,
            channel_manager,
        }
    }

    async fn track_channel(&self, ctx: &Context, channel_id: ChannelId) {
        self.track_channel_with_io(
            channel_id,
            async || {
                let messages = self
                    .channel_service
                    .fetch_all_messages(ctx, channel_id)
                    .await?;
                let cached = messages
                    .into_iter()
                    .map(|(id, msg)| (id, CachedMessage::from_message(msg)))
                    .collect();
                Ok(cached)
            },
            || self.roster_provider.warm_up(ctx, channel_id),
        )
        .await;
    }

    async fn track_channel_with_io<FFetch, FutFetch, FWarm, FutWarm>(
        &self,
        channel_id: ChannelId,
        fetch_all_messages: FFetch,
        warm_up_roster: FWarm,
    ) where
        FFetch: FnOnce() -> FutFetch,
        FutFetch: Future<Output = Result<IndexMap<MessageId, CachedMessage>, ServiceError>>,
        FWarm: FnOnce() -> FutWarm,
        FutWarm: Future<Output = Result<(), ServiceError>>,
    {
        let Some(ChannelEvent::Tracked(tracked_id)) = self.channel_manager.track(channel_id) else {
            return;
        };

        let channel_id = tracked_id.get();
        tracing::info!("Tracking channel {}", channel_id);
        match fetch_all_messages().await {
            Ok(cached) => {
                if self.channel_manager.mark_fetch_succeeded(channel_id) {
                    self.message_cache.insert(tracked_id, cached);
                }
            }
            Err(e) => {
                if self.channel_manager.mark_fetch_failed(channel_id) {
                    tracing::warn!(
                        "Failed to fetch initial messages for {}: {:?}",
                        channel_id,
                        e
                    );
                }
            }
        }

        if let Err(e) = warm_up_roster().await {
            tracing::warn!(
                "Failed to build member cache for channel {}: {:?}",
                channel_id,
                e
            );
        }
    }

    fn untrack_channel(&self, channel_id: ChannelId) {
        if let Some(ChannelEvent::Untracked(untracked_id)) =
            self.channel_manager.untrack(channel_id)
        {
            self.message_cache.remove(untracked_id);
            tracing::info!("Untracked channel {untracked_id}");
        }
    }

    async fn initialize_enabled_channels(&self, ctx: &Context, ready: &Ready) {
        use serenity::model::channel::ChannelType;

        tracing::info!("Scanning guild channels for #walicord flag");
        for guild in &ready.guilds {
            let guild_id = guild.id;
            match guild_id.channels(&ctx.http).await {
                Ok(channels) => {
                    for channel in channels.values() {
                        // Only scan text-based channels (not voice, category, etc.)
                        if channel.kind != ChannelType::Text {
                            continue;
                        }
                        if ChannelManager::topic_has_flag(channel.topic.as_deref()) {
                            self.track_channel(ctx, channel.id).await;
                        }
                    }
                }
                Err(e) => {
                    tracing::warn!("Failed to fetch channels for guild {}: {:?}", guild_id, e);
                }
            }
        }
    }

    fn has_pending_messages(&self, tracked_id: TrackedChannelId) -> bool {
        self.message_cache
            .with_messages(tracked_id, |messages| {
                messages.values().any(CachedMessage::is_pending_evaluation)
            })
            .unwrap_or(false)
    }

    fn mark_pending(mut cached: CachedMessage) -> CachedMessage {
        cached.mark_pending_evaluation();
        cached
    }

    fn defer_current_message_if_pending_remains(
        &self,
        tracked_id: TrackedChannelId,
        cached: CachedMessage,
    ) -> bool {
        if !self.has_pending_messages(tracked_id) {
            return false;
        }

        self.message_cache
            .upsert_message(tracked_id, Self::mark_pending(cached));
        true
    }

    pub(crate) async fn ensure_cache_loaded(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> CacheLoadResult {
        self.ensure_cache_loaded_with_fetch(channel_id, async || {
            let messages = self
                .channel_service
                .fetch_all_messages(ctx, channel_id)
                .await?;
            let cached_messages = messages
                .into_iter()
                .map(|(id, msg)| (id, CachedMessage::from_message(msg)))
                .collect();
            Ok(cached_messages)
        })
        .await
    }

    async fn ensure_cache_loaded_with_fetch<F, Fut>(
        &self,
        channel_id: ChannelId,
        fetch_all_messages: F,
    ) -> CacheLoadResult
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = Result<IndexMap<MessageId, CachedMessage>, ServiceError>>,
    {
        let Some(tracked_id) = self.channel_manager.get_tracked(channel_id) else {
            return CacheLoadResult::NotTracked;
        };

        if let Some(is_empty) = self
            .message_cache
            .with_messages(tracked_id, |msgs| msgs.is_empty())
        {
            return if is_empty {
                CacheLoadResult::LoadedEmpty
            } else {
                CacheLoadResult::LoadedNonEmpty
            };
        }

        match fetch_all_messages().await {
            Ok(cached_messages) => {
                let Some(tracked_id) = self.channel_manager.get_tracked(channel_id) else {
                    return CacheLoadResult::NotTracked;
                };
                let _ = self.channel_manager.mark_fetch_succeeded(channel_id);
                let is_empty = cached_messages.is_empty();
                self.message_cache.insert(tracked_id, cached_messages);
                if is_empty {
                    CacheLoadResult::LoadedEmpty
                } else {
                    CacheLoadResult::LoadedNonEmpty
                }
            }
            Err(e) => {
                if self.channel_manager.mark_fetch_failed(channel_id) {
                    tracing::warn!("Fetch failed for {}: {:?}", channel_id, e);
                }
                CacheLoadResult::Failed
            }
        }
    }

    /// Rebuild cache and update reactions for a channel.
    ///
    /// Requires at least one message to evaluate (either from cache or updated_cached).
    /// - LoadedEmpty continues execution (cache exists but empty, updated_cached may exist)
    /// - If no messages after merging cache + updated_cached, returns early without evaluation
    /// - Cache entry is preserved even when empty (distinguishes "loaded empty" from "not loaded")
    ///
    /// # Performance Note
    /// Currently fetches all channel messages on cache miss, even when updated_cached is present.
    /// Future optimization: skip full fetch when updated_cached exists and accept empty context
    /// (trades accuracy of line offsets for reduced API load).
    async fn rebuild_channel_cache(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
        updated_cached: Option<CachedMessage>,
    ) {
        let updated_message_id = updated_cached.as_ref().map(|m| m.id);
        let load_result = self.ensure_cache_loaded(ctx, channel_id).await;

        if should_abort_rebuild_after_cache_load(load_result) {
            return;
        }

        let Some(tracked_id) = self.channel_manager.get_tracked(channel_id) else {
            return;
        };

        let cached: Option<Vec<CachedMessage>> =
            self.message_cache.with_messages(tracked_id, |msgs| {
                msgs.iter().map(|(_, m)| m.clone()).collect()
            });

        let mut messages = cached.unwrap_or_default();

        if let Some(updated_msg) = updated_cached {
            if let Some(pos) = messages.iter().position(|m| m.id == updated_msg.id) {
                messages[pos] = updated_msg;
            } else {
                messages.push(updated_msg);
            }
        }

        if messages.is_empty() {
            if let Some(tracked_id) = self.channel_manager.get_tracked(channel_id) {
                self.message_cache.insert(tracked_id, IndexMap::new());
            }
            return;
        }

        let roster = match self
            .roster_provider
            .roster_for_channel(ctx, channel_id)
            .await
        {
            Ok(snapshot) => snapshot,
            Err(e) => {
                tracing::warn!("Failed to fetch roster for {}: {:?}", channel_id, e);
                if let Some(updated_id) = updated_message_id
                    && let Some(updated) = messages.iter_mut().find(|m| m.id == updated_id)
                {
                    updated.mark_pending_evaluation();
                }
                let fallback_cache: IndexMap<MessageId, CachedMessage> =
                    messages.into_iter().map(|m| (m.id, m)).collect();
                if let Some(tracked_id) = self.channel_manager.get_tracked(channel_id) {
                    self.message_cache.insert(tracked_id, fallback_cache);
                }
                return;
            }
        };

        let plan = plan_cache_rebuild(
            &self.processor,
            &roster.member_ids,
            &roster.role_members,
            &messages,
        );

        if let Some(tracked_id) = self.channel_manager.get_tracked(channel_id) {
            self.message_cache.insert(tracked_id, plan.cache);
        }

        for (message_id, current, desired) in plan.reactions {
            ReactionService::update_state_diff(ctx, channel_id, message_id, current, desired).await;
        }
    }

    /// Process a single message for program evaluation.
    ///
    /// Evaluates the message in context of cached prior messages. If cache is absent,
    /// evaluates with empty context (valid for first message in channel or after cache miss).
    /// The message is then stored in cache if it contains effect statements.
    async fn process_program_message(
        &self,
        ctx: &Context,
        msg: &Message,
        tracked_id: TrackedChannelId,
    ) -> ProcessProgramMessageResult {
        let roster = match self
            .roster_provider
            .roster_for_channel(ctx, msg.channel_id)
            .await
        {
            Ok(snapshot) => snapshot,
            Err(e) => {
                tracing::warn!("Failed to fetch roster for {}: {:?}", msg.channel_id, e);
                return ProcessProgramMessageResult::Deferred;
            }
        };

        let author_id = MemberId(msg.author.id.get());
        let (cached_contents, next_line_offset) = self
            .message_cache
            .with_messages(tracked_id, |messages| {
                let offset = next_line_offset(messages.iter().map(|(_, m)| m));
                let contents: Vec<(ArcStr, Option<MemberId>)> = messages
                    .iter()
                    .filter(|(_, m)| {
                        !m.is_bot && !m.is_marked_invalid() && !m.is_pending_evaluation()
                    })
                    .map(|(_, m)| (m.content.clone(), Some(MemberId(m.author_id.get()))))
                    .collect();
                (contents, offset)
            })
            .unwrap_or_default();

        let result = match evaluate_program(
            &self.processor,
            &roster.member_ids,
            &roster.role_members,
            &cached_contents,
            msg.content.as_str(),
            author_id,
            next_line_offset,
        ) {
            Ok(result) => result,
            Err(failure) => {
                ReactionService::update_state(ctx, msg, MessageValidity::Invalid).await;
                let content = format_program_parse_error_reply(
                    failure,
                    &roster.role_visibility_diagnostics,
                    msg.author.mention(),
                );
                let _ = msg.reply(&ctx.http, content).await;
                return ProcessProgramMessageResult::Skip;
            }
        };

        if result.has_effect_statement {
            ReactionService::react(ctx, msg, '✅').await;
        }

        if let Some(program) = result.program.as_ref() {
            let settlement_service = SettlementService::new(&self.processor, &self.roster_provider);
            let mut member_directory: Option<HashMap<MemberId, smol_str::SmolStr>> = None;
            let effects = plan_program_command_effects(
                program,
                &roster.member_ids,
                &roster.role_visibility_diagnostics,
                next_line_offset,
            );

            for effect in effects {
                match effect {
                    ProgramCommandEffect::ReplyText(content) => {
                        let _ = msg.reply(&ctx.http, content).await;
                    }
                    ProgramCommandEffect::RunSettlement { stmt_index } => {
                        settlement_service
                            .handle_settlement_command(
                                ctx,
                                msg,
                                stmt_index,
                                program,
                                &roster.member_ids,
                                &mut member_directory,
                            )
                            .await;
                    }
                }
            }
        }

        if result.should_store {
            ProcessProgramMessageResult::Store
        } else {
            ProcessProgramMessageResult::Skip
        }
    }
}

/// Plan for rebuilding channel cache
struct CacheRebuildPlan {
    cache: IndexMap<MessageId, CachedMessage>,
    /// Reaction updates for ALL processed messages:
    /// - Invalid: add ❎ (parse error feedback)
    /// - Valid: add ✅
    /// - NotProgram: clear reactions
    ///
    /// Non-cached messages may receive redundant API calls on each rebuild,
    /// acceptable trade-off since they don't pollute the cache.
    reactions: Vec<(MessageId, BotReactionState, MessageValidity)>,
}

fn plan_cache_rebuild(
    processor: &MessageProcessor,
    member_ids: &[MemberId],
    role_members: &RoleMembers,
    messages: &[CachedMessage],
) -> CacheRebuildPlan {
    let mut sorted_indices: Vec<usize> = (0..messages.len()).collect();
    sorted_indices.sort_by_key(|&i| messages[i].id);

    let mut cache: IndexMap<MessageId, CachedMessage> = IndexMap::new();
    let mut reactions = Vec::new();
    let mut inputs: Vec<(&str, Option<MemberId>)> = Vec::new();
    let mut line_count = 0usize;
    let mut ends_with_newline = false;
    let mut has_any = false;

    for &idx in &sorted_indices {
        let message = &messages[idx];
        if message.is_bot {
            continue;
        }

        let author_id = MemberId(message.author_id.get());
        let line_offset = if has_any {
            line_count + if ends_with_newline { 1 } else { 0 }
        } else {
            0
        };

        let (desired_validity, should_store) = {
            let outcome = processor.parse_program_sequence(
                member_ids,
                role_members,
                inputs
                    .iter()
                    .copied()
                    .chain(std::iter::once((message.content.as_ref(), Some(author_id)))),
            );

            match outcome {
                walicord_application::ProcessingOutcome::Success(program) => {
                    let mut has_any_statement = false;
                    let mut has_effect_statement = false;

                    for stmt in program
                        .statements()
                        .iter()
                        .filter(|stmt| stmt.line > line_offset)
                    {
                        has_any_statement = true;
                        match &stmt.statement {
                            ScriptStatement::Domain(_) => {
                                has_effect_statement = true;
                            }
                            ScriptStatement::Command(command) => {
                                if let ProgramCommand::SettleUp { .. } = command {
                                    has_effect_statement = true;
                                }
                            }
                        }
                    }

                    let should_store = has_any_statement;
                    let desired = if has_any_statement && has_effect_statement {
                        MessageValidity::Valid
                    } else {
                        MessageValidity::NotProgram
                    };
                    (desired, should_store)
                }
                _ => (MessageValidity::Invalid, false),
            }
        };

        if should_store {
            let mut cached = message.clone();
            cached.reaction_state = BotReaction::state_from_validity(desired_validity);
            cached.clear_pending_evaluation();
            cache.insert(message.id, cached);
            if has_any {
                line_count += line_count_with_prior_newline(&message.content, ends_with_newline);
            } else {
                line_count = message.content.lines().count();
            }
            ends_with_newline = message.content.is_empty() || message.content.ends_with('\n');
            has_any = true;
            inputs.push((message.content.as_ref(), Some(author_id)));
        }
        // Always update reactions for all processed messages:
        // - Valid: add ✅
        // - Invalid: add ❎ (parse error feedback to user)
        // - NotProgram: clear reactions (cleanup)
        reactions.push((message.id, message.reaction_state, desired_validity));
    }

    CacheRebuildPlan { cache, reactions }
}

fn should_abort_rebuild_after_cache_load(load_result: CacheLoadResult) -> bool {
    matches!(
        load_result,
        CacheLoadResult::NotTracked | CacheLoadResult::Failed
    )
}

/// Calculate line count with prior newline consideration
fn line_count_with_prior_newline(content: &str, prior_ended_with_newline: bool) -> usize {
    content.lines().count() + if prior_ended_with_newline { 1 } else { 0 }
}

#[async_trait]
impl<CS, RP> EventHandler for BotHandler<'_, CS, RP>
where
    CS: ChannelService,
    RP: RosterProvider,
{
    async fn message(&self, ctx: Context, msg: Message) {
        if msg.author.bot {
            return;
        }

        if msg
            .reactions
            .iter()
            .any(|r| matches!(&r.reaction_type, ReactionType::Unicode(s) if s == "❎" && r.me))
        {
            return;
        }

        let Some(tracked_id) = self.channel_manager.get_tracked(msg.channel_id) else {
            return;
        };

        if self.has_pending_messages(tracked_id) {
            self.rebuild_channel_cache(&ctx, msg.channel_id, None).await;
            if self.defer_current_message_if_pending_remains(
                tracked_id,
                CachedMessage::from_message(msg.clone()),
            ) {
                return;
            }
        }

        match self.process_program_message(&ctx, &msg, tracked_id).await {
            ProcessProgramMessageResult::Store => {
                self.message_cache
                    .upsert_message(tracked_id, CachedMessage::from_message(msg));
            }
            ProcessProgramMessageResult::Deferred => {
                self.message_cache.upsert_message(
                    tracked_id,
                    Self::mark_pending(CachedMessage::from_message(msg)),
                );
            }
            ProcessProgramMessageResult::Skip => {}
        }
    }

    async fn ready(&self, ctx: Context, ready: Ready) {
        tracing::info!("Connected as {}", ready.user.name);
        self.initialize_enabled_channels(&ctx, &ready).await;
    }

    async fn channel_update(&self, ctx: Context, old: Option<GuildChannel>, new: GuildChannel) {
        let new_has_flag = ChannelManager::topic_has_flag(new.topic.as_deref());
        let old_has_flag = old
            .as_ref()
            .is_some_and(|o| ChannelManager::topic_has_flag(o.topic.as_deref()));

        // Process only when the flag presence actually changes
        if new_has_flag && !old_has_flag {
            self.track_channel(&ctx, new.id).await;
        } else if !new_has_flag && old_has_flag {
            self.untrack_channel(new.id);
        }
    }

    async fn guild_member_addition(&self, _ctx: Context, new_member: Member) {
        let guild_id = new_member.guild_id;
        let member_info = crate::discord::service::to_member_info(&new_member);
        let role_ids: Vec<RoleId> = new_member
            .roles
            .iter()
            .map(|role_id| RoleId(role_id.get()))
            .collect();
        self.roster_provider
            .apply_member_add(guild_id, member_info, &role_ids);
    }

    async fn guild_member_update(
        &self,
        _ctx: Context,
        _old_if_available: Option<Member>,
        new_if_available: Option<Member>,
        _event: GuildMemberUpdateEvent,
    ) {
        let Some(new_member) = new_if_available else {
            return;
        };
        let guild_id = new_member.guild_id;
        let member_info = crate::discord::service::to_member_info(&new_member);
        let role_ids: Vec<RoleId> = new_member
            .roles
            .iter()
            .map(|role_id| RoleId(role_id.get()))
            .collect();
        self.roster_provider
            .apply_member_update(guild_id, member_info, &role_ids);
    }

    async fn guild_member_removal(
        &self,
        _ctx: Context,
        guild_id: GuildId,
        user: User,
        _member_data: Option<Member>,
    ) {
        self.roster_provider
            .apply_member_remove(guild_id, MemberId(user.id.get()));
    }

    async fn message_delete(
        &self,
        _ctx: Context,
        channel_id: ChannelId,
        deleted_message_id: MessageId,
        _guild_id: Option<GuildId>,
    ) {
        let Some(tracked_id) = self.channel_manager.get_tracked(channel_id) else {
            return;
        };

        self.message_cache
            .remove_message(tracked_id, deleted_message_id);
    }

    async fn message_delete_bulk(
        &self,
        _ctx: Context,
        channel_id: ChannelId,
        deleted_message_ids: Vec<MessageId>,
        _guild_id: Option<GuildId>,
    ) {
        let Some(tracked_id) = self.channel_manager.get_tracked(channel_id) else {
            return;
        };

        self.message_cache
            .remove_messages(tracked_id, &deleted_message_ids);
    }

    async fn message_update(
        &self,
        ctx: Context,
        old_if_available: Option<Message>,
        new: Option<Message>,
        event: MessageUpdateEvent,
    ) {
        let Some(tracked_id) = self.channel_manager.get_tracked(event.channel_id) else {
            return;
        };

        let updated_cached = if let Some(message) = new {
            if message.author.bot {
                return;
            }
            Some(CachedMessage::from_message(message))
        } else if let Some(mut old) = old_if_available {
            if old.author.bot {
                return;
            }
            event.apply_to_message(&mut old);
            Some(CachedMessage::from_message(old))
        } else {
            let cached_updated = self
                .message_cache
                .with_messages(tracked_id, |msgs| {
                    msgs.get(&event.id).map(|cached| {
                        let mut updated = cached.clone();
                        updated.apply_event(&event);
                        updated
                    })
                })
                .flatten();

            if let Some(cached) = cached_updated {
                if cached.is_bot {
                    return;
                }
                Some(cached)
            } else {
                match event.channel_id.message(&ctx.http, event.id).await {
                    Ok(message) => {
                        if message.author.bot {
                            return;
                        }
                        Some(CachedMessage::from_message(message))
                    }
                    Err(e) => {
                        tracing::warn!(
                            "Failed to fetch updated message {} in {}: {:?}",
                            event.id,
                            event.channel_id,
                            e
                        );
                        return;
                    }
                }
            }
        };

        self.rebuild_channel_cache(&ctx, event.channel_id, updated_cached)
            .await;
    }

    async fn reaction_add(&self, ctx: Context, add_reaction: Reaction) {
        let Some(tracked_id) = self.channel_manager.get_tracked(add_reaction.channel_id) else {
            return;
        };

        let is_own_reaction = add_reaction.user_id == Some(ctx.cache.current_user().id);
        if !is_own_reaction {
            return;
        }

        let new_state = match &add_reaction.emoji {
            ReactionType::Unicode(s) if s == BotReaction::CHECK => Some(BotReactionState::HasCheck),
            ReactionType::Unicode(s) if s == BotReaction::CROSS => Some(BotReactionState::HasCross),
            _ => None,
        };

        let Some(new_state) = new_state else {
            return;
        };

        if self
            .message_cache
            .update_reaction_state(tracked_id, add_reaction.message_id, new_state)
        {
            self.rebuild_channel_cache(&ctx, add_reaction.channel_id, None)
                .await;
            return;
        }

        if let Ok(message) = add_reaction
            .channel_id
            .message(&ctx.http, add_reaction.message_id)
            .await
            && self.channel_manager.is_tracked(add_reaction.channel_id)
        {
            let cached = CachedMessage::from_message(message);
            self.rebuild_channel_cache(&ctx, add_reaction.channel_id, Some(cached))
                .await;
        }
    }

    async fn reaction_remove(&self, ctx: Context, removed_reaction: Reaction) {
        let Some(tracked_id) = self
            .channel_manager
            .get_tracked(removed_reaction.channel_id)
        else {
            return;
        };

        let is_own_reaction = removed_reaction.user_id == Some(ctx.cache.current_user().id);
        if !is_own_reaction {
            return;
        }

        let removed_check =
            matches!(&removed_reaction.emoji, ReactionType::Unicode(s) if s == BotReaction::CHECK);
        let removed_cross =
            matches!(&removed_reaction.emoji, ReactionType::Unicode(s) if s == BotReaction::CROSS);

        if !removed_check && !removed_cross {
            return;
        }

        let should_clear = self.message_cache.with_messages(tracked_id, |msgs| {
            msgs.get(&removed_reaction.message_id).map(|msg| {
                (removed_check && msg.reaction_state == BotReactionState::HasCheck)
                    || (removed_cross && msg.reaction_state == BotReactionState::HasCross)
            })
        });

        let should_clear = should_clear.flatten().unwrap_or(false);

        if should_clear
            && self.message_cache.update_reaction_state(
                tracked_id,
                removed_reaction.message_id,
                BotReactionState::None,
            )
        {
            self.rebuild_channel_cache(&ctx, removed_reaction.channel_id, None)
                .await;
            return;
        }

        if let Ok(message) = removed_reaction
            .channel_id
            .message(&ctx.http, removed_reaction.message_id)
            .await
            && self.channel_manager.is_tracked(removed_reaction.channel_id)
        {
            let cached = CachedMessage::from_message(message);
            self.rebuild_channel_cache(&ctx, removed_reaction.channel_id, Some(cached))
                .await;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        message_cache::CachedMessage,
        test_utils::{MockChannelService, MockRosterProvider},
    };
    use indexmap::IndexMap;
    use rstest::{fixture, rstest};
    use serenity::model::id::UserId;
    use std::sync::{
        Arc, OnceLock,
        atomic::{AtomicUsize, Ordering},
    };
    use walicord_application::{
        FilteredEmptyRoleParseError, ProgramParseError, RoleVisibilityDiagnostic,
        RoleVisibilityDiagnostics, Script, ScriptStatementWithLine,
    };
    use walicord_domain::{
        Declaration, Statement,
        model::{MemberSetExpr, MemberSetOp},
    };
    use walicord_infrastructure::{WalicordProgramParser, WalicordSettlementOptimizer};

    fn make_cached_message(id: u64, content: &str) -> CachedMessage {
        CachedMessage {
            id: MessageId::new(id),
            content: ArcStr::from(content),
            author_id: UserId::new(1),
            is_bot: false,
            reaction_state: BotReactionState::None,
            pending_evaluation: false,
        }
    }

    fn make_processor() -> MessageProcessor<'static> {
        MessageProcessor::new(&WalicordProgramParser, &WalicordSettlementOptimizer)
    }

    fn empty_roles() -> &'static RoleMembers {
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(RoleMembers::default)
    }

    fn role_expr(role_id: u64) -> MemberSetExpr<'static> {
        MemberSetExpr::new([MemberSetOp::PushRole(RoleId(role_id))])
    }

    fn make_script_with_role_reference_and_command(
        role_id: u64,
        command: ProgramCommand<'static>,
    ) -> Script<'static> {
        let members: &'static [MemberId] = &[];
        let roles = empty_roles();
        Script::new(
            members,
            roles,
            vec![
                ScriptStatementWithLine {
                    line: 1,
                    statement: ScriptStatement::Domain(Statement::Declaration(Declaration {
                        name: "team",
                        expression: role_expr(role_id),
                    })),
                },
                ScriptStatementWithLine {
                    line: 2,
                    statement: ScriptStatement::Command(command),
                },
            ],
        )
    }

    #[fixture]
    fn handler() -> BotHandler<'static, MockChannelService, MockRosterProvider> {
        let message_cache = MessageCache::new();
        let channel_service = MockChannelService::new();
        let roster_provider = MockRosterProvider::new();
        let processor = make_processor();
        let channel_manager = ChannelManager::new();
        BotHandler::new(
            message_cache,
            channel_service,
            roster_provider,
            processor,
            channel_manager,
        )
    }

    #[fixture]
    fn processor() -> MessageProcessor<'static> {
        make_processor()
    }

    #[rstest]
    fn untrack_channel_clears_cache_when_channel_was_tracked(
        handler: BotHandler<'static, MockChannelService, MockRosterProvider>,
    ) {
        let channel_id = ChannelId::new(1);

        let Some(ChannelEvent::Tracked(tracked_id)) = handler.channel_manager.track(channel_id)
        else {
            panic!("track should succeed");
        };
        let mut messages = IndexMap::new();
        messages.insert(MessageId::new(10), make_cached_message(10, "test content"));
        handler.message_cache.insert(tracked_id, messages);

        assert!(handler.channel_manager.is_tracked(channel_id));
        assert!(handler.message_cache.contains(channel_id));

        handler.untrack_channel(channel_id);

        assert!(!handler.channel_manager.is_tracked(channel_id));
        assert!(!handler.message_cache.contains(channel_id));
    }

    #[rstest]
    #[case::first_untrack_clears_cache(true, false)]
    #[case::second_untrack_is_noop(false, false)]
    fn untrack_channel_is_idempotent_for_cache(
        #[case] should_have_cache: bool,
        #[case] expected_cache_present: bool,
    ) {
        let handler = handler();
        let channel_id = ChannelId::new(1);

        let Some(ChannelEvent::Tracked(tracked_id)) = handler.channel_manager.track(channel_id)
        else {
            panic!("track should succeed");
        };
        let mut messages = IndexMap::new();
        messages.insert(MessageId::new(10), make_cached_message(10, "test"));
        handler.message_cache.insert(tracked_id, messages);

        handler.untrack_channel(channel_id);

        if !should_have_cache {
            handler.untrack_channel(channel_id);
        }

        assert_eq!(
            handler.message_cache.contains(channel_id),
            expected_cache_present
        );
    }

    #[rstest]
    fn untrack_channel_preserves_other_channel_cache(
        handler: BotHandler<'static, MockChannelService, MockRosterProvider>,
    ) {
        let channel_a = ChannelId::new(1);
        let channel_b = ChannelId::new(2);

        let Some(ChannelEvent::Tracked(tracked_a)) = handler.channel_manager.track(channel_a)
        else {
            panic!("track should succeed");
        };
        let Some(ChannelEvent::Tracked(tracked_b)) = handler.channel_manager.track(channel_b)
        else {
            panic!("track should succeed");
        };

        let mut messages_a = IndexMap::new();
        messages_a.insert(MessageId::new(10), make_cached_message(10, "a"));
        handler.message_cache.insert(tracked_a, messages_a);

        let mut messages_b = IndexMap::new();
        messages_b.insert(MessageId::new(20), make_cached_message(20, "b"));
        handler.message_cache.insert(tracked_b, messages_b);

        handler.untrack_channel(channel_a);

        assert!(!handler.channel_manager.is_tracked(channel_a));
        assert!(handler.channel_manager.is_tracked(channel_b));
        assert!(!handler.message_cache.contains(channel_a));
        assert!(handler.message_cache.contains(channel_b));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn track_channel_with_io_fetches_and_warms_up_on_first_track() {
        let handler = handler();
        let channel_id = ChannelId::new(1);

        let fetch_count = Arc::new(AtomicUsize::new(0));
        let fetch_count_clone = Arc::clone(&fetch_count);
        let warm_count = Arc::new(AtomicUsize::new(0));
        let warm_count_clone = Arc::clone(&warm_count);
        let mut fetched = IndexMap::new();
        fetched.insert(MessageId::new(10), make_cached_message(10, "test"));

        handler
            .track_channel_with_io(
                channel_id,
                move || {
                    fetch_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Ok(fetched))
                },
                move || {
                    warm_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Ok(()))
                },
            )
            .await;

        assert!(handler.channel_manager.is_tracked(channel_id));
        assert!(handler.message_cache.contains(channel_id));
        assert_eq!(fetch_count.load(Ordering::SeqCst), 1);
        assert_eq!(warm_count.load(Ordering::SeqCst), 1);
        assert!(!handler.channel_manager.has_fetch_failed(channel_id));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn track_channel_with_io_is_noop_when_already_tracked() {
        let handler = handler();
        let channel_id = ChannelId::new(1);
        let Some(_tracked_id) = handler.channel_manager.track(channel_id) else {
            panic!("track should succeed");
        };

        let fetch_count = Arc::new(AtomicUsize::new(0));
        let fetch_count_clone = Arc::clone(&fetch_count);
        let warm_count = Arc::new(AtomicUsize::new(0));
        let warm_count_clone = Arc::clone(&warm_count);

        handler
            .track_channel_with_io(
                channel_id,
                move || {
                    fetch_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Ok(IndexMap::new()))
                },
                move || {
                    warm_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Ok(()))
                },
            )
            .await;

        assert_eq!(fetch_count.load(Ordering::SeqCst), 0);
        assert_eq!(warm_count.load(Ordering::SeqCst), 0);
    }

    #[tokio::test(flavor = "current_thread")]
    async fn track_channel_with_io_marks_fetch_failed_and_still_warms_up() {
        let handler = handler();
        let channel_id = ChannelId::new(1);

        let fetch_count = Arc::new(AtomicUsize::new(0));
        let fetch_count_clone = Arc::clone(&fetch_count);
        let warm_count = Arc::new(AtomicUsize::new(0));
        let warm_count_clone = Arc::clone(&warm_count);

        handler
            .track_channel_with_io(
                channel_id,
                move || {
                    fetch_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Err(ServiceError::Request("boom".into())))
                },
                move || {
                    warm_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Err(ServiceError::Request("warm".into())))
                },
            )
            .await;

        assert!(handler.channel_manager.is_tracked(channel_id));
        assert!(handler.channel_manager.has_fetch_failed(channel_id));
        assert!(!handler.message_cache.contains(channel_id));
        assert_eq!(fetch_count.load(Ordering::SeqCst), 1);
        assert_eq!(warm_count.load(Ordering::SeqCst), 1);
    }

    #[rstest]
    fn plan_cache_rebuild_orders_messages_by_id_and_sets_reactions(
        processor: MessageProcessor<'static>,
    ) {
        let member_ids = [MemberId(1), MemberId(2)];
        let role_members = RoleMembers::default();
        let messages = vec![
            make_cached_message(3, "<@1> paid x to <@2>"),
            make_cached_message(1, "<@1> paid 100 to <@2>"),
            make_cached_message(2, "!variables"),
        ];

        let plan = plan_cache_rebuild(&processor, &member_ids, &role_members, &messages);

        let reaction_order: Vec<(u64, MessageValidity)> = plan
            .reactions
            .iter()
            .map(|(msg_id, _current, desired)| (msg_id.get(), *desired))
            .collect();
        assert_eq!(
            reaction_order,
            [
                (1, MessageValidity::Valid),
                (2, MessageValidity::NotProgram),
                (3, MessageValidity::Invalid),
            ]
        );

        let cached_ids: Vec<u64> = plan.cache.keys().map(|id| id.get()).collect();
        assert_eq!(cached_ids, [1, 2]);
    }

    #[rstest]
    #[case::failed(CacheLoadResult::Failed, true)]
    #[case::not_tracked(CacheLoadResult::NotTracked, true)]
    #[case::loaded_empty(CacheLoadResult::LoadedEmpty, false)]
    #[case::loaded_non_empty(CacheLoadResult::LoadedNonEmpty, false)]
    fn should_abort_rebuild_after_cache_load_matches_terminal_states(
        #[case] load_result: CacheLoadResult,
        #[case] expected: bool,
    ) {
        let actual = should_abort_rebuild_after_cache_load(load_result);
        assert_eq!(actual, expected);
    }

    #[tokio::test(flavor = "current_thread")]
    async fn ensure_cache_loaded_cache_hit_non_empty_does_not_fetch() {
        let handler = handler();
        let channel_id = ChannelId::new(1);
        let Some(ChannelEvent::Tracked(tracked_id)) = handler.channel_manager.track(channel_id)
        else {
            panic!("track should succeed");
        };
        let mut messages = IndexMap::new();
        messages.insert(MessageId::new(10), make_cached_message(10, "test"));
        handler.message_cache.insert(tracked_id, messages);

        let fetch_count = Arc::new(AtomicUsize::new(0));
        let fetch_count_clone = Arc::clone(&fetch_count);
        let result = handler
            .ensure_cache_loaded_with_fetch(channel_id, move || {
                fetch_count_clone.fetch_add(1, Ordering::SeqCst);
                std::future::ready(Ok(IndexMap::new()))
            })
            .await;

        assert_eq!(result, CacheLoadResult::LoadedNonEmpty);
        assert_eq!(fetch_count.load(Ordering::SeqCst), 0);
    }

    #[tokio::test(flavor = "current_thread")]
    async fn ensure_cache_loaded_cache_hit_empty_does_not_fetch() {
        let handler = handler();
        let channel_id = ChannelId::new(1);
        let Some(ChannelEvent::Tracked(tracked_id)) = handler.channel_manager.track(channel_id)
        else {
            panic!("track should succeed");
        };
        handler.message_cache.insert(tracked_id, IndexMap::new());

        let fetch_count = Arc::new(AtomicUsize::new(0));
        let fetch_count_clone = Arc::clone(&fetch_count);
        let result = handler
            .ensure_cache_loaded_with_fetch(channel_id, move || {
                fetch_count_clone.fetch_add(1, Ordering::SeqCst);
                std::future::ready(Ok(IndexMap::new()))
            })
            .await;

        assert_eq!(result, CacheLoadResult::LoadedEmpty);
        assert_eq!(fetch_count.load(Ordering::SeqCst), 0);
    }

    #[tokio::test(flavor = "current_thread")]
    async fn ensure_cache_loaded_not_tracked_does_not_fetch() {
        let handler = handler();
        let channel_id = ChannelId::new(1);

        let fetch_count = Arc::new(AtomicUsize::new(0));
        let fetch_count_clone = Arc::clone(&fetch_count);
        let result = handler
            .ensure_cache_loaded_with_fetch(channel_id, move || {
                fetch_count_clone.fetch_add(1, Ordering::SeqCst);
                std::future::ready(Ok(IndexMap::new()))
            })
            .await;

        assert_eq!(result, CacheLoadResult::NotTracked);
        assert_eq!(fetch_count.load(Ordering::SeqCst), 0);
    }

    #[tokio::test(flavor = "current_thread")]
    async fn ensure_cache_loaded_fetches_on_cache_miss_and_inserts_cache() {
        let handler = handler();
        let channel_id = ChannelId::new(1);
        let Some(_tracked_id) = handler.channel_manager.track(channel_id) else {
            panic!("track should succeed");
        };

        let fetch_count = Arc::new(AtomicUsize::new(0));
        let fetch_count_clone = Arc::clone(&fetch_count);
        let mut fetched = IndexMap::new();
        fetched.insert(MessageId::new(10), make_cached_message(10, "test"));

        let result = handler
            .ensure_cache_loaded_with_fetch(channel_id, move || {
                fetch_count_clone.fetch_add(1, Ordering::SeqCst);
                std::future::ready(Ok(fetched))
            })
            .await;

        assert_eq!(result, CacheLoadResult::LoadedNonEmpty);
        assert_eq!(fetch_count.load(Ordering::SeqCst), 1);
        assert!(handler.message_cache.contains(channel_id));
        assert!(!handler.channel_manager.has_fetch_failed(channel_id));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn ensure_cache_loaded_marks_failed_on_fetch_error() {
        let handler = handler();
        let channel_id = ChannelId::new(1);
        let Some(_tracked_id) = handler.channel_manager.track(channel_id) else {
            panic!("track should succeed");
        };

        let fetch_count = Arc::new(AtomicUsize::new(0));
        let fetch_count_clone = Arc::clone(&fetch_count);
        let result = handler
            .ensure_cache_loaded_with_fetch(channel_id, move || {
                fetch_count_clone.fetch_add(1, Ordering::SeqCst);
                std::future::ready(Err(ServiceError::Request("boom".into())))
            })
            .await;

        assert_eq!(result, CacheLoadResult::Failed);
        assert_eq!(fetch_count.load(Ordering::SeqCst), 1);
        assert!(handler.channel_manager.has_fetch_failed(channel_id));
    }

    #[rstest]
    #[case::multiline_domain_then_settleup(
        "<@1> paid 100 to <@2>\n<@1> paid 50 to <@2>",
        "!settleup <@1>",
        [(1, MessageValidity::Valid), (2, MessageValidity::Valid)],
        [1, 2]
    )]
    fn plan_cache_rebuild_respects_line_offsets(
        processor: MessageProcessor<'static>,
        #[case] first: &str,
        #[case] second: &str,
        #[case] expected_reactions: [(u64, MessageValidity); 2],
        #[case] expected_cached: [u64; 2],
    ) {
        let member_ids = [MemberId(1), MemberId(2)];
        let role_members = RoleMembers::default();
        let messages = vec![
            make_cached_message(1, first),
            make_cached_message(2, second),
        ];

        let plan = plan_cache_rebuild(&processor, &member_ids, &role_members, &messages);

        let reaction_order: Vec<(u64, MessageValidity)> = plan
            .reactions
            .iter()
            .map(|(msg_id, _current, desired)| (msg_id.get(), *desired))
            .collect();
        assert_eq!(reaction_order, expected_reactions);

        let cached_ids: Vec<u64> = plan.cache.keys().map(|id| id.get()).collect();
        assert_eq!(cached_ids, expected_cached);
    }

    #[test]
    fn remove_message_keeps_channel_when_empty() {
        let cache = MessageCache::new();
        let channel_id = ChannelId::new(1);
        let manager = ChannelManager::new();
        let Some(ChannelEvent::Tracked(tracked_id)) = manager.track(channel_id) else {
            panic!("track should succeed");
        };
        let mut messages = IndexMap::new();
        messages.insert(MessageId::new(10), make_cached_message(10, "test"));
        cache.insert(tracked_id, messages);

        cache.remove_message(tracked_id, MessageId::new(10));

        let is_empty = cache.with_messages(tracked_id, |msgs| msgs.is_empty());
        assert_eq!(is_empty, Some(true));
    }

    #[rstest]
    fn defer_current_message_if_pending_remains_stores_current_message_as_pending(
        handler: BotHandler<'static, MockChannelService, MockRosterProvider>,
    ) {
        let channel_id = ChannelId::new(1);
        let Some(ChannelEvent::Tracked(tracked_id)) = handler.channel_manager.track(channel_id)
        else {
            panic!("track should succeed");
        };

        let mut pending = make_cached_message(1, "<@1> paid 100 to <@2>");
        pending.pending_evaluation = true;
        let mut messages = IndexMap::new();
        messages.insert(pending.id, pending);
        handler.message_cache.insert(tracked_id, messages);

        let current = make_cached_message(2, "!variables");
        let deferred = handler.defer_current_message_if_pending_remains(tracked_id, current);

        assert!(deferred);

        let stored = handler
            .message_cache
            .with_messages(tracked_id, |msgs| msgs.get(&MessageId::new(2)).cloned())
            .flatten()
            .expect("current message should be cached");
        assert!(stored.pending_evaluation);
        assert_eq!(stored.reaction_state, BotReactionState::None);
    }

    #[rstest]
    fn plan_cache_rebuild_re_evaluates_marked_invalid_messages(
        processor: MessageProcessor<'static>,
    ) {
        let member_ids = [MemberId(1), MemberId(2)];

        let mut invalid_message = make_cached_message(1, "<@1> paid 100 to <@2>");
        invalid_message.reaction_state = BotReactionState::HasCross;

        let valid_message = make_cached_message(2, "!variables");

        let messages = vec![invalid_message, valid_message];
        let role_members = RoleMembers::default();

        let plan = plan_cache_rebuild(&processor, &member_ids, &role_members, &messages);

        let cached_ids: Vec<u64> = plan.cache.keys().map(|id| id.get()).collect();
        assert_eq!(cached_ids, [1, 2]);

        let reactions: Vec<(u64, MessageValidity)> = plan
            .reactions
            .iter()
            .map(|(id, _, validity)| (id.get(), *validity))
            .collect();
        assert_eq!(
            reactions,
            [
                (1, MessageValidity::Valid),
                (2, MessageValidity::NotProgram),
            ]
        );
    }

    #[rstest]
    fn plan_cache_rebuild_clears_pending_flag_on_re_evaluation(
        processor: MessageProcessor<'static>,
    ) {
        let member_ids = [MemberId(1), MemberId(2)];
        let role_members = RoleMembers::default();

        let mut pending = make_cached_message(1, "<@1> paid 100 to <@2>");
        pending.pending_evaluation = true;
        pending.reaction_state = BotReactionState::HasCross;

        let plan = plan_cache_rebuild(&processor, &member_ids, &role_members, &[pending]);

        let rebuilt = plan
            .cache
            .get(&MessageId::new(1))
            .expect("rebuild should cache valid message");
        assert!(!rebuilt.pending_evaluation);
        assert_eq!(rebuilt.reaction_state, BotReactionState::HasCheck);
    }

    #[rstest]
    fn plan_cache_rebuild_returns_empty_cache_when_all_messages_parse_invalid(
        processor: MessageProcessor<'static>,
    ) {
        let member_ids = [MemberId(1), MemberId(2)];

        let mut invalid1 = make_cached_message(1, "<@1> paid x to <@2>");
        invalid1.reaction_state = BotReactionState::HasCross;

        let mut invalid2 = make_cached_message(2, "<@1> paid y to <@2>");
        invalid2.reaction_state = BotReactionState::HasCross;

        let messages = vec![invalid1, invalid2];
        let role_members = RoleMembers::default();

        let plan = plan_cache_rebuild(&processor, &member_ids, &role_members, &messages);

        assert!(plan.cache.is_empty());
        let reactions: Vec<(u64, MessageValidity)> = plan
            .reactions
            .iter()
            .map(|(id, _, validity)| (id.get(), *validity))
            .collect();
        assert_eq!(
            reactions,
            [(1, MessageValidity::Invalid), (2, MessageValidity::Invalid),]
        );
    }

    #[test]
    fn format_program_parse_error_reply_uses_filtered_empty_role_message_when_diagnostic_matches() {
        let diagnostics = RoleVisibilityDiagnostics::from([(
            RoleId(10),
            RoleVisibilityDiagnostic {
                total_members: 2,
                visible_members: 0,
                excluded_members: 2,
            },
        )]);
        let mention = "@user";

        let actual = format_program_parse_error_reply(
            ProgramParseError::UndefinedRole { id: 10, line: 4 },
            &diagnostics,
            mention,
        );

        let expected = role_visibility_feedback::format_filtered_empty_role_parse_error(
            FilteredEmptyRoleParseError {
                role_id: RoleId(10),
                line: 4,
                excluded_members: 2,
            },
            mention,
        );
        assert_eq!(actual, expected);
    }

    #[test]
    fn format_program_parse_error_reply_falls_back_to_default_presenter_without_diagnostic() {
        let diagnostics = RoleVisibilityDiagnostics::default();
        let mention = "@user";

        let actual = format_program_parse_error_reply(
            ProgramParseError::UndefinedRole { id: 10, line: 4 },
            &diagnostics,
            mention,
        );

        let expected = format_program_parse_error(
            ProgramParseError::UndefinedRole { id: 10, line: 4 },
            mention,
        );
        assert_eq!(actual, expected);
    }

    #[test]
    fn command_warning_lines_for_prefix_formats_referenced_role_warnings() {
        let script = make_script_with_role_reference_and_command(10, ProgramCommand::Review);
        let diagnostics = RoleVisibilityDiagnostics::from([
            (
                RoleId(10),
                RoleVisibilityDiagnostic {
                    total_members: 3,
                    visible_members: 2,
                    excluded_members: 1,
                },
            ),
            (
                RoleId(20),
                RoleVisibilityDiagnostic {
                    total_members: 1,
                    visible_members: 1,
                    excluded_members: 0,
                },
            ),
        ]);

        let actual = command_warning_lines_for_prefix(&script, 1, &diagnostics);

        let expected =
            vec![walicord_i18n::role_members_filtered_by_channel_visibility(10, 2, 1).to_string()];
        assert_eq!(actual, expected);
    }

    #[test]
    fn command_warning_lines_for_prefix_warns_for_command_only_filtered_empty_roles() {
        let script = make_script_with_role_reference_and_command(
            10,
            ProgramCommand::SettleUp {
                members: role_expr(20),
                cash_members: None,
            },
        );
        let diagnostics = RoleVisibilityDiagnostics::from([
            (
                RoleId(10),
                RoleVisibilityDiagnostic {
                    total_members: 3,
                    visible_members: 3,
                    excluded_members: 0,
                },
            ),
            (
                RoleId(20),
                RoleVisibilityDiagnostic {
                    total_members: 2,
                    visible_members: 0,
                    excluded_members: 2,
                },
            ),
        ]);

        let actual = command_warning_lines_for_prefix(&script, 1, &diagnostics);

        let expected =
            vec![walicord_i18n::role_members_filtered_by_channel_visibility(20, 0, 2).to_string()];
        assert_eq!(actual, expected);
    }

    #[test]
    fn plan_program_command_effects_appends_warning_to_variables_reply() {
        let script = make_script_with_role_reference_and_command(10, ProgramCommand::Variables);
        let diagnostics = RoleVisibilityDiagnostics::from([(
            RoleId(10),
            RoleVisibilityDiagnostic {
                total_members: 3,
                visible_members: 2,
                excluded_members: 1,
            },
        )]);

        let actual = plan_program_command_effects(&script, &[], &diagnostics, 0);

        let expected_variables =
            VariablesPresenter::render_for_prefix_with_members(&script, 1, &[]);
        let expected_warning =
            walicord_i18n::role_members_filtered_by_channel_visibility(10, 2, 1).to_string();
        let expected = vec![ProgramCommandEffect::ReplyText(format!(
            "{expected_variables}\n\n{expected_warning}"
        ))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn plan_program_command_effects_sends_warning_before_settlement_for_review() {
        let script = make_script_with_role_reference_and_command(10, ProgramCommand::Review);
        let diagnostics = RoleVisibilityDiagnostics::from([(
            RoleId(10),
            RoleVisibilityDiagnostic {
                total_members: 3,
                visible_members: 2,
                excluded_members: 1,
            },
        )]);

        let actual = plan_program_command_effects(&script, &[], &diagnostics, 0);

        let expected = vec![
            ProgramCommandEffect::ReplyText(
                walicord_i18n::role_members_filtered_by_channel_visibility(10, 2, 1).to_string(),
            ),
            ProgramCommandEffect::RunSettlement { stmt_index: 1 },
        ];
        assert_eq!(actual, expected);
    }
}
