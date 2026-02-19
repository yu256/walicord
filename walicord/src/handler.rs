use crate::{
    channel::{ChannelEvent, ChannelManager, TrackedChannelId},
    discord::ports::{ChannelService, RosterProvider},
    message_cache::{CachedMessage, MessageCache, next_line_offset},
    reaction::{BotReaction, BotReactionState, MessageValidity, ReactionService},
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
use walicord_application::{Command as ProgramCommand, MessageProcessor, ScriptStatement};
use walicord_domain::model::MemberId;
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
        let Some(ChannelEvent::Tracked(tracked_id)) = self.channel_manager.track(channel_id) else {
            return;
        };

        let channel_id = tracked_id.get();
        tracing::info!("Tracking channel {}", channel_id);
        match self
            .channel_service
            .fetch_all_messages(ctx, channel_id)
            .await
        {
            Ok(messages) => {
                if self.channel_manager.mark_fetch_succeeded(channel_id) {
                    let cached: IndexMap<MessageId, CachedMessage> = messages
                        .into_iter()
                        .map(|(id, msg)| (id, CachedMessage::from_message(msg)))
                        .collect();
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

        if let Err(e) = self.roster_provider.warm_up(ctx, channel_id).await {
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

    pub(crate) async fn ensure_cache_loaded(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> CacheLoadResult {
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

        match self
            .channel_service
            .fetch_all_messages(ctx, channel_id)
            .await
        {
            Ok(messages) => {
                let Some(tracked_id) = self.channel_manager.get_tracked(channel_id) else {
                    return CacheLoadResult::NotTracked;
                };
                let _ = self.channel_manager.mark_fetch_succeeded(channel_id);
                let cached_messages: IndexMap<MessageId, CachedMessage> = messages
                    .into_iter()
                    .map(|(id, msg)| (id, CachedMessage::from_message(msg)))
                    .collect();
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
        let load_result = self.ensure_cache_loaded(ctx, channel_id).await;

        match load_result {
            CacheLoadResult::NotTracked | CacheLoadResult::Failed => return,
            CacheLoadResult::LoadedEmpty | CacheLoadResult::LoadedNonEmpty => {}
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

        let member_ids = match self
            .roster_provider
            .roster_for_channel(ctx, channel_id)
            .await
        {
            Ok(ids) => ids,
            Err(e) => {
                tracing::warn!(
                    "Failed to fetch channel member IDs for {}: {:?}",
                    channel_id,
                    e
                );
                Vec::new()
            }
        };

        let plan = plan_cache_rebuild(&self.processor, &member_ids, &messages);

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
    ) -> bool {
        let member_ids = match self
            .roster_provider
            .roster_for_channel(ctx, msg.channel_id)
            .await
        {
            Ok(member_ids) => member_ids,
            Err(e) => {
                tracing::warn!(
                    "Failed to fetch channel member IDs for {}: {:?}",
                    msg.channel_id,
                    e
                );
                Vec::new()
            }
        };

        let author_id = MemberId(msg.author.id.get());
        let (cached_contents, next_line_offset) = self
            .message_cache
            .with_messages(tracked_id, |messages| {
                let offset = next_line_offset(messages.iter().map(|(_, m)| m));
                let contents: Vec<(ArcStr, Option<MemberId>)> = messages
                    .iter()
                    .filter(|(_, m)| !m.is_bot && !m.is_marked_invalid())
                    .map(|(_, m)| (m.content.clone(), Some(MemberId(m.author_id.get()))))
                    .collect();
                (contents, offset)
            })
            .unwrap_or_default();

        let result = match evaluate_program(
            &self.processor,
            &member_ids,
            &cached_contents,
            msg.content.as_str(),
            author_id,
            next_line_offset,
        ) {
            Ok(result) => result,
            Err(failure) => {
                ReactionService::update_state(ctx, msg, MessageValidity::Invalid).await;
                let content = format_program_parse_error(failure, msg.author.mention());
                let _ = msg.reply(&ctx.http, content).await;
                return false;
            }
        };

        if result.has_effect_statement {
            ReactionService::react(ctx, msg, '✅').await;
        }

        if result.has_effect_statement
            && let Some(program) = result.program.as_ref()
        {
            let settlement_service = SettlementService::new(&self.processor, &self.roster_provider);
            let mut member_directory: Option<HashMap<MemberId, smol_str::SmolStr>> = None;

            for (stmt_index, stmt) in program.statements().iter().enumerate() {
                if stmt.line <= next_line_offset {
                    continue;
                }
                if let walicord_application::ScriptStatement::Command(command) = &stmt.statement {
                    match command {
                        ProgramCommand::Variables => {
                            let reply = VariablesPresenter::render_for_prefix_with_members(
                                program,
                                stmt_index,
                                &member_ids,
                            );
                            let _ = msg.reply(&ctx.http, reply).await;
                        }
                        ProgramCommand::Review | ProgramCommand::SettleUp { .. } => {
                            settlement_service
                                .handle_settlement_command(
                                    ctx,
                                    msg,
                                    stmt_index,
                                    program,
                                    &member_ids,
                                    &mut member_directory,
                                )
                                .await;
                        }
                        ProgramCommand::MemberAddCash { .. } => {}
                    }
                }
            }
        }

        result.should_store
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
        if message.is_bot || message.is_marked_invalid() {
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

        if self.process_program_message(&ctx, &msg, tracked_id).await {
            self.message_cache
                .upsert_message(tracked_id, CachedMessage::from_message(msg));
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
        self.roster_provider.apply_member_add(guild_id, member_info);
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
        self.roster_provider
            .apply_member_update(guild_id, member_info);
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
    use walicord_infrastructure::{WalicordProgramParser, WalicordSettlementOptimizer};

    fn make_cached_message(id: u64, content: &str) -> CachedMessage {
        CachedMessage {
            id: MessageId::new(id),
            content: ArcStr::from(content),
            author_id: UserId::new(1),
            is_bot: false,
            reaction_state: BotReactionState::None,
        }
    }

    fn make_processor() -> MessageProcessor<'static> {
        MessageProcessor::new(&WalicordProgramParser, &WalicordSettlementOptimizer)
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

    #[rstest]
    fn plan_cache_rebuild_orders_messages_by_id_and_sets_reactions(
        processor: MessageProcessor<'static>,
    ) {
        let member_ids = [MemberId(1), MemberId(2)];
        let messages = vec![
            make_cached_message(3, "<@1> paid x to <@2>"),
            make_cached_message(1, "<@1> paid 100 to <@2>"),
            make_cached_message(2, "!variables"),
        ];

        let plan = plan_cache_rebuild(&processor, &member_ids, &messages);

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
        let messages = vec![
            make_cached_message(1, first),
            make_cached_message(2, second),
        ];

        let plan = plan_cache_rebuild(&processor, &member_ids, &messages);

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
    fn plan_cache_rebuild_skips_marked_invalid_messages(processor: MessageProcessor<'static>) {
        let member_ids = [MemberId(1), MemberId(2)];

        let mut invalid_message = make_cached_message(1, "<@1> paid 100 to <@2>");
        invalid_message.reaction_state = BotReactionState::HasCross;

        let valid_message = make_cached_message(2, "!variables");

        let messages = vec![invalid_message, valid_message];

        let plan = plan_cache_rebuild(&processor, &member_ids, &messages);

        let cached_ids: Vec<u64> = plan.cache.keys().map(|id| id.get()).collect();
        assert_eq!(cached_ids, [2]);

        let reaction_ids: Vec<u64> = plan.reactions.iter().map(|(id, _, _)| id.get()).collect();
        assert_eq!(reaction_ids, [2]);
    }

    #[rstest]
    fn plan_cache_rebuild_returns_empty_when_all_messages_invalid(
        processor: MessageProcessor<'static>,
    ) {
        let member_ids = [MemberId(1), MemberId(2)];

        let mut invalid1 = make_cached_message(1, "<@1> paid 100 to <@2>");
        invalid1.reaction_state = BotReactionState::HasCross;

        let mut invalid2 = make_cached_message(2, "!variables");
        invalid2.reaction_state = BotReactionState::HasCross;

        let messages = vec![invalid1, invalid2];

        let plan = plan_cache_rebuild(&processor, &member_ids, &messages);

        assert!(plan.cache.is_empty());
        assert!(plan.reactions.is_empty());
    }
}
