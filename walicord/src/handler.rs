use crate::{
    channel::{ChannelManager, ChannelToggle},
    discord::ports::{ChannelService, RosterProvider},
    message_cache::{MessageCache, next_line_offset},
    reaction::{ReactionService, ReactionState},
    settlement::{SettlementService, process_program_message},
};
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
use walicord_presentation::VariablesPresenter;

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

    async fn enable_channel(&self, ctx: &Context, channel_id: ChannelId) {
        if !matches!(
            self.channel_manager.enable(channel_id),
            ChannelToggle::Enabled
        ) {
            return;
        }

        tracing::info!("Enabling channel {}", channel_id);
        match self
            .channel_service
            .fetch_all_messages(ctx, channel_id)
            .await
        {
            Ok(messages) => {
                if self.channel_manager.is_enabled(channel_id) {
                    if messages.is_empty() {
                        self.message_cache.remove(&channel_id);
                    } else {
                        self.message_cache.insert(channel_id, messages);
                    }
                }
            }
            Err(e) => {
                tracing::warn!(
                    "Failed to fetch initial messages for {}: {:?}",
                    channel_id,
                    e
                );
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

    fn disable_channel(&self, channel_id: ChannelId) {
        if let ChannelToggle::Disabled = self.channel_manager.disable(channel_id) {
            tracing::info!("Disabled channel {}", channel_id);
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
                            self.enable_channel(ctx, channel.id).await;
                        }
                    }
                }
                Err(e) => {
                    tracing::warn!("Failed to fetch channels for guild {}: {:?}", guild_id, e);
                }
            }
        }
    }

    async fn rebuild_channel_cache(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
        updated_message: Option<Message>,
    ) {
        let mut messages: Vec<Message> = match self.message_cache.get(channel_id) {
            Some(messages) => messages.iter().map(|(_, m)| m.clone()).collect(),
            None => Vec::new(),
        };

        if let Some(updated_message) = updated_message {
            if let Some(pos) = messages.iter().position(|m| m.id == updated_message.id) {
                messages[pos] = updated_message;
            } else {
                messages.push(updated_message);
            }
        }

        if messages.is_empty() {
            self.message_cache.remove(&channel_id);
            return;
        }

        let member_ids = match self
            .roster_provider
            .roster_for_channel(ctx, channel_id)
            .await
        {
            Ok(member_ids) => member_ids,
            Err(e) => {
                tracing::warn!(
                    "Failed to fetch channel member IDs for {}: {:?}",
                    channel_id,
                    e
                );
                Vec::new()
            }
        };

        let plan = plan_cache_rebuild(&self.processor, &member_ids, messages);

        for (message, desired_reaction) in plan.reactions {
            ReactionService::update_state(ctx, &message, desired_reaction).await;
        }

        if plan.cache.is_empty() {
            self.message_cache.remove(&channel_id);
        } else {
            self.message_cache.insert(channel_id, plan.cache);
        }
    }

    async fn process_program_message(&self, ctx: &Context, msg: &Message) -> bool {
        let (cached_contents, next_line_offset) = {
            let guard = self.message_cache.get(msg.channel_id);
            match &guard {
                Some(messages) => {
                    let offset = next_line_offset(messages.iter().map(|(_, m)| m));
                    let contents: Vec<(String, Option<MemberId>)> = messages
                        .iter()
                        .map(|(_, m)| (m.content.clone(), Some(MemberId(m.author.id.get()))))
                        .collect();
                    (contents, offset)
                }
                None => (Vec::new(), 0),
            }
        };

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

        let result = process_program_message(
            ctx,
            msg,
            &self.processor,
            &member_ids,
            &cached_contents,
            next_line_offset,
        )
        .await;

        if result.has_effect_statement {
            ReactionService::react(ctx, msg, '✅').await;
        }

        // Execute commands
        if result.has_effect_statement
            && let Some(program) = result.program.as_ref()
        {
            let settlement_service = SettlementService::new(&self.processor, &self.roster_provider);
            let mut member_directory: Option<HashMap<MemberId, String>> = None;

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
                        ProgramCommand::MemberAddCash { .. } => {
                            // No immediate action needed
                        }
                    }
                }
            }
        }

        result.should_store
    }
}

/// Plan for rebuilding channel cache
struct CacheRebuildPlan {
    cache: IndexMap<MessageId, Message>,
    reactions: Vec<(Message, ReactionState)>,
}

/// Plan cache rebuild based on message processing
fn plan_cache_rebuild(
    processor: &MessageProcessor,
    member_ids: &[MemberId],
    mut messages: Vec<Message>,
) -> CacheRebuildPlan {
    messages.sort_by_key(|m| m.id.get());

    let mut cache: IndexMap<MessageId, Message> = IndexMap::new();
    let mut reactions = Vec::new();
    let mut inputs: Vec<(String, Option<MemberId>)> = Vec::new();
    let mut line_count = 0usize;
    let mut ends_with_newline = false;
    let mut has_any = false;

    for message in messages {
        if message.author.bot {
            continue;
        }

        let message_content = message.content.clone();
        let author_id = MemberId(message.author.id.get());
        let line_offset = if has_any {
            line_count + if ends_with_newline { 1 } else { 0 }
        } else {
            0
        };

        let (desired_reaction, should_store) = {
            let outcome = processor.parse_program_sequence(
                member_ids,
                inputs
                    .iter()
                    .map(|(content, author_id)| (content.as_str(), *author_id))
                    .chain(std::iter::once((message_content.as_str(), Some(author_id)))),
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
                        ReactionState::Valid
                    } else {
                        ReactionState::Clear
                    };
                    (desired, should_store)
                }
                _ => (ReactionState::Invalid, false),
            }
        };

        reactions.push((message.clone(), desired_reaction));

        if should_store {
            cache.insert(message.id, message);
            inputs.push((message_content.clone(), Some(author_id)));
            if has_any {
                line_count += line_count_with_prior_newline(&message_content, ends_with_newline);
            } else {
                line_count = message_content.lines().count();
            }
            ends_with_newline = message_content.is_empty() || message_content.ends_with('\n');
            has_any = true;
        }
    }

    CacheRebuildPlan { cache, reactions }
}

/// Calculate line count with prior newline consideration
fn line_count_with_prior_newline(content: &str, prior_ended_with_newline: bool) -> usize {
    content.lines().count() + if prior_ended_with_newline { 1 } else { 0 }
}

/// Resolution for message update events
enum MessageUpdateResolution {
    Message(Box<Message>),
    Fetch,
}

/// Resolve message update from various sources
fn resolve_message_update(
    event: &MessageUpdateEvent,
    new: Option<Message>,
    old: Option<Message>,
    cached: Option<Message>,
) -> MessageUpdateResolution {
    if let Some(message) = new {
        return MessageUpdateResolution::Message(Box::new(message));
    }
    if let Some(mut message) = old {
        event.apply_to_message(&mut message);
        return MessageUpdateResolution::Message(Box::new(message));
    }
    if let Some(mut message) = cached {
        event.apply_to_message(&mut message);
        return MessageUpdateResolution::Message(Box::new(message));
    }

    MessageUpdateResolution::Fetch
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

        if !self.channel_manager.is_enabled(msg.channel_id) {
            return;
        }

        if self.process_program_message(&ctx, &msg).await {
            self.message_cache
                .entry(msg.channel_id)
                .or_default()
                .insert(msg.id, msg);
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
            self.enable_channel(&ctx, new.id).await;
        } else if !new_has_flag && old_has_flag {
            self.disable_channel(new.id);
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
        if !self.channel_manager.is_enabled(channel_id) {
            return;
        }

        self.message_cache
            .remove_message(channel_id, deleted_message_id);
    }

    async fn message_delete_bulk(
        &self,
        _ctx: Context,
        channel_id: ChannelId,
        deleted_message_ids: Vec<MessageId>,
        _guild_id: Option<GuildId>,
    ) {
        if !self.channel_manager.is_enabled(channel_id) {
            return;
        }

        self.message_cache
            .remove_messages(channel_id, &deleted_message_ids);
    }

    async fn message_update(
        &self,
        ctx: Context,
        old_if_available: Option<Message>,
        new: Option<Message>,
        event: MessageUpdateEvent,
    ) {
        if !self.channel_manager.is_enabled(event.channel_id) {
            return;
        }

        let cached_message = self
            .message_cache
            .get(event.channel_id)
            .and_then(|messages| messages.get(&event.id).cloned());

        let updated_message =
            match resolve_message_update(&event, new, old_if_available, cached_message) {
                MessageUpdateResolution::Message(message) => *message,
                MessageUpdateResolution::Fetch => {
                    match event.channel_id.message(&ctx.http, event.id).await {
                        Ok(message) => message,
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

        if updated_message.author.bot {
            return;
        }

        self.rebuild_channel_cache(&ctx, event.channel_id, Some(updated_message))
            .await;
    }

    async fn reaction_add(&self, ctx: Context, add_reaction: Reaction) {
        if !self.channel_manager.is_enabled(add_reaction.channel_id) {
            return;
        }

        let current_user_id = ctx.cache.current_user().id;
        let is_me = add_reaction.user_id == Some(current_user_id);
        if let Some(mut messages) = self.message_cache.get_mut(add_reaction.channel_id)
            && let Some(message) = messages.get_mut(&add_reaction.message_id)
        {
            let updated = crate::message_cache::update_cached_reaction_add(
                message,
                &add_reaction.emoji,
                is_me,
                add_reaction.burst,
            );
            if updated {
                return;
            }
        }

        if let Ok(message) = add_reaction
            .channel_id
            .message(&ctx.http, add_reaction.message_id)
            .await
            && let Some(mut messages) = self.message_cache.get_mut(add_reaction.channel_id)
        {
            messages.insert(message.id, message);
        }
    }

    async fn reaction_remove(&self, ctx: Context, removed_reaction: Reaction) {
        if !self.channel_manager.is_enabled(removed_reaction.channel_id) {
            return;
        }

        let current_user_id = ctx.cache.current_user().id;
        let is_me = removed_reaction.user_id == Some(current_user_id);
        if let Some(mut messages) = self.message_cache.get_mut(removed_reaction.channel_id)
            && let Some(message) = messages.get_mut(&removed_reaction.message_id)
        {
            let updated = crate::message_cache::update_cached_reaction_remove(
                message,
                &removed_reaction.emoji,
                is_me,
                removed_reaction.burst,
            );
            if updated {
                return;
            }
        }

        if let Ok(message) = removed_reaction
            .channel_id
            .message(&ctx.http, removed_reaction.message_id)
            .await
            && let Some(mut messages) = self.message_cache.get_mut(removed_reaction.channel_id)
        {
            messages.insert(message.id, message);
        }
    }
}
