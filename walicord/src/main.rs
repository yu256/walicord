#![warn(clippy::uninlined_format_args)]

mod infrastructure;
mod member_roster_provider;

use dashmap::DashMap;
use indexmap::IndexMap;
use infrastructure::{
    discord::{ChannelError, DiscordChannelService, to_member_info},
    svg_renderer::svg_to_png,
};
use member_roster_provider::MemberRosterProvider;
use serenity::{
    all::MessageId,
    async_trait,
    model::{
        channel::{Message, Reaction, ReactionType},
        event::{GuildMemberUpdateEvent, MessageUpdateEvent},
        gateway::Ready,
        guild::Member,
        id::{ChannelId, GuildId},
        user::User,
    },
    prelude::*,
};
use std::{collections::HashMap, env};
use walicord_application::{
    Command as ProgramCommand, MessageProcessor, ProcessingOutcome, ScriptStatement,
    SettlementOptimizationError, SettlementResult,
};
use walicord_domain::model::MemberId;
use walicord_infrastructure::{WalicordProgramParser, WalicordSettlementOptimizer};
use walicord_presentation::{SettlementPresenter, SettlementView, VariablesPresenter};

fn load_target_channel_ids() -> Vec<ChannelId> {
    let var = env::var("TARGET_CHANNEL_IDS");
    var.iter()
        .flat_map(|ids_str| {
            ids_str
                .split(',')
                .filter_map(|id_str| id_str.trim().parse::<u64>().ok())
                .map(ChannelId::new)
        })
        .collect()
}

fn format_settlement_error(err: SettlementOptimizationError) -> String {
    match err {
        SettlementOptimizationError::ImbalancedTotal(total) => {
            format!(
                "{} (total: {total})",
                walicord_i18n::SETTLEMENT_CALCULATION_FAILED
            )
        }
        SettlementOptimizationError::NoSolution => {
            walicord_i18n::SETTLEMENT_CALCULATION_FAILED.to_string()
        }
        SettlementOptimizationError::RoundingMismatch => {
            walicord_i18n::SETTLEMENT_CALCULATION_FAILED.to_string()
        }
    }
}

struct Handler<'a> {
    target_channel: Option<ChannelId>,
    message_cache: DashMap<ChannelId, IndexMap<MessageId, Message>>,
    channel_service: DiscordChannelService,
    roster_provider: MemberRosterProvider,
    processor: MessageProcessor<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ReactionState {
    Clear,
    Valid,
    Invalid,
}

enum MessageUpdateResolution {
    Message(Box<Message>),
    Fetch,
}

struct CacheRebuildPlan {
    cache: IndexMap<MessageId, Message>,
    reactions: Vec<(Message, ReactionState)>,
}

impl<'a> Handler<'a> {
    fn new(
        target_channel: Option<ChannelId>,
        channel_service: DiscordChannelService,
        roster_provider: MemberRosterProvider,
        processor: MessageProcessor<'a>,
    ) -> Self {
        Self {
            target_channel,
            message_cache: DashMap::new(),
            channel_service,
            roster_provider,
            processor,
        }
    }

    fn is_target_channel(&self, channel_id: ChannelId) -> bool {
        self.target_channel
            .as_ref()
            .is_some_and(|target| *target == channel_id)
    }

    fn member_ids(result: &SettlementResult) -> impl Iterator<Item = MemberId> + '_ {
        let balances = result.balances.iter().map(|balance| balance.id);
        let transfers = result
            .optimized_transfers
            .iter()
            .flat_map(|transfer| [transfer.from, transfer.to]);
        let settle_up_ids = result.settle_up.iter().flat_map(|settle_up| {
            let immediate = settle_up
                .immediate_transfers
                .iter()
                .flat_map(|transfer| [transfer.from, transfer.to]);
            immediate.chain(settle_up.settle_members.iter().copied())
        });

        balances.chain(transfers).chain(settle_up_ids)
    }

    async fn ensure_member_directory<'b, I>(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
        member_ids: I,
        member_directory: &'b mut Option<HashMap<MemberId, String>>,
    ) -> Result<&'b HashMap<MemberId, String>, ChannelError>
    where
        I: IntoIterator<Item = MemberId>,
    {
        let channel = channel_id
            .to_channel(&ctx.http)
            .await
            .map_err(|e| ChannelError::Request(format!("{e:?}")))?;
        let Some(guild_channel) = channel.guild() else {
            return Err(ChannelError::NotGuildChannel);
        };
        let guild_id = guild_channel.guild_id;

        // Ensure roster is loaded (warm_up if needed)
        if let Err(e) = self.roster_provider.warm_up(ctx, channel_id).await {
            tracing::warn!(
                "Failed to warm up roster for channel {}: {:?}",
                channel_id,
                e
            );
        }

        let display_names = self
            .roster_provider
            .display_names_for_guild(guild_id, member_ids);
        let directory = member_directory.get_or_insert_with(HashMap::new);
        directory.extend(display_names);

        Ok(directory)
    }

    async fn reply(&self, ctx: &Context, msg: &Message, content: impl Into<String>) {
        let content = content.into();
        if let Err(e) = msg.reply(&ctx.http, content).await {
            tracing::error!("Failed to send message: {:?}", e);
        }
    }

    async fn reply_with_settlement(&self, ctx: &Context, msg: &Message, response: SettlementView) {
        use serenity::{all::CreateAttachment, builder::CreateMessage};

        let message_builder = CreateMessage::new().reference_message(msg);

        let attachment = svg_to_png(&response.combined_svg)
            .map(|png| CreateAttachment::bytes(png, "settlement.png"));

        let message_builder = if let Some(a) = attachment {
            message_builder.add_file(a)
        } else {
            message_builder
        };

        if let Err(e) = msg
            .channel_id
            .send_message(&ctx.http, message_builder)
            .await
        {
            tracing::error!("Failed to send message with attachments: {:?}", e);
        }
    }

    async fn react(&self, ctx: &Context, msg: &Message, emoji: char) {
        if let Err(e) = msg.react(ctx, emoji).await {
            tracing::error!("Failed to add reaction: {:?}", e);
        }
    }

    async fn delete_reaction(&self, ctx: &Context, msg: &Message, emoji: char) {
        if let Err(e) = msg
            .delete_reaction(&ctx.http, None, ReactionType::Unicode(emoji.to_string()))
            .await
        {
            tracing::warn!("Failed to remove reaction: {:?}", e);
        }
    }

    fn update_cached_reaction_add(
        message: &mut Message,
        emoji: &ReactionType,
        is_me: bool,
        is_burst: bool,
    ) -> bool {
        let Some(entry) = message
            .reactions
            .iter_mut()
            .find(|reaction| reaction.reaction_type == *emoji)
        else {
            return false;
        };

        entry.count = entry.count.saturating_add(1);
        if is_burst {
            entry.count_details.burst = entry.count_details.burst.saturating_add(1);
            if is_me {
                entry.me_burst = true;
            }
        } else {
            entry.count_details.normal = entry.count_details.normal.saturating_add(1);
        }
        if is_me {
            entry.me = true;
        }

        true
    }

    fn update_cached_reaction_remove(
        message: &mut Message,
        emoji: &ReactionType,
        is_me: bool,
        is_burst: bool,
    ) -> bool {
        let Some(position) = message
            .reactions
            .iter()
            .position(|reaction| reaction.reaction_type == *emoji)
        else {
            return false;
        };

        let entry = &mut message.reactions[position];
        if entry.count > 0 {
            entry.count -= 1;
        }
        if is_burst {
            entry.count_details.burst = entry.count_details.burst.saturating_sub(1);
            if is_me {
                entry.me_burst = false;
            }
        } else {
            entry.count_details.normal = entry.count_details.normal.saturating_sub(1);
        }
        if is_me {
            entry.me = false;
        }

        if entry.count == 0 {
            message.reactions.remove(position);
        }

        true
    }

    async fn update_reaction_state(&self, ctx: &Context, msg: &Message, desired: ReactionState) {
        match desired {
            ReactionState::Valid => {
                self.delete_reaction(ctx, msg, '❎').await;
                self.react(ctx, msg, '✅').await;
            }
            ReactionState::Invalid => {
                self.delete_reaction(ctx, msg, '✅').await;
                self.react(ctx, msg, '❎').await;
            }
            ReactionState::Clear => {
                self.delete_reaction(ctx, msg, '✅').await;
                self.delete_reaction(ctx, msg, '❎').await;
            }
        }
    }

    async fn rebuild_channel_cache(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
        updated_message: Option<Message>,
    ) {
        let mut messages: Vec<Message> = match self.message_cache.get(&channel_id) {
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
            self.update_reaction_state(ctx, &message, desired_reaction)
                .await;
        }

        if plan.cache.is_empty() {
            self.message_cache.remove(&channel_id);
        } else {
            self.message_cache.insert(channel_id, plan.cache);
        }
    }

    async fn process_program_message(&self, ctx: &Context, msg: &Message) -> bool {
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
        let mut member_directory: Option<HashMap<MemberId, String>> = None;
        let author_id = MemberId(msg.author.id.get());

        let messages_guard = self.message_cache.get(&msg.channel_id);

        let next_line_offset = match &messages_guard {
            Some(messages) => next_line_offset(messages.iter().map(|(_, m)| m)),
            None => 0,
        };

        let parse_outcome = match &messages_guard {
            Some(messages) => {
                let message_iter = messages
                    .iter()
                    .map(|(_, m)| (m.content.as_str(), Some(MemberId(m.author.id.get()))))
                    .chain(std::iter::once((msg.content.as_str(), Some(author_id))));
                self.processor
                    .parse_program_sequence(&member_ids, message_iter)
            }
            None => self.processor.parse_program_sequence(
                &member_ids,
                std::iter::once((msg.content.as_str(), Some(author_id))),
            ),
        };

        match parse_outcome {
            ProcessingOutcome::Success(program) => {
                let mut commands: Vec<(usize, ProgramCommand)> = Vec::new();
                let mut has_effect_statement = false;
                let mut should_store = false;

                {
                    let statements = program.statements();
                    for (stmt_index, stmt) in statements
                        .iter()
                        .enumerate()
                        .filter(|(_, stmt)| stmt.line > next_line_offset)
                    {
                        should_store = true;
                        match &stmt.statement {
                            ScriptStatement::Domain(_) => {
                                has_effect_statement = true;
                            }
                            ScriptStatement::Command(command) => {
                                if matches!(command, ProgramCommand::SettleUp(_)) {
                                    has_effect_statement = true;
                                }
                                commands.push((stmt_index, command.clone()));
                            }
                        }
                    }
                }

                if has_effect_statement {
                    self.react(ctx, msg, '✅').await;
                }

                for (stmt_index, command) in commands {
                    match command {
                        ProgramCommand::Variables => {
                            let reply = VariablesPresenter::render_for_prefix_with_members(
                                &program,
                                stmt_index,
                                &member_ids,
                            );
                            self.reply(ctx, msg, reply).await;
                        }
                        ProgramCommand::Review => {
                            match self
                                .processor
                                .build_settlement_result_for_prefix(&program, stmt_index)
                            {
                                Ok(response) => {
                                    let member_ids: Vec<MemberId> =
                                        Self::member_ids(&response).collect();
                                    let directory = match self
                                        .ensure_member_directory(
                                            ctx,
                                            msg.channel_id,
                                            member_ids.iter().copied(),
                                            &mut member_directory,
                                        )
                                        .await
                                    {
                                        Ok(directory) => directory,
                                        Err(e) => {
                                            tracing::warn!(
                                                "Failed to fetch member directory for channel {} ({} member IDs): {:?}",
                                                msg.channel_id,
                                                member_ids.len(),
                                                e
                                            );
                                            member_directory.get_or_insert_with(HashMap::new)
                                        }
                                    };
                                    let view = SettlementPresenter::render_with_members(
                                        &response, directory,
                                    );
                                    self.reply_with_settlement(ctx, msg, view).await
                                }
                                Err(err) => {
                                    self.reply(ctx, msg, format_settlement_error(err)).await
                                }
                            }
                        }
                        ProgramCommand::SettleUp(_) => {
                            match self
                                .processor
                                .build_settlement_result_for_prefix(&program, stmt_index)
                            {
                                Ok(response) => {
                                    let member_ids = Self::member_ids(&response);
                                    let directory = match self
                                        .ensure_member_directory(
                                            ctx,
                                            msg.channel_id,
                                            member_ids,
                                            &mut member_directory,
                                        )
                                        .await
                                    {
                                        Ok(directory) => directory,
                                        Err(e) => {
                                            tracing::warn!(
                                                "Failed to fetch member directory for channel {}: {:?}",
                                                msg.channel_id,
                                                e
                                            );
                                            member_directory.get_or_insert_with(HashMap::new)
                                        }
                                    };
                                    let view = SettlementPresenter::render_with_members(
                                        &response, directory,
                                    );
                                    self.reply_with_settlement(ctx, msg, view).await
                                }
                                Err(err) => {
                                    self.reply(ctx, msg, format_settlement_error(err)).await
                                }
                            }
                        }
                    }
                }

                should_store
            }
            ProcessingOutcome::FailedToEvaluateGroup { name, line } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!(
                        "{} {} (line {line})",
                        msg.author.mention(),
                        walicord_i18n::failed_to_evaluate_group(name)
                    ),
                )
                .await;
                false
            }
            ProcessingOutcome::UndefinedGroup { name, line } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!(
                        "{} {} (line {line})",
                        msg.author.mention(),
                        walicord_i18n::undefined_group(name)
                    ),
                )
                .await;
                false
            }
            ProcessingOutcome::UndefinedMember { id, line } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!(
                        "{} {} (line {line})",
                        msg.author.mention(),
                        walicord_i18n::undefined_member(id)
                    ),
                )
                .await;
                false
            }
            ProcessingOutcome::SyntaxError { line, detail } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!(
                        "{} {}",
                        msg.author.mention(),
                        walicord_i18n::syntax_error(line, detail)
                    ),
                )
                .await;
                false
            }
            ProcessingOutcome::MissingContextForImplicitPayment { line } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!(
                        "{} {}",
                        msg.author.mention(),
                        walicord_i18n::implicit_payer_missing(line)
                    ),
                )
                .await;
                false
            }
            ProcessingOutcome::InvalidAmountExpression { line, detail } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!(
                        "{} {}",
                        msg.author.mention(),
                        walicord_i18n::invalid_amount_expression(line, detail)
                    ),
                )
                .await;
                false
            }
        }
    }
}

fn line_count_with_prior_newline(content: &str, prior_ended_with_newline: bool) -> usize {
    content.lines().count() + if prior_ended_with_newline { 1 } else { 0 }
}

fn next_line_offset<'a, I>(messages: I) -> usize
where
    I: IntoIterator<Item = &'a Message>,
{
    let mut line_count = 0usize;
    let mut ends_with_newline = false;
    let mut has_any = false;

    for message in messages {
        let content = message.content.as_str();
        if has_any {
            line_count += line_count_with_prior_newline(content, ends_with_newline);
        } else {
            line_count = content.lines().count();
        }
        ends_with_newline = content.is_empty() || content.ends_with('\n');
        has_any = true;
    }

    if has_any {
        line_count + if ends_with_newline { 1 } else { 0 }
    } else {
        0
    }
}

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
                ProcessingOutcome::Success(program) => {
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
                                if matches!(command, ProgramCommand::SettleUp(_)) {
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
impl EventHandler for Handler<'_> {
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

        if !self.is_target_channel(msg.channel_id) {
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

        let Some(channel_id) = self.target_channel else {
            tracing::warn!("Warning: TARGET_CHANNEL_ID is not set");
            return;
        };

        tracing::info!("Target channel ID: {}", channel_id);
        tracing::info!("Building message cache...");

        match self
            .channel_service
            .fetch_all_messages(&ctx, channel_id)
            .await
        {
            Ok(messages) => {
                self.message_cache.insert(channel_id, messages);
                tracing::info!("Message cache built successfully.");
            }
            Err(e) => {
                tracing::error!("Failed to fetch initial messages: {:?}", e);
            }
        }

        if let Err(e) = self.roster_provider.warm_up(&ctx, channel_id).await {
            tracing::warn!(
                "Failed to build member cache for channel {}: {:?}",
                channel_id,
                e
            );
        } else {
            tracing::info!("Member cache built successfully.");
        }
    }

    async fn guild_member_addition(&self, _ctx: Context, new_member: Member) {
        let guild_id = new_member.guild_id;
        let member_info = to_member_info(&new_member);
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
        let member_info = to_member_info(&new_member);
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
        if !self.is_target_channel(channel_id) {
            return;
        }

        if let Some(mut messages) = self.message_cache.get_mut(&channel_id) {
            let should_remove_channel = {
                messages.shift_remove(&deleted_message_id);
                messages.is_empty()
            };

            if should_remove_channel {
                drop(messages);
                self.message_cache.remove(&channel_id);
            }
        }
    }

    async fn message_delete_bulk(
        &self,
        _ctx: Context,
        channel_id: ChannelId,
        deleted_message_ids: Vec<MessageId>,
        _guild_id: Option<GuildId>,
    ) {
        if !self.is_target_channel(channel_id) {
            return;
        }

        if let Some(mut messages) = self.message_cache.get_mut(&channel_id) {
            let should_remove_channel = {
                for message_id in deleted_message_ids {
                    messages.shift_remove(&message_id);
                }
                messages.is_empty()
            };

            if should_remove_channel {
                drop(messages);
                self.message_cache.remove(&channel_id);
            }
        }
    }

    async fn message_update(
        &self,
        ctx: Context,
        old_if_available: Option<Message>,
        new: Option<Message>,
        event: MessageUpdateEvent,
    ) {
        if !self.is_target_channel(event.channel_id) {
            return;
        }

        let cached_message = self
            .message_cache
            .get(&event.channel_id)
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
        if !self.is_target_channel(add_reaction.channel_id) {
            return;
        }

        let current_user_id = ctx.cache.current_user().id;
        let is_me = add_reaction.user_id == Some(current_user_id);
        if let Some(mut messages) = self.message_cache.get_mut(&add_reaction.channel_id)
            && let Some(message) = messages.get_mut(&add_reaction.message_id)
        {
            let updated = Self::update_cached_reaction_add(
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
            && let Some(mut messages) = self.message_cache.get_mut(&add_reaction.channel_id)
        {
            messages.insert(message.id, message);
        }
    }

    async fn reaction_remove(&self, ctx: Context, removed_reaction: Reaction) {
        if !self.is_target_channel(removed_reaction.channel_id) {
            return;
        }

        let current_user_id = ctx.cache.current_user().id;
        let is_me = removed_reaction.user_id == Some(current_user_id);
        if let Some(mut messages) = self.message_cache.get_mut(&removed_reaction.channel_id)
            && let Some(message) = messages.get_mut(&removed_reaction.message_id)
        {
            let updated = Self::update_cached_reaction_remove(
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
            && let Some(mut messages) = self.message_cache.get_mut(&removed_reaction.channel_id)
        {
            messages.insert(message.id, message);
        }
    }
}

#[tokio::main]
async fn main() {
    let _ = dotenvy::dotenv();
    tracing_subscriber::fmt::init();

    let token = env::var("DISCORD_TOKEN").expect("DISCORD_TOKEN is not set");
    let intents = GatewayIntents::GUILD_MESSAGES
        | GatewayIntents::MESSAGE_CONTENT
        | GatewayIntents::GUILDS
        | GatewayIntents::GUILD_MEMBERS
        | GatewayIntents::GUILD_MESSAGE_REACTIONS;

    let target_channel = load_target_channel_ids();
    let processor = MessageProcessor::new(&WalicordProgramParser, &WalicordSettlementOptimizer);

    let handles: Vec<tokio::task::JoinHandle<()>> = target_channel
        .into_iter()
        .map(|channel_id| {
            let roster_provider = MemberRosterProvider::new(DiscordChannelService);
            let handler = Handler::new(
                Some(channel_id),
                DiscordChannelService,
                roster_provider,
                processor,
            );

            tokio::spawn({
                let client_builder = Client::builder(&token, intents);
                async move {
                    let mut client = client_builder
                        .event_handler(handler)
                        .await
                        .expect("Failed to create client");

                    if let Err(why) = client.start().await {
                        tracing::error!("Client error: {:?}", why);
                    }
                }
            })
        })
        .collect();

    for handle in handles {
        let _ = handle.await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use serenity::model::id::UserId;
    use walicord_application::{
        ProgramParseError, ProgramParser, Script, ScriptStatementWithLine, SettlementOptimizer,
    };
    use walicord_domain::{MemberSetExpr, MemberSetOp, Money, Payment, Statement, Transfer};

    struct StubParser;

    impl ProgramParser for StubParser {
        fn parse<'a>(
            &self,
            _member_ids: &'a [MemberId],
            content: &'a str,
            _author_id: Option<MemberId>,
        ) -> Result<Script<'a>, ProgramParseError<'a>> {
            if content.starts_with("MULTI") {
                let payment = Payment {
                    amount: Money::try_from(100).expect("amount should fit in i64"),
                    payer: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(1))]),
                    payee: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(2))]),
                };
                return Ok(Script::new(
                    &[],
                    vec![ScriptStatementWithLine {
                        line: 2,
                        statement: ScriptStatement::Domain(Statement::Payment(payment)),
                    }],
                ));
            }

            match content {
                "PAY" => {
                    let payment = Payment {
                        amount: Money::try_from(100).expect("amount should fit in i64"),
                        payer: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(1))]),
                        payee: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(2))]),
                    };
                    Ok(Script::new(
                        &[],
                        vec![ScriptStatementWithLine {
                            line: 1,
                            statement: ScriptStatement::Domain(Statement::Payment(payment)),
                        }],
                    ))
                }
                "SETTLE" => Ok(Script::new(
                    &[],
                    vec![ScriptStatementWithLine {
                        line: 1,
                        statement: ScriptStatement::Command(ProgramCommand::SettleUp(
                            MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(1))]),
                        )),
                    }],
                )),
                "REVIEW" => Ok(Script::new(
                    &[],
                    vec![ScriptStatementWithLine {
                        line: 1,
                        statement: ScriptStatement::Command(ProgramCommand::Review),
                    }],
                )),
                "EMPTY" => Ok(Script::new(&[], Vec::new())),
                "BAD" => Err(ProgramParseError::SyntaxError {
                    line: 1,
                    detail: "bad".to_string(),
                }),
                _ => Ok(Script::new(&[], Vec::new())),
            }
        }
    }

    struct NoopOptimizer;

    impl SettlementOptimizer for NoopOptimizer {
        fn optimize(
            &self,
            _balances: &[walicord_application::PersonBalance],
        ) -> Result<Vec<Transfer>, walicord_application::SettlementOptimizationError> {
            Ok(Vec::new())
        }
    }

    fn make_message(id: u64, channel_id: u64, author_id: u64, content: &str) -> Message {
        let mut message = Message::default();
        message.id = MessageId::new(id);
        message.channel_id = ChannelId::new(channel_id);
        message.author = User::default();
        message.author.id = UserId::new(author_id);
        message.content = content.to_string();
        message
    }

    fn make_reaction(
        emoji: &str,
        count: u64,
        normal: u64,
        burst: u64,
        me: bool,
        me_burst: bool,
    ) -> serenity::model::channel::MessageReaction {
        serde_json::from_value(json!({
            "count": count,
            "count_details": { "normal": normal, "burst": burst },
            "me": me,
            "me_burst": me_burst,
            "emoji": { "id": null, "name": emoji },
            "burst_colors": []
        }))
        .expect("MessageReaction should deserialize")
    }

    fn message_update_event(
        channel_id: u64,
        message_id: u64,
        content: Option<&str>,
    ) -> MessageUpdateEvent {
        let mut value = json!({
            "id": message_id.to_string(),
            "channel_id": channel_id.to_string()
        });
        if let Some(content) = content {
            value["content"] = json!(content);
        }
        serde_json::from_value(value).expect("MessageUpdateEvent should deserialize")
    }

    #[test]
    fn plan_cache_rebuild_reacts_and_caches_messages() {
        let processor = MessageProcessor::new(&StubParser, &NoopOptimizer);
        let members: [MemberId; 0] = [];
        let messages = vec![
            make_message(3, 1, 1, "PAY"),
            make_message(1, 1, 1, "PAY"),
            make_message(2, 1, 1, "BAD"),
        ];

        let plan = plan_cache_rebuild(&processor, &members, messages);
        let reaction_states: Vec<(u64, ReactionState)> = plan
            .reactions
            .iter()
            .map(|(message, state)| (message.id.get(), *state))
            .collect();

        assert_eq!(
            reaction_states,
            vec![
                (1, ReactionState::Valid),
                (2, ReactionState::Invalid),
                (3, ReactionState::Valid),
            ]
        );
        assert_eq!(
            plan.cache.keys().map(|id| id.get()).collect::<Vec<_>>(),
            vec![1, 3]
        );
    }

    #[test]
    fn plan_cache_rebuild_handles_multiline_offsets() {
        let processor = MessageProcessor::new(&StubParser, &NoopOptimizer);
        let members: [MemberId; 0] = [];
        let messages = vec![
            make_message(1, 1, 1, "MULTI\nCOMMENT"),
            make_message(2, 1, 1, "PAY"),
        ];

        let plan = plan_cache_rebuild(&processor, &members, messages);
        let reaction_states: Vec<(u64, ReactionState)> = plan
            .reactions
            .iter()
            .map(|(message, state)| (message.id.get(), *state))
            .collect();

        assert_eq!(
            reaction_states,
            vec![(1, ReactionState::Valid), (2, ReactionState::Valid)]
        );
        assert_eq!(
            plan.cache.keys().map(|id| id.get()).collect::<Vec<_>>(),
            vec![1, 2]
        );
    }

    #[test]
    fn update_cached_reaction_add_updates_existing_entry() {
        let mut message = Message::default();
        message.reactions = vec![make_reaction("✅", 1, 1, 0, false, false)];

        let updated = Handler::update_cached_reaction_add(
            &mut message,
            &ReactionType::Unicode("✅".to_string()),
            true,
            false,
        );

        assert!(updated);
        assert_eq!(message.reactions.len(), 1);
        let reaction = &message.reactions[0];
        assert_eq!(reaction.count, 2);
        assert!(reaction.me);
        assert_eq!(reaction.count_details.normal, 2);
    }

    #[test]
    fn update_cached_reaction_add_returns_false_when_missing() {
        let mut message = Message::default();

        let updated = Handler::update_cached_reaction_add(
            &mut message,
            &ReactionType::Unicode("✅".to_string()),
            true,
            false,
        );

        assert!(!updated);
        assert!(message.reactions.is_empty());
    }

    #[test]
    fn update_cached_reaction_remove_deletes_entry_when_count_reaches_zero() {
        let mut message = Message::default();
        message.reactions = vec![make_reaction("✅", 1, 1, 0, true, false)];

        let updated = Handler::update_cached_reaction_remove(
            &mut message,
            &ReactionType::Unicode("✅".to_string()),
            true,
            false,
        );

        assert!(updated);
        assert!(message.reactions.is_empty());
    }

    #[test]
    fn update_cached_reaction_remove_returns_false_when_missing() {
        let mut message = Message::default();

        let updated = Handler::update_cached_reaction_remove(
            &mut message,
            &ReactionType::Unicode("✅".to_string()),
            true,
            false,
        );

        assert!(!updated);
        assert!(message.reactions.is_empty());
    }

    #[test]
    fn resolve_message_update_prefers_new_then_old_then_cached() {
        let event = message_update_event(1, 10, Some("edited"));
        let new_message = make_message(10, 1, 1, "new");
        let old_message = make_message(10, 1, 1, "old");
        let cached_message = make_message(10, 1, 1, "cached");

        let resolved = resolve_message_update(
            &event,
            Some(new_message.clone()),
            Some(old_message.clone()),
            Some(cached_message.clone()),
        );
        let MessageUpdateResolution::Message(message) = resolved else {
            panic!("expected message resolution");
        };
        assert_eq!(message.content, "new");

        let resolved = resolve_message_update(
            &event,
            None,
            Some(old_message.clone()),
            Some(cached_message.clone()),
        );
        let MessageUpdateResolution::Message(message) = resolved else {
            panic!("expected message resolution");
        };
        assert_eq!(message.content, "edited");

        let resolved = resolve_message_update(&event, None, None, Some(cached_message));
        let MessageUpdateResolution::Message(message) = resolved else {
            panic!("expected message resolution");
        };
        assert_eq!(message.content, "edited");
    }

    #[test]
    fn resolve_message_update_falls_back_to_fetch_when_empty() {
        let event = message_update_event(1, 10, None);

        let resolved = resolve_message_update(&event, None, None, None);
        assert!(matches!(resolved, MessageUpdateResolution::Fetch));
    }
}
