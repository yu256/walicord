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
        channel::{Message, ReactionType},
        event::GuildMemberUpdateEvent,
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
    }
}

struct Handler<'a> {
    target_channel: Option<ChannelId>,
    message_cache: DashMap<ChannelId, IndexMap<MessageId, Message>>,
    channel_service: DiscordChannelService,
    roster_provider: MemberRosterProvider,
    processor: MessageProcessor<'a>,
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

        let attachments = {
            fn svg_to_attachment(
                filename: &str,
            ) -> impl Fn(&String) -> Option<CreateAttachment> + '_ {
                move |svg| svg_to_png(svg).map(|png| CreateAttachment::bytes(png, filename))
            }
            std::iter::once(&response.balance_table_svg)
                .filter_map(svg_to_attachment("balance.png"))
                .chain(
                    response
                        .transfer_table_svg
                        .iter()
                        .filter_map(svg_to_attachment("transfers.png")),
                )
        };

        let message_builder = attachments.fold(message_builder, |m, a| m.add_file(a));

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

        let previous_statement_count = match &messages_guard {
            Some(messages) => {
                let message_iter = messages
                    .iter()
                    .map(|(_, m)| (m.content.as_str(), Some(MemberId(m.author.id.get()))));
                match self
                    .processor
                    .parse_program_sequence(&member_ids, message_iter)
                {
                    ProcessingOutcome::Success(program) => program.statements().len(),
                    ProcessingOutcome::FailedToEvaluateGroup { .. }
                    | ProcessingOutcome::UndefinedGroup { .. }
                    | ProcessingOutcome::UndefinedMember { .. }
                    | ProcessingOutcome::SyntaxError { .. }
                    | ProcessingOutcome::ImplicitPayerWithoutAuthor { .. } => 0,
                }
            }
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
                    let new_statements = if statements.len() >= previous_statement_count {
                        &statements[previous_statement_count..]
                    } else {
                        &[]
                    };

                    if !new_statements.is_empty() {
                        should_store = true;
                    }

                    for (offset, stmt) in new_statements.iter().enumerate() {
                        let stmt_index = previous_statement_count + offset;
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
                        ProgramCommand::Evaluate => {
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
            ProcessingOutcome::SyntaxError { message } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!("{} Syntax error: {message}", msg.author.mention()),
                )
                .await;
                false
            }
            ProcessingOutcome::ImplicitPayerWithoutAuthor { line } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!(
                        "{} Implicit payer syntax requires message author context at line {line}.",
                        msg.author.mention()
                    ),
                )
                .await;
                false
            }
        }
    }
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
}

#[tokio::main]
async fn main() {
    let _ = dotenvy::dotenv();
    tracing_subscriber::fmt::init();

    let token = env::var("DISCORD_TOKEN").expect("DISCORD_TOKEN is not set");
    let intents = GatewayIntents::GUILD_MESSAGES
        | GatewayIntents::MESSAGE_CONTENT
        | GatewayIntents::GUILDS
        | GatewayIntents::GUILD_MEMBERS;

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
