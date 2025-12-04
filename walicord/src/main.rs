#![warn(clippy::uninlined_format_args)]

mod infrastructure;

use dashmap::DashMap;
use indexmap::IndexMap;
use infrastructure::{
    discord::{ChannelError, DiscordChannelService},
    svg_renderer::svg_to_png,
};
use serenity::{
    all::MessageId,
    async_trait,
    model::{
        channel::{Message, ReactionType},
        gateway::Ready,
        id::{ChannelId, GuildId},
    },
    prelude::*,
};
use std::env;
use walicord_core::{
    SettlementResponse,
    application::{MessageProcessor, ProcessingOutcome},
    domain::model::{Command as ProgramCommand, Statement},
    infrastructure::parser::WalicordProgramParser,
};
use walicord_parser::extract_members_from_topic;

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

const MISSING_MEMBERS_MESSAGE: &str =
    "チャンネルのtopicに `MEMBERS := ...` の宣言が見つかりません。";

enum MembersError {
    Channel(ChannelError),
    MissingDeclaration,
}

struct Handler<'a> {
    target_channel: Option<ChannelId>,
    message_cache: DashMap<ChannelId, IndexMap<MessageId, Message>>,
    channel_service: DiscordChannelService,
    processor: MessageProcessor<'a>,
}

impl<'a> Handler<'a> {
    fn new(
        target_channel: Option<ChannelId>,
        channel_service: DiscordChannelService,
        processor: MessageProcessor<'a>,
    ) -> Self {
        Self {
            target_channel,
            message_cache: DashMap::new(),
            channel_service,
            processor,
        }
    }

    fn is_target_channel(&self, channel_id: ChannelId) -> bool {
        self.target_channel
            .as_ref()
            .is_some_and(|target| *target == channel_id)
    }

    fn get_combined_content(&self, channel_id: &ChannelId) -> String {
        self.message_cache
            .get(channel_id)
            .map(|messages| {
                messages
                    .iter()
                    .map(|(_, m)| m.content.as_str())
                    .collect::<Vec<_>>()
                    .join("\n")
            })
            .unwrap_or_default()
    }

    async fn fetch_topic(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> Result<String, MembersError> {
        let channel = self
            .channel_service
            .fetch_guild_channel(ctx, channel_id)
            .await
            .map_err(MembersError::Channel)?;

        channel.topic.ok_or(MembersError::MissingDeclaration)
    }

    async fn reply(&self, ctx: &Context, msg: &Message, content: impl Into<String>) {
        let content = content.into();
        if let Err(e) = msg.reply(&ctx.http, content).await {
            tracing::error!("Failed to send message: {:?}", e);
        }
    }

    async fn reply_with_settlement(
        &self,
        ctx: &Context,
        msg: &Message,
        response: SettlementResponse,
    ) {
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
        let topic = match self.fetch_topic(ctx, msg.channel_id).await {
            Ok(topic) => topic,
            Err(MembersError::Channel(err)) => {
                tracing::error!("Failed to fetch channel info: {}", err);
                return false;
            }
            Err(MembersError::MissingDeclaration) => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!("{} エラー: {MISSING_MEMBERS_MESSAGE}", msg.author.mention()),
                )
                .await;
                return false;
            }
        };

        let members = match extract_members_from_topic(&topic) {
            Ok(members) => members,
            Err(_) => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!("{} エラー: {MISSING_MEMBERS_MESSAGE}", msg.author.mention()),
                )
                .await;
                return false;
            }
        };
        let existing_content = self.get_combined_content(&msg.channel_id);

        let previous_statement_count = if existing_content.is_empty() {
            0
        } else {
            match self.processor.parse_program(&members, &existing_content) {
                ProcessingOutcome::Success(program) => program.statements.len(),
                ProcessingOutcome::MissingMembersDeclaration
                | ProcessingOutcome::UndefinedMember { .. }
                | ProcessingOutcome::SyntaxError { .. } => 0,
            }
        };

        let mut content = existing_content;
        if !content.is_empty() {
            content.push('\n');
        }
        content.push_str(&msg.content);

        match self.processor.parse_program(&members, &content) {
            ProcessingOutcome::Success(program) => {
                let mut commands: Vec<(usize, ProgramCommand)> = Vec::new();
                let mut has_effect_statement = false;
                let mut should_store = false;

                {
                    let new_statements = if program.statements.len() >= previous_statement_count {
                        &program.statements[previous_statement_count..]
                    } else {
                        &[]
                    };

                    if !new_statements.is_empty() {
                        should_store = true;
                    }

                    for (offset, stmt) in new_statements.iter().enumerate() {
                        let stmt_index = previous_statement_count + offset;
                        match stmt {
                            Statement::Declaration(_) | Statement::Payment(_) => {
                                has_effect_statement = true;
                            }
                            Statement::Command(command) => {
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
                            let reply = self
                                .processor
                                .format_variables_response_for_prefix(&program, stmt_index);
                            self.reply(ctx, msg, reply).await;
                        }
                        ProgramCommand::Evaluate => {
                            match self
                                .processor
                                .format_settlement_response_for_prefix(&program, stmt_index)
                            {
                                Ok(response) => {
                                    self.reply_with_settlement(ctx, msg, response).await
                                }
                                Err(err_msg) => self.reply(ctx, msg, err_msg).await,
                            }
                        }
                        ProgramCommand::SettleUp(_) => {
                            match self
                                .processor
                                .format_settlement_response_for_prefix(&program, stmt_index)
                            {
                                Ok(response) => {
                                    self.reply_with_settlement(ctx, msg, response).await
                                }
                                Err(err_msg) => self.reply(ctx, msg, err_msg).await,
                            }
                        }
                    }
                }

                should_store
            }
            ProcessingOutcome::MissingMembersDeclaration => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!("{} エラー: {MISSING_MEMBERS_MESSAGE}", msg.author.mention()),
                )
                .await;
                false
            }
            ProcessingOutcome::UndefinedMember { name, line } => {
                self.react(ctx, msg, '❎').await;
                let error_msg = format!(
                    "エラー: 行 {line} に未定義のメンバー「{name}」が使用されています。\nチャンネルtopicのMEMBERS宣言で定義してください。\n現在のメンバー: {members:?}"
                );
                self.reply(ctx, msg, format!("{} {error_msg}", msg.author.mention()))
                    .await;
                false
            }
            ProcessingOutcome::SyntaxError { message } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!("{} 構文エラー: {message}", msg.author.mention()),
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
    let intents =
        GatewayIntents::GUILD_MESSAGES | GatewayIntents::MESSAGE_CONTENT | GatewayIntents::GUILDS;

    let target_channel = load_target_channel_ids();
    let processor = MessageProcessor::new(&WalicordProgramParser);

    let handles: Vec<tokio::task::JoinHandle<()>> = target_channel
        .into_iter()
        .map(|channel_id| {
            let handler = Handler::new(Some(channel_id), DiscordChannelService, processor);

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
