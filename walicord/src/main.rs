#![warn(clippy::uninlined_format_args)]

mod application;
mod domain;
mod infrastructure;

use application::{MessageProcessor, ProcessingOutcome};
use dashmap::DashMap;
use domain::Program;
use indexmap::IndexMap;
use infrastructure::{
    discord::{ChannelError, DiscordChannelService},
    parser::WalicordProgramParser,
};
use serenity::{
    all::MessageId,
    async_trait,
    model::{
        channel::{Message, ReactionType},
        gateway::Ready,
        id::ChannelId,
    },
    prelude::*,
};
use std::env;
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

const VARIABLES_COMMAND: &str = "!variables";
const EVALUATE_COMMAND: &str = "!evaluate";
const MISSING_MEMBERS_MESSAGE: &str =
    "チャンネルのtopicに `MEMBERS := ...` の宣言が見つかりません。";

enum CommandKind {
    Variables,
    Evaluate,
}

impl CommandKind {
    fn from_message(content: &str) -> Option<Self> {
        match content.split_whitespace().next()? {
            VARIABLES_COMMAND => Some(Self::Variables),
            EVALUATE_COMMAND => Some(Self::Evaluate),
            _ => None,
        }
    }

    fn context_label(&self) -> &'static str {
        match self {
            Self::Variables => "変数の解析",
            Self::Evaluate => "割り勘計算",
        }
    }

    fn success_reply<'a>(
        &self,
        processor: MessageProcessor<'a>,
        program: Program<'a>,
    ) -> Result<String, String> {
        match self {
            Self::Variables => Ok(processor.format_variables_response(program)),
            Self::Evaluate => processor.format_settlement_response(program),
        }
    }

    fn undefined_member_error(&self, line: usize, name: &str, members: &[&str]) -> String {
        format!(
            "{}中にエラーが発生しました: 行 {line} に未定義のメンバー「{name}」が使用されています。\n現在のメンバー: {members:?}",
            self.context_label()
        )
    }

    fn syntax_error_message(&self, message: &str) -> String {
        format!(
            "{}中にエラーが発生しました: {message}",
            self.context_label()
        )
    }
}

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

    async fn react(&self, ctx: &Context, msg: &Message, emoji: char) {
        if let Err(e) = msg.react(ctx, emoji).await {
            tracing::error!("Failed to add reaction: {:?}", e);
        }
    }

    async fn handle_command(&self, ctx: &Context, msg: &Message, command: CommandKind) {
        let topic = match self.fetch_topic(ctx, msg.channel_id).await {
            Ok(topic) => topic,
            Err(MembersError::Channel(err)) => {
                self.reply(
                    ctx,
                    msg,
                    format!("チャンネル情報の取得に失敗しました: {err}"),
                )
                .await;
                return;
            }
            Err(MembersError::MissingDeclaration) => {
                self.reply(ctx, msg, MISSING_MEMBERS_MESSAGE).await;
                return;
            }
        };

        let members = match extract_members_from_topic(&topic) {
            Ok(members) => members,
            Err(_) => {
                self.reply(ctx, msg, MISSING_MEMBERS_MESSAGE).await;
                return;
            }
        };

        let combined_content = self.get_combined_content(&msg.channel_id);

        match self.processor.parse_program(&members, &combined_content) {
            ProcessingOutcome::Success(program) => {
                match command.success_reply(self.processor, program) {
                    Ok(reply) => self.reply(ctx, msg, reply).await,
                    Err(err_msg) => self.reply(ctx, msg, err_msg).await,
                }
            }
            ProcessingOutcome::MissingMembersDeclaration => {
                self.reply(ctx, msg, MISSING_MEMBERS_MESSAGE).await;
            }
            ProcessingOutcome::UndefinedMember { name, line } => {
                self.reply(
                    ctx,
                    msg,
                    command.undefined_member_error(line, &name, &members),
                )
                .await;
            }
            ProcessingOutcome::SyntaxError { message } => {
                self.reply(ctx, msg, command.syntax_error_message(&message))
                    .await;
            }
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
                self
                    .reply(
                        ctx,
                        msg,
                        format!(
                            "{} エラー: チャンネルのtopicに `MEMBERS := メンバー1, メンバー2, ...` の宣言が必要です。",
                            msg.author.mention()
                        ),
                    )
                    .await;
                return false;
            }
        };

        let members = match extract_members_from_topic(&topic) {
            Ok(members) => members,
            Err(_) => {
                self.react(ctx, msg, '❎').await;
                self
                    .reply(
                        ctx,
                        msg,
                        format!(
                            "{} エラー: チャンネルのtopicに `MEMBERS := メンバー1, メンバー2, ...` の宣言が必要です。",
                            msg.author.mention()
                        ),
                    )
                    .await;
                return false;
            }
        };

        let mut content = self.get_combined_content(&msg.channel_id);
        if !content.is_empty() {
            content.push('\n');
        }
        content.push_str(&msg.content);

        match self.processor.parse_program(&members, &content) {
            ProcessingOutcome::Success(_) => {
                self.react(ctx, msg, '✅').await;
                true
            }
            ProcessingOutcome::MissingMembersDeclaration => {
                self.react(ctx, msg, '❎').await;
                self
                    .reply(
                        ctx,
                        msg,
                        format!(
                            "{} エラー: チャンネルのtopicに `MEMBERS := メンバー1, メンバー2, ...` の宣言が必要です。",
                            msg.author.mention()
                        ),
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

        if let Some(command) = CommandKind::from_message(&msg.content) {
            self.handle_command(&ctx, &msg, command).await;
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

    let mut handles = Vec::new();

    for channel_id in target_channel {
        let handler = Handler::new(Some(channel_id), DiscordChannelService, processor);

        handles.push(tokio::spawn({
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
        }));
    }

    for handle in handles {
        let _ = handle.await;
    }
}
