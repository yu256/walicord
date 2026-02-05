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
use std::{
    collections::{HashMap, HashSet},
    env,
    sync::Arc,
};
use walicord_application::{
    Command as ProgramCommand, MessageProcessor, ProcessingOutcome, ReceiptAttachment,
    ReceiptContext, ReceiptResolveError, ScriptStatement, SettlementBuildError,
};
use walicord_infrastructure::{OcrsReceiptOcr, WalicordProgramParser, WalicordSettlementOptimizer};
use walicord_parser::extract_members_from_topic;
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

const MISSING_MEMBERS_MESSAGE: &str = walicord_i18n::MISSING_MEMBERS_DECLARATION;

fn format_settlement_error(err: SettlementBuildError) -> String {
    match err {
        SettlementBuildError::Optimization(err) => match err {
            walicord_application::SettlementOptimizationError::ImbalancedTotal(total) => format!(
                "{} (total: {total})",
                walicord_i18n::SETTLEMENT_CALCULATION_FAILED
            ),
            walicord_application::SettlementOptimizationError::NoSolution => {
                walicord_i18n::SETTLEMENT_CALCULATION_FAILED.to_string()
            }
        },
        SettlementBuildError::Receipt(err) => format_receipt_error(err),
    }
}

fn format_receipt_error(err: ReceiptResolveError) -> String {
    match err {
        ReceiptResolveError::OcrUnavailable { .. } => {
            walicord_i18n::RECEIPT_OCR_UNAVAILABLE.to_string()
        }
        ReceiptResolveError::MissingAttachment { .. } => {
            walicord_i18n::RECEIPT_ATTACHMENT_MISSING.to_string()
        }
        ReceiptResolveError::OcrFailed { source } => {
            format!("{} ({source})", walicord_i18n::RECEIPT_OCR_FAILED)
        }
        ReceiptResolveError::TotalNotFound { .. } => {
            walicord_i18n::RECEIPT_TOTAL_NOT_FOUND.to_string()
        }
        ReceiptResolveError::TotalAmbiguous { .. } => {
            walicord_i18n::RECEIPT_TOTAL_AMBIGUOUS.to_string()
        }
    }
}

enum MembersError {
    Channel(ChannelError),
    MissingDeclaration,
}

fn count_lines(text: &str) -> usize {
    if text.is_empty() {
        0
    } else {
        text.lines().count()
    }
}

fn is_image_attachment(attachment: &serenity::model::channel::Attachment) -> bool {
    attachment
        .content_type
        .as_deref()
        .is_some_and(|content_type| content_type.starts_with("image/"))
        || attachment.width.is_some()
        || attachment.height.is_some()
}

struct CombinedContent {
    content: String,
    line_message_ids: Vec<MessageId>,
}

struct Handler<'a> {
    target_channel: Option<ChannelId>,
    message_cache: DashMap<ChannelId, IndexMap<MessageId, Message>>,
    channel_service: DiscordChannelService,
    processor: MessageProcessor<'a>,
    ocr: Option<Arc<OcrsReceiptOcr>>,
}

impl<'a> Handler<'a> {
    fn new(
        target_channel: Option<ChannelId>,
        channel_service: DiscordChannelService,
        processor: MessageProcessor<'a>,
        ocr: Option<Arc<OcrsReceiptOcr>>,
    ) -> Self {
        Self {
            target_channel,
            message_cache: DashMap::new(),
            channel_service,
            processor,
            ocr,
        }
    }

    fn is_target_channel(&self, channel_id: ChannelId) -> bool {
        self.target_channel
            .as_ref()
            .is_some_and(|target| *target == channel_id)
    }

    fn get_combined_content(&self, channel_id: &ChannelId) -> CombinedContent {
        let Some(messages) = self.message_cache.get(channel_id) else {
            return CombinedContent {
                content: String::new(),
                line_message_ids: Vec::new(),
            };
        };

        let mut content = String::new();
        let mut line_message_ids = Vec::new();

        for (message_id, message) in messages.iter() {
            if !content.is_empty() {
                content.push('\n');
            }
            content.push_str(&message.content);

            let line_count = count_lines(&message.content);
            line_message_ids.extend(std::iter::repeat_n(*message_id, line_count));
        }

        CombinedContent {
            content,
            line_message_ids,
        }
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

    fn collect_receipt_lines<'b>(
        &self,
        program: &'b walicord_application::Script<'b>,
    ) -> Vec<usize> {
        let mut lines = Vec::new();
        for stmt in program.statements() {
            let line = stmt.line;
            if let walicord_application::ScriptStatement::Domain(
                walicord_application::Statement::Payment(payment),
            ) = &stmt.statement
                && matches!(
                    &payment.amount,
                    walicord_application::AmountExpr::ReceiptRef { .. }
                )
            {
                lines.push(line);
            }
        }
        lines
    }

    async fn build_receipt_context<'b>(
        &self,
        program: &'b walicord_application::Script<'b>,
        line_message_ids: &[MessageId],
        msg: &Message,
    ) -> Result<ReceiptContext, ReceiptResolveError> {
        let receipt_lines = self.collect_receipt_lines(program);
        if receipt_lines.is_empty() {
            return Ok(ReceiptContext::default());
        }

        if self.ocr.is_none() {
            return Err(ReceiptResolveError::OcrUnavailable {
                line: receipt_lines[0],
            });
        }

        let mut message_ids = HashSet::new();
        for line in &receipt_lines {
            let message_id = line_message_ids
                .get(line.saturating_sub(1))
                .copied()
                .ok_or(ReceiptResolveError::MissingAttachment {
                    line: *line,
                    index: 0,
                })?;
            message_ids.insert(message_id);
        }

        let mut attachments_by_message = HashMap::new();
        for message_id in message_ids {
            let attachments = if message_id == msg.id {
                msg.attachments.clone()
            } else {
                let Some(messages) = self.message_cache.get(&msg.channel_id) else {
                    return Err(ReceiptResolveError::MissingAttachment { line: 0, index: 0 });
                };
                let Some(message) = messages.get(&message_id) else {
                    return Err(ReceiptResolveError::MissingAttachment { line: 0, index: 0 });
                };
                message.attachments.clone()
            };

            let mut receipt_attachments = Vec::new();
            for attachment in attachments.into_iter().filter(is_image_attachment) {
                let bytes =
                    attachment
                        .download()
                        .await
                        .map_err(|err| ReceiptResolveError::OcrFailed {
                            source: walicord_application::ReceiptOcrError::OcrRun {
                                source: Box::new(err),
                            },
                        })?;
                receipt_attachments.push(ReceiptAttachment {
                    bytes,
                    filename: Some(attachment.filename),
                    content_type: attachment.content_type,
                });
            }

            attachments_by_message.insert(message_id, receipt_attachments);
        }

        let mut context = ReceiptContext::default();
        for line in receipt_lines {
            if let Some(message_id) = line_message_ids.get(line.saturating_sub(1)).copied() {
                let attachments = attachments_by_message
                    .get(&message_id)
                    .cloned()
                    .unwrap_or_default();
                context.insert_line_attachments(line, attachments);
            }
        }

        Ok(context)
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
                    format!("{} Error: {MISSING_MEMBERS_MESSAGE}", msg.author.mention()),
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
                    format!("{} Error: {MISSING_MEMBERS_MESSAGE}", msg.author.mention()),
                )
                .await;
                return false;
            }
        };
        let existing_content = self.get_combined_content(&msg.channel_id);

        let previous_statement_count = if existing_content.content.is_empty() {
            0
        } else {
            match self
                .processor
                .parse_program(&members, &existing_content.content)
            {
                ProcessingOutcome::Success(program) => program.statements().len(),
                ProcessingOutcome::MissingMembersDeclaration
                | ProcessingOutcome::UndefinedMember { .. }
                | ProcessingOutcome::FailedToEvaluateGroup { .. }
                | ProcessingOutcome::SyntaxError { .. } => 0,
            }
        };

        let mut content = existing_content.content;
        let mut line_message_ids = existing_content.line_message_ids;
        if !content.is_empty() {
            content.push('\n');
        }
        content.push_str(&msg.content);
        line_message_ids.extend(std::iter::repeat_n(msg.id, count_lines(&msg.content)));

        match self.processor.parse_program(&members, &content) {
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

                let needs_settlement = commands.iter().any(|(_, command)| {
                    matches!(
                        command,
                        ProgramCommand::Evaluate | ProgramCommand::SettleUp(_)
                    )
                });
                let receipt_context = if needs_settlement {
                    match self
                        .build_receipt_context(&program, &line_message_ids, msg)
                        .await
                    {
                        Ok(context) => context,
                        Err(err) => {
                            self.reply(ctx, msg, format_receipt_error(err)).await;
                            return false;
                        }
                    }
                } else {
                    ReceiptContext::default()
                };
                let ocr = self
                    .ocr
                    .as_deref()
                    .map(|ocr| ocr as &dyn walicord_application::ReceiptOcr);

                for (stmt_index, command) in commands {
                    match command {
                        ProgramCommand::Variables => {
                            let reply = VariablesPresenter::render_for_prefix(&program, stmt_index);
                            self.reply(ctx, msg, reply).await;
                        }
                        ProgramCommand::Evaluate => {
                            match self
                                .processor
                                .build_settlement_result_for_prefix_with_context(
                                    &program,
                                    stmt_index,
                                    &receipt_context,
                                    ocr,
                                ) {
                                Ok(response) => {
                                    let view = SettlementPresenter::render(&response);
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
                                .build_settlement_result_for_prefix_with_context(
                                    &program,
                                    stmt_index,
                                    &receipt_context,
                                    ocr,
                                ) {
                                Ok(response) => {
                                    let view = SettlementPresenter::render(&response);
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
            ProcessingOutcome::MissingMembersDeclaration => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!("{} Error: {MISSING_MEMBERS_MESSAGE}", msg.author.mention()),
                )
                .await;
                false
            }
            ProcessingOutcome::UndefinedMember { name, line } => {
                self.react(ctx, msg, '❎').await;
                let error_msg = format!(
                    "Error: Undefined member '{name}' is used at line {line}.\nPlease define it in the channel topic MEMBERS declaration.\nCurrent members: {members:?}"
                );
                self.reply(ctx, msg, format!("{} {error_msg}", msg.author.mention()))
                    .await;
                false
            }
            ProcessingOutcome::FailedToEvaluateGroup { name } => {
                self.react(ctx, msg, '❎').await;
                self.reply(
                    ctx,
                    msg,
                    format!(
                        "{} {}",
                        msg.author.mention(),
                        walicord_i18n::failed_to_evaluate_group(name)
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
    let processor = MessageProcessor::new(&WalicordProgramParser, &WalicordSettlementOptimizer);

    let ocr = match (
        env::var("OCRS_DETECTION_MODEL").ok(),
        env::var("OCRS_RECOGNITION_MODEL").ok(),
    ) {
        (Some(detection), Some(recognition)) => Some(Arc::new(
            OcrsReceiptOcr::new(&detection, &recognition).expect("Failed to load OCRS models"),
        )),
        (None, None) => None,
        _ => panic!("Both OCRS_DETECTION_MODEL and OCRS_RECOGNITION_MODEL must be set"),
    };

    let handles: Vec<tokio::task::JoinHandle<()>> = target_channel
        .into_iter()
        .map(|channel_id| {
            let handler = Handler::new(
                Some(channel_id),
                DiscordChannelService,
                processor,
                ocr.clone(),
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
