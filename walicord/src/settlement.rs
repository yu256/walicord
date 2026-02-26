use crate::discord::{
    ports::{RosterProvider, ServiceError},
    svg_renderer::svg_to_png,
};
use serenity::{
    all::CreateAttachment, builder::CreateMessage, model::channel::Message, prelude::*,
};
use std::{collections::HashMap, fmt::Debug};
use walicord_application::{
    Command as ProgramCommand, FailureKind, MessageProcessor, ProgramParseError, ScriptStatement,
    SettlementOptimizationError, SettlementResult,
};
use walicord_domain::model::{MemberId, RoleMembers};
use walicord_presentation::{SettlementPresenter, SettlementView};

/// Formats settlement optimization error for user display
pub fn format_settlement_error(err: SettlementOptimizationError) -> String {
    match err {
        SettlementOptimizationError::ImbalancedTotal(total) => {
            walicord_i18n::settlement_imbalanced_total(total).to_string()
        }
        SettlementOptimizationError::InvalidGrid { g1, g2 } => {
            walicord_i18n::settlement_invalid_grid(g1, g2).to_string()
        }
        SettlementOptimizationError::ModelTooLarge {
            edge_count,
            max_edges,
        } => walicord_i18n::settlement_model_too_large(edge_count, max_edges).to_string(),
        SettlementOptimizationError::NoSolution => {
            walicord_i18n::SETTLEMENT_CALCULATION_FAILED.to_string()
        }
        SettlementOptimizationError::RoundingMismatch => {
            walicord_i18n::SETTLEMENT_CALCULATION_FAILED.to_string()
        }
        SettlementOptimizationError::QuantizationImbalancedTotal { total } => {
            walicord_i18n::settlement_quantization_imbalanced(total).to_string()
        }
        SettlementOptimizationError::QuantizationInvalidAdjustmentCount => {
            walicord_i18n::SETTLEMENT_QUANTIZATION_INVALID_ADJUSTMENT.to_string()
        }
        SettlementOptimizationError::QuantizationInsufficientCandidates => {
            walicord_i18n::SETTLEMENT_QUANTIZATION_INSUFFICIENT_CANDIDATES.to_string()
        }
        SettlementOptimizationError::QuantizationZeroSumInvariantViolation => {
            walicord_i18n::SETTLEMENT_QUANTIZATION_ZERO_SUM_INVARIANT.to_string()
        }
        SettlementOptimizationError::QuantizationNonIntegral => {
            walicord_i18n::SETTLEMENT_QUANTIZATION_NON_INTEGRAL.to_string()
        }
        SettlementOptimizationError::QuantizationOutOfRange => {
            walicord_i18n::SETTLEMENT_QUANTIZATION_FAILED.to_string()
        }
        SettlementOptimizationError::QuantizationUnsupportedScale {
            scale,
            max_supported,
        } => walicord_i18n::settlement_quantization_unsupported_scale(scale, max_supported)
            .to_string(),
        SettlementOptimizationError::WeightOverflow => walicord_i18n::WEIGHT_OVERFLOW.to_string(),
        SettlementOptimizationError::ZeroTotalWeight => {
            walicord_i18n::ZERO_TOTAL_WEIGHT.to_string()
        }
    }
}

/// Service for handling settlement-related operations
pub struct SettlementService<'a, RP>
where
    RP: RosterProvider,
{
    processor: &'a MessageProcessor<'a>,
    roster_provider: &'a RP,
}

impl<'a, RP> SettlementService<'a, RP>
where
    RP: RosterProvider,
{
    pub fn new(processor: &'a MessageProcessor<'a>, roster_provider: &'a RP) -> Self {
        Self {
            processor,
            roster_provider,
        }
    }

    /// Extract member IDs from settlement result
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

    /// Ensure member directory is loaded for displaying names
    async fn ensure_member_directory<'b, I>(
        &self,
        ctx: &Context,
        channel_id: serenity::model::id::ChannelId,
        member_ids: I,
        member_directory: &'b mut Option<HashMap<MemberId, smol_str::SmolStr>>,
    ) -> Result<&'b HashMap<MemberId, smol_str::SmolStr>, ServiceError>
    where
        I: IntoIterator<Item = MemberId>,
    {
        self.ensure_member_directory_with_io(
            channel_id,
            member_ids,
            member_directory,
            async move |channel_id| {
                let channel = channel_id
                    .to_channel(&ctx.http)
                    .await
                    .map_err(|e| ServiceError::Request(format!("{e:?}")))?;
                let Some(guild_channel) = channel.guild() else {
                    return Err(ServiceError::NotGuildChannel);
                };
                Ok(guild_channel.guild_id)
            },
            |channel_id| self.roster_provider.warm_up(ctx, channel_id),
        )
        .await
    }

    async fn ensure_member_directory_with_io<'b, I, FResolve, FutResolve, FWarm, FutWarm>(
        &self,
        channel_id: serenity::model::id::ChannelId,
        member_ids: I,
        member_directory: &'b mut Option<HashMap<MemberId, smol_str::SmolStr>>,
        resolve_guild_id: FResolve,
        warm_up_roster: FWarm,
    ) -> Result<&'b HashMap<MemberId, smol_str::SmolStr>, ServiceError>
    where
        I: IntoIterator<Item = MemberId>,
        FResolve: FnOnce(serenity::model::id::ChannelId) -> FutResolve,
        FutResolve: Future<Output = Result<serenity::model::id::GuildId, ServiceError>>,
        FWarm: FnOnce(serenity::model::id::ChannelId) -> FutWarm,
        FutWarm: Future<Output = Result<(), ServiceError>>,
    {
        let guild_id = resolve_guild_id(channel_id).await?;

        // Warm-up failure is non-fatal because settlement rendering can continue with
        // whatever member data is already cached.
        if let Err(e) = warm_up_roster(channel_id).await {
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

    /// Send a text reply to a message
    async fn reply(&self, ctx: &Context, msg: &Message, content: impl Into<String>) {
        let content = content.into();
        self.reply_with_io(content, async move |content| {
            msg.reply(&ctx.http, content).await.map(|_| ())
        })
        .await;
    }

    async fn reply_with_io<FSend, FutSend, E>(&self, content: String, send_message: FSend)
    where
        FSend: FnOnce(String) -> FutSend,
        FutSend: Future<Output = Result<(), E>>,
        E: Debug,
    {
        if let Err(e) = send_message(content).await {
            tracing::error!("Failed to send message: {:?}", e);
        }
    }

    /// Send a settlement result with image attachment
    async fn reply_with_settlement(&self, ctx: &Context, msg: &Message, response: SettlementView) {
        self.reply_with_settlement_with_io(
            msg,
            response,
            svg_to_png,
            async move |attachment_png| {
                let message_builder = CreateMessage::new().reference_message(msg);
                let message_builder = if let Some(png) = attachment_png {
                    message_builder.add_file(CreateAttachment::bytes(png, "settlement.png"))
                } else {
                    message_builder
                };

                msg.channel_id
                    .send_message(&ctx.http, message_builder)
                    .await
                    .map(|_| ())
            },
        )
        .await;
    }

    async fn reply_with_settlement_with_io<FRender, FSend, FutSend, E>(
        &self,
        msg: &Message,
        response: SettlementView,
        render_png: FRender,
        send_message: FSend,
    ) where
        FRender: FnOnce(&str) -> Option<Vec<u8>>,
        FSend: FnOnce(Option<Vec<u8>>) -> FutSend,
        FutSend: Future<Output = Result<(), E>>,
        E: Debug,
    {
        let attachment_png = render_png(&response.combined_svg);
        let _ = msg;
        if let Err(e) = send_message(attachment_png).await {
            tracing::error!("Failed to send message with attachments: {:?}", e);
        }
    }

    /// Handle settlement command and send appropriate response
    pub async fn handle_settlement_command(
        &self,
        ctx: &Context,
        msg: &Message,
        stmt_index: usize,
        program: &walicord_application::Script<'_>,
        _member_ids: &[MemberId],
        member_directory: &mut Option<HashMap<MemberId, smol_str::SmolStr>>,
    ) {
        match self
            .processor
            .build_settlement_result_for_prefix(program, stmt_index)
            .await
        {
            Ok(response) => {
                let ids: Vec<MemberId> = Self::member_ids(&response).collect();
                let directory = match self
                    .ensure_member_directory(
                        ctx,
                        msg.channel_id,
                        ids.iter().copied(),
                        member_directory,
                    )
                    .await
                {
                    Ok(directory) => directory,
                    Err(e) => {
                        tracing::warn!(
                            "Failed to fetch member directory for channel {} ({} member IDs): {:?}",
                            msg.channel_id,
                            ids.len(),
                            e
                        );
                        member_directory.get_or_insert_with(HashMap::new)
                    }
                };
                let view = SettlementPresenter::render_with_members(&response, directory);
                self.reply_with_settlement(ctx, msg, view).await;
            }
            Err(err) => {
                self.log_settlement_error(&err);
                self.reply(ctx, msg, format_settlement_error(err)).await;
            }
        }
    }

    /// Log settlement error with appropriate level
    fn log_settlement_error(&self, err: &SettlementOptimizationError) {
        let kind = err.kind();
        match kind {
            FailureKind::InternalBug => {
                tracing::error!(error = ?err, "Settlement processing failed due to internal bug");
            }
            FailureKind::Misconfiguration => {
                tracing::warn!(error = ?err, "Settlement processing failed due to misconfiguration");
            }
            FailureKind::UserInput => {
                tracing::info!(error = ?err, "Settlement processing failed due to user input");
            }
        }
    }
}

/// Result of processing a program message
pub struct ProcessResult<'a> {
    pub should_store: bool,
    pub has_effect_statement: bool,
    pub program: Option<walicord_application::Script<'a>>,
}

pub fn evaluate_program<'b>(
    processor: &MessageProcessor<'b>,
    member_ids: &'b [MemberId],
    role_members: &'b RoleMembers,
    cached_contents: &'b [(arcstr::ArcStr, Option<MemberId>)],
    content: &'b str,
    author_id: MemberId,
    next_line_offset: usize,
) -> Result<ProcessResult<'b>, ProgramParseError<'b>> {
    let parse_outcome = if cached_contents.is_empty() {
        processor.parse_program_sequence(
            member_ids,
            role_members,
            std::iter::once((content, Some(author_id))),
        )
    } else {
        processor.parse_program_sequence(
            member_ids,
            role_members,
            cached_contents
                .iter()
                .map(|(cached, author)| (cached.as_ref(), *author))
                .chain(std::iter::once((content, Some(author_id)))),
        )
    };

    let program = parse_outcome.into_result()?;

    let mut has_effect_statement = false;
    let mut should_store = false;

    let statements = program.statements();
    for (_, stmt) in statements
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
                if let ProgramCommand::SettleUp { .. } = command {
                    has_effect_statement = true;
                }
            }
        }
    }
    Ok(ProcessResult {
        should_store,
        has_effect_statement,
        program: Some(program),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{MockRosterProvider, member_info};
    use rstest::rstest;
    use serenity::model::{
        channel::Message,
        id::{ChannelId, GuildId, UserId},
    };
    use std::sync::{
        Arc, Mutex,
        atomic::{AtomicUsize, Ordering},
    };
    use walicord_infrastructure::{WalicordProgramParser, WalicordSettlementOptimizer};

    fn make_message(content: &str, author_id: u64, channel_id: u64) -> Message {
        let mut message = Message::default();
        message.content = content.to_string();
        message.author = serenity::model::user::User::default();
        message.author.id = UserId::new(author_id);
        message.channel_id = serenity::model::id::ChannelId::new(channel_id);
        message
    }

    fn make_processor() -> MessageProcessor<'static> {
        MessageProcessor::new(&WalicordProgramParser, &WalicordSettlementOptimizer)
    }

    fn make_settlement_service<'a>(
        processor: &'a MessageProcessor<'a>,
        roster_provider: &'a MockRosterProvider,
    ) -> SettlementService<'a, MockRosterProvider> {
        SettlementService::new(processor, roster_provider)
    }

    #[rstest]
    #[case::variables_command("!variables", true, false)]
    #[case::settleup_command("!settleup <@1>", true, true)]
    fn evaluate_program_sets_flags(
        #[case] content: &str,
        #[case] expected_store: bool,
        #[case] expected_effect: bool,
    ) {
        let msg = make_message(content, 1, 1);
        let processor = make_processor();
        let member_ids = vec![MemberId(1)];
        let role_members = RoleMembers::default();

        let result = evaluate_program(
            &processor,
            &member_ids,
            &role_members,
            &[],
            msg.content.as_str(),
            MemberId(1),
            0,
        )
        .expect("program should parse");

        assert_eq!(result.should_store, expected_store);
        assert_eq!(result.has_effect_statement, expected_effect);
        assert!(result.program.is_some());
    }

    #[rstest]
    #[case::offset_skips_statement("!variables", 1)]
    fn evaluate_program_respects_offset(#[case] content: &str, #[case] next_line_offset: usize) {
        let msg = make_message(content, 1, 1);
        let processor = make_processor();
        let member_ids = vec![MemberId(1)];
        let role_members = RoleMembers::default();

        let result = evaluate_program(
            &processor,
            &member_ids,
            &role_members,
            &[],
            msg.content.as_str(),
            MemberId(1),
            next_line_offset,
        )
        .expect("program should parse");

        assert!(!result.should_store);
        assert!(!result.has_effect_statement);
        assert!(result.program.is_some());
    }

    #[rstest]
    #[case::png_and_send_success(Some(vec![1, 2, 3]), Ok(()), Some(Some(vec![1, 2, 3])))]
    #[case::png_render_fails(None, Ok(()), Some(None))]
    #[case::send_error_is_ignored(Some(vec![1]), Err(()), Some(Some(vec![1])))]
    #[tokio::test(flavor = "current_thread")]
    async fn reply_with_settlement_with_io_cases(
        #[case] rendered_png: Option<Vec<u8>>,
        #[case] send_result: Result<(), ()>,
        #[case] expected_sent_attachment: Option<Option<Vec<u8>>>,
    ) {
        let processor = make_processor();
        let roster_provider = MockRosterProvider::new();
        let service = make_settlement_service(&processor, &roster_provider);
        let msg = make_message("!settleup <@1>", 1, 10);
        let response = SettlementView {
            combined_svg: "<svg/>".to_string(),
        };

        let sent_attachment = Arc::new(Mutex::new(None::<Option<Vec<u8>>>));
        let sent_attachment_clone = Arc::clone(&sent_attachment);
        let send_count = Arc::new(AtomicUsize::new(0));
        let send_count_clone = Arc::clone(&send_count);
        let rendered_png_for_render = rendered_png.clone();
        let send_result_for_send = send_result;

        service
            .reply_with_settlement_with_io(
                &msg,
                response,
                move |_svg| rendered_png_for_render,
                async move |attachment_png| {
                    send_count_clone.fetch_add(1, Ordering::SeqCst);
                    let sent_attachment_clone = Arc::clone(&sent_attachment_clone);
                    *sent_attachment_clone
                        .lock()
                        .expect("sent attachment mutex poisoned") = Some(attachment_png);
                    send_result_for_send
                },
            )
            .await;

        assert_eq!(send_count.load(Ordering::SeqCst), 1);
        let actual = sent_attachment
            .lock()
            .expect("sent attachment mutex poisoned")
            .clone();
        assert_eq!(actual, expected_sent_attachment);
    }

    #[rstest]
    #[case::send_success(Ok(()))]
    #[case::send_error_is_ignored(Err(()))]
    #[tokio::test(flavor = "current_thread")]
    async fn reply_with_io_cases(#[case] send_result: Result<(), ()>) {
        let processor = make_processor();
        let roster_provider = MockRosterProvider::new();
        let service = make_settlement_service(&processor, &roster_provider);
        let sent = Arc::new(Mutex::new(None::<String>));
        let sent_clone = Arc::clone(&sent);
        let send_count = Arc::new(AtomicUsize::new(0));
        let send_count_clone = Arc::clone(&send_count);
        let send_result_for_send = send_result;

        service
            .reply_with_io("hello".to_string(), async move |content| {
                send_count_clone.fetch_add(1, Ordering::SeqCst);
                let sent_clone = Arc::clone(&sent_clone);
                *sent_clone.lock().expect("sent content mutex poisoned") = Some(content);
                send_result_for_send
            })
            .await;

        assert_eq!(send_count.load(Ordering::SeqCst), 1);
        let actual = sent.lock().expect("sent content mutex poisoned").clone();
        assert_eq!(actual, Some("hello".to_string()));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn ensure_member_directory_with_io_merges_display_names_after_warm_up() {
        let processor = make_processor();
        let guild_id = GuildId::new(1);
        let channel_id = ChannelId::new(10);
        let roster_provider = MockRosterProvider::new()
            .with_member(guild_id, member_info(1, "Alice"))
            .with_member(guild_id, member_info(2, "Bob"));
        let service = SettlementService::new(&processor, &roster_provider);
        let mut member_directory = Some(HashMap::from([(
            MemberId(9),
            smol_str::SmolStr::from("Existing"),
        )]));

        let resolve_count = Arc::new(AtomicUsize::new(0));
        let resolve_count_clone = Arc::clone(&resolve_count);
        let warm_count = Arc::new(AtomicUsize::new(0));
        let warm_count_clone = Arc::clone(&warm_count);

        let result = service
            .ensure_member_directory_with_io(
                channel_id,
                [MemberId(1), MemberId(2), MemberId(99)],
                &mut member_directory,
                move |_| {
                    resolve_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Ok(guild_id))
                },
                move |_| {
                    warm_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Ok(()))
                },
            )
            .await
            .expect("member directory should load");

        assert_eq!(resolve_count.load(Ordering::SeqCst), 1);
        assert_eq!(warm_count.load(Ordering::SeqCst), 1);
        assert_eq!(
            result.get(&MemberId(1)),
            Some(&smol_str::SmolStr::from("Alice"))
        );
        assert_eq!(
            result.get(&MemberId(2)),
            Some(&smol_str::SmolStr::from("Bob"))
        );
        assert_eq!(
            result.get(&MemberId(9)),
            Some(&smol_str::SmolStr::from("Existing"))
        );
        assert!(!result.contains_key(&MemberId(99)));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn ensure_member_directory_with_io_tolerates_warm_up_failure() {
        let processor = make_processor();
        let guild_id = GuildId::new(1);
        let channel_id = ChannelId::new(10);
        let roster_provider =
            MockRosterProvider::new().with_member(guild_id, member_info(1, "Alice"));
        let service = SettlementService::new(&processor, &roster_provider);
        let mut member_directory = None;

        let warm_count = Arc::new(AtomicUsize::new(0));
        let warm_count_clone = Arc::clone(&warm_count);

        let result = service
            .ensure_member_directory_with_io(
                channel_id,
                [MemberId(1)],
                &mut member_directory,
                move |_| std::future::ready(Ok(guild_id)),
                move |_| {
                    warm_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Err(ServiceError::Request("warm failed".into())))
                },
            )
            .await
            .expect("warm-up failure should be non-fatal");

        assert_eq!(warm_count.load(Ordering::SeqCst), 1);
        assert_eq!(
            result.get(&MemberId(1)),
            Some(&smol_str::SmolStr::from("Alice"))
        );
    }

    #[tokio::test(flavor = "current_thread")]
    async fn ensure_member_directory_with_io_propagates_guild_resolution_error() {
        let processor = make_processor();
        let guild_id = GuildId::new(1);
        let channel_id = ChannelId::new(10);
        let roster_provider =
            MockRosterProvider::new().with_member(guild_id, member_info(1, "Alice"));
        let service = SettlementService::new(&processor, &roster_provider);
        let mut member_directory = Some(HashMap::from([(
            MemberId(9),
            smol_str::SmolStr::from("Existing"),
        )]));

        let warm_count = Arc::new(AtomicUsize::new(0));
        let warm_count_clone = Arc::clone(&warm_count);
        let actual = service
            .ensure_member_directory_with_io(
                channel_id,
                [MemberId(1)],
                &mut member_directory,
                move |_| std::future::ready(Err(ServiceError::NotGuildChannel)),
                move |_| {
                    warm_count_clone.fetch_add(1, Ordering::SeqCst);
                    std::future::ready(Ok(()))
                },
            )
            .await
            .map(|_| ());

        assert_eq!(actual, Err(ServiceError::NotGuildChannel));
        assert_eq!(warm_count.load(Ordering::SeqCst), 0);
        assert_eq!(
            member_directory
                .as_ref()
                .and_then(|directory| directory.get(&MemberId(9))),
            Some(&smol_str::SmolStr::from("Existing"))
        );
    }

    #[rstest]
    #[case::zero_sum_invariant(
        SettlementOptimizationError::QuantizationZeroSumInvariantViolation,
        walicord_i18n::SETTLEMENT_QUANTIZATION_ZERO_SUM_INVARIANT.to_string()
    )]
    #[case::non_integral(
        SettlementOptimizationError::QuantizationNonIntegral,
        walicord_i18n::SETTLEMENT_QUANTIZATION_NON_INTEGRAL.to_string()
    )]
    #[case::out_of_range(
        SettlementOptimizationError::QuantizationOutOfRange,
        walicord_i18n::SETTLEMENT_QUANTIZATION_FAILED.to_string()
    )]
    #[case::unsupported_scale(
        SettlementOptimizationError::QuantizationUnsupportedScale {
            scale: 30,
            max_supported: 22,
        },
        walicord_i18n::settlement_quantization_unsupported_scale(30, 22).to_string()
    )]
    #[case::invalid_grid(
        SettlementOptimizationError::InvalidGrid { g1: 1000, g2: 300 },
        walicord_i18n::settlement_invalid_grid(1000, 300).to_string()
    )]
    #[case::model_too_large(
        SettlementOptimizationError::ModelTooLarge {
            edge_count: 121,
            max_edges: 120,
        },
        walicord_i18n::settlement_model_too_large(121, 120).to_string()
    )]
    fn format_settlement_error_uses_quantization_message(
        #[case] error: SettlementOptimizationError,
        #[case] expected: String,
    ) {
        let message = format_settlement_error(error);
        assert_eq!(message, expected);
    }
}
