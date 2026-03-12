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
    SettlementOptimizationError,
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

    fn member_ids(
        result: &walicord_application::SettlementResult,
    ) -> impl Iterator<Item = MemberId> + '_ {
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
        self.reply_with_settlement_with_io(response, svg_to_png, async move |png| {
            let create_message = CreateMessage::new()
                .reference_message(msg)
                .add_file(CreateAttachment::bytes(png, "settlement.png"));

            msg.channel_id
                .send_message(&ctx.http, create_message)
                .await
                .map(|_| ())
        })
        .await;
    }

    async fn reply_with_settlement_with_io<FRender, FSend, FutSend, E>(
        &self,
        response: SettlementView,
        render_png: FRender,
        send_message: FSend,
    ) where
        FRender: FnOnce(&str) -> Option<Vec<u8>>,
        FSend: FnOnce(Vec<u8>) -> FutSend,
        FutSend: Future<Output = Result<(), E>>,
        E: Debug,
    {
        let Some(png) = render_png(&response.combined_svg) else {
            tracing::error!("Failed to render settlement SVG to PNG");
            return;
        };
        if let Err(e) = send_message(png).await {
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
            Ok(result) => {
                let ids: Vec<MemberId> = Self::member_ids(&result).collect();
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
                let view = SettlementPresenter::render_with_members(&result, directory);
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
    use insta::assert_snapshot;
    use rstest::rstest;
    use serenity::model::{
        channel::Message,
        id::{ChannelId, GuildId, UserId},
    };
    use std::sync::{
        Arc, Mutex, OnceLock,
        atomic::{AtomicUsize, Ordering},
    };
    use walicord_application::{Command, Script, ScriptStatementWithLine};
    use walicord_domain::{
        AllocationStrategy, MemberSetExpr, Statement, WeightOverrideTarget, model::RoleId,
    };
    use walicord_infrastructure::{WalicordProgramParser, WalicordSettlementOptimizer};

    const TEST_MEMBER_IDS: [MemberId; 6] = [
        MemberId(1),
        MemberId(2),
        MemberId(3),
        MemberId(4),
        MemberId(5),
        MemberId(6),
    ];

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

    fn role_members() -> &'static RoleMembers {
        static ROLE_MEMBERS: OnceLock<RoleMembers> = OnceLock::new();
        ROLE_MEMBERS.get_or_init(|| {
            RoleMembers::from_iter([
                (
                    RoleId(10),
                    [MemberId(2), MemberId(3), MemberId(4)]
                        .into_iter()
                        .collect(),
                ),
                (RoleId(20), [MemberId(4), MemberId(5)].into_iter().collect()),
            ])
        })
    }

    fn snapshot_member_directory() -> HashMap<MemberId, smol_str::SmolStr> {
        HashMap::from([
            (MemberId(1), "Alice".into()),
            (MemberId(2), "Bob".into()),
            (MemberId(3), "Carol".into()),
            (MemberId(4), "Dave".into()),
            (MemberId(5), "Eve".into()),
            (MemberId(6), "Frank".into()),
        ])
    }

    fn format_role_resolver_input() -> String {
        let mut entries: Vec<String> = role_members()
            .iter()
            .map(|(role_id, members)| {
                let mut member_ids: Vec<u64> =
                    members.iter().map(|member_id| member_id.0).collect();
                member_ids.sort_unstable();
                format!("<@&{}> => {member_ids:?}", role_id.0)
            })
            .collect();
        entries.sort_unstable();
        entries.join("\n")
    }

    fn format_set_expr(expr: &MemberSetExpr<'_>) -> String {
        let mut members: Vec<String> = expr
            .referenced_ids()
            .map(|member_id| format!("<@{}>", member_id.0))
            .collect();
        members.sort_unstable();
        members.dedup();

        let mut roles: Vec<String> = expr
            .referenced_role_ids()
            .map(|role_id| format!("<@&{}>", role_id.0))
            .collect();
        roles.sort_unstable();
        roles.dedup();

        let mut groups: Vec<&str> = expr.referenced_groups().collect();
        groups.sort_unstable();
        groups.dedup();

        format!(
            "members=[{}] roles=[{}] groups=[{}]",
            members.join(", "),
            roles.join(", "),
            groups.join(", ")
        )
    }

    fn format_allocation(allocation: &AllocationStrategy) -> std::borrow::Cow<'static, str> {
        match allocation {
            AllocationStrategy::Even => std::borrow::Cow::Borrowed("even"),
            AllocationStrategy::Weighted(overrides) => {
                let entries = overrides
                    .entries()
                    .iter()
                    .map(|entry| {
                        let target = match &entry.target {
                            WeightOverrideTarget::Member(member_id) => {
                                format!("<@{}>", member_id.0)
                            }
                            WeightOverrideTarget::Role(role_id) => format!("<@&{}>", role_id.0),
                            WeightOverrideTarget::Group(group_name) => group_name.to_string(),
                        };
                        format!("{target}*{}", entry.weight.0)
                    })
                    .collect::<Vec<_>>();
                std::borrow::Cow::Owned(format!("weighted[{}]", entries.join(", ")))
            }
        }
    }

    fn format_ast_line(statement: &ScriptStatementWithLine<'_>) -> String {
        match &statement.statement {
            ScriptStatement::Domain(Statement::Declaration(declaration)) => {
                format!(
                    "L{} declaration {} {}",
                    statement.line,
                    declaration.name,
                    format_set_expr(&declaration.expression)
                )
            }
            ScriptStatement::Domain(Statement::Payment(payment)) => {
                format!(
                    "L{} payment amount={} payer=({}) payee=({}) allocation={}",
                    statement.line,
                    payment.amount,
                    format_set_expr(&payment.payer),
                    format_set_expr(&payment.payee),
                    format_allocation(&payment.allocation)
                )
            }
            ScriptStatement::Command(Command::Variables) => {
                format!("L{} command !variables", statement.line)
            }
            ScriptStatement::Command(Command::Review) => {
                format!("L{} command !review", statement.line)
            }
            ScriptStatement::Command(Command::MemberAddCash { members }) => {
                format!(
                    "L{} command !member set cash members=({})",
                    statement.line,
                    format_set_expr(members)
                )
            }
            ScriptStatement::Command(Command::SettleUp {
                members,
                cash_members,
            }) => {
                let cash = cash_members
                    .as_ref()
                    .map_or(std::borrow::Cow::Borrowed("none"), |expr| {
                        std::borrow::Cow::Owned(format_set_expr(expr))
                    });
                format!(
                    "L{} command !settleup members=({}) cash=({cash})",
                    statement.line,
                    format_set_expr(members),
                )
            }
        }
    }

    fn format_script_ast(script: &Script<'_>) -> String {
        script
            .statements()
            .iter()
            .map(format_ast_line)
            .collect::<Vec<_>>()
            .join("\n")
    }

    async fn render_pipeline_snapshot_body(content: &str, author_id: Option<MemberId>) -> String {
        let processor = make_processor();
        let script = processor
            .parse_program(&TEST_MEMBER_IDS, role_members(), content, author_id)
            .into_result()
            .expect("program should parse");
        let result = processor
            .build_settlement_result(&script)
            .await
            .expect("result generation failed");
        let directory = snapshot_member_directory();
        let view = SettlementPresenter::render_with_members(&result, &directory);
        format!(
            "### resolver_input\n{}\n### source\n{content}\n### ast\n{}\n### combined_svg\n{}\n",
            format_role_resolver_input(),
            format_script_ast(&script),
            view.combined_svg
        )
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
    #[case::render_success(Some(vec![1, 2, 3]), Ok(()), Some(vec![1, 2, 3]))]
    #[case::render_fails(None, Ok(()), None)]
    #[case::send_error_is_ignored(Some(vec![7]), Err(()), Some(vec![7]))]
    #[tokio::test(flavor = "current_thread")]
    async fn reply_with_settlement_with_io_cases(
        #[case] rendered_png: Option<Vec<u8>>,
        #[case] send_result: Result<(), ()>,
        #[case] expected_sent_png: Option<Vec<u8>>,
    ) {
        let processor = make_processor();
        let roster_provider = MockRosterProvider::new();
        let service = make_settlement_service(&processor, &roster_provider);
        let response = SettlementView {
            combined_svg: "<svg/>".to_string(),
        };

        let sent_png = Arc::new(Mutex::new(None::<Vec<u8>>));
        let sent_png_clone = Arc::clone(&sent_png);
        let send_count = Arc::new(AtomicUsize::new(0));
        let send_count_clone = Arc::clone(&send_count);
        let rendered_png_for_render = rendered_png.clone();
        let send_result_for_send = send_result;

        service
            .reply_with_settlement_with_io(
                response,
                move |_svg| rendered_png_for_render.clone(),
                async move |png| {
                    send_count_clone.fetch_add(1, Ordering::SeqCst);
                    *sent_png_clone.lock().expect("sent png mutex poisoned") = Some(png);
                    send_result_for_send
                },
            )
            .await;

        let actual = sent_png.lock().expect("sent png mutex poisoned").clone();

        if expected_sent_png.is_some() {
            assert_eq!(send_count.load(Ordering::SeqCst), 1);
        }
        assert_eq!(actual, expected_sent_png);
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

    #[tokio::test(flavor = "multi_thread")]
    async fn snapshot_integration_review_view_from_resolver_and_ast() {
        let content = "trip := <@&10> <@5>\n12000 to trip\n5250 to <@&20>*2 <@3> <@5>\n!review";
        assert_snapshot!(
            "integration_review_view_from_resolver_and_ast",
            render_pipeline_snapshot_body(content, Some(MemberId(1))).await
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn snapshot_integration_settleup_view_from_resolver_and_ast() {
        let content = "core := <@&10> <@5>\n<@1> paid 8400 to core\n<@6> paid 3600 to <@2> <@3> <@4>\n!member set <@5> cash\n!settleup core --cash <@5> <@6>";
        assert_snapshot!(
            "integration_settleup_view_from_resolver_and_ast",
            render_pipeline_snapshot_body(content, None).await
        );
    }
}
