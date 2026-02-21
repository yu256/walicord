use crate::discord::{
    ports::{RosterProvider, ServiceError},
    svg_renderer::svg_to_png,
};
use serenity::{
    all::CreateAttachment, builder::CreateMessage, model::channel::Message, prelude::*,
};
use std::collections::HashMap;
use walicord_application::{
    Command as ProgramCommand, FailureKind, MessageProcessor, ProgramParseError, ScriptStatement,
    SettlementOptimizationError, SettlementResult,
};
use walicord_domain::model::MemberId;
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
        let channel = channel_id
            .to_channel(&ctx.http)
            .await
            .map_err(|e| ServiceError::Request(format!("{e:?}")))?;
        let Some(guild_channel) = channel.guild() else {
            return Err(ServiceError::NotGuildChannel);
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

    /// Send a text reply to a message
    async fn reply(&self, ctx: &Context, msg: &Message, content: impl Into<String>) {
        let content = content.into();
        if let Err(e) = msg.reply(&ctx.http, content).await {
            tracing::error!("Failed to send message: {:?}", e);
        }
    }

    /// Send a settlement result with image attachment
    async fn reply_with_settlement(&self, ctx: &Context, msg: &Message, response: SettlementView) {
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
    cached_contents: &'b [(arcstr::ArcStr, Option<MemberId>)],
    content: &'b str,
    author_id: MemberId,
    next_line_offset: usize,
) -> Result<ProcessResult<'b>, ProgramParseError<'b>> {
    let parse_outcome = if cached_contents.is_empty() {
        processor.parse_program_sequence(member_ids, std::iter::once((content, Some(author_id))))
    } else {
        processor.parse_program_sequence(
            member_ids,
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
    use rstest::rstest;
    use serenity::model::{channel::Message, id::UserId};
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

        let result = evaluate_program(
            &processor,
            &member_ids,
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

        let result = evaluate_program(
            &processor,
            &member_ids,
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
