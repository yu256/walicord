//! Settle dispatch entry point.
//!
//! `dispatch_settle_command` is a thin adapter shim over the application
//! `settle_execute_v1` use case: it defers the interaction, builds per-request
//! ports + command DTO, calls the use case, and translates the typed outcome
//! into the matching Discord response. The `settle_attempt_error_message`
//! helper maps `SettleAttemptError` into user-facing copy on the
//! `AttemptFailed` branch.

use serenity::{all::CommandInteraction, prelude::Context};
use walicord_application::ledger::{
    DiscordLedgerSourceDescriptor,
    preview_store::PreviewStoreError,
    settle_execute::{SettleExecuteCommand, SettleExecuteOutcome, settle_execute_v1},
    settle_flow::SettleAttemptError,
    write_coordinator::WriteTargetKey,
};
use walicord_domain::model::MemberId;
use walicord_i18n as i18n;

use super::{
    CanonicalLoadRoute, DiscordCallSite, InteractionDispatch, LedgerRouteError, LedgerRouter,
    canonical_message::DiscordSettlementEntryRenderer, discord_call_error,
};
use crate::discord::ledger::{
    locator::RequestBoundLocatorPublisher,
    store::{RequestBoundCanonicalAppender, RequestBoundCanonicalReader},
};

impl LedgerRouter {
    pub(super) async fn dispatch_settle_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = match self
            .guard_scope(ctx, command.guild_id, command.channel_id, command)
            .await
        {
            Ok(scope) => scope,
            Err(LedgerRouteError::NotInTrackedChannel) => {
                return Err(LedgerRouteError::NotInTrackedChannel);
            }
            Err(error) => return Err(error),
        };
        if !scope.is_thread_interaction() {
            return Err(LedgerRouteError::SettleThreadOnly);
        }

        command
            .defer_ephemeral(&ctx.http)
            .await
            .map_err(discord_call_error(DiscordCallSite::SettleDeferEphemeral))?;

        let binding = self.resolve_existing_ledger(ctx, scope).await?;
        let ledger_id = binding.ledger_id();
        let actor_id = MemberId(command.user.id.get());

        let roster = self
            .deps
            .roster_fetcher
            .fetch(ctx, scope.guild_id(), scope.channel_id())
            .await?;
        let appender = RequestBoundCanonicalAppender {
            ctx,
            store: self.deps.canonical_store.as_ref(),
            canonical_thread_id: binding.canonical_thread_id(),
        };
        let publisher = RequestBoundLocatorPublisher {
            locator: self.deps.locator.as_ref(),
            binding,
        };
        let reader = RequestBoundCanonicalReader {
            ctx,
            store: self.deps.canonical_store.as_ref(),
            canonical_thread_id: binding.canonical_thread_id(),
            ledger_id,
            load_route_label: CanonicalLoadRoute::WritePrelude.label(),
        };
        let renderer = DiscordSettlementEntryRenderer {
            display_names: &roster.display_names,
        };
        let outcome = settle_execute_v1(
            &appender,
            &publisher,
            &reader,
            &renderer,
            self.deps.preview_store.as_ref(),
            self.deps.uncertain_writes.as_ref(),
            self.deps.write_coordinator.as_ref(),
            self.deps.observability.as_ref(),
            self.deps.clock.as_ref(),
            SettleExecuteCommand {
                ledger_id,
                actor_id,
                write_target: WriteTargetKey::Published(ledger_id),
                source_descriptor: DiscordLedgerSourceDescriptor::settle_thread_v1(),
            },
        )
        .await
        .map_err(LedgerRouteError::from)?;

        match outcome {
            SettleExecuteOutcome::Recorded { .. } => {
                self.edit_command_response(
                    ctx,
                    command,
                    i18n::settlement_recorded_message(),
                    DiscordCallSite::SettleEditResponse,
                )
                .await
            }
            SettleExecuteOutcome::NoPreviewRequired => {
                self.edit_command_response(
                    ctx,
                    command,
                    i18n::review_preview_required_message(),
                    DiscordCallSite::SettleEditResponse,
                )
                .await
            }
            SettleExecuteOutcome::NoTransferNeeded => {
                self.edit_command_response(
                    ctx,
                    command,
                    i18n::settlement_no_transfer_message(),
                    DiscordCallSite::SettleEditResponse,
                )
                .await
            }
            SettleExecuteOutcome::UncertainBlocked
            | SettleExecuteOutcome::UncertainAppendFailed => {
                let (message, components) =
                    self.uncertain_write_block_response(ledger_id, false, true);
                self.edit_command_response_with_components(
                    ctx,
                    command,
                    message,
                    components,
                    DiscordCallSite::SettleEditResponse,
                )
                .await
            }
            SettleExecuteOutcome::AttemptFailed { error } => {
                self.edit_command_response(
                    ctx,
                    command,
                    settle_attempt_error_message(&error),
                    DiscordCallSite::SettleEditResponse,
                )
                .await
            }
        }
    }
}

pub(super) fn settle_attempt_error_message(error: &SettleAttemptError) -> &'static str {
    match error {
        SettleAttemptError::NoPreviewStored => i18n::review_preview_required_message(),
        SettleAttemptError::StaleHead { .. } | SettleAttemptError::Expired { .. } => {
            i18n::stale_settlement_preview_message()
        }
        SettleAttemptError::Record(
            walicord_application::ledger::SettlementRecordError::PreviewNotDelivered,
        ) => i18n::settlement_preview_not_delivered_message(),
        SettleAttemptError::Store(PreviewStoreError::CommitInProgress { .. }) => {
            i18n::uncertain_write_block_message()
        }
        SettleAttemptError::Store(_)
        | SettleAttemptError::Record(_)
        | SettleAttemptError::EnvelopeEncode(_) => i18n::settlement_confirmation_failed_message(),
    }
}
