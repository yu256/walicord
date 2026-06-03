//! Settle dispatch entry point and the closed taxonomy of `/settle` recovery messages.
//!
//! The single `dispatch_settle_command` impl method coordinates preview validation,
//! per-ledger lock acquisition, retain freezing via `commit_canonical_authoritative`,
//! and preview commit / abort. The two free helpers
//! (`settle_attempt_error_message`, `should_clear_preview_after_settle_error`) map
//! `SettleAttemptError` into the user-facing copy and clear-policy decisions that the
//! dispatcher applies.

use serenity::{all::CommandInteraction, prelude::Context};
use walicord_application::ledger::{
    DiscordLedgerSourceDescriptor,
    preview_store::{
        PreviewCommitGuard, PreviewStoreError, PreviewStoreKey, PreviewStoreTransition,
    },
    settle_flow::{
        SettleAttemptError, SettleAttemptOutcome, compose_settlement_entry_from_preview,
    },
    write_coordinator::WriteTargetKey,
};
use walicord_domain::model::MemberId;
use walicord_i18n as i18n;

use super::{
    CanonicalLoadRoute, CommitOutcome, DiscordCallSite, InteractionDispatch, LedgerRouteError,
    LedgerRouter, canonical_message::render_public_settlement_message, discord_call_error,
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
        let key = PreviewStoreKey::new(ledger_id, actor_id);
        // Pre-lock fast bail: avoid taking the per-ledger lock when uncertain_write
        // is already unresolvable. Cheap when no retain exists (DashMap read only).
        if !self.clear_resolved_uncertain_write(ctx, binding).await {
            let (message, components) = self.uncertain_write_block_response(ledger_id, false, true);
            return self
                .edit_command_response_with_components(
                    ctx,
                    command,
                    message,
                    components,
                    DiscordCallSite::SettleEditResponse,
                )
                .await;
        }

        let lock = self.deps.write_coordinator.lock_for(ledger_id);
        let _guard = lock.lock().await;
        // Post-lock recheck: another writer may have set a fresh uncertain_write
        // between the pre-lock check and acquiring the lock; revalidate inside the
        // critical section before committing to set_live / append.
        if !self.clear_resolved_uncertain_write(ctx, binding).await {
            let (message, components) = self.uncertain_write_block_response(ledger_id, false, true);
            return self
                .edit_command_response_with_components(
                    ctx,
                    command,
                    message,
                    components,
                    DiscordCallSite::SettleEditResponse,
                )
                .await;
        }

        let load = self
            .load_verified_thread(ctx, binding, CanonicalLoadRoute::WritePrelude)
            .await?;
        let next_entry_id = load.next_entry_id()?;
        let Some(preview_instance_id) = self
            .deps
            .preview_store
            .current(key)
            .map(|state| state.preview_instance_id())
        else {
            return self
                .edit_command_response(
                    ctx,
                    command,
                    i18n::review_preview_required_message(),
                    DiscordCallSite::SettleEditResponse,
                )
                .await;
        };
        let outcome = match compose_settlement_entry_from_preview(
            load.snapshot(),
            ledger_id,
            actor_id,
            next_entry_id,
            DiscordLedgerSourceDescriptor::settle_thread_v1(),
            self.deps.preview_store.as_ref(),
            self.deps.clock.as_ref(),
        ) {
            Ok(outcome) => outcome,
            Err(error) => {
                if should_clear_preview_after_settle_error(&error) {
                    // Intentional swallow: ClearMatching only removes when the stored
                    // instance still matches; a mismatch means another interaction
                    // already replaced the preview and we must not touch it.
                    let _ = self.deps.preview_store.transition(
                        key,
                        PreviewStoreTransition::ClearMatching {
                            preview_instance_id,
                        },
                    );
                }
                return self
                    .edit_command_response(
                        ctx,
                        command,
                        settle_attempt_error_message(&error),
                        DiscordCallSite::SettleEditResponse,
                    )
                    .await;
            }
        };

        let SettleAttemptOutcome::RecordableEntry { entry, envelope } = outcome else {
            self.deps.preview_store.transition(
                key,
                PreviewStoreTransition::ClearMatching {
                    preview_instance_id,
                },
            )?;
            return self
                .edit_command_response(
                    ctx,
                    command,
                    i18n::settlement_no_transfer_message(),
                    DiscordCallSite::SettleEditResponse,
                )
                .await;
        };

        let roster = self
            .deps
            .roster_fetcher
            .fetch(ctx, scope.guild_id(), scope.channel_id())
            .await?;
        let rendered_message =
            render_public_settlement_message(&entry, ledger_id, &roster.display_names)?;
        let preview_commit = match PreviewCommitGuard::begin(
            self.deps.preview_store.as_ref(),
            key,
            preview_instance_id,
        ) {
            Ok(preview_commit) => preview_commit,
            Err(error) => {
                return self
                    .edit_command_response(
                        ctx,
                        command,
                        settle_attempt_error_message(&SettleAttemptError::Store(error)),
                        DiscordCallSite::SettleEditResponse,
                    )
                    .await;
            }
        };

        match self
            .commit_canonical_authoritative(
                ctx,
                WriteTargetKey::Published(ledger_id),
                binding,
                entry.entry(),
                &envelope,
                &rendered_message,
            )
            .await?
        {
            CommitOutcome::Recorded => {
                preview_commit.finish()?;
                self.edit_command_response(
                    ctx,
                    command,
                    i18n::settlement_recorded_message(),
                    DiscordCallSite::SettleEditResponse,
                )
                .await
            }
            CommitOutcome::UncertainAppendFailed => {
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

pub(super) fn should_clear_preview_after_settle_error(error: &SettleAttemptError) -> bool {
    matches!(
        error,
        SettleAttemptError::StaleHead { .. }
            | SettleAttemptError::Expired { .. }
            | SettleAttemptError::Record(
                walicord_application::ledger::SettlementRecordError::PreviewNotDelivered
            )
            | SettleAttemptError::Record(_)
            | SettleAttemptError::EnvelopeEncode(_)
    )
}
