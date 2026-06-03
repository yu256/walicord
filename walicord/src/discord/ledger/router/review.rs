//! `/review` and panel `清算確認` dispatch entry points. The command and component
//! variants converge on a single `dispatch_review` implementation that loads the
//! verified thread, computes a preview via `compose_and_store_preview`, picks the
//! review / no-transfers / empty page model, paginates, registers a
//! `ReadViewSession`, marks the preview delivered, and emits the criterion-293
//! advisory when the underlying ledger is in `uncertain_write`.

use serenity::{all::CommandInteraction, prelude::Context};
use walicord_application::ledger::{
    preview_store::{PreviewStoreError, PreviewStoreKey, PreviewStoreTransition},
    projection::project_verified_entries,
    read_view_session::{ReadViewSession, ReadViewSessionKey},
    settle_flow::{
        PreviewAttemptError, PreviewAttemptOutcome, compose_and_store_preview,
        mark_preview_delivered,
    },
};
use walicord_domain::model::MemberId;
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    DiscordLedgerPresenter, ReadViewRoute, RecoveryCta, SurfaceMemberLabels,
    build_review_empty_page_model, build_review_no_transfers_page_model, build_review_page_model,
    paginate_read_view_model,
};

use super::{
    CanonicalLoadRoute, DeferredEphemeralInteraction, DiscordCallSite, InteractionDispatch,
    LedgerRouteError, LedgerRouter, read_view_navigation_row, uncertain_write_block_message,
};
use crate::discord::ledger::{
    response_writer::rendered_surface_to_message, route_guard::LedgerInteractionScope,
};

impl LedgerRouter {
    pub(super) async fn dispatch_review_command(
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
                return Ok(InteractionDispatch::Ignored);
            }
            Err(error) => return Err(error),
        };
        if !scope.is_thread_interaction() {
            return Ok(InteractionDispatch::Ignored);
        }
        self.dispatch_review(
            ctx,
            scope,
            DeferredEphemeralInteraction::Command(command),
            ReadViewRoute::ReviewThread,
        )
        .await
    }

    pub(super) async fn dispatch_review(
        &self,
        ctx: &Context,
        scope: LedgerInteractionScope,
        interaction: DeferredEphemeralInteraction<'_>,
        route: ReadViewRoute,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        interaction
            .defer(ctx, DiscordCallSite::ReviewDeferEphemeral)
            .await?;
        let Some(binding) = self.resolve_readable_ledger(ctx, scope).await? else {
            let rendered = DiscordLedgerPresenter::render_read_view_page(
                &build_review_empty_page_model(route, false, None),
            )?;
            let (body, components) = rendered_surface_to_message(rendered);
            return interaction
                .edit(ctx, body, components, DiscordCallSite::ReviewEditResponse)
                .await;
        };
        let ledger_id = binding.ledger_id();
        let uncertain_write = self.deps.uncertain_writes.current(ledger_id).is_some();
        let load = self
            .load_verified_thread(ctx, binding, CanonicalLoadRoute::Preview)
            .await?;
        let roster = self
            .deps
            .roster_fetcher
            .fetch(ctx, scope.guild_id(), scope.channel_id())
            .await?;
        let labels = SurfaceMemberLabels::from_member_names(
            roster
                .display_names
                .iter()
                .map(|(member_id, name)| (*member_id, Some(name.as_str()))),
        );
        let views = project_verified_entries(&load).map_err(LedgerRouteError::from)?;

        let mut stored_preview_instance_id = None;
        let key = PreviewStoreKey::new(ledger_id, MemberId(interaction.user_id().get()));
        let prior_preview_instance_id = self
            .deps
            .preview_store
            .current(key)
            .map(|state| state.preview_instance_id());

        let pages = if views.is_empty() {
            if let Some(preview_instance_id) = prior_preview_instance_id {
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
            vec![build_review_empty_page_model(route, uncertain_write, None)]
        } else {
            let actor_id = MemberId(interaction.user_id().get());
            match compose_and_store_preview(
                load.snapshot(),
                ledger_id,
                actor_id,
                self.deps.planner.as_ref(),
                self.deps.clock.as_ref(),
                self.deps.nonce_provider.as_ref(),
                self.deps.preview_store.as_ref(),
            ) {
                Err(error) => {
                    if let Some(preview_instance_id) = prior_preview_instance_id {
                        // Intentional swallow: see comment on the empty-views branch above.
                        let _ = self.deps.preview_store.transition(
                            key,
                            PreviewStoreTransition::ClearMatching {
                                preview_instance_id,
                            },
                        );
                    }
                    let message = match error {
                        PreviewAttemptError::Store(PreviewStoreError::CommitInProgress {
                            ..
                        }) => uncertain_write_block_message(false, true),
                        _ => i18n::review_render_failed_message().to_owned(),
                    };
                    return interaction
                        .edit(
                            ctx,
                            message,
                            Vec::new(),
                            DiscordCallSite::ReviewEditResponse,
                        )
                        .await;
                }
                Ok(outcome) => match outcome {
                    PreviewAttemptOutcome::NoTransfersNeeded => {
                        if let Some(preview_instance_id) = prior_preview_instance_id {
                            // Intentional swallow: see comment on the empty-views branch above.
                            let _ = self.deps.preview_store.transition(
                                key,
                                PreviewStoreTransition::ClearMatching {
                                    preview_instance_id,
                                },
                            );
                        }
                        vec![build_review_no_transfers_page_model(route, uncertain_write)]
                    }
                    PreviewAttemptOutcome::Stored {
                        record,
                        preview_instance_id,
                    } => {
                        stored_preview_instance_id = Some(preview_instance_id);
                        let mut model = build_review_page_model(
                            walicord_presentation::discord_ledger::ReviewPageInputs {
                                route,
                                state: load.snapshot().projected().state(),
                                previewed: record.previewed(),
                                labels: &labels,
                                uncertain_write,
                                recovery_cta: RecoveryCta::ParentLink,
                                recovery_url: None,
                            },
                        );
                        if prior_preview_instance_id.is_some() {
                            model.route_guidance_lines =
                                review_route_guidance_lines_with_replacement_notice(route);
                        }
                        paginate_read_view_model(model)
                    }
                },
            }
        };

        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        let actor_id = MemberId(interaction.user_id().get());
        self.deps.read_view_sessions.replace(ReadViewSession::new(
            ReadViewSessionKey {
                ledger_id,
                actor_id,
            },
            nonce,
            pages.clone(),
            self.deps.clock.now(),
        ));

        let total_pages = pages.len();
        let rendered = DiscordLedgerPresenter::render_read_view_page(&pages[0])
            .map_err(LedgerRouteError::from)?;
        let (body, mut components) = rendered_surface_to_message(rendered);
        if total_pages > 1 {
            components.push(read_view_navigation_row(nonce, 0, total_pages));
        }
        interaction
            .edit(ctx, body, components, DiscordCallSite::ReviewEditResponse)
            .await?;

        if let Some(preview_instance_id) = stored_preview_instance_id {
            mark_preview_delivered(self.deps.preview_store.as_ref(), key, preview_instance_id)?;
        }

        Ok(InteractionDispatch::Handled)
    }
}

pub(super) fn review_route_guidance_lines_with_replacement_notice(
    route: ReadViewRoute,
) -> Vec<String> {
    let mut lines = vec![
        i18n::settlement_preview_replaced_message().to_owned(),
        i18n::route_task_guidance().to_owned(),
    ];
    if matches!(route, ReadViewRoute::ReviewParent) {
        lines.push(i18n::parent_preview_entry_guidance().to_owned());
    }
    lines
}
