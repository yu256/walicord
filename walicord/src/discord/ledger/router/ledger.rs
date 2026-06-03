//! `/ledger` and panel `台帳` dispatch entry points, plus the prev/next pagination
//! component handler. The command and panel variants share the same `dispatch_ledger`
//! implementation; `dispatch_read_view_navigate` walks the stored `ReadViewSession`
//! one page at a time on prev/next clicks.

use serenity::{
    all::{CommandInteraction, ComponentInteraction, CreateInteractionResponse},
    prelude::Context,
};
use walicord_application::ledger::{
    projection::project_verified_entries,
    read_view_session::{ReadViewSession, ReadViewSessionKey},
};
use walicord_domain::model::MemberId;
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    DiscordLedgerPresenter, LedgerPageInputs, ReadViewRoute, SurfaceMemberLabels,
    build_ledger_empty_page_model, build_ledger_page_model, paginate_read_view_model,
};

use super::{
    CanonicalLoadRoute, DeferredEphemeralInteraction, DiscordCallSite, InteractionDispatch,
    LedgerRouteError, LedgerRouter, ReadViewNavigation, discord_call_error,
    read_view_navigation_row,
};
use crate::discord::ledger::{
    response_writer::{rendered_surface_to_message, safe_interaction_response_message},
    route_guard::LedgerInteractionScope,
};

impl LedgerRouter {
    pub(super) async fn dispatch_ledger_command(
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
        self.dispatch_ledger(
            ctx,
            scope,
            DeferredEphemeralInteraction::Command(command),
            ReadViewRoute::LedgerCommand,
        )
        .await
    }

    pub(super) async fn dispatch_ledger(
        &self,
        ctx: &Context,
        scope: LedgerInteractionScope,
        interaction: DeferredEphemeralInteraction<'_>,
        route: ReadViewRoute,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        interaction
            .defer(ctx, DiscordCallSite::LedgerDeferEphemeral)
            .await?;
        let Some(binding) = self.resolve_readable_ledger(ctx, scope).await? else {
            let rendered = DiscordLedgerPresenter::render_read_view_page(
                &build_ledger_empty_page_model(route, false),
            )?;
            let (body, components) = rendered_surface_to_message(rendered);
            return interaction
                .edit(ctx, body, components, DiscordCallSite::LedgerEditResponse)
                .await;
        };
        let uncertain_write = self
            .deps
            .uncertain_writes
            .current(binding.ledger_id())
            .is_some();
        let load = self
            .load_verified_thread(ctx, binding, CanonicalLoadRoute::Read)
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

        let pages = if views.is_empty() {
            vec![build_ledger_empty_page_model(route, uncertain_write)]
        } else {
            let model = build_ledger_page_model(LedgerPageInputs {
                route,
                views: &views,
                state: load.snapshot().projected().state(),
                labels: &labels,
                ledger_id: binding.ledger_id(),
                uncertain_write,
            })?;
            paginate_read_view_model(model)
        };

        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        let actor_id = MemberId(interaction.user_id().get());
        let session = ReadViewSession::new(
            ReadViewSessionKey {
                ledger_id: binding.ledger_id(),
                actor_id,
            },
            nonce,
            pages.clone(),
            self.deps.clock.now(),
        );
        let total_pages = pages.len();
        self.deps.read_view_sessions.replace(session);

        let rendered = DiscordLedgerPresenter::render_read_view_page(&pages[0])
            .map_err(LedgerRouteError::from)?;
        let (body, mut components) = rendered_surface_to_message(rendered);
        if total_pages > 1 {
            components.push(read_view_navigation_row(nonce, 0, total_pages));
        }

        interaction
            .edit(ctx, body, components, DiscordCallSite::LedgerEditResponse)
            .await
    }

    pub(super) async fn dispatch_read_view_navigate(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        nonce: walicord_application::InteractionNonce,
        direction: ReadViewNavigation,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let binding = self.resolve_existing_ledger(ctx, scope).await?;
        let actor_id = MemberId(component.user.id.get());

        let key = ReadViewSessionKey {
            ledger_id: binding.ledger_id(),
            actor_id,
        };
        let now = self.deps.clock.now();
        let session_opt = self
            .deps
            .read_view_sessions
            .access(key, nonce, now)
            .unwrap_or_default();
        let Some(mut session) = session_opt else {
            return self
                .reply_component_ephemeral(
                    ctx,
                    component,
                    i18n::stale_interaction_message(),
                    DiscordCallSite::ReadViewNavUpdateResponse,
                )
                .await;
        };

        let _moved = match direction {
            ReadViewNavigation::Previous => session.retreat(now),
            ReadViewNavigation::Next => session.advance(now),
        };
        let index = session.current_index();
        let total_pages = session.page_count();
        let rendered = DiscordLedgerPresenter::render_read_view_page(session.current_page())
            .map_err(LedgerRouteError::from)?;
        let (body, mut components) = rendered_surface_to_message(rendered);
        if total_pages > 1 {
            components.push(read_view_navigation_row(nonce, index, total_pages));
        }
        self.deps.read_view_sessions.replace(session);

        component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ReadViewNavUpdateResponse,
            ))?;
        Ok(InteractionDispatch::Handled)
    }
}
