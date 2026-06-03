//! Panel launcher dispatch: `/panel` posts the operations panel; each of the four
//! fixed-ID launcher buttons (`記録する` / `清算確認` / `台帳` / `取り消し`) lands here
//! and delegates to the matching wizard / read-view / void path.

use serenity::{
    all::{CommandInteraction, ComponentInteraction, CreateInteractionResponse},
    prelude::Context,
};
use walicord_application::ledger::expense_session::{ExpenseLaunchOrigin, ExpenseModalIntent};
use walicord_domain::model::MemberId;
use walicord_presentation::discord_ledger::ReadViewRoute;

use super::{
    DeferredEphemeralInteraction, DiscordCallSite, InteractionDispatch, LedgerRouteError,
    LedgerRouter, discord_call_error,
};
use crate::discord::ledger::{
    expense_modal_open::{ExpenseModalPrefill, build_expense_modal_response},
    panel::{
        LEDGER_PANEL_EXPENSE_ID, LEDGER_PANEL_LEDGER_ID, LEDGER_PANEL_REVIEW_ID,
        LEDGER_PANEL_VOID_ID, render_panel_post_message_for_locator_state,
    },
    response_writer::safe_interaction_response_message,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PanelLauncher {
    Expense,
    Review,
    Ledger,
    Void,
}

pub(super) fn panel_launcher(custom_id: &str) -> Option<PanelLauncher> {
    match custom_id {
        LEDGER_PANEL_EXPENSE_ID => Some(PanelLauncher::Expense),
        LEDGER_PANEL_REVIEW_ID => Some(PanelLauncher::Review),
        LEDGER_PANEL_LEDGER_ID => Some(PanelLauncher::Ledger),
        LEDGER_PANEL_VOID_ID => Some(PanelLauncher::Void),
        _ => None,
    }
}

impl LedgerRouter {
    /// /panel: post the operations panel with the 4 fixed launcher buttons. Panel
    /// posts are direct responses (no defer) per criterion 178, and the body /
    /// thread cue come straight from i18n + presentation, not from any per-channel
    /// computation in the router.
    pub(super) async fn dispatch_panel_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, command.guild_id, command.channel_id, command)
            .await?;
        let locator_state = self.deps.locator.cached(scope.tracked_parent());
        if let Some(state) = locator_state.as_ref() {
            self.observe_blocked_locator_state(state);
        }
        let (body, components) =
            match render_panel_post_message_for_locator_state(locator_state.as_ref(), false) {
                Ok(rendered) => rendered,
                Err(message) => message.into_parts(),
            };
        let response = CreateInteractionResponse::Message(
            safe_interaction_response_message()
                .content(body)
                .components(components),
        );
        command
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(DiscordCallSite::PanelCreateResponse))?;
        Ok(InteractionDispatch::Handled)
    }

    pub(super) async fn dispatch_panel_expense_launcher(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        if let Some(ledger_id) = self.blocked_expense_launcher_ledger(ctx, scope).await? {
            let (message, components) =
                self.uncertain_write_block_response(ledger_id, false, false);
            return self
                .reply_component_ephemeral_with_components(
                    ctx,
                    component,
                    message,
                    components,
                    DiscordCallSite::ExpenseUncertainWriteReply,
                )
                .await;
        }
        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        self.store_expense_modal_submission(
            nonce,
            MemberId(component.user.id.get()),
            scope.expense_draft_scope_id(),
            ExpenseModalIntent::Create {
                origin: ExpenseLaunchOrigin::PanelButton,
            },
        );
        let response = build_expense_modal_response(
            self.deps.clock.as_ref(),
            nonce,
            &ExpenseModalPrefill::default(),
        )
        .map_err(LedgerRouteError::from)?;
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(
                DiscordCallSite::PanelExpenseLauncherCreateResponse,
            ))?;
        Ok(InteractionDispatch::Handled)
    }

    pub(super) async fn dispatch_panel_review_launcher(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        self.dispatch_review(
            ctx,
            scope,
            DeferredEphemeralInteraction::Component(component),
            ReadViewRoute::ReviewParent,
        )
        .await
    }

    pub(super) async fn dispatch_panel_ledger_launcher(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        self.dispatch_ledger(
            ctx,
            scope,
            DeferredEphemeralInteraction::Component(component),
            ReadViewRoute::LedgerPanel,
        )
        .await
    }

    pub(super) async fn dispatch_panel_void_launcher(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        self.dispatch_void(
            ctx,
            scope,
            DeferredEphemeralInteraction::Component(component),
        )
        .await
    }
}
