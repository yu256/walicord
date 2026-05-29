use serenity::{
    all::{
        CommandInteraction, ComponentInteraction, CreateInteractionResponse,
        CreateInteractionResponseMessage, ModalInteraction,
    },
    prelude::Context,
};
use std::sync::Arc;
use walicord_application::{Clock, NonceProvider, SettlementPlanner};
use walicord_domain::model::MemberId;
use walicord_presentation::discord_ledger::{
    DiscordLedgerPresenter, PanelButtonStates, PanelSurfaceModel,
};

use crate::channel::ChannelManager;

use super::{
    expense_flow::{NavigationError, bootstrap_expense_session, navigate_back},
    expense_modal::{ExpenseModalValidationError, validate_expense_modal_submission},
    expense_modal_open::{
        ExpenseModalBuildError, ExpenseModalCustomIdMatch, ExpenseModalPrefill,
        build_expense_modal_response, extract_raw_expense_modal_submission,
        parse_expense_modal_custom_id,
    },
    observability::LedgerObservability,
    panel::LEDGER_PANEL_EXPENSE_ID,
    preview_store::PreviewStore,
    response_writer::{rendered_surface_to_message, suppressed_allowed_mentions},
    route_guard::{LedgerInteractionGuardError, guard_ledger_interaction},
    sessions::{
        ExpenseSessionConstructionError, ExpenseSessionKey, ExpenseSessionStore, ModalRetryBinding,
        ModalRetryBindingStore, ModalRetryPreserved, VoidSessionStore,
    },
    store::DiscordCanonicalLedgerStore,
    write_coordinator::{UncertainWriteRegistry, WriteCoordinator},
};

/// Wired dependency graph required to dispatch any Discord ledger interaction. The
/// router itself holds no behavior; each route handler reads the dependencies it needs
/// and calls into the appropriate flow module (expense_flow, settle_flow, void_flow,
/// maintenance, etc.).
pub struct LedgerRouterDependencies {
    pub clock: Arc<dyn Clock>,
    pub nonce_provider: Arc<dyn NonceProvider>,
    pub channels: Arc<ChannelManager>,
    pub expense_sessions: Arc<ExpenseSessionStore>,
    pub void_sessions: Arc<VoidSessionStore>,
    pub modal_retries: Arc<ModalRetryBindingStore>,
    pub preview_store: Arc<PreviewStore>,
    pub write_coordinator: Arc<WriteCoordinator>,
    pub uncertain_writes: Arc<UncertainWriteRegistry>,
    pub planner: Arc<dyn SettlementPlanner>,
    pub canonical_store: Arc<DiscordCanonicalLedgerStore>,
    pub observability: Arc<dyn LedgerObservability>,
}

/// Three-way dispatch outcome: the router either fully handled the interaction (so
/// `handler.rs` should not fall through to the legacy code path), did not handle it
/// because the interaction is for a different feature, or hit an error mid-dispatch
/// that the caller surfaces to the actor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InteractionDispatch {
    Handled,
    Ignored,
}

/// Top-level route failure surface. Each variant maps to a concrete user-facing
/// message at the render boundary; the router itself does not render — it returns
/// the typed failure so the caller can apply i18n / surface budget rules.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerRouteError {
    /// The interaction is not from a guild context (criterion 164-166).
    GuildOnly,
    /// The interaction targets a channel that is not currently tracked (criterion
    /// 42 / 132 / 283).
    NotInTrackedChannel,
    /// Bot or actor permissions are insufficient (criterion 231 / 237).
    Permission(String),
    /// A flow-internal error that the caller should surface as a generic recovery
    /// guidance; the detail string is for observability logging only.
    Internal(String),
}

pub struct LedgerRouter {
    deps: LedgerRouterDependencies,
}

impl LedgerRouter {
    pub fn new(deps: LedgerRouterDependencies) -> Self {
        Self { deps }
    }

    pub fn deps(&self) -> &LedgerRouterDependencies {
        &self.deps
    }

    /// Slash-command dispatch. Returns `Ignored` for commands the router does not
    /// own so the caller can fall through to the legacy non-canonical `/review` /
    /// DSL record paths preserved by criterion 13 / 94.
    pub async fn handle_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        match command.data.name.as_str() {
            "expense" => self.dispatch_expense_command(ctx, command).await,
            "panel" => self.dispatch_panel_command(ctx, command).await,
            _ => Ok(InteractionDispatch::Ignored),
        }
    }

    /// /panel: post the operations panel with the 4 fixed launcher buttons. Panel
    /// posts are direct responses (no defer) per criterion 178, and the body /
    /// thread cue come straight from i18n + presentation, not from any per-channel
    /// computation in the router.
    async fn dispatch_panel_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let _scope = guard_ledger_interaction(
            command.guild_id,
            command.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(map_guard_error)?;

        let model = PanelSurfaceModel {
            thread_cue: walicord_i18n::panel_thread_cue_pending().to_owned(),
            status_line: None,
            button_states: PanelButtonStates::default(),
            ephemeral: false,
        };
        let rendered = DiscordLedgerPresenter::render_panel(&model)
            .map_err(|error| LedgerRouteError::Internal(format!("panel render: {error:?}")))?;
        let (body, components) = rendered_surface_to_message(rendered);
        let response = CreateInteractionResponse::Message(
            CreateInteractionResponseMessage::new()
                .content(body)
                .components(components)
                .allowed_mentions(suppressed_allowed_mentions()),
        );
        command
            .create_response(&ctx.http, response)
            .await
            .map_err(|error| {
                LedgerRouteError::Internal(format!("panel create_response: {error}"))
            })?;
        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_expense_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let _scope = guard_ledger_interaction(
            command.guild_id,
            command.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(map_guard_error)?;

        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        let response = build_expense_modal_response(
            self.deps.clock.as_ref(),
            nonce,
            &ExpenseModalPrefill::default(),
        )
        .map_err(map_modal_build_error)?;
        command
            .create_response(&ctx.http, response)
            .await
            .map_err(|error| {
                LedgerRouteError::Internal(format!("expense modal create_response: {error}"))
            })?;
        Ok(InteractionDispatch::Handled)
    }

    /// Component (button / select-menu) dispatch. Currently handles the panel
    /// `記録する` launcher; subsequent slices add the rest of the
    /// `ledger:panel:*` buttons and session-scoped controls.
    pub async fn handle_component(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        if component.data.custom_id == LEDGER_PANEL_EXPENSE_ID {
            return self.dispatch_panel_expense_launcher(ctx, component).await;
        }
        if parse_expense_session_button_nonce(
            &component.data.custom_id,
            EXPENSE_CANCEL_CUSTOM_ID_PREFIX,
        )
        .is_some()
        {
            return self.dispatch_expense_cancel(ctx, component).await;
        }
        if parse_expense_session_button_nonce(
            &component.data.custom_id,
            EXPENSE_BACK_CUSTOM_ID_PREFIX,
        )
        .is_some()
        {
            return self.dispatch_expense_back(ctx, component).await;
        }
        if parse_expense_session_button_nonce(
            &component.data.custom_id,
            EXPENSE_BASIC_EDIT_CUSTOM_ID_PREFIX,
        )
        .is_some()
        {
            return self.dispatch_expense_basic_edit(ctx, component).await;
        }
        Ok(InteractionDispatch::Ignored)
    }

    /// Re-open the expense modal prefilled from the session's current basic_info so
    /// the actor can edit amount / note / date without losing their selection
    /// (criterion 229).
    async fn dispatch_expense_basic_edit(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let (guild_id, channel_id) = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(map_guard_error)?;
        let key = ExpenseSessionKey::new(guild_id, channel_id, MemberId(component.user.id.get()));
        let Some(session) = self.deps.expense_sessions.clear(key) else {
            return self.respond_expense_session_missing(ctx, component).await;
        };
        // Put it back unchanged so other paths still see the same session; the modal
        // submit will overwrite the basic_info via apply_modified_basic_info.
        let prefill = session
            .draft()
            .basic_info()
            .map(|info| ExpenseModalPrefill {
                raw_amount: Some(format_money_for_modal(info.amount)),
                raw_note: info.note.as_ref().map(|note| note.as_str().to_owned()),
                raw_date: Some(info.effective_date.as_str().to_owned()),
            })
            .unwrap_or_default();
        self.deps.expense_sessions.replace(session);
        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        let response = build_expense_modal_response(self.deps.clock.as_ref(), nonce, &prefill)
            .map_err(map_modal_build_error)?;
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(|error| {
                LedgerRouteError::Internal(format!(
                    "expense basic-edit modal create_response: {error}"
                ))
            })?;
        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_expense_back(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let (guild_id, channel_id) = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(map_guard_error)?;
        let key = ExpenseSessionKey::new(guild_id, channel_id, MemberId(component.user.id.get()));
        let Some(current) = self.deps.expense_sessions.clear(key) else {
            return self.respond_expense_session_missing(ctx, component).await;
        };
        match navigate_back(current, self.deps.clock.as_ref()) {
            Ok(updated) => {
                self.deps.expense_sessions.replace(updated);
                // The actual rendered selection-step body is wired in a later slice;
                // for now we acknowledge so Discord does not time out and the legacy
                // handler does not also try to handle this button.
                self.acknowledge_navigation(ctx, component).await
            }
            Err(NavigationError::AlreadyAtFirstStep) => {
                // From the first phase Back == Cancel (criterion 201).
                self.dispatch_expense_cancel(ctx, component).await
            }
            Err(other) => Err(map_navigation_error(other)),
        }
    }

    async fn respond_expense_session_missing(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = CreateInteractionResponse::Message(
            CreateInteractionResponseMessage::new()
                .ephemeral(true)
                .allowed_mentions(suppressed_allowed_mentions())
                .content(walicord_i18n::expense_session_expired_message()),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(|error| {
                LedgerRouteError::Internal(format!("expense session missing reply: {error}"))
            })?;
        Ok(InteractionDispatch::Handled)
    }

    async fn acknowledge_navigation(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = CreateInteractionResponse::UpdateMessage(
            CreateInteractionResponseMessage::new()
                .ephemeral(true)
                .allowed_mentions(suppressed_allowed_mentions())
                .content(walicord_i18n::expense_step_title_payer()),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(|error| {
                LedgerRouteError::Internal(format!("expense navigation ack: {error}"))
            })?;
        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_expense_cancel(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let (guild_id, channel_id) = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(map_guard_error)?;
        let key = ExpenseSessionKey::new(guild_id, channel_id, MemberId(component.user.id.get()));
        self.deps.expense_sessions.clear(key);
        let response = CreateInteractionResponse::UpdateMessage(
            CreateInteractionResponseMessage::new()
                .ephemeral(true)
                .allowed_mentions(suppressed_allowed_mentions())
                .content(walicord_i18n::expense_cancelled_message())
                .components(Vec::new()),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(|error| {
                LedgerRouteError::Internal(format!("expense cancel create_response: {error}"))
            })?;
        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_panel_expense_launcher(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let _scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(map_guard_error)?;
        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        let response = build_expense_modal_response(
            self.deps.clock.as_ref(),
            nonce,
            &ExpenseModalPrefill::default(),
        )
        .map_err(map_modal_build_error)?;
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(|error| {
                LedgerRouteError::Internal(format!(
                    "panel expense launcher create_response: {error}"
                ))
            })?;
        Ok(InteractionDispatch::Handled)
    }

    /// Modal submission dispatch. Currently handles the expense-new modal; weight
    /// editor and retry modals are added in subsequent slices.
    pub async fn handle_modal(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        match parse_expense_modal_custom_id(&modal.data.custom_id) {
            ExpenseModalCustomIdMatch::Match { .. } => {
                self.dispatch_expense_modal_submit(ctx, modal).await
            }
            ExpenseModalCustomIdMatch::Stale => {
                // Stale-nonce expense modal — silently treat as Handled to suppress the
                // legacy fallback; the renderer will be replaced with the criterion-167
                // stale message in a follow-up slice once the modal-retry binding store
                // is wired.
                Ok(InteractionDispatch::Handled)
            }
            ExpenseModalCustomIdMatch::NoMatch => Ok(InteractionDispatch::Ignored),
        }
    }

    async fn dispatch_expense_modal_submit(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let (guild_id, channel_id) = guard_ledger_interaction(
            modal.guild_id,
            modal.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(map_guard_error)?;

        let raw = extract_raw_expense_modal_submission(modal).ok_or_else(|| {
            LedgerRouteError::Internal("expense modal submission missing required fields".into())
        })?;

        match validate_expense_modal_submission(&raw, self.deps.clock.as_ref()) {
            Err(error) => {
                let preserved = ModalRetryPreserved {
                    raw_amount: raw.raw_amount.clone(),
                    raw_note: raw.raw_note.clone(),
                    raw_date: raw.raw_date.clone(),
                };
                self.respond_with_retry_modal(ctx, modal, channel_id, preserved, error)
                    .await
            }
            Ok(validated) => {
                let _ = guild_id;
                let key =
                    ExpenseSessionKey::new(guild_id, channel_id, MemberId(modal.user.id.get()));
                let (session, _nonce) = bootstrap_expense_session(
                    key,
                    validated,
                    self.deps.clock.as_ref(),
                    self.deps.nonce_provider.as_ref(),
                )
                .map_err(map_construction_error)?;
                self.deps.expense_sessions.replace(session);
                self.acknowledge_modal_success(ctx, modal).await
            }
        }
    }

    async fn respond_with_retry_modal(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        channel_id: serenity::all::ChannelId,
        preserved: ModalRetryPreserved,
        validation_error: ExpenseModalValidationError,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        // Reserve a fresh retry-binding nonce, persist it, then re-open the modal with
        // the actor's last raw values so they can correct the offending field
        // (criteria 124-125, 145, 205).
        let binding_nonce = self.deps.nonce_provider.next_interaction_nonce();
        let actor = MemberId(modal.user.id.get());
        let binding = ModalRetryBinding::capture(
            binding_nonce,
            actor,
            channel_id,
            preserved.clone(),
            self.deps.clock.now(),
        );
        self.deps.modal_retries.store(binding);

        let prefill = ExpenseModalPrefill {
            raw_amount: Some(preserved.raw_amount),
            raw_note: Some(preserved.raw_note),
            raw_date: Some(preserved.raw_date),
        };
        let response =
            build_expense_modal_response(self.deps.clock.as_ref(), binding_nonce, &prefill)
                .map_err(map_modal_build_error)?;
        modal
            .create_response(&ctx.http, response)
            .await
            .map_err(|error| {
                LedgerRouteError::Internal(format!("expense retry modal create_response: {error}"))
            })?;
        let _ = validation_error;
        Ok(InteractionDispatch::Handled)
    }

    async fn acknowledge_modal_success(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = CreateInteractionResponse::Message(
            CreateInteractionResponseMessage::new()
                .ephemeral(true)
                .allowed_mentions(suppressed_allowed_mentions())
                .content(walicord_i18n::expense_step_title_payer()),
        );
        modal
            .create_response(&ctx.http, response)
            .await
            .map_err(|error| {
                LedgerRouteError::Internal(format!("expense modal success ack: {error}"))
            })?;
        Ok(InteractionDispatch::Handled)
    }
}

fn map_guard_error(error: LedgerInteractionGuardError) -> LedgerRouteError {
    match error {
        LedgerInteractionGuardError::GuildOnly => LedgerRouteError::GuildOnly,
        LedgerInteractionGuardError::NotInTrackedChannel { .. } => {
            LedgerRouteError::NotInTrackedChannel
        }
    }
}

fn map_modal_build_error(error: ExpenseModalBuildError) -> LedgerRouteError {
    match error {
        ExpenseModalBuildError::Budget(budget) => {
            LedgerRouteError::Internal(format!("expense modal exceeded budget: {budget:?}"))
        }
    }
}

fn map_construction_error(error: ExpenseSessionConstructionError) -> LedgerRouteError {
    LedgerRouteError::Internal(format!("expense session construction failed: {error:?}"))
}

pub(crate) const EXPENSE_CANCEL_CUSTOM_ID_PREFIX: &str = "ledger:expense:cancel:";
pub(crate) const EXPENSE_BACK_CUSTOM_ID_PREFIX: &str = "ledger:expense:back:";
pub(crate) const EXPENSE_BASIC_EDIT_CUSTOM_ID_PREFIX: &str = "ledger:expense:basic-edit:";

fn map_navigation_error(error: NavigationError) -> LedgerRouteError {
    LedgerRouteError::Internal(format!("expense navigation failed: {error:?}"))
}

fn format_money_for_modal(money: walicord_domain::Money) -> String {
    money.to_string()
}

/// Parse a session-scoped button custom_id of the form `{prefix}{nonce}` and return
/// the carried [`InteractionNonce`] when the prefix matches. Returns `None` on prefix
/// mismatch or on a non-numeric / zero nonce — both treated as "not for this route"
/// by the caller.
pub(crate) fn parse_expense_session_button_nonce(
    custom_id: &str,
    prefix: &str,
) -> Option<walicord_application::InteractionNonce> {
    let remainder = custom_id.strip_prefix(prefix)?;
    let value = remainder.parse::<u64>().ok()?;
    walicord_application::InteractionNonce::new(value).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dispatch_outcomes_distinguish_handled_from_ignored() {
        assert_ne!(InteractionDispatch::Handled, InteractionDispatch::Ignored);
    }

    #[test]
    fn route_error_variants_can_be_pattern_matched_distinctly() {
        let cases = [
            LedgerRouteError::GuildOnly,
            LedgerRouteError::NotInTrackedChannel,
            LedgerRouteError::Permission("denied".into()),
            LedgerRouteError::Internal("oops".into()),
        ];
        for error in &cases {
            match error {
                LedgerRouteError::GuildOnly => {}
                LedgerRouteError::NotInTrackedChannel => {}
                LedgerRouteError::Permission(detail) => assert!(!detail.is_empty()),
                LedgerRouteError::Internal(detail) => assert!(!detail.is_empty()),
            }
        }
    }
}
