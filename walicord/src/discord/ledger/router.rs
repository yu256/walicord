use serenity::{
    all::{
        ButtonStyle, ChannelId, CommandInteraction, ComponentInteraction, CreateActionRow,
        CreateButton, CreateInteractionResponse, CreateInteractionResponseMessage,
        ModalInteraction,
    },
    async_trait,
    prelude::Context,
};
use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap},
    sync::Arc,
};
use walicord_application::{Clock, NonceProvider, SettlementPlanner};
use walicord_domain::model::MemberId;
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    DiscordLedgerPresenter, ExpenseDraftSummary, PanelButtonStates, PanelSurfaceModel,
    RenderBudgetError, SurfaceMemberLabels,
};

use crate::channel::ChannelManager;

use super::{
    expense_flow::{
        ConfirmationBuildError, NavigationError, bootstrap_expense_session,
        build_confirmation_for_session, navigate_back, navigate_modify_selection,
        navigate_to_phase, toggle_members_group,
    },
    expense_modal::{ExpenseModalValidationError, validate_expense_modal_submission},
    expense_modal_open::{
        ExpenseModalBuildError, ExpenseModalCustomIdMatch, ExpenseModalPrefill,
        build_expense_modal_response, extract_raw_expense_modal_submission,
        parse_expense_modal_custom_id,
    },
    observability::LedgerObservability,
    panel::LEDGER_PANEL_EXPENSE_ID,
    participant_resolution::RosterSnapshot,
    preview_store::PreviewStore,
    response_writer::{rendered_surface_to_message, suppressed_allowed_mentions},
    route_guard::{LedgerInteractionGuardError, guard_ledger_interaction},
    sessions::{
        ExpenseSelectionPhase, ExpenseSessionConstructionError, ExpenseSessionKey,
        ExpenseSessionStage, ExpenseSessionStore, ModalRetryBinding, ModalRetryBindingStore,
        ModalRetryPreserved, VoidSessionStore,
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
    pub roster_fetcher: Arc<dyn RouterRosterFetcher>,
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

/// Roster snapshot the router needs at confirmation rebuild / record time. Combines the
/// participant-resolution `RosterSnapshot` (membership + roles) with the per-member
/// display names so the confirmation page can render labelled rows without a second
/// round-trip. The Discord adapter populates this from its `RosterProvider`.
#[derive(Debug, Clone)]
pub struct RouterRosterSnapshot {
    pub roster: RosterSnapshot,
    pub display_names: HashMap<MemberId, smol_str::SmolStr>,
}

/// Object-safe roster port for the router. The discord-side `RosterProvider` trait is
/// generic (returns `impl Future` and uses generic `IntoIterator`) so it cannot live in
/// an `Arc<dyn ...>`; this trait is the thin object-safe boundary the router depends on.
/// Implementations are responsible for converting the underlying port's `RosterSnapshot`
/// into the participant-resolution shape and for resolving display names.
#[async_trait]
pub trait RouterRosterFetcher: Send + Sync {
    async fn fetch(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> Result<RouterRosterSnapshot, RouterRosterFetchError>;
}

#[derive(Debug, thiserror::Error)]
pub enum RouterRosterFetchError {
    #[error("roster fetch failed: {0}")]
    Service(String),
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

/// Top-level route failure surface. Telemetry / loggers see the variant; user-facing
/// copy comes from i18n at the render boundary. `Display` composes the message lazily
/// (via thiserror) so we never allocate a String at the error site — the writer at
/// the boundary calls Display once.
#[derive(Debug, thiserror::Error)]
pub enum LedgerRouteError {
    #[error("interaction lacks guild context")]
    GuildOnly,
    #[error("interaction targets an untracked channel")]
    NotInTrackedChannel,
    #[error("permission denied: {0}")]
    Permission(Cow<'static, str>),
    #[error("internal failure: {0}")]
    Internal(#[from] InternalLedgerRouteError),
}

/// Closed enumeration of every internal failure the router can encounter. Each variant
/// carries the underlying cause as a typed value (no `format!` at the error site);
/// thiserror's `Display` composes the message at the boundary. `#[from]` on the
/// single-source variants installs `From` conversions, so call sites use `?` and
/// `LedgerRouteError::from` rather than dedicated `map_*` helpers.
#[derive(Debug, thiserror::Error)]
pub enum InternalLedgerRouteError {
    #[error("panel render: {0}")]
    PanelRender(#[from] RenderBudgetError),
    #[error("expense modal build: {0}")]
    ExpenseModalBuild(#[from] ExpenseModalBuildError),
    #[error("expense session construction: {0}")]
    SessionConstruction(#[from] ExpenseSessionConstructionError),
    #[error("expense navigation: {0}")]
    Navigation(#[from] NavigationError),
    #[error("expense confirmation build: {0}")]
    ConfirmationBuild(#[from] ConfirmationBuildError),
    #[error("roster fetch: {0}")]
    RosterFetch(#[from] RouterRosterFetchError),
    #[error("expense modal submission missing required fields")]
    ModalSubmissionMissingFields,
    #[error("expense {operation} navigation landed on non-selection stage: {observed_stage:?}")]
    PostNavigationStageInvariant {
        operation: PostNavigationOperation,
        observed_stage: ExpenseSessionStage,
    },
    #[error("discord call ({site}) failed: {error}")]
    DiscordCall {
        site: DiscordCallSite,
        #[source]
        error: serenity::Error,
    },
}

impl From<LedgerInteractionGuardError> for LedgerRouteError {
    fn from(error: LedgerInteractionGuardError) -> Self {
        match error {
            LedgerInteractionGuardError::GuildOnly => Self::GuildOnly,
            LedgerInteractionGuardError::NotInTrackedChannel { .. } => Self::NotInTrackedChannel,
        }
    }
}

impl From<ExpenseModalBuildError> for LedgerRouteError {
    fn from(error: ExpenseModalBuildError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<ExpenseSessionConstructionError> for LedgerRouteError {
    fn from(error: ExpenseSessionConstructionError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<NavigationError> for LedgerRouteError {
    fn from(error: NavigationError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<RenderBudgetError> for LedgerRouteError {
    fn from(error: RenderBudgetError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<ConfirmationBuildError> for LedgerRouteError {
    fn from(error: ConfirmationBuildError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<RouterRosterFetchError> for LedgerRouteError {
    fn from(error: RouterRosterFetchError) -> Self {
        Self::Internal(error.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum PostNavigationOperation {
    #[error("forward")]
    Forward,
    #[error("back")]
    Back,
}

/// Every serenity API call we make from the router is tagged with a site so a failure
/// reported via `InternalLedgerRouteError::DiscordCall` is bucket-distinguishable in
/// logs and telemetry without parsing the underlying error string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum DiscordCallSite {
    #[error("panel create_response")]
    PanelCreateResponse,
    #[error("expense modal create_response")]
    ExpenseModalCreateResponse,
    #[error("panel expense launcher create_response")]
    PanelExpenseLauncherCreateResponse,
    #[error("expense cancel create_response")]
    ExpenseCancelCreateResponse,
    #[error("expense step refresh")]
    ExpenseStepRefresh,
    #[error("expense session missing reply")]
    ExpenseSessionMissingReply,
    #[error("expense retry modal create_response")]
    ExpenseRetryModalCreateResponse,
    #[error("expense modal success ack")]
    ExpenseModalSuccessAck,
    #[error("expense basic-edit modal create_response")]
    ExpenseBasicEditModalCreateResponse,
    #[error("expense confirmation page create_response")]
    ExpenseConfirmationCreateResponse,
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
        .map_err(LedgerRouteError::from)?;

        let model = PanelSurfaceModel {
            thread_cue: walicord_i18n::panel_thread_cue_pending().to_owned(),
            status_line: None,
            button_states: PanelButtonStates::default(),
            ephemeral: false,
        };
        let rendered = DiscordLedgerPresenter::render_panel(&model).map_err(|error| {
            LedgerRouteError::Internal(InternalLedgerRouteError::PanelRender(error))
        })?;
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
            .map_err(discord_call_error(DiscordCallSite::PanelCreateResponse))?;
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
        .map_err(LedgerRouteError::from)?;

        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        let response = build_expense_modal_response(
            self.deps.clock.as_ref(),
            nonce,
            &ExpenseModalPrefill::default(),
        )
        .map_err(LedgerRouteError::from)?;
        command
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseModalCreateResponse,
            ))?;
        Ok(InteractionDispatch::Handled)
    }

    /// Component (button / select-menu) dispatch. Handles the panel launcher and the
    /// session-scoped selection wizard navigation; the picker select menus and the
    /// final `record` write button are wired in follow-up commits.
    pub async fn handle_component(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        if component.data.custom_id == LEDGER_PANEL_EXPENSE_ID {
            return self.dispatch_panel_expense_launcher(ctx, component).await;
        }
        let custom_id = component.data.custom_id.as_str();
        if parse_expense_session_button_nonce(custom_id, EXPENSE_CANCEL_CUSTOM_ID_PREFIX).is_some()
        {
            return self.dispatch_expense_cancel(ctx, component).await;
        }
        if parse_expense_session_button_nonce(custom_id, EXPENSE_BACK_CUSTOM_ID_PREFIX).is_some() {
            return self.dispatch_expense_back(ctx, component).await;
        }
        if parse_expense_session_button_nonce(custom_id, EXPENSE_BASIC_EDIT_CUSTOM_ID_PREFIX)
            .is_some()
        {
            return self.dispatch_expense_basic_edit(ctx, component).await;
        }
        if parse_expense_session_button_nonce(custom_id, EXPENSE_TO_PARTICIPANTS_CUSTOM_ID_PREFIX)
            .is_some()
        {
            return self
                .dispatch_expense_forward(ctx, component, ExpenseSelectionPhase::ParticipantSource)
                .await;
        }
        if parse_expense_session_button_nonce(custom_id, EXPENSE_SOURCE_INDIVIDUAL_CUSTOM_ID_PREFIX)
            .is_some()
        {
            return self
                .dispatch_expense_forward(
                    ctx,
                    component,
                    ExpenseSelectionPhase::IndividualSelection,
                )
                .await;
        }
        if parse_expense_session_button_nonce(custom_id, EXPENSE_SOURCE_ROLES_CUSTOM_ID_PREFIX)
            .is_some()
        {
            return self
                .dispatch_expense_forward(ctx, component, ExpenseSelectionPhase::Roles)
                .await;
        }
        if parse_expense_session_button_nonce(custom_id, EXPENSE_SOURCE_MEMBERS_CUSTOM_ID_PREFIX)
            .is_some()
        {
            return self.dispatch_expense_members_toggle(ctx, component).await;
        }
        if parse_expense_session_button_nonce(custom_id, EXPENSE_TO_WEIGHTS_CUSTOM_ID_PREFIX)
            .is_some()
        {
            return self
                .dispatch_expense_forward(ctx, component, ExpenseSelectionPhase::WeightEditor)
                .await;
        }
        if parse_expense_session_button_nonce(custom_id, EXPENSE_TO_CONFIRM_CUSTOM_ID_PREFIX)
            .is_some()
        {
            return self.dispatch_expense_to_confirm(ctx, component).await;
        }
        if parse_expense_session_button_nonce(custom_id, EXPENSE_MODIFY_SELECTION_CUSTOM_ID_PREFIX)
            .is_some()
        {
            return self.dispatch_expense_modify_selection(ctx, component).await;
        }
        Ok(InteractionDispatch::Ignored)
    }

    /// Apply a legal forward selection-wizard transition, persist the new session,
    /// and refresh the actor's ephemeral with the next step's chrome. Illegal
    /// transitions silently refresh the current step instead of corrupting state.
    async fn dispatch_expense_forward(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        target: ExpenseSelectionPhase,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let (guild_id, channel_id) = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(LedgerRouteError::from)?;
        let key = ExpenseSessionKey::new(guild_id, channel_id, MemberId(component.user.id.get()));
        let Some(current) = self.deps.expense_sessions.clear(key) else {
            return self.respond_expense_session_missing(ctx, component).await;
        };
        match navigate_to_phase(current, target.clone(), self.deps.clock.as_ref()) {
            Ok(updated) => {
                let next_phase = match updated.stage() {
                    ExpenseSessionStage::InSelection { phase } => phase.clone(),
                    other => {
                        return Err(InternalLedgerRouteError::PostNavigationStageInvariant {
                            operation: PostNavigationOperation::Forward,
                            observed_stage: other.clone(),
                        }
                        .into());
                    }
                };
                let nonce = updated.nonce();
                self.deps.expense_sessions.replace(updated);
                self.respond_with_step_body(ctx, component, &next_phase, nonce)
                    .await
            }
            Err(NavigationError::IllegalForwardTransition { from, .. }) => {
                // The session was cleared above without being mutated; we have lost it
                // because navigate_to_phase consumed it. Treat this defensively as
                // session missing so the actor restarts cleanly.
                let _ = (key, from);
                self.respond_expense_session_missing(ctx, component).await
            }
            Err(other) => Err(other.into()),
        }
    }

    /// Transition the session to `InConfirmation` after a fresh roster fetch:
    /// `build_confirmation_for_session` re-resolves the selection against the live
    /// roster (criterion 81), drops stale weight overrides, and captures the
    /// participant snapshot the actor will see. The router renders that snapshot as
    /// an ephemeral confirmation page (criterion 145 / 4-4) with the record / edit
    /// selection / edit basic info / cancel actions.
    async fn dispatch_expense_to_confirm(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let (guild_id, channel_id) = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )?;
        let key = ExpenseSessionKey::new(guild_id, channel_id, MemberId(component.user.id.get()));
        let Some(current) = self.deps.expense_sessions.clear(key) else {
            return self.respond_expense_session_missing(ctx, component).await;
        };

        let roster_snapshot = self
            .deps
            .roster_fetcher
            .fetch(ctx, channel_id)
            .await
            .map_err(LedgerRouteError::from)?;

        let outcome = build_confirmation_for_session(
            current,
            &roster_snapshot.roster,
            self.deps.clock.as_ref(),
        )?;

        let nonce = outcome.session.nonce();
        let basic_info = outcome.session.draft().basic_info().cloned().ok_or(
            InternalLedgerRouteError::ConfirmationBuild(ConfirmationBuildError::BasicInfoMissing),
        )?;
        self.deps.expense_sessions.replace(outcome.session);

        let body = render_confirmation_body(
            &basic_info,
            &outcome.snapshot.participants,
            &outcome.defaulted_members,
            &roster_snapshot.display_names,
        );
        let components = confirmation_action_rows(nonce);

        let response = CreateInteractionResponse::UpdateMessage(
            CreateInteractionResponseMessage::new()
                .ephemeral(true)
                .allowed_mentions(suppressed_allowed_mentions())
                .content(body)
                .components(components),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseConfirmationCreateResponse,
            ))?;
        Ok(InteractionDispatch::Handled)
    }

    /// Walk a confirmation-stage session back to the first selection phase (criterion
    /// 145 / G17) with selection state preserved. The confirmation snapshot is dropped
    /// inside `navigate_modify_selection` so the next confirmation rebuild observes
    /// drift correctly.
    async fn dispatch_expense_modify_selection(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let (guild_id, channel_id) = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )?;
        let key = ExpenseSessionKey::new(guild_id, channel_id, MemberId(component.user.id.get()));
        let Some(current) = self.deps.expense_sessions.clear(key) else {
            return self.respond_expense_session_missing(ctx, component).await;
        };
        let updated = navigate_modify_selection(current, self.deps.clock.as_ref())?;
        let nonce = updated.nonce();
        self.deps.expense_sessions.replace(updated);
        self.respond_with_step_body(ctx, component, &ExpenseSelectionPhase::Payer, nonce)
            .await
    }

    /// Toggle the `MEMBERS` virtual group on the active session and refresh the
    /// participant-source step chrome.
    async fn dispatch_expense_members_toggle(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let (guild_id, channel_id) = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(LedgerRouteError::from)?;
        let key = ExpenseSessionKey::new(guild_id, channel_id, MemberId(component.user.id.get()));
        let Some(current) = self.deps.expense_sessions.clear(key) else {
            return self.respond_expense_session_missing(ctx, component).await;
        };
        let updated = toggle_members_group(current, self.deps.clock.as_ref())
            .map_err(LedgerRouteError::from)?;
        let nonce = updated.nonce();
        self.deps.expense_sessions.replace(updated);
        self.respond_with_step_body(
            ctx,
            component,
            &ExpenseSelectionPhase::ParticipantSource,
            nonce,
        )
        .await
    }

    /// Render the chrome (title + Back/Cancel + phase-specific buttons) for a selection
    /// phase. The picker select menus and the confirmation summary are added in a
    /// follow-up commit; this scaffolds the navigation so the actor can walk the wizard
    /// end-to-end without falling through to the legacy code path.
    async fn respond_with_step_body(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        phase: &ExpenseSelectionPhase,
        nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let title = step_title_for_phase(phase);
        let components = step_action_rows_for_phase(phase, nonce);
        let response = CreateInteractionResponse::UpdateMessage(
            CreateInteractionResponseMessage::new()
                .ephemeral(true)
                .allowed_mentions(suppressed_allowed_mentions())
                .content(title)
                .components(components),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
        Ok(InteractionDispatch::Handled)
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
        .map_err(LedgerRouteError::from)?;
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
            .map_err(LedgerRouteError::from)?;
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseBasicEditModalCreateResponse,
            ))?;
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
        .map_err(LedgerRouteError::from)?;
        let key = ExpenseSessionKey::new(guild_id, channel_id, MemberId(component.user.id.get()));
        let Some(current) = self.deps.expense_sessions.clear(key) else {
            return self.respond_expense_session_missing(ctx, component).await;
        };
        match navigate_back(current, self.deps.clock.as_ref()) {
            Ok(updated) => {
                let previous_phase = match updated.stage() {
                    ExpenseSessionStage::InSelection { phase } => phase.clone(),
                    other => {
                        return Err(InternalLedgerRouteError::PostNavigationStageInvariant {
                            operation: PostNavigationOperation::Back,
                            observed_stage: other.clone(),
                        }
                        .into());
                    }
                };
                let nonce = updated.nonce();
                self.deps.expense_sessions.replace(updated);
                self.respond_with_step_body(ctx, component, &previous_phase, nonce)
                    .await
            }
            Err(NavigationError::AlreadyAtFirstStep) => {
                // From the first phase Back == Cancel (criterion 201).
                self.dispatch_expense_cancel(ctx, component).await
            }
            Err(other) => Err(other.into()),
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
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseSessionMissingReply,
            ))?;
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
        .map_err(LedgerRouteError::from)?;
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
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseCancelCreateResponse,
            ))?;
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
        .map_err(LedgerRouteError::from)?;
        let nonce = self.deps.nonce_provider.next_interaction_nonce();
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
        .map_err(LedgerRouteError::from)?;

        let raw = extract_raw_expense_modal_submission(modal)
            .ok_or(InternalLedgerRouteError::ModalSubmissionMissingFields)?;

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
                let (session, nonce) = bootstrap_expense_session(
                    key,
                    validated,
                    self.deps.clock.as_ref(),
                    self.deps.nonce_provider.as_ref(),
                )
                .map_err(LedgerRouteError::from)?;
                self.deps.expense_sessions.replace(session);
                self.acknowledge_modal_success(ctx, modal, nonce).await
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
                .map_err(LedgerRouteError::from)?;
        modal
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseRetryModalCreateResponse,
            ))?;
        let _ = validation_error;
        Ok(InteractionDispatch::Handled)
    }

    async fn acknowledge_modal_success(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = CreateInteractionResponse::Message(
            CreateInteractionResponseMessage::new()
                .ephemeral(true)
                .allowed_mentions(suppressed_allowed_mentions())
                .content(i18n::expense_step_title_payer())
                .components(step_action_rows_for_phase(
                    &ExpenseSelectionPhase::Payer,
                    nonce,
                )),
        );
        modal
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseModalSuccessAck))?;
        Ok(InteractionDispatch::Handled)
    }
}

/// Discord-call failures need both a static `DiscordCallSite` (known at the call
/// site) and the runtime `serenity::Error`. `From` is inappropriate — a tuple of two
/// unrelated types isn't a semantic unit — so we expose a small curry helper instead:
/// `.map_err(discord_call_error(SITE))` reads as "treat any serenity error from this
/// call as a DiscordCall failure tagged with SITE".
fn discord_call_error(site: DiscordCallSite) -> impl FnOnce(serenity::Error) -> LedgerRouteError {
    move |error| LedgerRouteError::Internal(InternalLedgerRouteError::DiscordCall { site, error })
}

pub(crate) const EXPENSE_CANCEL_CUSTOM_ID_PREFIX: &str = "ledger:expense:cancel:";
pub(crate) const EXPENSE_BACK_CUSTOM_ID_PREFIX: &str = "ledger:expense:back:";
pub(crate) const EXPENSE_BASIC_EDIT_CUSTOM_ID_PREFIX: &str = "ledger:expense:basic-edit:";
pub(crate) const EXPENSE_TO_PARTICIPANTS_CUSTOM_ID_PREFIX: &str = "ledger:expense:to-participants:";
pub(crate) const EXPENSE_SOURCE_INDIVIDUAL_CUSTOM_ID_PREFIX: &str =
    "ledger:expense:source-individual:";
pub(crate) const EXPENSE_SOURCE_ROLES_CUSTOM_ID_PREFIX: &str = "ledger:expense:source-roles:";
pub(crate) const EXPENSE_SOURCE_MEMBERS_CUSTOM_ID_PREFIX: &str = "ledger:expense:source-members:";
pub(crate) const EXPENSE_TO_WEIGHTS_CUSTOM_ID_PREFIX: &str = "ledger:expense:to-weights:";
pub(crate) const EXPENSE_TO_CONFIRM_CUSTOM_ID_PREFIX: &str = "ledger:expense:to-confirm:";
pub(crate) const EXPENSE_MODIFY_SELECTION_CUSTOM_ID_PREFIX: &str =
    "ledger:expense:modify-selection:";

fn step_title_for_phase(phase: &ExpenseSelectionPhase) -> &'static str {
    match phase {
        ExpenseSelectionPhase::Payer => i18n::expense_step_title_payer(),
        ExpenseSelectionPhase::ParticipantSource
        | ExpenseSelectionPhase::IndividualSelection
        | ExpenseSelectionPhase::Roles => i18n::expense_step_title_participants(),
        ExpenseSelectionPhase::WeightEditor => i18n::expense_step_title_weight(),
    }
}

/// Build the chrome action rows for a selection-wizard step. Each phase ends with a
/// `Back` and `Cancel` row so the actor always has an exit; the phase-specific row
/// carries the forward-navigation buttons and any toggle (`MEMBERS`) controls. The
/// per-step picker select menus are added in a follow-up commit.
fn step_action_rows_for_phase(
    phase: &ExpenseSelectionPhase,
    nonce: walicord_application::InteractionNonce,
) -> Vec<CreateActionRow> {
    let mut rows = Vec::new();
    let phase_specific = match phase {
        ExpenseSelectionPhase::Payer => vec![
            CreateButton::new(format!(
                "{EXPENSE_TO_PARTICIPANTS_CUSTOM_ID_PREFIX}{}",
                nonce.get()
            ))
            .label(i18n::expense_next_label())
            .style(ButtonStyle::Primary),
        ],
        ExpenseSelectionPhase::ParticipantSource => vec![
            CreateButton::new(format!(
                "{EXPENSE_SOURCE_INDIVIDUAL_CUSTOM_ID_PREFIX}{}",
                nonce.get()
            ))
            .label(i18n::participant_source_individual_label())
            .style(ButtonStyle::Secondary),
            CreateButton::new(format!(
                "{EXPENSE_SOURCE_ROLES_CUSTOM_ID_PREFIX}{}",
                nonce.get()
            ))
            .label(i18n::participant_source_role_label())
            .style(ButtonStyle::Secondary),
            CreateButton::new(format!(
                "{EXPENSE_SOURCE_MEMBERS_CUSTOM_ID_PREFIX}{}",
                nonce.get()
            ))
            .label(i18n::participant_source_members_label())
            .style(ButtonStyle::Secondary),
            CreateButton::new(format!(
                "{EXPENSE_TO_WEIGHTS_CUSTOM_ID_PREFIX}{}",
                nonce.get()
            ))
            .label(i18n::expense_to_weights_label())
            .style(ButtonStyle::Primary),
        ],
        ExpenseSelectionPhase::IndividualSelection | ExpenseSelectionPhase::Roles => {
            vec![
                CreateButton::new(format!(
                    "{EXPENSE_TO_WEIGHTS_CUSTOM_ID_PREFIX}{}",
                    nonce.get()
                ))
                .label(i18n::expense_to_weights_label())
                .style(ButtonStyle::Primary),
                CreateButton::new(format!(
                    "{EXPENSE_TO_CONFIRM_CUSTOM_ID_PREFIX}{}",
                    nonce.get()
                ))
                .label(i18n::expense_to_confirm_label())
                .style(ButtonStyle::Primary),
            ]
        }
        ExpenseSelectionPhase::WeightEditor => vec![
            CreateButton::new(format!(
                "{EXPENSE_TO_CONFIRM_CUSTOM_ID_PREFIX}{}",
                nonce.get()
            ))
            .label(i18n::expense_to_confirm_label())
            .style(ButtonStyle::Primary),
        ],
    };
    if !phase_specific.is_empty() {
        rows.push(CreateActionRow::Buttons(phase_specific));
    }
    rows.push(CreateActionRow::Buttons(vec![
        CreateButton::new(format!("{EXPENSE_BACK_CUSTOM_ID_PREFIX}{}", nonce.get()))
            .label(i18n::expense_back_label())
            .style(ButtonStyle::Secondary),
        CreateButton::new(format!("{EXPENSE_CANCEL_CUSTOM_ID_PREFIX}{}", nonce.get()))
            .label(i18n::expense_cancel_label())
            .style(ButtonStyle::Danger),
    ]));
    rows
}

fn format_money_for_modal(money: walicord_domain::Money) -> String {
    money.to_string()
}

/// Compose the ephemeral confirmation body the actor sees after pressing 確認へ.
/// Layout:
/// 1. Step title (4/4)
/// 2. Draft summary (amount / date / note) via `ExpenseDraftSummary`
/// 3. One line per resolved participant with display name + weight; rows whose final
///    weight defaulted to 1 get the criterion-216 `既定値 1` cue
/// 4. Confirmation source disclosure (criterion 111: roles / MEMBERS re-evaluated at
///    record time)
///
/// Per-member share amounts are intentionally not rendered here: the canonical share
/// breakdown must match the settlement-rounding output that runs at record time
/// (`compose_expense_entry`). Showing a confirmation-time approximation would diverge
/// from the on-ledger amounts under integer-rounding edge cases. The shared breakdown
/// will land in a follow-up commit that wires the record path through the same
/// `ResolvedExpenseAuthoringInput` used at append time.
fn render_confirmation_body(
    basic_info: &super::sessions::ExpenseBasicInfo,
    participants: &[super::sessions::ExpenseParticipantSelection],
    defaulted_members: &[MemberId],
    display_names: &HashMap<MemberId, smol_str::SmolStr>,
) -> String {
    let defaulted: BTreeSet<MemberId> = defaulted_members.iter().copied().collect();
    let labels = SurfaceMemberLabels::from_member_names(participants.iter().map(|row| {
        (
            row.member_id,
            display_names.get(&row.member_id).map(|s| s.as_str()),
        )
    }));

    let summary_note = basic_info.note.as_ref().map(|note| {
        // `ExpenseNote::new` already enforces a non-empty, trimmed canonical string, so
        // `SafeLiteralText::from_note` returning None here would mean ExpenseNote and
        // SafeLiteralText disagree on validity — an upstream invariant violation, not
        // a runtime case we should silently swallow.
        walicord_presentation::discord_ledger::SafeLiteralText::from_note(note.as_str())
            .expect("validated ExpenseNote should always produce a SafeLiteralText")
    });
    let summary = ExpenseDraftSummary {
        amount: format_money_for_modal(basic_info.amount),
        effective_date: basic_info.effective_date.clone(),
        note: summary_note,
    };

    let mut lines: Vec<String> = Vec::new();
    lines.push(i18n::expense_step_title_confirm().to_owned());
    lines.extend(summary.render_lines());

    for row in participants {
        let display_name = labels
            .member(row.member_id)
            .map(|label| label.visible().as_str().to_owned())
            .unwrap_or_else(|| i18n::unknown_user_label(row.member_id.0).to_string());
        let defaulted_cue = if defaulted.contains(&row.member_id) {
            format!(" [{}]", i18n::weight_default_badge())
        } else {
            String::new()
        };
        lines.push(format!("- {display_name} ×{}{defaulted_cue}", row.weight.0));
    }
    lines.push(
        walicord_presentation::discord_ledger::confirmation_source_disclosure_line().to_owned(),
    );

    lines.join("\n")
}

fn confirmation_action_rows(nonce: walicord_application::InteractionNonce) -> Vec<CreateActionRow> {
    vec![
        CreateActionRow::Buttons(vec![
            CreateButton::new(format!(
                "{EXPENSE_MODIFY_SELECTION_CUSTOM_ID_PREFIX}{}",
                nonce.get()
            ))
            .label(i18n::expense_revise_label())
            .style(ButtonStyle::Secondary),
            CreateButton::new(format!(
                "{EXPENSE_BASIC_EDIT_CUSTOM_ID_PREFIX}{}",
                nonce.get()
            ))
            .label(i18n::expense_basic_info_edit_label())
            .style(ButtonStyle::Secondary),
        ]),
        CreateActionRow::Buttons(vec![
            CreateButton::new(format!("{EXPENSE_CANCEL_CUSTOM_ID_PREFIX}{}", nonce.get()))
                .label(i18n::expense_cancel_label())
                .style(ButtonStyle::Danger),
        ]),
    ]
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
        let cases: [LedgerRouteError; 4] = [
            LedgerRouteError::GuildOnly,
            LedgerRouteError::NotInTrackedChannel,
            LedgerRouteError::Permission(Cow::Borrowed("denied")),
            InternalLedgerRouteError::ModalSubmissionMissingFields.into(),
        ];
        for error in &cases {
            match error {
                LedgerRouteError::GuildOnly => {}
                LedgerRouteError::NotInTrackedChannel => {}
                LedgerRouteError::Permission(detail) => assert!(!detail.is_empty()),
                LedgerRouteError::Internal(_) => {}
            }
        }
    }

    #[test]
    fn route_error_display_includes_underlying_internal_message_via_thiserror() {
        let error: LedgerRouteError = InternalLedgerRouteError::DiscordCall {
            site: DiscordCallSite::ExpenseStepRefresh,
            error: serenity::Error::Other("simulated"),
        }
        .into();

        let rendered = error.to_string();

        assert!(rendered.contains("internal failure"));
        assert!(rendered.contains("expense step refresh"));
    }

    #[test]
    fn internal_route_error_post_navigation_invariant_renders_operation_and_stage() {
        let error = InternalLedgerRouteError::PostNavigationStageInvariant {
            operation: PostNavigationOperation::Forward,
            observed_stage: ExpenseSessionStage::InConfirmation,
        };

        let rendered = error.to_string();

        assert!(rendered.contains("forward"));
        assert!(rendered.contains("InConfirmation"));
    }
}
