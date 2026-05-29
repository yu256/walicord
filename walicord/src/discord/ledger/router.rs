use serenity::{
    all::{
        ChannelId, CommandInteraction, ComponentInteraction, ComponentInteractionDataKind,
        CreateInteractionResponse, CreateInteractionResponseMessage, GuildId, ModalInteraction,
    },
    async_trait,
    prelude::Context,
};
use std::{borrow::Cow, collections::HashMap, sync::Arc};
use walicord_application::{
    Clock, NonceProvider, SettlementPlanner,
    ledger::{
        DiscordLedgerSourceDescriptor, ExpenseAuthoringError, LedgerEntry, LedgerEntryId, LedgerId,
        UnverifiedLedgerStoreEnvelope,
        expense_session::{
            ExpenseConfirmationSnapshot, ExpenseDraftSnapshot, ExpenseParticipantSelection,
            ExpenseSelectionPhase, ExpenseSession, ExpenseSessionConstructionError,
            ExpenseSessionKey, ExpenseSessionStage, ExpenseSessionStore, ModalRetryBinding,
            ModalRetryBindingStore, ModalRetryPreserved, SessionAccessError, VoidSession,
            VoidSessionKey, VoidSessionStage, VoidSessionStore,
        },
        participant_resolution::{ParticipantDrift, RosterSnapshot},
    },
};
use walicord_domain::model::MemberId;
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    BusinessDateTime, DiscordLedgerPresenter, ExpenseConfirmationButtonIds,
    ExpenseSelectionStepButtonIds, LedgerPageInputs, PanelButtonStates, PanelSurfaceModel,
    PublicCanonicalMessageModel, PublicSettlementMessageModel, PublicVoidMessageModel,
    ReadViewBuildError, ReadViewPageModel, ReadViewRoute, RecoveryCta, RecoveryReference,
    RenderBudgetError, SurfaceActionRow, SurfaceButton, SurfaceInteractiveButtonStyle,
    SurfaceMemberLabels, SurfaceSelectMenu, SurfaceSelectOption, TransferRow, VoidCandidateRow,
    VoidConfirmationRecap, VoidRetargetReason, VoidSurfaceModel,
    build_expense_confirmation_surface, build_expense_selection_step_surface,
    build_ledger_empty_page_model, build_ledger_page_model, build_review_empty_page_model,
    build_review_no_transfers_page_model, build_review_page_model, paginate_read_view_model,
    summary_for_view,
};

use crate::channel::ChannelManager;

use super::{
    expense_modal_open::{
        ExpenseModalBuildError, ExpenseModalCustomIdMatch, ExpenseModalPrefill,
        build_expense_modal_response, extract_raw_expense_modal_submission,
        parse_expense_modal_custom_id,
    },
    observability::DiscordLedgerObservability,
    panel::LEDGER_PANEL_EXPENSE_ID,
    response_writer::{rendered_surface_to_message, suppressed_allowed_mentions},
    route_guard::{LedgerInteractionGuardError, guard_ledger_interaction},
    store::{
        DiscordCanonicalLedgerStore, StoreLoadError, StoreWriteError, VerifiedLedgerThreadLoad,
    },
};

use walicord_application::ledger::{
    expense_flow::{
        ConfirmationBuildError, NavigationError, bootstrap_expense_session,
        build_confirmation_for_session, navigate_back, navigate_modify_selection,
        navigate_to_phase, toggle_members_group,
    },
    expense_modal::{ExpenseModalValidationError, validate_expense_modal_submission},
    expense_write::{
        ExpenseWriteOrchestrationError, RecordTimeOutcome, build_canonical_envelope,
        compose_expense_entry,
    },
    preview_store::{PreviewStore, PreviewStoreError, PreviewStoreKey, PreviewStoreTransition},
    projection::{VerifiedLedgerEntryView, project_verified_entries},
    read_view_session::{ReadViewSession, ReadViewSessionKey, ReadViewSessionStore},
    settle_flow::{
        PreviewAttemptError, PreviewAttemptOutcome, SettleAttemptError, SettleAttemptOutcome,
        compose_and_store_preview, compose_settlement_entry_from_preview, mark_preview_delivered,
    },
    void_flow::{
        VoidCandidateEnumerationError, VoidComposeError, VoidConfirmTransitionError,
        VoidSessionBootstrapError, bootstrap_void_session, compose_void_entry,
        enumerate_void_candidates, transition_to_confirm,
    },
    write_coordinator::{
        RetainedCanonicalWrite, UncertainWriteRegistry, WriteCoordinator, WriteTargetKey,
    },
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
    pub thread_loader: Arc<dyn LedgerThreadLoader>,
    pub expense_sessions: Arc<ExpenseSessionStore>,
    pub void_sessions: Arc<VoidSessionStore>,
    pub modal_retries: Arc<ModalRetryBindingStore>,
    pub preview_store: Arc<PreviewStore>,
    pub read_view_sessions: Arc<ReadViewSessionStore<ReadViewPageModel>>,
    pub write_coordinator: Arc<WriteCoordinator>,
    pub uncertain_writes: Arc<UncertainWriteRegistry>,
    pub planner: Arc<dyn SettlementPlanner>,
    pub canonical_store: Arc<DiscordCanonicalLedgerStore>,
    pub observability: Arc<dyn DiscordLedgerObservability>,
}

/// Object-safe port the router uses to load the verified canonical thread for a
/// tracked channel. Wraps `DiscordCanonicalLedgerStore::load_verified_thread`; the
/// adapter is responsible for choosing the `route_label` and supplying the `ctx`.
#[async_trait]
pub trait LedgerThreadLoader: Send + Sync {
    async fn load(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
    ) -> Result<VerifiedLedgerThreadLoad, StoreLoadError>;
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
        guild_id: GuildId,
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
    #[error("expense share computation: {0}")]
    ExpenseAuthoring(#[from] ExpenseAuthoringError),
    #[error("expense write orchestration: {0}")]
    ExpenseWriteOrchestration(#[from] ExpenseWriteOrchestrationError),
    #[error("canonical thread load: {0}")]
    ThreadLoad(#[from] StoreLoadError),
    #[error("canonical thread write: {0}")]
    ThreadWrite(#[from] StoreWriteError),
    #[error("projection consistency: {0}")]
    Projection(#[from] walicord_application::ledger::projection::ProjectionConsistencyError),
    #[error("read view build: {0}")]
    ReadViewBuild(#[from] ReadViewBuildError),
    #[error("settlement preview composition: {0}")]
    PreviewAttempt(#[from] PreviewAttemptError),
    #[error("settlement commit composition: {0}")]
    SettleAttempt(#[from] SettleAttemptError),
    #[error("preview store transition: {0}")]
    PreviewStore(#[from] PreviewStoreError),
    #[error("void session bootstrap: {0}")]
    VoidBootstrap(#[from] VoidSessionBootstrapError),
    #[error("void candidate enumeration: {0}")]
    VoidCandidateEnumeration(#[from] VoidCandidateEnumerationError),
    #[error("void confirm transition: {0}")]
    VoidConfirmTransition(#[from] VoidConfirmTransitionError),
    #[error("void compose: {0}")]
    VoidCompose(#[from] VoidComposeError),
    #[error("void session access: {0}")]
    VoidSessionAccess(#[from] SessionAccessError),
    #[error("uncertain write already live for ledger {ledger_id:?}")]
    UncertainWriteAlreadyLive { ledger_id: LedgerId },
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

impl From<PreviewAttemptError> for LedgerRouteError {
    fn from(error: PreviewAttemptError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<SettleAttemptError> for LedgerRouteError {
    fn from(error: SettleAttemptError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<PreviewStoreError> for LedgerRouteError {
    fn from(error: PreviewStoreError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<VoidSessionBootstrapError> for LedgerRouteError {
    fn from(error: VoidSessionBootstrapError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<VoidCandidateEnumerationError> for LedgerRouteError {
    fn from(error: VoidCandidateEnumerationError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<VoidConfirmTransitionError> for LedgerRouteError {
    fn from(error: VoidConfirmTransitionError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<VoidComposeError> for LedgerRouteError {
    fn from(error: VoidComposeError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<SessionAccessError> for LedgerRouteError {
    fn from(error: SessionAccessError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<ExpenseAuthoringError> for LedgerRouteError {
    fn from(error: ExpenseAuthoringError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<ExpenseWriteOrchestrationError> for LedgerRouteError {
    fn from(error: ExpenseWriteOrchestrationError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<StoreLoadError> for LedgerRouteError {
    fn from(error: StoreLoadError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<StoreWriteError> for LedgerRouteError {
    fn from(error: StoreWriteError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<walicord_application::ledger::projection::ProjectionConsistencyError>
    for LedgerRouteError
{
    fn from(error: walicord_application::ledger::projection::ProjectionConsistencyError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<ReadViewBuildError> for LedgerRouteError {
    fn from(error: ReadViewBuildError) -> Self {
        Self::Internal(error.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ReadViewNavigation {
    Previous,
    Next,
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
    #[error("expense record success ack")]
    ExpenseRecordSuccessAck,
    #[error("ledger defer ephemeral")]
    LedgerDeferEphemeral,
    #[error("ledger edit response")]
    LedgerEditResponse,
    #[error("review defer ephemeral")]
    ReviewDeferEphemeral,
    #[error("review edit response")]
    ReviewEditResponse,
    #[error("settle defer ephemeral")]
    SettleDeferEphemeral,
    #[error("settle edit response")]
    SettleEditResponse,
    #[error("void defer ephemeral")]
    VoidDeferEphemeral,
    #[error("void edit response")]
    VoidEditResponse,
    #[error("void update response")]
    VoidUpdateResponse,
    #[error("void defer component")]
    VoidDeferComponent,
    #[error("read view nav update_response")]
    ReadViewNavUpdateResponse,
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
            "ledger" => self.dispatch_ledger_command(ctx, command).await,
            "review" => self.dispatch_review_command(ctx, command).await,
            "settle" => self.dispatch_settle_command(ctx, command).await,
            "void" => self.dispatch_void_command(ctx, command).await,
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

    async fn dispatch_review_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = match guard_ledger_interaction(
            command.guild_id,
            command.channel_id,
            self.deps.channels.as_ref(),
        ) {
            Ok(scope) => scope,
            Err(LedgerInteractionGuardError::NotInTrackedChannel { .. }) => {
                return Ok(InteractionDispatch::Ignored);
            }
            Err(error) => return Err(LedgerRouteError::from(error)),
        };

        command
            .defer_ephemeral(&ctx.http)
            .await
            .map_err(discord_call_error(DiscordCallSite::ReviewDeferEphemeral))?;

        let load = self
            .deps
            .thread_loader
            .load(ctx, scope.channel_id(), scope.ledger_id())
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
        let route = ReadViewRoute::ReviewThread;

        let mut stored_preview_instance_id = None;
        let key = PreviewStoreKey::new(scope.ledger_id(), MemberId(command.user.id.get()));
        let prior_preview_instance_id = self
            .deps
            .preview_store
            .current(key)
            .map(|state| state.preview_instance_id());

        let pages = if views.is_empty() {
            if let Some(preview_instance_id) = prior_preview_instance_id {
                let _ = self.deps.preview_store.transition(
                    key,
                    PreviewStoreTransition::ClearMatching {
                        preview_instance_id,
                    },
                );
            }
            vec![build_review_empty_page_model(route, false, None)]
        } else {
            let actor_id = MemberId(command.user.id.get());
            match compose_and_store_preview(
                load.snapshot(),
                scope.ledger_id(),
                actor_id,
                self.deps.planner.as_ref(),
                self.deps.clock.as_ref(),
                self.deps.nonce_provider.as_ref(),
                self.deps.preview_store.as_ref(),
            ) {
                Err(error) => {
                    if let Some(preview_instance_id) = prior_preview_instance_id {
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
                    return self
                        .edit_command_response(
                            ctx,
                            command,
                            message,
                            DiscordCallSite::ReviewEditResponse,
                        )
                        .await;
                }
                Ok(outcome) => match outcome {
                    PreviewAttemptOutcome::NoTransfersNeeded => {
                        if let Some(preview_instance_id) = prior_preview_instance_id {
                            let _ = self.deps.preview_store.transition(
                                key,
                                PreviewStoreTransition::ClearMatching {
                                    preview_instance_id,
                                },
                            );
                        }
                        vec![build_review_no_transfers_page_model(route, false)]
                    }
                    PreviewAttemptOutcome::Stored {
                        record,
                        preview_instance_id,
                    } => {
                        stored_preview_instance_id = Some(preview_instance_id);
                        paginate_read_view_model(build_review_page_model(
                            walicord_presentation::discord_ledger::ReviewPageInputs {
                                route,
                                state: load.snapshot().projected().state(),
                                previewed: record.previewed(),
                                labels: &labels,
                                uncertain_write: false,
                                recovery_cta: RecoveryCta::ParentLink,
                                recovery_url: None,
                            },
                        ))
                    }
                },
            }
        };

        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        let actor_id = MemberId(command.user.id.get());
        self.deps.read_view_sessions.replace(ReadViewSession::new(
            ReadViewSessionKey {
                ledger_id: scope.ledger_id(),
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
        command
            .edit_response(
                &ctx.http,
                serenity::all::EditInteractionResponse::new()
                    .content(body)
                    .components(components)
                    .allowed_mentions(suppressed_allowed_mentions()),
            )
            .await
            .map_err(discord_call_error(DiscordCallSite::ReviewEditResponse))?;

        if let Some(preview_instance_id) = stored_preview_instance_id {
            mark_preview_delivered(self.deps.preview_store.as_ref(), key, preview_instance_id)?;
        }

        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_settle_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = match guard_ledger_interaction(
            command.guild_id,
            command.channel_id,
            self.deps.channels.as_ref(),
        ) {
            Ok(scope) => scope,
            Err(LedgerInteractionGuardError::NotInTrackedChannel { .. }) => {
                return Ok(InteractionDispatch::Ignored);
            }
            Err(error) => return Err(LedgerRouteError::from(error)),
        };

        command
            .defer_ephemeral(&ctx.http)
            .await
            .map_err(discord_call_error(DiscordCallSite::SettleDeferEphemeral))?;

        let ledger_id = scope.ledger_id();
        let actor_id = MemberId(command.user.id.get());
        let key = PreviewStoreKey::new(ledger_id, actor_id);
        if self.deps.uncertain_writes.current(ledger_id).is_some() {
            return self
                .edit_command_response(
                    ctx,
                    command,
                    uncertain_write_block_message(false, true),
                    DiscordCallSite::SettleEditResponse,
                )
                .await;
        }

        let lock = self.deps.write_coordinator.lock_for(ledger_id);
        let _guard = lock.lock().await;

        let load = self
            .deps
            .thread_loader
            .load(ctx, scope.channel_id(), ledger_id)
            .await?;
        let next_entry_id =
            LedgerEntryId((load.snapshot().canonical_entry_count() as u64).saturating_add(1));
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
        if let Err(error) = self.deps.preview_store.transition(
            key,
            PreviewStoreTransition::BeginCommit {
                preview_instance_id,
            },
        ) {
            return self
                .edit_command_response(
                    ctx,
                    command,
                    settle_attempt_error_message(&SettleAttemptError::Store(error)),
                    DiscordCallSite::SettleEditResponse,
                )
                .await;
        }

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
                    let _ = self.deps.preview_store.transition(
                        key,
                        PreviewStoreTransition::ClearMatching {
                            preview_instance_id,
                        },
                    );
                } else {
                    let _ = self.deps.preview_store.transition(
                        key,
                        PreviewStoreTransition::AbortCommit {
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
                PreviewStoreTransition::FinishCommit {
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
        let prepared_body =
            render_public_settlement_body(&entry, ledger_id, &roster.display_names)?;
        let envelope_bytes =
            walicord_application::ledger::canonical_attachment::CanonicalAttachmentCodec::encode_with_pre_self_link_content(
                &envelope,
                Some(prepared_body.as_str()),
            )
            .map_err(|error| {
                LedgerRouteError::Internal(InternalLedgerRouteError::ThreadWrite(
                    StoreWriteError::Prepare(error),
                ))
            })?;
        let retained = RetainedCanonicalWrite::new(
            ledger_id,
            &envelope,
            envelope_bytes,
            prepared_body.clone(),
            short_summary_for_entry(&entry),
        );
        self.deps.uncertain_writes.set_live(retained).map_err(|_| {
            LedgerRouteError::Internal(InternalLedgerRouteError::UncertainWriteAlreadyLive {
                ledger_id,
            })
        })?;

        let append_result = self
            .deps
            .canonical_store
            .append_authoritative(ctx, scope.channel_id(), &envelope, prepared_body.as_str())
            .await;
        match append_result {
            Ok(_verified) => {
                self.deps.uncertain_writes.clear(ledger_id);
                self.deps.preview_store.transition(
                    key,
                    PreviewStoreTransition::FinishCommit {
                        preview_instance_id,
                    },
                )?;
                self.edit_command_response(
                    ctx,
                    command,
                    i18n::settlement_recorded_message(),
                    DiscordCallSite::SettleEditResponse,
                )
                .await
            }
            Err(_error) => {
                let _ = self.deps.preview_store.transition(
                    key,
                    PreviewStoreTransition::AbortCommit {
                        preview_instance_id,
                    },
                );
                self.edit_command_response(
                    ctx,
                    command,
                    uncertain_write_block_message(false, true),
                    DiscordCallSite::SettleEditResponse,
                )
                .await
            }
        }
    }

    async fn dispatch_void_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = match guard_ledger_interaction(
            command.guild_id,
            command.channel_id,
            self.deps.channels.as_ref(),
        ) {
            Ok(scope) => scope,
            Err(LedgerInteractionGuardError::NotInTrackedChannel { .. }) => {
                return Ok(InteractionDispatch::Ignored);
            }
            Err(error) => return Err(LedgerRouteError::from(error)),
        };

        command
            .defer_ephemeral(&ctx.http)
            .await
            .map_err(discord_call_error(DiscordCallSite::VoidDeferEphemeral))?;

        if self
            .deps
            .uncertain_writes
            .current(scope.ledger_id())
            .is_some()
        {
            return self
                .edit_command_response(
                    ctx,
                    command,
                    uncertain_write_block_message(false, false),
                    DiscordCallSite::VoidEditResponse,
                )
                .await;
        }

        let load = self
            .deps
            .thread_loader
            .load(ctx, scope.channel_id(), scope.ledger_id())
            .await?;
        if load.snapshot().canonical_entry_count() == 0 {
            return self
                .edit_command_with_void_model(
                    ctx,
                    command,
                    VoidSurfaceModel::empty(i18n::panel_void_button_label(), Vec::new(), true),
                )
                .await;
        }

        let actor_id = MemberId(command.user.id.get());
        let key = VoidSessionKey::new(scope.ledger_id(), actor_id);
        let (session, nonce, candidates) = match bootstrap_void_session(
            key,
            &load,
            self.deps.clock.as_ref(),
            self.deps.nonce_provider.as_ref(),
        ) {
            Ok(outcome) => outcome,
            Err(VoidSessionBootstrapError::NoVoidableCandidates) => {
                return self
                    .edit_command_with_void_model(
                        ctx,
                        command,
                        VoidSurfaceModel::no_candidates(
                            i18n::panel_void_button_label(),
                            Vec::new(),
                            true,
                        ),
                    )
                    .await;
            }
            Err(error) => return Err(error.into()),
        };

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
        let rows = void_candidate_rows(&candidates, &labels, scope.ledger_id())?;
        let action_rows = void_selection_action_rows(nonce, &candidates, &labels);
        self.deps.void_sessions.replace(session);
        self.edit_command_with_void_model(
            ctx,
            command,
            VoidSurfaceModel::selection(i18n::panel_void_button_label(), rows, action_rows, true),
        )
        .await
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

    async fn dispatch_ledger_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = match guard_ledger_interaction(
            command.guild_id,
            command.channel_id,
            self.deps.channels.as_ref(),
        ) {
            Ok(scope) => scope,
            Err(LedgerInteractionGuardError::NotInTrackedChannel { .. }) => {
                return Ok(InteractionDispatch::Ignored);
            }
            Err(error) => return Err(LedgerRouteError::from(error)),
        };

        command
            .defer_ephemeral(&ctx.http)
            .await
            .map_err(discord_call_error(DiscordCallSite::LedgerDeferEphemeral))?;

        let load = self
            .deps
            .thread_loader
            .load(ctx, scope.channel_id(), scope.ledger_id())
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
            vec![build_ledger_empty_page_model(
                ReadViewRoute::LedgerCommand,
                false,
            )]
        } else {
            let model = build_ledger_page_model(LedgerPageInputs {
                route: ReadViewRoute::LedgerCommand,
                views: &views,
                state: load.snapshot().projected().state(),
                labels: &labels,
                ledger_id: scope.ledger_id(),
                uncertain_write: false,
            })?;
            paginate_read_view_model(model)
        };

        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        let actor_id = MemberId(command.user.id.get());
        let session = ReadViewSession::new(
            ReadViewSessionKey {
                ledger_id: scope.ledger_id(),
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

        command
            .edit_response(
                &ctx.http,
                serenity::all::EditInteractionResponse::new()
                    .content(body)
                    .components(components)
                    .allowed_mentions(suppressed_allowed_mentions()),
            )
            .await
            .map_err(discord_call_error(DiscordCallSite::LedgerEditResponse))?;

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
        if parse_expense_session_button_nonce(custom_id, EXPENSE_RECORD_CUSTOM_ID_PREFIX).is_some()
        {
            return self.dispatch_expense_record(ctx, component).await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, READ_VIEW_PREV_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_read_view_navigate(ctx, component, nonce, ReadViewNavigation::Previous)
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, READ_VIEW_NEXT_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_read_view_navigate(ctx, component, nonce, ReadViewNavigation::Next)
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, VOID_PICK_CUSTOM_ID_PREFIX)
        {
            return self.dispatch_void_pick(ctx, component, nonce).await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, VOID_CONFIRM_CUSTOM_ID_PREFIX)
        {
            return self.dispatch_void_confirm(ctx, component, nonce).await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, VOID_RESELECT_CUSTOM_ID_PREFIX)
        {
            return self.dispatch_void_reselect(ctx, component, nonce).await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, VOID_CANCEL_CUSTOM_ID_PREFIX)
        {
            return self.dispatch_void_cancel(ctx, component, nonce).await;
        }
        Ok(InteractionDispatch::Ignored)
    }

    async fn dispatch_read_view_navigate(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        nonce: walicord_application::InteractionNonce,
        direction: ReadViewNavigation,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(LedgerRouteError::from)?;
        let actor_id = MemberId(component.user.id.get());

        let key = ReadViewSessionKey {
            ledger_id: scope.ledger_id(),
            actor_id,
        };
        let now = self.deps.clock.now();
        let session_opt = self
            .deps
            .read_view_sessions
            .access(key, nonce, now)
            .unwrap_or_default();
        let Some(mut session) = session_opt else {
            return Ok(InteractionDispatch::Handled);
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
                    CreateInteractionResponseMessage::new()
                        .content(body)
                        .components(components)
                        .allowed_mentions(suppressed_allowed_mentions()),
                ),
            )
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ReadViewNavUpdateResponse,
            ))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_void_pick(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )?;
        let key = VoidSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
        let Some(session) = self
            .void_session_or_stale_update(ctx, component, key, nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let Some(target_entry_id) = selected_void_target(component) else {
            return self
                .refresh_void_selection(
                    ctx,
                    component,
                    scope.guild_id(),
                    scope.channel_id(),
                    scope.ledger_id(),
                    session,
                    VoidSelectionRenderKind::MissingSelection,
                )
                .await;
        };

        let load = self
            .deps
            .thread_loader
            .load(ctx, scope.channel_id(), scope.ledger_id())
            .await?;
        let candidates = enumerate_void_candidates(&load)?;
        match transition_to_confirm(
            session.clone(),
            target_entry_id,
            &candidates,
            self.deps.clock.as_ref(),
        ) {
            Ok(next_session) => {
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
                let target = candidates
                    .iter()
                    .find(|view| view.entry().id == target_entry_id)
                    .expect("transition_to_confirm verified target is present");
                let model = void_confirmation_model(
                    target,
                    &labels,
                    scope.ledger_id(),
                    next_session.nonce(),
                )?;
                self.deps.void_sessions.replace(next_session);
                self.update_component_with_void_model(ctx, component, model)
                    .await
            }
            Err(VoidConfirmTransitionError::CandidateNotFound { .. }) => {
                self.refresh_void_selection(
                    ctx,
                    component,
                    scope.guild_id(),
                    scope.channel_id(),
                    scope.ledger_id(),
                    session,
                    VoidSelectionRenderKind::StaleTarget(
                        VoidRetargetReason::ExcludedFromCandidates,
                    ),
                )
                .await
            }
            Err(error) => Err(error.into()),
        }
    }

    async fn dispatch_void_reselect(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )?;
        let key = VoidSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
        let Some(session) = self
            .void_session_or_stale_update(ctx, component, key, nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        self.refresh_void_selection(
            ctx,
            component,
            scope.guild_id(),
            scope.channel_id(),
            scope.ledger_id(),
            session,
            VoidSelectionRenderKind::Normal,
        )
        .await
    }

    async fn dispatch_void_cancel(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )?;
        let key = VoidSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
        let Some(_session) = self
            .void_session_or_stale_update(ctx, component, key, nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        self.deps.void_sessions.clear(key);
        self.update_component_message(
            ctx,
            component,
            i18n::void_cancelled_message(),
            Vec::new(),
            DiscordCallSite::VoidUpdateResponse,
        )
        .await
    }

    async fn dispatch_void_confirm(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )?;
        let actor_id = MemberId(component.user.id.get());
        let key = VoidSessionKey::new(scope.ledger_id(), actor_id);
        let Some(session) = self
            .void_session_or_stale_update(ctx, component, key, nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        if !matches!(session.stage(), VoidSessionStage::Confirming) {
            return self
                .reply_component_ephemeral(
                    ctx,
                    component,
                    i18n::void_wrong_stage_copy(),
                    DiscordCallSite::VoidUpdateResponse,
                )
                .await;
        }

        component
            .defer(&ctx.http)
            .await
            .map_err(discord_call_error(DiscordCallSite::VoidDeferComponent))?;

        if self
            .deps
            .uncertain_writes
            .current(scope.ledger_id())
            .is_some()
        {
            return self
                .edit_component_response(
                    ctx,
                    component,
                    uncertain_write_block_message(true, false),
                    DiscordCallSite::VoidEditResponse,
                )
                .await;
        }

        let lock = self.deps.write_coordinator.lock_for(scope.ledger_id());
        let _guard = lock.lock().await;
        let load = self
            .deps
            .thread_loader
            .load(ctx, scope.channel_id(), scope.ledger_id())
            .await?;
        let next_entry_id =
            LedgerEntryId((load.snapshot().canonical_entry_count() as u64).saturating_add(1));
        let target_id = session
            .selection()
            .map(|selection| selection.target_entry_id())
            .ok_or(VoidComposeError::SessionNotConfirming)?;
        let Some(target_view) = enumerate_void_candidates(&load)?
            .into_iter()
            .find(|view| view.entry().id == target_id)
        else {
            self.deps.void_sessions.clear(key);
            return self
                .edit_component_response(
                    ctx,
                    component,
                    i18n::void_target_updated_message(),
                    DiscordCallSite::VoidEditResponse,
                )
                .await;
        };
        let (entry, envelope) = compose_void_entry(
            &session,
            &load,
            scope.ledger_id(),
            actor_id,
            next_entry_id,
            DiscordLedgerSourceDescriptor::void_parent_v1(),
            self.deps.clock.as_ref(),
        )?;
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
        let prepared_body =
            render_public_void_body(&entry, &target_view, scope.ledger_id(), &labels)?;
        let envelope_bytes =
            walicord_application::ledger::canonical_attachment::CanonicalAttachmentCodec::encode_with_pre_self_link_content(
                &envelope,
                Some(prepared_body.as_str()),
            )
            .map_err(|error| {
                LedgerRouteError::Internal(InternalLedgerRouteError::ThreadWrite(
                    StoreWriteError::Prepare(error),
                ))
            })?;
        let retained = RetainedCanonicalWrite::new(
            scope.ledger_id(),
            &envelope,
            envelope_bytes,
            prepared_body.clone(),
            short_summary_for_entry(&entry),
        );
        self.deps.uncertain_writes.set_live(retained).map_err(|_| {
            LedgerRouteError::Internal(InternalLedgerRouteError::UncertainWriteAlreadyLive {
                ledger_id: scope.ledger_id(),
            })
        })?;

        match self
            .deps
            .canonical_store
            .append_authoritative(ctx, scope.channel_id(), &envelope, prepared_body.as_str())
            .await
        {
            Ok(_verified) => {
                self.deps.uncertain_writes.clear(scope.ledger_id());
                self.deps.void_sessions.clear(key);
                self.edit_component_with_void_model(
                    ctx,
                    component,
                    VoidSurfaceModel::success(
                        i18n::void_success_title(),
                        format!("<#{}>", scope.channel_id().get()),
                        Vec::new(),
                        true,
                    ),
                )
                .await
            }
            Err(_error) => {
                self.edit_component_response(
                    ctx,
                    component,
                    uncertain_write_block_message(true, false),
                    DiscordCallSite::VoidEditResponse,
                )
                .await
            }
        }
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
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(LedgerRouteError::from)?;
        let key = ExpenseSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
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
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )?;
        let key = ExpenseSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
        let Some(current) = self.deps.expense_sessions.clear(key) else {
            return self.respond_expense_session_missing(ctx, component).await;
        };

        let roster_snapshot = self
            .deps
            .roster_fetcher
            .fetch(ctx, scope.guild_id(), scope.channel_id())
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

        let button_ids = build_confirmation_button_ids(nonce);
        let model = build_expense_confirmation_surface(
            &basic_info,
            &outcome.snapshot.participants,
            &outcome.defaulted_members,
            &roster_snapshot.display_names,
            &button_ids,
        )?;
        let rendered = DiscordLedgerPresenter::render_expense_step(&model)?;
        let (body, components) = rendered_surface_to_message(rendered);

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

    /// Record the confirmation-stage session as a canonical entry. The actor pressed
    /// 記録する on the confirmation page; the router serializes against the
    /// per-ledger write lock, re-resolves the selection against the live roster
    /// (criterion 81), composes a canonical entry + envelope, registers the
    /// `uncertain_write` retain bytes (criteria 217 / 279), appends via
    /// `DiscordCanonicalLedgerStore`, and clears the retain on a verified
    /// read-back.
    ///
    /// Drift handling: if the live roster has changed since the actor confirmed
    /// (criterion 111), the handler rebuilds the confirmation page in place rather
    /// than appending under stale assumptions.
    async fn dispatch_expense_record(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )?;
        let guild_id = scope.guild_id();
        let channel_id = scope.channel_id();
        let actor_id = MemberId(component.user.id.get());
        let key = ExpenseSessionKey::new(scope.ledger_id(), actor_id);
        let Some(session) = self.deps.expense_sessions.clear(key) else {
            return self.respond_expense_session_missing(ctx, component).await;
        };
        if !matches!(session.stage(), ExpenseSessionStage::InConfirmation) {
            // Defensive: the record button is only rendered on the confirmation page;
            // a non-confirmation stage here means a stale cached interaction. Restart
            // the actor cleanly.
            self.deps.expense_sessions.replace(session);
            return self.respond_expense_session_missing(ctx, component).await;
        }

        let ledger_id = LedgerId(channel_id.get());
        let write_target = ledger_id;

        // Per-ledger serialization (criterion 53 / 115 / 155 / 182): every canonical
        // append for this ledger holds the same async mutex for its whole lifecycle.
        let lock = self.deps.write_coordinator.lock_for(write_target);
        let _guard = lock.lock().await;

        let load_future = self.deps.thread_loader.load(ctx, channel_id, ledger_id);
        let roster_future = self.deps.roster_fetcher.fetch(ctx, guild_id, channel_id);
        let (load_result, roster_result) = tokio::join!(load_future, roster_future);
        let snapshot_load = load_result?;
        let roster_snapshot = roster_result?;

        let next_entry_id = LedgerEntryId(
            (snapshot_load.snapshot().canonical_entry_count() as u64).saturating_add(1),
        );
        let previous_hash = snapshot_load
            .snapshot()
            .current_head_hash()
            .unwrap_or_else(|| {
                walicord_application::ledger::ledger_chain_genesis_sha256_v1(ledger_id)
            });
        let outcome = compose_expense_entry(
            &session,
            &roster_snapshot.roster,
            next_entry_id,
            DiscordLedgerSourceDescriptor::expense_slash_modal_v1(),
            actor_id,
            self.deps.clock.as_ref(),
        )?;

        match outcome {
            RecordTimeOutcome::DriftDetected {
                drift,
                refreshed,
                defaulted_members,
                dropped_overrides: _,
            } => {
                self.refresh_confirmation_for_drift(
                    ctx,
                    component,
                    session,
                    drift,
                    refreshed,
                    defaulted_members,
                    &roster_snapshot.display_names,
                )
                .await
            }
            RecordTimeOutcome::Ready { entry, .. } => {
                self.commit_recorded_entry(
                    ctx,
                    component,
                    write_target,
                    ledger_id,
                    channel_id,
                    previous_hash,
                    entry,
                    &roster_snapshot.display_names,
                )
                .await
            }
        }
    }

    /// Render a fresh confirmation page reflecting the live roster, then leave the
    /// session in `InConfirmation` so the actor can either press 記録する again (now
    /// against the refreshed snapshot) or revise their selection.
    #[allow(clippy::too_many_arguments)]
    async fn refresh_confirmation_for_drift(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        session: ExpenseSession,
        drift: Vec<ParticipantDrift>,
        refreshed: Vec<ExpenseParticipantSelection>,
        defaulted_members: Vec<MemberId>,
        display_names: &HashMap<MemberId, smol_str::SmolStr>,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        // Rebuild the confirmation snapshot from the refreshed participants so the
        // next press observes no drift unless the roster moves again.
        let basic_info = session.draft().basic_info().cloned().ok_or(
            InternalLedgerRouteError::ConfirmationBuild(ConfirmationBuildError::BasicInfoMissing),
        )?;
        let selection = session.draft().selection_state().clone();
        let next_draft = ExpenseDraftSnapshot::empty()
            .with_basic_info(basic_info.clone())
            .with_selection_state(selection)
            .with_confirmation_snapshot(ExpenseConfirmationSnapshot {
                participants: refreshed.clone(),
            });
        let refreshed_session = ExpenseSession::new(
            session.key(),
            ExpenseSessionStage::InConfirmation,
            next_draft,
            session.nonce(),
            self.deps.clock.now(),
        )?;
        let nonce = refreshed_session.nonce();
        self.deps.expense_sessions.replace(refreshed_session);

        let button_ids = build_confirmation_button_ids(nonce);
        let model = build_expense_confirmation_surface(
            &basic_info,
            &refreshed,
            &defaulted_members,
            display_names,
            &button_ids,
        )?;
        let rendered = DiscordLedgerPresenter::render_expense_step(&model)?;
        let (mut body, components) = rendered_surface_to_message(rendered);
        if !drift.is_empty() {
            body.push('\n');
            body.push_str(i18n::expense_participants_drifted_cue());
        }

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

    /// Build the canonical envelope + attachment, register the retain (set_live), post
    /// via `append_authoritative`, then clear the retain on a verified read-back.
    /// Any failure between `set_live` and `clear` leaves the retain Live so lazy retry
    /// (criterion 217 / 279) can later determine whether the post landed.
    #[allow(clippy::too_many_arguments)]
    async fn commit_recorded_entry(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        write_target: WriteTargetKey,
        ledger_id: LedgerId,
        canonical_thread_id: ChannelId,
        previous_hash: walicord_application::ledger::EntryHash,
        entry: LedgerEntry,
        display_names: &HashMap<MemberId, smol_str::SmolStr>,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let envelope: UnverifiedLedgerStoreEnvelope<()> =
            build_canonical_envelope(ledger_id, previous_hash, entry.clone())?;

        let prepared_body = render_public_expense_body(&entry, ledger_id, display_names)?;
        let envelope_bytes =
            walicord_application::ledger::canonical_attachment::CanonicalAttachmentCodec::encode_with_pre_self_link_content(
                &envelope,
                Some(prepared_body.as_str()),
            )
            .map_err(|error| {
                LedgerRouteError::Internal(InternalLedgerRouteError::ThreadWrite(
                    StoreWriteError::Prepare(error),
                ))
            })?;

        let retained = RetainedCanonicalWrite::new(
            write_target,
            &envelope,
            envelope_bytes,
            prepared_body.clone(),
            short_summary_for_entry(&entry),
        );
        self.deps.uncertain_writes.set_live(retained).map_err(|_| {
            LedgerRouteError::Internal(InternalLedgerRouteError::UncertainWriteAlreadyLive {
                ledger_id,
            })
        })?;

        let append_result = self
            .deps
            .canonical_store
            .append_authoritative(ctx, canonical_thread_id, &envelope, prepared_body.as_str())
            .await;
        match append_result {
            Ok(_verified) => {
                self.deps.uncertain_writes.clear(write_target);
                self.respond_record_success(ctx, component).await
            }
            Err(error) => {
                // Retain stays Live: a transport error here is exactly the
                // criterion-217 / 279 case where lazy retry must decide whether the
                // canonical message actually posted.
                Err(error.into())
            }
        }
    }

    async fn respond_record_success(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = CreateInteractionResponse::UpdateMessage(
            CreateInteractionResponseMessage::new()
                .ephemeral(true)
                .allowed_mentions(suppressed_allowed_mentions())
                .content(i18n::expense_recorded_message())
                .components(Vec::new()),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseRecordSuccessAck))?;
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
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )?;
        let key = ExpenseSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
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
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(LedgerRouteError::from)?;
        let key = ExpenseSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
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
        let button_ids = build_selection_step_button_ids(nonce);
        let model = build_expense_selection_step_surface(phase, &button_ids);
        let rendered = DiscordLedgerPresenter::render_expense_step(&model)?;
        let (body, components) = rendered_surface_to_message(rendered);
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
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(LedgerRouteError::from)?;
        let key = ExpenseSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
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
                raw_date: Some(info.effective_date.to_string()),
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
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(LedgerRouteError::from)?;
        let key = ExpenseSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
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
        let scope = guard_ledger_interaction(
            component.guild_id,
            component.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(LedgerRouteError::from)?;
        let key = ExpenseSessionKey::new(scope.ledger_id(), MemberId(component.user.id.get()));
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
        let scope = guard_ledger_interaction(
            modal.guild_id,
            modal.channel_id,
            self.deps.channels.as_ref(),
        )
        .map_err(LedgerRouteError::from)?;
        let guild_id = scope.guild_id();

        let raw = extract_raw_expense_modal_submission(modal)
            .ok_or(InternalLedgerRouteError::ModalSubmissionMissingFields)?;

        match validate_expense_modal_submission(&raw, self.deps.clock.as_ref()) {
            Err(error) => {
                let preserved = ModalRetryPreserved {
                    raw_amount: raw.raw_amount.clone(),
                    raw_note: raw.raw_note.clone(),
                    raw_date: raw.raw_date.clone(),
                };
                self.respond_with_retry_modal(ctx, modal, scope.ledger_id(), preserved, error)
                    .await
            }
            Ok(validated) => {
                let _ = guild_id;
                let key = ExpenseSessionKey::new(scope.ledger_id(), MemberId(modal.user.id.get()));
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
        ledger_id: LedgerId,
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
            ledger_id,
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
        let button_ids = build_selection_step_button_ids(nonce);
        let model =
            build_expense_selection_step_surface(&ExpenseSelectionPhase::Payer, &button_ids);
        let rendered = DiscordLedgerPresenter::render_expense_step(&model)?;
        let (body, components) = rendered_surface_to_message(rendered);
        let response = CreateInteractionResponse::Message(
            CreateInteractionResponseMessage::new()
                .ephemeral(true)
                .allowed_mentions(suppressed_allowed_mentions())
                .content(body)
                .components(components),
        );
        modal
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseModalSuccessAck))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn edit_command_response(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        content: impl Into<String>,
        site: DiscordCallSite,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        command
            .edit_response(
                &ctx.http,
                serenity::all::EditInteractionResponse::new()
                    .content(content)
                    .components(Vec::new())
                    .allowed_mentions(suppressed_allowed_mentions()),
            )
            .await
            .map_err(discord_call_error(site))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn edit_command_with_void_model(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        model: VoidSurfaceModel,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let rendered =
            DiscordLedgerPresenter::render_void_flow(&model).map_err(LedgerRouteError::from)?;
        let (body, components) = rendered_surface_to_message(rendered);
        command
            .edit_response(
                &ctx.http,
                serenity::all::EditInteractionResponse::new()
                    .content(body)
                    .components(components)
                    .allowed_mentions(suppressed_allowed_mentions()),
            )
            .await
            .map_err(discord_call_error(DiscordCallSite::VoidEditResponse))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn update_component_with_void_model(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        model: VoidSurfaceModel,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let rendered =
            DiscordLedgerPresenter::render_void_flow(&model).map_err(LedgerRouteError::from)?;
        let (body, components) = rendered_surface_to_message(rendered);
        self.update_component_message(
            ctx,
            component,
            body,
            components,
            DiscordCallSite::VoidUpdateResponse,
        )
        .await
    }

    async fn edit_component_with_void_model(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        model: VoidSurfaceModel,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let rendered =
            DiscordLedgerPresenter::render_void_flow(&model).map_err(LedgerRouteError::from)?;
        let (body, components) = rendered_surface_to_message(rendered);
        component
            .edit_response(
                &ctx.http,
                serenity::all::EditInteractionResponse::new()
                    .content(body)
                    .components(components)
                    .allowed_mentions(suppressed_allowed_mentions()),
            )
            .await
            .map_err(discord_call_error(DiscordCallSite::VoidEditResponse))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn update_component_message(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        content: impl Into<String>,
        components: Vec<serenity::all::CreateActionRow>,
        site: DiscordCallSite,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    CreateInteractionResponseMessage::new()
                        .content(content)
                        .components(components)
                        .allowed_mentions(suppressed_allowed_mentions()),
                ),
            )
            .await
            .map_err(discord_call_error(site))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn edit_component_response(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        content: impl Into<String>,
        site: DiscordCallSite,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        component
            .edit_response(
                &ctx.http,
                serenity::all::EditInteractionResponse::new()
                    .content(content)
                    .components(Vec::new())
                    .allowed_mentions(suppressed_allowed_mentions()),
            )
            .await
            .map_err(discord_call_error(site))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn reply_component_ephemeral(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        content: impl Into<String>,
        site: DiscordCallSite,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    CreateInteractionResponseMessage::new()
                        .ephemeral(true)
                        .content(content)
                        .allowed_mentions(suppressed_allowed_mentions()),
                ),
            )
            .await
            .map_err(discord_call_error(site))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn void_session_or_stale_update(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        key: VoidSessionKey,
        nonce: walicord_application::InteractionNonce,
    ) -> Result<Option<VoidSession>, LedgerRouteError> {
        match self
            .deps
            .void_sessions
            .access(key, nonce, self.deps.clock.now())
        {
            Ok(Some(session)) => Ok(Some(session)),
            Ok(None) | Err(SessionAccessError::Expired | SessionAccessError::Superseded { .. }) => {
                self.update_component_with_void_model(
                    ctx,
                    component,
                    VoidSurfaceModel::stale_page(
                        i18n::panel_void_button_label(),
                        RecoveryCta::None,
                        None,
                        false,
                        Vec::new(),
                        true,
                    ),
                )
                .await?;
                Ok(None)
            }
            Err(error) => Err(error.into()),
        }
    }

    #[allow(clippy::too_many_arguments)]
    async fn refresh_void_selection(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        guild_id: GuildId,
        channel_id: ChannelId,
        ledger_id: LedgerId,
        session: VoidSession,
        render_kind: VoidSelectionRenderKind,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let load = self
            .deps
            .thread_loader
            .load(ctx, channel_id, ledger_id)
            .await?;
        let candidates = enumerate_void_candidates(&load)?;
        if candidates.is_empty() {
            self.deps.void_sessions.clear(session.key());
            return self
                .update_component_with_void_model(
                    ctx,
                    component,
                    VoidSurfaceModel::no_candidates(
                        i18n::panel_void_button_label(),
                        Vec::new(),
                        true,
                    ),
                )
                .await;
        }
        let roster = self
            .deps
            .roster_fetcher
            .fetch(ctx, guild_id, channel_id)
            .await?;
        let labels = SurfaceMemberLabels::from_member_names(
            roster
                .display_names
                .iter()
                .map(|(member_id, name)| (*member_id, Some(name.as_str()))),
        );
        let refreshed = VoidSession::new(
            session.key(),
            VoidSessionStage::SelectingCandidate,
            None,
            session.nonce(),
            self.deps.clock.now(),
        )
        .expect("selecting void session has no required selection");
        let rows = void_candidate_rows(&candidates, &labels, ledger_id)?;
        let action_rows = void_selection_action_rows(session.nonce(), &candidates, &labels);
        self.deps.void_sessions.replace(refreshed);
        let model = match render_kind {
            VoidSelectionRenderKind::Normal => VoidSurfaceModel::selection(
                i18n::panel_void_button_label(),
                rows,
                action_rows,
                true,
            ),
            VoidSelectionRenderKind::MissingSelection => VoidSurfaceModel::missing_selection(
                i18n::panel_void_button_label(),
                rows,
                action_rows,
                true,
            ),
            VoidSelectionRenderKind::StaleTarget(reason) => VoidSurfaceModel::stale_target(
                i18n::panel_void_button_label(),
                reason,
                rows,
                action_rows,
                true,
            ),
        };
        self.update_component_with_void_model(ctx, component, model)
            .await
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
pub(crate) const EXPENSE_RECORD_CUSTOM_ID_PREFIX: &str = "ledger:expense:record:";
pub(crate) const READ_VIEW_PREV_CUSTOM_ID_PREFIX: &str = "ledger:read-view:prev:";
pub(crate) const READ_VIEW_NEXT_CUSTOM_ID_PREFIX: &str = "ledger:read-view:next:";
pub(crate) const VOID_PICK_CUSTOM_ID_PREFIX: &str = "ledger:void:pick:";
pub(crate) const VOID_CONFIRM_CUSTOM_ID_PREFIX: &str = "ledger:void:confirm:";
pub(crate) const VOID_RESELECT_CUSTOM_ID_PREFIX: &str = "ledger:void:reselect:";
pub(crate) const VOID_CANCEL_CUSTOM_ID_PREFIX: &str = "ledger:void:cancel:";

fn read_view_navigation_row(
    nonce: walicord_application::InteractionNonce,
    current_index: usize,
    total_pages: usize,
) -> serenity::all::CreateActionRow {
    use serenity::all::{ButtonStyle, CreateActionRow, CreateButton};
    let n = nonce.get();
    CreateActionRow::Buttons(vec![
        CreateButton::new(format!("{READ_VIEW_PREV_CUSTOM_ID_PREFIX}{n}"))
            .label(walicord_i18n::picker_previous_page_label())
            .style(ButtonStyle::Secondary)
            .disabled(current_index == 0),
        CreateButton::new(format!("{READ_VIEW_NEXT_CUSTOM_ID_PREFIX}{n}"))
            .label(walicord_i18n::picker_next_page_label())
            .style(ButtonStyle::Secondary)
            .disabled(current_index + 1 >= total_pages),
    ])
}

/// Build the custom_id strings for every button the selection wizard can show. The
/// adapter owns the custom_id format (Discord component identity); the presentation
/// builder takes them as opaque strings.
fn build_selection_step_button_ids(
    nonce: walicord_application::InteractionNonce,
) -> ExpenseSelectionStepButtonIds {
    let n = nonce.get();
    ExpenseSelectionStepButtonIds {
        to_participants: format!("{EXPENSE_TO_PARTICIPANTS_CUSTOM_ID_PREFIX}{n}"),
        source_individual: format!("{EXPENSE_SOURCE_INDIVIDUAL_CUSTOM_ID_PREFIX}{n}"),
        source_roles: format!("{EXPENSE_SOURCE_ROLES_CUSTOM_ID_PREFIX}{n}"),
        source_members: format!("{EXPENSE_SOURCE_MEMBERS_CUSTOM_ID_PREFIX}{n}"),
        to_weights: format!("{EXPENSE_TO_WEIGHTS_CUSTOM_ID_PREFIX}{n}"),
        to_confirm: format!("{EXPENSE_TO_CONFIRM_CUSTOM_ID_PREFIX}{n}"),
        back: format!("{EXPENSE_BACK_CUSTOM_ID_PREFIX}{n}"),
        cancel: format!("{EXPENSE_CANCEL_CUSTOM_ID_PREFIX}{n}"),
    }
}

/// Build the custom_id strings for the confirmation-page buttons.
fn build_confirmation_button_ids(
    nonce: walicord_application::InteractionNonce,
) -> ExpenseConfirmationButtonIds {
    let n = nonce.get();
    ExpenseConfirmationButtonIds {
        record: format!("{EXPENSE_RECORD_CUSTOM_ID_PREFIX}{n}"),
        modify_selection: format!("{EXPENSE_MODIFY_SELECTION_CUSTOM_ID_PREFIX}{n}"),
        basic_edit: format!("{EXPENSE_BASIC_EDIT_CUSTOM_ID_PREFIX}{n}"),
        cancel: format!("{EXPENSE_CANCEL_CUSTOM_ID_PREFIX}{n}"),
    }
}

fn format_money_for_modal(money: walicord_domain::Money) -> String {
    money.to_string()
}

/// Short canonical summary string retained alongside an in-flight write (criterion 217
/// / 279). The lazy retry scan uses it as a debug breadcrumb; the value must be stable
/// across the post / read-back / scan cycle so the retain comparison still matches.
fn short_summary_for_entry(entry: &LedgerEntry) -> String {
    format!("entry:{}", entry.id.0)
}

/// Render the public canonical message body for a freshly composed expense entry.
/// Drives `DiscordLedgerPresenter::render_public_entry` so the rendered string passes
/// the canonical recovery-shape validation and matches the body that goes into the
/// hash-protected attachment.
#[allow(clippy::result_large_err)] // LedgerRouteError is the project's standard error envelope.
fn render_public_expense_body(
    entry: &LedgerEntry,
    ledger_id: LedgerId,
    display_names: &HashMap<MemberId, smol_str::SmolStr>,
) -> Result<String, LedgerRouteError> {
    use walicord_application::ledger::{LedgerEvent, MemberAmount};
    use walicord_presentation::discord_ledger::{
        ParticipantShareRow, PublicCanonicalMessageModel, PublicExpenseMessageModel,
        RecoveryReference, SafeLiteralText,
    };

    let LedgerEvent::ExpenseRecorded(event) = &entry.event else {
        // The record path only composes ExpenseRecorded entries; landing here means
        // compose_expense_entry produced a different event kind, which is a programmer
        // error rather than a runtime case.
        unreachable!(
            "expense record path produced non-expense entry {:?}",
            entry.id
        );
    };

    let paid_by: &[MemberAmount] = event.paid_by();
    let payer_member_id = paid_by
        .first()
        .map(|amount| amount.member_id)
        .ok_or_else(|| {
            LedgerRouteError::Internal(InternalLedgerRouteError::ConfirmationBuild(
                ConfirmationBuildError::PayerNotSelected,
            ))
        })?;
    let total_amount: walicord_domain::Money = paid_by.iter().map(|amount| amount.amount).sum();

    let labels = SurfaceMemberLabels::from_member_names(
        std::iter::once((
            payer_member_id,
            display_names.get(&payer_member_id).map(|s| s.as_str()),
        ))
        .chain(event.owed_by().iter().map(|amount| {
            (
                amount.member_id,
                display_names.get(&amount.member_id).map(|s| s.as_str()),
            )
        })),
    );

    let payer_display_name = labels
        .member(payer_member_id)
        .map(|label| label.visible().clone())
        .unwrap_or_else(|| {
            SafeLiteralText::from_roster_label(
                &i18n::unknown_user_label(payer_member_id.0).to_string(),
            )
            .expect("unknown_user_label is a fixed fallback that always sanitises")
        });

    let participant_rows: Vec<ParticipantShareRow> = event
        .owed_by()
        .iter()
        .map(|amount| ParticipantShareRow {
            display_name: labels
                .member(amount.member_id)
                .map(|label| label.visible().clone())
                .unwrap_or_else(|| {
                    SafeLiteralText::from_roster_label(
                        &i18n::unknown_user_label(amount.member_id.0).to_string(),
                    )
                    .expect("unknown_user_label is a fixed fallback that always sanitises")
                }),
            share_amount: format_money_for_modal(amount.amount),
        })
        .collect();

    let note = event.note().map(|note| {
        // ExpenseNote validates canonical form; SafeLiteralText must accept it.
        SafeLiteralText::from_note(note.as_str())
            .expect("validated ExpenseNote should always produce a SafeLiteralText")
    });

    let actor_member_id = entry
        .metadata
        .recorded_by
        .expect("composed entry always records `recorded_by`");
    let actor_labels = SurfaceMemberLabels::from_member_names(std::iter::once((
        actor_member_id,
        display_names.get(&actor_member_id).map(|s| s.as_str()),
    )));
    let actor_display_name = actor_labels
        .member(actor_member_id)
        .map(|label| label.visible().clone())
        .unwrap_or_else(|| {
            SafeLiteralText::from_roster_label(
                &i18n::unknown_user_label(actor_member_id.0).to_string(),
            )
            .expect("unknown_user_label is a fixed fallback that always sanitises")
        });

    let recorded_at = entry
        .metadata
        .recorded_at
        .expect("composed entry always records `recorded_at`");
    let effective_date = entry
        .metadata
        .effective_date
        .expect("composed expense entry always records `effective_date`");

    let model = PublicCanonicalMessageModel::Expense(PublicExpenseMessageModel {
        entry_id: entry.id,
        effective_date,
        payer_display_name,
        amount: format_money_for_modal(total_amount),
        participant_rows,
        note,
        actor_display_name,
        recorded_at: BusinessDateTime::from_system_time(recorded_at),
        // message_link stays None: the self-link enrichment that edits the posted
        // message to embed its own permalink lives behind `display_drift_guard`
        // (criterion 209-212) and lands as a follow-up commit.
        recovery_reference: RecoveryReference {
            ledger_id_short: format!("{:x}", ledger_id.0),
            entry_id: entry.id,
            message_link: None,
        },
    });

    let rendered = DiscordLedgerPresenter::render_public_entry(&model).map_err(|error| {
        LedgerRouteError::Internal(InternalLedgerRouteError::PanelRender(error))
    })?;
    Ok(rendered.body().to_owned())
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the project's standard error envelope.
fn render_public_settlement_body(
    entry: &LedgerEntry,
    ledger_id: LedgerId,
    display_names: &HashMap<MemberId, smol_str::SmolStr>,
) -> Result<String, LedgerRouteError> {
    let walicord_application::ledger::LedgerEvent::NormalizedSettlementPlanRecorded(event) =
        &entry.event
    else {
        unreachable!("settle path produced non-settlement entry {:?}", entry.id);
    };

    let labels =
        SurfaceMemberLabels::from_member_names(event.transfers().iter().flat_map(|transfer| {
            [
                (
                    transfer.from,
                    display_names.get(&transfer.from).map(|s| s.as_str()),
                ),
                (
                    transfer.to,
                    display_names.get(&transfer.to).map(|s| s.as_str()),
                ),
            ]
        }));
    let transfers = event
        .transfers()
        .iter()
        .map(|transfer| TransferRow {
            from_display_name: labels.safe_member_label(transfer.from),
            to_display_name: labels.safe_member_label(transfer.to),
            amount: format_money_for_modal(transfer.amount),
        })
        .collect();

    let actor_member_id = entry
        .metadata
        .recorded_by
        .expect("composed settlement entry always records `recorded_by`");
    let actor_labels = SurfaceMemberLabels::from_member_names(std::iter::once((
        actor_member_id,
        display_names.get(&actor_member_id).map(|s| s.as_str()),
    )));
    let actor_display_name = actor_labels.safe_member_label(actor_member_id);
    let recorded_at = entry
        .metadata
        .recorded_at
        .expect("composed settlement entry always records `recorded_at`");

    let model = PublicCanonicalMessageModel::Settlement(PublicSettlementMessageModel {
        entry_id: entry.id,
        recorded_date: walicord_application::ledger::LedgerEffectiveDate::from_system_time(
            recorded_at,
        ),
        transfers,
        actor_display_name,
        recorded_at: BusinessDateTime::from_system_time(recorded_at),
        recovery_reference: RecoveryReference {
            ledger_id_short: format!("{:08x}", ledger_id.0),
            entry_id: entry.id,
            message_link: None,
        },
    });

    let rendered = DiscordLedgerPresenter::render_public_entry(&model).map_err(|error| {
        LedgerRouteError::Internal(InternalLedgerRouteError::PanelRender(error))
    })?;
    Ok(rendered.body().to_owned())
}

fn settle_attempt_error_message(error: &SettleAttemptError) -> &'static str {
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

fn should_clear_preview_after_settle_error(error: &SettleAttemptError) -> bool {
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

fn uncertain_write_block_message(preserve_input: bool, preserve_preview: bool) -> String {
    let mut message = String::from(i18n::uncertain_write_block_message());
    if preserve_input {
        message.push('\n');
        message.push_str(i18n::uncertain_write_input_preserved_message());
    }
    if preserve_preview {
        message.push('\n');
        message.push_str(i18n::uncertain_write_preview_preserved_message());
    }
    message
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VoidSelectionRenderKind {
    Normal,
    MissingSelection,
    StaleTarget(VoidRetargetReason),
}

fn selected_void_target(component: &ComponentInteraction) -> Option<LedgerEntryId> {
    match &component.data.kind {
        ComponentInteractionDataKind::StringSelect { values } => values
            .first()
            .and_then(|value| value.parse::<u64>().ok())
            .map(LedgerEntryId),
        _ => None,
    }
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
fn void_candidate_rows(
    candidates: &[VerifiedLedgerEntryView],
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
) -> Result<Vec<VoidCandidateRow>, LedgerRouteError> {
    candidates
        .iter()
        .map(|view| {
            Ok(VoidCandidateRow {
                summary: summary_for_view(view, labels)?,
                recovery_reference: void_recovery_reference(view, ledger_id),
            })
        })
        .collect()
}

fn void_recovery_reference(
    view: &VerifiedLedgerEntryView,
    ledger_id: LedgerId,
) -> RecoveryReference {
    RecoveryReference {
        ledger_id_short: format!("{:08x}", ledger_id.0),
        entry_id: view.entry().id,
        message_link: Some(view.message_link().to_owned()),
    }
}

fn void_selection_action_rows(
    nonce: walicord_application::InteractionNonce,
    candidates: &[VerifiedLedgerEntryView],
    labels: &SurfaceMemberLabels,
) -> Vec<SurfaceActionRow> {
    let n = nonce.get();
    vec![SurfaceActionRow::Select(SurfaceSelectMenu {
        custom_id: format!("{VOID_PICK_CUSTOM_ID_PREFIX}{n}"),
        placeholder: Some(i18n::void_select_placeholder().to_owned()),
        options: candidates
            .iter()
            .map(|view| SurfaceSelectOption {
                value: view.entry().id.0.to_string(),
                label: void_candidate_select_label(view, labels),
                description: None,
                selected: false,
            })
            .collect(),
        min_values: 1,
        max_values: 1,
        disabled: false,
    })]
}

fn void_confirmation_action_rows(
    nonce: walicord_application::InteractionNonce,
) -> Vec<SurfaceActionRow> {
    let n = nonce.get();
    vec![SurfaceActionRow::Buttons(vec![
        SurfaceButton::Interactive {
            label: i18n::void_confirm_label().to_owned(),
            custom_id: format!("{VOID_CONFIRM_CUSTOM_ID_PREFIX}{n}"),
            style: SurfaceInteractiveButtonStyle::Danger,
            disabled: false,
        },
        SurfaceButton::Interactive {
            label: i18n::void_reselect_label().to_owned(),
            custom_id: format!("{VOID_RESELECT_CUSTOM_ID_PREFIX}{n}"),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: false,
        },
        SurfaceButton::Interactive {
            label: i18n::void_cancel_label().to_owned(),
            custom_id: format!("{VOID_CANCEL_CUSTOM_ID_PREFIX}{n}"),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: false,
        },
    ])]
}

fn void_candidate_select_label(
    view: &VerifiedLedgerEntryView,
    labels: &SurfaceMemberLabels,
) -> String {
    use std::fmt::Write as _;
    use walicord_application::ledger::LedgerEvent;
    use walicord_domain::Money;

    let mut label = String::new();
    match &view.entry().event {
        LedgerEvent::ExpenseRecorded(event) => {
            let payer = event
                .paid_by()
                .first()
                .map(|paid| labels.safe_member_label(paid.member_id));
            let amount = event
                .paid_by()
                .iter()
                .map(|paid| paid.amount)
                .sum::<Money>();
            let _ = match payer {
                Some(payer) => write!(label, "#{} 経費 {amount}円 {payer}", view.entry().id.0),
                None => write!(label, "#{} 経費 {amount}円", view.entry().id.0),
            };
        }
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
            let _ = write!(label, "#{} 清算", view.entry().id.0);
            if let Some(first) = event.transfers().first() {
                let _ = write!(
                    label,
                    " {}->{} {}円",
                    labels.safe_member_label(first.from),
                    labels.safe_member_label(first.to),
                    first.amount
                );
            }
            let additional = event.transfers().len().saturating_sub(1);
            if additional > 0 {
                let _ = write!(label, " {}", i18n::additional_items(additional));
            }
        }
        _ => {
            let _ = write!(label, "#{}", view.entry().id.0);
        }
    }
    label
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
fn void_confirmation_model(
    target: &VerifiedLedgerEntryView,
    labels: &SurfaceMemberLabels,
    ledger_id: LedgerId,
    nonce: walicord_application::InteractionNonce,
) -> Result<VoidSurfaceModel, LedgerRouteError> {
    Ok(VoidSurfaceModel::confirmation(
        i18n::void_confirmation_title(),
        VoidConfirmationRecap {
            summary: summary_for_view(target, labels)?,
            total_amount: void_confirmation_total_amount(target.entry())?,
            recovery_reference: void_recovery_reference(target, ledger_id),
        },
        void_confirmation_action_rows(nonce),
        true,
    ))
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
fn void_confirmation_total_amount(entry: &LedgerEntry) -> Result<String, LedgerRouteError> {
    use walicord_application::ledger::LedgerEvent;
    use walicord_domain::Money;

    match &entry.event {
        LedgerEvent::ExpenseRecorded(event) => Ok(event
            .paid_by()
            .iter()
            .map(|paid| paid.amount)
            .sum::<Money>()
            .to_string()),
        LedgerEvent::NormalizedSettlementPlanRecorded(event) => Ok(event
            .transfers()
            .iter()
            .map(|transfer| transfer.amount)
            .sum::<Money>()
            .to_string()),
        _ => Err(LedgerRouteError::Internal(
            InternalLedgerRouteError::VoidCompose(VoidComposeError::TargetNoLongerVoidable {
                target_entry_id: entry.id,
            }),
        )),
    }
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
fn render_public_void_body(
    entry: &LedgerEntry,
    target: &VerifiedLedgerEntryView,
    ledger_id: LedgerId,
    labels: &SurfaceMemberLabels,
) -> Result<String, LedgerRouteError> {
    let actor_member_id = entry
        .metadata
        .recorded_by
        .expect("composed void entry always records `recorded_by`");
    let recorded_at = entry
        .metadata
        .recorded_at
        .expect("composed void entry always records `recorded_at`");
    let model = PublicCanonicalMessageModel::Void(PublicVoidMessageModel {
        entry_id: entry.id,
        voider_display_name: labels.safe_member_label(actor_member_id),
        voided_at: BusinessDateTime::from_system_time(recorded_at),
        original_summary: summary_for_view(target, labels)?,
        recorded_at: BusinessDateTime::from_system_time(recorded_at),
        recovery_reference: RecoveryReference {
            ledger_id_short: format!("{:08x}", ledger_id.0),
            entry_id: entry.id,
            message_link: None,
        },
    });
    let rendered = DiscordLedgerPresenter::render_public_entry(&model).map_err(|error| {
        LedgerRouteError::Internal(InternalLedgerRouteError::PanelRender(error))
    })?;
    Ok(rendered.body().to_owned())
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
    use rstest::rstest;
    use std::time::UNIX_EPOCH;
    use walicord_application::{
        PreviewInstanceId,
        ledger::{
            AllocationSnapshot, ExpenseRecorded, MemberAmount, NormalizedSettlementPlanRecorded,
            ledger_chain_genesis_sha256_v1,
        },
    };
    use walicord_domain::{Money, Transfer};

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum ExpectedClearPolicy {
        Clear,
        Preserve,
    }

    impl From<bool> for ExpectedClearPolicy {
        fn from(value: bool) -> Self {
            if value { Self::Clear } else { Self::Preserve }
        }
    }

    fn member_amount(member_id: u64, amount: i64) -> MemberAmount {
        MemberAmount {
            member_id: MemberId(member_id),
            amount: Money::from_i64(amount),
        }
    }

    fn transfer(from: u64, to: u64, amount: i64) -> Transfer {
        Transfer {
            from: MemberId(from),
            to: MemberId(to),
            amount: Money::from_i64(amount),
        }
    }

    fn expense_entry(amount: i64) -> LedgerEntry {
        LedgerEntry::expense(
            LedgerEntryId(1),
            ExpenseRecorded::new(
                vec![member_amount(1, amount)],
                vec![member_amount(2, amount)],
                None,
            )
            .expect("expense should be valid"),
            AllocationSnapshot::Even,
        )
        .expect("expense entry should be valid")
    }

    fn settlement_entry(amount: i64) -> LedgerEntry {
        LedgerEntry::non_expense(
            LedgerEntryId(2),
            NormalizedSettlementPlanRecorded::new(vec![transfer(2, 1, amount)])
                .expect("settlement should be valid"),
        )
    }

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

    #[rstest]
    #[case::no_preview(
        SettleAttemptError::NoPreviewStored,
        i18n::review_preview_required_message()
    )]
    #[case::stale_head(
        SettleAttemptError::StaleHead {
            stored_head: ledger_chain_genesis_sha256_v1(LedgerId(1)),
            observed_head: None,
        },
        i18n::stale_settlement_preview_message()
    )]
    #[case::expired(
        SettleAttemptError::Expired {
            now: UNIX_EPOCH,
            expires_at: UNIX_EPOCH,
        },
        i18n::stale_settlement_preview_message()
    )]
    #[case::preview_not_delivered(
        SettleAttemptError::Record(
            walicord_application::ledger::SettlementRecordError::PreviewNotDelivered
        ),
        i18n::settlement_preview_not_delivered_message()
    )]
    #[case::commit_in_progress(
        SettleAttemptError::Store(PreviewStoreError::CommitInProgress {
            preview_instance_id: PreviewInstanceId::new(7).expect("non-zero preview instance"),
        }),
        i18n::uncertain_write_block_message()
    )]
    fn settle_attempt_error_maps_to_user_message(
        #[case] error: SettleAttemptError,
        #[case] expected: &'static str,
    ) {
        assert_eq!(settle_attempt_error_message(&error), expected);
    }

    #[rstest]
    #[case::stale_head(
        SettleAttemptError::StaleHead {
            stored_head: ledger_chain_genesis_sha256_v1(LedgerId(1)),
            observed_head: None,
        },
        ExpectedClearPolicy::Clear
    )]
    #[case::no_preview(SettleAttemptError::NoPreviewStored, ExpectedClearPolicy::Preserve)]
    #[case::commit_in_progress(
        SettleAttemptError::Store(PreviewStoreError::CommitInProgress {
            preview_instance_id: PreviewInstanceId::new(7).expect("non-zero preview instance"),
        }),
        ExpectedClearPolicy::Preserve
    )]
    fn settle_attempt_error_clear_policy_is_explicit(
        #[case] error: SettleAttemptError,
        #[case] expected: ExpectedClearPolicy,
    ) {
        assert_eq!(
            ExpectedClearPolicy::from(should_clear_preview_after_settle_error(&error)),
            expected
        );
    }

    #[test]
    fn uncertain_write_block_message_preserves_preview_when_requested() {
        let actual = uncertain_write_block_message(false, true);

        assert!(actual.contains(i18n::uncertain_write_block_message()));
        assert!(actual.contains(i18n::uncertain_write_preview_preserved_message()));
        assert!(!actual.contains(i18n::uncertain_write_input_preserved_message()));
    }

    #[rstest]
    #[case::expense(expense_entry(1200), "1200")]
    #[case::settlement(settlement_entry(900), "900")]
    fn void_confirmation_total_amount_uses_recorded_amount(
        #[case] entry: LedgerEntry,
        #[case] expected: &str,
    ) {
        let actual =
            void_confirmation_total_amount(&entry).expect("entry should support void confirmation");

        assert_eq!(actual, expected);
    }
}
