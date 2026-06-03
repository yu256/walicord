use serenity::{
    all::{
        ChannelId, CommandInteraction, ComponentInteraction, ComponentInteractionDataKind,
        CreateActionRow, CreateButton, CreateInputText, CreateInteractionResponse, CreateModal,
        GuildId, InputTextStyle, ModalInteraction, Permissions, UserId,
    },
    async_trait,
    prelude::Context,
};
use std::{borrow::Cow, collections::HashMap, fmt::Write as _, sync::Arc, time::Duration};
use walicord_application::{
    Clock, NonceProvider, SettlementPlanner,
    ledger::{
        DiscordLedgerSourceDescriptor, ExpenseAuthoringError, LedgerEntry, LedgerEntryId, LedgerId,
        UnverifiedLedgerStoreEnvelope,
        expense_session::{
            ClaimedExpenseSession, ExpenseConfirmationSnapshot, ExpenseDraftScopeId,
            ExpenseDraftSnapshot, ExpenseLaunchOrigin, ExpenseModalIntent,
            ExpenseModalSubmissionBinding, ExpenseModalSubmissionBindingStore,
            ExpenseParticipantSelection, ExpensePickerKind, ExpenseSelectionPhase,
            ExpenseSelectionState, ExpenseSession, ExpenseSessionConstructionError,
            ExpenseSessionKey, ExpenseSessionStage, ExpenseSessionStore, ModalRetryBinding,
            ModalRetryBindingStore, ModalRetryPreserved, PickerSnapshotId, SessionAccessError,
            VoidSession, VoidSessionKey, VoidSessionStage, VoidSessionStore,
        },
        observability::LedgerObservabilityEvent,
        participant_resolution::{
            ParticipantDrift, RosterSnapshot, resolve_selection_against_roster,
        },
    },
};
use walicord_domain::model::{MemberId, RoleId};
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    BusinessDateTime, DiscordLedgerPresenter, ExpenseConfirmationButtonIds,
    ExpenseSelectionStepButtonIds, LedgerPageInputs, PublicCanonicalMessageModel,
    PublicSettlementMessageModel, PublicVoidMessageModel, ReadViewBuildError, ReadViewPageModel,
    ReadViewRoute, RecoveryCta, RecoveryReference, RenderBudgetError, SafeLiteralText,
    SurfaceActionRow, SurfaceButton, SurfaceInteractiveButtonStyle, SurfaceMemberLabels,
    SurfaceSelectMenu, SurfaceSelectOption, TransferRow, VoidCandidateRow, VoidConfirmationRecap,
    VoidRetargetReason, VoidSurfaceModel, build_expense_confirmation_surface,
    build_expense_selection_step_surface, build_ledger_empty_page_model, build_ledger_page_model,
    build_review_empty_page_model, build_review_no_transfers_page_model, build_review_page_model,
    paginate_read_view_model, summary_for_view, truncate_component_label, validate_custom_id,
    validate_modal_title, validate_text_input_label, validate_text_input_placeholder,
};

use crate::channel::ChannelManager;

use super::{
    adapters::DiscordCanonicalThreadLocator,
    expense_modal_open::{
        ExpenseModalBuildError, ExpenseModalCustomIdMatch, ExpenseModalPrefill,
        build_expense_modal_response, build_expense_weight_modal_response,
        extract_raw_expense_modal_submission, parse_expense_modal_custom_id,
        parse_expense_weight_modal_custom_id, parse_expense_weight_modal_submission,
    },
    locator::{
        CanonicalThreadBinding, CanonicalThreadLocatorState, LocatorError,
        LocatorRecoveryReference, TrackedParentKey,
    },
    observability::{
        DiscordLedgerObservability, DiscordLedgerObservabilityEvent, PermissionAction,
    },
    panel::{
        LEDGER_PANEL_EXPENSE_ID, LEDGER_PANEL_LEDGER_ID, LEDGER_PANEL_REVIEW_ID,
        LEDGER_PANEL_VOID_ID, render_panel_post_message_for_locator_state,
    },
    permissions::{
        LedgerRefreshAcknowledgement, RuntimePermissionScope, missing_runtime_permissions_for,
        render_ledger_refresh_acknowledgement, render_ledger_refresh_uncertain_write_message,
    },
    projection::{CanonicalLoadFailure, CanonicalLoadRoute},
    response_writer::{
        deferred_ephemeral_interaction_response_message, rendered_surface_to_message,
        safe_edit_interaction_response, safe_ephemeral_interaction_response_message,
        safe_interaction_response_message,
    },
    route_guard::{
        LedgerInteractionGuardError, LedgerInteractionScope,
        guard_ledger_interaction_resolving_parent,
    },
    store::{
        DiscordCanonicalLedgerStore, StoreLoadError, StoreWriteError, VerifiedLedgerThreadLoad,
        serenity_error_is_read_denied,
    },
};

use walicord_application::ledger::{
    expense_flow::{
        ConfirmationBuildError, NavigationError, apply_modified_basic_info,
        bootstrap_expense_session, build_confirmation_for_session, clear_picker_selection,
        navigate_back, navigate_modify_selection, navigate_to_phase, replace_individual_members,
        replace_payer, replace_selected_roles, replace_weight_overrides, set_picker_view_state,
        toggle_members_group,
    },
    expense_modal::{ExpenseModalValidationError, validate_expense_modal_submission},
    expense_write::{
        ExpenseWriteOrchestrationError, RecordTimeOutcome, build_canonical_envelope,
        compose_expense_entry,
    },
    preview_store::{
        PreviewCommitGuard, PreviewStore, PreviewStoreError, PreviewStoreKey,
        PreviewStoreTransition,
    },
    projection::{NextLedgerEntryIdError, VerifiedLedgerEntryView, project_verified_entries},
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
        BootstrapWriteTarget, ExactEnvelopeScanScope, RetainedCanonicalWrite, ScanCompleteness,
        UncertainWriteRegistry, UncertainWriteResolution, UncertainWriteState, WriteCoordinator,
        WriteTargetKey,
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
    pub locator: Arc<DiscordCanonicalThreadLocator>,
    pub thread_creator: Arc<dyn LedgerCanonicalThreadCreator>,
    pub expense_sessions: Arc<ExpenseSessionStore>,
    pub void_sessions: Arc<VoidSessionStore>,
    pub modal_retries: Arc<ModalRetryBindingStore>,
    pub modal_submissions: Arc<ExpenseModalSubmissionBindingStore>,
    pub preview_store: Arc<PreviewStore>,
    pub read_view_sessions: Arc<ReadViewSessionStore<ReadViewPageModel>>,
    pub write_coordinator: Arc<WriteCoordinator>,
    pub uncertain_writes: Arc<UncertainWriteRegistry>,
    pub planner: Arc<dyn SettlementPlanner>,
    pub canonical_store: Arc<DiscordCanonicalLedgerStore>,
    pub observability: Arc<dyn DiscordLedgerObservability>,
    pub bot_user_id: UserId,
}

trait LedgerInteractionActor {
    fn ledger_actor_id(&self) -> MemberId;
    fn ledger_actor_permissions(&self) -> Option<Permissions>;
}

impl LedgerInteractionActor for CommandInteraction {
    fn ledger_actor_id(&self) -> MemberId {
        MemberId(self.user.id.get())
    }

    fn ledger_actor_permissions(&self) -> Option<Permissions> {
        self.member.as_deref().and_then(|member| member.permissions)
    }
}

impl LedgerInteractionActor for ComponentInteraction {
    fn ledger_actor_id(&self) -> MemberId {
        MemberId(self.user.id.get())
    }

    fn ledger_actor_permissions(&self) -> Option<Permissions> {
        self.member.as_ref().and_then(|member| member.permissions)
    }
}

impl LedgerInteractionActor for ModalInteraction {
    fn ledger_actor_id(&self) -> MemberId {
        MemberId(self.user.id.get())
    }

    fn ledger_actor_permissions(&self) -> Option<Permissions> {
        self.member.as_ref().and_then(|member| member.permissions)
    }
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
    ) -> Result<VerifiedLedgerThreadLoad, Arc<StoreLoadError>>;
}

#[async_trait]
pub trait LedgerCanonicalThreadCreator: Send + Sync {
    async fn create(
        &self,
        ctx: &Context,
        tracked_parent_channel_id: ChannelId,
    ) -> Result<ChannelId, serenity::Error>;
}

#[derive(Debug, Clone)]
pub struct RouterRosterSnapshot {
    pub roster: RosterSnapshot,
    pub display_names: HashMap<MemberId, smol_str::SmolStr>,
    pub role_display_names: HashMap<RoleId, smol_str::SmolStr>,
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
    #[error("settlement confirmation requires a ledger thread")]
    SettleThreadOnly,
    #[error("permission denied: {0}")]
    Permission(Cow<'static, str>),
    #[error("internal failure: {0}")]
    Internal(#[from] InternalLedgerRouteError),
}

impl LedgerRouteError {
    pub fn user_message(&self) -> &str {
        match self {
            Self::GuildOnly => i18n::guild_only_command_message(),
            Self::NotInTrackedChannel => i18n::CHANNEL_NOT_TRACKED,
            Self::SettleThreadOnly => i18n::settlement_thread_only_message(),
            Self::Permission(message) => message,
            Self::Internal(InternalLedgerRouteError::ThreadLookup(LocatorError::Fetch {
                ..
            })) => i18n::ledger_retryable_load_message(),
            Self::Internal(InternalLedgerRouteError::ThreadLoad(error))
                if matches!(error.as_ref(), StoreLoadError::Fetch(_)) =>
            {
                i18n::ledger_retryable_load_message()
            }
            Self::Internal(InternalLedgerRouteError::ThreadLookup(LocatorError::Permission {
                ..
            })) => i18n::ledger_permission_failed_message(),
            Self::Internal(InternalLedgerRouteError::ThreadLoad(error))
                if matches!(error.as_ref(), StoreLoadError::Permission(_)) =>
            {
                i18n::ledger_permission_failed_message()
            }
            Self::Internal(InternalLedgerRouteError::CanonicalLoad { failure, .. }) => failure
                .user_message()
                .unwrap_or_else(|| i18n::ledger_thread_prepare_failed_message()),
            Self::Internal(InternalLedgerRouteError::ExistingLedgerNotReady) => {
                i18n::ledger_thread_prepare_failed_message()
            }
            Self::Internal(_) => i18n::ledger_thread_prepare_failed_message(),
        }
    }
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
    ThreadLoad(Arc<StoreLoadError>),
    #[error("canonical thread load ({failure:?}): {error}")]
    CanonicalLoad {
        failure: CanonicalLoadFailure,
        #[source]
        error: Arc<StoreLoadError>,
    },
    #[error("canonical thread lookup: {0}")]
    ThreadLookup(#[from] LocatorError),
    #[error("canonical thread lookup did not resolve an existing ledger")]
    ExistingLedgerNotReady,
    #[error("canonical thread bootstrap lock timed out")]
    BootstrapTimeout,
    #[error("interaction channel scope lookup failed for {observed}: {message}")]
    ChannelScopeLookup {
        observed: ChannelId,
        message: String,
    },
    #[error("canonical thread write: {0}")]
    ThreadWrite(#[from] StoreWriteError),
    #[error("projection consistency: {0}")]
    Projection(#[from] walicord_application::ledger::projection::ProjectionConsistencyError),
    #[error("ledger entry id allocation: {0}")]
    NextEntryId(#[from] NextLedgerEntryIdError),
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
    #[error("expense navigation landed on non-selection stage: {observed_stage:?}")]
    PostNavigationStageInvariant { observed_stage: ExpenseSessionStage },
    #[error("component selection parse: {0}")]
    ComponentSelectionParse(#[from] ComponentSelectionParseError),
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
            LedgerInteractionGuardError::ChannelScopeLookup { observed, message } => {
                Self::Internal(InternalLedgerRouteError::ChannelScopeLookup { observed, message })
            }
        }
    }
}

impl From<ExpenseModalBuildError> for LedgerRouteError {
    fn from(error: ExpenseModalBuildError) -> Self {
        Self::Internal(error.into())
    }
}

impl From<ComponentSelectionParseError> for LedgerRouteError {
    fn from(error: ComponentSelectionParseError) -> Self {
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
        Self::Internal(InternalLedgerRouteError::ThreadLoad(Arc::new(error)))
    }
}

impl From<Arc<StoreLoadError>> for LedgerRouteError {
    fn from(error: Arc<StoreLoadError>) -> Self {
        Self::Internal(InternalLedgerRouteError::ThreadLoad(error))
    }
}

impl From<LocatorError> for LedgerRouteError {
    fn from(error: LocatorError) -> Self {
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

impl From<NextLedgerEntryIdError> for LedgerRouteError {
    fn from(error: NextLedgerEntryIdError) -> Self {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PanelLauncher {
    Expense,
    Review,
    Ledger,
    Void,
}

fn panel_launcher(custom_id: &str) -> Option<PanelLauncher> {
    match custom_id {
        LEDGER_PANEL_EXPENSE_ID => Some(PanelLauncher::Expense),
        LEDGER_PANEL_REVIEW_ID => Some(PanelLauncher::Review),
        LEDGER_PANEL_LEDGER_ID => Some(PanelLauncher::Ledger),
        LEDGER_PANEL_VOID_ID => Some(PanelLauncher::Void),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy)]
enum DeferredEphemeralInteraction<'a> {
    Command(&'a CommandInteraction),
    Component(&'a ComponentInteraction),
}

impl DeferredEphemeralInteraction<'_> {
    fn user_id(&self) -> serenity::all::UserId {
        match self {
            Self::Command(command) => command.user.id,
            Self::Component(component) => component.user.id,
        }
    }

    async fn defer(&self, ctx: &Context, site: DiscordCallSite) -> Result<(), LedgerRouteError> {
        match self {
            Self::Command(command) => command
                .defer_ephemeral(&ctx.http)
                .await
                .map_err(discord_call_error(site))?,
            Self::Component(component) => component
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Defer(
                        deferred_ephemeral_interaction_response_message(),
                    ),
                )
                .await
                .map_err(discord_call_error(site))?,
        }
        Ok(())
    }

    async fn edit(
        &self,
        ctx: &Context,
        content: impl Into<String>,
        components: Vec<serenity::all::CreateActionRow>,
        site: DiscordCallSite,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = safe_edit_interaction_response()
            .content(content)
            .components(components);
        match self {
            Self::Command(command) => command.edit_response(&ctx.http, response).await,
            Self::Component(component) => component.edit_response(&ctx.http, response).await,
        }
        .map_err(discord_call_error(site))?;
        Ok(InteractionDispatch::Handled)
    }
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
    #[error("expense uncertain-write reply")]
    ExpenseUncertainWriteReply,
    #[error("canonical thread create")]
    CanonicalThreadCreate,
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

struct ExpenseSessionClaim<'a> {
    store: &'a ExpenseSessionStore,
    original: Option<ClaimedExpenseSession>,
}

impl ExpenseSessionClaim<'_> {
    fn session(&self) -> &ExpenseSession {
        self.original
            .as_ref()
            .expect("claimed expense session remains available until terminal transition")
            .session()
    }

    fn replace(&mut self, session: ExpenseSession) {
        if let Some(claimed) = self.original.take() {
            self.store.resolve_claim(claimed.token(), Some(session));
        }
    }

    fn discard(&mut self) {
        if let Some(claimed) = self.original.take() {
            self.store.resolve_claim(claimed.token(), None);
        }
    }
}

impl Drop for ExpenseSessionClaim<'_> {
    fn drop(&mut self) {
        if let Some(claimed) = self.original.take() {
            self.store.restore_claim(claimed);
        }
    }
}

impl LedgerRouter {
    pub fn new(deps: LedgerRouterDependencies) -> Self {
        Self { deps }
    }

    async fn clear_resolved_uncertain_write(
        &self,
        ctx: &Context,
        binding: CanonicalThreadBinding,
    ) -> bool {
        let ledger_id = binding.ledger_id();
        let Some(state) = self.deps.uncertain_writes.current(ledger_id) else {
            return true;
        };
        let UncertainWriteState::Live(retained) = state else {
            return false;
        };
        let now = self.deps.clock.now();
        let retain_expired = retained.requires_full_scan(now);
        let initial_scope = if retain_expired {
            ExactEnvelopeScanScope::FullHistory
        } else {
            ExactEnvelopeScanScope::RecentWindow
        };
        let probes = async {
            if retain_expired {
                self.deps
                    .canonical_store
                    .scan_all_canonical_messages(ctx, binding.canonical_thread_id(), ledger_id)
                    .await
            } else {
                self.deps
                    .canonical_store
                    .scan_recent_canonical_messages(ctx, binding.canonical_thread_id(), ledger_id)
                    .await
            }
            .map_err(LedgerRouteError::from)
        };
        let Ok((load, probes)) = tokio::try_join!(
            self.load_verified_thread(ctx, binding, CanonicalLoadRoute::WritePrelude),
            probes,
        ) else {
            return false;
        };
        let scan = UncertainWriteRegistry::scan_for_exact_envelope(
            &retained,
            &probes,
            initial_scope,
            ScanCompleteness::Complete,
        );
        let observed_head = load.snapshot().current_head_hash().unwrap_or_else(|| {
            walicord_application::ledger::ledger_chain_genesis_sha256_v1(ledger_id)
        });
        let mut resolution = UncertainWriteRegistry::classify_retry(
            &retained,
            scan,
            initial_scope,
            observed_head,
            now,
        );
        if resolution == UncertainWriteResolution::RequiresFullHistoryScan {
            let Ok(probes) = self
                .deps
                .canonical_store
                .scan_all_canonical_messages(ctx, binding.canonical_thread_id(), ledger_id)
                .await
            else {
                return false;
            };
            let scan = UncertainWriteRegistry::scan_for_exact_envelope(
                &retained,
                &probes,
                ExactEnvelopeScanScope::FullHistory,
                ScanCompleteness::Complete,
            );
            resolution = UncertainWriteRegistry::classify_retry(
                &retained,
                scan,
                ExactEnvelopeScanScope::FullHistory,
                observed_head,
                now,
            );
        }
        match resolution {
            UncertainWriteResolution::ClearedByExistingPost { .. }
            | UncertainWriteResolution::ClearedByConclusiveAbsence => {
                self.deps.uncertain_writes.clear(ledger_id);
                true
            }
            UncertainWriteResolution::Abandoned => {
                self.deps
                    .observability
                    .emit(LedgerObservabilityEvent::PersistentUncertainWrite {
                        ledger_id,
                        live_since: retained.live_since(),
                        now: self.deps.clock.now(),
                    });
                self.deps.uncertain_writes.abandon(ledger_id);
                false
            }
            UncertainWriteResolution::RequiresFullHistoryScan
            | UncertainWriteResolution::StillBlocked => false,
        }
    }

    async fn load_verified_thread(
        &self,
        ctx: &Context,
        binding: CanonicalThreadBinding,
        route: CanonicalLoadRoute,
    ) -> Result<VerifiedLedgerThreadLoad, LedgerRouteError> {
        match self
            .deps
            .thread_loader
            .load(ctx, binding.canonical_thread_id(), binding.ledger_id())
            .await
        {
            Ok(load) => Ok(load),
            Err(error) => {
                let failure = CanonicalLoadFailure::from_store_error(route, &error);
                if failure.observability_event().is_some() {
                    self.deps
                        .observability
                        .emit(LedgerObservabilityEvent::UnknownLedgerFormat {
                            ledger_id: binding.ledger_id(),
                            failing_entry_id: failure.failing_entry_id(),
                        });
                }
                if canonical_load_failure_blocks_locator(failure) {
                    let state = self.deps.locator.replace_with_damaged_blocked(
                        binding.tracked_parent(),
                        ledger_refresh_recovery_reference(binding),
                    );
                    self.observe_blocked_locator_state(&state);
                }
                Err(LedgerRouteError::Internal(
                    InternalLedgerRouteError::CanonicalLoad { failure, error },
                ))
            }
        }
    }

    pub(crate) fn clear_tracked_parent_channel(&self, tracked_parent_channel_id: ChannelId) {
        let draft_scope_id = ExpenseDraftScopeId::new(tracked_parent_channel_id.get())
            .expect("serenity channel IDs are always non-zero");
        self.deps.expense_sessions.clear_draft_scope(draft_scope_id);
        self.deps.modal_retries.clear_draft_scope(draft_scope_id);
        self.deps
            .modal_submissions
            .clear_draft_scope(draft_scope_id);
        let bindings = self
            .deps
            .locator
            .clear_tracked_parent_channel(tracked_parent_channel_id);
        for binding in bindings {
            self.deps.void_sessions.clear_ledger(binding.ledger_id());
            self.deps.preview_store.clear_ledger(binding.ledger_id());
            self.deps
                .read_view_sessions
                .clear_ledger(binding.ledger_id());
        }
    }

    #[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
    fn ensure_actor_visibility(
        &self,
        scope: LedgerInteractionScope,
        actor_id: MemberId,
        permissions: Option<Permissions>,
    ) -> Result<(), LedgerRouteError> {
        if actor_can_view_channel(permissions) {
            return Ok(());
        }
        self.clear_actor_state(scope, actor_id);
        Err(LedgerRouteError::Permission(Cow::Borrowed(
            i18n::ledger_permission_failed_message(),
        )))
    }

    fn clear_actor_state(&self, scope: LedgerInteractionScope, actor_id: MemberId) {
        let draft_scope_id = scope.expense_draft_scope_id();
        self.deps
            .expense_sessions
            .clear(ExpenseSessionKey::new(draft_scope_id, actor_id));
        self.deps
            .modal_retries
            .clear_actor_draft_scope(draft_scope_id, actor_id);
        self.deps
            .modal_submissions
            .clear_actor_draft_scope(draft_scope_id, actor_id);
        let Some(binding) = self
            .deps
            .locator
            .cached(scope.tracked_parent())
            .and_then(|state| state.binding())
        else {
            return;
        };
        self.deps
            .void_sessions
            .clear(VoidSessionKey::new(binding.ledger_id(), actor_id));
        self.deps
            .preview_store
            .clear(PreviewStoreKey::new(binding.ledger_id(), actor_id));
        self.deps.read_view_sessions.clear(ReadViewSessionKey {
            ledger_id: binding.ledger_id(),
            actor_id,
        });
    }

    #[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
    fn ensure_runtime_permissions(
        &self,
        ctx: &Context,
        scope: LedgerInteractionScope,
    ) -> Result<(), LedgerRouteError> {
        let Some(guild) = ctx.cache.guild(scope.guild_id()) else {
            return self.runtime_permission_failure(
                scope,
                PermissionAction::ViewChannel,
                Cow::Borrowed("guild permission cache is unavailable"),
            );
        };
        let Some(channel) = guild.channels.get(&scope.channel_id()) else {
            return self.runtime_permission_failure(
                scope,
                PermissionAction::ViewChannel,
                Cow::Borrowed("tracked parent permission cache is unavailable"),
            );
        };
        let Some(member) = guild.members.get(&self.deps.bot_user_id) else {
            return self.runtime_permission_failure(
                scope,
                PermissionAction::ViewChannel,
                Cow::Borrowed("bot member permission cache is unavailable"),
            );
        };
        let current = guild.user_permissions_in(channel, member);
        let missing =
            missing_runtime_permissions_for(RuntimePermissionScope::CanonicalSurface, current);
        if missing.is_empty() {
            return Ok(());
        }
        self.runtime_permission_failure(
            scope,
            first_missing_permission_action(current),
            Cow::Owned(format!("不足している権限: {}", missing.join(", "))),
        )
    }

    #[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
    fn runtime_permission_failure(
        &self,
        scope: LedgerInteractionScope,
        action: PermissionAction,
        detail: Cow<'static, str>,
    ) -> Result<(), LedgerRouteError> {
        self.deps
            .observability
            .emit_discord(DiscordLedgerObservabilityEvent::PermissionFailure {
                ledger_id: None,
                guild_id: Some(scope.guild_id()),
                channel_id: scope.channel_id(),
                action,
            });
        Err(LedgerRouteError::Permission(Cow::Owned(format!(
            "{} {detail}",
            i18n::ledger_permission_failed_message(),
        ))))
    }

    fn observe_blocked_locator_state(&self, state: &CanonicalThreadLocatorState) {
        match state {
            CanonicalThreadLocatorState::DuplicateBlocked {
                tracked_parent,
                recovery_references,
                ..
            } => self.deps.observability.emit_discord(
                DiscordLedgerObservabilityEvent::DuplicateThreadBlocked {
                    guild_id: tracked_parent.guild_id(),
                    tracked_parent_channel_id: tracked_parent.tracked_parent_channel_id(),
                    candidate_thread_ids: recovery_references
                        .iter()
                        .filter_map(recovery_reference_channel_id)
                        .collect(),
                },
            ),
            CanonicalThreadLocatorState::DamagedBlocked {
                tracked_parent,
                recovery_reference,
            } => {
                if let Some(candidate_thread_id) = recovery_reference_channel_id(recovery_reference)
                {
                    self.deps.observability.emit_discord(
                        DiscordLedgerObservabilityEvent::DamagedThreadBlocked {
                            guild_id: tracked_parent.guild_id(),
                            tracked_parent_channel_id: tracked_parent.tracked_parent_channel_id(),
                            candidate_thread_id,
                        },
                    );
                }
            }
            _ => {}
        }
    }

    async fn guard_scope(
        &self,
        ctx: &Context,
        guild_id: Option<GuildId>,
        interaction_channel_id: ChannelId,
        interaction: &impl LedgerInteractionActor,
    ) -> Result<LedgerInteractionScope, LedgerRouteError> {
        let scope = self
            .guard_scope_without_runtime_permissions(ctx, guild_id, interaction_channel_id)
            .await?;
        self.ensure_runtime_permissions(ctx, scope)?;
        self.ensure_actor_visibility(
            scope,
            interaction.ledger_actor_id(),
            interaction.ledger_actor_permissions(),
        )?;
        Ok(scope)
    }

    async fn guard_scope_without_runtime_permissions(
        &self,
        ctx: &Context,
        guild_id: Option<GuildId>,
        interaction_channel_id: ChannelId,
    ) -> Result<LedgerInteractionScope, LedgerRouteError> {
        let scope = guard_ledger_interaction_resolving_parent(
            ctx,
            guild_id,
            interaction_channel_id,
            self.deps.channels.as_ref(),
        )
        .await?;
        Ok(scope)
    }

    async fn resolve_existing_ledger(
        &self,
        ctx: &Context,
        scope: super::route_guard::LedgerInteractionScope,
    ) -> Result<CanonicalThreadBinding, LedgerRouteError> {
        let state = self
            .deps
            .locator
            .resolve(ctx, scope.tracked_parent())
            .await?;
        self.observe_blocked_locator_state(&state);
        match state {
            CanonicalThreadLocatorState::Provisioned(binding)
            | CanonicalThreadLocatorState::ReadyBound(binding) => Ok(binding),
            CanonicalThreadLocatorState::ReadyNoThread { .. }
            | CanonicalThreadLocatorState::ReadyEmptyThread { .. }
            | CanonicalThreadLocatorState::DuplicateBlocked { .. }
            | CanonicalThreadLocatorState::DamagedBlocked { .. } => Err(
                LedgerRouteError::Internal(InternalLedgerRouteError::ExistingLedgerNotReady),
            ),
        }
    }

    async fn resolve_readable_ledger(
        &self,
        ctx: &Context,
        scope: LedgerInteractionScope,
    ) -> Result<Option<CanonicalThreadBinding>, LedgerRouteError> {
        let state = self
            .deps
            .locator
            .resolve(ctx, scope.tracked_parent())
            .await?;
        self.observe_blocked_locator_state(&state);
        match state {
            CanonicalThreadLocatorState::Provisioned(binding)
            | CanonicalThreadLocatorState::ReadyBound(binding) => Ok(Some(binding)),
            CanonicalThreadLocatorState::ReadyNoThread { .. }
            | CanonicalThreadLocatorState::ReadyEmptyThread { .. } => Ok(None),
            CanonicalThreadLocatorState::DuplicateBlocked { .. }
            | CanonicalThreadLocatorState::DamagedBlocked { .. } => Err(
                LedgerRouteError::Internal(InternalLedgerRouteError::ExistingLedgerNotReady),
            ),
        }
    }

    async fn resolve_or_bootstrap_expense_ledger(
        &self,
        ctx: &Context,
        scope: super::route_guard::LedgerInteractionScope,
    ) -> Result<CanonicalThreadBinding, LedgerRouteError> {
        let tracked_parent = scope.tracked_parent();
        if let Some(binding) = self
            .deps
            .locator
            .cached(tracked_parent)
            .and_then(|state| state.binding())
        {
            return Ok(binding);
        }
        let lock = self
            .deps
            .write_coordinator
            .lock_for(WriteTargetKey::Bootstrap(BootstrapWriteTarget::new(
                scope.guild_id().get(),
                tracked_parent.tracked_parent_channel_id().get(),
            )));
        let _guard = tokio::time::timeout(Duration::from_secs(10), lock.lock())
            .await
            .map_err(|_| LedgerRouteError::Internal(InternalLedgerRouteError::BootstrapTimeout))?;
        if let Some(binding) = self
            .deps
            .locator
            .cached(tracked_parent)
            .and_then(|state| state.binding())
        {
            return Ok(binding);
        }
        let state = self.deps.locator.refresh(ctx, tracked_parent).await?;
        self.observe_blocked_locator_state(&state);
        match state {
            CanonicalThreadLocatorState::ReadyBound(binding) => Ok(binding),
            CanonicalThreadLocatorState::Provisioned(binding) => Ok(binding),
            CanonicalThreadLocatorState::ReadyEmptyThread {
                canonical_thread_id,
                ..
            } => {
                let binding = CanonicalThreadBinding::new(tracked_parent, canonical_thread_id);
                self.deps.locator.replace_with_provisioned_binding(binding);
                Ok(binding)
            }
            CanonicalThreadLocatorState::ReadyNoThread { .. } => {
                let canonical_thread_id = self
                    .deps
                    .thread_creator
                    .create(ctx, scope.channel_id())
                    .await
                    .map_err(|error| {
                        if serenity_error_is_read_denied(&error) {
                            self.deps.observability.emit_discord(
                                DiscordLedgerObservabilityEvent::PermissionFailure {
                                    ledger_id: None,
                                    guild_id: Some(scope.guild_id()),
                                    channel_id: scope.channel_id(),
                                    action: PermissionAction::CreatePublicThread,
                                },
                            );
                        }
                        discord_call_error(DiscordCallSite::CanonicalThreadCreate)(error)
                    })?;
                let binding = CanonicalThreadBinding::new(tracked_parent, canonical_thread_id);
                self.deps.locator.replace_with_provisioned_binding(binding);
                Ok(binding)
            }
            CanonicalThreadLocatorState::DuplicateBlocked { .. }
            | CanonicalThreadLocatorState::DamagedBlocked { .. } => Err(
                LedgerRouteError::Internal(InternalLedgerRouteError::ExistingLedgerNotReady),
            ),
        }
    }

    async fn blocked_expense_launcher_ledger(
        &self,
        ctx: &Context,
        scope: LedgerInteractionScope,
    ) -> Result<Option<LedgerId>, LedgerRouteError> {
        let state = self
            .deps
            .locator
            .resolve(ctx, scope.tracked_parent())
            .await?;
        self.observe_blocked_locator_state(&state);
        match state {
            CanonicalThreadLocatorState::Provisioned(binding)
            | CanonicalThreadLocatorState::ReadyBound(binding) => Ok(self
                .deps
                .uncertain_writes
                .current(binding.ledger_id())
                .is_some()
                .then_some(binding.ledger_id())),
            CanonicalThreadLocatorState::ReadyNoThread { .. }
            | CanonicalThreadLocatorState::ReadyEmptyThread { .. } => Ok(None),
            CanonicalThreadLocatorState::DuplicateBlocked { .. }
            | CanonicalThreadLocatorState::DamagedBlocked { .. } => Err(
                LedgerRouteError::Internal(InternalLedgerRouteError::ExistingLedgerNotReady),
            ),
        }
    }

    /// Slash-command dispatch. Returns `Ignored` for commands the router does not own.
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
            "ledger-refresh" => self.dispatch_ledger_refresh_command(ctx, command).await,
            _ => Ok(InteractionDispatch::Ignored),
        }
    }

    async fn dispatch_ledger_refresh_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope_without_runtime_permissions(ctx, command.guild_id, command.channel_id)
            .await?;
        let authorized = command.member.as_ref().is_some_and(|member| {
            ctx.cache.guild(scope.guild_id()).is_some_and(|guild| {
                guild
                    .channels
                    .get(&scope.tracked_parent().tracked_parent_channel_id())
                    .is_some_and(|channel| {
                        let permissions = guild.user_permissions_in(channel, member);
                        permissions.administrator() || permissions.manage_threads()
                    })
            })
        });
        let (content, components) = if !authorized {
            (
                render_ledger_refresh_acknowledgement(LedgerRefreshAcknowledgement::Unauthorized),
                Vec::new(),
            )
        } else {
            let state = self
                .deps
                .locator
                .stale_lookup_failed(scope.tracked_parent())
                .await?;
            self.observe_blocked_locator_state(&state);
            match state {
                CanonicalThreadLocatorState::ReadyNoThread { .. } => (
                    render_ledger_refresh_acknowledgement(
                        LedgerRefreshAcknowledgement::ReadyNoThread,
                    ),
                    Vec::new(),
                ),
                CanonicalThreadLocatorState::ReadyEmptyThread { .. } => (
                    render_ledger_refresh_acknowledgement(LedgerRefreshAcknowledgement::Ready),
                    Vec::new(),
                ),
                state @ (CanonicalThreadLocatorState::DuplicateBlocked { .. }
                | CanonicalThreadLocatorState::DamagedBlocked { .. }) => {
                    render_panel_post_message_for_locator_state(Some(&state), false)
                        .expect_err("blocked locator state must render a recovery response")
                        .into_parts()
                }
                CanonicalThreadLocatorState::Provisioned(binding)
                | CanonicalThreadLocatorState::ReadyBound(binding) => {
                    if self
                        .deps
                        .uncertain_writes
                        .current(binding.ledger_id())
                        .is_none()
                    {
                        (
                            render_ledger_refresh_acknowledgement(
                                LedgerRefreshAcknowledgement::Ready,
                            ),
                            Vec::new(),
                        )
                    } else {
                        render_ledger_refresh_uncertain_write_message(
                            &ledger_refresh_recovery_reference(binding),
                        )
                        .into_parts()
                    }
                }
            }
        };
        command
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    safe_ephemeral_interaction_response_message()
                        .content(content)
                        .components(components),
                ),
            )
            .await
            .map_err(discord_call_error(DiscordCallSite::LedgerEditResponse))?;
        Ok(InteractionDispatch::Handled)
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

    async fn dispatch_review_command(
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

    async fn dispatch_review(
        &self,
        ctx: &Context,
        scope: super::route_guard::LedgerInteractionScope,
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

    async fn dispatch_settle_command(
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
            self.deps.clock.now(),
        );
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
        if self.deps.uncertain_writes.set_live(retained).is_err() {
            return Err(LedgerRouteError::Internal(
                InternalLedgerRouteError::UncertainWriteAlreadyLive { ledger_id },
            ));
        }

        let append_result = self
            .deps
            .canonical_store
            .append_authoritative(
                ctx,
                binding.canonical_thread_id(),
                &envelope,
                prepared_body.as_str(),
            )
            .await;
        match append_result {
            Ok(_verified) => {
                self.deps.locator.replace_with_ready_binding(binding);
                self.deps.uncertain_writes.clear(ledger_id);
                preview_commit.finish()?;
                self.edit_command_response(
                    ctx,
                    command,
                    i18n::settlement_recorded_message(),
                    DiscordCallSite::SettleEditResponse,
                )
                .await
            }
            Err(_error) => {
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

    async fn dispatch_void_command(
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
        self.dispatch_void(ctx, scope, DeferredEphemeralInteraction::Command(command))
            .await
    }

    async fn dispatch_void(
        &self,
        ctx: &Context,
        scope: super::route_guard::LedgerInteractionScope,
        interaction: DeferredEphemeralInteraction<'_>,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        interaction
            .defer(ctx, DiscordCallSite::VoidDeferEphemeral)
            .await?;
        let Some(binding) = self.resolve_readable_ledger(ctx, scope).await? else {
            return self
                .edit_initial_void_model(
                    ctx,
                    interaction,
                    VoidSurfaceModel::empty(i18n::panel_void_button_label(), Vec::new(), true),
                )
                .await;
        };
        let ledger_id = binding.ledger_id();
        if !self.clear_resolved_uncertain_write(ctx, binding).await {
            let (message, components) =
                self.uncertain_write_block_response(ledger_id, false, false);
            return interaction
                .edit(ctx, message, components, DiscordCallSite::VoidEditResponse)
                .await;
        }

        let load = self
            .load_verified_thread(ctx, binding, CanonicalLoadRoute::Read)
            .await?;
        if load.snapshot().canonical_entry_count() == 0 {
            return self
                .edit_initial_void_model(
                    ctx,
                    interaction,
                    VoidSurfaceModel::empty(i18n::panel_void_button_label(), Vec::new(), true),
                )
                .await;
        }

        let actor_id = MemberId(interaction.user_id().get());
        let key = VoidSessionKey::new(ledger_id, actor_id);
        let (session, nonce, candidates) = match bootstrap_void_session(
            key,
            &load,
            self.deps.clock.as_ref(),
            self.deps.nonce_provider.as_ref(),
        ) {
            Ok(outcome) => outcome,
            Err(VoidSessionBootstrapError::NoVoidableCandidates) => {
                return self
                    .edit_initial_void_model(
                        ctx,
                        interaction,
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
        let rows = void_candidate_rows(&candidates, &labels, ledger_id)?;
        let action_rows = void_selection_action_rows(nonce, &candidates, &labels);
        let replaced = self
            .deps
            .void_sessions
            .has_active_session(key, self.deps.clock.now());
        self.deps.void_sessions.replace(session);
        let mut model =
            VoidSurfaceModel::selection(i18n::panel_void_button_label(), rows, action_rows, true);
        if replaced {
            model
                .phase_copy
                .insert(0, i18n::void_session_replaced_message().to_owned());
        }
        self.edit_initial_void_model(ctx, interaction, model).await
    }

    async fn dispatch_expense_command(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, command.guild_id, command.channel_id, command)
            .await?;
        if let Some(ledger_id) = self.blocked_expense_launcher_ledger(ctx, scope).await? {
            let (message, components) =
                self.uncertain_write_block_response(ledger_id, false, false);
            command
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_ephemeral_interaction_response_message()
                            .content(message)
                            .components(components),
                    ),
                )
                .await
                .map_err(discord_call_error(
                    DiscordCallSite::ExpenseModalCreateResponse,
                ))?;
            return Ok(InteractionDispatch::Handled);
        }

        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        self.store_expense_modal_submission(
            nonce,
            MemberId(command.user.id.get()),
            scope.expense_draft_scope_id(),
            ExpenseModalIntent::Create {
                origin: ExpenseLaunchOrigin::SlashCommand,
            },
        );
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

    async fn dispatch_ledger(
        &self,
        ctx: &Context,
        scope: super::route_guard::LedgerInteractionScope,
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

    /// Component (button / select-menu) dispatch for every ledger-owned custom id.
    pub async fn handle_component(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        if component.data.custom_id == UNCERTAIN_WRITE_ACKNOWLEDGE_CUSTOM_ID {
            return self
                .dispatch_abandoned_uncertain_write_acknowledge(ctx, component)
                .await;
        }
        match panel_launcher(&component.data.custom_id) {
            Some(PanelLauncher::Expense) => {
                return self.dispatch_panel_expense_launcher(ctx, component).await;
            }
            Some(PanelLauncher::Review) => {
                return self.dispatch_panel_review_launcher(ctx, component).await;
            }
            Some(PanelLauncher::Ledger) => {
                return self.dispatch_panel_ledger_launcher(ctx, component).await;
            }
            Some(PanelLauncher::Void) => {
                return self.dispatch_panel_void_launcher(ctx, component).await;
            }
            None => {}
        }
        let custom_id = component.data.custom_id.as_str();
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_MODAL_RETRY_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_modal_retry(ctx, component, nonce)
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_CANCEL_CUSTOM_ID_PREFIX)
        {
            return self.dispatch_expense_cancel(ctx, component, nonce).await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_BACK_CUSTOM_ID_PREFIX)
        {
            return self.dispatch_expense_back(ctx, component, nonce).await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_BASIC_EDIT_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_basic_edit(ctx, component, nonce)
                .await;
        }
        if let Some((nonce, snapshot_id)) =
            parse_expense_picker_selection_custom_id(custom_id, EXPENSE_PAYER_PICK_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_payer_pick(ctx, component, nonce, snapshot_id)
                .await;
        }
        if let Some((kind, nonce, snapshot_id)) =
            parse_expense_picker_custom_id(custom_id, EXPENSE_PICKER_PREV_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_picker_page(
                    ctx,
                    component,
                    kind,
                    PickerPageDirection::Previous,
                    nonce,
                    snapshot_id,
                )
                .await;
        }
        if let Some((kind, nonce, snapshot_id)) =
            parse_expense_picker_custom_id(custom_id, EXPENSE_PICKER_NEXT_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_picker_page(
                    ctx,
                    component,
                    kind,
                    PickerPageDirection::Next,
                    nonce,
                    snapshot_id,
                )
                .await;
        }
        if let Some((kind, nonce, snapshot_id)) =
            parse_expense_picker_custom_id(custom_id, EXPENSE_PICKER_SEARCH_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_picker_search_open(ctx, component, kind, nonce, snapshot_id)
                .await;
        }
        if let Some((kind, nonce, snapshot_id)) =
            parse_expense_picker_custom_id(custom_id, EXPENSE_PICKER_CLEAR_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_picker_clear(ctx, component, kind, nonce, snapshot_id)
                .await;
        }
        if let Some((nonce, snapshot_id)) = parse_expense_picker_selection_custom_id(
            custom_id,
            EXPENSE_INDIVIDUAL_PICK_CUSTOM_ID_PREFIX,
        ) {
            return self
                .dispatch_expense_individual_pick(ctx, component, nonce, snapshot_id)
                .await;
        }
        if let Some((nonce, snapshot_id)) =
            parse_expense_picker_selection_custom_id(custom_id, EXPENSE_ROLE_PICK_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_role_pick(ctx, component, nonce, snapshot_id)
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_TO_PARTICIPANTS_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_forward(
                    ctx,
                    component,
                    nonce,
                    ExpenseSelectionPhase::ParticipantSource,
                )
                .await;
        }
        if let Some(nonce) = parse_expense_session_button_nonce(
            custom_id,
            EXPENSE_SOURCE_INDIVIDUAL_CUSTOM_ID_PREFIX,
        ) {
            return self
                .dispatch_expense_forward(
                    ctx,
                    component,
                    nonce,
                    ExpenseSelectionPhase::IndividualSelection,
                )
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_SOURCE_ROLES_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_forward(ctx, component, nonce, ExpenseSelectionPhase::Roles)
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_SOURCE_MEMBERS_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_members_toggle(ctx, component, nonce)
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_TO_WEIGHTS_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_forward(
                    ctx,
                    component,
                    nonce,
                    ExpenseSelectionPhase::WeightEditor,
                )
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_WEIGHT_EDIT_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_weight_edit(ctx, component, nonce)
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_TO_CONFIRM_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_to_confirm(ctx, component, nonce)
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_MODIFY_SELECTION_CUSTOM_ID_PREFIX)
        {
            return self
                .dispatch_expense_modify_selection(ctx, component, nonce)
                .await;
        }
        if let Some(nonce) =
            parse_expense_session_button_nonce(custom_id, EXPENSE_RECORD_CUSTOM_ID_PREFIX)
        {
            return self.dispatch_expense_record(ctx, component, nonce).await;
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

    async fn dispatch_abandoned_uncertain_write_acknowledge(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let binding = self.resolve_existing_ledger(ctx, scope).await?;
        let ledger_id = binding.ledger_id();
        let lock = self.deps.write_coordinator.lock_for(ledger_id);
        let _guard = lock.lock().await;
        let acknowledged = matches!(
            self.deps.uncertain_writes.current(ledger_id),
            Some(UncertainWriteState::Abandoned(_))
        );
        if acknowledged {
            self.deps.uncertain_writes.clear(ledger_id);
        }
        self.reply_component_ephemeral(
            ctx,
            component,
            if acknowledged {
                i18n::abandoned_uncertain_write_acknowledged_message()
            } else {
                i18n::uncertain_write_block_message()
            },
            DiscordCallSite::ExpenseUncertainWriteReply,
        )
        .await
    }

    async fn dispatch_void_pick(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let binding = self.resolve_existing_ledger(ctx, scope).await?;
        let ledger_id = binding.ledger_id();
        let key = VoidSessionKey::new(ledger_id, MemberId(component.user.id.get()));
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
                    binding.canonical_thread_id(),
                    ledger_id,
                    session,
                    VoidSelectionRenderKind::MissingSelection,
                )
                .await;
        };

        let load = self
            .load_verified_thread(ctx, binding, CanonicalLoadRoute::Read)
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
                let model =
                    void_confirmation_model(target, &labels, ledger_id, next_session.nonce())?;
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
                    binding.canonical_thread_id(),
                    ledger_id,
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
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let binding = self.resolve_existing_ledger(ctx, scope).await?;
        let key = VoidSessionKey::new(binding.ledger_id(), MemberId(component.user.id.get()));
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
            binding.canonical_thread_id(),
            binding.ledger_id(),
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
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let binding = self.resolve_existing_ledger(ctx, scope).await?;
        let key = VoidSessionKey::new(binding.ledger_id(), MemberId(component.user.id.get()));
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
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let binding = self.resolve_existing_ledger(ctx, scope).await?;
        let ledger_id = binding.ledger_id();
        let actor_id = MemberId(component.user.id.get());
        let key = VoidSessionKey::new(ledger_id, actor_id);
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

        // Pre-lock fast bail: avoid taking the per-ledger lock when uncertain_write
        // is already unresolvable. Cheap when no retain exists (DashMap read only).
        if !self.clear_resolved_uncertain_write(ctx, binding).await {
            let (message, components) = self.uncertain_write_block_response(ledger_id, true, false);
            return self
                .edit_component_response_with_components(
                    ctx,
                    component,
                    message,
                    components,
                    DiscordCallSite::VoidEditResponse,
                )
                .await;
        }

        let lock = self.deps.write_coordinator.lock_for(ledger_id);
        let _guard = lock.lock().await;
        // Post-lock recheck: another writer may have set a fresh uncertain_write
        // between the pre-lock check and acquiring the lock; revalidate inside the
        // critical section before committing to set_live / append.
        if !self.clear_resolved_uncertain_write(ctx, binding).await {
            let (message, components) = self.uncertain_write_block_response(ledger_id, true, false);
            return self
                .edit_component_response_with_components(
                    ctx,
                    component,
                    message,
                    components,
                    DiscordCallSite::VoidEditResponse,
                )
                .await;
        }
        let load = self
            .load_verified_thread(ctx, binding, CanonicalLoadRoute::WritePrelude)
            .await?;
        let next_entry_id = load.next_entry_id()?;
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
            ledger_id,
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
        let prepared_body = render_public_void_body(&entry, &target_view, ledger_id, &labels)?;
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
            self.deps.clock.now(),
        );
        self.deps.uncertain_writes.set_live(retained).map_err(|_| {
            LedgerRouteError::Internal(InternalLedgerRouteError::UncertainWriteAlreadyLive {
                ledger_id,
            })
        })?;

        match self
            .deps
            .canonical_store
            .append_authoritative(
                ctx,
                binding.canonical_thread_id(),
                &envelope,
                prepared_body.as_str(),
            )
            .await
        {
            Ok(_verified) => {
                self.deps.locator.replace_with_ready_binding(binding);
                self.deps.uncertain_writes.clear(ledger_id);
                self.deps.void_sessions.clear(key);
                self.edit_component_with_void_model(
                    ctx,
                    component,
                    VoidSurfaceModel::success(
                        i18n::void_success_title(),
                        format!("<#{}>", binding.canonical_thread_id().get()),
                        Vec::new(),
                        true,
                    ),
                )
                .await
            }
            Err(_error) => {
                let (message, components) =
                    self.uncertain_write_block_response(ledger_id, true, false);
                self.edit_component_response_with_components(
                    ctx,
                    component,
                    message,
                    components,
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
        observed_nonce: walicord_application::InteractionNonce,
        target: ExpenseSelectionPhase,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        match navigate_to_phase(
            claim.session().clone(),
            target.clone(),
            self.deps.clock.as_ref(),
        ) {
            Ok(updated) => {
                let dispatch = self
                    .respond_with_step_body(ctx, component, scope, &updated)
                    .await?;
                claim.replace(updated);
                Ok(dispatch)
            }
            Err(NavigationError::IllegalForwardTransition { from, .. }) => {
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
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };

        let roster_snapshot = self
            .deps
            .roster_fetcher
            .fetch(ctx, scope.guild_id(), scope.channel_id())
            .await
            .map_err(LedgerRouteError::from)?;

        let outcome = build_confirmation_for_session(
            claim.session().clone(),
            &roster_snapshot.roster,
            self.deps.clock.as_ref(),
        )?;

        let nonce = outcome.session.nonce();
        let basic_info = outcome.session.draft().basic_info().cloned().ok_or(
            InternalLedgerRouteError::ConfirmationBuild(ConfirmationBuildError::BasicInfoMissing),
        )?;
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
            safe_ephemeral_interaction_response_message()
                .content(body)
                .components(components),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseConfirmationCreateResponse,
            ))?;
        claim.replace(outcome.session);
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
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let guild_id = scope.guild_id();
        let channel_id = scope.channel_id();
        let actor_id = MemberId(component.user.id.get());
        let key = ExpenseSessionKey::new(scope.expense_draft_scope_id(), actor_id);
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        if !matches!(claim.session().stage(), ExpenseSessionStage::InConfirmation) {
            // Defensive: the record button is only rendered on the confirmation page;
            // a non-confirmation stage here means a stale cached interaction. Restart
            // the actor cleanly.
            return self.respond_expense_session_missing(ctx, component).await;
        }

        let binding = self.resolve_or_bootstrap_expense_ledger(ctx, scope).await?;
        let ledger_id = binding.ledger_id();
        let write_target = WriteTargetKey::Published(ledger_id);

        // Pre-lock fast bail: avoid taking the per-ledger lock when uncertain_write
        // is already unresolvable. Cheap when no retain exists (DashMap read only).
        if !self.clear_resolved_uncertain_write(ctx, binding).await {
            let (message, components) = self.uncertain_write_block_response(ledger_id, true, false);
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

        // Per-ledger serialization (criterion 53 / 115 / 155 / 182): every canonical
        // append for this ledger holds the same async mutex for its whole lifecycle.
        let lock = self.deps.write_coordinator.lock_for(write_target);
        let _guard = lock.lock().await;
        // Post-lock recheck: another writer may have set a fresh uncertain_write
        // between the pre-lock check and acquiring the lock; revalidate inside the
        // critical section before committing to set_live / append.
        if !self.clear_resolved_uncertain_write(ctx, binding).await {
            let (message, components) = self.uncertain_write_block_response(ledger_id, true, false);
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

        let load_future = self.load_verified_thread(ctx, binding, CanonicalLoadRoute::WritePrelude);
        let roster_future = self.deps.roster_fetcher.fetch(ctx, guild_id, channel_id);
        let (load_result, roster_result) = tokio::join!(load_future, roster_future);
        let snapshot_load = load_result?;
        let roster_snapshot = roster_result?;

        let next_entry_id = snapshot_load.next_entry_id()?;
        let previous_hash = snapshot_load
            .snapshot()
            .current_head_hash()
            .unwrap_or_else(|| {
                walicord_application::ledger::ledger_chain_genesis_sha256_v1(ledger_id)
            });
        let outcome = compose_expense_entry(
            claim.session(),
            &roster_snapshot.roster,
            next_entry_id,
            expense_source_descriptor(claim.session().origin()),
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
                let session = claim.session().clone();
                self.refresh_confirmation_for_drift(
                    ctx,
                    component,
                    &mut claim,
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
                    &mut claim,
                    write_target,
                    binding,
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
        claim: &mut ExpenseSessionClaim<'_>,
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
            session.origin(),
            ExpenseSessionStage::InConfirmation,
            next_draft,
            session.nonce(),
            self.deps.clock.now(),
        )?;
        let nonce = refreshed_session.nonce();
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
            safe_ephemeral_interaction_response_message()
                .content(body)
                .components(components),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseConfirmationCreateResponse,
            ))?;
        claim.replace(refreshed_session);
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
        claim: &mut ExpenseSessionClaim<'_>,
        write_target: WriteTargetKey,
        binding: CanonicalThreadBinding,
        previous_hash: walicord_application::ledger::EntryHash,
        entry: LedgerEntry,
        display_names: &HashMap<MemberId, smol_str::SmolStr>,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let ledger_id = binding.ledger_id();
        let canonical_thread_id = binding.canonical_thread_id();
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
            self.deps.clock.now(),
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
                claim.discard();
                self.deps.locator.replace_with_ready_binding(binding);
                self.deps.uncertain_writes.clear(write_target);
                self.respond_record_success(ctx, component).await
            }
            Err(_error) => {
                // Retain stays Live: a transport error here is exactly the
                // criterion-217 / 279 case where lazy retry must decide whether the
                // canonical message actually posted.
                let (message, components) =
                    self.uncertain_write_block_response(ledger_id, true, false);
                self.reply_component_ephemeral_with_components(
                    ctx,
                    component,
                    message,
                    components,
                    DiscordCallSite::ExpenseUncertainWriteReply,
                )
                .await
            }
        }
    }

    async fn respond_record_success(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = CreateInteractionResponse::UpdateMessage(
            safe_ephemeral_interaction_response_message()
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
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let updated = navigate_modify_selection(claim.session().clone(), self.deps.clock.as_ref())?;
        let dispatch = self
            .respond_with_step_body(ctx, component, scope, &updated)
            .await?;
        claim.replace(updated);
        Ok(dispatch)
    }

    /// Toggle the `MEMBERS` virtual group on the active session and refresh the
    /// participant-source step chrome.
    async fn dispatch_expense_payer_pick(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        observed_nonce: walicord_application::InteractionNonce,
        expected_snapshot: PickerSnapshotId,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let page = self
            .expense_picker_page_for_session(ctx, scope, claim.session(), ExpensePickerKind::Payer)
            .await?;
        if page.snapshot_id != expected_snapshot {
            return self.reject_stale_picker_component(ctx, component).await;
        }
        let payer = selected_component_values(component)?
            .first()
            .copied()
            .map(MemberId);
        let updated = replace_payer(claim.session().clone(), payer, self.deps.clock.as_ref())?;
        let dispatch = self
            .respond_with_step_body(ctx, component, scope, &updated)
            .await?;
        claim.replace(updated);
        Ok(dispatch)
    }

    async fn dispatch_expense_individual_pick(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        observed_nonce: walicord_application::InteractionNonce,
        expected_snapshot: PickerSnapshotId,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let selected = selected_component_values(component)?;
        let page = self
            .expense_picker_page_for_session(
                ctx,
                scope,
                claim.session(),
                ExpensePickerKind::Individuals,
            )
            .await?;
        if page.snapshot_id != expected_snapshot {
            return self.reject_stale_picker_component(ctx, component).await;
        }
        let members = merge_paged_selection(
            &claim
                .session()
                .draft()
                .selection_state()
                .individual_members
                .iter()
                .map(|member_id| member_id.0)
                .collect::<Vec<_>>(),
            &page.visible_values,
            &selected,
        )
        .into_iter()
        .map(MemberId)
        .collect();
        let updated =
            replace_individual_members(claim.session().clone(), members, self.deps.clock.as_ref())?;
        let dispatch = self
            .respond_with_step_body(ctx, component, scope, &updated)
            .await?;
        claim.replace(updated);
        Ok(dispatch)
    }

    async fn dispatch_expense_role_pick(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        observed_nonce: walicord_application::InteractionNonce,
        expected_snapshot: PickerSnapshotId,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let selected = selected_component_values(component)?;
        let page = self
            .expense_picker_page_for_session(ctx, scope, claim.session(), ExpensePickerKind::Roles)
            .await?;
        if page.snapshot_id != expected_snapshot {
            return self.reject_stale_picker_component(ctx, component).await;
        }
        let roles = merge_paged_selection(
            &claim
                .session()
                .draft()
                .selection_state()
                .selected_roles
                .iter()
                .map(|role_id| role_id.0)
                .collect::<Vec<_>>(),
            &page.visible_values,
            &selected,
        )
        .into_iter()
        .map(RoleId)
        .collect();
        let updated =
            replace_selected_roles(claim.session().clone(), roles, self.deps.clock.as_ref())?;
        let dispatch = self
            .respond_with_step_body(ctx, component, scope, &updated)
            .await?;
        claim.replace(updated);
        Ok(dispatch)
    }

    async fn dispatch_expense_members_toggle(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let updated = toggle_members_group(claim.session().clone(), self.deps.clock.as_ref())
            .map_err(LedgerRouteError::from)?;
        let dispatch = self
            .respond_with_step_body(ctx, component, scope, &updated)
            .await?;
        claim.replace(updated);
        Ok(dispatch)
    }

    async fn respond_with_step_body(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        scope: LedgerInteractionScope,
        session: &ExpenseSession,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let (body, components) = self.build_expense_step_message(ctx, scope, session).await?;
        let response = CreateInteractionResponse::UpdateMessage(
            safe_ephemeral_interaction_response_message()
                .content(body)
                .components(components),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn build_expense_step_message(
        &self,
        ctx: &Context,
        scope: LedgerInteractionScope,
        session: &ExpenseSession,
    ) -> Result<(String, Vec<CreateActionRow>), LedgerRouteError> {
        let ExpenseSessionStage::InSelection { phase } = session.stage() else {
            return Err(InternalLedgerRouteError::PostNavigationStageInvariant {
                observed_stage: session.stage().clone(),
            }
            .into());
        };
        let nonce = session.nonce();
        let button_ids = build_selection_step_button_ids(nonce);
        let mut model = build_expense_selection_step_surface(phase, &button_ids);
        let picker = self
            .expense_picker_render_parts(ctx, scope, session, phase, nonce)
            .await?;
        model.detail_lines.extend(picker.detail_lines);
        if let Some(row) = picker.utility_row {
            model.action_rows.insert(0, row);
        }
        if let Some(menu) = picker.select_menu {
            model.action_rows.insert(0, SurfaceActionRow::Select(menu));
        }
        let rendered = DiscordLedgerPresenter::render_expense_step(&model)?;
        let (body, components) = rendered_surface_to_message(rendered);
        Ok((body, components))
    }

    async fn expense_picker_render_parts(
        &self,
        ctx: &Context,
        scope: LedgerInteractionScope,
        session: &ExpenseSession,
        phase: &ExpenseSelectionPhase,
        nonce: walicord_application::InteractionNonce,
    ) -> Result<ExpensePickerRenderParts, LedgerRouteError> {
        let Some(kind) = picker_kind_for_phase(phase) else {
            return Ok(ExpensePickerRenderParts::default());
        };
        let page = self
            .expense_picker_page_for_session(ctx, scope, session, kind)
            .await?;
        let detail_lines = page.detail_lines;
        let (custom_id, placeholder, min_values, max_values) = match phase {
            ExpenseSelectionPhase::Payer => (
                expense_picker_selection_custom_id(
                    EXPENSE_PAYER_PICK_CUSTOM_ID_PREFIX,
                    nonce,
                    page.snapshot_id,
                ),
                i18n::expense_payer_placeholder(),
                1,
                1,
            ),
            ExpenseSelectionPhase::IndividualSelection => (
                expense_picker_selection_custom_id(
                    EXPENSE_INDIVIDUAL_PICK_CUSTOM_ID_PREFIX,
                    nonce,
                    page.snapshot_id,
                ),
                i18n::participant_source_individual_placeholder(),
                0,
                page.page_item_count as u8,
            ),
            ExpenseSelectionPhase::Roles => (
                expense_picker_selection_custom_id(
                    EXPENSE_ROLE_PICK_CUSTOM_ID_PREFIX,
                    nonce,
                    page.snapshot_id,
                ),
                i18n::participant_source_role_placeholder(),
                0,
                page.page_item_count as u8,
            ),
            ExpenseSelectionPhase::ParticipantSource | ExpenseSelectionPhase::WeightEditor => {
                unreachable!("phases without menus return early")
            }
        };
        if page.total_items == 0 {
            return Ok(ExpensePickerRenderParts {
                select_menu: None,
                utility_row: Some(expense_picker_utility_row(
                    kind,
                    nonce,
                    page.snapshot_id,
                    page.current_page,
                    page.total_pages,
                )),
                detail_lines,
            });
        }
        Ok(ExpensePickerRenderParts {
            select_menu: Some(SurfaceSelectMenu {
                custom_id,
                placeholder: Some(placeholder.to_owned()),
                options: page.options,
                min_values,
                max_values: max_values.min(page.page_item_count as u8),
                disabled: false,
            }),
            utility_row: Some(expense_picker_utility_row(
                kind,
                nonce,
                page.snapshot_id,
                page.current_page,
                page.total_pages,
            )),
            detail_lines,
        })
    }

    async fn expense_picker_page_for_session(
        &self,
        ctx: &Context,
        scope: LedgerInteractionScope,
        session: &ExpenseSession,
        kind: ExpensePickerKind,
    ) -> Result<ExpensePickerPage, LedgerRouteError> {
        let roster = self
            .deps
            .roster_fetcher
            .fetch(ctx, scope.guild_id(), scope.channel_id())
            .await?;
        Ok(expense_picker_page(
            &roster,
            session.draft().selection_state(),
            kind,
        ))
    }

    /// Re-open the expense modal prefilled from the session's current basic_info so
    /// the actor can edit amount / note / date without losing their selection
    /// (criterion 229).
    async fn dispatch_expense_basic_edit(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let prefill = claim
            .session()
            .draft()
            .basic_info()
            .map(|info| ExpenseModalPrefill {
                raw_amount: Some(format_money_for_modal(info.amount)),
                raw_note: info.note.as_ref().map(|note| note.as_str().to_owned()),
                raw_date: Some(info.effective_date.to_string()),
            })
            .unwrap_or_default();
        let nonce = self.deps.nonce_provider.next_interaction_nonce();
        self.store_expense_modal_submission(
            nonce,
            MemberId(component.user.id.get()),
            scope.expense_draft_scope_id(),
            ExpenseModalIntent::ModifyExisting {
                session_nonce: observed_nonce,
            },
        );
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

    async fn dispatch_expense_weight_edit(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        if !matches!(
            claim.session().stage(),
            ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::WeightEditor
            }
        ) {
            return self.respond_expense_session_missing(ctx, component).await;
        }
        let roster = self
            .deps
            .roster_fetcher
            .fetch(ctx, scope.guild_id(), scope.channel_id())
            .await?;
        let participants = resolve_selection_against_roster(
            claim.session().draft().selection_state(),
            &roster.roster,
        )
        .resolved;
        if participants.len() > 40 {
            return self
                .reply_component_ephemeral(
                    ctx,
                    component,
                    i18n::weight_editor_too_many_message(),
                    DiscordCallSite::ExpenseStepRefresh,
                )
                .await;
        }
        let response = build_expense_weight_modal_response(observed_nonce, &participants)?;
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_expense_picker_page(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        kind: ExpensePickerKind,
        direction: PickerPageDirection,
        observed_nonce: walicord_application::InteractionNonce,
        expected_snapshot: PickerSnapshotId,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let page = self
            .expense_picker_page_for_session(ctx, scope, claim.session(), kind)
            .await?;
        if page.snapshot_id != expected_snapshot {
            return self.reject_stale_picker_component(ctx, component).await;
        }
        let state = claim
            .session()
            .draft()
            .selection_state()
            .picker_states
            .get(&kind);
        let next_page = match direction {
            PickerPageDirection::Previous => page.current_page.saturating_sub(1),
            PickerPageDirection::Next => page.current_page.saturating_add(1),
        }
        .min(page.total_pages.saturating_sub(1));
        let updated = set_picker_view_state(
            claim.session().clone(),
            kind,
            page.snapshot_id,
            next_page,
            state.and_then(|state| state.query().map(str::to_owned)),
            self.deps.clock.as_ref(),
        )?;
        let dispatch = self
            .respond_with_step_body(ctx, component, scope, &updated)
            .await?;
        claim.replace(updated);
        Ok(dispatch)
    }

    async fn dispatch_expense_picker_clear(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        kind: ExpensePickerKind,
        observed_nonce: walicord_application::InteractionNonce,
        expected_snapshot: PickerSnapshotId,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let page = self
            .expense_picker_page_for_session(ctx, scope, claim.session(), kind)
            .await?;
        if page.snapshot_id != expected_snapshot {
            return self.reject_stale_picker_component(ctx, component).await;
        }
        let session_with_snapshot = set_picker_view_state(
            claim.session().clone(),
            kind,
            page.snapshot_id,
            page.current_page,
            page.query.clone(),
            self.deps.clock.as_ref(),
        )?;
        let updated = clear_picker_selection(
            session_with_snapshot,
            kind,
            page.snapshot_id,
            self.deps.clock.as_ref(),
        )?;
        let dispatch = self
            .respond_with_step_body(ctx, component, scope, &updated)
            .await?;
        claim.replace(updated);
        Ok(dispatch)
    }

    async fn dispatch_expense_picker_search_open(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        kind: ExpensePickerKind,
        observed_nonce: walicord_application::InteractionNonce,
        expected_snapshot: PickerSnapshotId,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let page = self
            .expense_picker_page_for_session(ctx, scope, claim.session(), kind)
            .await?;
        if page.snapshot_id != expected_snapshot {
            return self.reject_stale_picker_component(ctx, component).await;
        }
        let response =
            build_expense_picker_search_modal_response(kind, observed_nonce, expected_snapshot)?;
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
        Ok(InteractionDispatch::Handled)
    }

    fn store_expense_modal_submission(
        &self,
        binding_nonce: walicord_application::InteractionNonce,
        actor_id: MemberId,
        draft_scope_id: ExpenseDraftScopeId,
        intent: ExpenseModalIntent,
    ) {
        self.deps
            .modal_submissions
            .store(ExpenseModalSubmissionBinding::capture(
                binding_nonce,
                actor_id,
                draft_scope_id,
                intent,
                self.deps.clock.now(),
            ));
    }

    async fn dispatch_expense_modal_retry(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        binding_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let actor = MemberId(component.user.id.get());
        let retry = match self.deps.modal_retries.try_consume(
            binding_nonce,
            actor,
            scope.expense_draft_scope_id(),
            self.deps.clock.now(),
        ) {
            Ok(retry) => retry,
            Err(_) => return self.respond_expense_session_missing(ctx, component).await,
        };
        let modal_nonce = self.deps.nonce_provider.next_interaction_nonce();
        self.store_expense_modal_submission(
            modal_nonce,
            actor,
            scope.expense_draft_scope_id(),
            retry.intent,
        );
        let prefill = ExpenseModalPrefill {
            raw_amount: Some(retry.preserved.raw_amount),
            raw_note: Some(retry.preserved.raw_note),
            raw_date: Some(retry.preserved.raw_date),
        };
        let response =
            build_expense_modal_response(self.deps.clock.as_ref(), modal_nonce, &prefill)?;
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseRetryModalCreateResponse,
            ))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn reply_stale_modal(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        modal
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    safe_ephemeral_interaction_response_message()
                        .content(i18n::stale_interaction_message()),
                ),
            )
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseRetryModalCreateResponse,
            ))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn reject_stale_picker_component(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        self.reply_component_ephemeral(
            ctx,
            component,
            i18n::stale_interaction_message(),
            DiscordCallSite::ExpenseStepRefresh,
        )
        .await
    }

    async fn dispatch_expense_back(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        match navigate_back(claim.session().clone(), self.deps.clock.as_ref()) {
            Ok(updated) => {
                let dispatch = self
                    .respond_with_step_body(ctx, component, scope, &updated)
                    .await?;
                claim.replace(updated);
                Ok(dispatch)
            }
            Err(NavigationError::AlreadyAtFirstStep) => {
                // From the first phase Back == Cancel (criterion 201).
                let dispatch = self.respond_expense_cancelled(ctx, component).await?;
                claim.discard();
                Ok(dispatch)
            }
            Err(other) => Err(other.into()),
        }
    }

    async fn take_expense_session_or_reply(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        key: ExpenseSessionKey,
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<Option<ExpenseSessionClaim<'_>>, LedgerRouteError> {
        let now = self.deps.clock.now();
        match self.deps.expense_sessions.claim(key, observed_nonce, now) {
            Ok(Some(session)) => Ok(Some(ExpenseSessionClaim {
                store: self.deps.expense_sessions.as_ref(),
                original: Some(session),
            })),
            Ok(None)
            | Err(
                SessionAccessError::Expired
                | SessionAccessError::Superseded { .. }
                | SessionAccessError::InFlight,
            ) => {
                let owner = self.expense_session_nonce_owner(key, observed_nonce, now);
                if owner.is_some() && owner != Some(key.actor_id()) {
                    self.respond_expense_session_wrong_actor(ctx, component)
                        .await?;
                } else {
                    self.respond_expense_session_missing(ctx, component).await?;
                }
                Ok(None)
            }
        }
    }

    fn expense_session_nonce_owner(
        &self,
        key: ExpenseSessionKey,
        observed_nonce: walicord_application::InteractionNonce,
        now: std::time::SystemTime,
    ) -> Option<MemberId> {
        self.deps
            .expense_sessions
            .active_owner_by_nonce(key.draft_scope_id(), observed_nonce, now)
    }

    async fn respond_expense_session_missing(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = CreateInteractionResponse::Message(
            safe_ephemeral_interaction_response_message()
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

    async fn respond_expense_session_wrong_actor(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = CreateInteractionResponse::Message(
            safe_ephemeral_interaction_response_message()
                .content(walicord_i18n::expense_session_wrong_actor_message()),
        );
        component
            .create_response(&ctx.http, response)
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_expense_cancel(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, component.guild_id, component.channel_id, component)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(component.user.id.get()),
        );
        let Some(mut claim) = self
            .take_expense_session_or_reply(ctx, component, key, observed_nonce)
            .await?
        else {
            return Ok(InteractionDispatch::Handled);
        };
        let dispatch = self.respond_expense_cancelled(ctx, component).await?;
        claim.discard();
        Ok(dispatch)
    }

    async fn respond_expense_cancelled(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let response = CreateInteractionResponse::UpdateMessage(
            safe_ephemeral_interaction_response_message()
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

    async fn dispatch_panel_review_launcher(
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

    async fn dispatch_panel_ledger_launcher(
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

    async fn dispatch_panel_void_launcher(
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

    pub async fn handle_modal(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        if let Some((kind, nonce, snapshot_id)) = parse_expense_picker_custom_id(
            &modal.data.custom_id,
            EXPENSE_PICKER_SEARCH_MODAL_CUSTOM_ID_PREFIX,
        ) {
            return self
                .dispatch_expense_picker_search_submit(ctx, modal, kind, nonce, snapshot_id)
                .await;
        }
        match parse_expense_weight_modal_custom_id(&modal.data.custom_id) {
            ExpenseModalCustomIdMatch::Match { nonce } => {
                return self
                    .dispatch_expense_weight_modal_submit(ctx, modal, nonce)
                    .await;
            }
            ExpenseModalCustomIdMatch::Stale => return self.reply_stale_modal(ctx, modal).await,
            ExpenseModalCustomIdMatch::NoMatch => {}
        }
        match parse_expense_modal_custom_id(&modal.data.custom_id) {
            ExpenseModalCustomIdMatch::Match { nonce } => {
                self.dispatch_expense_modal_submit(ctx, modal, nonce).await
            }
            ExpenseModalCustomIdMatch::Stale => self.reply_stale_modal(ctx, modal).await,
            ExpenseModalCustomIdMatch::NoMatch => Ok(InteractionDispatch::Ignored),
        }
    }

    async fn dispatch_expense_weight_modal_submit(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        observed_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, modal.guild_id, modal.channel_id, modal)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(modal.user.id.get()),
        );
        let claimed =
            match self
                .deps
                .expense_sessions
                .claim(key, observed_nonce, self.deps.clock.now())
            {
                Ok(Some(claimed)) => claimed,
                Ok(None)
                | Err(
                    SessionAccessError::Expired
                    | SessionAccessError::Superseded { .. }
                    | SessionAccessError::InFlight,
                ) => return self.reply_stale_modal(ctx, modal).await,
            };
        let mut claim = ExpenseSessionClaim {
            store: self.deps.expense_sessions.as_ref(),
            original: Some(claimed),
        };
        let weights = match parse_expense_weight_modal_submission(modal) {
            Ok(weights) => weights,
            Err(_) => {
                modal
                    .create_response(
                        &ctx.http,
                        CreateInteractionResponse::Message(
                            safe_ephemeral_interaction_response_message()
                                .content(i18n::weight_editor_parse_error()),
                        ),
                    )
                    .await
                    .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
                return Ok(InteractionDispatch::Handled);
            }
        };
        let updated =
            replace_weight_overrides(claim.session().clone(), weights, self.deps.clock.as_ref())?;
        let button_ids = build_selection_step_button_ids(updated.nonce());
        let model =
            build_expense_selection_step_surface(&ExpenseSelectionPhase::WeightEditor, &button_ids);
        let rendered = DiscordLedgerPresenter::render_expense_step(&model)?;
        let (body, components) = rendered_surface_to_message(rendered);
        modal
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    safe_ephemeral_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
        claim.replace(updated);
        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_expense_picker_search_submit(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        kind: ExpensePickerKind,
        observed_nonce: walicord_application::InteractionNonce,
        expected_snapshot: PickerSnapshotId,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, modal.guild_id, modal.channel_id, modal)
            .await?;
        let key = ExpenseSessionKey::new(
            scope.expense_draft_scope_id(),
            MemberId(modal.user.id.get()),
        );
        let Some(claimed) = self
            .deps
            .expense_sessions
            .claim(key, observed_nonce, self.deps.clock.now())
            .map_err(LedgerRouteError::from)?
        else {
            return self.reply_stale_modal(ctx, modal).await;
        };
        let mut claim = ExpenseSessionClaim {
            store: self.deps.expense_sessions.as_ref(),
            original: Some(claimed),
        };
        let query = extract_expense_picker_search_query(modal).trim();
        if query.is_empty() {
            modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_ephemeral_interaction_response_message()
                            .content(i18n::search_blank_error()),
                    ),
                )
                .await
                .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
            return Ok(InteractionDispatch::Handled);
        }
        let roster = self
            .deps
            .roster_fetcher
            .fetch(ctx, scope.guild_id(), scope.channel_id())
            .await?;
        let page = expense_picker_page(&roster, claim.session().draft().selection_state(), kind);
        if page.snapshot_id != expected_snapshot {
            return self.reply_stale_modal(ctx, modal).await;
        }
        if !expense_picker_query_matches(
            &roster,
            claim.session().draft().selection_state(),
            kind,
            query,
        ) {
            modal
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Message(
                        safe_ephemeral_interaction_response_message()
                            .content(expense_picker_search_not_found_message(kind)),
                    ),
                )
                .await
                .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
            return Ok(InteractionDispatch::Handled);
        }
        let updated = set_picker_view_state(
            claim.session().clone(),
            kind,
            page.snapshot_id,
            0,
            Some(query.to_owned()),
            self.deps.clock.as_ref(),
        )?;
        let (body, components) = self
            .build_expense_step_message(ctx, scope, &updated)
            .await?;
        modal
            .create_response(
                &ctx.http,
                CreateInteractionResponse::UpdateMessage(
                    safe_ephemeral_interaction_response_message()
                        .content(body)
                        .components(components),
                ),
            )
            .await
            .map_err(discord_call_error(DiscordCallSite::ExpenseStepRefresh))?;
        claim.replace(updated);
        Ok(InteractionDispatch::Handled)
    }

    async fn dispatch_expense_modal_submit(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        binding_nonce: walicord_application::InteractionNonce,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let scope = self
            .guard_scope(ctx, modal.guild_id, modal.channel_id, modal)
            .await?;
        let guild_id = scope.guild_id();
        let actor = MemberId(modal.user.id.get());
        let intent = match self.deps.modal_submissions.try_consume(
            binding_nonce,
            actor,
            scope.expense_draft_scope_id(),
            self.deps.clock.now(),
        ) {
            Ok(intent) => intent,
            Err(_) => return self.reply_stale_modal(ctx, modal).await,
        };

        let raw = extract_raw_expense_modal_submission(modal)
            .ok_or(InternalLedgerRouteError::ModalSubmissionMissingFields)?;

        match validate_expense_modal_submission(&raw, self.deps.clock.as_ref()) {
            Err(error) => {
                let preserved = ModalRetryPreserved {
                    raw_amount: raw.raw_amount.clone(),
                    raw_note: raw.raw_note.clone(),
                    raw_date: raw.raw_date.clone(),
                };
                self.respond_with_retry_modal(
                    ctx,
                    modal,
                    scope.expense_draft_scope_id(),
                    preserved,
                    intent,
                    error,
                )
                .await
            }
            Ok(validated) => {
                let _ = guild_id;
                let key = ExpenseSessionKey::new(scope.expense_draft_scope_id(), actor);
                match intent {
                    ExpenseModalIntent::Create { origin } => {
                        let (session, _) = bootstrap_expense_session(
                            key,
                            origin,
                            validated,
                            self.deps.clock.as_ref(),
                            self.deps.nonce_provider.as_ref(),
                        )?;
                        let replacement_notice = self
                            .deps
                            .expense_sessions
                            .has_active_session(key, self.deps.clock.now())
                            .then(i18n::expense_session_replaced_message);
                        let dispatch = self
                            .acknowledge_modal_success(
                                ctx,
                                modal,
                                scope,
                                &session,
                                replacement_notice,
                            )
                            .await?;
                        self.deps.expense_sessions.replace(session);
                        Ok(dispatch)
                    }
                    ExpenseModalIntent::ModifyExisting { session_nonce } => {
                        let Some(claimed) = (match self.deps.expense_sessions.claim(
                            key,
                            session_nonce,
                            self.deps.clock.now(),
                        ) {
                            Ok(claimed) => claimed,
                            Err(
                                SessionAccessError::Expired
                                | SessionAccessError::Superseded { .. }
                                | SessionAccessError::InFlight,
                            ) => return self.reply_stale_modal(ctx, modal).await,
                        }) else {
                            return self.reply_stale_modal(ctx, modal).await;
                        };
                        let mut claim = ExpenseSessionClaim {
                            store: self.deps.expense_sessions.as_ref(),
                            original: Some(claimed),
                        };
                        let updated = apply_modified_basic_info(
                            claim.session().clone(),
                            validated.into(),
                            self.deps.clock.as_ref(),
                        )?;
                        let dispatch = self
                            .acknowledge_modal_success(ctx, modal, scope, &updated, None)
                            .await?;
                        claim.replace(updated);
                        Ok(dispatch)
                    }
                }
            }
        }
    }

    async fn respond_with_retry_modal(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        draft_scope_id: ExpenseDraftScopeId,
        preserved: ModalRetryPreserved,
        intent: ExpenseModalIntent,
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
            draft_scope_id,
            preserved.clone(),
            intent,
            self.deps.clock.now(),
        );
        self.deps.modal_retries.store(binding);

        modal
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    safe_ephemeral_interaction_response_message()
                        .content(expense_modal_validation_message(&validation_error))
                        .components(vec![expense_modal_retry_row(binding_nonce)]),
                ),
            )
            .await
            .map_err(discord_call_error(
                DiscordCallSite::ExpenseRetryModalCreateResponse,
            ))?;
        Ok(InteractionDispatch::Handled)
    }

    async fn acknowledge_modal_success(
        &self,
        ctx: &Context,
        modal: &ModalInteraction,
        scope: LedgerInteractionScope,
        session: &ExpenseSession,
        notice: Option<&'static str>,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let nonce = session.nonce();
        let button_ids = build_selection_step_button_ids(nonce);
        let mut model =
            build_expense_selection_step_surface(&ExpenseSelectionPhase::Payer, &button_ids);
        let picker = self
            .expense_picker_render_parts(ctx, scope, session, &ExpenseSelectionPhase::Payer, nonce)
            .await?;
        model.detail_lines.extend(picker.detail_lines);
        if let Some(row) = picker.utility_row {
            model.action_rows.insert(0, row);
        }
        if let Some(menu) = picker.select_menu {
            model.action_rows.insert(0, SurfaceActionRow::Select(menu));
        }
        if let Some(notice) = notice {
            model.detail_lines.insert(0, notice.to_owned());
        }
        let rendered = DiscordLedgerPresenter::render_expense_step(&model)?;
        let (body, components) = rendered_surface_to_message(rendered);
        let response = CreateInteractionResponse::Message(
            safe_ephemeral_interaction_response_message()
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
        self.edit_command_response_with_components(ctx, command, content, Vec::new(), site)
            .await
    }

    async fn edit_command_response_with_components(
        &self,
        ctx: &Context,
        command: &CommandInteraction,
        content: impl Into<String>,
        components: Vec<CreateActionRow>,
        site: DiscordCallSite,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        command
            .edit_response(
                &ctx.http,
                safe_edit_interaction_response()
                    .content(content)
                    .components(components),
            )
            .await
            .map_err(discord_call_error(site))?;
        Ok(InteractionDispatch::Handled)
    }

    fn uncertain_write_block_response(
        &self,
        ledger_id: LedgerId,
        preserve_input: bool,
        preserve_preview: bool,
    ) -> (String, Vec<CreateActionRow>) {
        let mut message = uncertain_write_block_message(preserve_input, preserve_preview);
        let Some(UncertainWriteState::Abandoned(retained)) =
            self.deps.uncertain_writes.current(ledger_id)
        else {
            return (message, Vec::new());
        };
        message.push('\n');
        let _ = write!(
            message,
            "{}",
            i18n::abandoned_uncertain_write_message(retained.last_known_summary())
        );
        (
            message,
            vec![CreateActionRow::Buttons(vec![
                CreateButton::new(UNCERTAIN_WRITE_ACKNOWLEDGE_CUSTOM_ID)
                    .label(i18n::abandoned_uncertain_write_acknowledge_label()),
            ])],
        )
    }

    async fn edit_initial_void_model(
        &self,
        ctx: &Context,
        interaction: DeferredEphemeralInteraction<'_>,
        model: VoidSurfaceModel,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let rendered =
            DiscordLedgerPresenter::render_void_flow(&model).map_err(LedgerRouteError::from)?;
        let (body, components) = rendered_surface_to_message(rendered);
        interaction
            .edit(ctx, body, components, DiscordCallSite::VoidEditResponse)
            .await
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
                safe_edit_interaction_response()
                    .content(body)
                    .components(components),
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
                    safe_interaction_response_message()
                        .content(content)
                        .components(components),
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
        self.edit_component_response_with_components(ctx, component, content, Vec::new(), site)
            .await
    }

    async fn edit_component_response_with_components(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        content: impl Into<String>,
        components: Vec<CreateActionRow>,
        site: DiscordCallSite,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        component
            .edit_response(
                &ctx.http,
                safe_edit_interaction_response()
                    .content(content)
                    .components(components),
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
        self.reply_component_ephemeral_with_components(ctx, component, content, Vec::new(), site)
            .await
    }

    async fn reply_component_ephemeral_with_components(
        &self,
        ctx: &Context,
        component: &ComponentInteraction,
        content: impl Into<String>,
        components: Vec<CreateActionRow>,
        site: DiscordCallSite,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        component
            .create_response(
                &ctx.http,
                CreateInteractionResponse::Message(
                    safe_ephemeral_interaction_response_message()
                        .content(content)
                        .components(components),
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
        let now = self.deps.clock.now();
        match self.deps.void_sessions.access(key, nonce, now) {
            Ok(Some(session)) => Ok(Some(session)),
            Ok(None) | Err(SessionAccessError::Expired | SessionAccessError::Superseded { .. }) => {
                let owner =
                    self.deps
                        .void_sessions
                        .active_owner_by_nonce(key.ledger_id(), nonce, now);
                if owner.is_some() && owner != Some(key.actor_id()) {
                    self.reply_component_ephemeral(
                        ctx,
                        component,
                        i18n::void_session_wrong_actor_message(),
                        DiscordCallSite::VoidEditResponse,
                    )
                    .await?;
                } else {
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
                }
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
        tracked_parent_channel_id: ChannelId,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
        session: VoidSession,
        render_kind: VoidSelectionRenderKind,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let binding = CanonicalThreadBinding::new(
            TrackedParentKey::from_guarded_parent(guild_id, tracked_parent_channel_id),
            canonical_thread_id,
        );
        let load = self
            .load_verified_thread(ctx, binding, CanonicalLoadRoute::Read)
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
            .fetch(ctx, guild_id, tracked_parent_channel_id)
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
pub(crate) const EXPENSE_MODAL_RETRY_CUSTOM_ID_PREFIX: &str = "ledger:expense:retry:";
pub(crate) const EXPENSE_BACK_CUSTOM_ID_PREFIX: &str = "ledger:expense:back:";
pub(crate) const EXPENSE_BASIC_EDIT_CUSTOM_ID_PREFIX: &str = "ledger:expense:basic-edit:";
pub(crate) const EXPENSE_PAYER_PICK_CUSTOM_ID_PREFIX: &str = "ledger:expense:payer-pick:";
pub(crate) const EXPENSE_INDIVIDUAL_PICK_CUSTOM_ID_PREFIX: &str = "ledger:expense:individual-pick:";
pub(crate) const EXPENSE_ROLE_PICK_CUSTOM_ID_PREFIX: &str = "ledger:expense:role-pick:";
pub(crate) const EXPENSE_PICKER_PREV_CUSTOM_ID_PREFIX: &str = "ledger:expense:picker-prev:";
pub(crate) const EXPENSE_PICKER_NEXT_CUSTOM_ID_PREFIX: &str = "ledger:expense:picker-next:";
pub(crate) const EXPENSE_PICKER_SEARCH_CUSTOM_ID_PREFIX: &str = "ledger:expense:picker-search:";
pub(crate) const EXPENSE_PICKER_CLEAR_CUSTOM_ID_PREFIX: &str = "ledger:expense:picker-clear:";
pub(crate) const EXPENSE_PICKER_SEARCH_MODAL_CUSTOM_ID_PREFIX: &str =
    "ledger:expense:picker-search-modal:";
const EXPENSE_PICKER_SEARCH_FIELD: &str = "query";
const EXPENSE_PICKER_PAGE_SIZE: usize = 25;
pub(crate) const UNCERTAIN_WRITE_ACKNOWLEDGE_CUSTOM_ID: &str = "ledger:uncertain:acknowledge";
pub(crate) const EXPENSE_TO_PARTICIPANTS_CUSTOM_ID_PREFIX: &str = "ledger:expense:to-participants:";
pub(crate) const EXPENSE_SOURCE_INDIVIDUAL_CUSTOM_ID_PREFIX: &str =
    "ledger:expense:source-individual:";
pub(crate) const EXPENSE_SOURCE_ROLES_CUSTOM_ID_PREFIX: &str = "ledger:expense:source-roles:";
pub(crate) const EXPENSE_SOURCE_MEMBERS_CUSTOM_ID_PREFIX: &str = "ledger:expense:source-members:";
pub(crate) const EXPENSE_TO_WEIGHTS_CUSTOM_ID_PREFIX: &str = "ledger:expense:to-weights:";
pub(crate) const EXPENSE_WEIGHT_EDIT_CUSTOM_ID_PREFIX: &str = "ledger:expense:weight-edit:";
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
    let n = nonce;
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

fn expense_modal_retry_row(
    nonce: walicord_application::InteractionNonce,
) -> serenity::all::CreateActionRow {
    use serenity::all::{ButtonStyle, CreateActionRow, CreateButton};

    CreateActionRow::Buttons(vec![
        CreateButton::new(format!("{EXPENSE_MODAL_RETRY_CUSTOM_ID_PREFIX}{nonce}"))
            .label(i18n::expense_modal_retry_button_label())
            .style(ButtonStyle::Primary),
    ])
}

fn expense_modal_validation_message(error: &ExpenseModalValidationError) -> &'static str {
    match error {
        ExpenseModalValidationError::AmountBlank
        | ExpenseModalValidationError::AmountNonInteger
        | ExpenseModalValidationError::AmountNonPositive
        | ExpenseModalValidationError::AmountMalformed => i18n::expense_invalid_amount_message(),
        ExpenseModalValidationError::NoteTooLong { .. } => i18n::expense_note_too_long_message(),
        ExpenseModalValidationError::DateMalformed => i18n::expense_invalid_date_message(),
    }
}

fn expense_source_descriptor(origin: ExpenseLaunchOrigin) -> DiscordLedgerSourceDescriptor {
    match origin {
        ExpenseLaunchOrigin::SlashCommand => {
            DiscordLedgerSourceDescriptor::expense_slash_modal_v1()
        }
        ExpenseLaunchOrigin::PanelButton => DiscordLedgerSourceDescriptor::expense_panel_modal_v1(),
    }
}

/// Build the custom_id strings for every button the selection wizard can show. The
/// adapter owns the custom_id format (Discord component identity); the presentation
/// builder takes them as opaque strings.
fn build_selection_step_button_ids(
    nonce: walicord_application::InteractionNonce,
) -> ExpenseSelectionStepButtonIds {
    let n = nonce;
    ExpenseSelectionStepButtonIds {
        to_participants: format!("{EXPENSE_TO_PARTICIPANTS_CUSTOM_ID_PREFIX}{n}"),
        source_individual: format!("{EXPENSE_SOURCE_INDIVIDUAL_CUSTOM_ID_PREFIX}{n}"),
        source_roles: format!("{EXPENSE_SOURCE_ROLES_CUSTOM_ID_PREFIX}{n}"),
        source_members: format!("{EXPENSE_SOURCE_MEMBERS_CUSTOM_ID_PREFIX}{n}"),
        to_weights: format!("{EXPENSE_TO_WEIGHTS_CUSTOM_ID_PREFIX}{n}"),
        weight_edit: format!("{EXPENSE_WEIGHT_EDIT_CUSTOM_ID_PREFIX}{n}"),
        to_confirm: format!("{EXPENSE_TO_CONFIRM_CUSTOM_ID_PREFIX}{n}"),
        back: format!("{EXPENSE_BACK_CUSTOM_ID_PREFIX}{n}"),
        cancel: format!("{EXPENSE_CANCEL_CUSTOM_ID_PREFIX}{n}"),
    }
}

/// Build the custom_id strings for the confirmation-page buttons.
fn build_confirmation_button_ids(
    nonce: walicord_application::InteractionNonce,
) -> ExpenseConfirmationButtonIds {
    let n = nonce;
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
        // The permalink only exists after the canonical message has been posted.
        recovery_reference: RecoveryReference {
            ledger_id_short: format!("{ledger_id:x}"),
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
            ledger_id_short: format!("{ledger_id:08x}"),
            entry_id: entry.id,
            message_link: None,
        },
    });

    let rendered = DiscordLedgerPresenter::render_public_entry(&model).map_err(|error| {
        LedgerRouteError::Internal(InternalLedgerRouteError::PanelRender(error))
    })?;
    Ok(rendered.body().to_owned())
}

fn review_route_guidance_lines_with_replacement_notice(route: ReadViewRoute) -> Vec<String> {
    let mut lines = vec![
        i18n::settlement_preview_replaced_message().to_owned(),
        i18n::route_task_guidance().to_owned(),
    ];
    if matches!(route, ReadViewRoute::ReviewParent) {
        lines.push(i18n::parent_preview_entry_guidance().to_owned());
    }
    lines
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

fn actor_can_view_channel(permissions: Option<Permissions>) -> bool {
    permissions.is_some_and(|permissions| permissions.contains(Permissions::VIEW_CHANNEL))
}

fn first_missing_permission_action(current: Permissions) -> PermissionAction {
    if !current.contains(Permissions::VIEW_CHANNEL) {
        PermissionAction::ViewChannel
    } else if !current.contains(Permissions::READ_MESSAGE_HISTORY) {
        PermissionAction::ReadMessageHistory
    } else if !current.contains(Permissions::SEND_MESSAGES)
        || !current.contains(Permissions::SEND_MESSAGES_IN_THREADS)
    {
        PermissionAction::SendMessageInChannel
    } else if !current.contains(Permissions::ATTACH_FILES) {
        PermissionAction::AttachFiles
    } else if !current.contains(Permissions::CREATE_PUBLIC_THREADS) {
        PermissionAction::CreatePublicThread
    } else {
        PermissionAction::ManageThreads
    }
}

fn recovery_reference_channel_id(reference: &LocatorRecoveryReference) -> Option<ChannelId> {
    match reference {
        LocatorRecoveryReference::Channel { channel_id, .. } => Some(*channel_id),
        LocatorRecoveryReference::Ledger { .. } => None,
    }
}

fn ledger_refresh_recovery_reference(binding: CanonicalThreadBinding) -> LocatorRecoveryReference {
    LocatorRecoveryReference::ledger(
        format!("{:08x}", binding.ledger_id()),
        Some(format!(
            "https://discord.com/channels/{}/{}",
            binding.tracked_parent().guild_id().get(),
            binding.canonical_thread_id().get()
        )),
    )
}

fn canonical_load_failure_blocks_locator(failure: CanonicalLoadFailure) -> bool {
    matches!(
        failure,
        CanonicalLoadFailure::AttachmentCardinality { .. }
            | CanonicalLoadFailure::OversizeAttachment { .. }
            | CanonicalLoadFailure::Decode { .. }
            | CanonicalLoadFailure::VersionMismatch { .. }
            | CanonicalLoadFailure::WriterLineage { .. }
            | CanonicalLoadFailure::Chain { .. }
            | CanonicalLoadFailure::Structure { .. }
            | CanonicalLoadFailure::Projection { .. }
            | CanonicalLoadFailure::MetadataCoherence { .. }
            | CanonicalLoadFailure::DisplayDrift { .. }
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VoidSelectionRenderKind {
    Normal,
    MissingSelection,
    StaleTarget(VoidRetargetReason),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PickerPageDirection {
    Previous,
    Next,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct ExpensePickerRenderParts {
    select_menu: Option<SurfaceSelectMenu>,
    utility_row: Option<SurfaceActionRow>,
    detail_lines: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ExpensePickerPage {
    snapshot_id: PickerSnapshotId,
    query: Option<String>,
    options: Vec<SurfaceSelectOption>,
    visible_values: Vec<u64>,
    detail_lines: Vec<String>,
    current_page: usize,
    total_pages: usize,
    total_items: usize,
    page_item_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ExpensePickerItem {
    value: u64,
    label: SafeLiteralText,
    selected: bool,
}

fn picker_kind_for_phase(phase: &ExpenseSelectionPhase) -> Option<ExpensePickerKind> {
    match phase {
        ExpenseSelectionPhase::Payer => Some(ExpensePickerKind::Payer),
        ExpenseSelectionPhase::IndividualSelection => Some(ExpensePickerKind::Individuals),
        ExpenseSelectionPhase::Roles => Some(ExpensePickerKind::Roles),
        ExpenseSelectionPhase::ParticipantSource | ExpenseSelectionPhase::WeightEditor => None,
    }
}

fn expense_picker_page(
    roster: &RouterRosterSnapshot,
    selection: &ExpenseSelectionState,
    kind: ExpensePickerKind,
) -> ExpensePickerPage {
    let query = selection
        .picker_states
        .get(&kind)
        .and_then(|state| state.query().map(str::to_owned));
    let snapshot_id = expense_picker_snapshot_id(roster, kind);
    let requested_page = selection
        .picker_states
        .get(&kind)
        .map_or(0, |state| state.current_page());
    let mut items = expense_picker_items(roster, selection, kind);
    items.sort_by(|left, right| {
        left.label
            .as_str()
            .cmp(right.label.as_str())
            .then_with(|| left.value.cmp(&right.value))
    });
    if let Some(query) = query.as_deref() {
        let normalized = query.to_lowercase();
        items.retain(|item| item.label.as_str().to_lowercase().contains(&normalized));
    }
    let total_items = items.len();
    let total_pages = total_items.div_ceil(EXPENSE_PICKER_PAGE_SIZE).max(1);
    let current_page = requested_page.min(total_pages.saturating_sub(1));
    let page_start = current_page * EXPENSE_PICKER_PAGE_SIZE;
    let page_end = (page_start + EXPENSE_PICKER_PAGE_SIZE).min(total_items);
    let page_items = if total_items == 0 {
        Vec::new()
    } else {
        items[page_start..page_end].to_vec()
    };
    let mut detail_lines = Vec::new();
    match kind {
        ExpensePickerKind::Payer | ExpensePickerKind::Individuals => {
            detail_lines.push(i18n::member_picker_help().to_owned());
        }
        ExpensePickerKind::Roles => detail_lines.push(i18n::role_picker_help().to_owned()),
    }
    if let Some(query) = query.as_deref() {
        detail_lines.push(i18n::expense_search_line(query).to_string());
        if total_items == 0 {
            detail_lines.push(
                match kind {
                    ExpensePickerKind::Roles => i18n::role_search_not_found_error(),
                    ExpensePickerKind::Payer | ExpensePickerKind::Individuals => {
                        i18n::member_search_not_found_error()
                    }
                }
                .to_owned(),
            );
        }
    }
    detail_lines.push(i18n::page_indicator(current_page + 1, total_pages).to_string());
    if total_items > 0 {
        detail_lines
            .push(i18n::page_range_indicator(page_start + 1, page_end, total_items).to_string());
    }
    let visible_values = page_items.iter().map(|item| item.value).collect::<Vec<_>>();
    let options = page_items
        .into_iter()
        .map(|item| SurfaceSelectOption {
            value: item.value.to_string(),
            label: item.label,
            description: None,
            selected: item.selected,
        })
        .collect::<Vec<_>>();
    ExpensePickerPage {
        snapshot_id,
        query,
        page_item_count: options.len(),
        options,
        visible_values,
        detail_lines,
        current_page,
        total_pages,
        total_items,
    }
}

fn expense_picker_query_matches(
    roster: &RouterRosterSnapshot,
    selection: &ExpenseSelectionState,
    kind: ExpensePickerKind,
    query: &str,
) -> bool {
    let normalized = query.to_lowercase();
    expense_picker_items(roster, selection, kind)
        .into_iter()
        .any(|item| item.label.as_str().to_lowercase().contains(&normalized))
}

fn expense_picker_search_not_found_message(kind: ExpensePickerKind) -> &'static str {
    match kind {
        ExpensePickerKind::Roles => i18n::role_search_not_found_error(),
        ExpensePickerKind::Payer | ExpensePickerKind::Individuals => {
            i18n::member_search_not_found_error()
        }
    }
}

fn expense_picker_snapshot_id(
    roster: &RouterRosterSnapshot,
    kind: ExpensePickerKind,
) -> PickerSnapshotId {
    let mut hash = 0xcbf2_9ce4_8422_2325u64;
    for item in expense_picker_items(roster, &ExpenseSelectionState::default(), kind) {
        for byte in item.value.to_be_bytes() {
            hash = hash
                .wrapping_mul(0x100_0000_01b3)
                .wrapping_add(u64::from(byte));
        }
        for byte in item.label.as_str().as_bytes() {
            hash = hash
                .wrapping_mul(0x100_0000_01b3)
                .wrapping_add(u64::from(*byte));
        }
    }
    PickerSnapshotId::new(hash)
}

fn expense_picker_items(
    roster: &RouterRosterSnapshot,
    selection: &ExpenseSelectionState,
    kind: ExpensePickerKind,
) -> Vec<ExpensePickerItem> {
    match kind {
        ExpensePickerKind::Payer | ExpensePickerKind::Individuals => {
            let labels = SurfaceMemberLabels::from_member_names(
                roster.roster.all_members.iter().map(|member_id| {
                    (
                        *member_id,
                        roster
                            .display_names
                            .get(member_id)
                            .map(|name| name.as_str()),
                    )
                }),
            );
            roster
                .roster
                .all_members
                .iter()
                .map(|member_id| ExpensePickerItem {
                    value: member_id.0,
                    label: labels.safe_member_label(*member_id),
                    selected: match kind {
                        ExpensePickerKind::Payer => selection.payer == Some(*member_id),
                        ExpensePickerKind::Individuals => {
                            selection.individual_members.contains(member_id)
                        }
                        ExpensePickerKind::Roles => false,
                    },
                })
                .collect()
        }
        ExpensePickerKind::Roles => roster
            .roster
            .role_members
            .keys()
            .map(|role_id| ExpensePickerItem {
                value: role_id.0,
                label: roster
                    .role_display_names
                    .get(role_id)
                    .and_then(|name| SafeLiteralText::from_roster_label(name.as_str()))
                    .unwrap_or_else(|| {
                        SafeLiteralText::from_roster_label(
                            &i18n::unknown_role_label(role_id.0).to_string(),
                        )
                        .expect("fallback role label should sanitize")
                    }),
                selected: selection.selected_roles.contains(role_id),
            })
            .collect(),
    }
}

fn expense_picker_utility_row(
    kind: ExpensePickerKind,
    nonce: walicord_application::InteractionNonce,
    snapshot_id: PickerSnapshotId,
    current_page: usize,
    total_pages: usize,
) -> SurfaceActionRow {
    SurfaceActionRow::Buttons(vec![
        SurfaceButton::Interactive {
            label: i18n::picker_previous_page_label().to_owned(),
            custom_id: expense_picker_custom_id(
                EXPENSE_PICKER_PREV_CUSTOM_ID_PREFIX,
                kind,
                nonce,
                snapshot_id,
            ),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: current_page == 0,
        },
        SurfaceButton::Interactive {
            label: i18n::picker_next_page_label().to_owned(),
            custom_id: expense_picker_custom_id(
                EXPENSE_PICKER_NEXT_CUSTOM_ID_PREFIX,
                kind,
                nonce,
                snapshot_id,
            ),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: current_page + 1 >= total_pages,
        },
        SurfaceButton::Interactive {
            label: i18n::picker_search_label().to_owned(),
            custom_id: expense_picker_custom_id(
                EXPENSE_PICKER_SEARCH_CUSTOM_ID_PREFIX,
                kind,
                nonce,
                snapshot_id,
            ),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: false,
        },
        SurfaceButton::Interactive {
            label: picker_clear_label(kind).to_owned(),
            custom_id: expense_picker_custom_id(
                EXPENSE_PICKER_CLEAR_CUSTOM_ID_PREFIX,
                kind,
                nonce,
                snapshot_id,
            ),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: false,
        },
    ])
}

fn picker_clear_label(kind: ExpensePickerKind) -> &'static str {
    match kind {
        ExpensePickerKind::Payer => i18n::payer_clear_label(),
        ExpensePickerKind::Individuals => i18n::individual_clear_label(),
        ExpensePickerKind::Roles => i18n::role_clear_label(),
    }
}

fn expense_picker_custom_id(
    prefix: &str,
    kind: ExpensePickerKind,
    nonce: walicord_application::InteractionNonce,
    snapshot_id: PickerSnapshotId,
) -> String {
    format!(
        "{prefix}{}:{nonce}:{snapshot_id}",
        expense_picker_kind_slug(kind)
    )
}

fn parse_expense_picker_custom_id(
    custom_id: &str,
    prefix: &str,
) -> Option<(
    ExpensePickerKind,
    walicord_application::InteractionNonce,
    PickerSnapshotId,
)> {
    let remainder = custom_id.strip_prefix(prefix)?;
    let (kind, remainder) = remainder.split_once(':')?;
    let (nonce, snapshot_id) = remainder.split_once(':')?;
    let kind = parse_expense_picker_kind(kind)?;
    let nonce = nonce
        .parse::<u64>()
        .ok()
        .and_then(|value| walicord_application::InteractionNonce::new(value).ok())?;
    let snapshot_id = snapshot_id.parse::<PickerSnapshotId>().ok()?;
    Some((kind, nonce, snapshot_id))
}

fn expense_picker_selection_custom_id(
    prefix: &str,
    nonce: walicord_application::InteractionNonce,
    snapshot_id: PickerSnapshotId,
) -> String {
    format!("{prefix}{nonce}:{snapshot_id}")
}

fn parse_expense_picker_selection_custom_id(
    custom_id: &str,
    prefix: &str,
) -> Option<(walicord_application::InteractionNonce, PickerSnapshotId)> {
    let remainder = custom_id.strip_prefix(prefix)?;
    let (nonce, snapshot_id) = remainder.split_once(':')?;
    let nonce = nonce
        .parse::<u64>()
        .ok()
        .and_then(|value| walicord_application::InteractionNonce::new(value).ok())?;
    let snapshot_id = snapshot_id.parse::<PickerSnapshotId>().ok()?;
    Some((nonce, snapshot_id))
}

fn expense_picker_kind_slug(kind: ExpensePickerKind) -> &'static str {
    match kind {
        ExpensePickerKind::Payer => "payer",
        ExpensePickerKind::Individuals => "individuals",
        ExpensePickerKind::Roles => "roles",
    }
}

fn parse_expense_picker_kind(value: &str) -> Option<ExpensePickerKind> {
    match value {
        "payer" => Some(ExpensePickerKind::Payer),
        "individuals" => Some(ExpensePickerKind::Individuals),
        "roles" => Some(ExpensePickerKind::Roles),
        _ => None,
    }
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
fn build_expense_picker_search_modal_response(
    kind: ExpensePickerKind,
    nonce: walicord_application::InteractionNonce,
    snapshot_id: PickerSnapshotId,
) -> Result<CreateInteractionResponse, LedgerRouteError> {
    let custom_id = expense_picker_custom_id(
        EXPENSE_PICKER_SEARCH_MODAL_CUSTOM_ID_PREFIX,
        kind,
        nonce,
        snapshot_id,
    );
    validate_custom_id(&custom_id)?;
    let (title, label, placeholder) = match kind {
        ExpensePickerKind::Payer | ExpensePickerKind::Individuals => (
            i18n::member_search_modal_title(),
            i18n::member_search_input_label(),
            i18n::member_search_placeholder(),
        ),
        ExpensePickerKind::Roles => (
            i18n::role_search_modal_title(),
            i18n::role_search_input_label(),
            i18n::role_search_placeholder(),
        ),
    };
    let title = truncate_component_label(title);
    validate_modal_title(&title)?;
    let label = truncate_component_label(label);
    validate_text_input_label(&label)?;
    let placeholder = truncate_component_label(placeholder);
    validate_text_input_placeholder(&placeholder)?;
    let input = CreateInputText::new(InputTextStyle::Short, label, EXPENSE_PICKER_SEARCH_FIELD)
        .placeholder(placeholder)
        .required(true);
    Ok(CreateInteractionResponse::Modal(
        CreateModal::new(custom_id, title).components(vec![CreateActionRow::InputText(input)]),
    ))
}

fn extract_expense_picker_search_query(modal: &ModalInteraction) -> &str {
    for row in &modal.data.components {
        for component in &row.components {
            if let serenity::all::ActionRowComponent::InputText(input) = component
                && input.custom_id == EXPENSE_PICKER_SEARCH_FIELD
            {
                return input.value.as_deref().unwrap_or_default();
            }
        }
    }
    ""
}

fn merge_paged_selection(existing: &[u64], visible: &[u64], selected: &[u64]) -> Vec<u64> {
    let mut merged = existing
        .iter()
        .copied()
        .filter(|value| !visible.contains(value))
        .chain(selected.iter().copied())
        .collect::<Vec<_>>();
    merged.sort_unstable();
    merged.dedup();
    merged
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

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub(crate) enum ComponentSelectionParseError {
    #[error("component is not a string select")]
    NotStringSelect,
    #[error("selection value is not an unsigned integer: {0}")]
    InvalidValue(String),
}

fn selected_component_values(
    component: &ComponentInteraction,
) -> Result<Vec<u64>, ComponentSelectionParseError> {
    component_selection_values(&component.data.kind)
}

fn component_selection_values(
    kind: &ComponentInteractionDataKind,
) -> Result<Vec<u64>, ComponentSelectionParseError> {
    match kind {
        ComponentInteractionDataKind::StringSelect { values } => {
            parse_component_selection_values(values)
        }
        _ => Err(ComponentSelectionParseError::NotStringSelect),
    }
}

fn parse_component_selection_values(
    values: &[String],
) -> Result<Vec<u64>, ComponentSelectionParseError> {
    values
        .iter()
        .map(|value| {
            value
                .parse::<u64>()
                .map_err(|_| ComponentSelectionParseError::InvalidValue(value.clone()))
        })
        .collect()
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
        ledger_id_short: format!("{ledger_id:08x}"),
        entry_id: view.entry().id,
        message_link: Some(view.message_link().to_owned()),
    }
}

fn void_selection_action_rows(
    nonce: walicord_application::InteractionNonce,
    candidates: &[VerifiedLedgerEntryView],
    labels: &SurfaceMemberLabels,
) -> Vec<SurfaceActionRow> {
    let n = nonce;
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
    let n = nonce;
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
) -> SafeLiteralText {
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
    SafeLiteralText::from_roster_label(&label).expect("void candidate select label should sanitize")
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
            ledger_id_short: format!("{ledger_id:08x}"),
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
    use std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        time::UNIX_EPOCH,
    };
    use walicord_application::{
        InteractionNonce, PreviewInstanceId,
        ledger::{
            AllocationSnapshot, ExpenseRecorded, MemberAmount, NormalizedSettlementPlanRecorded,
            ledger_chain_genesis_sha256_v1, participant_resolution::RosterSnapshot,
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

    fn nonce(value: u64) -> InteractionNonce {
        InteractionNonce::new(value).expect("nonce should be non-zero")
    }

    #[rstest]
    #[case(None, false)]
    #[case(Some(Permissions::empty()), false)]
    #[case(Some(Permissions::VIEW_CHANNEL), true)]
    #[case(
        Some(Permissions::VIEW_CHANNEL | Permissions::SEND_MESSAGES),
        true
    )]
    fn actor_visibility_fails_closed_without_view_channel(
        #[case] permissions: Option<Permissions>,
        #[case] expected: bool,
    ) {
        assert_eq!(actor_can_view_channel(permissions), expected);
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

    fn expense_claim_session(nonce: u64) -> ExpenseSession {
        ExpenseSession::new(
            ExpenseSessionKey::new(
                ExpenseDraftScopeId::new(10).expect("draft scope should be non-zero"),
                MemberId(20),
            ),
            ExpenseLaunchOrigin::SlashCommand,
            ExpenseSessionStage::AwaitingBasicInfo,
            ExpenseDraftSnapshot::empty(),
            InteractionNonce::new(nonce).expect("nonce should be non-zero"),
            UNIX_EPOCH,
        )
        .expect("expense session should be valid")
    }

    fn inspect_expense_claim_session(
        store: &ExpenseSessionStore,
        session: &ExpenseSession,
    ) -> Result<Option<ExpenseSession>, SessionAccessError> {
        let claimed = store.claim(session.key(), session.nonce(), UNIX_EPOCH)?;
        Ok(claimed.map(|claimed| {
            let session = claimed.session().clone();
            store.restore_claim(claimed);
            session
        }))
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
    fn expense_session_claim_restores_session_when_transition_does_not_complete() {
        let store = ExpenseSessionStore::new();
        let session = expense_claim_session(30);
        store.replace(session.clone());
        let claimed = store
            .claim(session.key(), session.nonce(), UNIX_EPOCH)
            .expect("session access should succeed")
            .expect("session should exist");

        drop(ExpenseSessionClaim {
            store: &store,
            original: Some(claimed),
        });

        assert_eq!(
            inspect_expense_claim_session(&store, &session),
            Ok(Some(session))
        );
    }

    #[test]
    fn expense_session_claim_discard_keeps_completed_session_absent() {
        let store = ExpenseSessionStore::new();
        let session = expense_claim_session(30);
        store.replace(session.clone());
        let claimed = store
            .claim(session.key(), session.nonce(), UNIX_EPOCH)
            .expect("session access should succeed")
            .expect("session should exist");
        let mut claim = ExpenseSessionClaim {
            store: &store,
            original: Some(claimed),
        };

        claim.discard();
        drop(claim);

        assert_eq!(inspect_expense_claim_session(&store, &session), Ok(None));
    }

    #[test]
    fn expense_session_claim_replace_persists_completed_transition() {
        let store = ExpenseSessionStore::new();
        let session = expense_claim_session(30);
        let replacement = expense_claim_session(31);
        store.replace(session.clone());
        let claimed = store
            .claim(session.key(), session.nonce(), UNIX_EPOCH)
            .expect("session access should succeed")
            .expect("session should exist");
        let mut claim = ExpenseSessionClaim {
            store: &store,
            original: Some(claimed),
        };

        claim.replace(replacement.clone());
        drop(claim);

        assert_eq!(
            inspect_expense_claim_session(&store, &replacement),
            Ok(Some(replacement))
        );
    }

    #[rstest]
    #[case::expense(LEDGER_PANEL_EXPENSE_ID, Some(PanelLauncher::Expense))]
    #[case::review(LEDGER_PANEL_REVIEW_ID, Some(PanelLauncher::Review))]
    #[case::ledger(LEDGER_PANEL_LEDGER_ID, Some(PanelLauncher::Ledger))]
    #[case::void(LEDGER_PANEL_VOID_ID, Some(PanelLauncher::Void))]
    #[case::unrelated("ledger:other", None)]
    fn panel_launcher_recognizes_owned_component_ids(
        #[case] custom_id: &str,
        #[case] expected: Option<PanelLauncher>,
    ) {
        assert_eq!(panel_launcher(custom_id), expected);
    }

    #[test]
    fn route_error_variants_can_be_pattern_matched_distinctly() {
        let cases: [LedgerRouteError; 5] = [
            LedgerRouteError::GuildOnly,
            LedgerRouteError::NotInTrackedChannel,
            LedgerRouteError::SettleThreadOnly,
            LedgerRouteError::Permission(Cow::Borrowed("denied")),
            InternalLedgerRouteError::ModalSubmissionMissingFields.into(),
        ];
        for error in &cases {
            match error {
                LedgerRouteError::GuildOnly => {}
                LedgerRouteError::NotInTrackedChannel => {}
                LedgerRouteError::SettleThreadOnly => {}
                LedgerRouteError::Permission(detail) => assert!(!detail.is_empty()),
                LedgerRouteError::Internal(_) => {}
            }
        }
    }

    #[rstest]
    #[case::guild_only(
        LedgerRouteError::GuildOnly,
        walicord_i18n::guild_only_command_message()
    )]
    #[case::untracked(
        LedgerRouteError::NotInTrackedChannel,
        walicord_i18n::CHANNEL_NOT_TRACKED
    )]
    #[case::settle_parent(
        LedgerRouteError::SettleThreadOnly,
        walicord_i18n::settlement_thread_only_message()
    )]
    fn route_error_maps_to_user_message(#[case] error: LedgerRouteError, #[case] expected: &str) {
        assert_eq!(error.user_message(), expected);
    }

    #[rstest]
    #[case(
        CanonicalLoadFailure::FetchTimeout {
            route: CanonicalLoadRoute::Read,
        },
        walicord_i18n::ledger_load_timeout_message()
    )]
    #[case(
        CanonicalLoadFailure::VersionMismatch {
            route: CanonicalLoadRoute::Read,
            failing_entry_id: Some(LedgerEntryId(7)),
        },
        walicord_i18n::unknown_ledger_format_message()
    )]
    fn canonical_load_route_error_preserves_route_aware_user_message(
        #[case] failure: CanonicalLoadFailure,
        #[case] expected: &str,
    ) {
        let error = LedgerRouteError::Internal(InternalLedgerRouteError::CanonicalLoad {
            failure,
            error: Arc::new(StoreLoadError::Fetch("simulated".to_owned())),
        });

        assert_eq!(error.user_message(), expected);
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
            observed_stage: ExpenseSessionStage::InConfirmation,
        };

        let rendered = error.to_string();

        assert!(rendered.contains("navigation"));
        assert!(rendered.contains("InConfirmation"));
    }

    #[rstest]
    #[case::no_preview(
        SettleAttemptError::NoPreviewStored,
        i18n::review_preview_required_message()
    )]
    #[case::stale_head(
        SettleAttemptError::StaleHead {
            stored_head: ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(1)),
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
            stored_head: ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(1)),
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

    #[test]
    fn ledger_refresh_recovery_reference_links_the_canonical_thread() {
        let binding = CanonicalThreadBinding::new(
            TrackedParentKey::from_guarded_parent(GuildId::new(10), ChannelId::new(20)),
            ChannelId::new(30),
        );

        assert_eq!(
            ledger_refresh_recovery_reference(binding),
            LocatorRecoveryReference::ledger(
                "0000001e",
                Some("https://discord.com/channels/10/30")
            )
        );
    }

    #[rstest]
    #[case::view_channel(
        Permissions::all() - Permissions::VIEW_CHANNEL,
        PermissionAction::ViewChannel
    )]
    #[case::read_history(
        Permissions::all() - Permissions::READ_MESSAGE_HISTORY,
        PermissionAction::ReadMessageHistory
    )]
    #[case::send_in_thread(
        Permissions::all() - Permissions::SEND_MESSAGES_IN_THREADS,
        PermissionAction::SendMessageInChannel
    )]
    #[case::attach_files(
        Permissions::all() - Permissions::ATTACH_FILES,
        PermissionAction::AttachFiles
    )]
    #[case::create_thread(
        Permissions::all() - Permissions::CREATE_PUBLIC_THREADS,
        PermissionAction::CreatePublicThread
    )]
    #[case::manage_threads(
        Permissions::all() - Permissions::MANAGE_THREADS,
        PermissionAction::ManageThreads
    )]
    fn first_missing_permission_action_matches_runtime_permission(
        #[case] current: Permissions,
        #[case] expected: PermissionAction,
    ) {
        assert_eq!(first_missing_permission_action(current), expected);
    }

    #[rstest]
    #[case::slash(
        ExpenseLaunchOrigin::SlashCommand,
        DiscordLedgerSourceDescriptor::expense_slash_modal_v1()
    )]
    #[case::panel(
        ExpenseLaunchOrigin::PanelButton,
        DiscordLedgerSourceDescriptor::expense_panel_modal_v1()
    )]
    fn expense_source_descriptor_preserves_launch_origin(
        #[case] origin: ExpenseLaunchOrigin,
        #[case] expected: DiscordLedgerSourceDescriptor,
    ) {
        assert_eq!(expense_source_descriptor(origin), expected);
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

    #[rstest]
    #[case::empty(vec![], Ok(vec![]))]
    #[case::valid(vec!["7".to_owned(), "42".to_owned()], Ok(vec![7, 42]))]
    #[case::malformed(
        vec!["7".to_owned(), "not-a-member".to_owned()],
        Err(ComponentSelectionParseError::InvalidValue("not-a-member".to_owned()))
    )]
    fn component_selection_parser_is_fail_closed(
        #[case] values: Vec<String>,
        #[case] expected: Result<Vec<u64>, ComponentSelectionParseError>,
    ) {
        assert_eq!(parse_component_selection_values(&values), expected);
    }

    #[test]
    fn component_selection_parser_rejects_non_select_components() {
        assert_eq!(
            component_selection_values(&ComponentInteractionDataKind::Button),
            Err(ComponentSelectionParseError::NotStringSelect)
        );
    }

    fn picker_roster(member_count: u64) -> RouterRosterSnapshot {
        let all_members = (1..=member_count).map(MemberId).collect::<BTreeSet<_>>();
        let display_names = (1..=member_count)
            .map(|id| {
                (
                    MemberId(id),
                    smol_str::SmolStr::new(format!("member {id:02}")),
                )
            })
            .collect::<HashMap<_, _>>();
        RouterRosterSnapshot {
            roster: RosterSnapshot {
                all_members,
                role_members: BTreeMap::new(),
            },
            display_names,
            role_display_names: HashMap::new(),
        }
    }

    #[test]
    fn expense_picker_page_reaches_members_after_the_first_discord_page() {
        let mut selection = ExpenseSelectionState::default();
        selection.picker_states.insert(
            ExpensePickerKind::Individuals,
            walicord_application::ledger::expense_session::PagedPickerState::new(
                walicord_application::ledger::expense_session::PickerSnapshotId::new(21),
                1,
                None,
                Vec::new(),
            ),
        );

        let actual = expense_picker_page(
            &picker_roster(30),
            &selection,
            ExpensePickerKind::Individuals,
        );

        assert_eq!(actual.current_page, 1);
        assert_eq!(actual.total_pages, 2);
        assert_eq!(actual.visible_values, vec![26, 27, 28, 29, 30]);
    }

    #[test]
    fn expense_picker_page_filters_by_search_query() {
        let mut selection = ExpenseSelectionState::default();
        selection.picker_states.insert(
            ExpensePickerKind::Individuals,
            walicord_application::ledger::expense_session::PagedPickerState::new(
                walicord_application::ledger::expense_session::PickerSnapshotId::new(22),
                0,
                Some("03".to_owned()),
                Vec::new(),
            ),
        );

        let actual = expense_picker_page(
            &picker_roster(30),
            &selection,
            ExpensePickerKind::Individuals,
        );

        assert_eq!(actual.visible_values, vec![3]);
        assert_eq!(actual.total_items, 1);
    }

    #[test]
    fn role_picker_uses_unknown_role_label_when_role_names_are_not_available() {
        let roster = RouterRosterSnapshot {
            roster: RosterSnapshot {
                all_members: BTreeSet::new(),
                role_members: BTreeMap::from([(RoleId(10), BTreeSet::from([MemberId(1)]))]),
            },
            display_names: HashMap::new(),
            role_display_names: HashMap::new(),
        };

        let actual = expense_picker_page(
            &roster,
            &ExpenseSelectionState::default(),
            ExpensePickerKind::Roles,
        );

        assert_eq!(
            actual.options[0].label.as_str(),
            i18n::unknown_role_label(10).to_string()
        );
    }

    #[test]
    fn role_picker_uses_cached_role_display_name_when_available() {
        let roster = RouterRosterSnapshot {
            roster: RosterSnapshot {
                all_members: BTreeSet::new(),
                role_members: BTreeMap::from([(RoleId(10), BTreeSet::from([MemberId(1)]))]),
            },
            display_names: HashMap::new(),
            role_display_names: HashMap::from([(RoleId(10), smol_str::SmolStr::new("開発"))]),
        };

        let actual = expense_picker_page(
            &roster,
            &ExpenseSelectionState::default(),
            ExpensePickerKind::Roles,
        );

        assert_eq!(actual.options[0].label.as_str(), "開発");
    }

    #[test]
    fn member_picker_sanitizes_roster_sourced_select_labels() {
        let roster = RouterRosterSnapshot {
            roster: RosterSnapshot {
                all_members: BTreeSet::from([MemberId(1)]),
                role_members: BTreeMap::new(),
            },
            display_names: HashMap::from([(
                MemberId(1),
                smol_str::SmolStr::new(" \nA=@everyone <@123> `x`\u{202E} "),
            )]),
            role_display_names: HashMap::new(),
        };

        let actual = expense_picker_page(
            &roster,
            &ExpenseSelectionState::default(),
            ExpensePickerKind::Individuals,
        );

        assert_eq!(
            actual.options[0].label.as_str(),
            "A＝＠everyone ＜＠123＞ \\`x\\`"
        );
    }

    #[test]
    fn role_picker_sanitizes_roster_sourced_select_labels() {
        let roster = RouterRosterSnapshot {
            roster: RosterSnapshot {
                all_members: BTreeSet::new(),
                role_members: BTreeMap::from([(RoleId(10), BTreeSet::from([MemberId(1)]))]),
            },
            display_names: HashMap::new(),
            role_display_names: HashMap::from([(
                RoleId(10),
                smol_str::SmolStr::new(" \nRole=@everyone <@123> `x`\u{202E} "),
            )]),
        };

        let actual = expense_picker_page(
            &roster,
            &ExpenseSelectionState::default(),
            ExpensePickerKind::Roles,
        );

        assert_eq!(
            actual.options[0].label.as_str(),
            "Role＝＠everyone ＜＠123＞ \\`x\\`"
        );
    }

    #[test]
    fn picker_navigation_custom_id_round_trips_snapshot_id() {
        let custom_id = expense_picker_custom_id(
            EXPENSE_PICKER_NEXT_CUSTOM_ID_PREFIX,
            ExpensePickerKind::Individuals,
            nonce(7),
            PickerSnapshotId::new(42),
        );

        let actual =
            parse_expense_picker_custom_id(&custom_id, EXPENSE_PICKER_NEXT_CUSTOM_ID_PREFIX);

        assert_eq!(
            actual,
            Some((
                ExpensePickerKind::Individuals,
                nonce(7),
                PickerSnapshotId::new(42),
            ))
        );
    }

    #[test]
    fn picker_selection_custom_id_round_trips_snapshot_id() {
        let custom_id = expense_picker_selection_custom_id(
            EXPENSE_INDIVIDUAL_PICK_CUSTOM_ID_PREFIX,
            nonce(7),
            PickerSnapshotId::new(42),
        );

        let actual = parse_expense_picker_selection_custom_id(
            &custom_id,
            EXPENSE_INDIVIDUAL_PICK_CUSTOM_ID_PREFIX,
        );

        assert_eq!(actual, Some((nonce(7), PickerSnapshotId::new(42))));
    }

    #[rstest]
    #[case::matching_member("03", true)]
    #[case::missing_member("missing", false)]
    fn expense_picker_query_match_reports_whether_session_should_mutate(
        #[case] query: &str,
        #[case] expected: bool,
    ) {
        let actual = expense_picker_query_matches(
            &picker_roster(30),
            &ExpenseSelectionState::default(),
            ExpensePickerKind::Individuals,
            query,
        );

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::thread(
        ReadViewRoute::ReviewThread,
        vec![
            i18n::settlement_preview_replaced_message().to_owned(),
            i18n::route_task_guidance().to_owned(),
        ]
    )]
    #[case::parent(
        ReadViewRoute::ReviewParent,
        vec![
            i18n::settlement_preview_replaced_message().to_owned(),
            i18n::route_task_guidance().to_owned(),
            i18n::parent_preview_entry_guidance().to_owned(),
        ]
    )]
    fn replacement_review_guidance_preserves_route_guidance(
        #[case] route: ReadViewRoute,
        #[case] expected: Vec<String>,
    ) {
        assert_eq!(
            review_route_guidance_lines_with_replacement_notice(route),
            expected
        );
    }

    #[test]
    fn paged_selection_merge_replaces_only_the_visible_page() {
        assert_eq!(
            merge_paged_selection(&[1, 27], &[26, 27, 28], &[26]),
            vec![1, 26]
        );
    }
}
