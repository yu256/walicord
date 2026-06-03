#[cfg(test)]
use crate::discord::ledger::panel::{
    LEDGER_PANEL_EXPENSE_ID, LEDGER_PANEL_LEDGER_ID, LEDGER_PANEL_REVIEW_ID, LEDGER_PANEL_VOID_ID,
};
use serenity::{
    all::{
        ChannelId, CommandInteraction, ComponentInteraction, ComponentInteractionDataKind,
        CreateActionRow, CreateButton, CreateInteractionResponse, GuildId, ModalInteraction,
        Permissions, UserId,
    },
    async_trait,
    prelude::Context,
};
use std::{borrow::Cow, collections::HashMap, fmt::Write as _, sync::Arc, time::Duration};
use walicord_application::{
    Clock, NonceProvider, SettlementPlanner,
    ledger::{
        DiscordLedgerSourceDescriptor, ExpenseAuthoringError, LedgerEntry, LedgerId,
        UnverifiedLedgerStoreEnvelope,
        expense_session::{
            ClaimedExpenseSession, ExpenseConfirmationSnapshot, ExpenseDraftScopeId,
            ExpenseDraftSnapshot, ExpenseLaunchOrigin, ExpenseModalIntent,
            ExpenseModalSubmissionBinding, ExpenseModalSubmissionBindingStore,
            ExpenseParticipantSelection, ExpensePickerKind, ExpenseSelectionPhase, ExpenseSession,
            ExpenseSessionConstructionError, ExpenseSessionKey, ExpenseSessionStage,
            ExpenseSessionStore, ModalRetryBinding, ModalRetryBindingStore, ModalRetryPreserved,
            PickerSnapshotId, SessionAccessError, VoidSessionKey, VoidSessionStore,
        },
        observability::LedgerObservabilityEvent,
        participant_resolution::{
            ParticipantDrift, RosterSnapshot, resolve_selection_against_roster,
        },
    },
};
use walicord_domain::model::{MemberId, RoleId};
use walicord_i18n as i18n;
#[cfg(test)]
use walicord_presentation::discord_ledger::ReadViewRoute;
use walicord_presentation::discord_ledger::{
    DiscordLedgerPresenter, ExpenseConfirmationButtonIds, ExpenseSelectionStepButtonIds,
    ReadViewBuildError, ReadViewPageModel, RenderBudgetError, RenderedCanonicalMessage,
    SurfaceActionRow, SurfaceSelectMenu, VoidRetargetReason, build_expense_confirmation_surface,
    build_expense_selection_step_surface,
};

use crate::channel::ChannelManager;

mod canonical_message;
mod expense_picker;
mod ledger;
mod panel;
mod review;
use panel::{PanelLauncher, panel_launcher};
#[cfg(test)]
use review::review_route_guidance_lines_with_replacement_notice;
mod settle;
mod void;
#[cfg(test)]
use self::void::void_confirmation_total_amount;
use canonical_message::{render_public_expense_message, short_summary_for_entry};
#[cfg(test)]
use expense_picker::expense_picker_custom_id;
use expense_picker::{
    EXPENSE_INDIVIDUAL_PICK_CUSTOM_ID_PREFIX, EXPENSE_PAYER_PICK_CUSTOM_ID_PREFIX,
    EXPENSE_PICKER_CLEAR_CUSTOM_ID_PREFIX, EXPENSE_PICKER_NEXT_CUSTOM_ID_PREFIX,
    EXPENSE_PICKER_PREV_CUSTOM_ID_PREFIX, EXPENSE_PICKER_SEARCH_CUSTOM_ID_PREFIX,
    EXPENSE_PICKER_SEARCH_MODAL_CUSTOM_ID_PREFIX, EXPENSE_ROLE_PICK_CUSTOM_ID_PREFIX,
    ExpensePickerPage, build_expense_picker_search_modal_response, expense_picker_page,
    expense_picker_query_matches, expense_picker_search_not_found_message,
    expense_picker_selection_custom_id, expense_picker_utility_row,
    extract_expense_picker_search_query, merge_paged_selection, parse_expense_picker_custom_id,
    parse_expense_picker_selection_custom_id, picker_kind_for_phase,
};
#[cfg(test)]
use settle::{settle_attempt_error_message, should_clear_preview_after_settle_error};

use super::{
    adapters::DiscordCanonicalThreadLocator,
    expense_modal_open::{
        ExpenseModalBuildError, ExpenseModalCustomIdMatch, ExpenseModalPrefill,
        build_expense_modal_response, build_expense_weight_modal_response,
        extract_raw_expense_modal_submission, parse_expense_modal_custom_id,
        parse_expense_weight_modal_custom_id, parse_expense_weight_modal_submission,
    },
    locator::{
        CanonicalThreadBinding, CanonicalThreadLocatorState, LocatorError, LocatorRecoveryReference,
    },
    observability::{
        DiscordLedgerObservability, DiscordLedgerObservabilityEvent, PermissionAction,
    },
    panel::render_panel_post_message_for_locator_state,
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
        ExpenseWriteOrchestrationError, RecordTimeOutcome, RecordableExpenseEntry,
        build_canonical_envelope, compose_expense_entry,
    },
    preview_store::{PreviewStore, PreviewStoreError, PreviewStoreKey},
    projection::NextLedgerEntryIdError,
    read_view_session::{ReadViewSessionKey, ReadViewSessionStore},
    settle_flow::{PreviewAttemptError, SettleAttemptError},
    void_flow::{
        VoidCandidateEnumerationError, VoidComposeError, VoidConfirmTransitionError,
        VoidSessionBootstrapError,
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

pub(super) trait LedgerInteractionActor {
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
pub(super) enum ReadViewNavigation {
    Previous,
    Next,
}

#[derive(Debug, Clone, Copy)]
pub(super) enum DeferredEphemeralInteraction<'a> {
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

/// Closed outcome of [`LedgerRouter::commit_canonical_authoritative`] so the calling
/// route handler can branch on the only two terminal states without inspecting
/// `StoreWriteError` directly. `Recorded` means `append_authoritative` succeeded and the
/// retain has been cleared; `UncertainAppendFailed` means the retain is still `Live` and
/// the caller should render the criterion-217 / 279 block.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CommitOutcome {
    Recorded,
    UncertainAppendFailed,
}

impl LedgerRouter {
    pub fn new(deps: LedgerRouterDependencies) -> Self {
        Self { deps }
    }

    pub(super) async fn clear_resolved_uncertain_write(
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

    pub(super) async fn load_verified_thread(
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

    pub(super) async fn guard_scope(
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

    pub(super) async fn guard_scope_without_runtime_permissions(
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

    pub(super) async fn resolve_existing_ledger(
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

    pub(super) async fn resolve_readable_ledger(
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
        entry: RecordableExpenseEntry,
        display_names: &HashMap<MemberId, smol_str::SmolStr>,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        let ledger_id = binding.ledger_id();
        let envelope: UnverifiedLedgerStoreEnvelope<()> =
            build_canonical_envelope(ledger_id, previous_hash, entry.entry().clone())?;

        let rendered_message = render_public_expense_message(&entry, ledger_id, display_names)?;
        match self
            .commit_canonical_authoritative(
                ctx,
                write_target,
                binding,
                entry.entry(),
                &envelope,
                &rendered_message,
            )
            .await?
        {
            CommitOutcome::Recorded => {
                claim.discard();
                self.respond_record_success(ctx, component).await
            }
            CommitOutcome::UncertainAppendFailed => {
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

    /// Shared write-path orchestration: freeze the retain payload via `set_live`, hand
    /// the envelope + rendered body to `DiscordCanonicalLedgerStore::append_authoritative`,
    /// then on success refresh the locator and clear the retain. On Discord-side append
    /// failure the retain stays `Live`; the caller renders the criterion-217 / 279
    /// uncertain-write block. Owning this sequence in one place is the AC18 contract
    /// for `WriteCoordinator` + `UncertainWriteRegistry`: settle / void / expense all
    /// commit through the same critical section.
    pub(super) async fn commit_canonical_authoritative(
        &self,
        ctx: &Context,
        write_target: WriteTargetKey,
        binding: CanonicalThreadBinding,
        entry: &LedgerEntry,
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
        rendered: &RenderedCanonicalMessage,
    ) -> Result<CommitOutcome, LedgerRouteError> {
        let ledger_id = binding.ledger_id();
        let canonical_thread_id = binding.canonical_thread_id();
        let envelope_bytes =
            walicord_application::ledger::canonical_attachment::CanonicalAttachmentCodec::encode_with_pre_self_link_content(
                envelope,
                Some(rendered.body()),
            )
            .map_err(|error| {
                LedgerRouteError::Internal(InternalLedgerRouteError::ThreadWrite(
                    StoreWriteError::Prepare(error),
                ))
            })?;

        let retain_live_since = self.deps.clock.now();
        let retained = RetainedCanonicalWrite::new(
            write_target,
            envelope,
            envelope_bytes,
            rendered.body().to_owned(),
            short_summary_for_entry(entry),
            retain_live_since,
        );
        self.deps.uncertain_writes.set_live(retained).map_err(|_| {
            LedgerRouteError::Internal(InternalLedgerRouteError::UncertainWriteAlreadyLive {
                ledger_id,
            })
        })?;

        match self
            .deps
            .canonical_store
            .append_authoritative(ctx, canonical_thread_id, envelope, rendered)
            .await
        {
            Ok(_verified) => {
                self.deps.locator.replace_with_ready_binding(binding);
                self.deps.uncertain_writes.clear(write_target);
                Ok(CommitOutcome::Recorded)
            }
            Err(error) => {
                // Retain stays Live: a transport error here is exactly the
                // criterion-217 / 279 case where lazy retry must decide whether the
                // canonical message actually posted. Classify the underlying
                // StoreWriteError into the closed AppendFailureReason taxonomy and
                // emit so the failure is observable in production logs (criterion
                // 248 / AC28).
                let reason = error.append_failure_reason();
                tracing::error!(
                    ledger_id = ?ledger_id,
                    write_target = ?write_target,
                    reason = reason.label(),
                    error = %error,
                    "canonical append failed; uncertain_write remains Live for lazy retry",
                );
                self.deps.observability.emit(
                    walicord_application::ledger::observability::LedgerObservabilityEvent::CanonicalAppendFailed {
                        ledger_id,
                        reason,
                        retained_live_since: retain_live_since,
                    },
                );
                Ok(CommitOutcome::UncertainAppendFailed)
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
        // `picker_kind_for_phase` already narrowed the phase set: every phase that
        // reaches this point has a menu kind, so matching on `kind` makes the match
        // exhaustive without an `unreachable!` arm.
        let (custom_id, placeholder, min_values, max_values) = match kind {
            ExpensePickerKind::Payer => (
                expense_picker_selection_custom_id(
                    EXPENSE_PAYER_PICK_CUSTOM_ID_PREFIX,
                    nonce,
                    page.snapshot_id,
                ),
                i18n::expense_payer_placeholder(),
                1,
                1,
            ),
            ExpensePickerKind::Individuals => (
                expense_picker_selection_custom_id(
                    EXPENSE_INDIVIDUAL_PICK_CUSTOM_ID_PREFIX,
                    nonce,
                    page.snapshot_id,
                ),
                i18n::participant_source_individual_placeholder(),
                0,
                page.page_item_count as u8,
            ),
            ExpensePickerKind::Roles => (
                expense_picker_selection_custom_id(
                    EXPENSE_ROLE_PICK_CUSTOM_ID_PREFIX,
                    nonce,
                    page.snapshot_id,
                ),
                i18n::participant_source_role_placeholder(),
                0,
                page.page_item_count as u8,
            ),
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

    pub(super) fn uncertain_write_block_response(
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

    pub(super) async fn update_component_message(
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

    pub(super) async fn edit_component_response_with_components(
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
}

/// Discord-call failures need both a static `DiscordCallSite` (known at the call
/// site) and the runtime `serenity::Error`. `From` is inappropriate — a tuple of two
/// unrelated types isn't a semantic unit — so we expose a small curry helper instead:
/// `.map_err(discord_call_error(SITE))` reads as "treat any serenity error from this
/// call as a DiscordCall failure tagged with SITE".
pub(super) fn discord_call_error(
    site: DiscordCallSite,
) -> impl FnOnce(serenity::Error) -> LedgerRouteError {
    move |error| LedgerRouteError::Internal(InternalLedgerRouteError::DiscordCall { site, error })
}

pub(crate) const EXPENSE_CANCEL_CUSTOM_ID_PREFIX: &str = "ledger:expense:cancel:";
pub(crate) const EXPENSE_MODAL_RETRY_CUSTOM_ID_PREFIX: &str = "ledger:expense:retry:";
pub(crate) const EXPENSE_BACK_CUSTOM_ID_PREFIX: &str = "ledger:expense:back:";
pub(crate) const EXPENSE_BASIC_EDIT_CUSTOM_ID_PREFIX: &str = "ledger:expense:basic-edit:";
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

pub(super) fn read_view_navigation_row(
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

pub(super) fn expense_source_descriptor(
    origin: ExpenseLaunchOrigin,
) -> DiscordLedgerSourceDescriptor {
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

// canonical_message submodule owns the post-record public-message composers; the
// helpers below (short_summary_for_entry, render_public_*_message) live there now.

pub(super) fn uncertain_write_block_message(
    preserve_input: bool,
    preserve_preview: bool,
) -> String {
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
pub(super) enum VoidSelectionRenderKind {
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
            AllocationSnapshot, ExpenseRecorded, LedgerEntryId, MemberAmount,
            NormalizedSettlementPlanRecorded, expense_session::ExpenseSelectionState,
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
        use crate::discord::ledger::locator::TrackedParentKey;
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
