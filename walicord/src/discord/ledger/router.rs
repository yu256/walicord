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

use crate::channel::ChannelManager;

use super::{
    expense_flow::bootstrap_expense_session,
    expense_modal::{ExpenseModalValidationError, validate_expense_modal_submission},
    expense_modal_open::{
        ExpenseModalBuildError, ExpenseModalCustomIdMatch, ExpenseModalPrefill,
        build_expense_modal_response, extract_raw_expense_modal_submission,
        parse_expense_modal_custom_id,
    },
    observability::LedgerObservability,
    preview_store::PreviewStore,
    response_writer::suppressed_allowed_mentions,
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
            _ => Ok(InteractionDispatch::Ignored),
        }
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

    /// Component (button / select-menu) dispatch. Subsequent slices wire fixed
    /// panel-launcher buttons (`ledger:panel:*`) and session-scoped buttons keyed by
    /// `custom_id` prefix here.
    pub async fn handle_component(
        &self,
        _ctx: &Context,
        _component: &ComponentInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        Ok(InteractionDispatch::Ignored)
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
