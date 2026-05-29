use serenity::{
    all::{CommandInteraction, ComponentInteraction, ModalInteraction},
    prelude::Context,
};
use std::sync::Arc;
use walicord_application::{Clock, NonceProvider, SettlementPlanner};

use super::{
    observability::LedgerObservability,
    preview_store::PreviewStore,
    sessions::{ExpenseSessionStore, ModalRetryBindingStore, VoidSessionStore},
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

    /// Slash-command dispatch. Subsequent Step 11 slices wire each supported command
    /// (`/expense`, `/review`, `/settle`, `/ledger`, `/void`, `/panel`,
    /// `/ledger-refresh`) here. Returning `Ignored` means the command is not a ledger
    /// command and `handler.rs` should fall through to the legacy code path.
    pub async fn handle_command(
        &self,
        _ctx: &Context,
        _command: &CommandInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        Ok(InteractionDispatch::Ignored)
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

    /// Modal submission dispatch. Subsequent slices wire the expense / weight-editor /
    /// retry modals here.
    pub async fn handle_modal(
        &self,
        _ctx: &Context,
        _modal: &ModalInteraction,
    ) -> Result<InteractionDispatch, LedgerRouteError> {
        Ok(InteractionDispatch::Ignored)
    }
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
