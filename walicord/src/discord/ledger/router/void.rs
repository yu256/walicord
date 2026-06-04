//! Void-specific presentation helpers extracted from `router::mod`. Each function is a
//! free helper that the dispatch methods on `LedgerRouter` call to compose the void
//! selection menu, confirmation surface, recovery references, and select-menu labels.
//! Keeping them here keeps the void presentation glue out of the dispatcher and lets
//! the dispatcher focus on session / write-coordinator orchestration.

use std::fmt::Write as _;

use serenity::{
    all::{
        ChannelId, CommandInteraction, ComponentInteraction, ComponentInteractionDataKind, GuildId,
    },
    prelude::Context,
};
use walicord_application::ledger::{
    DiscordLedgerSourceDescriptor, LedgerEntry, LedgerEntryId, LedgerEvent, LedgerId,
    expense_session::{SessionAccessError, VoidSession, VoidSessionKey, VoidSessionStage},
    projection::VerifiedLedgerEntryView,
    void_execute::{VoidExecuteCommand, VoidExecuteOutcome, void_execute_v1},
    void_flow::{
        VoidComposeError, VoidConfirmTransitionError, VoidSessionBootstrapError,
        bootstrap_void_session, enumerate_void_candidates, transition_to_confirm,
    },
    write_coordinator::WriteTargetKey,
};
use walicord_domain::{Money, model::MemberId};
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    DiscordLedgerPresenter, RecoveryCta, RecoveryReference, SafeLiteralText, SurfaceActionRow,
    SurfaceButton, SurfaceInteractiveButtonStyle, SurfaceMemberLabels, SurfaceSelectMenu,
    SurfaceSelectOption, VoidCandidateRow, VoidConfirmationRecap, VoidRetargetReason,
    VoidSurfaceModel, summary_for_view,
};

use super::{
    CanonicalLoadRoute, DeferredEphemeralInteraction, DiscordCallSite, InteractionDispatch,
    InternalLedgerRouteError, LedgerRouteError, LedgerRouter, VOID_CANCEL_CUSTOM_ID_PREFIX,
    VOID_CONFIRM_CUSTOM_ID_PREFIX, VOID_PICK_CUSTOM_ID_PREFIX, VOID_RESELECT_CUSTOM_ID_PREFIX,
    VoidSelectionRenderKind, canonical_message::DiscordVoidEntryRenderer, discord_call_error,
};
use crate::discord::ledger::{
    locator::{CanonicalThreadBinding, RequestBoundLocatorPublisher, TrackedParentKey},
    response_writer::{rendered_surface_to_message, safe_edit_interaction_response},
    store::{RequestBoundCanonicalAppender, RequestBoundCanonicalReader},
};

pub(super) fn selected_void_target(component: &ComponentInteraction) -> Option<LedgerEntryId> {
    match &component.data.kind {
        ComponentInteractionDataKind::StringSelect { values } => values
            .first()
            .and_then(|value| value.parse::<u64>().ok())
            .map(LedgerEntryId),
        _ => None,
    }
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
pub(super) fn void_candidate_rows(
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

pub(super) fn void_recovery_reference(
    view: &VerifiedLedgerEntryView,
    ledger_id: LedgerId,
) -> RecoveryReference {
    RecoveryReference {
        ledger_id_short: format!("{ledger_id:08x}"),
        entry_id: view.entry().id,
        message_link: Some(view.message_link().to_owned()),
    }
}

pub(super) fn void_selection_action_rows(
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

pub(super) fn void_confirmation_action_rows(
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
pub(super) fn void_confirmation_model(
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
pub(super) fn void_confirmation_total_amount(
    entry: &LedgerEntry,
) -> Result<String, LedgerRouteError> {
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

impl LedgerRouter {
    pub(super) async fn dispatch_void_command(
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

    pub(super) async fn dispatch_void(
        &self,
        ctx: &Context,
        scope: crate::discord::ledger::route_guard::LedgerInteractionScope,
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

    pub(super) async fn dispatch_void_pick(
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

    pub(super) async fn dispatch_void_reselect(
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

    pub(super) async fn dispatch_void_cancel(
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

    pub(super) async fn dispatch_void_confirm(
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

        let appender = RequestBoundCanonicalAppender {
            ctx,
            store: self.deps.canonical_store.as_ref(),
            canonical_thread_id: binding.canonical_thread_id(),
        };
        let publisher = RequestBoundLocatorPublisher {
            locator: self.deps.locator.as_ref(),
            binding,
        };
        let reader = RequestBoundCanonicalReader {
            ctx,
            store: self.deps.canonical_store.as_ref(),
            canonical_thread_id: binding.canonical_thread_id(),
            ledger_id,
            load_route_label: CanonicalLoadRoute::WritePrelude.label(),
        };
        let renderer = DiscordVoidEntryRenderer { labels: &labels };
        let outcome = void_execute_v1(
            &appender,
            &publisher,
            &reader,
            &renderer,
            self.deps.void_sessions.as_ref(),
            self.deps.uncertain_writes.as_ref(),
            self.deps.write_coordinator.as_ref(),
            self.deps.observability.as_ref(),
            self.deps.clock.as_ref(),
            VoidExecuteCommand {
                session: &session,
                session_key: key,
                ledger_id,
                actor_id,
                write_target: WriteTargetKey::Published(ledger_id),
                source_descriptor: DiscordLedgerSourceDescriptor::void_parent_v1(),
            },
        )
        .await
        .map_err(LedgerRouteError::from)?;

        match outcome {
            VoidExecuteOutcome::Recorded { .. } => {
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
            VoidExecuteOutcome::TargetGone => {
                self.edit_component_response(
                    ctx,
                    component,
                    i18n::void_target_updated_message(),
                    DiscordCallSite::VoidEditResponse,
                )
                .await
            }
            VoidExecuteOutcome::UncertainBlocked | VoidExecuteOutcome::UncertainAppendFailed => {
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

    pub(super) async fn update_component_with_void_model(
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
