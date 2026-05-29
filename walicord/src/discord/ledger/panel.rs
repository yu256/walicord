use dashmap::{DashMap, DashSet};
use serenity::all::{ChannelId, CreateActionRow, GuildId, MessageId};
use walicord_i18n as i18n;
use walicord_presentation::{
    DiscordLedgerPresenter,
    discord_ledger::{PanelButtonStates, PanelSurfaceModel},
};

use super::{
    locator::{CanonicalThreadLocatorState, LocatorRecoveryReference},
    permissions::{
        RecoveryOutcomeMessage, recovery_reference_components,
        render_parent_channel_access_failure_message,
    },
    response_writer::rendered_surface_to_message,
    route_guard::outside_tracked_channel_message,
};

pub(crate) const LEDGER_PANEL_EXPENSE_ID: &str = "ledger:panel:expense";
pub(crate) const LEDGER_PANEL_REVIEW_ID: &str = "ledger:panel:review";
pub(crate) const LEDGER_PANEL_LEDGER_ID: &str = "ledger:panel:ledger";
pub(crate) const LEDGER_PANEL_VOID_ID: &str = "ledger:panel:void";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct PanelPostKey {
    guild_id: GuildId,
    tracked_parent_channel_id: ChannelId,
}

impl PanelPostKey {
    pub(crate) fn new(guild_id: GuildId, tracked_parent_channel_id: ChannelId) -> Self {
        Self {
            guild_id,
            tracked_parent_channel_id,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PanelPostAttempt {
    Accepted {
        previous_message_id: Option<MessageId>,
    },
    InProgress,
}

pub(crate) struct PanelPostRegistry {
    in_progress: DashSet<PanelPostKey>,
    last_panel_message_ids: DashMap<PanelPostKey, MessageId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum PanelPostFailure {
    MissingParentAccess {
        recovery_reference: LocatorRecoveryReference,
    },
    ParentNoLongerTracked {
        tracked_parent_hint: Option<ChannelId>,
    },
    DiscordRejected,
}

impl PanelPostRegistry {
    pub(crate) fn new() -> Self {
        Self {
            in_progress: DashSet::new(),
            last_panel_message_ids: DashMap::new(),
        }
    }

    pub(crate) fn begin(&self, key: PanelPostKey) -> PanelPostAttempt {
        if !self.in_progress.insert(key) {
            return PanelPostAttempt::InProgress;
        }

        PanelPostAttempt::Accepted {
            previous_message_id: self.cached_message_id(key),
        }
    }

    pub(crate) fn finish(&self, key: PanelPostKey, new_message_id: MessageId) -> Option<MessageId> {
        let previous_message_id = self.last_panel_message_ids.insert(key, new_message_id);
        self.in_progress.remove(&key);
        previous_message_id
    }

    pub(crate) fn abort(&self, key: PanelPostKey) {
        self.in_progress.remove(&key);
    }

    pub(crate) fn cached_message_id(&self, key: PanelPostKey) -> Option<MessageId> {
        self.last_panel_message_ids
            .get(&key)
            .map(|message_id| *message_id)
    }
}

pub(crate) fn is_ledger_panel_component_id(custom_id: &str) -> bool {
    matches!(
        custom_id,
        LEDGER_PANEL_EXPENSE_ID
            | LEDGER_PANEL_REVIEW_ID
            | LEDGER_PANEL_LEDGER_ID
            | LEDGER_PANEL_VOID_ID
    )
}

pub(crate) fn render_panel_post_failure_message(
    failure: &PanelPostFailure,
) -> RecoveryOutcomeMessage {
    match failure {
        PanelPostFailure::MissingParentAccess { recovery_reference } => {
            render_parent_channel_access_failure_message(recovery_reference)
        }
        PanelPostFailure::ParentNoLongerTracked {
            tracked_parent_hint,
        } => {
            RecoveryOutcomeMessage::from_body(outside_tracked_channel_message(*tracked_parent_hint))
        }
        PanelPostFailure::DiscordRejected => {
            RecoveryOutcomeMessage::from_body(i18n::panel_post_retry_message())
        }
    }
}

pub(crate) fn render_panel_post_message(
    canonical_thread_id: Option<ChannelId>,
    bootstrap_uncertain_write: bool,
) -> Result<(String, Vec<CreateActionRow>), RecoveryOutcomeMessage> {
    let (thread_cue, status_line) = if bootstrap_uncertain_write && canonical_thread_id.is_none() {
        (
            i18n::panel_thread_cue_pending().to_owned(),
            Some(i18n::panel_bootstrap_diagnosis().to_owned()),
        )
    } else if let Some(thread_id) = canonical_thread_id {
        (
            format!(
                "{}",
                i18n::panel_thread_cue_known(format!("<#{}>", thread_id.get()))
            ),
            None,
        )
    } else {
        (
            i18n::panel_thread_cue_pending().to_owned(),
            Some(i18n::panel_first_record_prompt().to_owned()),
        )
    };
    DiscordLedgerPresenter::render_panel(&PanelSurfaceModel {
        thread_cue,
        status_line,
        button_states: PanelButtonStates::default(),
        ephemeral: false,
    })
    .map(rendered_surface_to_message)
    .map_err(|_| RecoveryOutcomeMessage::from_body(i18n::panel_render_retry_message()))
}

pub(crate) fn render_panel_post_message_for_locator_state(
    locator_state: Option<&CanonicalThreadLocatorState>,
    bootstrap_uncertain_write: bool,
) -> Result<(String, Vec<CreateActionRow>), RecoveryOutcomeMessage> {
    match locator_state {
        None => Err(RecoveryOutcomeMessage::from_body(
            i18n::panel_cache_warmup_required_message(),
        )),
        Some(CanonicalThreadLocatorState::ReadyNoThread { .. }) => {
            render_panel_post_message(None, bootstrap_uncertain_write)
        }
        Some(CanonicalThreadLocatorState::ReadyBound(binding)) => render_panel_post_message(
            Some(binding.canonical_thread_id()),
            bootstrap_uncertain_write,
        ),
        Some(CanonicalThreadLocatorState::DuplicateBlocked {
            authoritative_candidate_known,
            recovery_references,
            ..
        }) => Err(render_duplicate_blocked_message(
            *authoritative_candidate_known,
            recovery_references,
        )),
        Some(CanonicalThreadLocatorState::DamagedBlocked {
            recovery_reference, ..
        }) => Err(render_damaged_blocked_message(recovery_reference)),
    }
}

fn render_duplicate_blocked_message(
    authoritative_candidate_known: bool,
    recovery_references: &[LocatorRecoveryReference],
) -> RecoveryOutcomeMessage {
    let mut lines = vec![if authoritative_candidate_known {
        i18n::duplicate_thread_blocked_message_authoritative().to_owned()
    } else {
        i18n::duplicate_thread_blocked_message_unresolved().to_owned()
    }];
    lines.extend(
        recovery_references
            .iter()
            .map(LocatorRecoveryReference::render_line)
            .collect::<Vec<_>>(),
    );
    lines.push(
        if authoritative_candidate_known {
            i18n::duplicate_thread_recovery_guidance_authoritative()
        } else {
            i18n::duplicate_thread_recovery_guidance_unresolved()
        }
        .to_owned(),
    );
    let components = authoritative_candidate_known
        .then(|| recovery_references.first())
        .flatten()
        .map(recovery_reference_components)
        .unwrap_or_default();
    RecoveryOutcomeMessage::new(lines.join("\n"), components)
}

fn render_damaged_blocked_message(
    recovery_reference: &LocatorRecoveryReference,
) -> RecoveryOutcomeMessage {
    RecoveryOutcomeMessage::new(
        [
            i18n::damaged_candidate_blocked_message().to_owned(),
            recovery_reference.render_line(),
            i18n::damaged_candidate_recovery_guidance().to_owned(),
        ]
        .join("\n"),
        recovery_reference_components(recovery_reference),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discord::ledger::locator::{
        CanonicalThreadBinding, CanonicalThreadLocatorState, LocatorRecoveryReference,
        TrackedParentKey,
    };
    use serenity::all::ChannelType;

    fn tracked_parent_key() -> TrackedParentKey {
        TrackedParentKey::new(
            GuildId::new(1),
            ChannelId::new(10),
            ChannelType::Text,
            Some("ops #walicord"),
        )
        .expect("tracked parent should satisfy predicate")
    }

    fn ledger_recovery_reference(label: &str) -> LocatorRecoveryReference {
        LocatorRecoveryReference::ledger(
            label,
            Some(format!("https://discord.example/messages/{label}")),
        )
    }

    fn channel_recovery_reference(channel_id: ChannelId) -> LocatorRecoveryReference {
        LocatorRecoveryReference::channel(
            channel_id,
            Some(format!(
                "https://discord.example/channels/1/{}",
                channel_id.get()
            )),
        )
    }

    #[test]
    fn panel_post_registry_rejects_concurrent_post_for_same_parent() {
        let registry = PanelPostRegistry::new();
        let key = PanelPostKey::new(GuildId::new(1), ChannelId::new(10));

        assert_eq!(
            registry.begin(key),
            PanelPostAttempt::Accepted {
                previous_message_id: None,
            }
        );
        assert_eq!(registry.begin(key), PanelPostAttempt::InProgress);

        registry.abort(key);

        assert_eq!(
            registry.begin(key),
            PanelPostAttempt::Accepted {
                previous_message_id: None,
            }
        );
    }

    #[test]
    fn panel_post_registry_updates_last_message_only_after_success() {
        let registry = PanelPostRegistry::new();
        let key = PanelPostKey::new(GuildId::new(1), ChannelId::new(10));

        assert_eq!(registry.finish(key, MessageId::new(50)), None);
        assert_eq!(registry.cached_message_id(key), Some(MessageId::new(50)));
        assert_eq!(
            registry.begin(key),
            PanelPostAttempt::Accepted {
                previous_message_id: Some(MessageId::new(50)),
            }
        );
        assert_eq!(
            registry.finish(key, MessageId::new(60)),
            Some(MessageId::new(50))
        );
        assert_eq!(registry.cached_message_id(key), Some(MessageId::new(60)));
    }

    #[test]
    fn panel_post_registry_abort_keeps_cached_message_after_failed_retry() {
        let registry = PanelPostRegistry::new();
        let key = PanelPostKey::new(GuildId::new(1), ChannelId::new(10));

        registry.finish(key, MessageId::new(50));
        assert_eq!(
            registry.begin(key),
            PanelPostAttempt::Accepted {
                previous_message_id: Some(MessageId::new(50)),
            }
        );

        registry.abort(key);

        assert_eq!(registry.cached_message_id(key), Some(MessageId::new(50)));
    }

    #[test]
    fn panel_post_requires_warm_locator_cache_before_posting() {
        let message = render_panel_post_message_for_locator_state(None, false)
            .expect_err("warm-cache miss should block panel posting");

        assert_eq!(message.body(), i18n::panel_cache_warmup_required_message());
        assert!(message.components().is_empty());
    }

    #[test]
    fn panel_post_uses_locator_binding_when_cache_is_ready() {
        let tracked_parent = tracked_parent_key();
        let ready = CanonicalThreadLocatorState::ReadyBound(CanonicalThreadBinding::new(
            tracked_parent,
            ChannelId::new(77),
        ));

        assert_eq!(
            render_panel_post_message_for_locator_state(Some(&ready), false)
                .expect("ready binding should render panel"),
            render_panel_post_message(Some(ChannelId::new(77)), false)
                .expect("known canonical thread should render panel")
        );
    }

    #[test]
    fn panel_post_refuses_duplicate_blocked_locator_state() {
        let tracked_parent = tracked_parent_key();
        let duplicate = CanonicalThreadLocatorState::DuplicateBlocked {
            tracked_parent,
            authoritative_candidate_known: true,
            recovery_references: vec![
                ledger_recovery_reference("keep"),
                ledger_recovery_reference("other"),
            ],
        };

        let message = render_panel_post_message_for_locator_state(Some(&duplicate), false)
            .expect_err("duplicate blocked state should block panel posting");

        assert_eq!(
            message.body(),
            [
                i18n::duplicate_thread_blocked_message_authoritative().to_owned(),
                ledger_recovery_reference("keep").render_line(),
                ledger_recovery_reference("other").render_line(),
                i18n::duplicate_thread_recovery_guidance_authoritative().to_owned(),
            ]
            .join("\n")
        );
        assert_eq!(
            serde_json::to_value(message.components()).expect("components should serialize")[0]["components"]
                [0]["label"],
            i18n::open_ledger_thread_label()
        );
    }

    #[test]
    fn panel_post_refuses_damaged_blocked_locator_state() {
        let tracked_parent = tracked_parent_key();
        let damaged = CanonicalThreadLocatorState::DamagedBlocked {
            tracked_parent,
            recovery_reference: channel_recovery_reference(
                tracked_parent.tracked_parent_channel_id(),
            ),
        };

        let message = render_panel_post_message_for_locator_state(Some(&damaged), false)
            .expect_err("damaged blocked state should block panel posting");

        assert_eq!(
            message.body(),
            [
                i18n::damaged_candidate_blocked_message().to_owned(),
                channel_recovery_reference(tracked_parent.tracked_parent_channel_id())
                    .render_line(),
                i18n::damaged_candidate_recovery_guidance().to_owned(),
            ]
            .join("\n")
        );
        assert_eq!(
            serde_json::to_value(message.components()).expect("components should serialize")[0]["components"]
                [0]["label"],
            i18n::open_parent_channel_label()
        );
    }

    #[test]
    fn panel_post_failure_uses_outside_tracked_message_for_untracked_parent() {
        let message = render_panel_post_failure_message(&PanelPostFailure::ParentNoLongerTracked {
            tracked_parent_hint: Some(ChannelId::new(42)),
        });

        assert_eq!(
            message.body(),
            i18n::outside_tracked_channel_message_with_hint("<#42>").to_string()
        );
        assert!(message.components().is_empty());
    }

    #[test]
    fn panel_post_failure_uses_parent_access_setup_message() {
        let message = render_panel_post_failure_message(&PanelPostFailure::MissingParentAccess {
            recovery_reference: channel_recovery_reference(ChannelId::new(10)),
        });

        assert_eq!(
            message.body(),
            [
                i18n::bot_cannot_operate_parent_channel_message().to_owned(),
                "復旧用の参照: channel:10 | <https://discord.example/channels/1/10>".to_owned(),
            ]
            .join("\n")
        );
        assert_eq!(
            serde_json::to_value(message.components()).expect("components should serialize")[0]["components"]
                [0]["label"],
            i18n::open_parent_channel_label()
        );
    }

    #[test]
    fn panel_post_failure_uses_generic_retry_only_for_other_discord_rejections() {
        let message = render_panel_post_failure_message(&PanelPostFailure::DiscordRejected);

        assert_eq!(message.body(), i18n::panel_post_retry_message());
        assert!(message.components().is_empty());
    }
}
