use crate::channel::ChannelManager;
use serenity::{
    all::{ChannelId, ChannelType, GuildId},
    prelude::Context,
};
use walicord_application::ledger::expense_session::ExpenseDraftScopeId;
use walicord_i18n as i18n;

use super::locator::TrackedParentKey;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ChannelFlagAction {
    Track,
    Untrack,
    Keep,
}

pub(crate) fn startup_channel_is_track_target(kind: ChannelType, topic: Option<&str>) -> bool {
    kind == ChannelType::Text && ChannelManager::topic_has_flag(topic)
}

pub(crate) fn startup_track_targets<'a, I>(channels: I) -> Vec<ChannelId>
where
    I: IntoIterator<Item = (ChannelId, ChannelType, Option<&'a str>)>,
{
    channels
        .into_iter()
        .filter_map(|(id, kind, topic)| startup_channel_is_track_target(kind, topic).then_some(id))
        .collect()
}

pub(crate) fn channel_flag_action(
    old_kind: Option<ChannelType>,
    old_topic: Option<&str>,
    new_kind: ChannelType,
    new_topic: Option<&str>,
) -> ChannelFlagAction {
    let old_has_flag = old_kind
        .map(|kind| startup_channel_is_track_target(kind, old_topic))
        .unwrap_or(false);
    let new_has_flag = startup_channel_is_track_target(new_kind, new_topic);
    match (old_has_flag, new_has_flag) {
        (false, true) => ChannelFlagAction::Track,
        (true, false) => ChannelFlagAction::Untrack,
        _ => ChannelFlagAction::Keep,
    }
}

pub(crate) fn outside_tracked_channel_message(
    tracked_parent_channel_id: Option<ChannelId>,
) -> String {
    tracked_parent_channel_id
        .map(|channel_id| {
            format!(
                "{}",
                i18n::outside_tracked_channel_message_with_hint(format_args!("<#{channel_id}>"))
            )
        })
        .unwrap_or_else(|| i18n::outside_tracked_channel_message_generic().to_owned())
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub(crate) enum SlashScopeError {
    #[error("thread channel {0} has no parent channel")]
    ThreadWithoutParent(ChannelId),
}

/// Per-interaction preconditions every ledger route must satisfy before any state
/// mutation (criteria 164-166 / 132 / 283 / 42).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum LedgerInteractionGuardError {
    /// Interaction has no `guild_id`. Routes return the criterion-164/165/166 message.
    GuildOnly,
    /// Channel is not currently tracked by `ChannelManager` (no `#walicord` topic flag
    /// observed on this guild). Routes return the criterion-42 / 132 wrong-channel
    /// message.
    NotInTrackedChannel { observed: ChannelId },
    /// Parent resolution must fail closed: otherwise a thread interaction could be
    /// checked against the wrong tracked scope.
    ChannelScopeLookup {
        observed: ChannelId,
        message: String,
    },
}

/// Proof that an incoming interaction satisfied the preconditions (guild context +
/// tracked channel). Construction is private to this module: holding a value of
/// this type is only possible via [`guard_ledger_interaction`], so downstream code
/// cannot fabricate a scope that bypasses the checks.
///
/// Aggregate identity is deliberately absent: the locator resolves an existing
/// canonical thread, or the expense bootstrap path issues a fresh `LedgerId`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LedgerInteractionScope {
    guild_id: GuildId,
    interaction_channel_id: ChannelId,
    tracked_parent_channel_id: ChannelId,
}

impl LedgerInteractionScope {
    pub(crate) fn guild_id(&self) -> GuildId {
        self.guild_id
    }
    pub(crate) fn channel_id(&self) -> ChannelId {
        self.tracked_parent_channel_id
    }
    pub(crate) fn interaction_channel_id(&self) -> ChannelId {
        self.interaction_channel_id
    }
    pub(crate) fn is_thread_interaction(&self) -> bool {
        self.interaction_channel_id != self.tracked_parent_channel_id
    }
    pub(crate) fn expense_draft_scope_id(&self) -> ExpenseDraftScopeId {
        ExpenseDraftScopeId::new(self.tracked_parent_channel_id.get())
            .expect("serenity channel IDs are always non-zero")
    }
    pub(crate) fn tracked_parent(&self) -> TrackedParentKey {
        TrackedParentKey::from_guarded_parent(self.guild_id, self.tracked_parent_channel_id)
    }
}

/// Guard a guild-scoped slash / component / modal interaction. Returns the typed
/// [`LedgerInteractionScope`] when the interaction is in-bounds; otherwise returns
/// a typed reason the route handler can render. The channel argument is whatever
/// scope the interaction targets — for thread interactions the caller is expected to
/// pre-resolve the parent via [`slash_scope_channel_id`].
///
pub(crate) fn guard_ledger_interaction(
    guild_id: Option<GuildId>,
    channel_id: ChannelId,
    channels: &ChannelManager,
) -> Result<LedgerInteractionScope, LedgerInteractionGuardError> {
    let guild_id = guild_id.ok_or(LedgerInteractionGuardError::GuildOnly)?;
    if !channels.is_tracked(channel_id) {
        return Err(LedgerInteractionGuardError::NotInTrackedChannel {
            observed: channel_id,
        });
    }
    Ok(LedgerInteractionScope {
        guild_id,
        interaction_channel_id: channel_id,
        tracked_parent_channel_id: channel_id,
    })
}

pub(crate) async fn guard_ledger_interaction_resolving_parent(
    ctx: &Context,
    guild_id: Option<GuildId>,
    interaction_channel_id: ChannelId,
    channels: &ChannelManager,
) -> Result<LedgerInteractionScope, LedgerInteractionGuardError> {
    let guild_id = guild_id.ok_or(LedgerInteractionGuardError::GuildOnly)?;
    let channel = interaction_channel_id
        .to_channel(&ctx.http)
        .await
        .map_err(|error| LedgerInteractionGuardError::ChannelScopeLookup {
            observed: interaction_channel_id,
            message: error.to_string(),
        })?;
    let tracked_parent_channel_id = match channel.guild() {
        Some(channel) => slash_scope_channel_id(channel.id, channel.kind, channel.parent_id)
            .map_err(|error| LedgerInteractionGuardError::ChannelScopeLookup {
                observed: interaction_channel_id,
                message: error.to_string(),
            })?,
        None => interaction_channel_id,
    };
    if !channels.is_tracked(tracked_parent_channel_id) {
        return Err(LedgerInteractionGuardError::NotInTrackedChannel {
            observed: interaction_channel_id,
        });
    }
    Ok(LedgerInteractionScope {
        guild_id,
        interaction_channel_id,
        tracked_parent_channel_id,
    })
}

pub(crate) fn slash_scope_channel_id(
    channel_id: ChannelId,
    kind: ChannelType,
    parent_id: Option<ChannelId>,
) -> Result<ChannelId, SlashScopeError> {
    let is_thread = matches!(
        kind,
        ChannelType::PublicThread | ChannelType::PrivateThread | ChannelType::NewsThread
    );
    if is_thread {
        parent_id.ok_or(SlashScopeError::ThreadWithoutParent(channel_id))
    } else {
        Ok(channel_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::channel::ChannelManager;
    use serenity::all::{ChannelId, GuildId};

    #[test]
    fn outside_tracked_channel_message_prefers_known_parent_hint() {
        assert_eq!(
            outside_tracked_channel_message(Some(ChannelId::new(42))),
            "このチャンネルは台帳の対象ではありません。<#42> で実行してください。"
        );
    }

    #[test]
    fn outside_tracked_channel_message_falls_back_to_generic_guidance() {
        assert_eq!(
            outside_tracked_channel_message(None),
            "このチャンネルは台帳の対象ではありません。記録用チャンネルで実行してください。わからない場合は管理者に確認してください。"
        );
    }

    #[test]
    fn guard_rejects_interaction_without_guild_context() {
        let channels = ChannelManager::new();
        let actual = guard_ledger_interaction(None, ChannelId::new(1), &channels);
        assert_eq!(actual, Err(LedgerInteractionGuardError::GuildOnly));
    }

    #[test]
    fn guard_rejects_interaction_in_untracked_channel() {
        let channels = ChannelManager::new();
        let actual = guard_ledger_interaction(Some(GuildId::new(1)), ChannelId::new(99), &channels);
        assert_eq!(
            actual,
            Err(LedgerInteractionGuardError::NotInTrackedChannel {
                observed: ChannelId::new(99)
            })
        );
    }

    #[test]
    fn guard_accepts_tracked_channel_with_guild_context() {
        let channels = ChannelManager::new();
        channels.track(ChannelId::new(42));

        let scope = guard_ledger_interaction(Some(GuildId::new(1)), ChannelId::new(42), &channels)
            .expect("guard should accept tracked channel with guild context");

        assert_eq!(scope.guild_id(), GuildId::new(1));
        assert_eq!(scope.channel_id(), ChannelId::new(42));
        assert_eq!(
            scope.tracked_parent(),
            TrackedParentKey::from_guarded_parent(GuildId::new(1), ChannelId::new(42))
        );
    }
}
