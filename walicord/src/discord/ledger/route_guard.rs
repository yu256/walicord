use crate::channel::ChannelManager;
use serenity::all::{ChannelId, ChannelType, GuildId};
use walicord_application::ledger::LedgerId;
use walicord_i18n as i18n;

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
                i18n::outside_tracked_channel_message_with_hint(format!("<#{}>", channel_id.get()))
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
}

/// Proof that an incoming interaction satisfied the preconditions (guild context +
/// tracked channel). Construction is private to this module: holding a value of
/// this type is only possible via [`guard_ledger_interaction`], so downstream code
/// cannot fabricate a scope that bypasses the checks.
///
/// `ledger_id()` is the **only** identifier downstream application-layer code uses
/// — session keys, write coordinator targets, etc. take `LedgerId`. `guild_id()` /
/// `channel_id()` stay accessible because the adapter still needs them to call
/// serenity APIs (`roster.display_names_for_guild`, `channel.send_message`, …), but
/// session-key construction is structurally type-safe: passing a `ChannelId` where
/// `LedgerId` is expected fails to compile.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LedgerInteractionScope {
    guild_id: GuildId,
    channel_id: ChannelId,
    ledger_id: LedgerId,
}

impl LedgerInteractionScope {
    pub(crate) fn guild_id(&self) -> GuildId {
        self.guild_id
    }
    pub(crate) fn channel_id(&self) -> ChannelId {
        self.channel_id
    }
    pub(crate) fn ledger_id(&self) -> LedgerId {
        self.ledger_id
    }
}

/// Guard a guild-scoped slash / component / modal interaction. Returns the typed
/// [`LedgerInteractionScope`] when the interaction is in-bounds; otherwise returns
/// a typed reason the route handler can render. The channel argument is whatever
/// scope the interaction targets — for thread interactions the caller is expected to
/// pre-resolve the parent via [`slash_scope_channel_id`].
///
/// The `(GuildId, ChannelId) → LedgerId` mapping is performed exactly once here so
/// the application layer (sessions, preview store, write coordinator targets) never
/// sees Discord-side identifiers. Adding alternative mappings (e.g. routing two
/// channels to the same ledger) means changing this function; the rest of the
/// router does not need to know.
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
        channel_id,
        ledger_id: LedgerId(channel_id.get()),
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
        assert_eq!(scope.ledger_id(), LedgerId(42));
    }
}
