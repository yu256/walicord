use crate::channel::ChannelManager;
use serenity::all::{ChannelId, ChannelType};
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
    use super::outside_tracked_channel_message;
    use serenity::all::ChannelId;

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
}
