use crate::message_cache::MessageCache;
use dashmap::DashSet;
use serenity::model::id::ChannelId;

const CHANNEL_TOPIC_FLAG: &str = "#walicord";

/// Result of toggling a channel's enabled state
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChannelToggle {
    Enabled,
    Disabled,
    NoChange,
}

/// Manages enabled/disabled state of channels
#[derive(Clone)]
pub struct ChannelManager {
    enabled_channels: DashSet<ChannelId>,
    message_cache: MessageCache,
}

impl ChannelManager {
    pub fn new(message_cache: MessageCache) -> Self {
        Self {
            enabled_channels: DashSet::new(),
            message_cache,
        }
    }

    /// Check if a channel is enabled
    pub fn is_enabled(&self, channel_id: ChannelId) -> bool {
        self.enabled_channels.contains(&channel_id)
    }

    /// Check if channel topic contains the walicord flag
    pub fn topic_has_flag(topic: Option<&str>) -> bool {
        topic.is_some_and(|topic| topic.contains(CHANNEL_TOPIC_FLAG))
    }

    /// Enable a channel and return the toggle result
    pub fn enable(&self, channel_id: ChannelId) -> ChannelToggle {
        if self.enabled_channels.insert(channel_id) {
            ChannelToggle::Enabled
        } else {
            ChannelToggle::NoChange
        }
    }

    /// Disable a channel, clear its cache, and return the toggle result
    pub fn disable(&self, channel_id: ChannelId) -> ChannelToggle {
        if self.enabled_channels.remove(&channel_id).is_some() {
            self.message_cache.remove(&channel_id);
            ChannelToggle::Disabled
        } else {
            ChannelToggle::NoChange
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;
    use serenity::{
        all::MessageId,
        model::{channel::Message, id::UserId},
    };

    fn make_message(id: u64, content: &str) -> Message {
        let mut message = Message::default();
        message.id = MessageId::new(id);
        message.content = content.to_string();
        message.author = serenity::model::user::User::default();
        message.author.id = UserId::new(1);
        message
    }

    #[rstest::rstest]
    #[case::flag_present(Some("topic #walicord enabled"), true)]
    #[case::flag_missing(Some("topic walicord"), false)]
    #[case::empty(None, false)]
    fn topic_has_flag_cases(#[case] topic: Option<&str>, #[case] expected: bool) {
        assert_eq!(ChannelManager::topic_has_flag(topic), expected);
    }

    #[test]
    fn enable_and_disable_channel() {
        let cache = MessageCache::new();
        let manager = ChannelManager::new(cache);
        let channel_id = ChannelId::new(1);

        assert_eq!(manager.enable(channel_id), ChannelToggle::Enabled);
        assert!(manager.is_enabled(channel_id));
        assert_eq!(manager.enable(channel_id), ChannelToggle::NoChange);

        assert_eq!(manager.disable(channel_id), ChannelToggle::Disabled);
        assert!(!manager.is_enabled(channel_id));
        assert_eq!(manager.disable(channel_id), ChannelToggle::NoChange);
    }

    #[test]
    fn disable_clears_cache() {
        let cache = MessageCache::new();
        let manager = ChannelManager::new(cache.clone());

        let mut messages = IndexMap::new();
        messages.insert(MessageId::new(10), make_message(10, "content"));
        cache.insert(ChannelId::new(1), messages);
        manager.enable(ChannelId::new(1));

        assert!(cache.get(ChannelId::new(1)).is_some());
        manager.disable(ChannelId::new(1));
        assert!(cache.get(ChannelId::new(1)).is_none());
    }

    #[test]
    fn preserves_other_channels_when_disabling() {
        let cache = MessageCache::new();
        let manager = ChannelManager::new(cache);

        manager.enable(ChannelId::new(1));
        manager.enable(ChannelId::new(2));

        manager.disable(ChannelId::new(1));

        assert!(!manager.is_enabled(ChannelId::new(1)));
        assert!(manager.is_enabled(ChannelId::new(2)));
    }
}
