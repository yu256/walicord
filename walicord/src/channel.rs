use dashmap::DashSet;
use serenity::model::id::ChannelId;

const CHANNEL_TOPIC_FLAG: &str = "#walicord";

/// Domain event emitted when channel state changes
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChannelEvent {
    Enabled(ChannelId),
    Disabled(ChannelId),
}

/// Aggregate root managing enabled/disabled state of channels
#[derive(Clone, Default)]
pub struct ChannelManager {
    enabled_channels: DashSet<ChannelId>,
}

impl ChannelManager {
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if a channel is enabled
    pub fn is_enabled(&self, channel_id: ChannelId) -> bool {
        self.enabled_channels.contains(&channel_id)
    }

    /// Check if channel topic contains the walicord flag
    pub fn topic_has_flag(topic: Option<&str>) -> bool {
        topic.is_some_and(|topic| topic.contains(CHANNEL_TOPIC_FLAG))
    }

    /// Enable a channel and return the domain event if state changed
    pub fn enable(&self, channel_id: ChannelId) -> Option<ChannelEvent> {
        if self.enabled_channels.insert(channel_id) {
            Some(ChannelEvent::Enabled(channel_id))
        } else {
            None
        }
    }

    /// Disable a channel and return the domain event if state changed
    /// Note: Cache cleanup is the caller's responsibility (separate aggregate)
    pub fn disable(&self, channel_id: ChannelId) -> Option<ChannelEvent> {
        self.enabled_channels
            .remove(&channel_id)
            .map(|_| ChannelEvent::Disabled(channel_id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::flag_present(Some("topic #walicord enabled"), true)]
    #[case::flag_missing(Some("topic walicord"), false)]
    #[case::empty(None, false)]
    fn topic_has_flag_cases(#[case] topic: Option<&str>, #[case] expected: bool) {
        assert_eq!(ChannelManager::topic_has_flag(topic), expected);
    }

    #[rstest]
    #[case::first_enable_emits_event(
        ChannelId::new(1),
        true,
        Some(ChannelEvent::Enabled(ChannelId::new(1)))
    )]
    #[case::second_enable_returns_none(ChannelId::new(1), false, None)]
    fn enable_returns_event_only_on_state_change(
        #[case] channel_id: ChannelId,
        #[case] is_first: bool,
        #[case] expected: Option<ChannelEvent>,
    ) {
        let manager = ChannelManager::new();
        if !is_first {
            manager.enable(channel_id);
        }
        assert_eq!(manager.enable(channel_id), expected);
    }

    #[rstest]
    #[case::disable_enabled_emits_event(true, Some(ChannelEvent::Disabled(ChannelId::new(1))))]
    #[case::disable_not_enabled_returns_none(false, None)]
    fn disable_returns_event_only_when_was_enabled(
        #[case] was_enabled: bool,
        #[case] expected: Option<ChannelEvent>,
    ) {
        let manager = ChannelManager::new();
        let channel_id = ChannelId::new(1);
        if was_enabled {
            manager.enable(channel_id);
        }
        assert_eq!(manager.disable(channel_id), expected);
    }

    #[test]
    fn enable_then_disable_produces_correct_state_sequence() {
        let manager = ChannelManager::new();
        let channel_id = ChannelId::new(1);

        assert!(!manager.is_enabled(channel_id));

        let enable_event = manager.enable(channel_id);
        assert_eq!(enable_event, Some(ChannelEvent::Enabled(channel_id)));
        assert!(manager.is_enabled(channel_id));

        let disable_event = manager.disable(channel_id);
        assert_eq!(disable_event, Some(ChannelEvent::Disabled(channel_id)));
        assert!(!manager.is_enabled(channel_id));

        let disable_again = manager.disable(channel_id);
        assert_eq!(disable_again, None);
    }

    #[rstest]
    fn preserves_other_channels_when_disabling(
        #[values(ChannelId::new(1), ChannelId::new(2))] target_channel: ChannelId,
        #[values(ChannelId::new(3), ChannelId::new(4))] other_channel: ChannelId,
    ) {
        let manager = ChannelManager::new();

        manager.enable(target_channel);
        manager.enable(other_channel);

        manager.disable(target_channel);

        assert!(!manager.is_enabled(target_channel));
        assert!(manager.is_enabled(other_channel));
    }
}
