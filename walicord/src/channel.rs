use dashmap::DashMap;
use serenity::model::id::ChannelId;

const CHANNEL_TOPIC_FLAG: &str = "#walicord";

/// A ChannelId that has been verified as tracked.
///
/// This newtype ensures that MessageCache operations only occur on tracked channels,
/// enforcing the invariant at the type level rather than relying on caller discipline.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TrackedChannelId(ChannelId);

impl TrackedChannelId {
    pub fn get(self) -> ChannelId {
        self.0
    }

    #[cfg(test)]
    pub fn new_for_test(id: ChannelId) -> Self {
        Self(id)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChannelEvent {
    Tracked(TrackedChannelId),
    Untracked(ChannelId),
}

/// Status of the last fetch attempt for a tracked channel.
///
/// This is purely observational/diagnostic and does NOT control fetch behavior.
/// A Failed status does not prevent future fetch attempts on cache miss.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum FetchStatus {
    #[default]
    Unknown,
    Ok,
    Failed,
}

#[derive(Clone, Default)]
pub struct ChannelManager {
    channels: DashMap<ChannelId, FetchStatus>,
}

impl ChannelManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_tracked(&self, channel_id: ChannelId) -> bool {
        self.channels.contains_key(&channel_id)
    }

    /// Returns TrackedChannelId if the channel is tracked, None otherwise.
    /// Use this to obtain a type-level proof of tracking for MessageCache operations.
    pub fn get_tracked(&self, channel_id: ChannelId) -> Option<TrackedChannelId> {
        self.channels
            .get(&channel_id)
            .map(|_| TrackedChannelId(channel_id))
    }

    #[cfg(test)]
    pub fn has_fetch_failed(&self, channel_id: ChannelId) -> bool {
        self.channels
            .get(&channel_id)
            .map(|s| *s == FetchStatus::Failed)
            .unwrap_or(false)
    }

    pub fn topic_has_flag(topic: Option<&str>) -> bool {
        topic.is_some_and(|topic| topic.contains(CHANNEL_TOPIC_FLAG))
    }

    pub fn track(&self, channel_id: ChannelId) -> Option<ChannelEvent> {
        use dashmap::mapref::entry::Entry;
        match self.channels.entry(channel_id) {
            Entry::Vacant(e) => {
                e.insert(FetchStatus::Unknown);
                Some(ChannelEvent::Tracked(TrackedChannelId(channel_id)))
            }
            Entry::Occupied(_) => None,
        }
    }

    #[must_use = "returns false if channel is not tracked, caller should decide whether to handle this"]
    pub fn mark_fetch_failed(&self, channel_id: ChannelId) -> bool {
        if let Some(mut status) = self.channels.get_mut(&channel_id) {
            *status = FetchStatus::Failed;
            true
        } else {
            false
        }
    }

    #[must_use = "returns false if channel is not tracked, caller should decide whether to handle this"]
    pub fn mark_fetch_succeeded(&self, channel_id: ChannelId) -> bool {
        if let Some(mut status) = self.channels.get_mut(&channel_id) {
            *status = FetchStatus::Ok;
            true
        } else {
            false
        }
    }

    pub fn untrack(&self, channel_id: ChannelId) -> Option<ChannelEvent> {
        self.channels
            .remove(&channel_id)
            .map(|_| ChannelEvent::Untracked(channel_id))
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
    #[case::first_track_emits_event(ChannelId::new(1), true, true)]
    #[case::second_track_returns_none(ChannelId::new(1), false, false)]
    fn track_returns_event_only_on_state_change(
        #[case] channel_id: ChannelId,
        #[case] is_first: bool,
        #[case] expected_emits_event: bool,
    ) {
        let manager = ChannelManager::new();
        if !is_first {
            manager.track(channel_id);
        }
        let result = manager.track(channel_id);
        assert_eq!(result.is_some(), expected_emits_event);
        if let Some(ChannelEvent::Tracked(tracked_id)) = result {
            assert_eq!(tracked_id.get(), channel_id);
        }
    }

    #[rstest]
    #[case::untrack_tracked_emits_event(true, true)]
    #[case::untrack_not_tracked_returns_none(false, false)]
    fn untrack_returns_event_only_when_was_tracked(
        #[case] was_tracked: bool,
        #[case] expected_emits_event: bool,
    ) {
        let manager = ChannelManager::new();
        let channel_id = ChannelId::new(1);
        if was_tracked {
            manager.track(channel_id);
        }
        let result = manager.untrack(channel_id);
        assert_eq!(result.is_some(), expected_emits_event);
        if let Some(ChannelEvent::Untracked(untracked_id)) = result {
            assert_eq!(untracked_id, channel_id);
        }
    }

    #[test]
    fn track_then_untrack_produces_correct_state_sequence() {
        let manager = ChannelManager::new();
        let channel_id = ChannelId::new(1);

        assert!(!manager.is_tracked(channel_id));

        let track_event = manager.track(channel_id);
        assert!(matches!(track_event, Some(ChannelEvent::Tracked(_))));
        assert!(manager.is_tracked(channel_id));

        let untrack_event = manager.untrack(channel_id);
        assert!(matches!(untrack_event, Some(ChannelEvent::Untracked(_))));
        assert!(!manager.is_tracked(channel_id));

        let untrack_again = manager.untrack(channel_id);
        assert_eq!(untrack_again, None);
    }

    #[rstest]
    fn preserves_other_channels_when_untracking(
        #[values(ChannelId::new(1), ChannelId::new(2))] target_channel: ChannelId,
        #[values(ChannelId::new(3), ChannelId::new(4))] other_channel: ChannelId,
    ) {
        let manager = ChannelManager::new();

        manager.track(target_channel);
        manager.track(other_channel);

        manager.untrack(target_channel);

        assert!(!manager.is_tracked(target_channel));
        assert!(manager.is_tracked(other_channel));
    }

    #[test]
    fn untrack_removes_channel_state() {
        let manager = ChannelManager::new();
        let channel_id = ChannelId::new(1);
        manager.track(channel_id);
        assert!(manager.mark_fetch_failed(channel_id));

        assert!(manager.untrack(channel_id).is_some());
        assert!(!manager.is_tracked(channel_id));
        assert!(!manager.has_fetch_failed(channel_id));
    }

    #[test]
    fn has_fetch_failed_reflects_fetch_status() {
        let manager = ChannelManager::new();
        let channel_id = ChannelId::new(1);

        assert!(!manager.has_fetch_failed(channel_id));

        manager.track(channel_id);
        assert!(!manager.has_fetch_failed(channel_id));

        assert!(manager.mark_fetch_failed(channel_id));
        assert!(manager.has_fetch_failed(channel_id));

        assert!(manager.mark_fetch_succeeded(channel_id));
        assert!(!manager.has_fetch_failed(channel_id));
    }

    #[test]
    fn mark_fetch_returns_false_when_not_tracked() {
        let manager = ChannelManager::new();
        let channel_id = ChannelId::new(1);

        assert!(!manager.mark_fetch_failed(channel_id));
        assert!(!manager.has_fetch_failed(channel_id));

        assert!(!manager.mark_fetch_succeeded(channel_id));
        assert!(!manager.has_fetch_failed(channel_id));
    }

    #[test]
    fn mark_fetch_returns_true_when_tracked() {
        let manager = ChannelManager::new();
        let channel_id = ChannelId::new(1);
        manager.track(channel_id);

        assert!(manager.mark_fetch_failed(channel_id));
        assert!(manager.has_fetch_failed(channel_id));

        assert!(manager.mark_fetch_succeeded(channel_id));
        assert!(!manager.has_fetch_failed(channel_id));
    }
}
