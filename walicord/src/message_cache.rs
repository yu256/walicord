use dashmap::DashMap;
use indexmap::IndexMap;
use serenity::{
    all::MessageId,
    model::channel::{Message, ReactionType},
};
use std::sync::Arc;

/// Cache for messages in enabled channels
#[derive(Clone)]
pub struct MessageCache {
    inner: Arc<DashMap<ChannelId, IndexMap<MessageId, Message>>>,
}

impl MessageCache {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(DashMap::new()),
        }
    }

    pub fn insert(&self, channel_id: ChannelId, messages: IndexMap<MessageId, Message>) {
        self.inner.insert(channel_id, messages);
    }

    pub fn remove(&self, channel_id: &ChannelId) -> Option<IndexMap<MessageId, Message>> {
        self.inner.remove(channel_id).map(|(_, v)| v)
    }

    pub fn entry(
        &self,
        channel_id: ChannelId,
    ) -> dashmap::mapref::entry::Entry<'_, ChannelId, IndexMap<MessageId, Message>> {
        self.inner.entry(channel_id)
    }

    /// Check if a channel exists in the cache
    #[cfg(test)]
    pub fn contains(&self, channel_id: ChannelId) -> bool {
        self.inner.contains_key(&channel_id)
    }

    /// Remove a specific message from a channel's cache
    pub fn remove_message(&self, channel_id: ChannelId, message_id: MessageId) -> bool {
        if let Some(mut messages) = self.inner.get_mut(&channel_id) {
            messages.shift_remove(&message_id);
            let should_remove_channel = messages.is_empty();
            drop(messages);
            if should_remove_channel {
                self.inner.remove(&channel_id);
            }
            true
        } else {
            false
        }
    }

    /// Remove multiple messages from a channel's cache
    pub fn remove_messages(&self, channel_id: ChannelId, message_ids: &[MessageId]) -> bool {
        if let Some(mut messages) = self.inner.get_mut(&channel_id) {
            for message_id in message_ids {
                messages.shift_remove(message_id);
            }
            let should_remove_channel = messages.is_empty();
            drop(messages);
            if should_remove_channel {
                self.inner.remove(&channel_id);
            }
            true
        } else {
            false
        }
    }

    /// Execute a closure with read access to a channel's messages
    /// This ensures the lock is held only for the duration of the closure
    pub fn with_messages<F, R>(&self, channel_id: ChannelId, f: F) -> Option<R>
    where
        F: FnOnce(&IndexMap<MessageId, Message>) -> R,
    {
        self.inner
            .get(&channel_id)
            .map(|ref_val| f(ref_val.value()))
    }

    /// Execute a closure with mutable access to a channel's messages
    /// This ensures the lock is held only for the duration of the closure
    pub fn with_messages_mut<F, R>(&self, channel_id: ChannelId, f: F) -> Option<R>
    where
        F: FnOnce(&mut IndexMap<MessageId, Message>) -> R,
    {
        self.inner
            .get_mut(&channel_id)
            .map(|mut ref_val| f(ref_val.value_mut()))
    }
}

impl Default for MessageCache {
    fn default() -> Self {
        Self::new()
    }
}

use serenity::model::id::ChannelId;

/// Update cached reactions when a reaction is added
pub fn update_cached_reaction_add(
    message: &mut Message,
    emoji: &ReactionType,
    is_me: bool,
    is_burst: bool,
) -> bool {
    let Some(entry) = message
        .reactions
        .iter_mut()
        .find(|reaction| reaction.reaction_type == *emoji)
    else {
        return false;
    };

    entry.count = entry.count.saturating_add(1);
    if is_burst {
        entry.count_details.burst = entry.count_details.burst.saturating_add(1);
        if is_me {
            entry.me_burst = true;
        }
    } else {
        entry.count_details.normal = entry.count_details.normal.saturating_add(1);
    }
    if is_me {
        entry.me = true;
    }

    true
}

/// Update cached reactions when a reaction is removed
pub fn update_cached_reaction_remove(
    message: &mut Message,
    emoji: &ReactionType,
    is_me: bool,
    is_burst: bool,
) -> bool {
    let Some(position) = message
        .reactions
        .iter()
        .position(|reaction| reaction.reaction_type == *emoji)
    else {
        return false;
    };

    let entry = &mut message.reactions[position];
    if entry.count > 0 {
        entry.count -= 1;
    }
    if is_burst {
        entry.count_details.burst = entry.count_details.burst.saturating_sub(1);
        if is_me {
            entry.me_burst = false;
        }
    } else {
        entry.count_details.normal = entry.count_details.normal.saturating_sub(1);
    }
    if is_me {
        entry.me = false;
    }

    if entry.count == 0 {
        message.reactions.remove(position);
    }

    true
}

/// Calculate line count increment for message concatenation
fn line_count_increment(content: &str, prior_ended_with_newline: bool) -> usize {
    content.lines().count() + if prior_ended_with_newline { 1 } else { 0 }
}

/// Calculate next line offset for a sequence of messages
pub fn next_line_offset<'a, I>(messages: I) -> usize
where
    I: IntoIterator<Item = &'a Message>,
{
    let mut line_count = 0usize;
    let mut ends_with_newline = false;
    let mut has_any = false;

    for message in messages {
        let content = message.content.as_str();
        if has_any {
            line_count += line_count_increment(content, ends_with_newline);
        } else {
            line_count = content.lines().count();
        }
        ends_with_newline = content.is_empty() || content.ends_with('\n');
        has_any = true;
    }

    if has_any {
        line_count + if ends_with_newline { 1 } else { 0 }
    } else {
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serenity::model::id::UserId;

    fn make_message(id: u64, channel_id: u64, author_id: u64, content: &str) -> Message {
        let mut message = Message::default();
        message.id = MessageId::new(id);
        message.channel_id = ChannelId::new(channel_id);
        message.author = serenity::model::user::User::default();
        message.author.id = UserId::new(author_id);
        message.content = content.to_string();
        message
    }

    fn make_reaction(
        emoji: &str,
        count: u64,
        normal: u64,
        burst: u64,
        me: bool,
        me_burst: bool,
    ) -> serenity::model::channel::MessageReaction {
        use serde_json::json;
        serde_json::from_value(json!({
            "count": count,
            "count_details": { "normal": normal, "burst": burst },
            "me": me,
            "me_burst": me_burst,
            "emoji": { "id": null, "name": emoji },
            "burst_colors": []
        }))
        .expect("MessageReaction should deserialize")
    }

    #[rstest::rstest]
    #[case::existing_entry(
        vec![make_reaction("✅", 1, 1, 0, false, false)],
        true,
        vec![(2, true, 2)],
    )]
    #[case::missing_entry(vec![], false, vec![])]
    fn update_cached_reaction_add_cases(
        #[case] initial: Vec<serenity::model::channel::MessageReaction>,
        #[case] expected_updated: bool,
        #[case] expected_snapshot: Vec<(u64, bool, u64)>,
    ) {
        let mut message = Message::default();
        message.reactions = initial;

        let updated = update_cached_reaction_add(
            &mut message,
            &ReactionType::Unicode("✅".to_string()),
            true,
            false,
        );

        let snapshot: Vec<(u64, bool, u64)> = message
            .reactions
            .iter()
            .map(|reaction| (reaction.count, reaction.me, reaction.count_details.normal))
            .collect();

        assert_eq!(updated, expected_updated);
        assert_eq!(snapshot, expected_snapshot);
    }

    #[rstest::rstest]
    #[case::delete_when_count_reaches_zero(
        vec![make_reaction("✅", 1, 1, 0, true, false)],
        true,
        vec![],
    )]
    #[case::missing_entry(vec![], false, vec![])]
    fn update_cached_reaction_remove_cases(
        #[case] initial: Vec<serenity::model::channel::MessageReaction>,
        #[case] expected_updated: bool,
        #[case] expected_snapshot: Vec<(u64, bool, u64)>,
    ) {
        let mut message = Message::default();
        message.reactions = initial;

        let updated = update_cached_reaction_remove(
            &mut message,
            &ReactionType::Unicode("✅".to_string()),
            true,
            false,
        );

        let snapshot: Vec<(u64, bool, u64)> = message
            .reactions
            .iter()
            .map(|reaction| (reaction.count, reaction.me, reaction.count_details.normal))
            .collect();

        assert_eq!(updated, expected_updated);
        assert_eq!(snapshot, expected_snapshot);
    }

    #[rstest::rstest]
    #[case("a\nb", "c", 3)]
    #[case("a\nb\n", "c", 4)]
    fn next_line_offset_calculates_correctly(
        #[case] first: &str,
        #[case] second: &str,
        #[case] expected_offset: usize,
    ) {
        let mut messages: IndexMap<MessageId, Message> = IndexMap::new();
        messages.insert(MessageId::new(1), make_message(1, 1, 1, first));

        let second_message = make_message(2, 1, 1, second);
        let offset = next_line_offset(
            messages
                .iter()
                .map(|(_, m)| m)
                .chain(std::iter::once(&second_message)),
        );

        assert_eq!(offset, expected_offset);
    }

    #[rstest::rstest]
    #[case::existing_channel_returns_some(ChannelId::new(1), Some(1))]
    #[case::missing_channel_returns_none(ChannelId::new(99), None)]
    fn with_messages_returns_result_or_none(
        #[case] channel_id: ChannelId,
        #[case] expected: Option<usize>,
    ) {
        let cache = MessageCache::new();
        let mut messages = IndexMap::new();
        messages.insert(MessageId::new(10), make_message(10, 1, 1, "test"));
        cache.insert(ChannelId::new(1), messages);

        let result = cache.with_messages(channel_id, |msgs| msgs.len());
        assert_eq!(result, expected);
    }

    #[test]
    fn with_messages_does_not_expose_lock_guard() {
        let cache = MessageCache::new();
        let mut messages = IndexMap::new();
        messages.insert(MessageId::new(10), make_message(10, 1, 1, "test"));
        cache.insert(ChannelId::new(1), messages);

        let result = cache.with_messages(ChannelId::new(1), |msgs| {
            msgs.get(&MessageId::new(10)).map(|m| m.content.clone())
        });
        assert_eq!(result, Some(Some("test".to_string())));
    }
}
