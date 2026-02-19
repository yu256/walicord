use dashmap::DashMap;
use indexmap::IndexMap;
use serenity::{
    all::MessageId,
    model::{
        channel::{Message, ReactionType},
        event::MessageUpdateEvent,
        id::{ChannelId, UserId},
    },
};
use std::sync::Arc;

use crate::channel::TrackedChannelId;
use crate::reaction::{BotReaction, BotReactionState};

#[derive(Clone, Debug)]
pub struct CachedMessage {
    pub id: MessageId,
    pub content: Arc<str>,
    pub author_id: UserId,
    pub is_bot: bool,
    pub reaction_state: BotReactionState,
}

impl CachedMessage {
    /// Returns true if bot has marked this message as invalid (has ❎ reaction).
    /// Invalidated messages are excluded from program evaluation.
    pub fn is_marked_invalid(&self) -> bool {
        self.reaction_state == BotReactionState::HasCross
    }
}

/// Extract bot's reaction state from a full Message object.
///
/// This serves as the reconciliation point for cache divergence caused by:
/// - Missed reaction events (gateway reconnect, partial intents)
/// - External reaction changes (manual deletion, moderation)
///
/// Called during `ensure_cache_loaded` to rebuild cache from Discord's authoritative state.
fn bot_reaction_state_from_message(msg: &Message) -> BotReactionState {
    let has_cross = msg.reactions.iter().any(
        |r| matches!(&r.reaction_type, ReactionType::Unicode(s) if s == BotReaction::CROSS && r.me),
    );
    let has_check = msg.reactions.iter().any(
        |r| matches!(&r.reaction_type, ReactionType::Unicode(s) if s == BotReaction::CHECK && r.me),
    );

    if has_cross {
        BotReactionState::HasCross
    } else if has_check {
        BotReactionState::HasCheck
    } else {
        BotReactionState::None
    }
}

impl CachedMessage {
    pub fn from_message(msg: Message) -> Self {
        let reaction_state = bot_reaction_state_from_message(&msg);
        Self {
            id: msg.id,
            content: Arc::from(msg.content.into_boxed_str()),
            author_id: msg.author.id,
            is_bot: msg.author.bot,
            reaction_state,
        }
    }

    /// Apply content changes from MessageUpdateEvent.
    ///
    /// Note: Reactions are NOT updated here. Discord does not guarantee that
    /// event.reactions is a complete snapshot. Reaction state is maintained via:
    /// - `reaction_add`/`reaction_remove` events (primary source)
    /// - `from_message` on full fetch (reconciliation)
    pub fn apply_event(&mut self, event: &MessageUpdateEvent) {
        if let Some(ref content) = event.content {
            self.content = Arc::from(content.as_str());
        }
    }
}

/// In-memory cache for channel messages.
///
/// # Invariant
/// This cache is a pure storage layer. The caller (BotHandler) is responsible for:
/// - Only caching messages from tracked channels
/// - Ensuring cache consistency with channel tracking state
///
/// Empty channels are represented as `IndexMap::new()` rather than being removed,
/// to distinguish "loaded but empty" from "not loaded".
#[derive(Clone)]
pub struct MessageCache {
    inner: Arc<DashMap<ChannelId, IndexMap<MessageId, CachedMessage>>>,
}

impl MessageCache {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(DashMap::new()),
        }
    }

    /// Insert messages for a tracked channel.
    ///
    /// The TrackedChannelId parameter ensures this is only called for tracked channels.
    pub fn insert(&self, channel_id: TrackedChannelId, messages: IndexMap<MessageId, CachedMessage>) {
        self.inner.insert(channel_id.get(), messages);
    }

    /// Remove all messages for a channel being untracked.
    ///
    /// The caller is responsible for only invoking this after the channel was tracked.
    pub fn remove(&self, channel_id: ChannelId) -> Option<IndexMap<MessageId, CachedMessage>> {
        self.inner.remove(&channel_id).map(|(_, v)| v)
    }

    #[cfg(test)]
    pub fn contains(&self, channel_id: ChannelId) -> bool {
        self.inner.contains_key(&channel_id)
    }

    pub fn remove_message(&self, channel_id: TrackedChannelId, message_id: MessageId) -> bool {
        if let Some(mut messages) = self.inner.get_mut(&channel_id.get()) {
            messages.shift_remove(&message_id);
            true
        } else {
            false
        }
    }

    pub fn remove_messages(&self, channel_id: TrackedChannelId, message_ids: &[MessageId]) -> bool {
        if let Some(mut messages) = self.inner.get_mut(&channel_id.get()) {
            for message_id in message_ids {
                messages.shift_remove(message_id);
            }
            true
        } else {
            false
        }
    }

    pub fn with_messages<F, R>(&self, channel_id: TrackedChannelId, f: F) -> Option<R>
    where
        F: FnOnce(&IndexMap<MessageId, CachedMessage>) -> R,
    {
        self.inner
            .get(&channel_id.get())
            .map(|ref_val| f(ref_val.value()))
    }

    /// Insert or update a message in the cache for a tracked channel.
    ///
    /// The TrackedChannelId parameter ensures this is only called for tracked channels.
    /// Creates a new channel entry if it doesn't exist.
    pub(crate) fn upsert_message(&self, channel_id: TrackedChannelId, message: CachedMessage) {
        self.inner
            .entry(channel_id.get())
            .or_insert_with(IndexMap::new)
            .insert(message.id, message);
    }

    /// Update reaction state for a message in a tracked channel.
    ///
    /// The TrackedChannelId parameter ensures this is only called for tracked channels.
    pub fn update_reaction_state(
        &self,
        channel_id: TrackedChannelId,
        message_id: MessageId,
        reaction_state: BotReactionState,
    ) -> bool {
        if let Some(mut messages) = self.inner.get_mut(&channel_id.get())
            && let Some(msg) = messages.get_mut(&message_id)
        {
            msg.reaction_state = reaction_state;
            true
        } else {
            false
        }
    }
}

impl Default for MessageCache {
    fn default() -> Self {
        Self::new()
    }
}

fn line_count_increment(content: &str, prior_ended_with_newline: bool) -> usize {
    content.lines().count() + if prior_ended_with_newline { 1 } else { 0 }
}

pub fn next_line_offset<'a, I>(messages: I) -> usize
where
    I: IntoIterator<Item = &'a CachedMessage>,
{
    let mut line_count = 0usize;
    let mut ends_with_newline = false;
    let mut has_any = false;

    for message in messages {
        if message.is_bot || message.is_marked_invalid() {
            continue;
        }
        let content = message.content.as_ref();
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
    use crate::channel::TrackedChannelId;

    fn make_cached_message(id: u64, author_id: u64, content: &str) -> CachedMessage {
        CachedMessage {
            id: MessageId::new(id),
            content: Arc::from(content),
            author_id: UserId::new(author_id),
            is_bot: false,
            reaction_state: BotReactionState::None,
        }
    }

    fn make_message(id: u64, channel_id: u64, author_id: u64, content: &str) -> Message {
        let mut message = Message::default();
        message.id = MessageId::new(id);
        message.channel_id = ChannelId::new(channel_id);
        message.author = serenity::model::user::User::default();
        message.author.id = UserId::new(author_id);
        message.content = content.to_string();
        message
    }

    #[rstest::rstest]
    #[case("a\nb", "c", 3)]
    #[case("a\nb\n", "c", 4)]
    fn next_line_offset_calculates_correctly(
        #[case] first: &str,
        #[case] second: &str,
        #[case] expected_offset: usize,
    ) {
        let mut messages: IndexMap<MessageId, CachedMessage> = IndexMap::new();
        messages.insert(MessageId::new(1), make_cached_message(1, 1, first));

        let second_message = make_cached_message(2, 1, second);
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
        messages.insert(MessageId::new(10), make_cached_message(10, 1, "test"));
        cache.insert(TrackedChannelId::new_for_test(ChannelId::new(1)), messages);

        let tracked_id = TrackedChannelId::new_for_test(channel_id);
        let result = cache.with_messages(tracked_id, |msgs| msgs.len());
        assert_eq!(result, expected);
    }

    #[test]
    fn cached_message_from_message() {
        let msg = make_message(1, 2, 3, "hello");
        let cached = CachedMessage::from_message(msg);
        assert_eq!(cached.id, MessageId::new(1));
        assert_eq!(&*cached.content, "hello");
        assert_eq!(cached.author_id, UserId::new(3));
        assert!(!cached.is_bot);
    }

    #[test]
    fn next_line_offset_skips_marked_invalid_messages() {
        let valid = make_cached_message(1, 1, "line1\nline2");
        let mut marked_invalid = make_cached_message(2, 1, "line3");
        marked_invalid.reaction_state = BotReactionState::HasCross;
        let valid2 = make_cached_message(3, 1, "line4");

        let offset = next_line_offset([&valid, &marked_invalid, &valid2]);
        assert_eq!(offset, 3);
    }

    #[rstest::rstest]
    #[case::marked_invalid_is_has_cross(BotReactionState::HasCross, true)]
    #[case::has_check_is_not_marked_invalid(BotReactionState::HasCheck, false)]
    #[case::none_is_not_marked_invalid(BotReactionState::None, false)]
    fn is_marked_invalid_matches_has_cross(
        #[case] reaction_state: BotReactionState,
        #[case] expected: bool,
    ) {
        let mut cached = make_cached_message(1, 1, "test");
        cached.reaction_state = reaction_state;
        assert_eq!(cached.is_marked_invalid(), expected);
    }
}
