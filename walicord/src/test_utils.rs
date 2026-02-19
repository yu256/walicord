//! Mock implementations for testing

use crate::discord::ports::{ChannelService, RosterProvider, ServiceError};
use indexmap::IndexMap;
use serenity::{
    all::MessageId,
    model::{
        channel::Message,
        id::{ChannelId, GuildId},
    },
    prelude::*,
};
use std::{
    collections::HashMap,
    future::{Ready, ready},
    sync::Arc,
    sync::atomic::{AtomicUsize, Ordering},
};
use walicord_domain::model::{MemberId, MemberInfo};

/// Mock ChannelService for testing with fetch count tracking
#[derive(Clone)]
pub struct MockChannelService {
    messages: HashMap<ChannelId, IndexMap<MessageId, Message>>,
    fetch_count: Arc<AtomicUsize>,
}

impl MockChannelService {
    pub fn new() -> Self {
        Self {
            messages: HashMap::new(),
            fetch_count: Arc::new(AtomicUsize::new(0)),
        }
    }

    #[allow(dead_code)]
    pub fn with_messages(
        mut self,
        channel_id: ChannelId,
        messages: IndexMap<MessageId, Message>,
    ) -> Self {
        self.messages.insert(channel_id, messages);
        self
    }

    /// Returns the number of times fetch_all_messages was called.
    ///
    /// Future integration tests for `ensure_cache_loaded`:
    /// - Cache exists (empty or not) → fetch_count == 0
    /// - Cache miss + tracked → fetch_count == 1
    /// - Cache miss + not tracked → fetch_count == 0
    /// Requires: Serenity Context construction in tests (currently difficult)
    #[allow(dead_code)]
    pub fn fetch_count(&self) -> usize {
        self.fetch_count.load(Ordering::SeqCst)
    }
}

impl Default for MockChannelService {
    fn default() -> Self {
        Self::new()
    }
}

impl ChannelService for MockChannelService {
    #[allow(refining_impl_trait)]
    fn fetch_all_messages(
        &self,
        _ctx: &Context,
        channel_id: ChannelId,
    ) -> Ready<Result<IndexMap<MessageId, Message>, ServiceError>> {
        self.fetch_count.fetch_add(1, Ordering::SeqCst);
        ready(
            self.messages
                .get(&channel_id)
                .cloned()
                .ok_or(ServiceError::Request("Channel not found".into())),
        )
    }
}

/// Mock RosterProvider for testing
#[derive(Clone)]
pub struct MockRosterProvider {
    members: HashMap<GuildId, HashMap<MemberId, MemberInfo>>,
}

impl MockRosterProvider {
    pub fn new() -> Self {
        Self {
            members: HashMap::new(),
        }
    }

    pub fn with_member(mut self, guild_id: GuildId, member: MemberInfo) -> Self {
        self.members
            .entry(guild_id)
            .or_default()
            .insert(member.id, member);
        self
    }
}

impl Default for MockRosterProvider {
    fn default() -> Self {
        Self::new()
    }
}

impl RosterProvider for MockRosterProvider {
    async fn roster_for_channel(
        &self,
        _ctx: &Context,
        _channel_id: ChannelId,
    ) -> Result<Vec<MemberId>, ServiceError> {
        // For testing, return all members from first guild
        Ok(self
            .members
            .values()
            .flat_map(|guild_members| guild_members.keys().copied())
            .collect())
    }

    async fn warm_up(&self, _ctx: &Context, _channel_id: ChannelId) -> Result<(), ServiceError> {
        Ok(())
    }

    fn apply_member_add(&self, _guild_id: GuildId, _member: MemberInfo) {
        // No-op for mock
    }

    fn apply_member_update(&self, _guild_id: GuildId, _member: MemberInfo) {
        // No-op for mock
    }

    fn apply_member_remove(&self, _guild_id: GuildId, _member_id: MemberId) {
        // No-op for mock
    }

    fn display_names_for_guild<I>(
        &self,
        guild_id: GuildId,
        member_ids: I,
    ) -> HashMap<MemberId, String>
    where
        I: IntoIterator<Item = MemberId>,
    {
        let mut result = HashMap::new();
        if let Some(guild_members) = self.members.get(&guild_id) {
            for member_id in member_ids {
                if let Some(member) = guild_members.get(&member_id) {
                    result.insert(member_id, member.effective_name().to_string());
                }
            }
        }
        result
    }
}

/// Helper to create a MemberInfo for testing
pub fn member_info(id: u64, name: &str) -> MemberInfo {
    MemberInfo {
        id: MemberId(id),
        display_name: std::sync::Arc::from(name),
        username: std::sync::Arc::from(name),
        avatar_url: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_names_for_guild_returns_display_name() {
        let guild_id = GuildId::new(1);
        let provider = MockRosterProvider::new().with_member(guild_id, member_info(1, "Alice"));

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(1)]);

        assert_eq!(result.get(&MemberId(1)), Some(&"Alice".to_string()));
    }
}
