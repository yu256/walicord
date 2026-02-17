//! Mock implementations for testing

use crate::discord::ports::{RosterProvider, ServiceError};
use serenity::{
    model::id::{ChannelId, GuildId},
    prelude::*,
};
use std::collections::HashMap;
use walicord_domain::model::{MemberId, MemberInfo};

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
