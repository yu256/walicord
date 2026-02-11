use crate::infrastructure::discord::{ChannelError, DiscordChannelService};
use dashmap::{DashMap, DashSet};
use serenity::{
    model::id::{ChannelId, GuildId},
    prelude::*,
};
use std::collections::HashMap;
use walicord_domain::model::{MemberId, MemberInfo};

pub struct MemberRosterProvider {
    channel_service: DiscordChannelService,
    members: DashMap<GuildId, HashMap<MemberId, MemberInfo>>,
    loaded: DashSet<GuildId>,
}

impl MemberRosterProvider {
    pub fn new(channel_service: DiscordChannelService) -> Self {
        Self {
            channel_service,
            members: DashMap::new(),
            loaded: DashSet::new(),
        }
    }

    pub async fn warm_up(&self, ctx: &Context, channel_id: ChannelId) -> Result<(), ChannelError> {
        let channel = channel_id
            .to_channel(&ctx.http)
            .await
            .map_err(|e| ChannelError::Request(format!("{e:?}")))?;
        let Some(guild_channel) = channel.guild() else {
            return Err(ChannelError::NotGuildChannel);
        };

        self.ensure_loaded(ctx, guild_channel.guild_id).await
    }

    pub async fn roster_for_channel(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> Result<Vec<MemberId>, ChannelError> {
        let channel = channel_id
            .to_channel(&ctx.http)
            .await
            .map_err(|e| ChannelError::Request(format!("{e:?}")))?;
        let Some(guild_channel) = channel.guild() else {
            return Err(ChannelError::NotGuildChannel);
        };
        let guild_id = guild_channel.guild_id;

        self.ensure_loaded(ctx, guild_id).await?;

        let Some(members) = self.members.get(&guild_id) else {
            return Ok(Vec::new());
        };

        let guild = guild_channel
            .guild(&ctx.cache)
            .ok_or(ChannelError::GuildNotCached)?;

        let mut member_ids = Vec::new();
        for (member_id, _member_info) in members.iter() {
            let user_id = serenity::model::id::UserId::new(member_id.0);

            if let Some(guild_member) = guild.members.get(&user_id) {
                if guild
                    .user_permissions_in(&guild_channel, guild_member)
                    .view_channel()
                {
                    member_ids.push(*member_id);
                }
            } else {
                member_ids.push(*member_id);
            }
        }

        member_ids.sort_unstable();
        member_ids.dedup();

        Ok(member_ids)
    }

    pub fn apply_member_add(&self, guild_id: GuildId, member: MemberInfo) {
        if let Some(mut members) = self.members.get_mut(&guild_id) {
            members.insert(member.id, member);
        }
    }

    pub fn apply_member_update(&self, guild_id: GuildId, member: MemberInfo) {
        if let Some(mut members) = self.members.get_mut(&guild_id) {
            members.insert(member.id, member);
        }
    }

    pub fn apply_member_remove(&self, guild_id: GuildId, member_id: MemberId) {
        if let Some(mut members) = self.members.get_mut(&guild_id) {
            members.remove(&member_id);
        }
    }

    async fn ensure_loaded(&self, ctx: &Context, guild_id: GuildId) -> Result<(), ChannelError> {
        if self.loaded.contains(&guild_id) {
            return Ok(());
        }

        let members = self
            .channel_service
            .fetch_guild_members(ctx, guild_id)
            .await?;
        let mut map = HashMap::with_capacity(members.len());
        for member in members {
            map.insert(member.id, member);
        }
        self.members.insert(guild_id, map);
        self.loaded.insert(guild_id);
        Ok(())
    }

    pub fn display_names_for_guild<I>(
        &self,
        guild_id: GuildId,
        member_ids: I,
    ) -> HashMap<MemberId, String>
    where
        I: IntoIterator<Item = MemberId>,
    {
        let Some(members) = self.members.get(&guild_id) else {
            return HashMap::new();
        };

        let mut result = HashMap::new();
        for member_id in member_ids {
            if let Some(member) = members.get(&member_id) {
                result.insert(member_id, member.effective_name().to_string());
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn create_test_member_info(user_id: u64, name: &str, nick: Option<&str>) -> MemberInfo {
        let display_name = nick.unwrap_or(name);
        MemberInfo {
            id: MemberId(user_id),
            display_name: Arc::from(display_name),
            username: Arc::from(name),
            avatar_url: None,
        }
    }

    fn setup_provider_with_members(
        guild_id: GuildId,
        members: Vec<MemberInfo>,
    ) -> MemberRosterProvider {
        let provider = MemberRosterProvider::new(DiscordChannelService);
        let mut map = HashMap::with_capacity(members.len());
        for member in members {
            map.insert(member.id, member);
        }
        provider.members.insert(guild_id, map);
        provider.loaded.insert(guild_id);
        provider
    }

    #[test]
    fn display_names_for_guild_returns_cached_names() {
        let guild_id = GuildId::new(1);
        let members = vec![
            create_test_member_info(1, "Alice", Some("Ali")),
            create_test_member_info(2, "Bob", None),
        ];
        let provider = setup_provider_with_members(guild_id, members);

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(1), MemberId(2)]);

        assert_eq!(result.get(&MemberId(1)), Some(&"Ali".to_string()));
        assert_eq!(result.get(&MemberId(2)), Some(&"Bob".to_string()));
    }

    #[test]
    fn display_names_for_guild_skips_missing_members() {
        let guild_id = GuildId::new(1);
        let members = vec![create_test_member_info(1, "Alice", None)];
        let provider = setup_provider_with_members(guild_id, members);

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(1), MemberId(2)]);

        assert_eq!(result.len(), 1);
        assert_eq!(result.get(&MemberId(1)), Some(&"Alice".to_string()));
    }

    #[test]
    fn display_names_for_guild_returns_empty_when_not_loaded() {
        let guild_id = GuildId::new(1);
        let provider = MemberRosterProvider::new(DiscordChannelService);

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(1)]);

        assert!(result.is_empty());
    }

    #[test]
    fn apply_member_add_inserts_member_to_existing_guild() {
        let guild_id = GuildId::new(1);
        let members = vec![create_test_member_info(1, "Alice", None)];
        let provider = setup_provider_with_members(guild_id, members);

        let new_member = create_test_member_info(2, "Bob", Some("Bobby"));
        provider.apply_member_add(guild_id, new_member);

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(2)]);
        assert_eq!(result.get(&MemberId(2)), Some(&"Bobby".to_string()));
    }

    #[test]
    fn apply_member_add_ignores_new_guild() {
        let guild_id = GuildId::new(1);
        let provider = MemberRosterProvider::new(DiscordChannelService);

        let new_member = create_test_member_info(2, "Bob", Some("Bobby"));
        provider.apply_member_add(guild_id, new_member);

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(2)]);
        assert!(result.is_empty());
    }

    #[test]
    fn apply_member_update_modifies_existing() {
        let guild_id = GuildId::new(1);
        let member = create_test_member_info(1, "OldName", Some("OldNick"));
        let provider = setup_provider_with_members(guild_id, vec![member]);

        let updated_member = create_test_member_info(1, "NewName", Some("NewNick"));
        provider.apply_member_update(guild_id, updated_member);

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(1)]);
        assert_eq!(result.get(&MemberId(1)), Some(&"NewNick".to_string()));
    }

    #[test]
    fn apply_member_remove_deletes_member() {
        let guild_id = GuildId::new(1);
        let members = vec![
            create_test_member_info(1, "Alice", None),
            create_test_member_info(2, "Bob", None),
        ];
        let provider = setup_provider_with_members(guild_id, members);

        provider.apply_member_remove(guild_id, MemberId(1));

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(1)]);
        assert!(!result.contains_key(&MemberId(1)));
    }

    #[test]
    fn apply_member_remove_nonexistent_is_noop() {
        let guild_id = GuildId::new(1);
        let members = vec![create_test_member_info(2, "Bob", None)];
        let provider = setup_provider_with_members(guild_id, members);

        provider.apply_member_remove(guild_id, MemberId(1));

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(2)]);
        assert!(result.contains_key(&MemberId(2)));
    }
}
