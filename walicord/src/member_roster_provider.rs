use crate::infrastructure::discord::{ChannelError, DiscordChannelService};
use dashmap::{DashMap, DashSet};
use serenity::{
    model::{
        guild::Member,
        id::{ChannelId, GuildId, UserId},
    },
    prelude::*,
};
use std::collections::HashMap;
use walicord_domain::model::MemberId;

pub struct MemberRosterProvider {
    channel_service: DiscordChannelService,
    members: DashMap<GuildId, HashMap<UserId, Member>>,
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

        if let Some(guild) = guild_channel.guild(&ctx.cache) {
            return Ok(self.member_ids_for_channel(&guild_id, |member| {
                guild
                    .user_permissions_in(&guild_channel, member)
                    .view_channel()
            }));
        }

        let guild = guild_id
            .to_partial_guild(&ctx.http)
            .await
            .map_err(|e| ChannelError::Request(format!("{e:?}")))?;
        Ok(self.member_ids_for_channel(&guild_id, |member| {
            guild
                .user_permissions_in(&guild_channel, member)
                .view_channel()
        }))
    }

    pub fn apply_member_add(&self, member: Member) {
        self.upsert_member(member);
    }

    pub fn apply_member_update(&self, member: Member) {
        self.upsert_member(member);
    }

    pub fn apply_member_remove(&self, guild_id: GuildId, user_id: UserId) {
        if let Some(mut members) = self.members.get_mut(&guild_id) {
            members.remove(&user_id);
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
            map.insert(member.user.id, member);
        }
        self.members.insert(guild_id, map);
        self.loaded.insert(guild_id);
        Ok(())
    }

    fn upsert_member(&self, member: Member) {
        let guild_id = member.guild_id;
        let user_id = member.user.id;
        if let Some(mut members) = self.members.get_mut(&guild_id) {
            members.insert(user_id, member);
        } else {
            self.members
                .insert(guild_id, HashMap::from([(user_id, member)]));
        }
    }

    fn member_ids_for_channel<F>(&self, guild_id: &GuildId, allowed: F) -> Vec<MemberId>
    where
        F: Fn(&Member) -> bool,
    {
        let Some(members) = self.members.get(guild_id) else {
            return Vec::new();
        };

        let mut member_ids = Vec::with_capacity(members.len());
        for member in members.values() {
            if allowed(member) {
                member_ids.push(MemberId(member.user.id.get()));
            }
        }

        member_ids.sort_unstable();
        member_ids.dedup();
        member_ids
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
            let user_id = UserId::new(member_id.0);
            if let Some(member) = members.get(&user_id) {
                let display_name = member
                    .nick
                    .clone()
                    .or_else(|| member.user.global_name.clone())
                    .unwrap_or_else(|| member.user.name.clone());
                result.insert(member_id, display_name);
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    fn create_test_member(user_id: u64, guild_id: u64, name: &str, nick: Option<&str>) -> Member {
        let mut member = Member::default();
        member.user.id = UserId::new(user_id);
        member.user.name = name.to_string();
        member.guild_id = GuildId::new(guild_id);
        if let Some(n) = nick {
            member.nick = Some(n.to_string());
        }
        member
    }

    fn setup_provider_with_members(
        guild_id: GuildId,
        members: Vec<Member>,
    ) -> MemberRosterProvider {
        let provider = MemberRosterProvider::new(DiscordChannelService);
        let mut map = HashMap::with_capacity(members.len());
        for member in &members {
            map.insert(member.user.id, member.clone());
        }
        provider.members.insert(guild_id, map);
        provider.loaded.insert(guild_id);
        provider
    }

    #[rstest]
    #[case::with_nickname(
        vec![(1, "Alice", Some("Ali"))],
        vec![MemberId(1)],
        vec![(MemberId(1), "Ali")]
    )]
    #[case::without_nickname(
        vec![(1, "Bob", None)],
        vec![MemberId(1)],
        vec![(MemberId(1), "Bob")]
    )]
    #[case::mixed_names(
        vec![(1, "Alice", Some("Ali")), (2, "Bob", None)],
        vec![MemberId(1), MemberId(2)],
        vec![(MemberId(1), "Ali"), (MemberId(2), "Bob")]
    )]
    fn display_names_for_guild_returns_cached_names(
        #[case] members_data: Vec<(u64, &str, Option<&str>)>,
        #[case] requested_ids: Vec<MemberId>,
        #[case] expected: Vec<(MemberId, &str)>,
    ) {
        let guild_id = GuildId::new(1);
        let members: Vec<Member> = members_data
            .into_iter()
            .map(|(id, name, nick)| create_test_member(id, 1, name, nick))
            .collect();
        let provider = setup_provider_with_members(guild_id, members);

        let result = provider.display_names_for_guild(guild_id, requested_ids);

        assert_eq!(result.len(), expected.len());
        for (id, name) in expected {
            assert_eq!(result.get(&id), Some(&name.to_string()));
        }
    }

    #[rstest]
    #[case::partial_match(
        vec![(1, "Alice", None), (2, "Bob", None)],
        vec![MemberId(1), MemberId(3)],
        vec![(MemberId(1), "Alice")]
    )]
    #[case::no_match(
        vec![(1, "Alice", None)],
        vec![MemberId(2)],
        vec![]
    )]
    fn display_names_for_guild_skips_missing_members(
        #[case] members_data: Vec<(u64, &str, Option<&str>)>,
        #[case] requested_ids: Vec<MemberId>,
        #[case] expected: Vec<(MemberId, &str)>,
    ) {
        let guild_id = GuildId::new(1);
        let members: Vec<Member> = members_data
            .into_iter()
            .map(|(id, name, nick)| create_test_member(id, 1, name, nick))
            .collect();
        let provider = setup_provider_with_members(guild_id, members);

        let result = provider.display_names_for_guild(guild_id, requested_ids);

        assert_eq!(result.len(), expected.len());
        for (id, name) in expected {
            assert_eq!(result.get(&id), Some(&name.to_string()));
        }
    }

    #[test]
    fn display_names_for_guild_returns_empty_when_not_loaded() {
        let guild_id = GuildId::new(1);
        let provider = MemberRosterProvider::new(DiscordChannelService);

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(1)]);

        assert!(result.is_empty());
    }

    #[rstest]
    #[case::add_to_existing(1, vec![(1, "Alice", None)])]
    #[case::add_to_empty(1, vec![])]
    fn apply_member_add_inserts_member(
        #[case] new_member_id: u64,
        #[case] existing_members: Vec<(u64, &str, Option<&str>)>,
    ) {
        let guild_id = GuildId::new(1);
        let members: Vec<Member> = existing_members
            .into_iter()
            .map(|(id, name, nick)| create_test_member(id, 1, name, nick))
            .collect();
        let provider = setup_provider_with_members(guild_id, members);

        let new_member = create_test_member(new_member_id, 1, "NewUser", Some("NewNick"));
        provider.apply_member_add(new_member);

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(new_member_id)]);
        assert_eq!(
            result.get(&MemberId(new_member_id)),
            Some(&"NewNick".to_string())
        );
    }

    #[test]
    fn apply_member_update_modifies_existing() {
        let guild_id = GuildId::new(1);
        let member = create_test_member(1, 1, "OldName", Some("OldNick"));
        let provider = setup_provider_with_members(guild_id, vec![member]);

        let mut updated_member = create_test_member(1, 1, "NewName", Some("NewNick"));
        updated_member.guild_id = guild_id;
        provider.apply_member_update(updated_member);

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(1)]);
        assert_eq!(result.get(&MemberId(1)), Some(&"NewNick".to_string()));
    }

    #[rstest]
    #[case::remove_existing(1, vec![(1, "Alice", None), (2, "Bob", None)], 1, true)]
    #[case::remove_nonexistent(1, vec![(2, "Bob", None)], 1, true)]
    fn apply_member_remove_deletes_member(
        #[case] remove_id: u64,
        #[case] members_data: Vec<(u64, &str, Option<&str>)>,
        #[case] check_id: u64,
        #[case] should_be_missing: bool,
    ) {
        let guild_id = GuildId::new(1);
        let members: Vec<Member> = members_data
            .into_iter()
            .map(|(id, name, nick)| create_test_member(id, 1, name, nick))
            .collect();
        let provider = setup_provider_with_members(guild_id, members);

        provider.apply_member_remove(guild_id, UserId::new(remove_id));

        let result = provider.display_names_for_guild(guild_id, vec![MemberId(check_id)]);
        assert_eq!(!result.contains_key(&MemberId(check_id)), should_be_missing);
    }
}
