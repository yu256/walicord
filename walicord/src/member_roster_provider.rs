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
}
