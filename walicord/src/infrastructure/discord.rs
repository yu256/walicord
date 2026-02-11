use indexmap::IndexMap;
use serenity::{
    all::MessageId,
    model::{
        channel::{Message, ReactionType},
        id::{ChannelId, UserId},
    },
    prelude::*,
};
use std::collections::HashMap;
use walicord_domain::model::MemberId;

#[derive(Debug, thiserror::Error)]
pub enum ChannelError {
    #[error("Failed to fetch channel information: {0}")]
    Request(String),
    #[error("Channel is not a guild channel")]
    NotGuildChannel,
    #[error("Guild is not available in cache")]
    GuildNotCached,
}

const COMMANDS: &[&str] = &["!variables", "!evaluate"];

pub struct DiscordChannelService;

impl DiscordChannelService {
    pub async fn fetch_member_display_names<I>(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
        member_ids: I,
    ) -> Result<HashMap<MemberId, String>, ChannelError>
    where
        I: IntoIterator<Item = MemberId>,
    {
        let channel = channel_id
            .to_channel(&ctx.http)
            .await
            .map_err(|e| ChannelError::Request(format!("{e:?}")))?;

        let Some(guild_channel) = channel.guild() else {
            return Err(ChannelError::NotGuildChannel);
        };

        let Some(guild) = guild_channel.guild(&ctx.cache) else {
            return Err(ChannelError::GuildNotCached);
        };

        let mut members = HashMap::new();
        for member_id in member_ids {
            let user_id = UserId::new(member_id.0);
            let Some(member) = guild.members.get(&user_id) else {
                continue;
            };
            if guild
                .user_permissions_in(&guild_channel, member)
                .view_channel()
            {
                let display_name = member
                    .nick
                    .clone()
                    .unwrap_or_else(|| member.user.name.clone());
                members.insert(member_id, display_name);
            }
        }

        Ok(members)
    }

    pub async fn fetch_channel_member_ids(
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

        let Some(guild) = guild_channel.guild(&ctx.cache) else {
            return Err(ChannelError::GuildNotCached);
        };

        let mut member_ids: Vec<MemberId> = guild
            .members
            .iter()
            .filter_map(|(user_id, member)| {
                guild
                    .user_permissions_in(&guild_channel, member)
                    .view_channel()
                    .then_some(MemberId(user_id.get()))
            })
            .collect();

        member_ids.sort_unstable();
        member_ids.dedup();

        Ok(member_ids)
    }

    pub async fn fetch_all_messages(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> Result<IndexMap<MessageId, Message>, ChannelError> {
        use serenity::builder::GetMessages;

        let mut all_messages = IndexMap::new();
        let mut last_message_id = None;

        fn should_ignore(message: &Message) -> bool {
            message.author.bot
                || message.reactions.iter().any(|r| {
                    matches!(
                        &r.reaction_type,
                        ReactionType::Unicode(s) if s == "❎" && r.me
                    )
                })
                || COMMANDS.iter().any(|&cmd| message.content.starts_with(cmd))
        }

        loop {
            let mut builder = GetMessages::new().limit(100);
            if let Some(before) = last_message_id {
                builder = builder.before(before);
            }

            let messages = channel_id
                .messages(&ctx.http, builder)
                .await
                .map_err(|e| ChannelError::Request(format!("{e:?}")))?;

            if messages.is_empty() {
                break;
            }

            last_message_id = messages.last().map(|m| m.id);
            all_messages.extend(
                messages
                    .into_iter()
                    .filter(|m| !should_ignore(m))
                    .map(|m| (m.id, m)),
            );
        }

        all_messages.reverse();
        Ok(all_messages)
    }
}
