use indexmap::IndexMap;
use serenity::{
    all::MessageId,
    model::{
        channel::{Message, ReactionType},
        id::ChannelId,
    },
    prelude::*,
};
use std::collections::HashMap;
use walicord_domain::model::MemberId;

#[derive(Debug, thiserror::Error)]
pub enum ChannelError {
    #[error("Failed to fetch channel information: {0}")]
    Request(String),
}

const COMMANDS: &[&str] = &["!variables", "!evaluate"];

pub struct DiscordChannelService;

impl DiscordChannelService {
    pub async fn fetch_channel_member_display_names(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> Result<HashMap<MemberId, String>, ChannelError> {
        let channel = channel_id
            .to_channel(&ctx.http)
            .await
            .map_err(|e| ChannelError::Request(format!("{e:?}")))?;

        let Some(guild_channel) = channel.guild() else {
            return Ok(HashMap::new());
        };

        let Some(guild) = guild_channel.guild(&ctx.cache) else {
            return Ok(HashMap::new());
        };

        let mut members = HashMap::new();
        for (user_id, member) in &guild.members {
            if guild
                .user_permissions_in(&guild_channel, member)
                .view_channel()
            {
                let display_name = member
                    .nick
                    .clone()
                    .unwrap_or_else(|| member.user.name.clone());
                members.insert(MemberId(user_id.get()), display_name);
            }
        }

        Ok(members)
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
