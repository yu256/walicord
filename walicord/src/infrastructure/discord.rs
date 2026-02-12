use indexmap::IndexMap;
use serenity::{
    all::MessageId,
    model::{
        channel::{Message, ReactionType},
        guild::Member,
        id::{ChannelId, GuildId},
    },
    prelude::*,
};
use std::sync::Arc;
use walicord_domain::model::{MemberId, MemberInfo};
use walicord_parser::COMMAND_PREFIXES;

#[derive(Debug, thiserror::Error)]
pub enum ChannelError {
    #[error("Failed to fetch channel information: {0}")]
    Request(String),
    #[error("Channel is not a guild channel")]
    NotGuildChannel,
    #[error("Guild is not available in cache")]
    GuildNotCached,
}

pub struct DiscordChannelService;

/// Convert serenity::Member to MemberInfo
pub fn to_member_info(member: &Member) -> MemberInfo {
    let display_name = member
        .nick
        .as_deref()
        .or(member.user.global_name.as_deref())
        .unwrap_or(member.user.name.as_str());

    MemberInfo {
        id: MemberId(member.user.id.get()),
        display_name: Arc::from(display_name),
        username: Arc::from(member.user.name.as_str()),
        avatar_url: member.user.avatar_url().map(|url| Arc::from(url.as_str())),
    }
}

impl DiscordChannelService {
    pub async fn fetch_guild_members(
        &self,
        ctx: &Context,
        guild_id: GuildId,
    ) -> Result<Vec<MemberInfo>, ChannelError> {
        use futures::stream::TryStreamExt;
        let bot_id = ctx.cache.current_user().id;
        guild_id
            .members_iter(&ctx.http)
            .try_filter(|m| std::future::ready(m.user.id != bot_id))
            .map_ok(|m| to_member_info(&m))
            .try_collect()
            .await
            .map_err(|e| ChannelError::Request(format!("{e:?}")))
    }

    pub async fn fetch_all_messages(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> Result<IndexMap<MessageId, Message>, ChannelError> {
        use serenity::builder::GetMessages;

        let mut all_messages = IndexMap::new();
        let mut last_message_id = None;

        fn is_command_prefix(content: &str, cmd: &str) -> bool {
            let is_prefix = if cmd.is_ascii() {
                content
                    .get(..cmd.len())
                    .is_some_and(|head| head.eq_ignore_ascii_case(cmd))
            } else {
                content.starts_with(cmd)
            };

            if !is_prefix {
                return false;
            }

            content
                .get(cmd.len()..)
                .and_then(|rest| rest.chars().next())
                .is_none_or(char::is_whitespace)
        }

        fn should_ignore(message: &Message) -> bool {
            message.author.bot
                || message.reactions.iter().any(|r| {
                    matches!(
                        &r.reaction_type,
                        ReactionType::Unicode(s) if s == "❎" && r.me
                    )
                })
                || COMMAND_PREFIXES
                    .iter()
                    .any(|&cmd| is_command_prefix(&message.content, cmd))
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

#[cfg(test)]
mod tests {
    use super::*;
    use serenity::model::id::UserId;

    fn create_test_member(
        user_id: u64,
        name: &str,
        global_name: Option<&str>,
        nick: Option<&str>,
    ) -> Member {
        let mut member = Member::default();
        member.user.id = UserId::new(user_id);
        member.user.name = name.to_string();
        member.user.global_name = global_name.map(|s| s.to_string());
        member.nick = nick.map(|s| s.to_string());
        member
    }

    #[test]
    fn to_member_info_uses_nick_when_available() {
        let member = create_test_member(1, "username", Some("global"), Some("nick"));
        let info = to_member_info(&member);

        assert_eq!(info.id, MemberId(1));
        assert_eq!(info.effective_name(), "nick");
        assert_eq!(info.username.as_ref(), "username");
    }

    #[test]
    fn to_member_info_uses_global_name_when_no_nick() {
        let member = create_test_member(1, "username", Some("global"), None);
        let info = to_member_info(&member);

        assert_eq!(info.effective_name(), "global");
    }

    #[test]
    fn to_member_info_uses_username_as_fallback() {
        let member = create_test_member(1, "username", None, None);
        let info = to_member_info(&member);

        assert_eq!(info.effective_name(), "username");
    }

    #[test]
    fn to_member_info_creates_arc_str() {
        let member = create_test_member(1, "username", None, Some("nick"));
        let info = to_member_info(&member);

        assert_eq!(info.display_name.as_ref(), "nick");
    }
}
