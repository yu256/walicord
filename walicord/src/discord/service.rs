use super::ports::ServiceError;
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
use walicord_application::{is_command_message, is_command_prefix};
use walicord_domain::model::{MemberId, MemberInfo};

#[derive(Debug, thiserror::Error)]
pub enum ChannelError {
    #[error("Failed to fetch channel information: {0}")]
    Request(String),
    #[error("Channel is not a guild channel")]
    NotGuildChannel,
    #[error("Guild is not available in cache")]
    GuildNotCached,
}

impl From<ChannelError> for ServiceError {
    fn from(err: ChannelError) -> Self {
        match err {
            ChannelError::Request(msg) => ServiceError::Request(msg),
            ChannelError::NotGuildChannel => ServiceError::NotGuildChannel,
            ChannelError::GuildNotCached => ServiceError::GuildNotCached,
        }
    }
}

#[derive(Clone, Copy)]
pub struct DiscordChannelService;

fn should_ignore_history_message(message: &Message) -> bool {
    let command_without_state_effect = is_command_message(&message.content)
        && !is_command_prefix(&message.content, "!member")
        && !is_command_prefix(&message.content, "!cash");
    message.author.bot
        || message
            .reactions
            .iter()
            .any(|r| matches!(&r.reaction_type, ReactionType::Unicode(s) if s == "❎" && r.me))
        || command_without_state_effect
}

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
                    .filter(|m| !should_ignore_history_message(m))
                    .map(|m| (m.id, m)),
            );
        }

        all_messages.reverse();
        Ok(all_messages)
    }
}

impl super::ports::ChannelService for DiscordChannelService {
    async fn fetch_all_messages(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> Result<IndexMap<MessageId, Message>, super::ports::ServiceError> {
        DiscordChannelService::fetch_all_messages(self, ctx, channel_id)
            .await
            .map_err(ServiceError::from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
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

    #[rstest]
    #[case::nick(Some("global"), Some("nick"), "nick")]
    #[case::global(Some("global"), None, "global")]
    #[case::username(None, None, "username")]
    fn to_member_info_picks_effective_name(
        #[case] global_name: Option<&str>,
        #[case] nick: Option<&str>,
        #[case] expected: &str,
    ) {
        let member = create_test_member(1, "username", global_name, nick);
        let info = to_member_info(&member);

        assert_eq!(info.id, MemberId(1));
        assert_eq!(info.effective_name(), expected);
        assert_eq!(info.username.as_ref(), "username");
    }

    #[rstest]
    #[case::display_name_from_nick(Some("global"), Some("nick"), "nick")]
    #[case::display_name_from_global(Some("global"), None, "global")]
    #[case::display_name_from_username(None, None, "username")]
    fn to_member_info_sets_display_name(
        #[case] global_name: Option<&str>,
        #[case] nick: Option<&str>,
        #[case] expected: &str,
    ) {
        let member = create_test_member(1, "username", global_name, nick);
        let info = to_member_info(&member);

        assert_eq!(info.display_name.as_ref(), expected);
    }

    #[rstest]
    #[case::member_cash_command_kept("!member set <@1> cash", false)]
    #[case::cash_command_kept("!cash", false)]
    #[case::settleup_command_ignored("!settleup <@1>", true)]
    fn history_filter_keeps_stateful_member_commands(
        #[case] content: &str,
        #[case] expected_ignored: bool,
    ) {
        let mut message = Message::default();
        message.content = content.to_string();
        message.author.bot = false;

        assert_eq!(should_ignore_history_message(&message), expected_ignored);
    }
}
