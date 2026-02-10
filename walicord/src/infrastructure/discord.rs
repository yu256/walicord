use indexmap::IndexMap;
use serenity::{
    all::MessageId,
    model::{
        channel::{Message, ReactionType},
        id::ChannelId,
    },
    prelude::*,
};

#[derive(Debug, thiserror::Error)]
pub enum ChannelError {
    #[error("Failed to fetch channel information: {0}")]
    Request(String),
}

const COMMANDS: &[&str] = &["!variables", "!evaluate"];

pub struct DiscordChannelService;

impl DiscordChannelService {
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
