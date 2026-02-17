use serenity::{
    model::channel::{Message, ReactionType},
    prelude::*,
};

/// State of a reaction on a message
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReactionState {
    Clear,
    Valid,
    Invalid,
}

/// Service for managing message reactions
pub struct ReactionService;

impl ReactionService {
    /// React to a message with an emoji
    pub async fn react(ctx: &Context, msg: &Message, emoji: char) {
        if let Err(e) = msg.react(ctx, emoji).await {
            tracing::error!("Failed to add reaction: {:?}", e);
        }
    }

    /// Delete a specific reaction from a message
    pub async fn delete_reaction(ctx: &Context, msg: &Message, emoji: char) {
        if let Err(e) = msg
            .delete_reaction(&ctx.http, None, ReactionType::Unicode(emoji.to_string()))
            .await
        {
            tracing::warn!("Failed to remove reaction: {:?}", e);
        }
    }

    /// Update reaction state to desired state
    pub async fn update_state(ctx: &Context, msg: &Message, desired: ReactionState) {
        match desired {
            ReactionState::Valid => {
                Self::delete_reaction(ctx, msg, '❎').await;
                Self::react(ctx, msg, '✅').await;
            }
            ReactionState::Invalid => {
                Self::delete_reaction(ctx, msg, '✅').await;
                Self::react(ctx, msg, '❎').await;
            }
            ReactionState::Clear => {
                Self::delete_reaction(ctx, msg, '✅').await;
                Self::delete_reaction(ctx, msg, '❎').await;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reaction_state_equality() {
        assert_eq!(ReactionState::Valid, ReactionState::Valid);
        assert_eq!(ReactionState::Invalid, ReactionState::Invalid);
        assert_eq!(ReactionState::Clear, ReactionState::Clear);
        assert_ne!(ReactionState::Valid, ReactionState::Invalid);
    }
}
