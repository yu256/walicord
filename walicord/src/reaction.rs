use serenity::{
    model::channel::{Message, ReactionType},
    prelude::*,
};
use smallvec::SmallVec;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MessageValidity {
    Valid,
    Invalid,
    NotProgram,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BotReactionState {
    None,
    HasCheck,
    HasCross,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReactionOp {
    DeleteCheck,
    DeleteCross,
    AddCheck,
    AddCross,
}

pub struct BotReaction;

impl BotReaction {
    pub const CHECK: &'static str = "✅";
    pub const CROSS: &'static str = "❎";

    pub fn state_from_validity(validity: MessageValidity) -> BotReactionState {
        match validity {
            MessageValidity::Valid => BotReactionState::HasCheck,
            MessageValidity::Invalid => BotReactionState::HasCross,
            MessageValidity::NotProgram => BotReactionState::None,
        }
    }
}

/// Compute the reaction operations needed to transition from current to desired state.
///
/// This is a pure function for easy testing of the diff logic.
///
/// # Operation Order (Policy)
/// Operations are returned in a specific order to ensure predictable state transitions:
/// 1. Delete unnecessary reactions first (DeleteCheck, DeleteCross)
/// 2. Add necessary reactions second (AddCheck, AddCross)
///
/// This order ensures that:
/// - Discord UI shows clean transitions (remove old before adding new)
/// - BotReactionState is exclusive (None/HasCheck/HasCross), so at most one add is needed
///
/// Future extensions (e.g., additional reaction types) should maintain this delete-then-add order.
pub fn reaction_ops(
    current: BotReactionState,
    desired: MessageValidity,
) -> SmallVec<[ReactionOp; 2]> {
    let desired_state = BotReaction::state_from_validity(desired);
    if current == desired_state {
        return SmallVec::new();
    }

    let has_check = matches!(current, BotReactionState::HasCheck);
    let has_cross = matches!(current, BotReactionState::HasCross);
    let needs_check = matches!(desired_state, BotReactionState::HasCheck);
    let needs_cross = matches!(desired_state, BotReactionState::HasCross);

    let mut ops = SmallVec::<[ReactionOp; 2]>::new();

    if has_check && !needs_check {
        ops.push(ReactionOp::DeleteCheck);
    }
    if has_cross && !needs_cross {
        ops.push(ReactionOp::DeleteCross);
    }
    if needs_check && !has_check {
        ops.push(ReactionOp::AddCheck);
    }
    if needs_cross && !has_cross {
        ops.push(ReactionOp::AddCross);
    }

    ops
}

pub struct ReactionService;

impl ReactionService {
    async fn delete_reaction_by_id(
        ctx: &Context,
        channel_id: serenity::model::id::ChannelId,
        message_id: serenity::all::MessageId,
        emoji: &str,
    ) {
        if let Err(e) = channel_id
            .delete_reaction(
                &ctx.http,
                message_id,
                None,
                ReactionType::Unicode(emoji.to_string()),
            )
            .await
        {
            tracing::warn!(
                "Failed to remove reaction {} from message {} in {}: {:?}",
                emoji,
                message_id,
                channel_id,
                e
            );
        }
    }

    async fn create_reaction_by_id(
        ctx: &Context,
        channel_id: serenity::model::id::ChannelId,
        message_id: serenity::all::MessageId,
        emoji: &str,
    ) {
        if let Err(e) = channel_id
            .create_reaction(
                &ctx.http,
                message_id,
                ReactionType::Unicode(emoji.to_string()),
            )
            .await
        {
            tracing::error!(
                "Failed to add reaction {} to message {} in {}: {:?}",
                emoji,
                message_id,
                channel_id,
                e
            );
        }
    }

    pub async fn react(ctx: &Context, msg: &Message, emoji: char) {
        if let Err(e) = msg.react(ctx, emoji).await {
            tracing::error!("Failed to add reaction: {:?}", e);
        }
    }

    pub async fn delete_reaction(ctx: &Context, msg: &Message, emoji: char) {
        if let Err(e) = msg
            .delete_reaction(&ctx.http, None, ReactionType::Unicode(emoji.to_string()))
            .await
        {
            tracing::warn!("Failed to remove reaction: {:?}", e);
        }
    }

    pub async fn update_state(ctx: &Context, msg: &Message, desired: MessageValidity) {
        match desired {
            MessageValidity::Valid => {
                Self::delete_reaction(ctx, msg, '❎').await;
                Self::react(ctx, msg, '✅').await;
            }
            MessageValidity::Invalid => {
                Self::delete_reaction(ctx, msg, '✅').await;
                Self::react(ctx, msg, '❎').await;
            }
            MessageValidity::NotProgram => {
                Self::delete_reaction(ctx, msg, '✅').await;
                Self::delete_reaction(ctx, msg, '❎').await;
            }
        }
    }

    /// Update reaction state with diff-based optimization.
    ///
    /// Skips API calls when current state matches desired state. This assumes `current`
    /// accurately reflects Discord's actual state. Cache divergence can occur due to:
    /// - Missed reaction_add/remove events (gateway reconnect, partial intents)
    /// - External reaction changes (manual deletion, moderation)
    ///
    /// Reconciliation occurs during full fetch via `CachedMessage::from_message`
    /// (triggered by ensure_cache_loaded on cache miss).
    pub async fn update_state_diff(
        ctx: &Context,
        channel_id: serenity::model::id::ChannelId,
        message_id: serenity::all::MessageId,
        current: BotReactionState,
        desired: MessageValidity,
    ) {
        for op in reaction_ops(current, desired) {
            match op {
                ReactionOp::DeleteCheck => {
                    Self::delete_reaction_by_id(ctx, channel_id, message_id, BotReaction::CHECK)
                        .await;
                }
                ReactionOp::DeleteCross => {
                    Self::delete_reaction_by_id(ctx, channel_id, message_id, BotReaction::CROSS)
                        .await;
                }
                ReactionOp::AddCheck => {
                    Self::create_reaction_by_id(ctx, channel_id, message_id, BotReaction::CHECK)
                        .await;
                }
                ReactionOp::AddCross => {
                    Self::create_reaction_by_id(ctx, channel_id, message_id, BotReaction::CROSS)
                        .await;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn message_validity_equality() {
        assert_eq!(MessageValidity::Valid, MessageValidity::Valid);
        assert_eq!(MessageValidity::Invalid, MessageValidity::Invalid);
        assert_eq!(MessageValidity::NotProgram, MessageValidity::NotProgram);
        assert_ne!(MessageValidity::Valid, MessageValidity::Invalid);
    }

    #[test]
    fn bot_reaction_state_from_validity() {
        assert_eq!(
            BotReaction::state_from_validity(MessageValidity::Valid),
            BotReactionState::HasCheck
        );
        assert_eq!(
            BotReaction::state_from_validity(MessageValidity::Invalid),
            BotReactionState::HasCross
        );
        assert_eq!(
            BotReaction::state_from_validity(MessageValidity::NotProgram),
            BotReactionState::None
        );
    }

    #[rstest::rstest]
    #[case::none_to_valid(BotReactionState::None, MessageValidity::Valid, true)]
    #[case::none_to_invalid(BotReactionState::None, MessageValidity::Invalid, true)]
    #[case::none_to_not_program(BotReactionState::None, MessageValidity::NotProgram, false)]
    #[case::has_check_to_valid(BotReactionState::HasCheck, MessageValidity::Valid, false)]
    #[case::has_check_to_invalid(BotReactionState::HasCheck, MessageValidity::Invalid, true)]
    #[case::has_check_to_not_program(BotReactionState::HasCheck, MessageValidity::NotProgram, true)]
    #[case::has_cross_to_valid(BotReactionState::HasCross, MessageValidity::Valid, true)]
    #[case::has_cross_to_invalid(BotReactionState::HasCross, MessageValidity::Invalid, false)]
    #[case::has_cross_to_not_program(BotReactionState::HasCross, MessageValidity::NotProgram, true)]
    fn update_state_diff_transitions(
        #[case] current: BotReactionState,
        #[case] desired: MessageValidity,
        #[case] expects_change: bool,
    ) {
        let desired_state = BotReaction::state_from_validity(desired);
        let should_change = current != desired_state;
        assert_eq!(should_change, expects_change);
    }

    #[rstest::rstest]
    #[case::none_to_valid(
        BotReactionState::None,
        MessageValidity::Valid,
        &[ReactionOp::AddCheck]
    )]
    #[case::none_to_invalid(
        BotReactionState::None,
        MessageValidity::Invalid,
        &[ReactionOp::AddCross]
    )]
    #[case::none_to_not_program(
        BotReactionState::None,
        MessageValidity::NotProgram,
        &[]
    )]
    #[case::has_check_to_valid(
        BotReactionState::HasCheck,
        MessageValidity::Valid,
        &[]
    )]
    #[case::has_check_to_invalid(
        BotReactionState::HasCheck,
        MessageValidity::Invalid,
        &[ReactionOp::DeleteCheck, ReactionOp::AddCross]
    )]
    #[case::has_check_to_not_program(
        BotReactionState::HasCheck,
        MessageValidity::NotProgram,
        &[ReactionOp::DeleteCheck]
    )]
    #[case::has_cross_to_valid(
        BotReactionState::HasCross,
        MessageValidity::Valid,
        &[ReactionOp::DeleteCross, ReactionOp::AddCheck]
    )]
    #[case::has_cross_to_invalid(
        BotReactionState::HasCross,
        MessageValidity::Invalid,
        &[]
    )]
    #[case::has_cross_to_not_program(
        BotReactionState::HasCross,
        MessageValidity::NotProgram,
        &[ReactionOp::DeleteCross]
    )]
    fn reaction_ops_returns_correct_operations(
        #[case] current: BotReactionState,
        #[case] desired: MessageValidity,
        #[case] expected_ops: &[ReactionOp],
    ) {
        assert_eq!(reaction_ops(current, desired).as_slice(), expected_ops);
    }
}
