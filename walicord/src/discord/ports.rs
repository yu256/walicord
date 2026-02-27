use indexmap::IndexMap;
use serenity::{
    all::MessageId,
    model::{channel::Message, id::ChannelId},
    prelude::*,
};
pub use walicord_application::{RoleVisibilityDiagnostic, RoleVisibilityDiagnostics};
use walicord_domain::model::{MemberId, MemberInfo, RoleId, RoleMembers};

/// Error type for Discord service operations
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ServiceError {
    #[error("Failed to fetch data: {0}")]
    Request(String),
    #[error("Channel is not a guild channel")]
    NotGuildChannel,
    #[error("Guild is not available in cache")]
    GuildNotCached,
}

/// Trait for channel-related operations
pub trait ChannelService: Clone + Send + Sync + 'static {
    /// Fetch all messages from a channel
    fn fetch_all_messages(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> impl Future<Output = Result<IndexMap<MessageId, Message>, ServiceError>> + Send;
}

/// Trait for member roster operations
pub trait RosterProvider: Clone + Send + Sync + 'static {
    /// Get roster for a channel
    fn roster_for_channel(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> impl Future<Output = Result<RosterSnapshot, ServiceError>> + Send;

    /// Warm up the roster for a channel
    fn warm_up(
        &self,
        ctx: &Context,
        channel_id: ChannelId,
    ) -> impl Future<Output = Result<(), ServiceError>> + Send;

    /// Apply member addition
    fn apply_member_add(
        &self,
        guild_id: serenity::model::id::GuildId,
        member: MemberInfo,
        role_ids: &[RoleId],
    );

    /// Apply member update
    fn apply_member_update(
        &self,
        guild_id: serenity::model::id::GuildId,
        member: MemberInfo,
        role_ids: &[RoleId],
    );

    /// Apply member removal
    fn apply_member_remove(&self, guild_id: serenity::model::id::GuildId, member_id: MemberId);

    /// Get display names for members in a guild
    fn display_names_for_guild<I>(
        &self,
        guild_id: serenity::model::id::GuildId,
        member_ids: I,
    ) -> std::collections::HashMap<MemberId, smol_str::SmolStr>
    where
        I: IntoIterator<Item = MemberId>;
}

#[derive(Clone, Default)]
pub struct RosterSnapshot {
    pub member_ids: Vec<MemberId>,
    pub role_members: RoleMembers,
    pub role_visibility_diagnostics: RoleVisibilityDiagnostics,
}
