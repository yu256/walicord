use crate::discord::ports::RosterProvider;
use serenity::{
    all::{ChannelId, GuildId},
    async_trait,
    prelude::Context,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};
use walicord_application::ledger::LedgerId;
use walicord_domain::model::{MemberId, RoleId};

use super::{
    participant_resolution::RosterSnapshot,
    router::{
        LedgerThreadLoader, RouterRosterFetchError, RouterRosterFetcher, RouterRosterSnapshot,
    },
    store::{DiscordCanonicalLedgerStore, StoreLoadError, VerifiedLedgerThreadLoad},
};

/// Bridge from the legacy `RosterProvider` (generic, not object-safe) to the
/// router-facing `RouterRosterFetcher` (object-safe). Converts the port's
/// `RosterSnapshot` shape into the participant-resolution shape and pulls per-member
/// display names in a single batch.
pub struct DiscordRouterRosterFetcher<RP: RosterProvider> {
    inner: RP,
}

impl<RP: RosterProvider> DiscordRouterRosterFetcher<RP> {
    pub fn new(inner: RP) -> Self {
        Self { inner }
    }
}

#[async_trait]
impl<RP: RosterProvider> RouterRosterFetcher for DiscordRouterRosterFetcher<RP> {
    async fn fetch(
        &self,
        ctx: &Context,
        guild_id: GuildId,
        channel_id: ChannelId,
    ) -> Result<RouterRosterSnapshot, RouterRosterFetchError> {
        let port_snapshot = self
            .inner
            .roster_for_channel(ctx, channel_id)
            .await
            .map_err(|error| RouterRosterFetchError::Service(error.to_string()))?;

        let display_names = self
            .inner
            .display_names_for_guild(guild_id, port_snapshot.member_ids.iter().copied());

        let all_members: BTreeSet<MemberId> = port_snapshot.member_ids.iter().copied().collect();
        let mut role_members: BTreeMap<RoleId, BTreeSet<MemberId>> = BTreeMap::new();
        for (role_id, members) in port_snapshot.role_members.iter() {
            role_members.insert(*role_id, members.iter().copied().collect());
        }

        Ok(RouterRosterSnapshot {
            roster: RosterSnapshot {
                all_members,
                role_members,
            },
            display_names,
        })
    }
}

/// Object-safe wrapper around `DiscordCanonicalLedgerStore::load_verified_thread`.
/// The router uses an `Arc<dyn LedgerThreadLoader>` so tests can substitute an
/// in-memory snapshot loader without touching Discord I/O.
pub struct DiscordLedgerThreadLoader {
    store: Arc<DiscordCanonicalLedgerStore>,
    route_label: &'static str,
}

impl DiscordLedgerThreadLoader {
    pub fn new(store: Arc<DiscordCanonicalLedgerStore>, route_label: &'static str) -> Self {
        Self { store, route_label }
    }
}

#[async_trait]
impl LedgerThreadLoader for DiscordLedgerThreadLoader {
    async fn load(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
    ) -> Result<VerifiedLedgerThreadLoad, StoreLoadError> {
        self.store
            .load_verified_thread(ctx, canonical_thread_id, ledger_id, self.route_label)
            .await
    }
}
