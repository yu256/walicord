use crate::discord::ports::RosterProvider;
use serenity::{
    all::{ChannelId, ChannelType, GuildId},
    async_trait,
    http::Http,
    prelude::Context,
};
use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    sync::Arc,
};
use walicord_application::ledger::{LedgerId, participant_resolution::RosterSnapshot};
use walicord_domain::model::{MemberId, RoleId};

use super::{
    locator::{
        CanonicalThreadBinding, CanonicalThreadDiscoveryPort, CanonicalThreadLocator,
        EmptyCanonicalThreadCandidate, LocatorDiscoveryCandidate, LocatorError,
        LocatorRecoveryReference, RediscoveringCanonicalThreadLocatorBackend, TrackedParentKey,
        VerifiedCanonicalThreadCandidate, canonical_thread_candidate_ids,
    },
    observability::PermissionAction,
    router::{
        LedgerCanonicalThreadCreator, LedgerThreadLoader, RouterRosterFetchError,
        RouterRosterFetcher, RouterRosterSnapshot,
    },
    store::{
        DiscordCanonicalLedgerStore, StoreLoadError, VerifiedLedgerThreadLoad,
        serenity_error_is_read_denied,
    },
};

pub struct DiscordLedgerCanonicalThreadCreator;

#[async_trait]
impl LedgerCanonicalThreadCreator for DiscordLedgerCanonicalThreadCreator {
    async fn create(
        &self,
        ctx: &Context,
        tracked_parent_channel_id: ChannelId,
    ) -> Result<ChannelId, serenity::Error> {
        use serenity::builder::CreateThread;

        tracked_parent_channel_id
            .create_thread(
                &ctx.http,
                CreateThread::new(super::locator::CANONICAL_LEDGER_THREAD_NAME)
                    .kind(ChannelType::PublicThread),
            )
            .await
            .map(|thread| thread.id)
    }
}

pub(crate) type DiscordCanonicalThreadLocator = CanonicalThreadLocator<
    RediscoveringCanonicalThreadLocatorBackend<DiscordCanonicalThreadDiscovery>,
>;

pub(crate) fn discord_canonical_thread_locator(
    store: Arc<DiscordCanonicalLedgerStore>,
    http: Arc<Http>,
) -> DiscordCanonicalThreadLocator {
    CanonicalThreadLocator::new(RediscoveringCanonicalThreadLocatorBackend::new(
        DiscordCanonicalThreadDiscovery::new(store, http),
    ))
}

#[derive(Clone)]
pub(crate) struct DiscordCanonicalThreadDiscovery {
    store: Arc<DiscordCanonicalLedgerStore>,
    http: Arc<Http>,
}

impl DiscordCanonicalThreadDiscovery {
    pub(crate) fn new(store: Arc<DiscordCanonicalLedgerStore>, http: Arc<Http>) -> Self {
        Self { store, http }
    }
}

#[async_trait]
impl CanonicalThreadDiscoveryPort for DiscordCanonicalThreadDiscovery {
    async fn candidate_thread_ids(
        &self,
        tracked_parent: TrackedParentKey,
    ) -> Result<Vec<ChannelId>, LocatorError> {
        let guild_id = tracked_parent.guild_id();
        let parent_channel_id = tracked_parent.tracked_parent_channel_id();
        let mut seen = HashSet::new();
        let mut candidates = Vec::new();
        let active_threads = guild_id
            .get_active_threads(&self.http)
            .await
            .map_err(|error| self.classify_discovery_error(tracked_parent, error))?;
        for thread_id in canonical_thread_candidate_ids(
            parent_channel_id,
            active_threads
                .threads
                .into_iter()
                .map(|thread| (thread.id, thread.kind, thread.name, thread.parent_id)),
        ) {
            if seen.insert(thread_id) {
                candidates.push(thread_id);
            }
        }

        let mut before = None;
        loop {
            let archived_threads = parent_channel_id
                .get_archived_public_threads(&self.http, before, Some(100))
                .await
                .map_err(|error| self.classify_discovery_error(tracked_parent, error))?;
            let next_before = archived_threads
                .threads
                .last()
                .map(|thread| thread.id.get());
            for thread_id in canonical_thread_candidate_ids(
                parent_channel_id,
                archived_threads
                    .threads
                    .into_iter()
                    .map(|thread| (thread.id, thread.kind, thread.name, thread.parent_id)),
            ) {
                if seen.insert(thread_id) {
                    candidates.push(thread_id);
                }
            }
            if !archived_threads.has_more || next_before.is_none() {
                break;
            }
            before = next_before;
        }
        Ok(candidates)
    }

    async fn classify_candidate(
        &self,
        tracked_parent: TrackedParentKey,
        canonical_thread_id: ChannelId,
        cached_binding: Option<CanonicalThreadBinding>,
    ) -> Result<Option<LocatorDiscoveryCandidate>, LocatorError> {
        let discovered = self
            .store
            .load_verified_thread_discovering_id(&self.http, canonical_thread_id)
            .await
            .map(|discovered| discovered.map(|(ledger_id, _)| ledger_id));
        classify_discovered_candidate(
            tracked_parent,
            canonical_thread_id,
            cached_binding,
            discovered,
        )
    }
}

impl DiscordCanonicalThreadDiscovery {
    fn classify_discovery_error(
        &self,
        tracked_parent: TrackedParentKey,
        error: serenity::Error,
    ) -> LocatorError {
        if serenity_error_is_read_denied(&error) {
            self.store.observe_permission_failure(
                None,
                tracked_parent.tracked_parent_channel_id(),
                PermissionAction::ReadMessageHistory,
            );
            LocatorError::Permission {
                tracked_parent,
                message: error.to_string(),
            }
        } else {
            LocatorError::Fetch {
                tracked_parent,
                message: error.to_string(),
            }
        }
    }
}

fn classify_discovered_candidate(
    tracked_parent: TrackedParentKey,
    canonical_thread_id: ChannelId,
    cached_binding: Option<CanonicalThreadBinding>,
    discovered: Result<Option<LedgerId>, StoreLoadError>,
) -> Result<Option<LocatorDiscoveryCandidate>, LocatorError> {
    match discovered {
        Ok(Some(ledger_id)) => Ok(Some(LocatorDiscoveryCandidate::Verified(
            VerifiedCanonicalThreadCandidate::new(
                canonical_thread_id,
                ledger_id,
                LocatorRecoveryReference::channel(canonical_thread_id, None::<String>),
                cached_binding.is_some_and(|binding| {
                    binding.canonical_thread_id() == canonical_thread_id
                        && binding.ledger_id() == ledger_id
                }),
            ),
        ))),
        Ok(None) => Ok(Some(LocatorDiscoveryCandidate::Empty(
            EmptyCanonicalThreadCandidate::new(
                canonical_thread_id,
                LocatorRecoveryReference::channel(canonical_thread_id, None::<String>),
            )
            .with_provisioned_ledger_id(
                cached_binding
                    .filter(|binding| binding.canonical_thread_id() == canonical_thread_id)
                    .map(CanonicalThreadBinding::ledger_id),
            ),
        ))),
        Err(error @ (StoreLoadError::Fetch(_) | StoreLoadError::FetchTimeout { .. })) => {
            Err(LocatorError::Fetch {
                tracked_parent,
                message: error.to_string(),
            })
        }
        Err(StoreLoadError::Permission(message)) => Err(LocatorError::Permission {
            tracked_parent,
            message,
        }),
        Err(_) => Ok(Some(LocatorDiscoveryCandidate::Damaged(
            super::locator::DamagedCanonicalThreadCandidate::new(
                canonical_thread_id,
                LocatorRecoveryReference::channel(canonical_thread_id, None::<String>),
            ),
        ))),
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discord::ledger::locator::CanonicalThreadLocatorState;

    fn tracked_parent() -> TrackedParentKey {
        TrackedParentKey::from_guarded_parent(GuildId::new(1), ChannelId::new(10))
    }

    #[test]
    fn discovery_classifies_empty_thread_as_reusable() {
        assert!(matches!(
            classify_discovered_candidate(tracked_parent(), ChannelId::new(20), None, Ok(None)),
            Ok(Some(LocatorDiscoveryCandidate::Empty(_)))
        ));
    }

    #[test]
    fn discovery_preserves_provisioned_binding_when_thread_is_still_empty() {
        let tracked_parent = tracked_parent();
        let binding = CanonicalThreadBinding::new(
            tracked_parent,
            ChannelId::new(20),
            walicord_ledger::test_fixtures::ledger_id(77),
        );
        let candidate = classify_discovered_candidate(
            tracked_parent,
            ChannelId::new(20),
            Some(binding),
            Ok(None),
        )
        .expect("discovery should succeed")
        .expect("candidate should remain visible");

        assert_eq!(
            CanonicalThreadLocatorState::derive_locator_state(tracked_parent, vec![candidate]),
            CanonicalThreadLocatorState::Provisioned(binding)
        );
    }

    #[test]
    fn discovery_recovery_reference_identifies_candidate_thread() {
        assert_eq!(
            classify_discovered_candidate(tracked_parent(), ChannelId::new(20), None, Ok(None)),
            Ok(Some(LocatorDiscoveryCandidate::Empty(
                EmptyCanonicalThreadCandidate::new(
                    ChannelId::new(20),
                    LocatorRecoveryReference::channel(ChannelId::new(20), None::<String>),
                )
            )))
        );
    }

    #[test]
    fn discovery_propagates_retryable_fetch_failure() {
        assert!(matches!(
            classify_discovered_candidate(
                tracked_parent(),
                ChannelId::new(20),
                None,
                Err(StoreLoadError::Fetch("temporary".to_owned())),
            ),
            Err(LocatorError::Fetch { .. })
        ));
    }

    #[test]
    fn discovery_propagates_permission_failure() {
        assert!(matches!(
            classify_discovered_candidate(
                tracked_parent(),
                ChannelId::new(20),
                None,
                Err(StoreLoadError::Permission("forbidden".to_owned())),
            ),
            Err(LocatorError::Permission { .. })
        ));
    }
}
