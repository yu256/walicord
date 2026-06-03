use dashmap::DashMap;
use serenity::{
    all::{ChannelId, ChannelType, GuildId},
    async_trait,
    prelude::Context,
};
use std::{collections::HashSet, num::NonZeroU64, sync::Arc};
use tokio::sync::Mutex;
use walicord_application::ledger::{LedgerId, canonical_write::LocatorBindingPublisher};
use walicord_i18n as i18n;

#[cfg(test)]
use super::route_guard::startup_channel_is_track_target;

pub(crate) const CANONICAL_LEDGER_THREAD_NAME: &str = "台帳";

pub(crate) fn is_canonical_thread_name(name: &str) -> bool {
    name == CANONICAL_LEDGER_THREAD_NAME
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum LocatorRecoveryReference {
    Ledger {
        ledger_id_short: String,
        thread_link: Option<String>,
    },
    Channel {
        channel_id: ChannelId,
        channel_link: Option<String>,
    },
}

impl LocatorRecoveryReference {
    pub(crate) fn ledger(
        ledger_id_short: impl Into<String>,
        thread_link: Option<impl Into<String>>,
    ) -> Self {
        Self::Ledger {
            ledger_id_short: ledger_id_short.into(),
            thread_link: thread_link.map(Into::into),
        }
    }

    pub(crate) fn channel(channel_id: ChannelId, channel_link: Option<impl Into<String>>) -> Self {
        Self::Channel {
            channel_id,
            channel_link: channel_link.map(Into::into),
        }
    }

    pub(crate) fn render_line(&self) -> String {
        match self {
            Self::Ledger {
                ledger_id_short,
                thread_link,
            } => {
                let mut line = format!(
                    "{}ledger:{ledger_id_short}",
                    i18n::RECOVERY_REFERENCE_PREFIX
                );
                if let Some(thread_link) = thread_link {
                    line.push_str(" | <");
                    line.push_str(thread_link);
                    line.push('>');
                }
                line
            }
            Self::Channel {
                channel_id,
                channel_link,
            } => {
                let mut line = format!(
                    "{}channel:{}",
                    i18n::RECOVERY_REFERENCE_PREFIX,
                    channel_id.get()
                );
                if let Some(channel_link) = channel_link {
                    line.push_str(" | <");
                    line.push_str(channel_link);
                    line.push('>');
                }
                line
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TrackedParentKey {
    guild_id: GuildId,
    tracked_parent_channel_id: ChannelId,
}

impl TrackedParentKey {
    #[cfg(test)]
    pub(crate) fn new(
        guild_id: GuildId,
        tracked_parent_channel_id: ChannelId,
        kind: ChannelType,
        topic: Option<&str>,
    ) -> Option<Self> {
        startup_channel_is_track_target(kind, topic).then_some(Self {
            guild_id,
            tracked_parent_channel_id,
        })
    }

    pub(crate) fn guild_id(self) -> GuildId {
        self.guild_id
    }

    pub(crate) fn from_guarded_parent(
        guild_id: GuildId,
        tracked_parent_channel_id: ChannelId,
    ) -> Self {
        Self {
            guild_id,
            tracked_parent_channel_id,
        }
    }

    pub(crate) fn tracked_parent_channel_id(self) -> ChannelId {
        self.tracked_parent_channel_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct CanonicalThreadBinding {
    tracked_parent: TrackedParentKey,
    canonical_thread_id: ChannelId,
    ledger_id: LedgerId,
}

impl CanonicalThreadBinding {
    pub(crate) fn new(tracked_parent: TrackedParentKey, canonical_thread_id: ChannelId) -> Self {
        Self {
            tracked_parent,
            canonical_thread_id,
            ledger_id: ledger_id_for_canonical_thread(canonical_thread_id),
        }
    }

    pub(crate) fn tracked_parent(self) -> TrackedParentKey {
        self.tracked_parent
    }

    pub(crate) fn canonical_thread_id(self) -> ChannelId {
        self.canonical_thread_id
    }

    pub(crate) fn ledger_id(self) -> LedgerId {
        self.ledger_id
    }
}

pub(crate) fn ledger_id_for_canonical_thread(canonical_thread_id: ChannelId) -> LedgerId {
    LedgerId::from(
        NonZeroU64::new(canonical_thread_id.get())
            .expect("Discord canonical thread identifiers are non-zero"),
    )
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct VerifiedCanonicalThreadCandidate {
    canonical_thread_id: ChannelId,
    recovery_reference: LocatorRecoveryReference,
    was_previously_authoritative: bool,
}

impl VerifiedCanonicalThreadCandidate {
    pub(crate) fn new(
        canonical_thread_id: ChannelId,
        recovery_reference: LocatorRecoveryReference,
        was_previously_authoritative: bool,
    ) -> Self {
        Self {
            canonical_thread_id,
            recovery_reference,
            was_previously_authoritative,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DamagedCanonicalThreadCandidate {
    canonical_thread_id: ChannelId,
    recovery_reference: LocatorRecoveryReference,
}

impl DamagedCanonicalThreadCandidate {
    pub(crate) fn new(
        canonical_thread_id: ChannelId,
        recovery_reference: LocatorRecoveryReference,
    ) -> Self {
        Self {
            canonical_thread_id,
            recovery_reference,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EmptyCanonicalThreadCandidate {
    canonical_thread_id: ChannelId,
    recovery_reference: LocatorRecoveryReference,
    was_provisioned: bool,
}

impl EmptyCanonicalThreadCandidate {
    pub(crate) fn new(
        canonical_thread_id: ChannelId,
        recovery_reference: LocatorRecoveryReference,
    ) -> Self {
        Self {
            canonical_thread_id,
            recovery_reference,
            was_provisioned: false,
        }
    }

    pub(crate) fn with_provisioned_binding(mut self, was_provisioned: bool) -> Self {
        self.was_provisioned = was_provisioned;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum LocatorDiscoveryCandidate {
    Verified(VerifiedCanonicalThreadCandidate),
    Empty(EmptyCanonicalThreadCandidate),
    Damaged(DamagedCanonicalThreadCandidate),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CanonicalThreadLocatorState {
    ReadyNoThread {
        tracked_parent: TrackedParentKey,
    },
    ReadyEmptyThread {
        tracked_parent: TrackedParentKey,
        canonical_thread_id: ChannelId,
    },
    Provisioned(CanonicalThreadBinding),
    ReadyBound(CanonicalThreadBinding),
    DuplicateBlocked {
        tracked_parent: TrackedParentKey,
        authoritative_candidate_known: bool,
        recovery_references: Vec<LocatorRecoveryReference>,
    },
    DamagedBlocked {
        tracked_parent: TrackedParentKey,
        recovery_reference: LocatorRecoveryReference,
    },
}

impl CanonicalThreadLocatorState {
    #[cfg(test)]
    pub(crate) fn tracked_parent(&self) -> TrackedParentKey {
        match self {
            Self::ReadyNoThread { tracked_parent }
            | Self::ReadyEmptyThread { tracked_parent, .. }
            | Self::DuplicateBlocked { tracked_parent, .. }
            | Self::DamagedBlocked { tracked_parent, .. } => *tracked_parent,
            Self::Provisioned(binding) | Self::ReadyBound(binding) => binding.tracked_parent(),
        }
    }

    pub(crate) fn binding(&self) -> Option<CanonicalThreadBinding> {
        match self {
            Self::Provisioned(binding) | Self::ReadyBound(binding) => Some(*binding),
            Self::ReadyNoThread { .. }
            | Self::ReadyEmptyThread { .. }
            | Self::DuplicateBlocked { .. }
            | Self::DamagedBlocked { .. } => None,
        }
    }

    pub(crate) fn derive_locator_state(
        tracked_parent: TrackedParentKey,
        candidates: Vec<LocatorDiscoveryCandidate>,
    ) -> CanonicalThreadLocatorState {
        let mut damaged_candidates = candidates
            .iter()
            .filter_map(|candidate| match candidate {
                LocatorDiscoveryCandidate::Damaged(candidate) => Some(candidate.clone()),
                LocatorDiscoveryCandidate::Verified(_) | LocatorDiscoveryCandidate::Empty(_) => {
                    None
                }
            })
            .collect::<Vec<_>>();
        damaged_candidates.sort_by_key(|candidate| candidate.canonical_thread_id.get());
        if let Some(damaged_candidate) = damaged_candidates.into_iter().next() {
            return CanonicalThreadLocatorState::DamagedBlocked {
                tracked_parent,
                recovery_reference: damaged_candidate.recovery_reference,
            };
        }

        let mut usable_candidates = candidates
            .into_iter()
            .filter_map(|candidate| match candidate {
                LocatorDiscoveryCandidate::Verified(candidate) => {
                    let recovery_reference = candidate.recovery_reference.clone();
                    Some((
                        candidate.canonical_thread_id,
                        recovery_reference,
                        Some(candidate),
                        None,
                    ))
                }
                LocatorDiscoveryCandidate::Empty(candidate) => {
                    let provisioned = candidate.was_provisioned.then(|| {
                        CanonicalThreadBinding::new(tracked_parent, candidate.canonical_thread_id)
                    });
                    Some((
                        candidate.canonical_thread_id,
                        candidate.recovery_reference,
                        None,
                        provisioned,
                    ))
                }
                LocatorDiscoveryCandidate::Damaged(_) => None,
            })
            .collect::<Vec<_>>();
        usable_candidates.sort_by_key(|(canonical_thread_id, _, verified, _)| {
            (
                !verified
                    .as_ref()
                    .is_some_and(|candidate| candidate.was_previously_authoritative),
                canonical_thread_id.get(),
            )
        });

        match usable_candidates.as_slice() {
            [] => CanonicalThreadLocatorState::ReadyNoThread { tracked_parent },
            [(canonical_thread_id, _, Some(_), _)] => CanonicalThreadLocatorState::ReadyBound(
                CanonicalThreadBinding::new(tracked_parent, *canonical_thread_id),
            ),
            [(_, _, None, Some(binding))] => CanonicalThreadLocatorState::Provisioned(*binding),
            [(canonical_thread_id, _, None, None)] => {
                CanonicalThreadLocatorState::ReadyEmptyThread {
                    tracked_parent,
                    canonical_thread_id: *canonical_thread_id,
                }
            }
            _ => CanonicalThreadLocatorState::DuplicateBlocked {
                tracked_parent,
                authoritative_candidate_known: usable_candidates
                    .iter()
                    .any(|(_, _, candidate, _)| candidate.is_some()),
                recovery_references: usable_candidates
                    .into_iter()
                    .map(|(_, recovery_reference, _, _)| recovery_reference)
                    .collect(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub(crate) enum LocatorError {
    #[error("tracked parent {tracked_parent:?} fetch failed: {message}")]
    Fetch {
        tracked_parent: TrackedParentKey,
        message: String,
    },
    #[error("tracked parent {tracked_parent:?} permission failed: {message}")]
    Permission {
        tracked_parent: TrackedParentKey,
        message: String,
    },
}

#[async_trait]
pub(crate) trait CanonicalThreadLocatorBackend: Send + Sync {
    async fn revalidate(
        &self,
        tracked_parent: TrackedParentKey,
        cached_binding: Option<CanonicalThreadBinding>,
    ) -> Result<CanonicalThreadLocatorState, LocatorError>;
}

#[async_trait]
pub(crate) trait CanonicalThreadDiscoveryPort: Send + Sync {
    async fn candidate_thread_ids(
        &self,
        tracked_parent: TrackedParentKey,
    ) -> Result<Vec<ChannelId>, LocatorError>;

    async fn classify_candidate(
        &self,
        tracked_parent: TrackedParentKey,
        canonical_thread_id: ChannelId,
        cached_binding: Option<CanonicalThreadBinding>,
    ) -> Result<Option<LocatorDiscoveryCandidate>, LocatorError>;
}

pub(crate) struct RediscoveringCanonicalThreadLocatorBackend<P> {
    port: P,
}

impl<P> RediscoveringCanonicalThreadLocatorBackend<P> {
    pub(crate) fn new(port: P) -> Self {
        Self { port }
    }
}

#[async_trait]
impl<P> CanonicalThreadLocatorBackend for RediscoveringCanonicalThreadLocatorBackend<P>
where
    P: CanonicalThreadDiscoveryPort,
{
    async fn revalidate(
        &self,
        tracked_parent: TrackedParentKey,
        cached_binding: Option<CanonicalThreadBinding>,
    ) -> Result<CanonicalThreadLocatorState, LocatorError> {
        let mut candidates = Vec::new();
        for canonical_thread_id in self.port.candidate_thread_ids(tracked_parent).await? {
            if let Some(candidate) = self
                .port
                .classify_candidate(tracked_parent, canonical_thread_id, cached_binding)
                .await?
            {
                candidates.push(candidate);
            }
        }

        Ok(CanonicalThreadLocatorState::derive_locator_state(
            tracked_parent,
            candidates,
        ))
    }
}

pub(crate) struct CanonicalThreadLocator<B> {
    backend: B,
    cache: DashMap<TrackedParentKey, CanonicalThreadLocatorState>,
    refresh_locks: DashMap<TrackedParentKey, Arc<Mutex<()>>>,
}

impl<B> CanonicalThreadLocator<B> {
    pub(crate) fn new(backend: B) -> Self {
        Self {
            backend,
            cache: DashMap::new(),
            refresh_locks: DashMap::new(),
        }
    }

    pub(crate) fn cached(
        &self,
        tracked_parent: TrackedParentKey,
    ) -> Option<CanonicalThreadLocatorState> {
        self.cache.get(&tracked_parent).map(|state| state.clone())
    }

    pub(crate) fn clear(&self, tracked_parent: TrackedParentKey) {
        self.cache.remove(&tracked_parent);
    }

    pub(crate) fn clear_tracked_parent_channel(
        &self,
        tracked_parent_channel_id: ChannelId,
    ) -> Vec<CanonicalThreadBinding> {
        let bindings = self
            .cache
            .iter()
            .filter(|entry| entry.key().tracked_parent_channel_id() == tracked_parent_channel_id)
            .filter_map(|entry| entry.value().binding())
            .collect();
        self.cache.retain(|tracked_parent, _| {
            tracked_parent.tracked_parent_channel_id() != tracked_parent_channel_id
        });
        self.refresh_locks.retain(|tracked_parent, _| {
            tracked_parent.tracked_parent_channel_id() != tracked_parent_channel_id
        });
        bindings
    }

    pub(crate) fn replace_with_damaged_blocked(
        &self,
        tracked_parent: TrackedParentKey,
        recovery_reference: LocatorRecoveryReference,
    ) -> CanonicalThreadLocatorState {
        let state = CanonicalThreadLocatorState::DamagedBlocked {
            tracked_parent,
            recovery_reference,
        };
        self.cache.insert(tracked_parent, state.clone());
        state
    }

    pub(crate) fn replace_with_ready_binding(
        &self,
        binding: CanonicalThreadBinding,
    ) -> CanonicalThreadLocatorState {
        let state = CanonicalThreadLocatorState::ReadyBound(binding);
        self.cache.insert(binding.tracked_parent(), state.clone());
        state
    }

    pub(crate) fn replace_with_provisioned_binding(
        &self,
        binding: CanonicalThreadBinding,
    ) -> CanonicalThreadLocatorState {
        let state = CanonicalThreadLocatorState::Provisioned(binding);
        self.cache.insert(binding.tracked_parent(), state.clone());
        state
    }

    fn lock_for(&self, tracked_parent: TrackedParentKey) -> Arc<Mutex<()>> {
        self.refresh_locks
            .entry(tracked_parent)
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone()
    }
}

impl<B> CanonicalThreadLocator<B>
where
    B: CanonicalThreadLocatorBackend,
{
    pub(crate) async fn resolve(
        &self,
        _ctx: &Context,
        tracked_parent: TrackedParentKey,
    ) -> Result<CanonicalThreadLocatorState, LocatorError> {
        self.resolve_without_context(tracked_parent).await
    }

    pub(crate) async fn refresh(
        &self,
        _ctx: &Context,
        tracked_parent: TrackedParentKey,
    ) -> Result<CanonicalThreadLocatorState, LocatorError> {
        self.refresh_without_context(tracked_parent).await
    }

    async fn resolve_without_context(
        &self,
        tracked_parent: TrackedParentKey,
    ) -> Result<CanonicalThreadLocatorState, LocatorError> {
        if let Some(cached) = self.cached(tracked_parent) {
            return Ok(cached);
        }

        self.revalidate(tracked_parent, None).await
    }

    async fn refresh_without_context(
        &self,
        tracked_parent: TrackedParentKey,
    ) -> Result<CanonicalThreadLocatorState, LocatorError> {
        let cached_binding = self
            .cached(tracked_parent)
            .and_then(|state| state.binding());
        self.revalidate(tracked_parent, cached_binding).await
    }

    pub(crate) async fn stale_lookup_failed(
        &self,
        tracked_parent: TrackedParentKey,
    ) -> Result<CanonicalThreadLocatorState, LocatorError> {
        self.clear(tracked_parent);
        self.revalidate(tracked_parent, None).await
    }

    async fn revalidate(
        &self,
        tracked_parent: TrackedParentKey,
        cached_binding: Option<CanonicalThreadBinding>,
    ) -> Result<CanonicalThreadLocatorState, LocatorError> {
        let lock = self.lock_for(tracked_parent);
        let _guard = lock.lock().await;

        let state = self
            .backend
            .revalidate(tracked_parent, cached_binding)
            .await?;
        self.cache.insert(tracked_parent, state.clone());
        Ok(state)
    }
}

pub(crate) fn canonical_thread_candidate_ids<I, S>(
    parent_channel_id: ChannelId,
    threads: I,
) -> Vec<ChannelId>
where
    I: IntoIterator<Item = (ChannelId, ChannelType, S, Option<ChannelId>)>,
    S: AsRef<str>,
{
    let mut seen = HashSet::new();
    let mut candidates = Vec::new();
    for (thread_id, kind, name, parent_id) in threads {
        if kind == ChannelType::PublicThread
            && parent_id == Some(parent_channel_id)
            && is_canonical_thread_name(name.as_ref())
            && seen.insert(thread_id)
        {
            candidates.push(thread_id);
        }
    }
    candidates
}

/// Per-request adapter binding the application
/// [`LocatorBindingPublisher`] port to a concrete locator instance + binding.
/// Constructed inside a single ledger interaction so the application use case
/// publishes the ready binding through a typed port instead of pulling the
/// `Arc<DiscordCanonicalThreadLocator>` from a shared dependency bag.
pub(crate) struct RequestBoundLocatorPublisher<'a> {
    pub(crate) locator: &'a super::adapters::DiscordCanonicalThreadLocator,
    pub(crate) binding: CanonicalThreadBinding,
}

impl LocatorBindingPublisher for RequestBoundLocatorPublisher<'_> {
    fn publish_ready(&self) {
        self.locator.replace_with_ready_binding(self.binding);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use std::{
        collections::{HashMap, VecDeque},
        sync::{
            Arc, Mutex as StdMutex,
            atomic::{AtomicUsize, Ordering},
        },
        time::Duration,
    };

    fn tracked_parent_key() -> TrackedParentKey {
        TrackedParentKey::new(
            GuildId::new(1),
            ChannelId::new(10),
            ChannelType::Text,
            Some("ops #walicord"),
        )
        .expect("tracked parent should satisfy predicate")
    }

    fn canonical_binding() -> CanonicalThreadBinding {
        CanonicalThreadBinding::new(tracked_parent_key(), ChannelId::new(20))
    }

    fn ledger_recovery_reference(label: &str) -> LocatorRecoveryReference {
        LocatorRecoveryReference::ledger(
            label,
            Some(format!("https://discord.example/messages/{label}")),
        )
    }

    fn channel_recovery_reference(channel_id: ChannelId) -> LocatorRecoveryReference {
        LocatorRecoveryReference::channel(
            channel_id,
            Some(format!(
                "https://discord.example/channels/1/{}",
                channel_id.get()
            )),
        )
    }

    #[derive(Clone)]
    struct FakeLocatorBackend {
        state: Arc<FakeLocatorBackendState>,
    }

    struct FakeLocatorBackendState {
        calls: StdMutex<Vec<(TrackedParentKey, Option<CanonicalThreadBinding>)>>,
        responses: StdMutex<VecDeque<Result<CanonicalThreadLocatorState, LocatorError>>>,
    }

    impl FakeLocatorBackend {
        fn new(responses: Vec<Result<CanonicalThreadLocatorState, LocatorError>>) -> Self {
            Self {
                state: Arc::new(FakeLocatorBackendState {
                    calls: StdMutex::new(Vec::new()),
                    responses: StdMutex::new(VecDeque::from(responses)),
                }),
            }
        }

        fn calls(&self) -> Vec<(TrackedParentKey, Option<CanonicalThreadBinding>)> {
            self.state
                .calls
                .lock()
                .expect("calls lock should not poison")
                .clone()
        }
    }

    #[async_trait]
    impl CanonicalThreadLocatorBackend for FakeLocatorBackend {
        async fn revalidate(
            &self,
            tracked_parent: TrackedParentKey,
            cached_binding: Option<CanonicalThreadBinding>,
        ) -> Result<CanonicalThreadLocatorState, LocatorError> {
            self.state
                .calls
                .lock()
                .expect("calls lock should not poison")
                .push((tracked_parent, cached_binding));
            self.state
                .responses
                .lock()
                .expect("responses lock should not poison")
                .pop_front()
                .expect("fake backend should have a queued response")
        }
    }

    #[derive(Clone)]
    struct ConcurrentTrackingBackend {
        state: Arc<ConcurrentTrackingBackendState>,
    }

    struct ConcurrentTrackingBackendState {
        calls: StdMutex<Vec<(TrackedParentKey, Option<CanonicalThreadBinding>)>>,
        responses: StdMutex<VecDeque<Result<CanonicalThreadLocatorState, LocatorError>>>,
        active_calls: AtomicUsize,
        max_active_calls: AtomicUsize,
    }

    impl ConcurrentTrackingBackend {
        fn new(responses: Vec<Result<CanonicalThreadLocatorState, LocatorError>>) -> Self {
            Self {
                state: Arc::new(ConcurrentTrackingBackendState {
                    calls: StdMutex::new(Vec::new()),
                    responses: StdMutex::new(VecDeque::from(responses)),
                    active_calls: AtomicUsize::new(0),
                    max_active_calls: AtomicUsize::new(0),
                }),
            }
        }

        fn max_active_calls(&self) -> usize {
            self.state.max_active_calls.load(Ordering::SeqCst)
        }
    }

    #[async_trait]
    impl CanonicalThreadLocatorBackend for ConcurrentTrackingBackend {
        async fn revalidate(
            &self,
            tracked_parent: TrackedParentKey,
            cached_binding: Option<CanonicalThreadBinding>,
        ) -> Result<CanonicalThreadLocatorState, LocatorError> {
            self.state
                .calls
                .lock()
                .expect("calls lock should not poison")
                .push((tracked_parent, cached_binding));

            let active_calls = self.state.active_calls.fetch_add(1, Ordering::SeqCst) + 1;
            self.state
                .max_active_calls
                .fetch_max(active_calls, Ordering::SeqCst);
            tokio::time::sleep(Duration::from_millis(20)).await;
            self.state.active_calls.fetch_sub(1, Ordering::SeqCst);

            self.state
                .responses
                .lock()
                .expect("responses lock should not poison")
                .pop_front()
                .expect("concurrent backend should have a queued response")
        }
    }

    #[derive(Clone)]
    struct FakeDiscoveryPort {
        state: Arc<FakeDiscoveryPortState>,
    }

    struct FakeDiscoveryPortState {
        candidate_thread_ids: Vec<ChannelId>,
        classifications:
            HashMap<ChannelId, Result<Option<LocatorDiscoveryCandidate>, LocatorError>>,
        classify_calls:
            StdMutex<Vec<(TrackedParentKey, ChannelId, Option<CanonicalThreadBinding>)>>,
    }

    impl FakeDiscoveryPort {
        fn new(
            candidate_thread_ids: Vec<ChannelId>,
            classifications: HashMap<
                ChannelId,
                Result<Option<LocatorDiscoveryCandidate>, LocatorError>,
            >,
        ) -> Self {
            Self {
                state: Arc::new(FakeDiscoveryPortState {
                    candidate_thread_ids,
                    classifications,
                    classify_calls: StdMutex::new(Vec::new()),
                }),
            }
        }

        fn classify_calls(
            &self,
        ) -> Vec<(TrackedParentKey, ChannelId, Option<CanonicalThreadBinding>)> {
            self.state
                .classify_calls
                .lock()
                .expect("classify calls lock should not poison")
                .clone()
        }
    }

    #[async_trait]
    impl CanonicalThreadDiscoveryPort for FakeDiscoveryPort {
        async fn candidate_thread_ids(
            &self,
            _tracked_parent: TrackedParentKey,
        ) -> Result<Vec<ChannelId>, LocatorError> {
            Ok(self.state.candidate_thread_ids.clone())
        }

        async fn classify_candidate(
            &self,
            tracked_parent: TrackedParentKey,
            canonical_thread_id: ChannelId,
            cached_binding: Option<CanonicalThreadBinding>,
        ) -> Result<Option<LocatorDiscoveryCandidate>, LocatorError> {
            self.state
                .classify_calls
                .lock()
                .expect("classify calls lock should not poison")
                .push((tracked_parent, canonical_thread_id, cached_binding));
            self.state
                .classifications
                .get(&canonical_thread_id)
                .expect("classification should be queued for every candidate")
                .clone()
        }
    }

    #[rstest]
    #[case::tracked_text_channel(ChannelType::Text, Some("ops #walicord"), true)]
    #[case::text_channel_without_flag(ChannelType::Text, Some("ops"), false)]
    #[case::voice_channel_with_flag(ChannelType::Voice, Some("#walicord"), false)]
    fn tracked_parent_key_only_accepts_text_channels_with_topic_flag(
        #[case] kind: ChannelType,
        #[case] topic: Option<&str>,
        #[case] expected_present: bool,
    ) {
        let key = TrackedParentKey::new(GuildId::new(1), ChannelId::new(10), kind, topic);

        assert_eq!(key.is_some(), expected_present);
    }

    #[test]
    fn locator_state_carries_binding_and_recovery_context() {
        let tracked_parent = tracked_parent_key();
        let binding = canonical_binding();
        let duplicate = CanonicalThreadLocatorState::DuplicateBlocked {
            tracked_parent,
            authoritative_candidate_known: false,
            recovery_references: vec![ledger_recovery_reference("dup")],
        };
        let damaged = CanonicalThreadLocatorState::DamagedBlocked {
            tracked_parent,
            recovery_reference: channel_recovery_reference(
                tracked_parent.tracked_parent_channel_id(),
            ),
        };

        assert_eq!(
            CanonicalThreadLocatorState::ReadyBound(binding).binding(),
            Some(binding)
        );
        assert_eq!(duplicate.binding(), None);
        assert_eq!(damaged.binding(), None);
        assert_eq!(duplicate.tracked_parent(), tracked_parent);
        assert_eq!(damaged.tracked_parent(), tracked_parent);
    }

    #[test]
    fn derive_locator_state_builds_ready_binding_from_single_verified_candidate() {
        let tracked_parent = tracked_parent_key();
        let state = CanonicalThreadLocatorState::derive_locator_state(
            tracked_parent,
            vec![LocatorDiscoveryCandidate::Verified(
                VerifiedCanonicalThreadCandidate::new(
                    ChannelId::new(42),
                    ledger_recovery_reference("ready"),
                    false,
                ),
            )],
        );

        assert_eq!(
            state,
            CanonicalThreadLocatorState::ReadyBound(CanonicalThreadBinding::new(
                tracked_parent,
                ChannelId::new(42),
            ))
        );
    }

    #[test]
    fn canonical_thread_binding_derives_ledger_id_from_thread_id() {
        let binding = CanonicalThreadBinding::new(tracked_parent_key(), ChannelId::new(42));

        assert_eq!(
            binding.ledger_id(),
            walicord_ledger::test_fixtures::ledger_id(42)
        );
    }

    #[test]
    fn derive_locator_state_returns_ready_no_thread_for_empty_candidates() {
        let tracked_parent = tracked_parent_key();

        assert_eq!(
            CanonicalThreadLocatorState::derive_locator_state(tracked_parent, Vec::new()),
            CanonicalThreadLocatorState::ReadyNoThread { tracked_parent }
        );
    }

    #[test]
    fn derive_locator_state_keeps_single_empty_thread_reusable() {
        let tracked_parent = tracked_parent_key();

        assert_eq!(
            CanonicalThreadLocatorState::derive_locator_state(
                tracked_parent,
                vec![LocatorDiscoveryCandidate::Empty(
                    EmptyCanonicalThreadCandidate::new(
                        ChannelId::new(42),
                        ledger_recovery_reference("empty"),
                    ),
                )],
            ),
            CanonicalThreadLocatorState::ReadyEmptyThread {
                tracked_parent,
                canonical_thread_id: ChannelId::new(42),
            }
        );
    }

    #[test]
    fn derive_locator_state_blocks_verified_and_empty_thread_duplicates() {
        let tracked_parent = tracked_parent_key();
        let state = CanonicalThreadLocatorState::derive_locator_state(
            tracked_parent,
            vec![
                LocatorDiscoveryCandidate::Verified(VerifiedCanonicalThreadCandidate::new(
                    ChannelId::new(20),
                    ledger_recovery_reference("verified"),
                    false,
                )),
                LocatorDiscoveryCandidate::Empty(EmptyCanonicalThreadCandidate::new(
                    ChannelId::new(30),
                    ledger_recovery_reference("empty"),
                )),
            ],
        );

        assert!(matches!(
            state,
            CanonicalThreadLocatorState::DuplicateBlocked {
                authoritative_candidate_known: true,
                ..
            }
        ));
    }

    #[test]
    fn derive_locator_state_blocks_duplicates_and_orders_recovery_references() {
        let tracked_parent = tracked_parent_key();
        let state = CanonicalThreadLocatorState::derive_locator_state(
            tracked_parent,
            vec![
                LocatorDiscoveryCandidate::Verified(VerifiedCanonicalThreadCandidate::new(
                    ChannelId::new(30),
                    ledger_recovery_reference("later"),
                    false,
                )),
                LocatorDiscoveryCandidate::Verified(VerifiedCanonicalThreadCandidate::new(
                    ChannelId::new(20),
                    ledger_recovery_reference("keep"),
                    true,
                )),
                LocatorDiscoveryCandidate::Verified(VerifiedCanonicalThreadCandidate::new(
                    ChannelId::new(10),
                    ledger_recovery_reference("earliest"),
                    false,
                )),
            ],
        );

        assert_eq!(
            state,
            CanonicalThreadLocatorState::DuplicateBlocked {
                tracked_parent,
                authoritative_candidate_known: true,
                recovery_references: vec![
                    ledger_recovery_reference("keep"),
                    ledger_recovery_reference("earliest"),
                    ledger_recovery_reference("later"),
                ],
            }
        );
    }

    #[test]
    fn derive_locator_state_prefers_damaged_blocked_outcome() {
        let tracked_parent = tracked_parent_key();
        let state = CanonicalThreadLocatorState::derive_locator_state(
            tracked_parent,
            vec![
                LocatorDiscoveryCandidate::Verified(VerifiedCanonicalThreadCandidate::new(
                    ChannelId::new(20),
                    ledger_recovery_reference("ready"),
                    true,
                )),
                LocatorDiscoveryCandidate::Damaged(DamagedCanonicalThreadCandidate::new(
                    ChannelId::new(15),
                    channel_recovery_reference(tracked_parent.tracked_parent_channel_id()),
                )),
            ],
        );

        assert_eq!(
            state,
            CanonicalThreadLocatorState::DamagedBlocked {
                tracked_parent,
                recovery_reference: channel_recovery_reference(
                    tracked_parent.tracked_parent_channel_id()
                ),
            }
        );
    }

    #[tokio::test]
    async fn resolve_uses_warm_cache_until_explicit_revalidation() {
        let tracked_parent = tracked_parent_key();
        let ready = CanonicalThreadLocatorState::ReadyBound(canonical_binding());
        let backend = FakeLocatorBackend::new(vec![Ok(ready.clone())]);
        let locator = CanonicalThreadLocator::new(backend.clone());

        assert_eq!(
            locator
                .resolve_without_context(tracked_parent)
                .await
                .expect("cache miss should revalidate"),
            ready
        );
        assert_eq!(
            locator
                .resolve_without_context(tracked_parent)
                .await
                .expect("warm cache should return directly"),
            ready
        );
        assert_eq!(backend.calls().len(), 1);
    }

    #[tokio::test]
    async fn refresh_revalidates_even_when_cached() {
        let tracked_parent = tracked_parent_key();
        let original = CanonicalThreadLocatorState::ReadyBound(canonical_binding());
        let refreshed = CanonicalThreadLocatorState::ReadyNoThread { tracked_parent };
        let backend = FakeLocatorBackend::new(vec![Ok(original.clone()), Ok(refreshed.clone())]);
        let locator = CanonicalThreadLocator::new(backend.clone());

        locator
            .resolve_without_context(tracked_parent)
            .await
            .expect("initial resolve should warm cache");
        assert_eq!(
            locator
                .refresh_without_context(tracked_parent)
                .await
                .expect("refresh should revalidate"),
            refreshed
        );
        assert_eq!(
            backend.calls(),
            vec![
                (tracked_parent, None),
                (tracked_parent, Some(canonical_binding()))
            ]
        );
    }

    #[tokio::test]
    async fn stale_lookup_failed_clears_cached_binding_before_revalidate() {
        let tracked_parent = tracked_parent_key();
        let backend = FakeLocatorBackend::new(vec![Ok(CanonicalThreadLocatorState::ReadyBound(
            canonical_binding(),
        ))]);
        let locator = CanonicalThreadLocator::new(backend.clone());
        locator.cache.insert(
            tracked_parent,
            CanonicalThreadLocatorState::ReadyBound(canonical_binding()),
        );

        locator
            .stale_lookup_failed(tracked_parent)
            .await
            .expect("stale lookup should trigger revalidation");

        assert_eq!(backend.calls(), vec![(tracked_parent, None)]);
    }

    #[test]
    fn clear_tracked_parent_channel_removes_only_matching_cached_bindings() {
        let locator = CanonicalThreadLocator::new(());
        let matching = canonical_binding();
        let retained_parent =
            TrackedParentKey::from_guarded_parent(GuildId::new(1), ChannelId::new(11));
        let retained = CanonicalThreadBinding::new(retained_parent, ChannelId::new(21));
        locator.replace_with_ready_binding(matching);
        locator.replace_with_ready_binding(retained);

        let cleared = locator.clear_tracked_parent_channel(ChannelId::new(10));

        assert_eq!(cleared, vec![matching]);
        assert_eq!(locator.cached(tracked_parent_key()), None);
        assert_eq!(
            locator.cached(retained_parent),
            Some(CanonicalThreadLocatorState::ReadyBound(retained))
        );
    }

    #[tokio::test]
    async fn stale_lookup_failed_keeps_same_parent_revalidation_serialized() {
        let tracked_parent = tracked_parent_key();
        let ready = CanonicalThreadLocatorState::ReadyNoThread { tracked_parent };
        let backend = ConcurrentTrackingBackend::new(vec![Ok(ready.clone()), Ok(ready.clone())]);
        let locator = CanonicalThreadLocator::new(backend.clone());

        locator.cache.insert(tracked_parent, ready.clone());

        let (first, second) = tokio::join!(
            locator.stale_lookup_failed(tracked_parent),
            locator.stale_lookup_failed(tracked_parent)
        );

        assert_eq!(first.expect("first stale miss should succeed"), ready);
        assert_eq!(second.expect("second stale miss should succeed"), ready);
        assert_eq!(backend.max_active_calls(), 1);
    }

    #[test]
    fn replace_with_ready_binding_promotes_provisioned_cache_entry() {
        let tracked_parent = tracked_parent_key();
        let binding = canonical_binding();
        let locator = CanonicalThreadLocator::new(FakeLocatorBackend::new(Vec::new()));
        locator.replace_with_provisioned_binding(binding);

        assert_eq!(
            locator.replace_with_ready_binding(binding),
            CanonicalThreadLocatorState::ReadyBound(binding)
        );
        assert_eq!(
            locator.cached(tracked_parent),
            Some(CanonicalThreadLocatorState::ReadyBound(binding))
        );
    }

    #[test]
    fn replace_with_damaged_blocked_overwrites_ready_binding_cache_entry() {
        let tracked_parent = tracked_parent_key();
        let locator = CanonicalThreadLocator::new(FakeLocatorBackend::new(Vec::new()));
        locator.cache.insert(
            tracked_parent,
            CanonicalThreadLocatorState::ReadyBound(canonical_binding()),
        );
        let damaged = locator.replace_with_damaged_blocked(
            tracked_parent,
            channel_recovery_reference(tracked_parent.tracked_parent_channel_id()),
        );

        assert_eq!(
            locator.cached(tracked_parent),
            Some(CanonicalThreadLocatorState::DamagedBlocked {
                tracked_parent,
                recovery_reference: channel_recovery_reference(
                    tracked_parent.tracked_parent_channel_id()
                ),
            })
        );
        assert_eq!(
            damaged,
            CanonicalThreadLocatorState::DamagedBlocked {
                tracked_parent,
                recovery_reference: channel_recovery_reference(
                    tracked_parent.tracked_parent_channel_id()
                ),
            }
        );
    }

    #[tokio::test]
    async fn refresh_serializes_same_parent_revalidation() {
        let tracked_parent = tracked_parent_key();
        let ready = CanonicalThreadLocatorState::ReadyNoThread { tracked_parent };
        let backend = ConcurrentTrackingBackend::new(vec![Ok(ready.clone()), Ok(ready.clone())]);
        let locator = CanonicalThreadLocator::new(backend.clone());

        let (first, second) = tokio::join!(
            locator.refresh_without_context(tracked_parent),
            locator.refresh_without_context(tracked_parent)
        );

        assert_eq!(first.expect("first refresh should succeed"), ready);
        assert_eq!(second.expect("second refresh should succeed"), ready);
        assert_eq!(backend.max_active_calls(), 1);
    }

    #[tokio::test]
    async fn rediscovering_backend_derives_blocked_state_from_candidate_classification() {
        let tracked_parent = tracked_parent_key();
        let mut classifications = HashMap::new();
        classifications.insert(
            ChannelId::new(20),
            Ok(Some(LocatorDiscoveryCandidate::Verified(
                VerifiedCanonicalThreadCandidate::new(
                    ChannelId::new(20),
                    ledger_recovery_reference("ready"),
                    true,
                ),
            ))),
        );
        classifications.insert(
            ChannelId::new(30),
            Ok(Some(LocatorDiscoveryCandidate::Verified(
                VerifiedCanonicalThreadCandidate::new(
                    ChannelId::new(30),
                    ledger_recovery_reference("other"),
                    false,
                ),
            ))),
        );
        let port = FakeDiscoveryPort::new(
            vec![ChannelId::new(20), ChannelId::new(30)],
            classifications,
        );
        let backend = RediscoveringCanonicalThreadLocatorBackend::new(port.clone());

        let state = backend
            .revalidate(tracked_parent, Some(canonical_binding()))
            .await
            .expect("rediscovery should succeed");

        assert_eq!(
            state,
            CanonicalThreadLocatorState::DuplicateBlocked {
                tracked_parent,
                authoritative_candidate_known: true,
                recovery_references: vec![
                    ledger_recovery_reference("ready"),
                    ledger_recovery_reference("other"),
                ],
            }
        );
        assert_eq!(
            port.classify_calls(),
            vec![
                (
                    tracked_parent,
                    ChannelId::new(20),
                    Some(canonical_binding())
                ),
                (
                    tracked_parent,
                    ChannelId::new(30),
                    Some(canonical_binding())
                ),
            ]
        );
    }

    #[tokio::test]
    async fn rediscovering_backend_returns_ready_no_thread_when_no_candidates_exist() {
        let tracked_parent = tracked_parent_key();
        let port = FakeDiscoveryPort::new(Vec::new(), HashMap::new());
        let backend = RediscoveringCanonicalThreadLocatorBackend::new(port);

        assert_eq!(
            backend
                .revalidate(tracked_parent, None)
                .await
                .expect("empty rediscovery should succeed"),
            CanonicalThreadLocatorState::ReadyNoThread { tracked_parent }
        );
    }
}
