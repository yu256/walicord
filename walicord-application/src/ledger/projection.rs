use std::{collections::BTreeMap, time::SystemTime};

use super::{
    entry::LedgerEntry, hash_chain::VerifiedLedgerStoreEnvelope, load::VerifiedLedgerSnapshot,
};
use walicord_ledger::{LedgerEntryId, ProjectedEntryInfo, ProjectedEntryKind};

/// Adapter-supplied transport metadata for a single verified canonical entry. The
/// application keeps just what projection callers consume — the recovery link and the
/// transport-side `recorded_at` fallback — so adapter-native ids (Discord `MessageId`,
/// SMTP message ids, etc.) never leak into application code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifiedEntryTransport {
    message_link: String,
    recorded_at: SystemTime,
}

impl VerifiedEntryTransport {
    pub fn new(message_link: String, recorded_at: SystemTime) -> Self {
        Self {
            message_link,
            recorded_at,
        }
    }

    pub fn message_link(&self) -> &str {
        &self.message_link
    }

    pub fn recorded_at(&self) -> SystemTime {
        self.recorded_at
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct VerifiedEntryTransportIndex {
    by_entry_id: BTreeMap<LedgerEntryId, VerifiedEntryTransport>,
}

impl VerifiedEntryTransportIndex {
    pub fn get(&self, entry_id: LedgerEntryId) -> Option<&VerifiedEntryTransport> {
        self.by_entry_id.get(&entry_id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (LedgerEntryId, &VerifiedEntryTransport)> {
        self.by_entry_id
            .iter()
            .map(|(entry_id, transport)| (*entry_id, transport))
    }
}

/// Verified canonical-thread load result the application reasons over. Parameterised on
/// the external id type so adapters can carry their native handle (e.g. Discord
/// `MessageId`) inside `VerifiedLedgerStoreEnvelope<ExternalId>` without pulling that
/// type into application code — projection helpers only ever touch the payload + the
/// adapter-supplied transport entries.
///
/// Construction goes through [`VerifiedLedgerThreadLoad::new`] which parses-then-checks
/// that every verified envelope has a paired transport entry; downstream projection
/// helpers therefore cannot encounter a missing-transport at runtime.
#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedLedgerThreadLoad<ExternalId> {
    snapshot: VerifiedLedgerSnapshot,
    transport_index: VerifiedEntryTransportIndex,
    verified: Vec<VerifiedLedgerStoreEnvelope<ExternalId>>,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum VerifiedLedgerThreadLoadError {
    #[error("verified envelope {entry_id:?} has no paired transport entry")]
    MissingTransport { entry_id: LedgerEntryId },
}

impl<ExternalId> VerifiedLedgerThreadLoad<ExternalId> {
    pub fn new(
        snapshot: VerifiedLedgerSnapshot,
        verified: Vec<VerifiedLedgerStoreEnvelope<ExternalId>>,
        transport_entries: BTreeMap<LedgerEntryId, VerifiedEntryTransport>,
    ) -> Result<Self, VerifiedLedgerThreadLoadError> {
        for envelope in &verified {
            let entry_id = envelope.payload().entry.id;
            if !transport_entries.contains_key(&entry_id) {
                return Err(VerifiedLedgerThreadLoadError::MissingTransport { entry_id });
            }
        }
        Ok(Self {
            snapshot,
            transport_index: VerifiedEntryTransportIndex {
                by_entry_id: transport_entries,
            },
            verified,
        })
    }

    pub fn snapshot(&self) -> &VerifiedLedgerSnapshot {
        &self.snapshot
    }

    pub fn transport_index(&self) -> &VerifiedEntryTransportIndex {
        &self.transport_index
    }

    pub fn verified(&self) -> &[VerifiedLedgerStoreEnvelope<ExternalId>] {
        &self.verified
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ProjectionConsistencyError {
    #[error("replayed entry {entry_id:?} is missing projected metadata")]
    MissingProjectedEntry { entry_id: LedgerEntryId },
}

#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedLedgerEntryView {
    entry: LedgerEntry,
    projected: ProjectedEntryInfo,
    message_link: String,
    recorded_at: SystemTime,
}

impl VerifiedLedgerEntryView {
    pub fn entry(&self) -> &LedgerEntry {
        &self.entry
    }

    pub fn projected(&self) -> &ProjectedEntryInfo {
        &self.projected
    }

    pub fn message_link(&self) -> &str {
        &self.message_link
    }

    pub fn recorded_at(&self) -> SystemTime {
        self.recorded_at
    }
}

pub fn project_verified_entries<ExternalId>(
    load: &VerifiedLedgerThreadLoad<ExternalId>,
) -> Result<Vec<VerifiedLedgerEntryView>, ProjectionConsistencyError> {
    let projected = load.snapshot().projected();
    let mut out = Vec::with_capacity(load.verified().len());

    for envelope in load.verified() {
        let entry = envelope.payload().entry.clone();
        let entry_id = entry.id;
        let projected_entry = projected
            .entry(entry_id)
            .cloned()
            .ok_or(ProjectionConsistencyError::MissingProjectedEntry { entry_id })?;
        let transport = load
            .transport_index()
            .get(entry_id)
            .expect("VerifiedLedgerThreadLoad::new validates every verified envelope has a paired transport entry");
        out.push(VerifiedLedgerEntryView {
            recorded_at: entry
                .metadata
                .recorded_at
                .unwrap_or_else(|| transport.recorded_at()),
            entry,
            projected: projected_entry,
            message_link: transport.message_link().to_owned(),
        });
    }

    Ok(out)
}

pub fn project_recent_voidable_entries<ExternalId>(
    load: &VerifiedLedgerThreadLoad<ExternalId>,
    limit: usize,
) -> Result<Vec<VerifiedLedgerEntryView>, ProjectionConsistencyError> {
    let all = project_verified_entries(load)?;
    let mut candidates = all
        .into_iter()
        .filter(|entry| {
            matches!(
                entry.projected().kind,
                ProjectedEntryKind::Expense | ProjectedEntryKind::SettlementTransfer
            ) && !entry.projected().voided
                && !entry.projected().sealed
        })
        .collect::<Vec<_>>();
    candidates.reverse();
    candidates.truncate(limit);
    Ok(candidates)
}
