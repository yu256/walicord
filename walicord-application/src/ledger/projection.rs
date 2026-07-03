use std::{collections::BTreeMap, time::SystemTime};

use super::{
    entry::{LedgerEntry, LedgerEntryMetadata},
    hash_chain::VerifiedLedgerStoreEnvelope,
    load::VerifiedLedgerSnapshot,
};
use walicord_ledger::{
    ExpenseRecorded, LedgerEntryId, LedgerEvent, NormalizedSettlementPlanRecorded,
    ProjectedEntryInfo,
};

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
    verified_transport: Vec<VerifiedEntryTransport>,
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
        let mut verified_transport = Vec::with_capacity(verified.len());
        for envelope in &verified {
            let entry_id = envelope.payload().entry.id;
            let Some(transport) = transport_entries.get(&entry_id) else {
                return Err(VerifiedLedgerThreadLoadError::MissingTransport { entry_id });
            };
            verified_transport.push(transport.clone());
        }
        Ok(Self {
            snapshot,
            transport_index: VerifiedEntryTransportIndex {
                by_entry_id: transport_entries,
            },
            verified,
            verified_transport,
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

    fn verified_with_transport(
        &self,
    ) -> impl Iterator<
        Item = (
            &VerifiedLedgerStoreEnvelope<ExternalId>,
            &VerifiedEntryTransport,
        ),
    > {
        self.verified.iter().zip(self.verified_transport.iter())
    }

    pub fn next_entry_id(&self) -> Result<LedgerEntryId, NextLedgerEntryIdError> {
        next_ledger_entry_id(
            self.verified
                .iter()
                .map(|envelope| envelope.payload().entry.id),
        )
    }

    /// Drop the transport-specific external id from every verified envelope so
    /// the load can cross the application boundary without carrying e.g.
    /// `serenity::MessageId`. The snapshot + transport index are preserved.
    pub fn forget_external_ids(self) -> VerifiedLedgerThreadLoad<()> {
        VerifiedLedgerThreadLoad {
            snapshot: self.snapshot,
            transport_index: self.transport_index,
            verified_transport: self.verified_transport,
            verified: self
                .verified
                .into_iter()
                .map(|envelope| envelope.forget_external_id())
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum NextLedgerEntryIdError {
    #[error("ledger entry id space is exhausted")]
    Exhausted,
}

const EMPTY_LEDGER_NEXT_ENTRY_ID: LedgerEntryId = LedgerEntryId(1);

pub fn next_ledger_entry_id(
    entry_ids: impl IntoIterator<Item = LedgerEntryId>,
) -> Result<LedgerEntryId, NextLedgerEntryIdError> {
    match entry_ids.into_iter().max() {
        Some(entry_id) => entry_id.next().ok_or(NextLedgerEntryIdError::Exhausted),
        None => Ok(EMPTY_LEDGER_NEXT_ENTRY_ID),
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

    for (envelope, transport) in load.verified_with_transport() {
        let entry = envelope.payload().entry.clone();
        let entry_id = entry.id;
        let projected_entry = projected
            .entry(entry_id)
            .cloned()
            .ok_or(ProjectionConsistencyError::MissingProjectedEntry { entry_id })?;
        out.push(VerifiedLedgerEntryView {
            recorded_at: recorded_at_with_transport_fallback(&entry.metadata, transport),
            entry,
            projected: projected_entry,
            message_link: transport.message_link().to_owned(),
        });
    }

    Ok(out)
}

fn recorded_at_with_transport_fallback(
    metadata: &LedgerEntryMetadata,
    transport: &VerifiedEntryTransport,
) -> SystemTime {
    match metadata.recorded_at {
        Some(recorded_at) => recorded_at,
        None => transport.recorded_at(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpenseOrSettlementEvent {
    Expense(ExpenseRecorded),
    Settlement(NormalizedSettlementPlanRecorded),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpenseOrSettlementView {
    entry_id: LedgerEntryId,
    event: ExpenseOrSettlementEvent,
    metadata: LedgerEntryMetadata,
    recorded_at: SystemTime,
    message_link: String,
    sealed: bool,
}

impl ExpenseOrSettlementView {
    pub fn entry_id(&self) -> LedgerEntryId {
        self.entry_id
    }

    pub fn event(&self) -> &ExpenseOrSettlementEvent {
        &self.event
    }

    pub fn metadata(&self) -> &LedgerEntryMetadata {
        &self.metadata
    }

    pub fn recorded_at(&self) -> SystemTime {
        self.recorded_at
    }

    pub fn message_link(&self) -> &str {
        &self.message_link
    }

    pub fn sealed(&self) -> bool {
        self.sealed
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("entry {entry_id:?} is not an expense or settlement")]
pub struct NotExpenseOrSettlement {
    pub entry_id: LedgerEntryId,
}

impl TryFrom<VerifiedLedgerEntryView> for ExpenseOrSettlementView {
    type Error = NotExpenseOrSettlement;

    fn try_from(view: VerifiedLedgerEntryView) -> Result<Self, Self::Error> {
        let sealed = view.projected().sealed;
        let entry_id = view.entry.id;
        let event = match view.entry.event {
            LedgerEvent::ExpenseRecorded(e) => ExpenseOrSettlementEvent::Expense(e),
            LedgerEvent::NormalizedSettlementPlanRecorded(e) => {
                ExpenseOrSettlementEvent::Settlement(e)
            }
            _ => return Err(NotExpenseOrSettlement { entry_id }),
        };
        Ok(ExpenseOrSettlementView {
            entry_id,
            metadata: view.entry.metadata,
            event,
            recorded_at: view.recorded_at,
            message_link: view.message_link,
            sealed,
        })
    }
}

pub fn project_recent_voidable_entries<ExternalId>(
    load: &VerifiedLedgerThreadLoad<ExternalId>,
    limit: usize,
) -> Result<Vec<ExpenseOrSettlementView>, ProjectionConsistencyError> {
    let all = project_verified_entries(load)?;
    let mut candidates: Vec<ExpenseOrSettlementView> = all
        .into_iter()
        .filter(|v| !v.projected().voided && !v.projected().sealed)
        .flat_map(ExpenseOrSettlementView::try_from)
        .collect();
    candidates.reverse();
    candidates.truncate(limit);
    Ok(candidates)
}

pub fn project_recent_entries<ExternalId>(
    load: &VerifiedLedgerThreadLoad<ExternalId>,
    limit: usize,
) -> Result<Vec<ExpenseOrSettlementView>, ProjectionConsistencyError> {
    let all = project_verified_entries(load)?;
    let mut entries: Vec<ExpenseOrSettlementView> = all
        .into_iter()
        .filter(|v| !v.projected().voided)
        .flat_map(ExpenseOrSettlementView::try_from)
        .collect();
    entries.reverse();
    entries.truncate(limit);
    Ok(entries)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::empty(Vec::new(), Ok(LedgerEntryId(1)))]
    #[case::contiguous(vec![LedgerEntryId(1), LedgerEntryId(2)], Ok(LedgerEntryId(3)))]
    #[case::sparse(vec![LedgerEntryId(1), LedgerEntryId(3)], Ok(LedgerEntryId(4)))]
    #[case::unordered(vec![LedgerEntryId(20), LedgerEntryId(10)], Ok(LedgerEntryId(21)))]
    #[case::exhausted(vec![LedgerEntryId(u64::MAX)], Err(NextLedgerEntryIdError::Exhausted))]
    fn next_id_follows_maximum_existing_id(
        #[case] entry_ids: Vec<LedgerEntryId>,
        #[case] expected: Result<LedgerEntryId, NextLedgerEntryIdError>,
    ) {
        assert_eq!(next_ledger_entry_id(entry_ids), expected);
    }
}
