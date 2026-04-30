mod correction;
mod entry;
mod hash_chain;
mod load;
mod seal;

pub use walicord_ledger::{
    AdjustmentReason, AdjustmentReasonError, BalanceAdjusted, BalanceAdjustedError,
    BalanceAdjustment, BalanceAdjustmentSource, EntryAppendPosition, EntryVoided, ExpenseNote,
    ExpenseNoteError, ExpenseRecorded, ExpenseRecordedError, LedgerEntryId, LedgerEvent,
    LedgerHistorySealed, LedgerId, LedgerProjectionError, LedgerState, LedgerStructureError,
    MemberAmount, NormalizedSettlementPlanRecorded, NormalizedSettlementPlanRecordedError,
    ProjectedEntryIndex, ProjectedEntryInfo, ProjectedEntryKind, ProjectedLedger, SealedHistory,
};
// `AdminCorrectionAuthority` is intentionally **not** re-exported at the application
// boundary. This does not turn it into a true authority boundary — external code can
// still import it from `walicord-ledger` — but it keeps the ordinary application surface
// centered on the audited helper [`admin_external_balance_correction`] instead of the raw
// token. The few sites that still need to construct it directly (in-crate tests, future
// migration tooling) import it from `walicord-ledger`.
#[cfg(test)]
pub(crate) use walicord_ledger::AdminCorrectionAuthority;

pub use correction::{
    AdminExternalBalanceCorrectionError, StandardPriorAdjustmentCorrectionError,
    StandardSealedEntryCorrectionError, admin_external_balance_correction,
    standard_prior_adjustment_correction, standard_sealed_entry_correction,
};
pub use entry::{
    AllocationSnapshot, AllocationSnapshotError, AppendOrderedLedgerEntries,
    AppendOrderedLedgerEntriesError, LedgerEntry, LedgerEntryConstructionError,
    LedgerEntryMetadata, LedgerSourceCanonical, LedgerSourceCanonicalError,
    LedgerSourceCanonicalKind, MemberWeight, NonExpenseLedgerEvent,
};
pub use hash_chain::{
    ChainPositionError, EntryHash, HashedLedgerPayload, LedgerCanonicalEncodeError,
    LedgerHashChainError, LedgerHashSuite, SchemaVersion, UnverifiedLedgerStoreEnvelope,
    VerifiedLedgerStoreEnvelope, ledger_chain_genesis_sha256_v1, verify_envelope_sha256_v1,
    verify_envelopes_in_append_order_sha256_v1,
};
#[cfg(test)]
pub(crate) use hash_chain::{
    DefaultLedgerCanonicalEncoder, LedgerCanonicalEncoder, LedgerDigest, Sha256V1Digest,
    verify_envelope, verify_envelopes_in_append_order,
};
#[cfg(test)]
pub(crate) use load::load_and_replay_verified_with_custom_digest;
pub use load::{
    LedgerLoadError, LedgerProjector, LedgerReplayError, load_and_replay_verified_sha256_v1,
    replay_entries,
};
pub use seal::{
    SealThroughTailError, seal_through_latest_unvoided_expense_or_settlement_entry,
    seal_through_tail, seal_through_tail_if_advances,
};

#[cfg(test)]
mod tests;
