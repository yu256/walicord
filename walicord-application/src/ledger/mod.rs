mod correction;
mod discord_authoring;
mod entry;
pub mod expense_flow;
pub mod expense_modal;
pub mod expense_session;
pub mod expense_write;
mod hash_chain;
mod load;
pub mod participant_resolution;
pub mod preview_store;
mod seal;
pub mod settle_flow;

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
    external_correction_source_for_transport_decode, standard_prior_adjustment_correction,
    standard_sealed_entry_correction,
};
pub use discord_authoring::{
    DiscordLedgerEntryError, DiscordLedgerSourceDescriptor, ExpenseAuthoringError,
    PreviewedSettlementOutcome, RecordableExpenseAuthoring, ResolvedExpenseAuthoringInput,
    SettlementPreviewError, SettlementRecordError, build_discord_expense_entry,
    build_discord_void_entry, compute_expense_owed_amounts, preview_settlement_from_snapshot,
    record_previewed_plan_matching,
};
pub use entry::{
    AllocationSnapshot, AllocationSnapshotError, AppendOrderedLedgerEntries,
    AppendOrderedLedgerEntriesError, LedgerEffectiveDate, LedgerEffectiveDateError, LedgerEntry,
    LedgerEntryConstructionError, LedgerEntryMetadata, LedgerSourceCanonical,
    LedgerSourceCanonicalError, LedgerSourceCanonicalKind, MemberWeight, NonExpenseLedgerEvent,
};
pub use hash_chain::{
    ChainPositionError, EntryHash, HashedLedgerPayload, LedgerCanonicalEncodeError,
    LedgerHashChainError, LedgerHashSuite, SchemaVersion, UnverifiedLedgerStoreEnvelope,
    VerifiedLedgerStoreEnvelope, ledger_chain_genesis_sha256_v1,
    make_unverified_envelope_sha256_v1, verify_envelope_sha256_v1,
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
    LEDGER_THREAD_GROWTH_WARNING_THRESHOLD, LedgerLoadError, LedgerProjector, LedgerReplayError,
    VerifiedLedgerSnapshot, load_and_replay_verified_sha256_v1, replay_entries,
    replay_verified_snapshot,
};
pub use seal::{
    SealThroughTailError, seal_through_latest_unvoided_expense_or_settlement_entry,
    seal_through_tail, seal_through_tail_if_advances,
};

#[cfg(test)]
mod tests;
