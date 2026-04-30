use super::{
    entry::{AppendOrderedLedgerEntries, AppendOrderedLedgerEntriesError, LedgerEntry},
    hash_chain::{
        ChainPositionError, DefaultLedgerCanonicalEncoder, EntryHash, LedgerCanonicalEncoder,
        LedgerDigest, Sha256V1Digest, UnverifiedLedgerStoreEnvelope, VerifiedLedgerStoreEnvelope,
        ledger_chain_genesis_sha256_v1, verify_envelopes_in_append_order,
    },
};
use walicord_ledger::{LedgerId, LedgerProjectionError, ProjectedLedger};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerReplayError {
    Structure(AppendOrderedLedgerEntriesError),
    Projection(LedgerProjectionError),
}

pub fn replay_entries(entries: Vec<LedgerEntry>) -> Result<ProjectedLedger, LedgerReplayError> {
    let entries = AppendOrderedLedgerEntries::new(entries).map_err(LedgerReplayError::Structure)?;
    LedgerProjector::replay(&entries).map_err(LedgerReplayError::Projection)
}

#[derive(Debug, Default)]
pub struct LedgerProjector;

impl LedgerProjector {
    pub fn replay(
        entries: &AppendOrderedLedgerEntries,
    ) -> Result<ProjectedLedger, LedgerProjectionError> {
        walicord_ledger::LedgerProjector::replay(&entries.records)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerLoadError {
    ChainVerification(ChainPositionError),
    Structure(AppendOrderedLedgerEntriesError),
    Projection(LedgerProjectionError),
}

/// Production load pipeline pinned to schema v1 + SHA-256 (the only suite supported by
/// `walicord-application` today). This is the API production code should call: it bakes
/// in `DefaultLedgerCanonicalEncoder`, `Sha256V1Digest`, and the deterministic genesis
/// policy from [`ledger_chain_genesis_sha256_v1`], so adapters cannot accidentally (or
/// deliberately) swap in a different digest or chain-start sentinel while still claiming
/// the same chain — the load path is suite-locked. Adding a new suite means adding a
/// sibling `load_and_replay_verified_<suite>` helper, not parameterizing this one.
///
/// For tests that need to exercise the chain-verification path with a stub digest, use
/// the crate-private `load_and_replay_verified_with_custom_digest` helper
/// (test/migration-only).
pub fn load_and_replay_verified_sha256_v1<ExternalId>(
    envelopes: Vec<UnverifiedLedgerStoreEnvelope<ExternalId>>,
    expected_ledger_id: LedgerId,
) -> Result<
    (
        Vec<VerifiedLedgerStoreEnvelope<ExternalId>>,
        ProjectedLedger,
    ),
    LedgerLoadError,
> {
    let chain_genesis = ledger_chain_genesis_sha256_v1(expected_ledger_id);
    load_and_replay_verified_with_custom_digest(
        envelopes,
        expected_ledger_id,
        chain_genesis,
        &DefaultLedgerCanonicalEncoder,
        &Sha256V1Digest,
    )
}

/// Generic load pipeline: chain-verify with the supplied encoder + digest, extract
/// entries, validate structural invariants, then project. Crate-private on purpose —
/// production callers must go through a suite-locked entry point (currently
/// [`load_and_replay_verified_sha256_v1`]) so digest selection happens at the API
/// surface, not at the call site. Available to in-crate tests and to future
/// suite-specific helpers (`load_and_replay_verified_<suite>`) that wrap it; if a
/// migration tool later needs a custom-digest entry, expose it via a feature gate
/// rather than re-publishing this function.
pub(crate) fn load_and_replay_verified_with_custom_digest<E, D, ExternalId>(
    envelopes: Vec<UnverifiedLedgerStoreEnvelope<ExternalId>>,
    expected_ledger_id: LedgerId,
    chain_genesis: EntryHash,
    encoder: &E,
    digest: &D,
) -> Result<
    (
        Vec<VerifiedLedgerStoreEnvelope<ExternalId>>,
        ProjectedLedger,
    ),
    LedgerLoadError,
>
where
    E: LedgerCanonicalEncoder,
    D: LedgerDigest,
{
    let verified = verify_envelopes_in_append_order(
        envelopes,
        expected_ledger_id,
        chain_genesis,
        encoder,
        digest,
    )
    .map_err(LedgerLoadError::ChainVerification)?;

    let entries: Vec<LedgerEntry> = verified
        .iter()
        .map(|envelope| envelope.payload().entry.clone())
        .collect();

    let ordered = AppendOrderedLedgerEntries::new(entries).map_err(LedgerLoadError::Structure)?;
    let projected = LedgerProjector::replay(&ordered).map_err(LedgerLoadError::Projection)?;

    Ok((verified, projected))
}
