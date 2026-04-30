use super::entry::{
    AllocationSnapshot, LedgerEntry, LedgerEntryMetadata, LedgerSourceCanonical,
    LedgerSourceCanonicalKind, MemberWeight,
};
use walicord_domain::{Money, Transfer, model::MemberId};
use walicord_ledger::{
    BalanceAdjustment, BalanceAdjustmentSource, LedgerEvent, LedgerId, MemberAmount,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SchemaVersion(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntryHash(pub [u8; 32]);

/// Identifies which digest construction (canonical encoder version + cryptographic digest
/// function) a chain was built with. The suite is part of the canonical bytes that the
/// chain protects, so an attacker cannot silently downgrade a chain to a weaker digest by
/// rewriting transport metadata. The application-owned digest implementation declares its
/// suite via its internal `suite()` method, and the suite-locked public verifiers reject
/// mismatches before recomputing the hash.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LedgerHashSuite {
    /// SHA-256 over schema v1 canonical bytes.
    Sha256V1,
}

/// Canonical bytes that the hash chain protects: `ledger_id`, `schema_version`,
/// `hash_suite`, and the application `entry`. Including `schema_version` and
/// `hash_suite` inside the hashed input prevents silent reinterpretation of stored bytes
/// by a future schema or a downgraded digest. Including `ledger_id` binds the chain to
/// a specific ledger so an envelope copied from another ledger cannot be replayed into
/// this one even when the local `previous_hash`/`entry_hash` relationships are
/// individually consistent — without this binding, "this is the history of *this*
/// ledger" would rest only on out-of-band convention, not cryptographic evidence.
/// Store-specific transport facts (external id, raw embed, etc.) deliberately stay
/// outside the payload because the same ledger entry may be migrated between stores.
#[derive(Debug, Clone, PartialEq)]
pub struct HashedLedgerPayload {
    pub ledger_id: LedgerId,
    pub schema_version: SchemaVersion,
    pub hash_suite: LedgerHashSuite,
    pub entry: LedgerEntry,
}

/// Envelope as it sits in a store, before any hash-chain check. The contents may have
/// been tampered with or transcribed incorrectly; consumers must call a suite-locked
/// verifier such as [`verify_envelope_sha256_v1`] or
/// [`verify_envelopes_in_append_order_sha256_v1`] before trusting the hashes. Use
/// [`VerifiedLedgerStoreEnvelope`] for any code path that depends on chain integrity.
#[derive(Debug, Clone, PartialEq)]
pub struct UnverifiedLedgerStoreEnvelope<ExternalId> {
    pub previous_hash: EntryHash,
    pub entry_hash: EntryHash,
    pub external_id: ExternalId,
    pub payload: HashedLedgerPayload,
}

/// Envelope whose `entry_hash` has been recomputed from `previous_hash || canonical_payload`
/// and matched against the declared `entry_hash`, and whose `previous_hash` has been
/// compared against the expected chain head. This type can only be produced by the
/// suite-locked verification helpers, so holding one is structural proof of chain
/// integrity.
#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedLedgerStoreEnvelope<ExternalId>(UnverifiedLedgerStoreEnvelope<ExternalId>);

impl<ExternalId> VerifiedLedgerStoreEnvelope<ExternalId> {
    pub fn previous_hash(&self) -> EntryHash {
        self.0.previous_hash
    }

    pub fn entry_hash(&self) -> EntryHash {
        self.0.entry_hash
    }

    pub fn external_id(&self) -> &ExternalId {
        &self.0.external_id
    }

    pub fn payload(&self) -> &HashedLedgerPayload {
        &self.0.payload
    }

    /// Releases the inner envelope for use cases such as migration into a different store
    /// or re-serialization. The returned value is no longer carrying chain-integrity
    /// evidence: any code path that depends on tamper-evidence must re-verify it before use,
    /// rather than treating it as still-verified. Prefer accessor methods on the verified
    /// type for read paths so the verified status is preserved through the call graph.
    pub fn into_unverified(self) -> UnverifiedLedgerStoreEnvelope<ExternalId> {
        self.0
    }
}

/// Encodes the canonical byte sequence that the entry hash protects. **Crate-private**:
/// canonicalization drift between adapters is the deepest tamper-evidence risk, so the
/// canonical bytes are produced by a single application-owned implementation chosen by
/// `schema_version`. External crates do not see this trait — they call suite-locked
/// public functions (`verify_envelope_sha256_v1`, `load_and_replay_verified_sha256_v1`)
/// which compose the canonical encoder internally.
///
/// The canonical bytes always include `previous_hash` followed by a versioned encoding of
/// `payload`. Including `previous_hash` is what makes the chain a true tamper-evident
/// chain rather than a sequence of independent hashes joined by an unprotected pointer.
pub(crate) trait LedgerCanonicalEncoder {
    fn encode(
        &self,
        previous_hash: EntryHash,
        payload: &HashedLedgerPayload,
    ) -> Result<Vec<u8>, LedgerCanonicalEncodeError>;
}

/// Computes a digest from canonical bytes. **Crate-private**: external crates cannot
/// implement this trait, eliminating the failure mode where two adapters claim the same
/// `LedgerHashSuite` while silently producing different bytes. In-crate test stubs can
/// still plug in deterministic digests via this trait. Each implementer declares which
/// `LedgerHashSuite` its digest realizes via [`LedgerDigest::suite`]; verifying an
/// envelope whose payload declares a different suite is rejected up-front so chains
/// cannot be silently verified with the wrong cryptographic primitive.
pub(crate) trait LedgerDigest {
    fn suite(&self) -> LedgerHashSuite;
    fn digest(&self, bytes: &[u8]) -> EntryHash;
}

/// Standard digest for [`LedgerHashSuite::Sha256V1`]: SHA-256 over the schema v1
/// canonical bytes. Crate-private; external code cannot construct or pass this value
/// because the only verification entry points (`verify_envelope_sha256_v1`,
/// `verify_envelopes_in_append_order_sha256_v1`, `load_and_replay_verified_sha256_v1`)
/// instantiate it internally. Keeping the only production digest implementation here
/// removes the drift risk an adapter-private digest would otherwise create.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Sha256V1Digest;

impl LedgerDigest for Sha256V1Digest {
    fn suite(&self) -> LedgerHashSuite {
        LedgerHashSuite::Sha256V1
    }

    fn digest(&self, bytes: &[u8]) -> EntryHash {
        use sha2::{Digest as _, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(bytes);
        EntryHash(hasher.finalize().into())
    }
}

/// Deterministic genesis value for the first `previous_hash` in a
/// [`LedgerHashSuite::Sha256V1`] chain. Public suite-locked APIs derive the chain start
/// from `ledger_id` instead of accepting an adapter-chosen sentinel, so every verifier
/// in production applies the same genesis policy. The domain-separated hash makes the
/// first link ledger-bound even before the first payload is examined.
pub fn ledger_chain_genesis_sha256_v1(ledger_id: LedgerId) -> EntryHash {
    use sha2::{Digest as _, Sha256};

    let mut hasher = Sha256::new();
    hasher.update(b"walicord:ledger-chain-genesis:sha256-v1");
    hasher.update(ledger_id.0.to_be_bytes());
    EntryHash(hasher.finalize().into())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerCanonicalEncodeError {
    UnsupportedSchemaVersion {
        version: SchemaVersion,
    },
    UnsupportedHashSuite {
        suite: LedgerHashSuite,
    },
    /// A length field (collection cardinality or string byte length) does not fit in the
    /// schema's `u32` length prefix. Saturating would let two semantically distinct
    /// payloads collide on canonical bytes once both exceed `u32::MAX`, which would weaken
    /// tamper-evidence at the boundary. Real ledgers will not hit this in practice but the
    /// canonical format is treated as a strict total function: every input either has a
    /// well-defined encoding or fails loudly.
    LengthOverflow {
        len: usize,
    },
}

/// Default canonical encoder selected automatically by `schema_version`. Crate-private;
/// the suite-locked public verifiers instantiate this internally, so adapters never see
/// it. Adding a new schema version is an application-layer change: register the new
/// version here so all callers automatically pick up the same canonicalization without
/// coordination.
pub(crate) struct DefaultLedgerCanonicalEncoder;

impl LedgerCanonicalEncoder for DefaultLedgerCanonicalEncoder {
    fn encode(
        &self,
        previous_hash: EntryHash,
        payload: &HashedLedgerPayload,
    ) -> Result<Vec<u8>, LedgerCanonicalEncodeError> {
        match payload.schema_version {
            SchemaVersion(1) => encode_schema_v1(previous_hash, payload),
            other => Err(LedgerCanonicalEncodeError::UnsupportedSchemaVersion { version: other }),
        }
    }
}

/// Schema v1 canonical encoding. Field order, length prefixes, big-endian integer width,
/// `Decimal` 16-byte serialization, and option/discriminant byte values are all part of the
/// v1 contract — do not change them; introduce a v2 encoder and bump `schema_version`
/// instead. Schema v1 supports **exactly** hash suite [`LedgerHashSuite::Sha256V1`]; any
/// other suite is rejected up-front so the suite ↔ schema pairing is enforced by code,
/// not by doc convention. Adding a new suite is a deliberate v2 schema change.
///
/// `Money` values are normalized via [`rust_decimal::Decimal::normalize`] before being
/// serialized so that any two `Money` values that compare equal as values produce identical
/// canonical bytes — `Money::from_i64(100)` and a `Money` whose internal `Decimal` happens
/// to carry trailing zeros (e.g. `100.0`) hash to the same chain entry. Without this, the
/// hash semantics would diverge from `Money`'s value semantics, making chain identity
/// dependent on construction history rather than the value itself.
fn encode_schema_v1(
    previous_hash: EntryHash,
    payload: &HashedLedgerPayload,
) -> Result<Vec<u8>, LedgerCanonicalEncodeError> {
    // Exhaustive match locks the schema v1 ↔ hash suite pairing in code: adding a new
    // `LedgerHashSuite` variant becomes a compile error here, forcing the implementer
    // to either confirm v1 supports the new suite (add a `=> {}` arm) or reject it
    // (add a `=> return Err(LedgerCanonicalEncodeError::UnsupportedHashSuite { ... })`
    // arm). Without this match, "schema v1 supports exactly Sha256V1" would survive
    // only as a doc-comment claim.
    match payload.hash_suite {
        LedgerHashSuite::Sha256V1 => {}
    }
    let mut out = Vec::new();
    out.extend_from_slice(&previous_hash.0);
    out.push(hash_suite_v1_discriminant(payload.hash_suite));
    out.extend_from_slice(&payload.schema_version.0.to_be_bytes());
    out.extend_from_slice(&payload.ledger_id.0.to_be_bytes());
    out.extend_from_slice(&payload.entry.id.0.to_be_bytes());
    encode_metadata_v1(&mut out, &payload.entry.metadata)?;
    encode_event_v1(&mut out, &payload.entry.event)?;
    Ok(out)
}

fn hash_suite_v1_discriminant(suite: LedgerHashSuite) -> u8 {
    match suite {
        LedgerHashSuite::Sha256V1 => 1,
    }
}

/// Schema v1 view of [`LedgerEntryMetadata`]. The fields here freeze what schema v1's
/// canonical bytes cover; converting from the live struct uses an **exhaustive destructure
/// without `..`**, so any new field added to `LedgerEntryMetadata` causes a compile error
/// here — forcing the implementer to make a deliberate choice:
///
/// 1. add the field to `MetadataV1Hashed` and update [`encode_metadata_v1`] (extends what
///    schema v1 hashes — only safe before any v1 chain has been published), or
/// 2. introduce a `MetadataV2Hashed` DTO and a v2 encoder, bump `SchemaVersion`, and route
///    new entries through v2 (the standard path once v1 is in production).
///
/// Without this DTO, a forgotten field would silently fall outside the hash without any
/// compiler signal — that is the exact failure mode this design prevents.
struct MetadataV1Hashed<'a> {
    recorded_by: Option<MemberId>,
    source: Option<&'a LedgerSourceCanonical>,
    allocation_snapshot: Option<&'a AllocationSnapshot>,
}

impl<'a> From<&'a LedgerEntryMetadata> for MetadataV1Hashed<'a> {
    fn from(metadata: &'a LedgerEntryMetadata) -> Self {
        // Do NOT add `..` here — see `MetadataV1Hashed` doc-comment above.
        let LedgerEntryMetadata {
            recorded_by,
            source,
            allocation_snapshot,
        } = metadata;
        Self {
            recorded_by: *recorded_by,
            source: source.as_ref(),
            allocation_snapshot: allocation_snapshot.as_ref(),
        }
    }
}

fn encode_metadata_v1(
    out: &mut Vec<u8>,
    metadata: &LedgerEntryMetadata,
) -> Result<(), LedgerCanonicalEncodeError> {
    let MetadataV1Hashed {
        recorded_by,
        source,
        allocation_snapshot,
    } = MetadataV1Hashed::from(metadata);
    encode_optional_member(out, recorded_by);
    encode_optional_source_v1(out, source)?;
    encode_optional_allocation_snapshot_v1(out, allocation_snapshot)?;
    Ok(())
}

fn encode_optional_source_v1(
    out: &mut Vec<u8>,
    source: Option<&LedgerSourceCanonical>,
) -> Result<(), LedgerCanonicalEncodeError> {
    match source {
        None => out.push(0),
        Some(source) => {
            out.push(1);
            out.push(source_kind_v1_discriminant(source.kind()));
            encode_string(out, source.as_str())?;
        }
    }
    Ok(())
}

fn source_kind_v1_discriminant(kind: LedgerSourceCanonicalKind) -> u8 {
    match kind {
        LedgerSourceCanonicalKind::LegacyDsl => 1,
    }
}

/// Schema v1 canonical encoding of `Option<AllocationSnapshot>`. Replaces the earlier
/// length-prefixed UTF-8 encoding of a free-form summary string. Layout:
///
/// * 1 byte option discriminant: 0 = None, 1 = Some.
/// * 1 byte snapshot variant: 1 = Even, 2 = Weighted, 3 = LegacyUnknown (only present
///   when option = Some).
/// * For Weighted: u32-be count, then for each weight `(u64-be member_id, u64-be weight)`,
///   in `MemberId` order (the constructor canonicalizes order, so encoding can rely on it).
fn encode_optional_allocation_snapshot_v1(
    out: &mut Vec<u8>,
    snapshot: Option<&AllocationSnapshot>,
) -> Result<(), LedgerCanonicalEncodeError> {
    match snapshot {
        None => out.push(0),
        Some(AllocationSnapshot::Even) => {
            out.push(1);
            out.push(1);
        }
        Some(AllocationSnapshot::Weighted { resolved_weights }) => {
            out.push(1);
            out.push(2);
            out.extend_from_slice(&u32_be_len(resolved_weights.len())?);
            for MemberWeight { member_id, weight } in resolved_weights {
                out.extend_from_slice(&member_id.0.to_be_bytes());
                out.extend_from_slice(&weight.0.to_be_bytes());
            }
        }
        Some(AllocationSnapshot::LegacyUnknown) => {
            out.push(1);
            out.push(3);
        }
    }
    Ok(())
}

fn encode_optional_member(out: &mut Vec<u8>, member: Option<MemberId>) {
    match member {
        Some(m) => {
            out.push(1);
            out.extend_from_slice(&m.0.to_be_bytes());
        }
        None => out.push(0),
    }
}

fn encode_optional_string(
    out: &mut Vec<u8>,
    s: Option<&str>,
) -> Result<(), LedgerCanonicalEncodeError> {
    match s {
        Some(text) => {
            out.push(1);
            encode_string(out, text)?;
        }
        None => out.push(0),
    }
    Ok(())
}

fn encode_string(out: &mut Vec<u8>, s: &str) -> Result<(), LedgerCanonicalEncodeError> {
    let bytes = s.as_bytes();
    out.extend_from_slice(&u32_be_len(bytes.len())?);
    out.extend_from_slice(bytes);
    Ok(())
}

fn u32_be_len(len: usize) -> Result<[u8; 4], LedgerCanonicalEncodeError> {
    u32::try_from(len)
        .map(u32::to_be_bytes)
        .map_err(|_| LedgerCanonicalEncodeError::LengthOverflow { len })
}

/// Canonical 16-byte representation of a `Money` value. `Decimal::normalize` strips
/// trailing zeros and converts -0 to +0, ensuring semantically equal amounts share
/// canonical bytes regardless of how the `Decimal` was constructed.
fn encode_money_v1(out: &mut Vec<u8>, money: Money) {
    out.extend_from_slice(&money.as_decimal().normalize().serialize());
}

fn encode_event_v1(
    out: &mut Vec<u8>,
    event: &LedgerEvent,
) -> Result<(), LedgerCanonicalEncodeError> {
    let discriminant: u8 = match event {
        LedgerEvent::ExpenseRecorded(_) => 1,
        LedgerEvent::NormalizedSettlementPlanRecorded(_) => 2,
        LedgerEvent::LedgerHistorySealed(_) => 3,
        LedgerEvent::EntryVoided(_) => 4,
        LedgerEvent::BalanceAdjusted(_) => 5,
    };
    out.push(discriminant);

    match event {
        LedgerEvent::ExpenseRecorded(e) => {
            encode_member_amounts(out, e.paid_by())?;
            encode_member_amounts(out, e.owed_by())?;
            encode_optional_string(out, e.note().map(|n| n.as_str()))?;
        }
        LedgerEvent::NormalizedSettlementPlanRecorded(e) => {
            encode_transfers(out, e.transfers())?;
        }
        LedgerEvent::LedgerHistorySealed(e) => {
            out.extend_from_slice(&e.through().0.to_be_bytes());
        }
        LedgerEvent::EntryVoided(e) => {
            out.extend_from_slice(&e.target().0.to_be_bytes());
        }
        LedgerEvent::BalanceAdjusted(e) => {
            encode_balance_adjustments(out, e.adjustments())?;
            encode_string(out, e.reason().as_str())?;
            encode_balance_adjustment_source(out, e.source());
        }
    }
    Ok(())
}

fn encode_member_amounts(
    out: &mut Vec<u8>,
    amounts: &[MemberAmount],
) -> Result<(), LedgerCanonicalEncodeError> {
    out.extend_from_slice(&u32_be_len(amounts.len())?);
    for amount in amounts {
        out.extend_from_slice(&amount.member_id.0.to_be_bytes());
        encode_money_v1(out, amount.amount);
    }
    Ok(())
}

fn encode_transfers(
    out: &mut Vec<u8>,
    transfers: &[Transfer],
) -> Result<(), LedgerCanonicalEncodeError> {
    out.extend_from_slice(&u32_be_len(transfers.len())?);
    for transfer in transfers {
        out.extend_from_slice(&transfer.from.0.to_be_bytes());
        out.extend_from_slice(&transfer.to.0.to_be_bytes());
        encode_money_v1(out, transfer.amount);
    }
    Ok(())
}

fn encode_balance_adjustments(
    out: &mut Vec<u8>,
    adjustments: &[BalanceAdjustment],
) -> Result<(), LedgerCanonicalEncodeError> {
    out.extend_from_slice(&u32_be_len(adjustments.len())?);
    for adjustment in adjustments {
        out.extend_from_slice(&adjustment.member_id.0.to_be_bytes());
        encode_money_v1(out, adjustment.amount);
    }
    Ok(())
}

fn encode_balance_adjustment_source(out: &mut Vec<u8>, source: &BalanceAdjustmentSource) {
    match source {
        // The capability token carries no data — only its presence is required to
        // construct the variant — so it does not contribute to canonical bytes.
        BalanceAdjustmentSource::ExternalCorrection(_) => out.push(0),
        BalanceAdjustmentSource::SealedEntryCorrection(id) => {
            out.push(1);
            out.extend_from_slice(&id.0.to_be_bytes());
        }
        BalanceAdjustmentSource::PriorAdjustmentCorrection(id) => {
            out.push(2);
            out.extend_from_slice(&id.0.to_be_bytes());
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerHashChainError {
    PreviousHashMismatch {
        expected: EntryHash,
        declared: EntryHash,
    },
    EntryHashMismatch {
        computed: EntryHash,
        declared: EntryHash,
    },
    Encoding(LedgerCanonicalEncodeError),
    /// The digest implementation declares a different `LedgerHashSuite` than the payload's
    /// declared suite. Indicates that the wrong digest is being used to verify this chain
    /// (or the payload was rewritten to claim a different suite).
    HashSuiteMismatch {
        expected: LedgerHashSuite,
        declared: LedgerHashSuite,
    },
    /// The envelope's payload claims a different `LedgerId` than the verifier was asked
    /// to validate. Surfaced when chain-internal invariants would still pass — i.e. the
    /// envelope was a valid record on *some* ledger, but not on this one — so silent
    /// cross-ledger replay attempts cannot succeed.
    LedgerIdMismatch {
        expected: LedgerId,
        declared: LedgerId,
    },
}

/// Promotes an unverified envelope to a verified one by:
/// 1. matching `previous_hash` against the expected chain head,
/// 2. checking that the digest implementation's declared [`LedgerHashSuite`] matches the
///    payload's declared suite,
/// 3. recomputing `entry_hash` as `digest(encoder.encode(previous_hash, payload))` and
///    comparing it to the declared `entry_hash`.
///
/// **Crate-private**: production callers must use [`verify_envelope_sha256_v1`] (or the
/// load pipeline equivalents) so digest selection happens at the public API surface, not
/// at the call site. This generic form is reachable from in-crate tests and suite-locked
/// helpers only. Splitting the encoder (application-owned) from the digest (also
/// application-owned via [`Sha256V1Digest`]) means adapters cannot drift on canonical
/// bytes; pinning `LedgerHashSuite` inside the canonical bytes means an attacker cannot
/// silently swap the chain's digest function.
pub(crate) fn verify_envelope<E, D, ExternalId>(
    unverified: UnverifiedLedgerStoreEnvelope<ExternalId>,
    expected_ledger_id: LedgerId,
    expected_previous_hash: EntryHash,
    encoder: &E,
    digest: &D,
) -> Result<VerifiedLedgerStoreEnvelope<ExternalId>, LedgerHashChainError>
where
    E: LedgerCanonicalEncoder,
    D: LedgerDigest,
{
    if unverified.payload.ledger_id != expected_ledger_id {
        return Err(LedgerHashChainError::LedgerIdMismatch {
            expected: expected_ledger_id,
            declared: unverified.payload.ledger_id,
        });
    }

    if unverified.previous_hash != expected_previous_hash {
        return Err(LedgerHashChainError::PreviousHashMismatch {
            expected: expected_previous_hash,
            declared: unverified.previous_hash,
        });
    }

    if digest.suite() != unverified.payload.hash_suite {
        return Err(LedgerHashChainError::HashSuiteMismatch {
            expected: unverified.payload.hash_suite,
            declared: digest.suite(),
        });
    }

    let canonical_bytes = encoder
        .encode(unverified.previous_hash, &unverified.payload)
        .map_err(LedgerHashChainError::Encoding)?;
    let computed = digest.digest(&canonical_bytes);
    if computed != unverified.entry_hash {
        return Err(LedgerHashChainError::EntryHashMismatch {
            computed,
            declared: unverified.entry_hash,
        });
    }

    Ok(VerifiedLedgerStoreEnvelope(unverified))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChainPositionError {
    pub position: usize,
    pub error: LedgerHashChainError,
}

/// Verifies a sequence of envelopes given in append order against a single chain.
/// **Crate-private**: production callers use [`verify_envelopes_in_append_order_sha256_v1`].
/// The first envelope's `previous_hash` must equal `chain_genesis`; each subsequent
/// envelope's `previous_hash` must equal the prior envelope's recomputed `entry_hash`.
/// Stops at the first failing envelope and returns its append-order position alongside
/// the underlying [`LedgerHashChainError`].
pub(crate) fn verify_envelopes_in_append_order<E, D, ExternalId>(
    envelopes: Vec<UnverifiedLedgerStoreEnvelope<ExternalId>>,
    expected_ledger_id: LedgerId,
    chain_genesis: EntryHash,
    encoder: &E,
    digest: &D,
) -> Result<Vec<VerifiedLedgerStoreEnvelope<ExternalId>>, ChainPositionError>
where
    E: LedgerCanonicalEncoder,
    D: LedgerDigest,
{
    let mut expected_previous_hash = chain_genesis;
    let mut verified = Vec::with_capacity(envelopes.len());

    for (position, envelope) in envelopes.into_iter().enumerate() {
        match verify_envelope(
            envelope,
            expected_ledger_id,
            expected_previous_hash,
            encoder,
            digest,
        ) {
            Ok(envelope) => {
                expected_previous_hash = envelope.entry_hash();
                verified.push(envelope);
            }
            Err(error) => return Err(ChainPositionError { position, error }),
        }
    }

    Ok(verified)
}

/// Public, suite-locked verifier for the **first** envelope in a chain. Composes the
/// application-canonical encoder + `Sha256V1Digest` internally and derives the genesis
/// `previous_hash` from [`ledger_chain_genesis_sha256_v1`], so external callers cannot
/// supply alternative implementations or choose a different chain-start sentinel. The
/// `expected_ledger_id` is the cryptographic binding of "this is the history of *this*
/// ledger" — an envelope from another ledger fails verification even if the bytes are
/// individually well-formed. Adding a new suite is a deliberate change here, not at any
/// call site.
pub fn verify_envelope_sha256_v1<ExternalId>(
    unverified: UnverifiedLedgerStoreEnvelope<ExternalId>,
    expected_ledger_id: LedgerId,
) -> Result<VerifiedLedgerStoreEnvelope<ExternalId>, LedgerHashChainError> {
    let expected_previous_hash = ledger_chain_genesis_sha256_v1(expected_ledger_id);
    verify_envelope(
        unverified,
        expected_ledger_id,
        expected_previous_hash,
        &DefaultLedgerCanonicalEncoder,
        &Sha256V1Digest,
    )
}

/// Public, suite-locked append-order verifier. Same suite-locking + ledger-id binding
/// rationale as [`verify_envelope_sha256_v1`]. The first envelope is checked against the
/// deterministic genesis returned by [`ledger_chain_genesis_sha256_v1`]; adapters do not
/// choose their own chain-start sentinel.
pub fn verify_envelopes_in_append_order_sha256_v1<ExternalId>(
    envelopes: Vec<UnverifiedLedgerStoreEnvelope<ExternalId>>,
    expected_ledger_id: LedgerId,
) -> Result<Vec<VerifiedLedgerStoreEnvelope<ExternalId>>, ChainPositionError> {
    let chain_genesis = ledger_chain_genesis_sha256_v1(expected_ledger_id);
    verify_envelopes_in_append_order(
        envelopes,
        expected_ledger_id,
        chain_genesis,
        &DefaultLedgerCanonicalEncoder,
        &Sha256V1Digest,
    )
}

#[cfg(test)]
mod schema_v1_tests {
    use super::*;
    use walicord_domain::{Money, Transfer, model::MemberId};
    use walicord_ledger::{
        AdjustmentReason, AdminCorrectionAuthority, BalanceAdjusted, BalanceAdjustment,
        BalanceAdjustmentSource, EntryVoided, ExpenseNote, ExpenseRecorded, LedgerEntryId,
        LedgerEvent, LedgerHistorySealed, MemberAmount, NormalizedSettlementPlanRecorded,
    };

    fn encode_schema_v1_unwrap(previous_hash: EntryHash, payload: &HashedLedgerPayload) -> Vec<u8> {
        encode_schema_v1(previous_hash, payload).expect("schema v1 encode should succeed in tests")
    }

    fn payload_with_event(id: u64, event: LedgerEvent) -> HashedLedgerPayload {
        HashedLedgerPayload {
            ledger_id: LedgerId(0),
            schema_version: SchemaVersion(1),
            hash_suite: LedgerHashSuite::Sha256V1,
            entry: match event {
                LedgerEvent::ExpenseRecorded(expense) => {
                    LedgerEntry::legacy_expense_without_allocation_snapshot(
                        LedgerEntryId(id),
                        expense,
                    )
                }
                LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
                    LedgerEntry::non_expense(LedgerEntryId(id), event)
                }
                LedgerEvent::LedgerHistorySealed(event) => {
                    LedgerEntry::non_expense(LedgerEntryId(id), event)
                }
                LedgerEvent::EntryVoided(event) => {
                    LedgerEntry::non_expense(LedgerEntryId(id), event)
                }
                LedgerEvent::BalanceAdjusted(event) => {
                    LedgerEntry::non_expense(LedgerEntryId(id), event)
                }
            },
        }
    }

    fn expense_recorded(paid: u64, owed: u64, amount: i64) -> ExpenseRecorded {
        ExpenseRecorded::new(
            vec![MemberAmount {
                member_id: MemberId(paid),
                amount: Money::from_i64(amount),
            }],
            vec![MemberAmount {
                member_id: MemberId(owed),
                amount: Money::from_i64(amount),
            }],
            None,
        )
        .expect("expense should be valid")
    }

    fn expense_event(paid: u64, owed: u64, amount: i64) -> LedgerEvent {
        LedgerEvent::ExpenseRecorded(expense_recorded(paid, owed, amount))
    }

    #[test]
    fn schema_v1_distinguishes_different_amounts() {
        // Same entry id, same paid_by/owed_by members, different amount -> different bytes.
        let bytes_100 = encode_schema_v1(
            EntryHash([0; 32]),
            &payload_with_event(1, expense_event(1, 2, 100)),
        );
        let bytes_200 = encode_schema_v1(
            EntryHash([0; 32]),
            &payload_with_event(1, expense_event(1, 2, 200)),
        );
        assert_ne!(bytes_100, bytes_200);
    }

    #[test]
    fn schema_v1_distinguishes_different_participants() {
        let bytes_a = encode_schema_v1(
            EntryHash([0; 32]),
            &payload_with_event(1, expense_event(1, 2, 100)),
        );
        let bytes_b = encode_schema_v1(
            EntryHash([0; 32]),
            &payload_with_event(1, expense_event(3, 2, 100)),
        );
        assert_ne!(bytes_a, bytes_b);
    }

    #[test]
    fn schema_v1_distinguishes_note_changes() {
        let with_note = LedgerEvent::ExpenseRecorded(
            ExpenseRecorded::new(
                vec![MemberAmount {
                    member_id: MemberId(1),
                    amount: Money::from_i64(100),
                }],
                vec![MemberAmount {
                    member_id: MemberId(2),
                    amount: Money::from_i64(100),
                }],
                Some(ExpenseNote::new("lunch").expect("note should be valid")),
            )
            .expect("expense should be valid"),
        );
        let without_note = expense_event(1, 2, 100);
        assert_ne!(
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(1, with_note)),
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(1, without_note)),
        );
    }

    #[test]
    fn schema_v1_distinguishes_settlement_transfers() {
        let plan_a = LedgerEvent::NormalizedSettlementPlanRecorded(
            NormalizedSettlementPlanRecorded::new(vec![Transfer {
                from: MemberId(1),
                to: MemberId(2),
                amount: Money::from_i64(50),
            }])
            .expect("plan should be valid"),
        );
        let plan_b = LedgerEvent::NormalizedSettlementPlanRecorded(
            NormalizedSettlementPlanRecorded::new(vec![Transfer {
                from: MemberId(1),
                to: MemberId(2),
                amount: Money::from_i64(60),
            }])
            .expect("plan should be valid"),
        );
        assert_ne!(
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(1, plan_a)),
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(1, plan_b)),
        );
    }

    #[test]
    fn schema_v1_distinguishes_seal_targets() {
        let seal_a = LedgerEvent::LedgerHistorySealed(LedgerHistorySealed::new(LedgerEntryId(1)));
        let seal_b = LedgerEvent::LedgerHistorySealed(LedgerHistorySealed::new(LedgerEntryId(2)));
        assert_ne!(
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(99, seal_a)),
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(99, seal_b)),
        );
    }

    #[test]
    fn schema_v1_distinguishes_void_targets() {
        let void_a = LedgerEvent::EntryVoided(EntryVoided::new(LedgerEntryId(1)));
        let void_b = LedgerEvent::EntryVoided(EntryVoided::new(LedgerEntryId(2)));
        assert_ne!(
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(99, void_a)),
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(99, void_b)),
        );
    }

    #[test]
    fn schema_v1_distinguishes_adjustment_reason_and_source() {
        let adjustment_a = LedgerEvent::BalanceAdjusted(
            BalanceAdjusted::new(
                vec![
                    BalanceAdjustment {
                        member_id: MemberId(1),
                        amount: Money::from_i64(-25),
                    },
                    BalanceAdjustment {
                        member_id: MemberId(2),
                        amount: Money::from_i64(25),
                    },
                ],
                AdjustmentReason::new("first reason").expect("reason should be valid"),
                BalanceAdjustmentSource::ExternalCorrection(
                    AdminCorrectionAuthority::explicit_admin_capability(),
                ),
            )
            .expect("adjustment should be valid"),
        );
        let adjustment_b = LedgerEvent::BalanceAdjusted(
            BalanceAdjusted::new(
                vec![
                    BalanceAdjustment {
                        member_id: MemberId(1),
                        amount: Money::from_i64(-25),
                    },
                    BalanceAdjustment {
                        member_id: MemberId(2),
                        amount: Money::from_i64(25),
                    },
                ],
                AdjustmentReason::new("different reason").expect("reason should be valid"),
                BalanceAdjustmentSource::ExternalCorrection(
                    AdminCorrectionAuthority::explicit_admin_capability(),
                ),
            )
            .expect("adjustment should be valid"),
        );
        let adjustment_c = LedgerEvent::BalanceAdjusted(
            BalanceAdjusted::new(
                vec![
                    BalanceAdjustment {
                        member_id: MemberId(1),
                        amount: Money::from_i64(-25),
                    },
                    BalanceAdjustment {
                        member_id: MemberId(2),
                        amount: Money::from_i64(25),
                    },
                ],
                AdjustmentReason::new("first reason").expect("reason should be valid"),
                BalanceAdjustmentSource::SealedEntryCorrection(LedgerEntryId(7)),
            )
            .expect("adjustment should be valid"),
        );

        let bytes_a =
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(99, adjustment_a));
        let bytes_b =
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(99, adjustment_b));
        let bytes_c =
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_event(99, adjustment_c));
        assert_ne!(bytes_a, bytes_b);
        assert_ne!(bytes_a, bytes_c);
    }

    #[test]
    fn schema_v1_distinguishes_metadata_changes() {
        let mut payload_with_meta = payload_with_event(1, expense_event(1, 2, 100));
        let payload_no_meta = payload_with_meta.clone();
        payload_with_meta.entry.metadata.recorded_by = Some(MemberId(9));
        // payload_no_meta keeps recorded_by = None
        assert!(payload_no_meta.entry.metadata.recorded_by.is_none());

        let bytes_a = encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_with_meta);
        let bytes_b = encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_no_meta);
        assert_ne!(bytes_a, bytes_b);
    }

    #[test]
    fn schema_v1_source_encoding_includes_source_kind_discriminant() {
        let source =
            LedgerSourceCanonical::legacy_dsl("/expense 100").expect("source should parse");
        let mut encoded = Vec::new();

        encode_optional_source_v1(&mut encoded, Some(&source))
            .expect("source encoding should succeed");

        assert_eq!(encoded[0], 1, "source option discriminant should be Some");
        assert_eq!(
            encoded[1], 1,
            "legacy DSL source kind should be encoded explicitly"
        );
        assert_eq!(
            &encoded[2..6],
            &u32::try_from(source.canonical_text().len())
                .expect("source length should fit in u32")
                .to_be_bytes()
        );
        assert_eq!(&encoded[6..], source.canonical_text().as_bytes());
    }

    #[test]
    fn schema_v1_distinguishes_allocation_snapshot_variants() {
        use walicord_domain::model::Weight;
        // Even, Weighted, and a different Weighted layout must all produce different
        // canonical bytes. This is the type-ized replacement for the old
        // `AllocationSummary` string comparison: chain identity now reflects the
        // *structured* allocation, not surface text.
        let mut payload_even = payload_with_event(1, expense_event(1, 2, 100));
        payload_even.entry.metadata.allocation_snapshot = Some(AllocationSnapshot::Even);

        let mut payload_weighted_a = payload_with_event(1, expense_event(1, 2, 100));
        payload_weighted_a.entry.metadata.allocation_snapshot = Some(
            AllocationSnapshot::weighted([
                MemberWeight {
                    member_id: MemberId(1),
                    weight: Weight(2),
                },
                MemberWeight {
                    member_id: MemberId(2),
                    weight: Weight(1),
                },
            ])
            .expect("weighted snapshot should be valid"),
        );

        let mut payload_weighted_b = payload_with_event(1, expense_event(1, 2, 100));
        payload_weighted_b.entry.metadata.allocation_snapshot = Some(
            AllocationSnapshot::weighted([
                MemberWeight {
                    member_id: MemberId(1),
                    weight: Weight(1),
                },
                MemberWeight {
                    member_id: MemberId(2),
                    weight: Weight(1),
                },
            ])
            .expect("weighted snapshot should be valid"),
        );

        let bytes_even = encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_even);
        let bytes_weighted_a = encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_weighted_a);
        let bytes_weighted_b = encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_weighted_b);

        // Different variants -> different bytes.
        assert_ne!(bytes_even, bytes_weighted_a);
        // Same variant but different weights -> different bytes.
        assert_ne!(bytes_weighted_a, bytes_weighted_b);
    }

    #[test]
    fn schema_v1_canonicalizes_weighted_snapshot_member_order() {
        use walicord_domain::model::Weight;
        // The constructor sorts by `MemberId`, so callers passing the same logical
        // weights in different iteration orders must produce identical canonical bytes —
        // chain identity must not depend on caller-side ordering of the input vector.
        let weights_one = AllocationSnapshot::weighted([
            MemberWeight {
                member_id: MemberId(1),
                weight: Weight(2),
            },
            MemberWeight {
                member_id: MemberId(2),
                weight: Weight(1),
            },
        ])
        .expect("weighted snapshot should be valid");
        let weights_two = AllocationSnapshot::weighted([
            MemberWeight {
                member_id: MemberId(2),
                weight: Weight(1),
            },
            MemberWeight {
                member_id: MemberId(1),
                weight: Weight(2),
            },
        ])
        .expect("weighted snapshot should be valid");

        let mut payload_one = payload_with_event(1, expense_event(1, 2, 100));
        payload_one.entry.metadata.allocation_snapshot = Some(weights_one);
        let mut payload_two = payload_with_event(1, expense_event(1, 2, 100));
        payload_two.entry.metadata.allocation_snapshot = Some(weights_two);

        assert_eq!(
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_one),
            encode_schema_v1_unwrap(EntryHash([0; 32]), &payload_two),
        );
    }

    #[test]
    fn schema_v1_canonicalizes_equivalent_money_values() {
        use rust_decimal::Decimal;
        use std::str::FromStr;

        // Two `Money` instances representing the same value but with different internal
        // `Decimal` scales (the second was multiplied by `Decimal "1.00"`, leaving a
        // scale-2 representation with trailing zeros). The hashed canonical bytes must be
        // identical so chain identity reflects value semantics, not the construction
        // history of the underlying `Decimal`. Without normalization, two events that
        // pay the same yen amount could produce different chain entries depending on
        // upstream arithmetic — making chain comparisons across migrations fragile.
        let normalized = Money::from_i64(100);
        let with_trailing_zeros =
            normalized * Decimal::from_str("1.00").expect("decimal literal should parse");

        // Sanity: as values they are equal, but their underlying `Decimal::serialize`
        // bytes are different (this is what we are normalizing away).
        assert_eq!(normalized, with_trailing_zeros);
        assert_ne!(
            normalized.as_decimal().serialize(),
            with_trailing_zeros.as_decimal().serialize(),
        );

        let bytes_a = encode_schema_v1_unwrap(
            EntryHash([0; 32]),
            &payload_with_event(
                1,
                LedgerEvent::ExpenseRecorded(
                    ExpenseRecorded::new(
                        vec![MemberAmount {
                            member_id: MemberId(1),
                            amount: normalized,
                        }],
                        vec![MemberAmount {
                            member_id: MemberId(2),
                            amount: normalized,
                        }],
                        None,
                    )
                    .expect("expense should be valid"),
                ),
            ),
        );
        let bytes_b = encode_schema_v1_unwrap(
            EntryHash([0; 32]),
            &payload_with_event(
                1,
                LedgerEvent::ExpenseRecorded(
                    ExpenseRecorded::new(
                        vec![MemberAmount {
                            member_id: MemberId(1),
                            amount: with_trailing_zeros,
                        }],
                        vec![MemberAmount {
                            member_id: MemberId(2),
                            amount: with_trailing_zeros,
                        }],
                        None,
                    )
                    .expect("expense should be valid"),
                ),
            ),
        );
        assert_eq!(bytes_a, bytes_b);
    }

    #[test]
    fn sha256_v1_digest_is_deterministic_and_pure_function_of_input() {
        // Stability of the canonical digest is the load-bearing property: if the same
        // bytes ever produced a different `EntryHash`, every chain that contains that
        // entry would fail verification. Pin both determinism and input-dependence.
        let digest = Sha256V1Digest;
        let bytes = b"walicord:ledger:test";
        assert_eq!(digest.digest(bytes), digest.digest(bytes));
        assert_ne!(digest.digest(bytes), digest.digest(b"different"));
        assert_eq!(digest.suite(), LedgerHashSuite::Sha256V1);
    }

    #[test]
    fn sha256_v1_digest_matches_known_sha256_of_canonical_bytes() {
        // Sanity: the digest is plain SHA-256 over the input. Encodes the v1 suite
        // identity contract — if anyone changes `Sha256V1Digest` to use a different
        // primitive, every existing chain becomes unverifiable.
        use sha2::{Digest as _, Sha256};
        let bytes = b"abc";
        let expected: [u8; 32] = Sha256::digest(bytes).into();
        assert_eq!(Sha256V1Digest.digest(bytes).0, expected);
    }

    #[test]
    fn ledger_chain_genesis_sha256_v1_is_deterministic_and_ledger_bound() {
        let ledger_zero_a = ledger_chain_genesis_sha256_v1(LedgerId(0));
        let ledger_zero_b = ledger_chain_genesis_sha256_v1(LedgerId(0));
        let ledger_one = ledger_chain_genesis_sha256_v1(LedgerId(1));

        assert_eq!(ledger_zero_a, ledger_zero_b);
        assert_ne!(ledger_zero_a, ledger_one);
    }

    #[test]
    fn verify_envelope_sha256_v1_round_trips_without_explicit_digest() {
        // The suite-locked public API does not expose `LedgerDigest` or
        // `LedgerCanonicalEncoder` to the caller — verifying that the round-trip works
        // through the public surface is the regression check for "adapters cannot drift
        // on canonical bytes".
        let ledger_id = LedgerId(0);
        let payload = payload_with_event(1, expense_event(1, 2, 100));
        let previous = ledger_chain_genesis_sha256_v1(ledger_id);
        let bytes = encode_schema_v1_unwrap(previous, &payload);
        let entry_hash = Sha256V1Digest.digest(&bytes);

        let unverified = UnverifiedLedgerStoreEnvelope {
            previous_hash: previous,
            entry_hash,
            external_id: (),
            payload,
        };
        let verified = verify_envelope_sha256_v1(unverified, ledger_id)
            .expect("suite-locked verifier should accept a properly hashed envelope");
        assert_eq!(verified.entry_hash(), entry_hash);
    }

    #[test]
    fn verify_envelope_sha256_v1_rejects_envelope_from_a_different_ledger() {
        // Cross-ledger replay protection: the envelope's payload claims `LedgerId(7)` but
        // the verifier is asked to validate it as part of `LedgerId(42)`. Even though the
        // entry_hash is correctly computed for ledger 7, verification must refuse to
        // promote it to a verified envelope on ledger 42 — this is the structural
        // guarantee that an envelope copied from another ledger cannot pass verification
        // here even when its internal previous_hash/entry_hash relationships are
        // individually consistent.
        let payload = HashedLedgerPayload {
            ledger_id: LedgerId(7),
            schema_version: SchemaVersion(1),
            hash_suite: LedgerHashSuite::Sha256V1,
            entry: LedgerEntry::expense(
                LedgerEntryId(1),
                expense_recorded(1, 2, 100),
                AllocationSnapshot::Even,
            )
            .expect("authored expense should accept an explicit structured snapshot"),
        };
        let previous = ledger_chain_genesis_sha256_v1(LedgerId(7));
        let bytes = encode_schema_v1_unwrap(previous, &payload);
        let entry_hash = Sha256V1Digest.digest(&bytes);

        let unverified = UnverifiedLedgerStoreEnvelope {
            previous_hash: previous,
            entry_hash,
            external_id: (),
            payload,
        };

        let actual = verify_envelope_sha256_v1(unverified, LedgerId(42));

        assert_eq!(
            actual,
            Err(LedgerHashChainError::LedgerIdMismatch {
                expected: LedgerId(42),
                declared: LedgerId(7),
            })
        );
    }

    #[test]
    fn verify_envelope_round_trips_through_sha256_v1() {
        let payload = payload_with_event(1, expense_event(1, 2, 100));
        let previous = EntryHash([0; 32]);
        let bytes = encode_schema_v1_unwrap(previous, &payload);
        let entry_hash = Sha256V1Digest.digest(&bytes);

        let unverified = UnverifiedLedgerStoreEnvelope {
            previous_hash: previous,
            entry_hash,
            external_id: (),
            payload,
        };
        let verified = verify_envelope(
            unverified,
            LedgerId(0),
            previous,
            &DefaultLedgerCanonicalEncoder,
            &Sha256V1Digest,
        )
        .expect("envelope should verify against its own digest");
        assert_eq!(verified.entry_hash(), entry_hash);
    }

    #[test]
    fn schema_v1_rejects_overflowing_string_length_loudly() {
        // We cannot construct a string of length > u32::MAX in a unit test, so exercise the
        // boundary helper directly. The contract is that `u32_be_len` returns a typed error
        // rather than silently saturating, which would let two distinct payloads collide on
        // canonical bytes once both exceed `u32::MAX`.
        let too_big = (u32::MAX as usize) + 1;
        let err = u32_be_len(too_big).expect_err("oversize length must produce a typed error");
        assert_eq!(
            err,
            LedgerCanonicalEncodeError::LengthOverflow { len: too_big }
        );
    }
}
