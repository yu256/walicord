# Ledger Event Replay

This note captures the first migration step from script replay to ledger event
replay. It mixes two kinds of content. **Implemented in this step** describes
shape and invariants that exist in code today. **Planned event-store
boundary** describes where the next phase is heading; those parts are
explicitly forward-looking and the named symbols may not exist yet.

## Implemented in this step

`Script` remains the current compatibility model. The ledger business rules live
in the inner `walicord-ledger` crate: event payloads, replay, validation, and
projected state do not depend on Discord, parser adapters, or application use
cases. `walicord-application` keeps metadata-bearing `LedgerEntry` values, the
append-order wrapper that an event store or migration adapter would return, and
the store envelope that wraps a ledger entry with hash-chain and store-specific
transport fields. Discord and infrastructure adapters should import ledger event
types through `walicord-application` unless they are pure core tests or
explicitly documented low-level adapters. The ledger crate stays directly usable
for pure domain-rule tests and future non-Discord adapters.

The first replay surface is intentionally small:

- `ExpenseRecorded` stores resolved member amounts together with the optional
  `note` field. Role, group, and expression inputs must be resolved before this
  event is created. The event constructor canonicalizes each side by member id,
  merges same-side duplicates, rejects non-positive amounts, and requires
  paid/owed totals to match. A member may still appear on both `paid_by` and
  `owed_by`: for example, the payer can also owe their own share. `ExpenseNote`
  carries the human-readable memo a user attaches to an expense — it is part of
  the business event because two entries that differ only in their note are
  meaningfully different and must hash differently. Provenance information
  (which input channel produced the entry, which command, etc.) is *not* event
  payload; it lives on `LedgerEntryMetadata::source` as `LedgerSourceCanonical` so
  the core ledger event payload stays free of input-channel concerns. Today that
  source type intentionally models one source kind only (`LegacyDsl`) instead of
  pretending that future slash/modal/migration inputs already share one stable
  canonical-string contract; broadening it should be a deliberate schema/API
  change. Both `ExpenseNote` and `LedgerSourceCanonical` are stored trimmed and
  reject blank input. The hash chain still covers `LedgerSourceCanonical` because
  `LedgerEntryMetadata` is part of the canonical hash input via
  `HashedLedgerPayload::entry`. Store-specific transport details (Discord
  message id, raw embed, etc.) stay outside the hashed payload, on the store
  envelope. For Discord specifically, verified payload fields remain the audit
  source of truth; persisted embed/display text is transport cache, not
  authoritative business state.
- `NormalizedSettlementPlanRecorded` stores fixed settlement transfers and applies
  them directly; replay never asks a settlement planner to recompute them. The
  constructor rejects empty transfers, structurally invalid transfers, and
  opposing transfer pairs. It also requires sender and receiver member sets to
  be disjoint within the same settlement batch. It canonicalizes transfers by
  `(from, to)`, merges duplicate pairs, and sorts by member id. Projection
  additionally requires each transfer to reduce existing debt and credit without
  flipping either side's balance. This event records transfer effects only;
  history sealing is always represented by a separate `LedgerHistorySealed`
  event.

  **Scope:** `NormalizedSettlementPlanRecorded` is *planner output*, not a
  general payment receipt. Its constraints (no opposing pair, sender/receiver
  disjoint in a batch, no chained transfers) reflect the canonical-plan
  semantics produced by `SettlementPlanner`. If Walicord later needs to record
  real-world payments that do not fit this shape — for example chained
  payments (A→B→C) recorded directly by users, or payments asserted
  independently of any plan — that is a different domain concept and should be a
  separate event such as `ActualSettlementPaymentRecorded`. Keep this event
  tight so the audit semantics of "this is what the planner said to do" remain
  unambiguous.

  Cross-request settle-up confirmation should persist
  `walicord_application::PreviewConfirmationBinding`, not just a bare preview
  digest. The digest binds the previewed settlement value itself; the binding's
  surrounding fields (`ledger_id`, `ledger_head_hash`, `actor_id`, expiry) are
  what let a confirm flow reject stale or replayed confirmations after ledger
  state or authority context has changed.
- `LedgerHistorySealed { through }` is the explicit history barrier. It records
  that prior ledger history is sealed through a specific prior entry without
  applying transfers. The marker entry is not itself the seal target; repeated
  seal markers may seal through earlier seal markers because they are ordinary
  ledger history entries. A seal target may be any valid prior ledger entry,
  including void and adjustment markers. Seal targets are ledger-position
  markers, not effective-state entries, so a voided entry may still be used as a
  seal target. The event was named `Sealed` rather than `Closed` to avoid the
  end-user reading of `closed = settlement complete`; application command
  vocabulary should follow the same rule and prefer `seal`, `freeze`, or
  `confirm reviewed range` over `close`.
- `EntryVoided` targets a valid prior expense or settlement transfer entry.
  Projection handles it in append order as a compensating event: the target is
  applied when it appears, then reversed when the void entry appears. The
  initial model does not allow voiding void entries, sealed entries, balance
  adjustments, history seal markers, or entries that have already been sealed.
- `BalanceAdjusted` applies a zero-sum manual balance adjustment after history
  has been sealed. This compensates for mistakes found in immutable sealed
  history; unsealed mistakes should use `EntryVoided` plus a new
  `ExpenseRecorded`. The constructor canonicalizes by member id, rejects
  zero-valued input and zero-valued aggregated adjustments, and requires the
  total adjustment to remain zero. A human-readable `AdjustmentReason` is
  required and stored trimmed so manual corrections remain auditable.
  `BalanceAdjustmentSource::SealedEntryCorrection` names a known sealed entry;
  structural validation requires that entry to be valid and prior, and projection
  requires it to be inside sealed history, not voided, to be an expense or
  settlement transfer entry, and to share at least one member with the adjustment.
  `ExternalCorrection` is reserved for current corrections that are not
  attributable to a specific ledger entry, but it is still allowed only after
  history has been sealed. The core `AdminCorrectionAuthority` token is an
  explicit intent marker, not a hard authority boundary; normal application
  flows should reach this variant through the audited admin helper in
  `walicord-application`.

The core's "at least one shared member" rule is intentionally permissive so
that a sealed entry recorded with a missing participant can still be corrected
by an adjustment that introduces that participant. Standard correction flows
should narrow this further by restricting candidates to the related entry's
participants. `walicord-application::standard_sealed_entry_correction`
implements this stricter check and is the helper that ordinary use cases should
use; administrative flows that legitimately need to touch non-participants
should bypass it and construct `BalanceAdjusted` directly with prominent
auditing.

Replay separates structural validation from stateful projection. Structural
validation returns `LedgerStructureError` for entry id uniqueness, prior seal
targets, and prior non-self void targets. Projection returns
`LedgerProjectionError` for invariants that need current balances or event
effects: transfer direction, overpayment checks, void target kind, duplicate
voids, sealed-history conflicts, adjustment placement, and final zero-sum
state. `NormalizedSettlementPlanRecorded` does not advance sealed history; only
`LedgerHistorySealed` creates a history barrier.
`AppendOrderedLedgerEntries` is the application boundary for event stream order:
its constructor validates structural invariants once and preserves the input
order exactly. Event stores and migration adapters must build this type from
ledger append order. Entry ids are stable keys, not the source of replay
ordering. `LedgerEntryId` remains a compact `u64` newtype for efficient replay
lookups; storage adapters must explicitly map external ids such as Discord
message ids into it rather than treating the external id as a replay order.
`LedgerProjector::replay` accepts `AppendOrderedLedgerEntries` rather than a raw
slice so projection cannot run on an ambiguous or unvalidated entry sequence.
`AppendOrderedLedgerEntries` validates only order-dependent structural
invariants; stateful balance invariants are still checked during projection, so
loading this type is not the same as proving the ledger can replay successfully.
The application-level `replay_entries` helper wraps both phases in
`LedgerReplayError` for callers that want one load-and-project operation.

Sealed history tracks both the marker entry that created the barrier and the
target entry sealed by that marker. `LedgerState::sealed_history` exposes both
ids; `LedgerState::sealed_through` is the target-only convenience API.
`LedgerState::voided_entry_ids` exposes all voided target entries, while
`voided_expense_entry_ids` remains available for callers that specifically need
expense targets.

`LedgerState::balances` is a pure balance map: zero-balance entries are pruned
after replay so the map only contains members with current debt or credit.
`LedgerState::participants` is the canonical participant set, holding every
member that ever appeared in an applied event payload. Voiding does not remove
participants because the void is a compensating event, not an erasure of
history. Review and presentation flows should consult `participants` for "who
took part" and `balances` for "who currently owes or is owed".

The initial correction story is deliberately narrow: unsealed expenses can be
replaced with `EntryVoided` plus a new `ExpenseRecorded`, and unsealed
settlement transfers can be replaced with `EntryVoided` plus a new
`NormalizedSettlementPlanRecorded`. Mistakes found after history is sealed are
represented by a current `BalanceAdjusted` event. `BalanceAdjusted` is rejected
before any history has been sealed. There is no separate `EntryCorrected` event
yet.

`ExpenseNote` is part of the canonical event payload and is therefore part of
the hash chain: two entries that differ only in their note are different events.
The implication for editing flows is explicit. For an unsealed entry, a note
edit is modeled as `EntryVoided` plus a new `ExpenseRecorded` carrying the
revised note — never as in-place mutation of the existing entry, since that
would invalidate the hash. For a sealed entry, the initial model treats the
note as immutable: there is no note-only edit event, and `BalanceAdjusted`
cannot rewrite payload fields. Discord ledger UI work must therefore decide,
before exposing note editing for sealed entries, whether to add a dedicated
note-correction event or to leave such notes immutable; the current code path
deliberately keeps that decision deferred so the hash semantics stay stable.

Application use cases should keep the core model's broader primitives narrow in
normal user flows. `ExternalCorrection` should be restricted to explicit
administrative workflows with a meaningful reason and prominent presentation;
the token used to construct it is intentionally review-visible, but it is not a
sealed permission object. Similarly, `LedgerHistorySealed` may target any valid
prior ledger position in the core model, but ordinary seal/confirm workflows
should seal through the current last append entry or the last entry included in
the reviewed ledger range.

`walicord-ledger::LedgerRecord` is the metadata-free replay record used by the
core model. `walicord-application::LedgerEntry` wraps the same event with
application-level metadata. Application consumers should use `LedgerEntry` and
`AppendOrderedLedgerEntries`; `LedgerRecord` remains a core crate detail for
metadata-free replay tests and conversion. Application metadata may grow to
include fields such as `recorded_at`, `source_kind`, and `command_id`;
store-specific fields such as Discord message ids and raw embed data live on
the store envelope rather than `LedgerEntryMetadata`.

The hash-protected payload and the store envelope are split into separate
types so the boundary is explicit in code rather than convention.
`HashedLedgerPayload { schema_version, hash_suite, entry }` is the canonical
hash input: `schema_version` defines how `entry` is serialized for hashing
and `hash_suite` (e.g. `Sha256V1`) pins the digest function the chain was
built with — both live inside the hashed payload because keeping them
outside would let the same byte sequence be reinterpreted by a future schema
or silently downgraded to a weaker digest. The hash itself covers the
canonical ledger event plus application-level audit metadata such as
`recorded_by`, `source`, and the typed `allocation_snapshot`.
`UnverifiedLedgerStoreEnvelope<ExternalId>` wraps that payload with the chain
itself (`previous_hash`, `entry_hash`) and store-specific transport
(`external_id`), as it sits on the wire or in storage.
`VerifiedLedgerStoreEnvelope<ExternalId>` is produced only by
`verify_envelope`, which checks that the digest's declared `LedgerHashSuite`
matches the payload's declared suite, recomputes `entry_hash` via the
selected `LedgerCanonicalEncoder` + `LedgerDigest`, and compares
`previous_hash` against the expected chain head; holding the verified type is
structural proof of chain integrity, so any code path that depends on
tamper-evidence should accept the verified type rather than the unverified
one.

Canonicalization is **application-owned**: `LedgerCanonicalEncoder` (and the
default `DefaultLedgerCanonicalEncoder` selected by `schema_version`) lives
in `walicord-application`, so adapters cannot drift on canonical bytes. The
digest is also application-owned: `Sha256V1Digest` is the canonical
realization of the `Sha256V1` suite, and production code uses
`load_and_replay_verified_sha256_v1` (which bakes in
`DefaultLedgerCanonicalEncoder` + `Sha256V1Digest`) rather than the generic
trait-parameterized helper. `LedgerCanonicalEncoder` and `LedgerDigest` are
crate-private: in-crate tests can still substitute deterministic digests, but
production adapters do not implement either trait and instead call the
suite-locked public API. External ids are deliberately outside the hashed
payload because the same ledger entry may be migrated between stores;
`schema_version` and `hash_suite` are inside because they are part of how the
entry is hashed. The suite-locked append-order/load APIs also derive the chain
genesis from `ledger_id` via `ledger_chain_genesis_sha256_v1`, so adapters do
not choose their own `[0; 32]`-style genesis sentinel. Transport-facing
display fields remain cache only: Discord adapters should re-render audited
views from the verified payload (or compare cached display text against a
verified re-render and surface drift as a warning) rather than trusting the
cached human-display text as part of the ledger truth.

`VerifiedLedgerStoreEnvelope::into_unverified` exists for migration and
re-storage flows that need to write the envelope into a different store, but
the returned value is no longer carrying chain-integrity evidence. Code paths
that depend on tamper-evidence must re-verify before use; convenience
re-conversion to verified-without-rechecking is intentionally not provided so
the verified type cannot decay into "verified-by-trust" in practice.

Single-envelope `verify_envelope` remains the in-crate primitive; ordinary
production callers use the suite-locked helpers. `verify_envelope_sha256_v1`
verifies the first envelope against the deterministic
`ledger_chain_genesis_sha256_v1`, while
`verify_envelopes_in_append_order_sha256_v1` threads each envelope's
recomputed `entry_hash` into the next envelope's expected `previous_hash` and
reports the append-order position of the first failure.
`load_and_replay_verified_sha256_v1` is the canonical event-store load pipeline
production code calls: it chain-verifies with `Sha256V1Digest`, extracts
entries, runs structural validation, then projects, and returns both the
verified envelopes (for
`external_id`/transport access) and the projected `ProjectedLedger` (state
plus per-entry index). The generic `load_and_replay_verified_with_custom_digest`
exists only for tests and migration tooling that need to substitute a stub
digest — production paths cannot reach for it accidentally because the suite
is locked in the function name. The split between `LedgerCanonicalEncoder`
(application-owned, schema-versioned) and `LedgerDigest` (application-owned
standard impl per suite, test-stubbable trait) makes adapter drift on
canonical bytes structurally impossible — adapters never implement either in
production.

The current event set is designed to preserve the zero-sum balance invariant by
construction, and projection still verifies the final total in release builds as
the last defense against future snapshot, import, or codec bugs.
`walicord-ledger` is split by concern (`event`, `projection`, `validation`,
`error`, and focused payload modules) so new event kinds can be added without
growing a single ledger file.

This keeps the initial model independent from Discord, the legacy parser, and
the HiGHS optimizer while making the future event-store boundary explicit.

## Planned event-store boundary

The pieces below are deliberately not implemented yet; they document where the
next phase will land so reviewers and adapter authors can see the intended
shape rather than discover it during implementation.

`LedgerEventStore` is the planned trait that adapters (Discord ledger codec,
migration tooling, in-memory test store) will implement. Its `load` method is
expected to delegate to `load_and_replay_verified_sha256_v1` rather than
reimplementing the verify → extract → structure → project pipeline, so the
verify-before-replay boundary cannot accidentally be skipped by store
authors and the digest cannot drift from the application-canonical
`Sha256V1Digest` implementation. The trait itself, including write paths and
snapshot semantics, is still TBD.

The expected `LedgerEventStore::load` flow:

```text
unverified envelopes in append order
    ↓
verify_envelopes_in_append_order
    ↓
extract LedgerEntry payloads
    ↓
AppendOrderedLedgerEntries::new (structural invariants)
    ↓
LedgerProjector::replay
    ↓
ProjectedLedger
```

Adapters do not provide canonical encoding or digest implementations:
canonicalization stays in `walicord-application` via `LedgerCanonicalEncoder`
(selected by `schema_version`), and the standard digest for each
`LedgerHashSuite` ships with the application crate (e.g. `Sha256V1Digest`).
Adapters select a suite by passing the corresponding suite-locked load API
(currently `load_and_replay_verified_sha256_v1`) and never implement
`LedgerDigest` themselves in production code. Note edits for sealed entries
are also deferred to this phase: the initial model treats the `ExpenseNote`
field as immutable once history is sealed, and any policy change (dedicated
note-correction event, admin override) is a planned-phase decision.
