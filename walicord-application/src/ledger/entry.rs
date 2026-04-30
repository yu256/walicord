use walicord_domain::model::{MemberId, Weight};
use walicord_ledger::{
    BalanceAdjusted, EntryVoided, ExpenseRecorded, LedgerEntryId, LedgerEvent, LedgerHistorySealed,
    LedgerRecord, LedgerStructureError, NormalizedSettlementPlanRecorded,
};

/// Source kind currently supported by `LedgerSourceCanonical`. The enum is intentionally
/// narrow: today's application only commits canonicalized legacy-DSL text. Adding another
/// kind (slash command, migration descriptor, modal submission, ...) should be a
/// deliberate schema/API design step rather than an ad-hoc extra string convention.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LedgerSourceCanonicalKind {
    LegacyDsl,
}

/// Canonical representation of the input that produced a ledger entry. This is
/// application-level audit metadata, not event payload, so it lives on
/// `LedgerEntryMetadata` rather than inside `ExpenseRecorded`, keeping the ledger event
/// payload free of input-channel concerns.
///
/// The current model intentionally supports **one** source kind only:
/// [`LedgerSourceCanonicalKind::LegacyDsl`]. That keeps the hashed metadata honest about
/// what the application can canonicalize today instead of pretending that arbitrary future
/// adapters already share one stable string format. The canonical text (not a hash) is
/// stored so callers can also render it directly. The hash chain still covers it because
/// `LedgerEntryMetadata` is part of the canonical hash input via `HashedLedgerPayload::entry`.
/// Free-form display prose belongs in adapter/presentation envelopes, not here — only
/// stable, reproducible canonical strings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LedgerSourceCanonical {
    kind: LedgerSourceCanonicalKind,
    canonical: String,
}

impl LedgerSourceCanonical {
    /// Canonicalized text from the current supported input surface: the legacy DSL /
    /// message-parser path. Additional source kinds should be added intentionally, not by
    /// overloading this constructor with new conventions.
    pub fn legacy_dsl(canonical: impl Into<String>) -> Result<Self, LedgerSourceCanonicalError> {
        let canonical = canonical.into().trim().to_owned();
        if canonical.is_empty() {
            return Err(LedgerSourceCanonicalError::Empty);
        }
        Ok(Self {
            kind: LedgerSourceCanonicalKind::LegacyDsl,
            canonical,
        })
    }

    pub fn kind(&self) -> LedgerSourceCanonicalKind {
        self.kind
    }

    pub fn canonical_text(&self) -> &str {
        &self.canonical
    }

    pub(crate) fn as_str(&self) -> &str {
        self.canonical_text()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerSourceCanonicalError {
    Empty,
}

/// `(member, weight)` pair captured at the time an expense was resolved into amounts. The
/// weight is the per-member weight that the planner used to split the expense — `1` for
/// members covered only by the default weight, an explicit override value otherwise. This
/// is the granular form that replaces the earlier free-form `AllocationSummary` string:
/// audit / display flows can render any UI label they want from it, and i18n / wording
/// changes don't invalidate the hash chain because canonical bytes derive from the
/// structured weights, not the surface text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemberWeight {
    pub member_id: MemberId,
    pub weight: Weight,
}

/// Typed audit snapshot of how the resolved member amounts in an `ExpenseRecorded` event
/// were derived. The event payload stores only post-resolution amounts so balance
/// calculation is independent of group/role re-resolution; this snapshot is the
/// *explanation* (kind + per-member weights) that audit/review flows present alongside
/// the calculated balances.
///
/// **Hash semantics — read carefully:** `AllocationSnapshot` is hash-protected metadata.
/// It lives on `LedgerEntryMetadata`, which is part of the canonical hash input via
/// `HashedLedgerPayload::entry`, so changing the snapshot after the fact invalidates the
/// chain. The structured shape (an enum + member weights) is exactly what gets canonical-
/// encoded in schema v1 — UI prose / i18n / display labels are derived from this snapshot
/// in the presentation layer and are *not* hashed, so wording can evolve freely without
/// touching ledger identity.
///
/// Replaces the earlier free-form `AllocationSummary(String)`: the typed form removes the
/// long-term operational risk of binding hash identity to free text that may need i18n,
/// re-wording, or admin re-display.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AllocationSnapshot {
    /// Expense was split evenly among all `owed_by` members.
    Even,
    /// Expense was split using explicit per-member weights. `resolved_weights` is the
    /// canonical (sorted-by-`MemberId`) list of `(member, weight)` pairs the planner used
    /// at split time. `Default::default` (= `Weight(0)`) is forbidden in this list — only
    /// members that participated with a non-zero weight should appear.
    Weighted { resolved_weights: Vec<MemberWeight> },
    /// Allocation strategy is structurally unknown — reserved for migration from
    /// pre-snapshot data sources where the original `Even` / `Weighted` cannot be
    /// reconstructed. Distinct from `None` on the option so the absence of a structured
    /// allocation is *explicit*: a chain entry recorded with `LegacyUnknown` is a record
    /// of "we did not capture this when the entry was first written," which is auditable
    /// information rather than silent missing data. Newly authored expenses must set
    /// `Even` or `Weighted`; the validator rejects an unset (`None`) allocation snapshot
    /// on an `ExpenseRecorded` entry, so absence is never propagated through the chain.
    LegacyUnknown,
}

impl AllocationSnapshot {
    /// Constructs a [`AllocationSnapshot::Weighted`] from a flat collection of
    /// `(member, weight)` pairs. Sorts by `MemberId` and rejects empty / zero-only inputs
    /// up-front so canonical bytes never depend on iteration order or inadvertent zeros.
    pub fn weighted(
        weights: impl IntoIterator<Item = MemberWeight>,
    ) -> Result<Self, AllocationSnapshotError> {
        let mut resolved_weights: Vec<MemberWeight> = weights.into_iter().collect();
        if resolved_weights.is_empty() {
            return Err(AllocationSnapshotError::EmptyWeights);
        }
        if resolved_weights.iter().any(|mw| mw.weight == Weight::ZERO) {
            return Err(AllocationSnapshotError::ZeroWeight);
        }
        resolved_weights.sort_by_key(|mw| mw.member_id);
        let mut seen = None;
        for mw in &resolved_weights {
            if Some(mw.member_id) == seen {
                return Err(AllocationSnapshotError::DuplicateMember {
                    member_id: mw.member_id,
                });
            }
            seen = Some(mw.member_id);
        }
        Ok(Self::Weighted { resolved_weights })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AllocationSnapshotError {
    EmptyWeights,
    ZeroWeight,
    DuplicateMember { member_id: MemberId },
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct LedgerEntryMetadata {
    pub recorded_by: Option<MemberId>,
    pub source: Option<LedgerSourceCanonical>,
    /// Typed audit snapshot of an expense's allocation strategy and resolved per-member
    /// weights. The projection never reads this field; it is hash-protected (because it
    /// is part of `LedgerEntryMetadata`) so audit data stays stable, but presentation
    /// flows can derive any display label from it without touching ledger identity.
    pub allocation_snapshot: Option<AllocationSnapshot>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LedgerEntry {
    pub id: LedgerEntryId,
    pub metadata: LedgerEntryMetadata,
    pub event: LedgerEvent,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NonExpenseLedgerEvent {
    NormalizedSettlementPlanRecorded(NormalizedSettlementPlanRecorded),
    LedgerHistorySealed(LedgerHistorySealed),
    EntryVoided(EntryVoided),
    BalanceAdjusted(BalanceAdjusted),
}

impl From<NormalizedSettlementPlanRecorded> for NonExpenseLedgerEvent {
    fn from(event: NormalizedSettlementPlanRecorded) -> Self {
        Self::NormalizedSettlementPlanRecorded(event)
    }
}

impl From<LedgerHistorySealed> for NonExpenseLedgerEvent {
    fn from(event: LedgerHistorySealed) -> Self {
        Self::LedgerHistorySealed(event)
    }
}

impl From<EntryVoided> for NonExpenseLedgerEvent {
    fn from(event: EntryVoided) -> Self {
        Self::EntryVoided(event)
    }
}

impl From<BalanceAdjusted> for NonExpenseLedgerEvent {
    fn from(event: BalanceAdjusted) -> Self {
        Self::BalanceAdjusted(event)
    }
}

impl From<NonExpenseLedgerEvent> for LedgerEvent {
    fn from(event: NonExpenseLedgerEvent) -> Self {
        match event {
            NonExpenseLedgerEvent::NormalizedSettlementPlanRecorded(event) => {
                Self::NormalizedSettlementPlanRecorded(event)
            }
            NonExpenseLedgerEvent::LedgerHistorySealed(event) => Self::LedgerHistorySealed(event),
            NonExpenseLedgerEvent::EntryVoided(event) => Self::EntryVoided(event),
            NonExpenseLedgerEvent::BalanceAdjusted(event) => Self::BalanceAdjusted(event),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerEntryConstructionError {
    LegacyUnknownRequiresLegacyExpenseConstructor,
}

impl LedgerEntry {
    /// Constructs a newly authored expense entry. Callers must choose the resolved
    /// allocation snapshot explicitly, so forgetting to persist `Even` / `Weighted`
    /// cannot silently degrade into `LegacyUnknown`.
    pub fn expense(
        id: LedgerEntryId,
        event: ExpenseRecorded,
        allocation_snapshot: AllocationSnapshot,
    ) -> Result<Self, LedgerEntryConstructionError> {
        if matches!(allocation_snapshot, AllocationSnapshot::LegacyUnknown) {
            return Err(
                LedgerEntryConstructionError::LegacyUnknownRequiresLegacyExpenseConstructor,
            );
        }

        Ok(Self {
            id,
            metadata: LedgerEntryMetadata {
                allocation_snapshot: Some(allocation_snapshot),
                ..LedgerEntryMetadata::default()
            },
            event: LedgerEvent::ExpenseRecorded(event),
        })
    }

    /// Explicit migration-only constructor for expense entries whose original allocation
    /// strategy was never captured and cannot be reconstructed. Keeping this as a named
    /// constructor prevents `LegacyUnknown` from becoming the accidental default for new
    /// writes.
    pub fn legacy_expense_without_allocation_snapshot(
        id: LedgerEntryId,
        event: ExpenseRecorded,
    ) -> Self {
        Self {
            id,
            metadata: LedgerEntryMetadata {
                allocation_snapshot: Some(AllocationSnapshot::LegacyUnknown),
                ..LedgerEntryMetadata::default()
            },
            event: LedgerEvent::ExpenseRecorded(event),
        }
    }

    /// Constructs a `LedgerEntry` with default audit metadata only. For expense entries
    /// this intentionally leaves `allocation_snapshot` unset; use [`LedgerEntry::expense`]
    /// for newly authored expenses or
    /// [`LedgerEntry::legacy_expense_without_allocation_snapshot`] when migrating legacy
    /// data that lacks the original allocation strategy.
    pub(crate) fn new(id: LedgerEntryId, event: LedgerEvent) -> Self {
        Self {
            id,
            metadata: LedgerEntryMetadata::default(),
            event,
        }
    }

    /// Named constructor for non-expense entries. This is the public constructor ordinary
    /// callers should use when the entry is known not to be an expense. The type
    /// guarantees this at the constructor boundary.
    pub fn non_expense(id: LedgerEntryId, event: impl Into<NonExpenseLedgerEvent>) -> Self {
        Self::new(id, event.into().into())
    }

    fn to_record(&self) -> LedgerRecord {
        LedgerRecord::new(self.id, self.event.clone())
    }
}

/// Errors returned by [`AppendOrderedLedgerEntries::new`]. Wraps the ledger crate's
/// structural validation and adds application-level metadata-coherence checks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AppendOrderedLedgerEntriesError {
    Structure(LedgerStructureError),
    /// `LedgerEntryMetadata::allocation_snapshot` is set on an entry whose event is not
    /// `ExpenseRecorded`. Allocation snapshots only describe how an expense was split,
    /// so attaching one to a settlement / void / seal / adjustment entry would be
    /// incoherent metadata that the hash chain would otherwise faithfully record forever.
    AllocationSnapshotOnNonExpenseEntry {
        entry_id: LedgerEntryId,
    },
    /// `LedgerEntryMetadata::allocation_snapshot` is `None` on an `ExpenseRecorded`
    /// entry. Newly written expenses must record the structured allocation (`Even` /
    /// `Weighted`) via [`LedgerEntry::expense`]; legacy chains where the allocation cannot
    /// be reconstructed should use
    /// [`LedgerEntry::legacy_expense_without_allocation_snapshot`] so the absence is
    /// recorded explicitly rather than silently missing.
    MissingAllocationSnapshotOnExpense {
        entry_id: LedgerEntryId,
    },
}

impl From<LedgerStructureError> for AppendOrderedLedgerEntriesError {
    fn from(err: LedgerStructureError) -> Self {
        Self::Structure(err)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AppendOrderedLedgerEntries {
    entries: Vec<LedgerEntry>,
    pub(super) records: walicord_ledger::AppendOrderedLedgerRecords,
}

impl AppendOrderedLedgerEntries {
    pub fn new(entries: Vec<LedgerEntry>) -> Result<Self, AppendOrderedLedgerEntriesError> {
        for entry in &entries {
            validate_metadata_event_coherence(entry)?;
        }
        let records = walicord_ledger::AppendOrderedLedgerRecords::new(
            entries.iter().map(LedgerEntry::to_record).collect(),
        )?;

        Ok(Self { entries, records })
    }

    pub fn as_slice(&self) -> &[LedgerEntry] {
        &self.entries
    }

    pub fn iter(&self) -> impl Iterator<Item = &LedgerEntry> {
        self.entries.iter()
    }
}

fn validate_metadata_event_coherence(
    entry: &LedgerEntry,
) -> Result<(), AppendOrderedLedgerEntriesError> {
    let is_expense = matches!(entry.event, LedgerEvent::ExpenseRecorded(_));
    match (&entry.metadata.allocation_snapshot, is_expense) {
        (Some(_), false) => Err(
            AppendOrderedLedgerEntriesError::AllocationSnapshotOnNonExpenseEntry {
                entry_id: entry.id,
            },
        ),
        (None, true) => Err(
            AppendOrderedLedgerEntriesError::MissingAllocationSnapshotOnExpense {
                entry_id: entry.id,
            },
        ),
        _ => Ok(()),
    }
}
