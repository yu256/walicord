use crate::{
    LedgerEvent, LedgerStructureError,
    validation::{StructuralValidation, validate_structure},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LedgerEntryId(pub u64);

/// Identifier of the ledger this entry belongs to. Two chains with different `LedgerId`
/// values are isolated tamper-evidence chains: the application binds the id into the
/// canonical hash input via `HashedLedgerPayload`, so an envelope copied from another
/// ledger fails verification even if its internal `previous_hash`/`entry_hash`
/// relationships are individually consistent. For the planned Discord deployment this is
/// expected to be the channel snowflake of the dedicated ledger channel; the type is
/// intentionally agnostic to that choice so non-Discord stores (in-memory tests,
/// migration tooling, alternate transports) can use any stable identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LedgerId(pub u64);

#[derive(Debug, Clone, PartialEq)]
pub struct LedgerRecord {
    pub id: LedgerEntryId,
    pub event: LedgerEvent,
}

impl LedgerRecord {
    pub fn new(id: LedgerEntryId, event: LedgerEvent) -> Self {
        Self { id, event }
    }
}

/// Append-ordered ledger log with structural invariants validated; projection still checks balances.
#[derive(Debug, Clone, PartialEq)]
pub struct AppendOrderedLedgerRecords {
    entries: Vec<LedgerRecord>,
    pub(crate) structural_validation: StructuralValidation,
}

impl AppendOrderedLedgerRecords {
    pub fn new(entries: Vec<LedgerRecord>) -> Result<Self, LedgerStructureError> {
        let structural_validation = validate_structure(&entries)?;

        Ok(Self {
            entries,
            structural_validation,
        })
    }

    pub fn as_slice(&self) -> &[LedgerRecord] {
        &self.entries
    }

    pub fn iter(&self) -> impl Iterator<Item = &LedgerRecord> {
        self.entries.iter()
    }
}
