use crate::{
    LedgerEvent, LedgerStructureError,
    validation::{StructuralValidation, validate_structure},
};
use std::num::NonZeroU64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LedgerEntryId(pub u64);

impl std::fmt::Display for LedgerEntryId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl LedgerEntryId {
    pub fn next(self) -> Option<Self> {
        self.0.checked_add(1).map(Self)
    }

    /// Provide the schema-v1 canonical representation without making hash encoders
    /// depend on the tuple layout.
    pub fn with_canonical_bytes(self, consume: impl FnOnce(&[u8])) {
        consume(&self.0.to_be_bytes());
    }
}

/// Identifier of the ledger this entry belongs to. Two chains with different `LedgerId`
/// values are isolated tamper-evidence chains: the application binds the id into the
/// canonical hash input via `HashedLedgerPayload`, so an envelope copied from another
/// ledger fails verification even if its internal `previous_hash`/`entry_hash`
/// relationships are individually consistent. For the planned Discord deployment this is
/// intentionally agnostic to its transport so non-Discord stores (in-memory tests,
/// migration tooling, alternate transports) can use any stable identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LedgerId(NonZeroU64);

impl LedgerId {
    /// Provide the schema-v1 canonical representation without exposing the inner value.
    pub fn with_canonical_bytes(self, consume: impl FnOnce(&[u8])) {
        consume(&self.0.get().to_be_bytes());
    }
}

impl From<NonZeroU64> for LedgerId {
    fn from(value: NonZeroU64) -> Self {
        Self(value)
    }
}

#[cfg(feature = "canonical-attachment-codec")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct CanonicalLedgerId(NonZeroU64);

#[cfg(feature = "canonical-attachment-codec")]
impl From<LedgerId> for CanonicalLedgerId {
    fn from(ledger_id: LedgerId) -> Self {
        Self(ledger_id.0)
    }
}

#[cfg(feature = "canonical-attachment-codec")]
impl From<CanonicalLedgerId> for LedgerId {
    fn from(ledger_id: CanonicalLedgerId) -> Self {
        Self(ledger_id.0)
    }
}

#[cfg(feature = "test-fixtures")]
pub mod test_fixtures {
    use super::LedgerId;
    use std::num::NonZeroU64;

    pub fn ledger_id(value: u64) -> LedgerId {
        LedgerId(NonZeroU64::new(value).expect("fixture ledger id should be non-zero"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ledger_id_parses_untrusted_nonzero_decimal_input() {
        assert_eq!(
            "42".parse::<LedgerId>(),
            Ok(LedgerId(NonZeroU64::new(42).expect("non-zero")))
        );
    }

    #[test]
    fn ledger_id_rejects_zero_decimal_input() {
        assert_eq!("0".parse::<LedgerId>(), Err(LedgerIdParseError));
    }

    #[test]
    fn ledger_entry_id_canonical_bytes_are_big_endian() {
        let mut actual = Vec::new();

        LedgerEntryId(0x0102_0304_0506_0708)
            .with_canonical_bytes(|bytes| actual.extend_from_slice(bytes));

        assert_eq!(actual, vec![1, 2, 3, 4, 5, 6, 7, 8]);
    }
}

impl std::fmt::LowerHex for LedgerId {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::LowerHex::fmt(&self.0.get(), formatter)
    }
}

impl std::fmt::Display for LedgerId {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.get(), formatter)
    }
}

impl std::str::FromStr for LedgerId {
    type Err = LedgerIdParseError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        value
            .parse::<NonZeroU64>()
            .map(Self)
            .map_err(|_| LedgerIdParseError)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
#[error("ledger id must be a non-zero unsigned integer")]
pub struct LedgerIdParseError;

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
