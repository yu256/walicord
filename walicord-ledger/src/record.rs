use crate::{
    LedgerEvent, LedgerStructureError,
    validation::{StructuralValidation, validate_structure},
};
use std::{
    num::NonZeroU64,
    sync::atomic::{AtomicU64, Ordering},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LedgerEntryId(pub u64);

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

pub struct LedgerIdIssuer {
    salt: u64,
    counter: AtomicU64,
}

impl LedgerIdIssuer {
    pub fn from_entropy(entropy: NonZeroU64) -> Self {
        Self {
            salt: entropy.get(),
            counter: AtomicU64::new(1),
        }
    }

    pub fn issue(&self) -> LedgerId {
        loop {
            let counter = self.counter.fetch_add(1, Ordering::Relaxed);
            if let Some(value) = NonZeroU64::new(mix(self.salt, counter)) {
                return LedgerId(value);
            }
        }
    }
}

fn mix(salt: u64, counter: u64) -> u64 {
    salt.wrapping_add(counter).wrapping_mul(0x9E3779B97F4A7C15)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ledger_id_issuer_returns_distinct_ids_across_calls() {
        let issuer = LedgerIdIssuer::from_entropy(NonZeroU64::new(1).expect("non-zero"));

        assert_ne!(issuer.issue(), issuer.issue());
    }

    #[test]
    fn ledger_id_issuer_separates_distinct_entropy_sources() {
        let first = LedgerIdIssuer::from_entropy(NonZeroU64::new(1).expect("non-zero"));
        let second = LedgerIdIssuer::from_entropy(NonZeroU64::new(2).expect("non-zero"));

        assert_ne!(first.issue(), second.issue());
    }

    #[test]
    fn ledger_id_issuer_skips_zero_candidate() {
        let issuer = LedgerIdIssuer {
            salt: 1,
            counter: AtomicU64::new(u64::MAX),
        };

        assert_eq!(
            issuer.issue(),
            LedgerId(NonZeroU64::new(0x9E3779B97F4A7C15).expect("non-zero"))
        );
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
