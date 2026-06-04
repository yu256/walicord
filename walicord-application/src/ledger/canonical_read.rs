//! Application-layer port + types for canonical ledger reads.
//!
//! Defines the I/O boundary that read-side use cases (currently
//! [`crate::ledger::write_coordinator::resolve_uncertain_write_v1`]) sit behind.
//! Adapter implementations construct per-request port instances so the
//! application use case never sees `serenity::Context`, `ChannelId`, or any
//! other transport-native handle.

use std::future::Future;

use crate::ledger::{EntryHash, LedgerEntryId, write_coordinator::CanonicalMessageProbe};

/// Verified-load summary the application use cases need on the write path:
/// the canonical head hash to seal the next envelope against, and the next
/// available [`LedgerEntryId`]. Adapters compute this once from a full
/// `VerifiedLedgerThreadLoad` so the application boundary never sees the
/// transport-bound `VerifiedLedgerStoreEnvelope<ExternalId>` collection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LedgerHead {
    /// `None` for an empty / fresh chain — callers fall back to the per-ledger
    /// genesis hash.
    pub head_hash: Option<EntryHash>,
    pub next_entry_id: LedgerEntryId,
}

/// Application-pure error returned by [`CanonicalThreadReader`] methods.
///
/// Read failures share a single envelope at the application boundary: the
/// orchestration that resolves uncertain writes only branches on success vs
/// failure, never on the kind of read failure. Adapters MAY log the typed
/// underlying error before constructing this so the typed source stays
/// observable in production logs.
#[derive(Debug, thiserror::Error)]
#[error("canonical read failed")]
pub struct CanonicalReadError {
    #[source]
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl CanonicalReadError {
    pub fn new() -> Self {
        Self { source: None }
    }

    pub fn with_source(source: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self {
            source: Some(Box::new(source)),
        }
    }
}

impl Default for CanonicalReadError {
    fn default() -> Self {
        Self::new()
    }
}

/// Per-request port that performs the canonical-thread reads the resolve /
/// retry orchestration needs. Adapters construct one instance per interaction
/// so the transport context (`serenity::Context`, target `ChannelId`, ledger
/// id) is bound at the boundary and never leaks into application code.
pub trait CanonicalThreadReader: Send + Sync {
    /// Verified [`LedgerHead`] summary (head hash + next entry id). Adapters
    /// derive this from a full `VerifiedLedgerThreadLoad` so the application
    /// boundary never sees the transport-bound envelope collection.
    fn load_ledger_head(
        &self,
    ) -> impl Future<Output = Result<LedgerHead, CanonicalReadError>> + Send;

    /// Recent-window scan used by lazy retry — bounded by the criterion-176
    /// retain window. Probes are returned in canonical order.
    fn scan_recent(
        &self,
    ) -> impl Future<Output = Result<Vec<CanonicalMessageProbe>, CanonicalReadError>> + Send;

    /// Full-history scan used when the retain interval has expired and the
    /// recent window cannot prove either presence or absence.
    fn scan_all(
        &self,
    ) -> impl Future<Output = Result<Vec<CanonicalMessageProbe>, CanonicalReadError>> + Send;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_read_error_carries_optional_source() {
        let error =
            CanonicalReadError::with_source(std::io::Error::other("simulated read failure"));
        assert!(std::error::Error::source(&error).is_some());
    }

    #[test]
    fn canonical_read_error_default_has_no_source() {
        let error = CanonicalReadError::default();
        assert!(std::error::Error::source(&error).is_none());
    }
}
