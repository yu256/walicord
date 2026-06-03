//! Application-layer ports + types for canonical ledger writes.
//!
//! Step 1 of the canonical-write use-case lift: defines the I/O boundary that
//! [`commit_authoritative_v1`] (Step 2) will sit behind. Adapter implementations
//! construct per-request port instances so the application use case never sees
//! `serenity::Context`, `ChannelId`, or any other transport-native handle.

use std::future::Future;

use crate::ledger::{UnverifiedLedgerStoreEnvelope, observability::AppendFailureReason};

/// Application-pure error returned by [`CanonicalThreadAppender::append_authoritative`].
///
/// Adapters classify their transport-native errors (e.g. the serenity-wrapping
/// `StoreWriteError`) into the closed [`AppendFailureReason`] taxonomy before
/// constructing this so the application layer never references `serenity` types.
/// The adapter MAY log the typed underlying error before returning; the
/// boundary surfaces only the taxonomy plus an optional erased source so error
/// chains stay observable in tests without piercing the layer.
#[derive(Debug, thiserror::Error)]
#[error("canonical append failed: {reason}")]
pub struct CanonicalAppendError {
    reason: AppendFailureReason,
    #[source]
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl CanonicalAppendError {
    pub fn new(reason: AppendFailureReason) -> Self {
        Self {
            reason,
            source: None,
        }
    }

    pub fn with_source(
        reason: AppendFailureReason,
        source: impl std::error::Error + Send + Sync + 'static,
    ) -> Self {
        Self {
            reason,
            source: Some(Box::new(source)),
        }
    }

    pub fn reason(&self) -> AppendFailureReason {
        self.reason
    }
}

/// Per-request port that posts the prepared canonical message to the bound
/// canonical thread. Adapters construct one instance per interaction so the
/// transport context (`serenity::Context`, target `ChannelId`, etc.) is bound
/// at the boundary and never leaks into application code.
///
/// Implementations MUST treat `body` as already validated against the canonical
/// message contract (recovery reference, truncation cues, Discord budget). The
/// application use case constructs the body via the presentation layer and
/// passes the validated string through; the port performs no re-validation.
pub trait CanonicalThreadAppender: Send + Sync {
    fn append_authoritative(
        &self,
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
        body: &str,
    ) -> impl Future<Output = Result<(), CanonicalAppendError>> + Send;
}

/// Per-request port that publishes a successful canonical write back to the
/// thread locator cache. Adapters bind the canonical thread identifier at
/// construction so the application use case calls a single zero-argument
/// method. Called only on the success path of a verified `append_authoritative`.
pub trait LocatorBindingPublisher: Send + Sync {
    fn publish_ready(&self);
}

/// Closed outcome of [`commit_authoritative_v1`] so callers branch on the two
/// terminal states without inspecting the underlying error. `Recorded` means
/// the canonical post landed and the retain has been cleared; `UncertainAppendFailed`
/// means the retain is still `Live` for criterion-217 / 279 lazy retry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommitOutcome {
    Recorded,
    UncertainAppendFailed,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_append_error_carries_reason() {
        let error = CanonicalAppendError::new(AppendFailureReason::Transport);
        assert_eq!(error.reason(), AppendFailureReason::Transport);
        assert!(std::error::Error::source(&error).is_none());
    }

    #[test]
    fn canonical_append_error_with_source_preserves_chain() {
        let source = std::io::Error::other("simulated transport failure");
        let error = CanonicalAppendError::with_source(AppendFailureReason::Transport, source);
        assert_eq!(error.reason(), AppendFailureReason::Transport);
        assert!(std::error::Error::source(&error).is_some());
    }

    #[test]
    fn canonical_append_error_display_includes_reason() {
        let error = CanonicalAppendError::new(AppendFailureReason::ReadBack);
        assert_eq!(error.to_string(), "canonical append failed: read_back");
    }
}
