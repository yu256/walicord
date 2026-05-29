use serenity::all::{ChannelId, GuildId, UserId};
use std::{
    sync::Mutex,
    time::{Duration, SystemTime},
};
use walicord_application::ledger::{LedgerEntryId, LedgerId};

use super::projection::CanonicalLoadRoute;

/// Growth-warning emission threshold required by criterion 251. Logging triggers at
/// >= 4000 entries; operations continue even past 5000.
pub const LEDGER_GROWTH_WARNING_THRESHOLD: u64 = 4000;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerObservabilityEvent {
    /// Two or more processes were observed holding what should be a single-writer
    /// position. Triggered by writer-lineage drift across cutover or active-active
    /// deployment mistakes.
    ActiveActiveMisconfiguration {
        ledger_id: LedgerId,
        observed_writer: UserId,
        expected_writer: UserId,
    },
    /// A canonical message previously bound to an entry has drifted from its
    /// authoritative payload (other than the one-shot self-link completion edit).
    IntegrityDrift {
        ledger_id: LedgerId,
        entry_id: LedgerEntryId,
        kind: IntegrityDriftKind,
    },
    /// Locator observed two or more candidate canonical threads under one tracked
    /// parent (criterion 140).
    DuplicateThreadBlocked {
        guild_id: GuildId,
        tracked_parent_channel_id: ChannelId,
        candidate_thread_ids: Vec<ChannelId>,
    },
    /// Locator observed exactly one candidate thread but its history is missing or
    /// corrupted; manual recovery required (criterion 244).
    DamagedThreadBlocked {
        guild_id: GuildId,
        tracked_parent_channel_id: ChannelId,
        candidate_thread_id: ChannelId,
    },
    /// An operation failed because Discord-native permissions were missing or the
    /// thread was archived/locked (criterion 237/210).
    PermissionFailure {
        ledger_id: Option<LedgerId>,
        guild_id: GuildId,
        channel_id: ChannelId,
        action: PermissionAction,
    },
    /// A canonical action could not be completed because Discord returned a transport
    /// or rate-limit error after the retry budget was exhausted.
    RetryBudgetExhausted {
        ledger_id: Option<LedgerId>,
        route: CanonicalLoadRoute,
        attempts: u32,
    },
    /// An `uncertain_write` flag has been live longer than the operator-handoff
    /// threshold without lazy-retry conclusively clearing it.
    PersistentUncertainWrite {
        ledger_id: LedgerId,
        live_since: SystemTime,
        now: SystemTime,
    },
    /// Verified replay observed `entry_count >= LEDGER_GROWTH_WARNING_THRESHOLD`
    /// (criterion 251). Operations continue; this is an observability-only signal.
    GrowthWarning {
        ledger_id: LedgerId,
        entry_count: u64,
    },
    /// Canonical load exceeded the 20-second warning budget but is still progressing
    /// inside the 30-second hard timeout (criterion 212).
    LoadTimeoutWarning {
        ledger_id: LedgerId,
        elapsed: Duration,
    },
    /// Canonical load exceeded the 30-second hard timeout (criterion 212).
    LoadTimeout {
        ledger_id: LedgerId,
        elapsed: Duration,
    },
    /// User-facing operator handoff was rendered (older-than-20 void or damaged-thread
    /// recovery, criteria 106 / 244 / 235).
    OperatorHandoff {
        ledger_id: Option<LedgerId>,
        reason: OperatorHandoffReason,
    },
    /// Fail-closed AC 29 outcome: a canonical attachment used an unknown
    /// `schema_version` or `LedgerEvent` variant (criterion 109 with AC 29 addendum).
    UnknownLedgerFormat {
        ledger_id: LedgerId,
        failing_entry_id: Option<LedgerEntryId>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegrityDriftKind {
    SelfLinkLateEdit,
    UnexpectedEdit,
    Deletion,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PermissionAction {
    CreatePublicThread,
    SendMessageInChannel,
    AttachFiles,
    ManageThreads,
    UnarchiveThread,
    ReadMessageHistory,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorHandoffReason {
    OlderThanLatestTwentyVoid,
    DamagedHistoryReplacement,
    DuplicateThreadResolution,
    BootstrapTimeout,
    PersistentUncertainWrite,
}

pub trait LedgerObservability: Send + Sync {
    fn emit(&self, event: LedgerObservabilityEvent);
}

/// Production sink: routes each event to a `tracing` macro at the right severity. The
/// concrete severity classification lives here so call sites can stay agnostic; tests
/// stay decoupled from the global tracing subscriber by using
/// [`CapturingLedgerObservability`].
pub struct TracingLedgerObservability;

impl LedgerObservability for TracingLedgerObservability {
    fn emit(&self, event: LedgerObservabilityEvent) {
        match &event {
            LedgerObservabilityEvent::ActiveActiveMisconfiguration { .. }
            | LedgerObservabilityEvent::IntegrityDrift { .. }
            | LedgerObservabilityEvent::DuplicateThreadBlocked { .. }
            | LedgerObservabilityEvent::DamagedThreadBlocked { .. }
            | LedgerObservabilityEvent::UnknownLedgerFormat { .. }
            | LedgerObservabilityEvent::PersistentUncertainWrite { .. }
            | LedgerObservabilityEvent::LoadTimeout { .. } => {
                tracing::error!(?event, "ledger observability event");
            }
            LedgerObservabilityEvent::PermissionFailure { .. }
            | LedgerObservabilityEvent::RetryBudgetExhausted { .. }
            | LedgerObservabilityEvent::LoadTimeoutWarning { .. }
            | LedgerObservabilityEvent::GrowthWarning { .. }
            | LedgerObservabilityEvent::OperatorHandoff { .. } => {
                tracing::warn!(?event, "ledger observability event");
            }
        }
    }
}

pub struct CapturingLedgerObservability {
    events: Mutex<Vec<LedgerObservabilityEvent>>,
}

impl Default for CapturingLedgerObservability {
    fn default() -> Self {
        Self::new()
    }
}

impl CapturingLedgerObservability {
    pub fn new() -> Self {
        Self {
            events: Mutex::new(Vec::new()),
        }
    }

    pub fn snapshot(&self) -> Vec<LedgerObservabilityEvent> {
        self.events
            .lock()
            .expect("CapturingLedgerObservability mutex poisoned")
            .clone()
    }
}

impl LedgerObservability for CapturingLedgerObservability {
    fn emit(&self, event: LedgerObservabilityEvent) {
        self.events
            .lock()
            .expect("CapturingLedgerObservability mutex poisoned")
            .push(event);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ledger() -> LedgerId {
        LedgerId(77)
    }

    #[test]
    fn capturing_sink_records_each_emitted_event_in_order() {
        let sink = CapturingLedgerObservability::new();
        sink.emit(LedgerObservabilityEvent::GrowthWarning {
            ledger_id: ledger(),
            entry_count: LEDGER_GROWTH_WARNING_THRESHOLD,
        });
        sink.emit(LedgerObservabilityEvent::LoadTimeoutWarning {
            ledger_id: ledger(),
            elapsed: Duration::from_secs(20),
        });

        let events = sink.snapshot();

        assert_eq!(
            events,
            vec![
                LedgerObservabilityEvent::GrowthWarning {
                    ledger_id: ledger(),
                    entry_count: LEDGER_GROWTH_WARNING_THRESHOLD,
                },
                LedgerObservabilityEvent::LoadTimeoutWarning {
                    ledger_id: ledger(),
                    elapsed: Duration::from_secs(20),
                },
            ]
        );
    }

    #[test]
    fn growth_threshold_matches_required_criterion_value() {
        assert_eq!(LEDGER_GROWTH_WARNING_THRESHOLD, 4000);
    }
}
