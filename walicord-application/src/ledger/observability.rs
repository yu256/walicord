use std::time::{Duration, SystemTime};

use walicord_ledger::{LedgerEntryId, LedgerId};

/// Growth-warning emission threshold required by criterion 251. Logging triggers at
/// >= 4000 entries; operations continue even past 5000.
pub const LEDGER_GROWTH_WARNING_THRESHOLD: u64 = 4_000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalLoadRoute {
    Read,
    Preview,
    Refresh,
    WritePrelude,
}

impl CanonicalLoadRoute {
    pub fn label(self) -> &'static str {
        match self {
            Self::Read => "read",
            Self::Preview => "preview",
            Self::Refresh => "refresh",
            Self::WritePrelude => "write_prelude",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegrityDriftKind {
    SelfLinkLateEdit,
    UnexpectedEdit,
    Deletion,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorHandoffReason {
    OlderThanLatestTwentyVoid,
    DamagedHistoryReplacement,
    DuplicateThreadResolution,
    BootstrapTimeout,
    PersistentUncertainWrite,
}

/// Closed taxonomy mirroring the adapter-side `StoreWriteError` variants so application
/// observability can surface canonical-append failure mode without depending on the
/// raw Discord error type. The adapter maps `StoreWriteError` into this on emit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppendFailureReason {
    Prepare,
    Permission,
    ArchivedOrLocked,
    Transport,
    ReadBack,
    WriteTimeout,
}

impl AppendFailureReason {
    pub fn label(self) -> &'static str {
        match self {
            Self::Prepare => "prepare",
            Self::Permission => "permission",
            Self::ArchivedOrLocked => "archived_or_locked",
            Self::Transport => "transport",
            Self::ReadBack => "read_back",
            Self::WriteTimeout => "write_timeout",
        }
    }
}

/// Observability signals emitted by application-layer ledger logic. Carries no
/// transport-native identifiers (Discord `UserId` / `ChannelId` / `GuildId`); those
/// live on the adapter-side `DiscordLedgerObservabilityEvent`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerObservabilityEvent {
    IntegrityDrift {
        ledger_id: LedgerId,
        entry_id: LedgerEntryId,
        kind: IntegrityDriftKind,
    },
    PersistentUncertainWrite {
        ledger_id: LedgerId,
        live_since: SystemTime,
        now: SystemTime,
    },
    GrowthWarning {
        ledger_id: LedgerId,
        entry_count: u64,
    },
    LoadTimeoutWarning {
        ledger_id: LedgerId,
        elapsed: Duration,
        route_label: &'static str,
        fetched_entry_count: usize,
    },
    LoadTimeout {
        ledger_id: LedgerId,
        elapsed: Duration,
        route_label: &'static str,
        fetched_entry_count: usize,
    },
    OperatorHandoff {
        ledger_id: Option<LedgerId>,
        reason: OperatorHandoffReason,
    },
    UnknownLedgerFormat {
        ledger_id: LedgerId,
        failing_entry_id: Option<LedgerEntryId>,
    },
    /// Canonical `append_authoritative` returned a transport / permission / read-back /
    /// timeout error so the retain stays `Live` for criterion-217 / 279 lazy retry.
    /// Adapter classifies the underlying `StoreWriteError` into the closed
    /// `AppendFailureReason` set so this layer never references serenity types.
    CanonicalAppendFailed {
        ledger_id: LedgerId,
        reason: AppendFailureReason,
        retained_live_since: SystemTime,
    },
}

pub trait LedgerObservability: Send + Sync {
    fn emit(&self, event: LedgerObservabilityEvent);
}
