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
    },
    LoadTimeout {
        ledger_id: LedgerId,
        elapsed: Duration,
    },
    OperatorHandoff {
        ledger_id: Option<LedgerId>,
        reason: OperatorHandoffReason,
    },
    UnknownLedgerFormat {
        ledger_id: LedgerId,
        failing_entry_id: Option<LedgerEntryId>,
    },
}

pub trait LedgerObservability: Send + Sync {
    fn emit(&self, event: LedgerObservabilityEvent);
}
