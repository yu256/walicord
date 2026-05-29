use serenity::all::{ChannelId, GuildId, UserId};
use std::sync::Mutex;
use walicord_application::ledger::{
    LedgerId,
    observability::{CanonicalLoadRoute, LedgerObservability, LedgerObservabilityEvent},
};

/// Adapter-side observability variants that carry Discord-native identifiers. Kept
/// separate from `LedgerObservabilityEvent` so application code cannot accidentally
/// depend on serenity types through the trait surface.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiscordLedgerObservabilityEvent {
    ActiveActiveMisconfiguration {
        ledger_id: LedgerId,
        observed_writer: UserId,
        expected_writer: UserId,
    },
    DuplicateThreadBlocked {
        guild_id: GuildId,
        tracked_parent_channel_id: ChannelId,
        candidate_thread_ids: Vec<ChannelId>,
    },
    DamagedThreadBlocked {
        guild_id: GuildId,
        tracked_parent_channel_id: ChannelId,
        candidate_thread_id: ChannelId,
    },
    PermissionFailure {
        ledger_id: Option<LedgerId>,
        guild_id: GuildId,
        channel_id: ChannelId,
        action: PermissionAction,
    },
    RetryBudgetExhausted {
        ledger_id: Option<LedgerId>,
        route: CanonicalLoadRoute,
        attempts: u32,
    },
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

pub trait DiscordLedgerObservability: Send + Sync {
    fn emit_discord(&self, event: DiscordLedgerObservabilityEvent);
}

pub struct TracingLedgerObservability;

impl LedgerObservability for TracingLedgerObservability {
    fn emit(&self, event: LedgerObservabilityEvent) {
        match &event {
            LedgerObservabilityEvent::IntegrityDrift { .. }
            | LedgerObservabilityEvent::UnknownLedgerFormat { .. }
            | LedgerObservabilityEvent::PersistentUncertainWrite { .. }
            | LedgerObservabilityEvent::LoadTimeout { .. } => {
                tracing::error!(?event, "ledger observability event");
            }
            LedgerObservabilityEvent::LoadTimeoutWarning { .. }
            | LedgerObservabilityEvent::GrowthWarning { .. }
            | LedgerObservabilityEvent::OperatorHandoff { .. } => {
                tracing::warn!(?event, "ledger observability event");
            }
        }
    }
}

impl DiscordLedgerObservability for TracingLedgerObservability {
    fn emit_discord(&self, event: DiscordLedgerObservabilityEvent) {
        match &event {
            DiscordLedgerObservabilityEvent::ActiveActiveMisconfiguration { .. }
            | DiscordLedgerObservabilityEvent::DuplicateThreadBlocked { .. }
            | DiscordLedgerObservabilityEvent::DamagedThreadBlocked { .. } => {
                tracing::error!(?event, "ledger observability event");
            }
            DiscordLedgerObservabilityEvent::PermissionFailure { .. }
            | DiscordLedgerObservabilityEvent::RetryBudgetExhausted { .. } => {
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
    use std::time::Duration;
    use walicord_application::ledger::observability::LEDGER_GROWTH_WARNING_THRESHOLD;

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
