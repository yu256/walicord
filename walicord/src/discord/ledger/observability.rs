#[cfg(test)]
use parking_lot::Mutex;
use serenity::all::{ChannelId, GuildId, UserId};
use walicord_application::ledger::{
    LedgerId,
    observability::{LedgerObservability, LedgerObservabilityEvent},
};

/// Adapter-side observability variants that carry Discord-native identifiers. Kept
/// separate from `LedgerObservabilityEvent` so application code cannot accidentally
/// depend on serenity types through the trait surface.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiscordLedgerObservabilityEvent {
    UnauthorizedWriterDetected {
        ledger_id: Option<LedgerId>,
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
        guild_id: Option<GuildId>,
        channel_id: ChannelId,
        action: PermissionAction,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PermissionAction {
    ViewChannel,
    CreatePublicThread,
    AppendCanonicalMessage,
    SendMessageInChannel,
    AttachFiles,
    ManageThreads,
    UnarchiveThread,
    ReadMessageHistory,
}

pub trait DiscordLedgerObservability: LedgerObservability {
    fn emit_discord(&self, event: DiscordLedgerObservabilityEvent);
}

pub struct TracingLedgerObservability;

impl LedgerObservability for TracingLedgerObservability {
    fn emit(&self, event: LedgerObservabilityEvent) {
        match event {
            LedgerObservabilityEvent::GrowthWarning {
                ledger_id,
                entry_count,
            } => {
                tracing::warn!(
                    event = "ledger_thread_growth_warning",
                    ledger_id = %ledger_id,
                    canonical_entry_count = entry_count,
                    "canonical ledger thread exceeded growth warning threshold"
                );
            }
            LedgerObservabilityEvent::LoadTimeoutWarning {
                ledger_id,
                elapsed,
                route_label,
                fetched_entry_count,
            } => {
                tracing::warn!(
                    event = "ledger_load_slow_warning",
                    ledger_id = %ledger_id,
                    route = route_label,
                    elapsed_secs = elapsed.as_secs(),
                    fetched_entry_count,
                    "canonical ledger load exceeded warning threshold"
                );
            }
            LedgerObservabilityEvent::LoadTimeout {
                ledger_id,
                elapsed,
                route_label,
                fetched_entry_count,
            } => {
                tracing::error!(
                    event = "ledger_load_timeout",
                    ledger_id = %ledger_id,
                    route = route_label,
                    elapsed_secs = elapsed.as_secs(),
                    fetched_entry_count,
                    "canonical ledger load timed out"
                );
            }
            event @ (LedgerObservabilityEvent::IntegrityDrift { .. }
            | LedgerObservabilityEvent::UnknownLedgerFormat { .. }
            | LedgerObservabilityEvent::PersistentUncertainWrite { .. }) => {
                tracing::error!(?event, "ledger observability event");
            }
            event @ LedgerObservabilityEvent::OperatorHandoff { .. } => {
                tracing::warn!(?event, "ledger observability event");
            }
            LedgerObservabilityEvent::CanonicalAppendFailed {
                ledger_id,
                reason,
                retained_live_since,
            } => {
                tracing::error!(
                    event = "ledger_canonical_append_failed",
                    ledger_id = %ledger_id,
                    reason = %reason,
                    ?retained_live_since,
                    "canonical append failed; uncertain_write retain remains Live for lazy retry"
                );
            }
        }
    }
}

impl DiscordLedgerObservability for TracingLedgerObservability {
    fn emit_discord(&self, event: DiscordLedgerObservabilityEvent) {
        match &event {
            DiscordLedgerObservabilityEvent::UnauthorizedWriterDetected { .. }
            | DiscordLedgerObservabilityEvent::DuplicateThreadBlocked { .. }
            | DiscordLedgerObservabilityEvent::DamagedThreadBlocked { .. } => {
                tracing::error!(?event, "ledger observability event");
            }
            DiscordLedgerObservabilityEvent::PermissionFailure { .. } => {
                tracing::warn!(?event, "ledger observability event");
            }
        }
    }
}

/// Test-only sink that captures both trait surfaces into one ordered log. Holding a
/// single `Vec` (rather than two per-trait `Vec`s) preserves the relative ordering
/// between application-side and Discord-side emissions, which integration tests
/// covering the split rely on.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg(test)]
pub enum CapturedLedgerObservabilityEvent {
    Application(LedgerObservabilityEvent),
    Discord(DiscordLedgerObservabilityEvent),
}

#[cfg(test)]
pub struct CapturingLedgerObservability {
    events: Mutex<Vec<CapturedLedgerObservabilityEvent>>,
}

#[cfg(test)]
impl Default for CapturingLedgerObservability {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
impl CapturingLedgerObservability {
    pub fn new() -> Self {
        Self {
            events: Mutex::new(Vec::new()),
        }
    }

    pub fn snapshot(&self) -> Vec<CapturedLedgerObservabilityEvent> {
        self.events.lock().clone()
    }
}

#[cfg(test)]
impl LedgerObservability for CapturingLedgerObservability {
    fn emit(&self, event: LedgerObservabilityEvent) {
        self.events
            .lock()
            .push(CapturedLedgerObservabilityEvent::Application(event));
    }
}

#[cfg(test)]
impl DiscordLedgerObservability for CapturingLedgerObservability {
    fn emit_discord(&self, event: DiscordLedgerObservabilityEvent) {
        self.events
            .lock()
            .push(CapturedLedgerObservabilityEvent::Discord(event));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;
    use walicord_application::ledger::observability::LEDGER_GROWTH_WARNING_THRESHOLD;

    fn ledger() -> LedgerId {
        walicord_ledger::test_fixtures::ledger_id(77)
    }

    #[test]
    fn capturing_sink_records_application_and_discord_emissions_in_relative_order() {
        let sink = CapturingLedgerObservability::new();
        sink.emit(LedgerObservabilityEvent::GrowthWarning {
            ledger_id: ledger(),
            entry_count: LEDGER_GROWTH_WARNING_THRESHOLD,
        });
        sink.emit_discord(DiscordLedgerObservabilityEvent::PermissionFailure {
            ledger_id: Some(ledger()),
            guild_id: Some(GuildId::new(1)),
            channel_id: ChannelId::new(2),
            action: PermissionAction::AttachFiles,
        });
        sink.emit(LedgerObservabilityEvent::LoadTimeoutWarning {
            ledger_id: ledger(),
            elapsed: Duration::from_secs(20),
            route_label: "test",
            fetched_entry_count: 7,
        });

        assert_eq!(
            sink.snapshot(),
            vec![
                CapturedLedgerObservabilityEvent::Application(
                    LedgerObservabilityEvent::GrowthWarning {
                        ledger_id: ledger(),
                        entry_count: LEDGER_GROWTH_WARNING_THRESHOLD,
                    }
                ),
                CapturedLedgerObservabilityEvent::Discord(
                    DiscordLedgerObservabilityEvent::PermissionFailure {
                        ledger_id: Some(ledger()),
                        guild_id: Some(GuildId::new(1)),
                        channel_id: ChannelId::new(2),
                        action: PermissionAction::AttachFiles,
                    }
                ),
                CapturedLedgerObservabilityEvent::Application(
                    LedgerObservabilityEvent::LoadTimeoutWarning {
                        ledger_id: ledger(),
                        elapsed: Duration::from_secs(20),
                        route_label: "test",
                        fetched_entry_count: 7,
                    }
                ),
            ]
        );
    }

    #[test]
    fn growth_threshold_matches_required_criterion_value() {
        assert_eq!(LEDGER_GROWTH_WARNING_THRESHOLD, 4000);
    }
}
