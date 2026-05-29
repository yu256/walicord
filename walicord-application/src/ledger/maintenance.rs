use std::num::NonZeroU64;

use walicord_domain::model::MemberId;
use walicord_ledger::{LedgerEntryId, LedgerId};

use super::{
    projection::{VerifiedLedgerThreadLoad, project_recent_voidable_entries},
    void_flow::VOID_CANDIDATE_WINDOW,
};

/// Adapter-agnostic transport-channel identifier carried by maintenance commands. The
/// application crate does not depend on serenity, so adapter call sites convert their
/// native channel id (e.g. `serenity::all::ChannelId`) via [`TransportChannelId::new`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TransportChannelId(NonZeroU64);

impl TransportChannelId {
    pub fn new(id: u64) -> Result<Self, TransportChannelIdError> {
        NonZeroU64::new(id)
            .map(Self)
            .ok_or(TransportChannelIdError::Zero)
    }

    pub fn get(self) -> u64 {
        self.0.get()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum TransportChannelIdError {
    #[error("transport channel id must be non-zero")]
    Zero,
}

/// Closed set of operator-only maintenance commands required by criteria 235, 244,
/// 267-269, 274, 288-291. Each variant intentionally captures its full input so the
/// trusted-operator binary serializes a single request rather than carrying ambient
/// session state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MaintenanceCommand {
    /// Recover an unsealed expense or settlement that is older than the latest-20
    /// voidable window (criterion 106 / 289). The replacement entry is appended after
    /// the void via the same canonical-thread store boundary.
    OlderThanTwentyVoidRecovery(OlderThanTwentyVoidRecoveryRequest),
    /// Repair a tracked parent that locator reported `DuplicateBlocked` for; the
    /// operator confirms which candidate thread is the canonical one (criterion 140 /
    /// 244).
    DuplicateThreadResolution {
        ledger_id: LedgerId,
        authoritative_thread_keep: TransportChannelId,
        retired_thread_ids: Vec<TransportChannelId>,
    },
    /// Whole-thread replacement to a fresh `LedgerId` on a new thread (criterion 199 /
    /// 291) after irrecoverable damage; same-parent dual-thread is forbidden.
    DamagedThreadReplacement {
        retired_ledger_id: LedgerId,
        new_parent_channel_id: TransportChannelId,
    },
    /// Ordinary seal advancing to the current append tail (criterion 274).
    OrdinarySeal { ledger_id: LedgerId },
    /// Standard sealed-entry correction targeting an existing sealed, non-voided
    /// expense or settlement participant set (criterion 267).
    StandardSealedEntryCorrection {
        ledger_id: LedgerId,
        target_entry_id: LedgerEntryId,
    },
    /// Standard prior-adjustment correction (criterion 268). The target must be a
    /// prior `BalanceAdjusted` entry inside sealed history.
    StandardPriorAdjustmentCorrection {
        ledger_id: LedgerId,
        target_entry_id: LedgerEntryId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OlderThanTwentyReplacement {
    pub replacement_recorded_by: MemberId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OlderThanTwentyVoidRecoveryRequest {
    pub ledger_id: LedgerId,
    pub target_entry_id: LedgerEntryId,
    pub actor_id: MemberId,
    pub replacement: Box<OlderThanTwentyReplacement>,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum MaintenanceValidationError {
    #[error(
        "target entry {target_entry_id:?} is inside the latest {window} voidable window; use /void instead"
    )]
    TargetInsideWindow {
        target_entry_id: LedgerEntryId,
        window: usize,
    },
    #[error("target entry {target_entry_id:?} is not present in the canonical thread")]
    TargetNotInLedger { target_entry_id: LedgerEntryId },
    #[error("target entry {target_entry_id:?} was already voided")]
    TargetAlreadyVoided { target_entry_id: LedgerEntryId },
    #[error("target entry {target_entry_id:?} is inside sealed history")]
    TargetSealed { target_entry_id: LedgerEntryId },
    #[error("duplicate-resolution requires at least one retired thread")]
    DuplicateResolutionEmpty,
    #[error("authoritative thread is not among the observed duplicate candidates")]
    AuthoritativeNotAmongCandidates,
}

/// Validate that the older-than-window void recovery target is in the ledger,
/// unsealed, unvoided, and strictly older than the latest-20 voidable window
/// (criterion 289). The latest-20 path is owned by the end-user `/void` UI; operators
/// are not allowed to bypass it.
pub fn validate_older_than_twenty_void<ExternalId>(
    target_entry_id: LedgerEntryId,
    load: &VerifiedLedgerThreadLoad<ExternalId>,
) -> Result<(), MaintenanceValidationError> {
    let projected = load.snapshot().projected();
    let info = projected
        .entry(target_entry_id)
        .ok_or(MaintenanceValidationError::TargetNotInLedger { target_entry_id })?;
    if info.voided {
        return Err(MaintenanceValidationError::TargetAlreadyVoided { target_entry_id });
    }
    if info.sealed {
        return Err(MaintenanceValidationError::TargetSealed { target_entry_id });
    }

    let recent_voidable = project_recent_voidable_entries(load, VOID_CANDIDATE_WINDOW)
        .map_err(|_| MaintenanceValidationError::TargetNotInLedger { target_entry_id })?;
    let target_in_window = recent_voidable
        .iter()
        .any(|view| view.entry().id == target_entry_id);
    if target_in_window {
        return Err(MaintenanceValidationError::TargetInsideWindow {
            target_entry_id,
            window: VOID_CANDIDATE_WINDOW,
        });
    }

    Ok(())
}

/// Validate a `DuplicateThreadResolution` command: must list >= 1 retired thread, and
/// the chosen authoritative thread must be among the duplicates the locator reported.
pub fn validate_duplicate_resolution(
    authoritative_thread_keep: TransportChannelId,
    retired_thread_ids: &[TransportChannelId],
    observed_candidates: &[TransportChannelId],
) -> Result<(), MaintenanceValidationError> {
    if retired_thread_ids.is_empty() {
        return Err(MaintenanceValidationError::DuplicateResolutionEmpty);
    }
    if !observed_candidates.contains(&authoritative_thread_keep) {
        return Err(MaintenanceValidationError::AuthoritativeNotAmongCandidates);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn channel(id: u64) -> TransportChannelId {
        TransportChannelId::new(id).expect("test channel id is non-zero")
    }

    #[test]
    fn transport_channel_id_rejects_zero() {
        assert_eq!(
            TransportChannelId::new(0),
            Err(TransportChannelIdError::Zero)
        );
    }

    #[test]
    fn duplicate_resolution_rejects_empty_retired_list() {
        let actual = validate_duplicate_resolution(channel(1), &[], &[channel(1), channel(2)]);
        assert_eq!(
            actual,
            Err(MaintenanceValidationError::DuplicateResolutionEmpty)
        );
    }

    #[test]
    fn duplicate_resolution_rejects_authoritative_not_in_observed() {
        let actual =
            validate_duplicate_resolution(channel(999), &[channel(2)], &[channel(1), channel(2)]);
        assert_eq!(
            actual,
            Err(MaintenanceValidationError::AuthoritativeNotAmongCandidates)
        );
    }

    #[test]
    fn duplicate_resolution_accepts_authoritative_in_observed_with_at_least_one_retired() {
        let actual =
            validate_duplicate_resolution(channel(1), &[channel(2)], &[channel(1), channel(2)]);
        assert_eq!(actual, Ok(()));
    }
}
