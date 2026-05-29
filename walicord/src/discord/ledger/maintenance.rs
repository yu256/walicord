use walicord_application::ledger::{LedgerEntryId, LedgerId};
use walicord_domain::model::MemberId;

use super::{store::VerifiedLedgerThreadLoad, void_flow::VOID_CANDIDATE_WINDOW};

/// Closed set of operator-only maintenance commands required by criteria 235, 244,
/// 267-269, 274, 288-291. Each variant intentionally captures its full input so the
/// trusted-operator binary serializes a single request rather than carrying ambient
/// session state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MaintenanceCommand {
    /// Recover an unsealed expense or settlement that is older than the latest-20
    /// voidable window (criterion 106 / 289). The replacement entry is appended after
    /// the void via the same canonical-thread store boundary.
    OlderThanTwentyVoidRecovery {
        ledger_id: LedgerId,
        target_entry_id: LedgerEntryId,
        actor_id: MemberId,
        replacement: Box<OlderThanTwentyReplacement>,
    },
    /// Repair a tracked parent that locator reported `DuplicateBlocked` for; the
    /// operator confirms which candidate thread is the canonical one (criterion 140 /
    /// 244).
    DuplicateThreadResolution {
        ledger_id: LedgerId,
        authoritative_thread_keep: serenity::all::ChannelId,
        retired_thread_ids: Vec<serenity::all::ChannelId>,
    },
    /// Whole-thread replacement to a fresh `LedgerId` on a new thread (criterion 199 /
    /// 291) after irrecoverable damage; same-parent dual-thread is forbidden.
    DamagedThreadReplacement {
        retired_ledger_id: LedgerId,
        new_parent_channel_id: serenity::all::ChannelId,
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
pub enum MaintenanceValidationError {
    /// Target was within the latest-20 voidable window; operators must use the normal
    /// `/void` UI for those (criterion 289).
    TargetInsideWindow {
        target_entry_id: LedgerEntryId,
        window: usize,
    },
    /// Target entry is not present in the canonical thread.
    TargetNotInLedger { target_entry_id: LedgerEntryId },
    /// Target entry was already voided.
    TargetAlreadyVoided { target_entry_id: LedgerEntryId },
    /// Target entry is inside sealed history but this command requires unsealed.
    TargetSealed { target_entry_id: LedgerEntryId },
    /// Damaged-thread replacement cannot reuse the retired ledger's parent channel
    /// (criterion 199: same-parent dual thread forbidden).
    SameParentDualThread,
    /// Duplicate-resolution must list at least one retired thread.
    DuplicateResolutionEmpty,
    /// The authoritative thread the operator chose to keep is not among the observed
    /// duplicate candidates.
    AuthoritativeNotAmongCandidates,
}

/// Validate an `OlderThanTwentyVoidRecovery` command against a verified canonical
/// thread load: confirm the target is in the ledger, unsealed (or sealed depending on
/// criterion 289's exact wording), not voided, and OLDER than the latest-20 window
/// (criterion 289: end-user UI bypass is forbidden for older-than-window entries).
pub fn validate_older_than_twenty_void(
    target_entry_id: LedgerEntryId,
    load: &VerifiedLedgerThreadLoad,
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

    let recent_voidable =
        super::projection::project_recent_voidable_entries(load, VOID_CANDIDATE_WINDOW)
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
    authoritative_thread_keep: serenity::all::ChannelId,
    retired_thread_ids: &[serenity::all::ChannelId],
    observed_candidates: &[serenity::all::ChannelId],
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
    use serenity::all::ChannelId;

    fn channel(id: u64) -> ChannelId {
        ChannelId::new(id)
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
