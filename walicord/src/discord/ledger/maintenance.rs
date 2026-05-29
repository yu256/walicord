use serenity::all::ChannelId;
use walicord_application::ledger::{
    LedgerEntryId, LedgerId, maintenance::OlderThanTwentyVoidRecoveryRequest,
};

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
        authoritative_thread_keep: ChannelId,
        retired_thread_ids: Vec<ChannelId>,
    },
    /// Whole-thread replacement to a fresh `LedgerId` on a new thread (criterion 199 /
    /// 291) after irrecoverable damage; same-parent dual-thread is forbidden.
    DamagedThreadReplacement {
        retired_ledger_id: LedgerId,
        new_parent_channel_id: ChannelId,
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

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum DiscordMaintenanceValidationError {
    /// Damaged-thread replacement cannot reuse the retired ledger's parent channel
    /// (criterion 199: same-parent dual thread forbidden).
    #[error("damaged-thread replacement cannot reuse the retired thread's parent channel")]
    SameParentDualThread,
    /// Duplicate-resolution must list at least one retired thread.
    #[error("duplicate-resolution requires at least one retired thread")]
    DuplicateResolutionEmpty,
    /// The authoritative thread the operator chose to keep is not among the observed
    /// duplicate candidates.
    #[error("authoritative thread is not among the observed duplicate candidates")]
    AuthoritativeNotAmongCandidates,
}

/// Validate a `DuplicateThreadResolution` command: must list >= 1 retired thread, and
/// the chosen authoritative thread must be among the duplicates the locator reported.
pub fn validate_duplicate_resolution(
    authoritative_thread_keep: ChannelId,
    retired_thread_ids: &[ChannelId],
    observed_candidates: &[ChannelId],
) -> Result<(), DiscordMaintenanceValidationError> {
    if retired_thread_ids.is_empty() {
        return Err(DiscordMaintenanceValidationError::DuplicateResolutionEmpty);
    }
    if !observed_candidates.contains(&authoritative_thread_keep) {
        return Err(DiscordMaintenanceValidationError::AuthoritativeNotAmongCandidates);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn channel(id: u64) -> ChannelId {
        ChannelId::new(id)
    }

    #[test]
    fn duplicate_resolution_rejects_empty_retired_list() {
        let actual = validate_duplicate_resolution(channel(1), &[], &[channel(1), channel(2)]);
        assert_eq!(
            actual,
            Err(DiscordMaintenanceValidationError::DuplicateResolutionEmpty)
        );
    }

    #[test]
    fn duplicate_resolution_rejects_authoritative_not_in_observed() {
        let actual =
            validate_duplicate_resolution(channel(999), &[channel(2)], &[channel(1), channel(2)]);
        assert_eq!(
            actual,
            Err(DiscordMaintenanceValidationError::AuthoritativeNotAmongCandidates)
        );
    }

    #[test]
    fn duplicate_resolution_accepts_authoritative_in_observed_with_at_least_one_retired() {
        let actual =
            validate_duplicate_resolution(channel(1), &[channel(2)], &[channel(1), channel(2)]);
        assert_eq!(actual, Ok(()));
    }
}
