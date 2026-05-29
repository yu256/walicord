use walicord_domain::model::MemberId;
use walicord_ledger::{LedgerEntryId, LedgerId};

use super::{
    projection::{VerifiedLedgerThreadLoad, project_recent_voidable_entries},
    void_flow::VOID_CANDIDATE_WINDOW,
};

/// Inputs an operator binary must supply when running the criterion-289
/// older-than-window void recovery. The application crate owns this shape because the
/// payload is pure ledger data; the adapter-side `MaintenanceCommand` enum wraps it
/// alongside Discord-specific transport identifiers.
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
