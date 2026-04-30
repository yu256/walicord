use std::collections::BTreeMap;

use crate::{LedgerEntryId, LedgerEvent, LedgerRecord, LedgerStructureError};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StructuralValidation {
    pub(crate) entry_positions: BTreeMap<LedgerEntryId, usize>,
}

pub(crate) fn validate_structure(
    entries: &[LedgerRecord],
) -> Result<StructuralValidation, LedgerStructureError> {
    let mut entry_positions = BTreeMap::new();

    for (index, entry) in entries.iter().enumerate() {
        if entry_positions.insert(entry.id, index).is_some() {
            return Err(LedgerStructureError::DuplicateEntryId { entry_id: entry.id });
        }
    }

    for (index, entry) in entries.iter().enumerate() {
        match &entry.event {
            LedgerEvent::LedgerHistorySealed(event) => {
                validate_seal_target(&entry_positions, index, entry.id, event.through())?;
            }
            LedgerEvent::EntryVoided(event) => {
                validate_void_reference(&entry_positions, index, entry.id, event.target())?;
            }
            LedgerEvent::BalanceAdjusted(event) => {
                if let Some(related_entry) = event.related_entry() {
                    validate_balance_adjustment_related_entry(
                        &entry_positions,
                        index,
                        entry.id,
                        related_entry,
                    )?;
                }
            }
            LedgerEvent::ExpenseRecorded(_) | LedgerEvent::NormalizedSettlementPlanRecorded(_) => {}
        }
    }

    Ok(StructuralValidation { entry_positions })
}

fn validate_seal_target(
    entry_positions: &BTreeMap<LedgerEntryId, usize>,
    entry_position: usize,
    entry_id: LedgerEntryId,
    target: LedgerEntryId,
) -> Result<usize, LedgerStructureError> {
    let Some(&target_position) = entry_positions.get(&target) else {
        return Err(LedgerStructureError::UnknownSealTarget { entry_id, target });
    };

    if target_position >= entry_position {
        return Err(LedgerStructureError::FutureSealTarget { entry_id, target });
    }

    Ok(target_position)
}

fn validate_balance_adjustment_related_entry(
    entry_positions: &BTreeMap<LedgerEntryId, usize>,
    entry_position: usize,
    entry_id: LedgerEntryId,
    related_entry: LedgerEntryId,
) -> Result<usize, LedgerStructureError> {
    let Some(&related_position) = entry_positions.get(&related_entry) else {
        return Err(LedgerStructureError::UnknownBalanceAdjustmentRelatedEntry {
            entry_id,
            related_entry,
        });
    };

    if related_position >= entry_position {
        return Err(LedgerStructureError::FutureBalanceAdjustmentRelatedEntry {
            entry_id,
            related_entry,
        });
    }

    Ok(related_position)
}

fn validate_void_reference(
    entry_positions: &BTreeMap<LedgerEntryId, usize>,
    entry_position: usize,
    entry_id: LedgerEntryId,
    target: LedgerEntryId,
) -> Result<(), LedgerStructureError> {
    if entry_id == target {
        return Err(LedgerStructureError::SelfVoid { entry_id });
    }

    let Some(&target_position) = entry_positions.get(&target) else {
        return Err(LedgerStructureError::UnknownVoidTarget { entry_id, target });
    };

    if target_position > entry_position {
        return Err(LedgerStructureError::FutureVoidTarget { entry_id, target });
    }

    Ok(())
}
