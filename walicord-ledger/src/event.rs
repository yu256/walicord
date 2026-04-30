use crate::{BalanceAdjusted, ExpenseRecorded, LedgerEntryId, NormalizedSettlementPlanRecorded};

/// Seals ledger history through a specific prior entry without implying that settlement is complete.
/// Application-layer command vocabulary should prefer `seal` / `freeze` / `confirm reviewed range`
/// over `close`, since end users tend to read `closed` as `settled`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LedgerHistorySealed {
    through: LedgerEntryId,
}

impl LedgerHistorySealed {
    pub fn new(through: LedgerEntryId) -> Self {
        Self { through }
    }

    pub fn through(&self) -> LedgerEntryId {
        self.through
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EntryVoided {
    target: LedgerEntryId,
}

impl EntryVoided {
    pub fn new(target: LedgerEntryId) -> Self {
        Self { target }
    }

    pub fn target(&self) -> LedgerEntryId {
        self.target
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LedgerEvent {
    ExpenseRecorded(ExpenseRecorded),
    NormalizedSettlementPlanRecorded(NormalizedSettlementPlanRecorded),
    LedgerHistorySealed(LedgerHistorySealed),
    EntryVoided(EntryVoided),
    BalanceAdjusted(BalanceAdjusted),
}
