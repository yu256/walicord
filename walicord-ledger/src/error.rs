use walicord_domain::{Money, model::MemberId};

use crate::LedgerEntryId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerStructureError {
    DuplicateEntryId {
        entry_id: LedgerEntryId,
    },
    UnknownVoidTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    FutureVoidTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    SelfVoid {
        entry_id: LedgerEntryId,
    },
    UnknownSealTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    FutureSealTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    UnknownBalanceAdjustmentRelatedEntry {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    FutureBalanceAdjustmentRelatedEntry {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerProjectionError {
    VoidTargetIsVoid {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    VoidTargetIsLedgerHistorySealed {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    VoidTargetIsBalanceAdjustment {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    DuplicateVoidTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    VoidTargetAlreadySealed {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    SettlementTransferWithoutDebt {
        entry_id: LedgerEntryId,
        member_id: MemberId,
        balance: Money,
    },
    SettlementTransferWithoutCredit {
        entry_id: LedgerEntryId,
        member_id: MemberId,
        balance: Money,
    },
    SettlementTransferOverpaysDebt {
        entry_id: LedgerEntryId,
        member_id: MemberId,
        balance: Money,
        amount: Money,
    },
    SettlementTransferOverpaysCredit {
        entry_id: LedgerEntryId,
        member_id: MemberId,
        balance: Money,
        amount: Money,
    },
    BalanceAdjustmentWithoutSealedHistory {
        entry_id: LedgerEntryId,
    },
    BalanceAdjustmentRelatedEntryNotSealed {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    BalanceAdjustmentRelatedEntryVoided {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    BalanceAdjustmentRelatedEntryUnsupported {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    BalanceAdjustmentRelatedEntryUnrelated {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    ImbalancedLedgerState {
        total: Money,
    },
}
