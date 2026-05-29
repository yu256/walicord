use walicord_domain::{Money, model::MemberId};

use crate::LedgerEntryId;

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum LedgerStructureError {
    #[error("duplicate ledger entry id: {entry_id:?}")]
    DuplicateEntryId { entry_id: LedgerEntryId },
    #[error("entry {entry_id:?} voids unknown target {target:?}")]
    UnknownVoidTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    #[error("entry {entry_id:?} voids future target {target:?}")]
    FutureVoidTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    #[error("entry {entry_id:?} voids itself")]
    SelfVoid { entry_id: LedgerEntryId },
    #[error("entry {entry_id:?} seals unknown target {target:?}")]
    UnknownSealTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    #[error("entry {entry_id:?} seals future target {target:?}")]
    FutureSealTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    #[error(
        "balance adjustment entry {entry_id:?} references unknown related entry {related_entry:?}"
    )]
    UnknownBalanceAdjustmentRelatedEntry {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    #[error(
        "balance adjustment entry {entry_id:?} references future related entry {related_entry:?}"
    )]
    FutureBalanceAdjustmentRelatedEntry {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum LedgerProjectionError {
    #[error("entry {entry_id:?} voids a void entry {target:?}")]
    VoidTargetIsVoid {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    #[error("entry {entry_id:?} voids a seal entry {target:?}")]
    VoidTargetIsLedgerHistorySealed {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    #[error("entry {entry_id:?} voids a balance adjustment entry {target:?}")]
    VoidTargetIsBalanceAdjustment {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    #[error("entry {entry_id:?} duplicates a void on target {target:?}")]
    DuplicateVoidTarget {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    #[error("entry {entry_id:?} voids already-sealed target {target:?}")]
    VoidTargetAlreadySealed {
        entry_id: LedgerEntryId,
        target: LedgerEntryId,
    },
    #[error(
        "settlement transfer entry {entry_id:?}: member {member_id:?} has no debt (balance={balance:?})"
    )]
    SettlementTransferWithoutDebt {
        entry_id: LedgerEntryId,
        member_id: MemberId,
        balance: Money,
    },
    #[error(
        "settlement transfer entry {entry_id:?}: member {member_id:?} has no credit (balance={balance:?})"
    )]
    SettlementTransferWithoutCredit {
        entry_id: LedgerEntryId,
        member_id: MemberId,
        balance: Money,
    },
    #[error(
        "settlement transfer entry {entry_id:?} overpays debt of member {member_id:?} (balance={balance:?}, amount={amount:?})"
    )]
    SettlementTransferOverpaysDebt {
        entry_id: LedgerEntryId,
        member_id: MemberId,
        balance: Money,
        amount: Money,
    },
    #[error(
        "settlement transfer entry {entry_id:?} overpays credit of member {member_id:?} (balance={balance:?}, amount={amount:?})"
    )]
    SettlementTransferOverpaysCredit {
        entry_id: LedgerEntryId,
        member_id: MemberId,
        balance: Money,
        amount: Money,
    },
    #[error("balance adjustment entry {entry_id:?} has no preceding sealed ledger history")]
    BalanceAdjustmentWithoutSealedHistory { entry_id: LedgerEntryId },
    #[error(
        "balance adjustment entry {entry_id:?} references unsealed related entry {related_entry:?}"
    )]
    BalanceAdjustmentRelatedEntryNotSealed {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    #[error(
        "balance adjustment entry {entry_id:?} references voided related entry {related_entry:?}"
    )]
    BalanceAdjustmentRelatedEntryVoided {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    #[error(
        "balance adjustment entry {entry_id:?} references unsupported related entry kind ({related_entry:?})"
    )]
    BalanceAdjustmentRelatedEntryUnsupported {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    #[error("balance adjustment entry {entry_id:?} references unrelated entry {related_entry:?}")]
    BalanceAdjustmentRelatedEntryUnrelated {
        entry_id: LedgerEntryId,
        related_entry: LedgerEntryId,
    },
    #[error("ledger state is imbalanced (residual total: {total:?})")]
    ImbalancedLedgerState { total: Money },
}
