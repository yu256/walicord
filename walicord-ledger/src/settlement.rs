use std::collections::{BTreeMap, BTreeSet};
use walicord_domain::{Money, NonEmptyVec, Transfer, model::MemberId};

/// Records normalized transfers without closing ledger history.
#[derive(Debug, Clone, PartialEq)]
pub struct NormalizedSettlementPlanRecorded {
    transfers: NonEmptyVec<Transfer>,
}

impl NormalizedSettlementPlanRecorded {
    pub fn new(transfers: Vec<Transfer>) -> Result<Self, NormalizedSettlementPlanRecordedError> {
        if transfers.is_empty() {
            return Err(NormalizedSettlementPlanRecordedError::EmptyTransfers);
        }

        let mut canonical = BTreeMap::new();
        let mut senders = BTreeSet::new();
        let mut receivers = BTreeSet::new();

        for transfer in transfers {
            if transfer.amount <= Money::ZERO || transfer.from == transfer.to {
                return Err(NormalizedSettlementPlanRecordedError::InvalidTransfer {
                    from: transfer.from,
                    to: transfer.to,
                    amount: transfer.amount,
                });
            }

            if canonical.contains_key(&(transfer.to, transfer.from)) {
                return Err(
                    NormalizedSettlementPlanRecordedError::OpposingTransferPair {
                        from: transfer.from,
                        to: transfer.to,
                    },
                );
            }

            if receivers.contains(&transfer.from) {
                return Err(
                    NormalizedSettlementPlanRecordedError::OverlappingTransferMember {
                        member_id: transfer.from,
                    },
                );
            }
            if senders.contains(&transfer.to) {
                return Err(
                    NormalizedSettlementPlanRecordedError::OverlappingTransferMember {
                        member_id: transfer.to,
                    },
                );
            }

            senders.insert(transfer.from);
            receivers.insert(transfer.to);

            *canonical
                .entry((transfer.from, transfer.to))
                .or_insert(Money::ZERO) += transfer.amount;
        }

        let transfers: Vec<Transfer> = canonical
            .into_iter()
            .map(|((from, to), amount)| Transfer { from, to, amount })
            .collect();

        Ok(Self {
            transfers: NonEmptyVec::new(transfers).expect(
                "transfers is non-empty (checked above) and the canonicalizing fold only \
                 inserts entries, never removes them",
            ),
        })
    }

    pub fn transfers(&self) -> &NonEmptyVec<Transfer> {
        &self.transfers
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum NormalizedSettlementPlanRecordedError {
    #[error("settlement plan has no transfers")]
    EmptyTransfers,
    #[error("invalid transfer: {from:?} -> {to:?} amount {amount:?}")]
    InvalidTransfer {
        from: MemberId,
        to: MemberId,
        amount: Money,
    },
    #[error("opposing transfer pair: {from:?} <-> {to:?}")]
    OpposingTransferPair { from: MemberId, to: MemberId },
    #[error("transfers overlap on member {member_id:?}")]
    OverlappingTransferMember { member_id: MemberId },
}
