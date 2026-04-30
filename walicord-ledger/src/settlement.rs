use std::collections::{BTreeMap, BTreeSet};
use walicord_domain::{Money, Transfer, model::MemberId};

/// Records normalized transfers without closing ledger history.
#[derive(Debug, Clone, PartialEq)]
pub struct NormalizedSettlementPlanRecorded {
    transfers: Vec<Transfer>,
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

        let transfers = canonical
            .into_iter()
            .map(|((from, to), amount)| Transfer { from, to, amount })
            .collect();

        Ok(Self { transfers })
    }

    pub fn transfers(&self) -> &[Transfer] {
        &self.transfers
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NormalizedSettlementPlanRecordedError {
    EmptyTransfers,
    InvalidTransfer {
        from: MemberId,
        to: MemberId,
        amount: Money,
    },
    OpposingTransferPair {
        from: MemberId,
        to: MemberId,
    },
    OverlappingTransferMember {
        member_id: MemberId,
    },
}
