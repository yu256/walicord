use std::collections::BTreeMap;
use walicord_domain::{Money, model::MemberId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberAmount {
    pub member_id: MemberId,
    pub amount: Money,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseNote(String);

impl ExpenseNote {
    pub fn new(note: impl Into<String>) -> Result<Self, ExpenseNoteError> {
        let note = note.into().trim().to_owned();
        if note.is_empty() {
            return Err(ExpenseNoteError::Empty);
        }
        Ok(Self(note))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpenseNoteError {
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpenseRecorded {
    paid_by: Vec<MemberAmount>,
    owed_by: Vec<MemberAmount>,
    note: Option<ExpenseNote>,
}

impl ExpenseRecorded {
    pub fn new(
        paid_by: Vec<MemberAmount>,
        owed_by: Vec<MemberAmount>,
        note: Option<ExpenseNote>,
    ) -> Result<Self, ExpenseRecordedError> {
        let paid_by = canonicalize_positive_amounts(paid_by)?;
        let owed_by = canonicalize_positive_amounts(owed_by)?;

        if paid_by.is_empty() || owed_by.is_empty() {
            return Err(ExpenseRecordedError::EmptyParticipants);
        }

        let paid_total = paid_by.iter().map(|paid| paid.amount).sum();
        let owed_total = owed_by.iter().map(|owed| owed.amount).sum();

        if paid_total != owed_total {
            return Err(ExpenseRecordedError::Imbalanced {
                paid_total,
                owed_total,
            });
        }

        Ok(Self {
            paid_by,
            owed_by,
            note,
        })
    }

    pub fn paid_by(&self) -> &[MemberAmount] {
        &self.paid_by
    }

    pub fn owed_by(&self) -> &[MemberAmount] {
        &self.owed_by
    }

    pub fn note(&self) -> Option<&ExpenseNote> {
        self.note.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpenseRecordedError {
    EmptyParticipants,
    NonPositiveAmount {
        member_id: MemberId,
        amount: Money,
    },
    Imbalanced {
        paid_total: Money,
        owed_total: Money,
    },
}

fn canonicalize_positive_amounts(
    amounts: Vec<MemberAmount>,
) -> Result<Vec<MemberAmount>, ExpenseRecordedError> {
    let mut canonical = BTreeMap::new();

    for amount in amounts {
        if amount.amount <= Money::ZERO {
            return Err(ExpenseRecordedError::NonPositiveAmount {
                member_id: amount.member_id,
                amount: amount.amount,
            });
        }
        *canonical.entry(amount.member_id).or_insert(Money::ZERO) += amount.amount;
    }

    Ok(canonical
        .into_iter()
        .map(|(member_id, amount)| MemberAmount { member_id, amount })
        .collect())
}
