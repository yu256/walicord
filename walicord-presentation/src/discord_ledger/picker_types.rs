use std::{fmt, num::ParseIntError, str::FromStr};

use walicord_application::ledger::expense_session::ParticipantSelectionMode;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, strum::Display, strum::EnumString, strum::EnumIter,
)]
#[strum(serialize_all = "snake_case")]
pub enum ExpensePickerKind {
    Payer,
    Individuals,
    Roles,
}

impl ExpensePickerKind {
    pub fn all() -> impl Iterator<Item = Self> {
        use strum::IntoEnumIterator;
        Self::iter()
    }
}

impl From<ParticipantSelectionMode> for ExpensePickerKind {
    fn from(mode: ParticipantSelectionMode) -> Self {
        match mode {
            ParticipantSelectionMode::Payer => Self::Payer,
            ParticipantSelectionMode::Individual => Self::Individuals,
            ParticipantSelectionMode::Roles => Self::Roles,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PickerSnapshotId(u64);

impl PickerSnapshotId {
    pub fn new(value: u64) -> Self {
        Self(value)
    }
}

impl fmt::Display for PickerSnapshotId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for PickerSnapshotId {
    type Err = ParseIntError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        value.parse::<u64>().map(Self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PagedPickerState {
    pub snapshot_id: PickerSnapshotId,
    pub current_page: usize,
    pub query: Option<String>,
}
