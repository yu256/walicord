use std::fmt;

use super::picker_types::{ExpensePickerKind, PickerSnapshotId};
use walicord_application::SessionNonce;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpenseComponentId {
    SwitchIndividual(SessionNonce),
    SwitchRoles(SessionNonce),
    SwitchPayer(SessionNonce),
    MembersToggle(SessionNonce),
    ToConfirm(SessionNonce),
    Cancel(SessionNonce),

    Record(SessionNonce),
    WeightEdit(SessionNonce),
    ModifySelection(SessionNonce),
    BasicEdit(SessionNonce),

    ModalRetry(SessionNonce),

    PickerSelectPayer {
        nonce: SessionNonce,
        snapshot_id: PickerSnapshotId,
    },
    PickerSelectIndividual {
        nonce: SessionNonce,
        snapshot_id: PickerSnapshotId,
    },
    PickerSelectRole {
        nonce: SessionNonce,
        snapshot_id: PickerSnapshotId,
    },
    PickerPrev {
        kind: ExpensePickerKind,
        nonce: SessionNonce,
        snapshot_id: PickerSnapshotId,
    },
    PickerNext {
        kind: ExpensePickerKind,
        nonce: SessionNonce,
        snapshot_id: PickerSnapshotId,
    },
    PickerSearch {
        kind: ExpensePickerKind,
        nonce: SessionNonce,
        snapshot_id: PickerSnapshotId,
    },
    PickerClear {
        kind: ExpensePickerKind,
        nonce: SessionNonce,
        snapshot_id: PickerSnapshotId,
    },
    PickerSearchModal {
        kind: ExpensePickerKind,
        nonce: SessionNonce,
        snapshot_id: PickerSnapshotId,
    },
}

const PREFIX: &str = "ledger:expense:";

impl ExpenseComponentId {
    pub fn parse(custom_id: &str) -> Option<Self> {
        let rest = custom_id.strip_prefix(PREFIX)?;
        let (slug, payload) = rest.split_once(':')?;
        match slug {
            "switch-individual" => parse_nonce(payload).map(Self::SwitchIndividual),
            "switch-roles" => parse_nonce(payload).map(Self::SwitchRoles),
            "switch-payer" => parse_nonce(payload).map(Self::SwitchPayer),
            "members-toggle" => parse_nonce(payload).map(Self::MembersToggle),
            "to-confirm" => parse_nonce(payload).map(Self::ToConfirm),
            "cancel" => parse_nonce(payload).map(Self::Cancel),
            "record" => parse_nonce(payload).map(Self::Record),
            "weight-edit" => parse_nonce(payload).map(Self::WeightEdit),
            "modify-selection" => parse_nonce(payload).map(Self::ModifySelection),
            "basic-edit" => parse_nonce(payload).map(Self::BasicEdit),
            "retry" => parse_nonce(payload).map(Self::ModalRetry),
            "picker-select-payer" => parse_nonce_snapshot(payload, |nonce, snapshot_id| {
                Self::PickerSelectPayer { nonce, snapshot_id }
            }),
            "picker-select-individual" => parse_nonce_snapshot(payload, |nonce, snapshot_id| {
                Self::PickerSelectIndividual { nonce, snapshot_id }
            }),
            "picker-select-role" => parse_nonce_snapshot(payload, |nonce, snapshot_id| {
                Self::PickerSelectRole { nonce, snapshot_id }
            }),
            "picker-prev" => parse_picker(payload, |kind, nonce, snapshot_id| Self::PickerPrev {
                kind,
                nonce,
                snapshot_id,
            }),
            "picker-next" => parse_picker(payload, |kind, nonce, snapshot_id| Self::PickerNext {
                kind,
                nonce,
                snapshot_id,
            }),
            "picker-search" => {
                parse_picker(payload, |kind, nonce, snapshot_id| Self::PickerSearch {
                    kind,
                    nonce,
                    snapshot_id,
                })
            }
            "picker-clear" => parse_picker(payload, |kind, nonce, snapshot_id| Self::PickerClear {
                kind,
                nonce,
                snapshot_id,
            }),
            "picker-search-modal" => parse_picker(payload, |kind, nonce, snapshot_id| {
                Self::PickerSearchModal {
                    kind,
                    nonce,
                    snapshot_id,
                }
            }),
            _ => None,
        }
    }
}

impl fmt::Display for ExpenseComponentId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::SwitchIndividual(n) => write!(f, "{PREFIX}switch-individual:{n}"),
            Self::SwitchRoles(n) => write!(f, "{PREFIX}switch-roles:{n}"),
            Self::SwitchPayer(n) => write!(f, "{PREFIX}switch-payer:{n}"),
            Self::MembersToggle(n) => write!(f, "{PREFIX}members-toggle:{n}"),
            Self::ToConfirm(n) => write!(f, "{PREFIX}to-confirm:{n}"),
            Self::Cancel(n) => write!(f, "{PREFIX}cancel:{n}"),
            Self::Record(n) => write!(f, "{PREFIX}record:{n}"),
            Self::WeightEdit(n) => write!(f, "{PREFIX}weight-edit:{n}"),
            Self::ModifySelection(n) => write!(f, "{PREFIX}modify-selection:{n}"),
            Self::BasicEdit(n) => write!(f, "{PREFIX}basic-edit:{n}"),
            Self::ModalRetry(n) => write!(f, "{PREFIX}retry:{n}"),
            Self::PickerSelectPayer { nonce, snapshot_id } => {
                write!(f, "{PREFIX}picker-select-payer:{nonce}:{snapshot_id}")
            }
            Self::PickerSelectIndividual { nonce, snapshot_id } => {
                write!(f, "{PREFIX}picker-select-individual:{nonce}:{snapshot_id}")
            }
            Self::PickerSelectRole { nonce, snapshot_id } => {
                write!(f, "{PREFIX}picker-select-role:{nonce}:{snapshot_id}")
            }
            Self::PickerPrev {
                kind,
                nonce,
                snapshot_id,
            } => write!(f, "{PREFIX}picker-prev:{kind}:{nonce}:{snapshot_id}"),
            Self::PickerNext {
                kind,
                nonce,
                snapshot_id,
            } => write!(f, "{PREFIX}picker-next:{kind}:{nonce}:{snapshot_id}"),
            Self::PickerSearch {
                kind,
                nonce,
                snapshot_id,
            } => write!(f, "{PREFIX}picker-search:{kind}:{nonce}:{snapshot_id}"),
            Self::PickerClear {
                kind,
                nonce,
                snapshot_id,
            } => write!(f, "{PREFIX}picker-clear:{kind}:{nonce}:{snapshot_id}"),
            Self::PickerSearchModal {
                kind,
                nonce,
                snapshot_id,
            } => write!(
                f,
                "{PREFIX}picker-search-modal:{kind}:{nonce}:{snapshot_id}"
            ),
        }
    }
}

fn parse_nonce(payload: &str) -> Option<SessionNonce> {
    let value = payload.parse::<u64>().ok()?;
    SessionNonce::new(value).ok()
}

fn parse_nonce_snapshot(
    payload: &str,
    constructor: impl FnOnce(SessionNonce, PickerSnapshotId) -> ExpenseComponentId,
) -> Option<ExpenseComponentId> {
    let (nonce_str, snapshot_str) = payload.split_once(':')?;
    let nonce = parse_nonce(nonce_str)?;
    let snapshot_id = snapshot_str.parse::<PickerSnapshotId>().ok()?;
    Some(constructor(nonce, snapshot_id))
}

fn parse_picker(
    payload: &str,
    constructor: impl FnOnce(ExpensePickerKind, SessionNonce, PickerSnapshotId) -> ExpenseComponentId,
) -> Option<ExpenseComponentId> {
    let (kind_str, rest) = payload.split_once(':')?;
    let (nonce_str, snapshot_str) = rest.split_once(':')?;
    let kind = kind_str.parse::<ExpensePickerKind>().ok()?;
    let nonce = parse_nonce(nonce_str)?;
    let snapshot_id = snapshot_str.parse::<PickerSnapshotId>().ok()?;
    Some(constructor(kind, nonce, snapshot_id))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    fn nonce(v: u64) -> SessionNonce {
        SessionNonce::new(v).unwrap()
    }

    #[rstest]
    #[case::switch_individual(ExpenseComponentId::SwitchIndividual(nonce(1)))]
    #[case::switch_roles(ExpenseComponentId::SwitchRoles(nonce(2)))]
    #[case::switch_payer(ExpenseComponentId::SwitchPayer(nonce(3)))]
    #[case::members_toggle(ExpenseComponentId::MembersToggle(nonce(4)))]
    #[case::to_confirm(ExpenseComponentId::ToConfirm(nonce(5)))]
    #[case::cancel(ExpenseComponentId::Cancel(nonce(6)))]
    #[case::record(ExpenseComponentId::Record(nonce(7)))]
    #[case::weight_edit(ExpenseComponentId::WeightEdit(nonce(8)))]
    #[case::modify_selection(ExpenseComponentId::ModifySelection(nonce(9)))]
    #[case::basic_edit(ExpenseComponentId::BasicEdit(nonce(10)))]
    #[case::modal_retry(ExpenseComponentId::ModalRetry(nonce(11)))]
    #[case::picker_select_payer(ExpenseComponentId::PickerSelectPayer {
        nonce: nonce(42), snapshot_id: PickerSnapshotId::new(999)
    })]
    #[case::picker_select_individual(ExpenseComponentId::PickerSelectIndividual {
        nonce: nonce(43), snapshot_id: PickerSnapshotId::new(999)
    })]
    #[case::picker_select_role(ExpenseComponentId::PickerSelectRole {
        nonce: nonce(44), snapshot_id: PickerSnapshotId::new(999)
    })]
    #[case::picker_prev(ExpenseComponentId::PickerPrev {
        kind: ExpensePickerKind::Individuals, nonce: nonce(43), snapshot_id: PickerSnapshotId::new(999)
    })]
    #[case::picker_next(ExpenseComponentId::PickerNext {
        kind: ExpensePickerKind::Roles, nonce: nonce(44), snapshot_id: PickerSnapshotId::new(999)
    })]
    #[case::picker_search(ExpenseComponentId::PickerSearch {
        kind: ExpensePickerKind::Payer, nonce: nonce(45), snapshot_id: PickerSnapshotId::new(999)
    })]
    #[case::picker_clear(ExpenseComponentId::PickerClear {
        kind: ExpensePickerKind::Individuals, nonce: nonce(46), snapshot_id: PickerSnapshotId::new(999)
    })]
    #[case::picker_search_modal(ExpenseComponentId::PickerSearchModal {
        kind: ExpensePickerKind::Roles, nonce: nonce(47), snapshot_id: PickerSnapshotId::new(999)
    })]
    fn display_then_parse_roundtrips(#[case] id: ExpenseComponentId) {
        let serialized = id.to_string();
        assert_eq!(ExpenseComponentId::parse(&serialized), Some(id));
    }

    #[rstest]
    #[case::unknown_slug("ledger:expense:unknown:1")]
    #[case::zero_nonce("ledger:expense:cancel:0")]
    #[case::wrong_prefix("ledger:void:cancel:1")]
    #[case::non_numeric_nonce("ledger:expense:cancel:abc")]
    #[case::empty("")]
    #[case::picker_missing_snapshot("ledger:expense:picker-next:payer:1")]
    #[case::picker_invalid_kind("ledger:expense:picker-next:unknown:1:2")]
    fn parse_rejects_malformed_input(#[case] input: &str) {
        assert_eq!(ExpenseComponentId::parse(input), None);
    }
}
