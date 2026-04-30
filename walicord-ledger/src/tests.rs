use std::collections::BTreeSet;

use walicord_domain::{MemberBalances, Money, Transfer, model::MemberId};

use super::*;

fn amount(member_id: u64, amount: i64) -> MemberAmount {
    MemberAmount {
        member_id: MemberId(member_id),
        amount: Money::from_i64(amount),
    }
}

fn transfer(from: u64, to: u64, amount: i64) -> Transfer {
    Transfer {
        from: MemberId(from),
        to: MemberId(to),
        amount: Money::from_i64(amount),
    }
}

fn entry(id: u64, event: LedgerEvent) -> LedgerRecord {
    LedgerRecord::new(LedgerEntryId(id), event)
}

fn ledger<const N: usize>(
    entries: [LedgerRecord; N],
) -> Result<AppendOrderedLedgerRecords, LedgerStructureError> {
    AppendOrderedLedgerRecords::new(entries.into())
}

fn replay<const N: usize>(
    entries: [LedgerRecord; N],
) -> Result<LedgerState, LedgerProjectionError> {
    project(entries).map(ProjectedLedger::into_state)
}

fn project<const N: usize>(
    entries: [LedgerRecord; N],
) -> Result<ProjectedLedger, LedgerProjectionError> {
    let entries = ledger(entries).expect("ledger should be structurally valid");
    LedgerProjector::replay(&entries)
}

fn expense_event(paid_by: Vec<MemberAmount>, owed_by: Vec<MemberAmount>) -> LedgerEvent {
    LedgerEvent::ExpenseRecorded(raw_expense(paid_by, owed_by))
}

fn raw_expense(paid_by: Vec<MemberAmount>, owed_by: Vec<MemberAmount>) -> ExpenseRecorded {
    ExpenseRecorded::new(paid_by, owed_by, None).expect("expense should be valid")
}

fn settlement_transfers_recorded(transfers: Vec<Transfer>) -> LedgerEvent {
    LedgerEvent::NormalizedSettlementPlanRecorded(
        NormalizedSettlementPlanRecorded::new(transfers).expect("transfers should be valid"),
    )
}

fn seal(through: u64) -> LedgerEvent {
    LedgerEvent::LedgerHistorySealed(LedgerHistorySealed::new(LedgerEntryId(through)))
}

fn void(target: u64) -> LedgerEvent {
    LedgerEvent::EntryVoided(EntryVoided::new(LedgerEntryId(target)))
}

fn balance_adjustment(adjustments: Vec<BalanceAdjustment>) -> LedgerEvent {
    LedgerEvent::BalanceAdjusted(
        BalanceAdjusted::new(
            adjustments,
            reason(),
            BalanceAdjustmentSource::ExternalCorrection(
                AdminCorrectionAuthority::explicit_admin_capability(),
            ),
        )
        .expect("balance adjustment should be valid"),
    )
}

fn balance_adjustment_related(
    adjustments: Vec<BalanceAdjustment>,
    related_entry: u64,
) -> LedgerEvent {
    LedgerEvent::BalanceAdjusted(
        BalanceAdjusted::new(
            adjustments,
            reason(),
            BalanceAdjustmentSource::SealedEntryCorrection(LedgerEntryId(related_entry)),
        )
        .expect("balance adjustment should be valid"),
    )
}

fn balance_adjustment_correcting_prior(
    adjustments: Vec<BalanceAdjustment>,
    prior_adjustment_entry: u64,
) -> LedgerEvent {
    LedgerEvent::BalanceAdjusted(
        BalanceAdjusted::new(
            adjustments,
            reason(),
            BalanceAdjustmentSource::PriorAdjustmentCorrection(LedgerEntryId(
                prior_adjustment_entry,
            )),
        )
        .expect("balance adjustment should be valid"),
    )
}

fn reason() -> AdjustmentReason {
    AdjustmentReason::new("manual correction").expect("reason should be valid")
}

fn adjustment(member_id: u64, amount: i64) -> BalanceAdjustment {
    BalanceAdjustment {
        member_id: MemberId(member_id),
        amount: Money::from_i64(amount),
    }
}

fn member_set<const N: usize>(ids: [u64; N]) -> BTreeSet<MemberId> {
    ids.into_iter().map(MemberId).collect()
}

#[test]
fn expense_recorded_canonicalizes_same_side_duplicate_members() {
    let expense = raw_expense(
        vec![amount(2, 100), amount(1, 100), amount(1, 200)],
        vec![amount(3, 200), amount(3, 100), amount(4, 100)],
    );

    assert_eq!(
        expense.paid_by(),
        &[
            MemberAmount {
                member_id: MemberId(1),
                amount: Money::from_i64(300),
            },
            MemberAmount {
                member_id: MemberId(2),
                amount: Money::from_i64(100),
            },
        ]
    );
    assert_eq!(
        expense.owed_by(),
        &[
            MemberAmount {
                member_id: MemberId(3),
                amount: Money::from_i64(300),
            },
            MemberAmount {
                member_id: MemberId(4),
                amount: Money::from_i64(100),
            },
        ]
    );
}

#[test]
fn expense_recorded_carries_optional_note() {
    let expense = ExpenseRecorded::new(
        vec![amount(1, 100)],
        vec![amount(2, 100)],
        Some(ExpenseNote::new("  shared lunch  ").expect("note should be valid")),
    )
    .expect("expense should be valid");

    assert_eq!(
        expense.note().map(ExpenseNote::as_str),
        Some("shared lunch")
    );
}

#[test]
fn expense_recorded_allows_omitting_note() {
    let expense = ExpenseRecorded::new(vec![amount(1, 100)], vec![amount(2, 100)], None)
        .expect("expense should be valid");

    assert_eq!(expense.note(), None);
}

#[test]
fn expense_note_rejects_blank_input() {
    assert_eq!(ExpenseNote::new("   "), Err(ExpenseNoteError::Empty));
}

#[test]
fn settlement_transfers_canonicalize_duplicate_transfers() {
    let settlement_transfers_recorded = NormalizedSettlementPlanRecorded::new(vec![
        transfer(3, 4, 200),
        transfer(1, 2, 100),
        transfer(1, 2, 200),
    ])
    .expect("transfers should be valid");

    assert_eq!(
        settlement_transfers_recorded.transfers(),
        &[transfer(1, 2, 300), transfer(3, 4, 200)]
    );
}

#[test]
fn rejects_settlement_transfers_with_opposing_pairs() {
    let actual =
        NormalizedSettlementPlanRecorded::new(vec![transfer(1, 2, 100), transfer(2, 1, 50)]);

    assert_eq!(
        actual,
        Err(
            NormalizedSettlementPlanRecordedError::OpposingTransferPair {
                from: MemberId(2),
                to: MemberId(1),
            }
        )
    );
}

#[test]
fn rejects_settlement_transfers_where_a_member_is_both_sender_and_receiver() {
    let actual =
        NormalizedSettlementPlanRecorded::new(vec![transfer(1, 2, 100), transfer(3, 1, 50)]);

    assert_eq!(
        actual,
        Err(
            NormalizedSettlementPlanRecordedError::OverlappingTransferMember {
                member_id: MemberId(1),
            }
        )
    );
}

#[test]
fn replays_expense_recorded_events_into_zero_sum_balances() {
    let entries = [
        entry(
            1,
            expense_event(
                vec![amount(1, 900)],
                vec![amount(1, 300), amount(2, 300), amount(3, 300)],
            ),
        ),
        entry(
            2,
            expense_event(vec![amount(2, 600)], vec![amount(1, 300), amount(2, 300)]),
        ),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(
        state.balances(),
        &MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(300)),
            (MemberId(3), Money::from_i64(-300)),
        ])
    );
    assert_eq!(state.participants(), &member_set([1, 2, 3]));
    assert_eq!(state.balances().values().sum::<Money>(), Money::ZERO);
}

#[test]
fn replays_settlement_transfers_without_recomputing_them() {
    let entries = [
        entry(
            1,
            expense_event(
                vec![amount(1, 900)],
                vec![amount(1, 300), amount(2, 300), amount(3, 300)],
            ),
        ),
        entry(2, settlement_transfers_recorded(vec![transfer(3, 1, 300)])),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(
        state.balances(),
        &MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(300)),
            (MemberId(2), Money::from_i64(-300)),
        ])
    );
    assert_eq!(state.participants(), &member_set([1, 2, 3]));
    assert_eq!(state.sealed_through(), None);
}

#[test]
fn voided_expense_entry_ids_do_not_affect_projected_balances() {
    let entries = [
        entry(
            1,
            expense_event(
                vec![amount(1, 900)],
                vec![amount(1, 300), amount(2, 300), amount(3, 300)],
            ),
        ),
        entry(2, void(1)),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(state.balances(), &MemberBalances::default());
    assert_eq!(state.participants(), &member_set([1, 2, 3]));
    assert_eq!(
        state.voided_expense_entry_ids(),
        &BTreeSet::from_iter([LedgerEntryId(1)])
    );
    assert_eq!(
        state.voided_entry_ids(),
        &BTreeSet::from_iter([LedgerEntryId(1)])
    );
}

#[test]
fn append_order_is_preserved_even_when_entry_ids_are_not_sorted() {
    let entries = ledger([
        entry(
            20,
            expense_event(vec![amount(1, 100)], vec![amount(2, 100)]),
        ),
        entry(10, void(20)),
    ])
    .expect("ledger should be structurally valid");

    let state = LedgerProjector::replay(&entries)
        .expect("ledger should replay")
        .into_state();

    assert_eq!(entries.as_slice()[0].id, LedgerEntryId(20));
    assert_eq!(entries.as_slice()[1].id, LedgerEntryId(10));
    assert_eq!(state.balances(), &MemberBalances::default());
    assert_eq!(state.participants(), &member_set([1, 2]));
    assert_eq!(
        state.voided_expense_entry_ids(),
        &BTreeSet::from_iter([LedgerEntryId(20)])
    );
    assert_eq!(
        state.voided_entry_ids(),
        &BTreeSet::from_iter([LedgerEntryId(20)])
    );
}

#[test]
fn rejects_imbalanced_expense_events() {
    let actual = ExpenseRecorded::new(vec![amount(1, 100)], vec![amount(2, 90)], None);

    assert_eq!(
        actual,
        Err(ExpenseRecordedError::Imbalanced {
            paid_total: Money::from_i64(100),
            owed_total: Money::from_i64(90),
        })
    );
}

#[test]
fn rejects_duplicate_entry_ids() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(1, expense_event(vec![amount(2, 100)], vec![amount(1, 100)])),
    ];

    let actual = ledger(entries);

    assert_eq!(
        actual,
        Err(LedgerStructureError::DuplicateEntryId {
            entry_id: LedgerEntryId(1),
        })
    );
}

#[test]
fn rejects_voiding_future_entries() {
    let entries = [
        entry(1, void(2)),
        entry(2, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
    ];

    let actual = ledger(entries);

    assert_eq!(
        actual,
        Err(LedgerStructureError::FutureVoidTarget {
            entry_id: LedgerEntryId(1),
            target: LedgerEntryId(2),
        })
    );
}

#[test]
fn rejects_voiding_unknown_entries() {
    let entries = [entry(1, void(999))];

    let actual = ledger(entries);

    assert_eq!(
        actual,
        Err(LedgerStructureError::UnknownVoidTarget {
            entry_id: LedgerEntryId(1),
            target: LedgerEntryId(999),
        })
    );
}

#[test]
fn rejects_voiding_self() {
    let entries = [entry(1, void(1))];

    let actual = ledger(entries);

    assert_eq!(
        actual,
        Err(LedgerStructureError::SelfVoid {
            entry_id: LedgerEntryId(1),
        })
    );
}

#[test]
fn rejects_voiding_void_entries() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, void(1)),
        entry(3, void(2)),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::VoidTargetIsVoid {
            entry_id: LedgerEntryId(3),
            target: LedgerEntryId(2),
        })
    );
}

#[test]
fn void_reverses_settlement_transfer_effect_when_replayed_in_append_order() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, settlement_transfers_recorded(vec![transfer(2, 1, 100)])),
        entry(3, void(2)),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(
        state.balances(),
        &MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(-100)),
        ])
    );
    assert_eq!(state.voided_expense_entry_ids(), &BTreeSet::new());
    assert_eq!(
        state.voided_entry_ids(),
        &BTreeSet::from_iter([LedgerEntryId(2)])
    );
}

#[test]
fn rejects_voiding_sealed_settlement_transfer_entries() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, settlement_transfers_recorded(vec![transfer(2, 1, 100)])),
        entry(3, seal(2)),
        entry(4, void(2)),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::VoidTargetAlreadySealed {
            entry_id: LedgerEntryId(4),
            target: LedgerEntryId(2),
        })
    );
}

#[test]
fn rejects_voiding_seal_marker_entries() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(3, void(2)),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::VoidTargetIsLedgerHistorySealed {
            entry_id: LedgerEntryId(3),
            target: LedgerEntryId(2),
        })
    );
}

#[test]
fn rejects_voiding_balance_adjustments() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(
            3,
            balance_adjustment(vec![adjustment(1, -25), adjustment(2, 25)]),
        ),
        entry(4, void(3)),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::VoidTargetIsBalanceAdjustment {
            entry_id: LedgerEntryId(4),
            target: LedgerEntryId(3),
        })
    );
}

#[test]
fn rejects_voiding_the_same_entry_twice() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, void(1)),
        entry(3, void(1)),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::DuplicateVoidTarget {
            entry_id: LedgerEntryId(3),
            target: LedgerEntryId(1),
        })
    );
}

#[test]
fn settlement_transfers_do_not_seal_history_by_themselves() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, settlement_transfers_recorded(vec![transfer(2, 1, 100)])),
        entry(3, void(1)),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(
        state.balances(),
        &MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(-100)),
            (MemberId(2), Money::from_i64(100))
        ])
    );
    assert_eq!(state.sealed_through(), None);
}

#[test]
fn reports_invalid_transfer_before_later_void() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, settlement_transfers_recorded(vec![transfer(1, 2, 100)])),
        entry(3, void(1)),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::SettlementTransferWithoutDebt {
            entry_id: LedgerEntryId(2),
            member_id: MemberId(1),
            balance: Money::from_i64(100),
        })
    );
}

#[test]
fn void_reverses_expense_effect_when_replayed_in_append_order() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, void(1)),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(state.balances(), &MemberBalances::default());
    assert_eq!(state.participants(), &member_set([1, 2]));
    assert_eq!(
        state.voided_expense_entry_ids(),
        &BTreeSet::from_iter([LedgerEntryId(1)])
    );
}

#[test]
fn rejects_voiding_entries_before_a_history_sealed_event() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(3, void(1)),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::VoidTargetAlreadySealed {
            entry_id: LedgerEntryId(3),
            target: LedgerEntryId(1),
        })
    );
}

#[test]
fn rejects_history_sealed_with_unknown_target() {
    let entries = [entry(1, seal(999))];

    let actual = ledger(entries);

    assert_eq!(
        actual,
        Err(LedgerStructureError::UnknownSealTarget {
            entry_id: LedgerEntryId(1),
            target: LedgerEntryId(999),
        })
    );
}

#[test]
fn rejects_history_sealed_with_future_target() {
    let entries = [
        entry(1, seal(2)),
        entry(2, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
    ];

    let actual = ledger(entries);

    assert_eq!(
        actual,
        Err(LedgerStructureError::FutureSealTarget {
            entry_id: LedgerEntryId(1),
            target: LedgerEntryId(2),
        })
    );
}

#[test]
fn rejects_empty_expense_participants() {
    let actual = ExpenseRecorded::new(Vec::new(), Vec::new(), None);

    assert_eq!(actual, Err(ExpenseRecordedError::EmptyParticipants));
}

#[test]
fn rejects_non_positive_expense_amounts() {
    let actual = ExpenseRecorded::new(vec![amount(1, -100)], vec![amount(2, -100)], None);

    assert_eq!(
        actual,
        Err(ExpenseRecordedError::NonPositiveAmount {
            member_id: MemberId(1),
            amount: Money::from_i64(-100),
        })
    );
}

#[test]
fn rejects_invalid_recorded_transfers() {
    let actual = NormalizedSettlementPlanRecorded::new(vec![transfer(1, 1, 100)]);

    assert_eq!(
        actual,
        Err(NormalizedSettlementPlanRecordedError::InvalidTransfer {
            from: MemberId(1),
            to: MemberId(1),
            amount: Money::from_i64(100),
        })
    );
}

#[test]
fn rejects_empty_settlement_transfers() {
    let actual = NormalizedSettlementPlanRecorded::new(Vec::new());

    assert_eq!(
        actual,
        Err(NormalizedSettlementPlanRecordedError::EmptyTransfers)
    );
}

#[test]
fn rejects_transfers_without_prior_debt() {
    let entries = [entry(
        1,
        settlement_transfers_recorded(vec![transfer(1, 2, 100)]),
    )];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::SettlementTransferWithoutDebt {
            entry_id: LedgerEntryId(1),
            member_id: MemberId(1),
            balance: Money::ZERO,
        })
    );
}

#[test]
fn rejects_recorded_transfer_in_the_wrong_direction() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, settlement_transfers_recorded(vec![transfer(1, 2, 100)])),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::SettlementTransferWithoutDebt {
            entry_id: LedgerEntryId(2),
            member_id: MemberId(1),
            balance: Money::from_i64(100),
        })
    );
}

#[test]
fn rejects_recorded_transfer_overpaying_debt() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, settlement_transfers_recorded(vec![transfer(2, 1, 200)])),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::SettlementTransferOverpaysDebt {
            entry_id: LedgerEntryId(2),
            member_id: MemberId(2),
            balance: Money::from_i64(-100),
            amount: Money::from_i64(200),
        })
    );
}

#[test]
fn rejects_recorded_transfer_overpaying_credit() {
    let entries = [
        entry(
            1,
            expense_event(vec![amount(1, 100), amount(3, 100)], vec![amount(2, 200)]),
        ),
        entry(2, settlement_transfers_recorded(vec![transfer(2, 1, 150)])),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::SettlementTransferOverpaysCredit {
            entry_id: LedgerEntryId(2),
            member_id: MemberId(1),
            balance: Money::from_i64(100),
            amount: Money::from_i64(150),
        })
    );
}

#[test]
fn rejects_recorded_transfer_for_a_voided_expense() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, void(1)),
        entry(3, settlement_transfers_recorded(vec![transfer(2, 1, 100)])),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::SettlementTransferWithoutDebt {
            entry_id: LedgerEntryId(3),
            member_id: MemberId(2),
            balance: Money::ZERO,
        })
    );
}

#[test]
fn history_sealed_acts_as_a_barrier_without_applying_transfers() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(
        state.balances(),
        &MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(-100)),
        ])
    );
    assert_eq!(state.sealed_through(), Some(LedgerEntryId(1)));
}

#[test]
fn balance_adjustments_can_compensate_after_history_is_sealed() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, settlement_transfers_recorded(vec![transfer(2, 1, 100)])),
        entry(3, seal(2)),
        entry(
            4,
            balance_adjustment_related(vec![adjustment(1, -25), adjustment(2, 25)], 1),
        ),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(
        state.balances(),
        &MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(-25)),
            (MemberId(2), Money::from_i64(25)),
        ])
    );
    assert_eq!(state.sealed_through(), Some(LedgerEntryId(2)));
}

#[test]
fn external_balance_adjustments_are_allowed_after_history_is_sealed() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(
            3,
            balance_adjustment(vec![adjustment(1, -25), adjustment(2, 25)]),
        ),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(
        state.balances(),
        &MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(75)),
            (MemberId(2), Money::from_i64(-75)),
        ])
    );
}

#[test]
fn rejects_balance_adjustments_before_history_is_sealed() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(
            2,
            balance_adjustment(vec![adjustment(1, -25), adjustment(2, 25)]),
        ),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(
            LedgerProjectionError::BalanceAdjustmentWithoutSealedHistory {
                entry_id: LedgerEntryId(2),
            }
        )
    );
}

#[test]
fn rejects_balance_adjustments_related_to_unknown_entries() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(
            3,
            balance_adjustment_related(vec![adjustment(1, -25), adjustment(2, 25)], 999),
        ),
    ];

    let actual = ledger(entries);

    assert_eq!(
        actual,
        Err(LedgerStructureError::UnknownBalanceAdjustmentRelatedEntry {
            entry_id: LedgerEntryId(3),
            related_entry: LedgerEntryId(999),
        })
    );
}

#[test]
fn rejects_balance_adjustments_related_to_future_entries() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(
            3,
            balance_adjustment_related(vec![adjustment(1, -25), adjustment(2, 25)], 4),
        ),
        entry(4, expense_event(vec![amount(3, 100)], vec![amount(4, 100)])),
    ];

    let actual = ledger(entries);

    assert_eq!(
        actual,
        Err(LedgerStructureError::FutureBalanceAdjustmentRelatedEntry {
            entry_id: LedgerEntryId(3),
            related_entry: LedgerEntryId(4),
        })
    );
}

#[test]
fn rejects_balance_adjustments_related_to_unsealed_entries() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, expense_event(vec![amount(3, 100)], vec![amount(4, 100)])),
        entry(3, seal(1)),
        entry(
            4,
            balance_adjustment_related(vec![adjustment(1, -25), adjustment(2, 25)], 2),
        ),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(
            LedgerProjectionError::BalanceAdjustmentRelatedEntryNotSealed {
                entry_id: LedgerEntryId(4),
                related_entry: LedgerEntryId(2),
            }
        )
    );
}

#[test]
fn rejects_balance_adjustments_related_to_voided_entries() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, void(1)),
        entry(3, seal(2)),
        entry(
            4,
            balance_adjustment_related(vec![adjustment(1, -25), adjustment(2, 25)], 1),
        ),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(LedgerProjectionError::BalanceAdjustmentRelatedEntryVoided {
            entry_id: LedgerEntryId(4),
            related_entry: LedgerEntryId(1),
        })
    );
}

#[test]
fn rejects_balance_adjustments_unrelated_to_the_sealed_entry() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(
            3,
            balance_adjustment_related(vec![adjustment(3, -25), adjustment(4, 25)], 1),
        ),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(
            LedgerProjectionError::BalanceAdjustmentRelatedEntryUnrelated {
                entry_id: LedgerEntryId(3),
                related_entry: LedgerEntryId(1),
            }
        )
    );
}

#[test]
fn balance_adjustment_can_correct_prior_balance_adjustment() {
    // 1: expense, 2: seal(1), 3: balance adjustment that introduced an error,
    // 4: seal(3), 5: balance adjustment correcting #3.
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(
            3,
            balance_adjustment(vec![adjustment(1, -25), adjustment(2, 25)]),
        ),
        entry(4, seal(3)),
        entry(
            5,
            balance_adjustment_correcting_prior(vec![adjustment(1, 25), adjustment(2, -25)], 3),
        ),
    ];

    let state = replay(entries).expect("ledger should replay");

    // The first adjustment shifted balances; the prior-correction reverts it. Final state
    // matches the post-expense state (1: +100, 2: -100).
    assert_eq!(
        state.balances(),
        &MemberBalances::from_iter([
            (MemberId(1), Money::from_i64(100)),
            (MemberId(2), Money::from_i64(-100)),
        ])
    );
}

#[test]
fn rejects_prior_adjustment_correction_targeting_non_adjustment() {
    // SealedEntryCorrection targets entry 1 (expense): OK.
    // PriorAdjustmentCorrection targets entry 1 (expense): not OK — must target a prior
    // BalanceAdjusted entry.
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(
            3,
            balance_adjustment_correcting_prior(vec![adjustment(1, -25), adjustment(2, 25)], 1),
        ),
    ];

    let actual = replay(entries);

    assert_eq!(
        actual,
        Err(
            LedgerProjectionError::BalanceAdjustmentRelatedEntryUnsupported {
                entry_id: LedgerEntryId(3),
                related_entry: LedgerEntryId(1),
            }
        )
    );
}

#[test]
fn sealed_through_does_not_move_backwards_after_later_seal_marker() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, expense_event(vec![amount(3, 100)], vec![amount(4, 100)])),
        entry(3, settlement_transfers_recorded(vec![transfer(2, 1, 100)])),
        entry(4, seal(3)),
        entry(5, seal(1)),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(state.sealed_through(), Some(LedgerEntryId(3)));
}

#[test]
fn ledger_state_exposes_seal_marker_and_target_separately() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
    ];

    let state = replay(entries).expect("ledger should replay");

    assert_eq!(
        state.sealed_history(),
        Some(&SealedHistory {
            marker_entry_id: LedgerEntryId(2),
            through_entry_id: LedgerEntryId(1),
        })
    );
}

#[test]
fn balance_adjusted_canonicalizes_duplicate_members() {
    let adjustment = BalanceAdjusted::new(
        vec![adjustment(2, -25), adjustment(1, 100), adjustment(1, -75)],
        reason(),
        BalanceAdjustmentSource::SealedEntryCorrection(LedgerEntryId(7)),
    )
    .expect("balance adjustment should be valid");

    assert_eq!(
        adjustment.adjustments(),
        &[
            BalanceAdjustment {
                member_id: MemberId(1),
                amount: Money::from_i64(25),
            },
            BalanceAdjustment {
                member_id: MemberId(2),
                amount: Money::from_i64(-25),
            },
        ]
    );
    assert_eq!(adjustment.related_entry(), Some(LedgerEntryId(7)));
}

#[test]
fn adjustment_reason_trims_user_input() {
    let reason =
        AdjustmentReason::new("  corrected lunch amount  ").expect("reason should be valid");

    assert_eq!(reason.as_str(), "corrected lunch amount");
}

#[test]
fn rejects_imbalanced_balance_adjustments() {
    let actual = BalanceAdjusted::new(
        vec![adjustment(1, 100)],
        reason(),
        BalanceAdjustmentSource::ExternalCorrection(
            AdminCorrectionAuthority::explicit_admin_capability(),
        ),
    );

    assert_eq!(
        actual,
        Err(BalanceAdjustedError::Imbalanced {
            total: Money::from_i64(100),
        })
    );
}

#[test]
fn rejects_balance_adjustments_that_cancel_a_member_to_zero() {
    let actual = BalanceAdjusted::new(
        vec![
            adjustment(1, 100),
            adjustment(1, -100),
            adjustment(2, 50),
            adjustment(3, -50),
        ],
        reason(),
        BalanceAdjustmentSource::ExternalCorrection(
            AdminCorrectionAuthority::explicit_admin_capability(),
        ),
    );

    assert_eq!(
        actual,
        Err(BalanceAdjustedError::CancelledOutAdjustment {
            member_id: MemberId(1),
        })
    );
}

#[test]
fn projected_entry_index_exposes_participants_for_applied_entries() {
    let entries = [
        entry(
            1,
            expense_event(vec![amount(1, 200)], vec![amount(2, 100), amount(3, 100)]),
        ),
        entry(2, settlement_transfers_recorded(vec![transfer(2, 1, 100)])),
    ];

    let projected = project(entries).expect("ledger should replay");

    assert_eq!(
        projected.participants_of(LedgerEntryId(1)),
        Some(&member_set([1, 2, 3])),
    );
    assert_eq!(
        projected.participants_of(LedgerEntryId(2)),
        Some(&member_set([1, 2])),
    );
}

#[test]
fn projected_entry_index_keeps_voided_entries_with_voided_flag() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, void(1)),
    ];

    let projected = project(entries).expect("ledger should replay");

    assert!(projected.entry_index().contains(LedgerEntryId(1)));
    assert!(projected.is_entry_voided(LedgerEntryId(1)));
    assert_eq!(
        projected.participants_of(LedgerEntryId(1)),
        Some(&member_set([1, 2])),
    );
}

#[test]
fn projected_entry_index_records_marker_entries_with_empty_participants() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
    ];

    let projected = project(entries).expect("ledger should replay");

    assert!(projected.participants_of(LedgerEntryId(1)).is_some());
    let seal_info = projected
        .entry(LedgerEntryId(2))
        .expect("seal marker should be in the index");
    assert!(matches!(
        seal_info.kind,
        ProjectedEntryKind::HistorySealMarker { target } if target == LedgerEntryId(1)
    ));
    assert!(seal_info.participants.is_empty());
    assert!(seal_info.effect.is_empty());
}

#[test]
fn projected_entry_index_records_balance_adjustment_with_adjustment_members() {
    let entries = [
        entry(1, expense_event(vec![amount(1, 100)], vec![amount(2, 100)])),
        entry(2, seal(1)),
        entry(
            3,
            balance_adjustment(vec![adjustment(1, -25), adjustment(2, 25)]),
        ),
    ];

    let projected = project(entries).expect("ledger should replay");

    let adjustment_info = projected
        .entry(LedgerEntryId(3))
        .expect("balance adjustment should be in the index");
    assert!(matches!(
        adjustment_info.kind,
        ProjectedEntryKind::BalanceAdjustment
    ));
    assert_eq!(adjustment_info.participants, member_set([1, 2]));
}

#[test]
fn rejects_zero_balance_adjustments() {
    let actual = BalanceAdjusted::new(
        vec![adjustment(1, 0), adjustment(2, 0)],
        reason(),
        BalanceAdjustmentSource::ExternalCorrection(
            AdminCorrectionAuthority::explicit_admin_capability(),
        ),
    );

    assert_eq!(
        actual,
        Err(BalanceAdjustedError::ZeroAmount {
            member_id: MemberId(1),
        })
    );
}
