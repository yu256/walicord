use super::*;
use walicord_domain::{Money, model::MemberId};

fn amount(member_id: u64, amount: i64) -> MemberAmount {
    MemberAmount {
        member_id: MemberId(member_id),
        amount: Money::from_i64(amount),
    }
}

fn expense_recorded(paid_by: u64, owed_by: u64, value: i64) -> ExpenseRecorded {
    ExpenseRecorded::new(
        vec![amount(paid_by, value)],
        vec![amount(owed_by, value)],
        None,
    )
    .expect("expense should be valid")
}

fn expense_entry(id: u64, paid_by: u64, owed_by: u64, value: i64) -> LedgerEntry {
    LedgerEntry::expense(
        LedgerEntryId(id),
        expense_recorded(paid_by, owed_by, value),
        AllocationSnapshot::Even,
    )
    .expect("authored expense should require an explicit non-legacy allocation snapshot")
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

fn member_set<const N: usize>(ids: [u64; N]) -> std::collections::BTreeSet<MemberId> {
    ids.into_iter().map(MemberId).collect()
}

fn projected_for_correction(related_entry: u64, paid_by: u64, owed_by: u64) -> ProjectedLedger {
    let expense = expense_entry(related_entry, paid_by, owed_by, 100);
    let seal = LedgerEntry::non_expense(
        LedgerEntryId(related_entry + 1000),
        LedgerHistorySealed::new(LedgerEntryId(related_entry)),
    );
    replay_entries(vec![expense, seal]).expect("ledger should replay")
}

fn projected_with_unsealed_expense(entry_id: u64, paid_by: u64, owed_by: u64) -> ProjectedLedger {
    let entry = expense_entry(entry_id, paid_by, owed_by, 100);
    replay_entries(vec![entry]).expect("ledger should replay")
}

fn projected_with_sealed_adjustment(adjustment_entry: u64) -> ProjectedLedger {
    // 1: expense, 2: seal(1), 3: balance adjustment (entry id = adjustment_entry),
    // 4: seal through 3 (so the adjustment is sealed too).
    let expense = expense_entry(1, 1, 2, 100);
    let first_seal =
        LedgerEntry::non_expense(LedgerEntryId(2), LedgerHistorySealed::new(LedgerEntryId(1)));
    let adjustment_event = LedgerEntry::non_expense(
        LedgerEntryId(adjustment_entry),
        BalanceAdjusted::new(
            vec![adjustment(1, -25), adjustment(2, 25)],
            reason(),
            BalanceAdjustmentSource::ExternalCorrection(
                AdminCorrectionAuthority::explicit_admin_capability(),
            ),
        )
        .expect("adjustment should be valid"),
    );
    let second_seal = LedgerEntry::non_expense(
        LedgerEntryId(adjustment_entry + 1000),
        LedgerHistorySealed::new(LedgerEntryId(adjustment_entry)),
    );
    replay_entries(vec![expense, first_seal, adjustment_event, second_seal])
        .expect("ledger should replay")
}

fn projected_with_voided_expense(entry_id: u64, paid_by: u64, owed_by: u64) -> ProjectedLedger {
    let expense = expense_entry(entry_id, paid_by, owed_by, 100);
    let void = LedgerEntry::non_expense(
        LedgerEntryId(entry_id + 100),
        EntryVoided::new(LedgerEntryId(entry_id)),
    );
    let seal = LedgerEntry::non_expense(
        LedgerEntryId(entry_id + 1000),
        LedgerHistorySealed::new(LedgerEntryId(entry_id)),
    );
    replay_entries(vec![expense, void, seal]).expect("ledger should replay")
}

#[test]
fn append_ordered_entries_preserve_application_metadata() {
    let metadata = LedgerEntryMetadata {
        recorded_by: Some(MemberId(9)),
        source: Some(
            LedgerSourceCanonical::legacy_dsl("/expense 100").expect("source should parse"),
        ),
        effective_date: Some(
            LedgerEffectiveDate::new("2026-05-01").expect("effective date should parse"),
        ),
        allocation_snapshot: Some(
            AllocationSnapshot::weighted([
                MemberWeight {
                    member_id: MemberId(1),
                    weight: walicord_domain::model::Weight(2),
                },
                MemberWeight {
                    member_id: MemberId(2),
                    weight: walicord_domain::model::Weight(1),
                },
            ])
            .expect("weighted snapshot should be valid"),
        ),
    };
    let entry = LedgerEntry {
        id: LedgerEntryId(1),
        metadata: metadata.clone(),
        event: LedgerEvent::ExpenseRecorded(expense_recorded(1, 2, 100)),
    };

    let entries =
        AppendOrderedLedgerEntries::new(vec![entry]).expect("ledger should be structurally valid");

    assert_eq!(entries.as_slice()[0].metadata, metadata);
}

#[test]
fn ledger_source_digest_rejects_blank_input() {
    assert_eq!(
        LedgerSourceCanonical::legacy_dsl("\t\n"),
        Err(LedgerSourceCanonicalError::Empty),
    );
}

#[test]
fn ledger_source_canonical_marks_current_supported_source_kind() {
    let source = LedgerSourceCanonical::legacy_dsl("/expense 100").expect("source should parse");

    assert_eq!(source.kind(), LedgerSourceCanonicalKind::LegacyDsl);
    assert_eq!(source.canonical_text(), "/expense 100");
}

#[test]
fn discord_ui_source_canonical_marks_discord_kind() {
    let source =
        LedgerSourceCanonical::discord_ui("expense/slash-modal/v1").expect("source should parse");

    assert_eq!(source.kind(), LedgerSourceCanonicalKind::DiscordUi);
    assert_eq!(source.canonical_text(), "expense/slash-modal/v1");
}

#[test]
fn ledger_effective_date_accepts_iso_date() {
    let actual = LedgerEffectiveDate::new(" 2026-05-01 ");

    assert_eq!(
        actual,
        Ok(LedgerEffectiveDate::new("2026-05-01").expect("date should parse"))
    );
}

#[test]
fn ledger_effective_date_rejects_non_iso_date() {
    let actual = LedgerEffectiveDate::new("05/01/2026");

    assert_eq!(actual, Err(LedgerEffectiveDateError::InvalidFormat));
}

#[test]
fn ledger_effective_date_rejects_impossible_calendar_date() {
    let actual = LedgerEffectiveDate::new("2026-02-31");

    assert_eq!(actual, Err(LedgerEffectiveDateError::InvalidFormat));
}

#[test]
fn allocation_snapshot_weighted_preserves_zero_weight_members_for_audit() {
    let actual = AllocationSnapshot::weighted([
        MemberWeight {
            member_id: MemberId(1),
            weight: walicord_domain::model::Weight(2),
        },
        MemberWeight {
            member_id: MemberId(2),
            weight: walicord_domain::model::Weight::ZERO,
        },
        MemberWeight {
            member_id: MemberId(3),
            weight: walicord_domain::model::Weight(1),
        },
    ]);

    assert_eq!(
        actual,
        Ok(AllocationSnapshot::Weighted {
            resolved_weights: vec![
                MemberWeight {
                    member_id: MemberId(1),
                    weight: walicord_domain::model::Weight(2),
                },
                MemberWeight {
                    member_id: MemberId(2),
                    weight: walicord_domain::model::Weight::ZERO,
                },
                MemberWeight {
                    member_id: MemberId(3),
                    weight: walicord_domain::model::Weight(1),
                },
            ],
        })
    );
}

#[test]
fn append_ordered_entries_reject_allocation_snapshot_on_non_expense() {
    // A void marker carrying an allocation snapshot is incoherent — allocation snapshots
    // describe how an expense was split; they have no meaning on a void / seal /
    // settlement / adjustment entry. The hash chain would otherwise faithfully record
    // this nonsense forever, so the application validator rejects at construction time.
    let mut entry = LedgerEntry::non_expense(LedgerEntryId(1), EntryVoided::new(LedgerEntryId(2)));
    entry.metadata.allocation_snapshot = Some(AllocationSnapshot::Even);

    let actual = AppendOrderedLedgerEntries::new(vec![entry]);

    assert_eq!(
        actual.err(),
        Some(
            AppendOrderedLedgerEntriesError::AllocationSnapshotOnNonExpenseEntry {
                entry_id: LedgerEntryId(1),
            }
        )
    );
}

#[test]
fn ledger_entry_new_leaves_expense_allocation_snapshot_unset() {
    let entry = LedgerEntry::new(
        LedgerEntryId(1),
        LedgerEvent::ExpenseRecorded(expense_recorded(1, 2, 100)),
    );

    assert!(entry.metadata.allocation_snapshot.is_none());
}

#[test]
fn legacy_expense_entry_marks_allocation_snapshot_as_legacy_unknown() {
    let entry = LedgerEntry::legacy_expense_without_allocation_snapshot(
        LedgerEntryId(1),
        expense_recorded(1, 2, 100),
    );

    assert_eq!(
        entry.metadata.allocation_snapshot,
        Some(AllocationSnapshot::LegacyUnknown)
    );
}

#[test]
fn expense_constructor_rejects_legacy_unknown_snapshot() {
    let actual = LedgerEntry::expense(
        LedgerEntryId(1),
        expense_recorded(1, 2, 100),
        AllocationSnapshot::LegacyUnknown,
    );

    assert_eq!(
        actual,
        Err(LedgerEntryConstructionError::LegacyUnknownRequiresLegacyExpenseConstructor)
    );
}

#[test]
fn append_ordered_entries_reject_missing_allocation_snapshot_on_expense() {
    // Newly authored expenses must carry a structured allocation snapshot — `None` is
    // never the right answer for a fresh write. Legacy migration uses the dedicated
    // constructor so absence becomes explicit data rather than silent missing data.
    let entry = LedgerEntry::new(
        LedgerEntryId(1),
        LedgerEvent::ExpenseRecorded(expense_recorded(1, 2, 100)),
    );

    let actual = AppendOrderedLedgerEntries::new(vec![entry]);

    assert_eq!(
        actual.err(),
        Some(
            AppendOrderedLedgerEntriesError::MissingAllocationSnapshotOnExpense {
                entry_id: LedgerEntryId(1),
            }
        )
    );
}

#[test]
fn replay_entries_wraps_structure_errors() {
    let entries = vec![LedgerEntry::non_expense(
        LedgerEntryId(1),
        EntryVoided::new(LedgerEntryId(1)),
    )];

    let actual = replay_entries(entries);

    assert_eq!(
        actual,
        Err(LedgerReplayError::Structure(
            AppendOrderedLedgerEntriesError::Structure(LedgerStructureError::SelfVoid {
                entry_id: LedgerEntryId(1),
            })
        ))
    );
}

#[test]
fn replay_entries_wraps_projection_errors() {
    let entries = vec![LedgerEntry::non_expense(
        LedgerEntryId(1),
        NormalizedSettlementPlanRecorded::new(vec![walicord_domain::Transfer {
            from: MemberId(1),
            to: MemberId(2),
            amount: Money::from_i64(100),
        }])
        .expect("transfers should be structurally valid"),
    )];

    let actual = replay_entries(entries);

    assert_eq!(
        actual,
        Err(LedgerReplayError::Projection(
            LedgerProjectionError::SettlementTransferWithoutDebt {
                entry_id: LedgerEntryId(1),
                member_id: MemberId(1),
                balance: Money::ZERO,
            }
        ))
    );
}

#[test]
fn standard_sealed_entry_correction_accepts_only_participants() {
    let projected = projected_for_correction(7, 1, 2);
    let adjustments = vec![adjustment(1, -25), adjustment(2, 25)];

    let event =
        standard_sealed_entry_correction(adjustments, reason(), LedgerEntryId(7), &projected)
            .expect("standard correction should be accepted");

    assert_eq!(event.related_entry(), Some(LedgerEntryId(7)));
}

#[test]
fn standard_sealed_entry_correction_rejects_non_participant_members() {
    let projected = projected_for_correction(7, 1, 2);
    let adjustments = vec![adjustment(1, -25), adjustment(3, 25)];

    let actual =
        standard_sealed_entry_correction(adjustments, reason(), LedgerEntryId(7), &projected);

    assert_eq!(
        actual,
        Err(StandardSealedEntryCorrectionError::NonParticipantMember {
            member_id: MemberId(3),
        })
    );
}

#[test]
fn standard_sealed_entry_correction_rejects_unknown_related_entry() {
    let projected = projected_for_correction(7, 1, 2);
    let adjustments = vec![adjustment(1, -25), adjustment(2, 25)];

    let actual =
        standard_sealed_entry_correction(adjustments, reason(), LedgerEntryId(999), &projected);

    assert_eq!(
        actual,
        Err(StandardSealedEntryCorrectionError::RelatedEntryNotApplied {
            related_entry: LedgerEntryId(999),
        })
    );
}

#[test]
fn standard_sealed_entry_correction_rejects_unsealed_related_entry() {
    let projected = projected_with_unsealed_expense(7, 1, 2);
    let adjustments = vec![adjustment(1, -25), adjustment(2, 25)];

    let actual =
        standard_sealed_entry_correction(adjustments, reason(), LedgerEntryId(7), &projected);

    assert_eq!(
        actual,
        Err(StandardSealedEntryCorrectionError::RelatedEntryNotSealed {
            related_entry: LedgerEntryId(7),
        })
    );
}

#[test]
fn standard_sealed_entry_correction_rejects_voided_related_entry() {
    let projected = projected_with_voided_expense(7, 1, 2);
    let adjustments = vec![adjustment(1, -25), adjustment(2, 25)];

    let actual =
        standard_sealed_entry_correction(adjustments, reason(), LedgerEntryId(7), &projected);

    assert_eq!(
        actual,
        Err(StandardSealedEntryCorrectionError::RelatedEntryVoided {
            related_entry: LedgerEntryId(7),
        })
    );
}

#[test]
fn standard_prior_adjustment_correction_accepts_sealed_prior_adjustment() {
    let projected = projected_with_sealed_adjustment(50);
    // Prior adjustment touched members 1 and 2 with -25 / +25.
    let adjustments = vec![adjustment(1, 25), adjustment(2, -25)];

    let event =
        standard_prior_adjustment_correction(adjustments, reason(), LedgerEntryId(50), &projected)
            .expect("prior-adjustment correction should be accepted");

    assert_eq!(event.related_entry(), Some(LedgerEntryId(50)));
}

#[test]
fn standard_prior_adjustment_correction_rejects_non_adjustment_related_entry() {
    let projected = projected_for_correction(7, 1, 2);
    let adjustments = vec![adjustment(1, 25), adjustment(2, -25)];

    // Entry 7 is an expense, not a balance adjustment.
    let actual =
        standard_prior_adjustment_correction(adjustments, reason(), LedgerEntryId(7), &projected);

    assert_eq!(
        actual,
        Err(
            StandardPriorAdjustmentCorrectionError::RelatedEntryNotAdjustment {
                related_entry: LedgerEntryId(7),
            }
        )
    );
}

#[test]
fn standard_prior_adjustment_correction_rejects_unknown_related_entry() {
    let projected = projected_with_sealed_adjustment(50);
    let adjustments = vec![adjustment(1, 25), adjustment(2, -25)];

    let actual =
        standard_prior_adjustment_correction(adjustments, reason(), LedgerEntryId(999), &projected);

    assert_eq!(
        actual,
        Err(
            StandardPriorAdjustmentCorrectionError::RelatedEntryNotApplied {
                related_entry: LedgerEntryId(999),
            }
        )
    );
}

#[test]
fn standard_prior_adjustment_correction_rejects_non_participant_member() {
    let projected = projected_with_sealed_adjustment(50);
    // Adjustment members are 1 and 2; introducing 3 should be rejected.
    let adjustments = vec![adjustment(1, 25), adjustment(3, -25)];

    let actual =
        standard_prior_adjustment_correction(adjustments, reason(), LedgerEntryId(50), &projected);

    assert_eq!(
        actual,
        Err(
            StandardPriorAdjustmentCorrectionError::NonParticipantMember {
                member_id: MemberId(3),
            }
        )
    );
}

#[test]
fn seal_through_tail_targets_latest_append_entry() {
    let entries = vec![expense_entry(1, 1, 2, 100), expense_entry(2, 3, 4, 50)];
    let projected = replay_entries(entries).expect("ledger should replay");

    let seal = seal_through_tail(&projected).expect("seal should be produced");

    assert_eq!(seal.through(), LedgerEntryId(2));
}

#[test]
fn seal_through_tail_includes_balance_adjustment_marker_in_next_seal_range() {
    // 1: expense, 2: seal through 1, 3: balance adjustment.
    // Ordinary seal flow should advance the seal range to include the adjustment, not
    // stop at the latest applied expense.
    let entries = vec![
        expense_entry(1, 1, 2, 100),
        LedgerEntry::non_expense(LedgerEntryId(2), LedgerHistorySealed::new(LedgerEntryId(1))),
        LedgerEntry::non_expense(
            LedgerEntryId(3),
            BalanceAdjusted::new(
                vec![adjustment(1, -10), adjustment(2, 10)],
                reason(),
                BalanceAdjustmentSource::ExternalCorrection(
                    AdminCorrectionAuthority::explicit_admin_capability(),
                ),
            )
            .expect("adjustment should be valid"),
        ),
    ];
    let projected = replay_entries(entries).expect("ledger should replay");

    let seal = seal_through_tail(&projected).expect("seal should be produced");

    assert_eq!(seal.through(), LedgerEntryId(3));
}

#[test]
fn seal_through_tail_rejects_empty_ledger() {
    let projected = replay_entries(Vec::new()).expect("empty ledger should replay");

    let actual = seal_through_tail(&projected);

    assert_eq!(actual, Err(SealThroughTailError::EmptyLedger));
}

#[test]
fn seal_through_tail_if_advances_refuses_redundant_seal() {
    // 1: expense, 2: seal through 1. Append tail is the seal marker itself.
    let projected = projected_for_correction(7, 1, 2);

    let actual = seal_through_tail_if_advances(&projected);

    assert_eq!(actual, Err(SealThroughTailError::AlreadySealed));
}

#[test]
fn seal_through_tail_if_advances_seals_when_marker_or_adjustment_appended_after_seal() {
    // 1: expense, 2: seal(1), 3: balance adjustment.
    // The append tail (entry 3) is not the existing seal marker (entry 2), so the helper
    // advances and seals through entry 3.
    let entries = vec![
        expense_entry(1, 1, 2, 100),
        LedgerEntry::non_expense(LedgerEntryId(2), LedgerHistorySealed::new(LedgerEntryId(1))),
        LedgerEntry::non_expense(
            LedgerEntryId(3),
            BalanceAdjusted::new(
                vec![adjustment(1, -10), adjustment(2, 10)],
                reason(),
                BalanceAdjustmentSource::ExternalCorrection(
                    AdminCorrectionAuthority::explicit_admin_capability(),
                ),
            )
            .expect("adjustment should be valid"),
        ),
    ];
    let projected = replay_entries(entries).expect("ledger should replay");

    let seal = seal_through_tail_if_advances(&projected).expect("seal should advance");

    assert_eq!(seal.through(), LedgerEntryId(3));
}

#[test]
fn seal_through_tail_if_advances_rejects_empty_ledger() {
    let projected = replay_entries(Vec::new()).expect("empty ledger should replay");

    let actual = seal_through_tail_if_advances(&projected);

    assert_eq!(actual, Err(SealThroughTailError::EmptyLedger));
}

#[test]
fn seal_through_latest_unvoided_expense_or_settlement_entry_rejects_when_no_new_business_entries() {
    // 1: expense, 2: seal through 1. Latest unvoided expense/settlement is entry 1, sealed.
    let projected = projected_for_correction(7, 1, 2);

    let actual = seal_through_latest_unvoided_expense_or_settlement_entry(&projected);

    assert_eq!(actual, Err(SealThroughTailError::AlreadySealed));
}

#[test]
fn seal_through_latest_unvoided_expense_or_settlement_entry_targets_unsealed_business_entry() {
    let entries = vec![
        expense_entry(1, 1, 2, 100),
        LedgerEntry::non_expense(LedgerEntryId(2), LedgerHistorySealed::new(LedgerEntryId(1))),
        expense_entry(3, 3, 4, 50),
    ];
    let projected = replay_entries(entries).expect("ledger should replay");

    let seal = seal_through_latest_unvoided_expense_or_settlement_entry(&projected)
        .expect("seal should target the new expense");

    assert_eq!(seal.through(), LedgerEntryId(3));
}

/// Digest stub returning a fixed 32-byte hash regardless of input. Used by tests that
/// only care about chain matching, not byte-sensitivity.
struct FixedDigest(EntryHash);

impl LedgerDigest for FixedDigest {
    fn suite(&self) -> LedgerHashSuite {
        LedgerHashSuite::Sha256V1
    }
    fn digest(&self, _bytes: &[u8]) -> EntryHash {
        self.0
    }
}

/// Digest stub returning successive single-byte fills (1, 2, 3, ...). Used by chain
/// tests that need distinct hashes per envelope without depending on actual byte input.
struct CountingDigest {
    next: std::cell::Cell<u8>,
}

impl CountingDigest {
    fn new() -> Self {
        Self {
            next: std::cell::Cell::new(1),
        }
    }
}

impl LedgerDigest for CountingDigest {
    fn suite(&self) -> LedgerHashSuite {
        LedgerHashSuite::Sha256V1
    }
    fn digest(&self, _bytes: &[u8]) -> EntryHash {
        let n = self.next.get();
        self.next.set(n + 1);
        EntryHash([n; 32])
    }
}

/// Digest that copies the first 32 input bytes (right-padded with zero) into the
/// output hash. This makes the digest fully byte-sensitive: any change to the encoded
/// canonical bytes — including the embedded `previous_hash` — changes the output.
struct ByteSensitiveDigest;

impl LedgerDigest for ByteSensitiveDigest {
    fn suite(&self) -> LedgerHashSuite {
        LedgerHashSuite::Sha256V1
    }
    fn digest(&self, bytes: &[u8]) -> EntryHash {
        let mut out = [0u8; 32];
        for (i, b) in bytes.iter().enumerate().take(32) {
            out[i] = *b;
        }
        EntryHash(out)
    }
}

fn payload(version: u32) -> HashedLedgerPayload {
    HashedLedgerPayload {
        ledger_id: LedgerId(0),
        schema_version: SchemaVersion(version),
        hash_suite: LedgerHashSuite::Sha256V1,
        entry: expense_entry(1, 1, 2, 100),
    }
}

fn balanced_entry(id: u64, paid: u64, owed: u64) -> LedgerEntry {
    expense_entry(id, paid, owed, 100)
}

#[test]
fn verify_envelope_returns_verified_when_hashes_match() {
    let computed = EntryHash([1; 32]);
    let previous = EntryHash([0; 32]);
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = FixedDigest(computed);
    let unverified = UnverifiedLedgerStoreEnvelope {
        previous_hash: previous,
        entry_hash: computed,
        external_id: "external-1",
        payload: payload(1),
    };

    let verified = verify_envelope(unverified, LedgerId(0), previous, &encoder, &digest)
        .expect("envelope should verify");

    assert_eq!(verified.entry_hash(), computed);
    assert_eq!(verified.previous_hash(), previous);
    assert_eq!(*verified.external_id(), "external-1");
}

#[test]
fn verify_envelope_rejects_mismatched_previous_hash() {
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = FixedDigest(EntryHash([1; 32]));
    let unverified = UnverifiedLedgerStoreEnvelope {
        previous_hash: EntryHash([9; 32]),
        entry_hash: EntryHash([1; 32]),
        external_id: (),
        payload: payload(1),
    };

    let actual = verify_envelope(
        unverified,
        LedgerId(0),
        EntryHash([0; 32]),
        &encoder,
        &digest,
    );

    assert_eq!(
        actual,
        Err(LedgerHashChainError::PreviousHashMismatch {
            expected: EntryHash([0; 32]),
            declared: EntryHash([9; 32]),
        })
    );
}

#[test]
fn verify_envelope_rejects_recomputed_entry_hash_mismatch() {
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = FixedDigest(EntryHash([7; 32]));
    let unverified = UnverifiedLedgerStoreEnvelope {
        previous_hash: EntryHash([0; 32]),
        entry_hash: EntryHash([1; 32]),
        external_id: (),
        payload: payload(1),
    };

    let actual = verify_envelope(
        unverified,
        LedgerId(0),
        EntryHash([0; 32]),
        &encoder,
        &digest,
    );

    assert_eq!(
        actual,
        Err(LedgerHashChainError::EntryHashMismatch {
            computed: EntryHash([7; 32]),
            declared: EntryHash([1; 32]),
        })
    );
}

#[test]
fn verify_envelopes_in_append_order_chains_each_entry_hash_to_the_next_previous_hash() {
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = CountingDigest::new();
    let envelopes = vec![
        UnverifiedLedgerStoreEnvelope {
            previous_hash: EntryHash([0; 32]),
            entry_hash: EntryHash([1; 32]),
            external_id: "first",
            payload: HashedLedgerPayload {
                ledger_id: LedgerId(0),
                schema_version: SchemaVersion(1),
                hash_suite: LedgerHashSuite::Sha256V1,
                entry: balanced_entry(1, 1, 2),
            },
        },
        UnverifiedLedgerStoreEnvelope {
            previous_hash: EntryHash([1; 32]),
            entry_hash: EntryHash([2; 32]),
            external_id: "second",
            payload: HashedLedgerPayload {
                ledger_id: LedgerId(0),
                schema_version: SchemaVersion(1),
                hash_suite: LedgerHashSuite::Sha256V1,
                entry: balanced_entry(2, 3, 4),
            },
        },
    ];

    let verified = verify_envelopes_in_append_order(
        envelopes,
        LedgerId(0),
        EntryHash([0; 32]),
        &encoder,
        &digest,
    )
    .expect("chain should verify");

    assert_eq!(verified.len(), 2);
    assert_eq!(*verified[0].external_id(), "first");
    assert_eq!(*verified[1].external_id(), "second");
}

#[test]
fn verify_envelopes_in_append_order_reports_position_of_first_failing_envelope() {
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = CountingDigest::new();
    let envelopes = vec![
        UnverifiedLedgerStoreEnvelope {
            previous_hash: EntryHash([0; 32]),
            entry_hash: EntryHash([1; 32]),
            external_id: (),
            payload: HashedLedgerPayload {
                ledger_id: LedgerId(0),
                schema_version: SchemaVersion(1),
                hash_suite: LedgerHashSuite::Sha256V1,
                entry: balanced_entry(1, 1, 2),
            },
        },
        UnverifiedLedgerStoreEnvelope {
            previous_hash: EntryHash([9; 32]),
            entry_hash: EntryHash([2; 32]),
            external_id: (),
            payload: HashedLedgerPayload {
                ledger_id: LedgerId(0),
                schema_version: SchemaVersion(1),
                hash_suite: LedgerHashSuite::Sha256V1,
                entry: balanced_entry(2, 3, 4),
            },
        },
    ];

    let actual = verify_envelopes_in_append_order(
        envelopes,
        LedgerId(0),
        EntryHash([0; 32]),
        &encoder,
        &digest,
    );

    assert_eq!(
        actual,
        Err(ChainPositionError {
            position: 1,
            error: LedgerHashChainError::PreviousHashMismatch {
                expected: EntryHash([1; 32]),
                declared: EntryHash([9; 32]),
            },
        })
    );
}

#[test]
fn load_and_replay_verified_runs_the_full_pipeline() {
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = CountingDigest::new();
    let envelopes = vec![
        UnverifiedLedgerStoreEnvelope {
            previous_hash: EntryHash([0; 32]),
            entry_hash: EntryHash([1; 32]),
            external_id: "msg-1",
            payload: HashedLedgerPayload {
                ledger_id: LedgerId(0),
                schema_version: SchemaVersion(1),
                hash_suite: LedgerHashSuite::Sha256V1,
                entry: balanced_entry(1, 1, 2),
            },
        },
        UnverifiedLedgerStoreEnvelope {
            previous_hash: EntryHash([1; 32]),
            entry_hash: EntryHash([2; 32]),
            external_id: "msg-2",
            payload: HashedLedgerPayload {
                ledger_id: LedgerId(0),
                schema_version: SchemaVersion(1),
                hash_suite: LedgerHashSuite::Sha256V1,
                entry: balanced_entry(2, 2, 1),
            },
        },
    ];

    let (verified, projected) = load_and_replay_verified_with_custom_digest(
        envelopes,
        LedgerId(0),
        EntryHash([0; 32]),
        &encoder,
        &digest,
    )
    .expect("pipeline should succeed");

    assert_eq!(verified.len(), 2);
    assert_eq!(projected.state().balances(), &Default::default());
    assert_eq!(projected.state().participants(), &member_set([1, 2]));
}

#[test]
fn load_and_replay_verified_propagates_chain_failures() {
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = CountingDigest::new();
    let envelopes = vec![UnverifiedLedgerStoreEnvelope {
        previous_hash: EntryHash([0; 32]),
        entry_hash: EntryHash([42; 32]),
        external_id: (),
        payload: HashedLedgerPayload {
            ledger_id: LedgerId(0),
            schema_version: SchemaVersion(1),
            hash_suite: LedgerHashSuite::Sha256V1,
            entry: balanced_entry(1, 1, 2),
        },
    }];

    let actual = load_and_replay_verified_with_custom_digest(
        envelopes,
        LedgerId(0),
        EntryHash([0; 32]),
        &encoder,
        &digest,
    );

    assert_eq!(
        actual,
        Err(LedgerLoadError::ChainVerification(ChainPositionError {
            position: 0,
            error: LedgerHashChainError::EntryHashMismatch {
                computed: EntryHash([1; 32]),
                declared: EntryHash([42; 32]),
            },
        }))
    );
}

#[test]
fn load_and_replay_verified_propagates_structure_failures_from_inner_replay() {
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = CountingDigest::new();
    let entry = LedgerEntry::non_expense(LedgerEntryId(1), EntryVoided::new(LedgerEntryId(1)));
    let envelopes = vec![UnverifiedLedgerStoreEnvelope {
        previous_hash: EntryHash([0; 32]),
        entry_hash: EntryHash([1; 32]),
        external_id: (),
        payload: HashedLedgerPayload {
            ledger_id: LedgerId(0),
            schema_version: SchemaVersion(1),
            hash_suite: LedgerHashSuite::Sha256V1,
            entry,
        },
    }];

    let actual = load_and_replay_verified_with_custom_digest(
        envelopes,
        LedgerId(0),
        EntryHash([0; 32]),
        &encoder,
        &digest,
    );

    assert_eq!(
        actual,
        Err(LedgerLoadError::Structure(
            AppendOrderedLedgerEntriesError::Structure(LedgerStructureError::SelfVoid {
                entry_id: LedgerEntryId(1),
            })
        ))
    );
}

#[test]
fn load_and_replay_verified_sha256_v1_round_trips_real_sha256_envelopes() {
    // Production load path: digest is suite-locked to Sha256V1Digest, so callers cannot
    // accidentally swap in a different digest while still claiming the chain is valid.
    // The test constructs envelopes whose entry_hash is the actual SHA-256 of the
    // canonical bytes and confirms the suite-locked API accepts them.
    use super::Sha256V1Digest;
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = Sha256V1Digest;
    let ledger_id = LedgerId(0);
    let chain_genesis = ledger_chain_genesis_sha256_v1(ledger_id);

    let payload_one = HashedLedgerPayload {
        ledger_id,
        schema_version: SchemaVersion(1),
        hash_suite: LedgerHashSuite::Sha256V1,
        entry: balanced_entry(1, 1, 2),
    };
    let bytes_one = encoder
        .encode(chain_genesis, &payload_one)
        .expect("encoding should succeed");
    let entry_hash_one = digest.digest(&bytes_one);

    let payload_two = HashedLedgerPayload {
        ledger_id,
        schema_version: SchemaVersion(1),
        hash_suite: LedgerHashSuite::Sha256V1,
        entry: balanced_entry(2, 2, 1),
    };
    let bytes_two = encoder
        .encode(entry_hash_one, &payload_two)
        .expect("encoding should succeed");
    let entry_hash_two = digest.digest(&bytes_two);

    let envelopes = vec![
        UnverifiedLedgerStoreEnvelope {
            previous_hash: chain_genesis,
            entry_hash: entry_hash_one,
            external_id: "msg-1",
            payload: payload_one,
        },
        UnverifiedLedgerStoreEnvelope {
            previous_hash: entry_hash_one,
            entry_hash: entry_hash_two,
            external_id: "msg-2",
            payload: payload_two,
        },
    ];

    let (verified, projected) = load_and_replay_verified_sha256_v1(envelopes, ledger_id)
        .expect("suite-locked production load should succeed");

    assert_eq!(verified.len(), 2);
    assert_eq!(projected.state().participants(), &member_set([1, 2]));
}

#[test]
fn previous_hash_change_propagates_into_entry_hash() {
    // Same payload, different previous_hash -> different canonical bytes -> different
    // entry_hash. The application encoder embeds previous_hash; a byte-sensitive digest
    // therefore cannot produce the same hash for two different prefixes.
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = ByteSensitiveDigest;
    let payload_a = payload(1);

    let bytes_with_zero_prev = encoder
        .encode(EntryHash([0; 32]), &payload_a)
        .expect("encoding should succeed");
    let bytes_with_other_prev = encoder
        .encode(EntryHash([7; 32]), &payload_a)
        .expect("encoding should succeed");

    let hash_zero = digest.digest(&bytes_with_zero_prev);
    let hash_other = digest.digest(&bytes_with_other_prev);

    assert_ne!(hash_zero, hash_other);
}

#[test]
fn verify_envelope_with_byte_sensitive_digest_rejects_tampered_prefix() {
    // The envelope was signed assuming previous_hash = [0; 32]; verifying it against a
    // different expected_previous_hash should fail at the `previous_hash` check, not at
    // entry_hash recomputation, because the encoded bytes (and therefore the digest)
    // would diverge as well.
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = ByteSensitiveDigest;
    let payload = payload(1);
    let bytes = encoder
        .encode(EntryHash([0; 32]), &payload)
        .expect("encoding should succeed");
    let entry_hash = digest.digest(&bytes);
    let unverified = UnverifiedLedgerStoreEnvelope {
        previous_hash: EntryHash([0; 32]),
        entry_hash,
        external_id: (),
        payload,
    };

    let actual = verify_envelope(
        unverified,
        LedgerId(0),
        EntryHash([42; 32]),
        &encoder,
        &digest,
    );

    assert_eq!(
        actual,
        Err(LedgerHashChainError::PreviousHashMismatch {
            expected: EntryHash([42; 32]),
            declared: EntryHash([0; 32]),
        })
    );
}

#[test]
fn verify_envelope_propagates_canonical_encoder_failure() {
    // Schema version 99 is not registered with DefaultLedgerCanonicalEncoder.
    let encoder = DefaultLedgerCanonicalEncoder;
    let digest = FixedDigest(EntryHash([1; 32]));
    let unverified = UnverifiedLedgerStoreEnvelope {
        previous_hash: EntryHash([0; 32]),
        entry_hash: EntryHash([1; 32]),
        external_id: (),
        payload: payload(99),
    };

    let actual = verify_envelope(
        unverified,
        LedgerId(0),
        EntryHash([0; 32]),
        &encoder,
        &digest,
    );

    assert_eq!(
        actual,
        Err(LedgerHashChainError::Encoding(
            LedgerCanonicalEncodeError::UnsupportedSchemaVersion {
                version: SchemaVersion(99),
            }
        ))
    );
}

#[test]
fn standard_sealed_entry_correction_propagates_adjustment_errors() {
    let projected = projected_for_correction(7, 1, 2);
    let adjustments = vec![adjustment(1, 100)];

    let actual =
        standard_sealed_entry_correction(adjustments, reason(), LedgerEntryId(7), &projected);

    assert_eq!(
        actual,
        Err(StandardSealedEntryCorrectionError::Adjustment(
            BalanceAdjustedError::Imbalanced {
                total: Money::from_i64(100),
            }
        ))
    );
}

#[test]
fn admin_external_balance_correction_succeeds_when_history_is_sealed() {
    let projected = projected_for_correction(7, 1, 2);
    let adjustments = vec![adjustment(1, -25), adjustment(2, 25)];

    let event = admin_external_balance_correction(adjustments, reason(), &projected)
        .expect("admin external correction should succeed");

    assert!(matches!(
        event.source(),
        walicord_ledger::BalanceAdjustmentSource::ExternalCorrection(_)
    ));
}

#[test]
fn admin_external_balance_correction_rejects_when_no_sealed_history() {
    // A projection with only an unsealed expense — no sealed history exists yet, so
    // ExternalCorrection cannot be applied. The admin helper must surface the same
    // refusal that the projector would, but at command-construction time.
    let entries = vec![expense_entry(1, 1, 2, 100)];
    let projected = replay_entries(entries).expect("ledger should replay");
    let adjustments = vec![adjustment(1, -25), adjustment(2, 25)];

    let actual = admin_external_balance_correction(adjustments, reason(), &projected);

    assert_eq!(
        actual,
        Err(AdminExternalBalanceCorrectionError::NoSealedHistory)
    );
}

#[test]
fn admin_external_balance_correction_propagates_imbalanced_adjustments() {
    let projected = projected_for_correction(7, 1, 2);
    // Non-zero-sum adjustments — the admin path is no escape hatch from the basic
    // BalanceAdjusted invariants; the helper still propagates the underlying error.
    let adjustments = vec![adjustment(1, 100)];

    let actual = admin_external_balance_correction(adjustments, reason(), &projected);

    assert_eq!(
        actual,
        Err(AdminExternalBalanceCorrectionError::Adjustment(
            BalanceAdjustedError::Imbalanced {
                total: Money::from_i64(100),
            }
        ))
    );
}
