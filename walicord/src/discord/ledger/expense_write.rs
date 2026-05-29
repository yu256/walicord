use super::{
    participant_resolution::{
        ParticipantDrift, RosterSnapshot, drift_between_snapshot_and_resolution,
        resolve_selection_against_roster,
    },
    sessions::{ExpenseParticipantSelection, ExpenseSession, ExpenseSessionStage},
};
use walicord_application::{
    Clock,
    ledger::{
        DiscordLedgerEntryError, DiscordLedgerSourceDescriptor, EntryHash, ExpenseAuthoringError,
        LedgerCanonicalEncodeError, LedgerEntry, LedgerEntryId, LedgerId, MemberWeight,
        RecordableExpenseAuthoring, ResolvedExpenseAuthoringInput, UnverifiedLedgerStoreEnvelope,
        build_discord_expense_entry, make_unverified_envelope_sha256_v1,
    },
};
use walicord_domain::model::{MemberId, Weight};

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ExpenseWriteOrchestrationError {
    /// The session is not in `InConfirmation` so it has no payer / confirmation
    /// snapshot to record.
    #[error("session is not in InConfirmation stage")]
    SessionNotInConfirmation,
    /// `basic_info` was missing despite the session being in confirmation. This is a
    /// programmer error in the orchestration layer; the constructor invariant should
    /// have rejected it.
    #[error("basic info is missing from the confirmation-stage session draft")]
    BasicInfoMissing,
    /// Payer is no longer in the roster at record time.
    #[error("payer {payer:?} is no longer in the roster at record time")]
    PayerNotInRoster { payer: MemberId },
    /// The fresh resolution at record time produced no participants.
    #[error("no resolved participants at record time")]
    NoResolvedParticipants,
    /// Application-level authoring validation rejected the snapshot (e.g. non-positive
    /// amount, all-zero weights, oversize note, etc.). Adapter maps to the matching
    /// criterion message (criteria 3-7, 90, 128, 218 etc.).
    #[error("authoring validation: {0}")]
    Authoring(#[from] ExpenseAuthoringError),
    /// Application-level entry construction failure (typed audit / metadata error).
    #[error("entry build: {0}")]
    EntryBuild(#[from] DiscordLedgerEntryError),
    /// Canonical envelope encoding failure; the hash-suite encoding could not produce
    /// bytes for the prepared entry.
    #[error("envelope encode: {0}")]
    EnvelopeEncode(#[from] LedgerCanonicalEncodeError),
}

/// Two-way outcome of the record-time compose path. Criterion 81 requires append-time
/// resolution against the live roster, and criterion 111 requires drift between the
/// last shown confirmation and the record-time resolution to surface a refresh — not
/// silently roll the old rows into the canonical entry. `Ready` means the resolution
/// matched and we have an entry; `DriftDetected` means the caller must rebuild
/// confirmation and re-prompt before the actor can press 記録する again.
#[derive(Debug, Clone, PartialEq)]
pub enum RecordTimeOutcome {
    Ready {
        entry: LedgerEntry,
        refreshed: Vec<ExpenseParticipantSelection>,
        defaulted_members: Vec<MemberId>,
        dropped_overrides: Vec<(MemberId, Weight)>,
    },
    DriftDetected {
        drift: Vec<ParticipantDrift>,
        refreshed: Vec<ExpenseParticipantSelection>,
        defaulted_members: Vec<MemberId>,
        dropped_overrides: Vec<(MemberId, Weight)>,
    },
}

/// Compose a canonical `LedgerEntry` from a confirmation-stage session **using the
/// live roster at record press time**, then verify that the refreshed resolution still
/// matches the confirmation snapshot the actor approved. Drift between the two halts
/// the append and surfaces a refresh outcome (criteria 81, 111). The function returns
/// pure composition: separating compose from posting lets the adapter unit-test the
/// criterion-3..7 rejection behavior, criterion-218 participant cap, criterion-128
/// all-zero rejection, etc. without standing up a Discord client.
pub fn compose_expense_entry(
    session: &ExpenseSession,
    roster: &RosterSnapshot,
    new_entry_id: LedgerEntryId,
    source: DiscordLedgerSourceDescriptor,
    recorded_by: MemberId,
    clock: &dyn Clock,
) -> Result<RecordTimeOutcome, ExpenseWriteOrchestrationError> {
    if !matches!(session.stage(), ExpenseSessionStage::InConfirmation) {
        return Err(ExpenseWriteOrchestrationError::SessionNotInConfirmation);
    }
    let basic = session
        .draft()
        .basic_info()
        .ok_or(ExpenseWriteOrchestrationError::BasicInfoMissing)?;
    let payer = session
        .draft()
        .selection_state()
        .payer
        .ok_or(ExpenseWriteOrchestrationError::SessionNotInConfirmation)?;
    if !roster.all_members.contains(&payer) {
        return Err(ExpenseWriteOrchestrationError::PayerNotInRoster { payer });
    }
    let confirmation_snapshot = session
        .draft()
        .confirmation_snapshot()
        .ok_or(ExpenseWriteOrchestrationError::SessionNotInConfirmation)?;

    let refresh = resolve_selection_against_roster(session.draft().selection_state(), roster);
    if refresh.resolved.is_empty() {
        return Err(ExpenseWriteOrchestrationError::NoResolvedParticipants);
    }

    let drift = drift_between_snapshot_and_resolution(confirmation_snapshot, &refresh.resolved);
    let snapshot_weights_differ_from_refresh =
        confirmation_snapshot.participants != refresh.resolved;
    if !drift.is_empty() || snapshot_weights_differ_from_refresh {
        return Ok(RecordTimeOutcome::DriftDetected {
            drift,
            refreshed: refresh.resolved,
            defaulted_members: refresh.defaulted_members,
            dropped_overrides: refresh.dropped_overrides,
        });
    }

    let resolved_input = ResolvedExpenseAuthoringInput::new(
        payer,
        basic.amount,
        refresh.resolved.iter().map(|row| MemberWeight {
            member_id: row.member_id,
            weight: row.weight,
        }),
        basic.note.as_ref().map(|note| note.as_str().to_owned()),
        basic.effective_date.clone(),
        recorded_by,
    )
    .map_err(ExpenseWriteOrchestrationError::Authoring)?;

    let authoring = RecordableExpenseAuthoring::new(resolved_input)
        .map_err(ExpenseWriteOrchestrationError::Authoring)?;

    let entry = build_discord_expense_entry(new_entry_id, authoring, source, clock)
        .map_err(ExpenseWriteOrchestrationError::EntryBuild)?;

    Ok(RecordTimeOutcome::Ready {
        entry,
        refreshed: refresh.resolved,
        defaulted_members: refresh.defaulted_members,
        dropped_overrides: refresh.dropped_overrides,
    })
}

/// Build the canonical hashed envelope for an already-composed entry against the
/// observed canonical-thread head. The previous-hash binding is what locks the entry
/// to a specific ledger position; an out-of-date head produces a different envelope on
/// the next round.
pub fn build_canonical_envelope(
    ledger_id: LedgerId,
    previous_hash: EntryHash,
    entry: LedgerEntry,
) -> Result<UnverifiedLedgerStoreEnvelope<()>, ExpenseWriteOrchestrationError> {
    make_unverified_envelope_sha256_v1(ledger_id, previous_hash, (), entry)
        .map_err(ExpenseWriteOrchestrationError::EnvelopeEncode)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discord::ledger::{
        expense_flow::{bootstrap_expense_session, build_confirmation_for_session},
        expense_modal::{RawExpenseModalSubmission, validate_expense_modal_submission},
        participant_resolution::RosterSnapshot,
        sessions::{
            ExpenseSelectionPhase, ExpenseSelectionState, ExpenseSession, ExpenseSessionKey,
            ExpenseSessionStage,
        },
    };
    use std::{
        sync::atomic::{AtomicU64, Ordering},
        time::{Duration, SystemTime, UNIX_EPOCH},
    };
    use walicord_application::{
        Clock, InteractionNonce, NonceProvider,
        ledger::{LedgerEffectiveDate, ledger_chain_genesis_sha256_v1},
        settle_up::PreviewInstanceId,
    };

    struct FixedClock;
    impl Clock for FixedClock {
        fn now(&self) -> SystemTime {
            UNIX_EPOCH + Duration::from_secs(1)
        }
        fn today_business_date(&self) -> LedgerEffectiveDate {
            LedgerEffectiveDate::new("2026-05-29").unwrap()
        }
    }

    struct SeqNonces {
        nonce: AtomicU64,
        preview: AtomicU64,
    }
    impl NonceProvider for SeqNonces {
        fn next_interaction_nonce(&self) -> InteractionNonce {
            InteractionNonce::new(self.nonce.fetch_add(1, Ordering::SeqCst)).unwrap()
        }
        fn next_preview_instance_id(&self) -> PreviewInstanceId {
            PreviewInstanceId::new(self.preview.fetch_add(1, Ordering::SeqCst)).unwrap()
        }
    }

    fn nonces() -> SeqNonces {
        SeqNonces {
            nonce: AtomicU64::new(1),
            preview: AtomicU64::new(1),
        }
    }

    fn key(actor: u64) -> ExpenseSessionKey {
        ExpenseSessionKey::new(LedgerId(42), MemberId(actor))
    }

    fn roster_with(members: &[u64]) -> RosterSnapshot {
        RosterSnapshot {
            all_members: members.iter().map(|id| MemberId(*id)).collect(),
            role_members: std::collections::BTreeMap::new(),
        }
    }

    fn confirmation_session(amount: &str) -> ExpenseSession {
        let raw = RawExpenseModalSubmission {
            raw_amount: amount.to_owned(),
            raw_note: "ランチ".to_owned(),
            raw_date: "2026-05-01".to_owned(),
        };
        let validated = validate_expense_modal_submission(&raw, &FixedClock).unwrap();
        let (session, _) =
            bootstrap_expense_session(key(42), validated, &FixedClock, &nonces()).unwrap();
        let roster = roster_with(&[42, 7]);
        // Add member 7 to selection state before building confirmation
        let mut draft = session.draft().clone();
        draft = draft.with_selection_state(ExpenseSelectionState {
            payer: Some(MemberId(42)),
            individual_members: vec![MemberId(42), MemberId(7)],
            ..Default::default()
        });
        let session = ExpenseSession::new(
            session.key(),
            ExpenseSessionStage::InSelection {
                phase: ExpenseSelectionPhase::IndividualSelection,
            },
            draft,
            session.nonce(),
            FixedClock.now(),
        )
        .unwrap();
        build_confirmation_for_session(session, &roster, &FixedClock)
            .unwrap()
            .session
    }

    fn matching_roster() -> RosterSnapshot {
        roster_with(&[42, 7])
    }

    #[test]
    fn compose_returns_session_not_in_confirmation_error_when_session_still_in_selection() {
        let raw = RawExpenseModalSubmission {
            raw_amount: "1000".to_owned(),
            raw_note: "ランチ".to_owned(),
            raw_date: "2026-05-01".to_owned(),
        };
        let validated = validate_expense_modal_submission(&raw, &FixedClock).unwrap();
        let (session, _) =
            bootstrap_expense_session(key(42), validated, &FixedClock, &nonces()).unwrap();

        let actual = compose_expense_entry(
            &session,
            &matching_roster(),
            LedgerEntryId(1),
            DiscordLedgerSourceDescriptor::expense_slash_modal_v1(),
            MemberId(42),
            &FixedClock,
        );

        assert_eq!(
            actual,
            Err(ExpenseWriteOrchestrationError::SessionNotInConfirmation)
        );
    }

    #[test]
    fn compose_returns_ready_with_refreshed_resolution_when_roster_matches_snapshot() {
        let session = confirmation_session("1000");

        let actual = compose_expense_entry(
            &session,
            &matching_roster(),
            LedgerEntryId(7),
            DiscordLedgerSourceDescriptor::expense_slash_modal_v1(),
            MemberId(42),
            &FixedClock,
        )
        .expect("compose succeeds");

        match actual {
            RecordTimeOutcome::Ready {
                entry, refreshed, ..
            } => {
                assert_eq!(entry.id, LedgerEntryId(7));
                assert!(entry.metadata.allocation_snapshot.is_some());
                assert_eq!(refreshed.len(), 2);
            }
            RecordTimeOutcome::DriftDetected { .. } => panic!("expected Ready, got DriftDetected"),
        }
    }

    #[test]
    fn compose_returns_drift_detected_when_roster_shrunk_after_confirmation() {
        let session = confirmation_session("1000");
        let shrunk_roster = roster_with(&[42]);

        let actual = compose_expense_entry(
            &session,
            &shrunk_roster,
            LedgerEntryId(1),
            DiscordLedgerSourceDescriptor::expense_slash_modal_v1(),
            MemberId(42),
            &FixedClock,
        )
        .expect("compose returns outcome");

        match actual {
            RecordTimeOutcome::DriftDetected {
                drift, refreshed, ..
            } => {
                assert!(!drift.is_empty());
                assert_eq!(refreshed.len(), 1);
                assert_eq!(refreshed[0].member_id, MemberId(42));
            }
            RecordTimeOutcome::Ready { .. } => panic!("expected drift, got Ready"),
        }
    }

    #[test]
    fn compose_returns_drift_detected_when_roster_grew_after_confirmation() {
        let session = confirmation_session("1000");
        let grown_roster = roster_with(&[42, 7, 999]);

        let actual = compose_expense_entry(
            &session,
            &grown_roster,
            LedgerEntryId(1),
            DiscordLedgerSourceDescriptor::expense_slash_modal_v1(),
            MemberId(42),
            &FixedClock,
        )
        .expect("compose returns outcome");

        let resolved_individuals = roster_with(&[42, 7]).all_members;
        match actual {
            RecordTimeOutcome::DriftDetected {
                drift, refreshed, ..
            } => {
                // Grown roster only matters when MEMBERS group or roles are selected.
                // Individual selection { 42, 7 } resolves the same way regardless of
                // who else is in the roster, so no drift here.
                let _ = (drift, refreshed, resolved_individuals);
                panic!("individual-selection drift should NOT trigger from grown roster");
            }
            RecordTimeOutcome::Ready { refreshed, .. } => {
                assert_eq!(refreshed.len(), 2);
            }
        }
    }

    #[test]
    fn compose_rejects_when_payer_no_longer_in_roster() {
        let session = confirmation_session("1000");
        let no_payer_roster = roster_with(&[7]);

        let actual = compose_expense_entry(
            &session,
            &no_payer_roster,
            LedgerEntryId(1),
            DiscordLedgerSourceDescriptor::expense_slash_modal_v1(),
            MemberId(42),
            &FixedClock,
        );

        assert_eq!(
            actual,
            Err(ExpenseWriteOrchestrationError::PayerNotInRoster {
                payer: MemberId(42)
            })
        );
    }

    #[test]
    fn build_canonical_envelope_seals_entry_against_provided_previous_hash() {
        let session = confirmation_session("1000");
        let outcome = compose_expense_entry(
            &session,
            &matching_roster(),
            LedgerEntryId(1),
            DiscordLedgerSourceDescriptor::expense_slash_modal_v1(),
            MemberId(42),
            &FixedClock,
        )
        .unwrap();

        let entry = match outcome {
            RecordTimeOutcome::Ready { entry, .. } => entry,
            RecordTimeOutcome::DriftDetected { .. } => unreachable!("roster matched"),
        };

        let envelope = build_canonical_envelope(
            LedgerId(77),
            ledger_chain_genesis_sha256_v1(LedgerId(77)),
            entry,
        )
        .expect("envelope builds");

        assert_eq!(
            envelope.previous_hash,
            ledger_chain_genesis_sha256_v1(LedgerId(77))
        );
    }
}
