use super::preview_store::{
    PreviewStore, PreviewStoreError, PreviewStoreKey, PreviewStoreRecord, PreviewStoreState,
    PreviewStoreTransition,
};
use crate::{
    Clock, NonceProvider, PreviewBindingError, PreviewConfirmationBinding, PreviewInstanceId,
    SettlementPlanner,
    ledger::{
        DiscordLedgerSourceDescriptor, EntryHash, LedgerCanonicalEncodeError, LedgerEntry,
        LedgerEntryId, LedgerId, PreviewedSettlementOutcome, SettlementPreviewError,
        SettlementRecordError, UnverifiedLedgerStoreEnvelope, VerifiedLedgerSnapshot,
        make_unverified_envelope_sha256_v1, preview_settlement_from_snapshot,
        record_previewed_plan_matching,
    },
};
use std::time::SystemTime;
use walicord_domain::model::MemberId;

/// Outcome of `/review` (canonical-thread) and panel `清算確認` preview composition.
#[derive(Debug, Clone, PartialEq)]
pub enum PreviewAttemptOutcome {
    /// Balance set is settled; no transfers needed (criterion 113). The preview store
    /// is not modified — see plan note that no-op previews must not persist.
    NoTransfersNeeded,
    /// A recordable preview was composed and stored as Ready(record). The caller is
    /// expected to deliver the preview body to the actor and then `mark_delivered`
    /// the binding via `MarkDelivered` transition before `/settle` is allowed
    /// (criterion 14 / 247).
    Stored {
        preview_instance_id: PreviewInstanceId,
        record: Box<PreviewStoreRecord>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum PreviewAttemptError {
    /// The verified snapshot has no head hash — this happens only for a fresh empty
    /// ledger; the preview path should not be invoked there per criterion 113 / 118.
    #[error("snapshot has no ledger head; preview path was invoked on a fresh ledger")]
    MissingLedgerHead,
    /// Replanning the snapshot failed (validation/quantization/etc.).
    #[error("settlement preview: {0}")]
    Settlement(#[from] SettlementPreviewError),
    /// Preview binding capture rejected the proposed creation/expiry (e.g. expires_at
    /// <= created_at). This is a programmer/clock error and should be impossible if
    /// the application-owned defaults are used.
    #[error("preview binding capture: {0}")]
    BindingCapture(#[from] PreviewBindingError),
    /// Preview store rejected the replace (e.g. CommitInProgress for the same key).
    /// Caller surfaces criterion-104 rerun guidance.
    #[error("preview store: {0}")]
    Store(#[from] PreviewStoreError),
}

/// Compose a settlement preview from a verified snapshot and persist it in the preview
/// store as `Ready(record)`. The caller delivers the preview body to the actor and
/// then invokes `mark_preview_delivered` to flip the binding to `Delivered`; only then
/// does `/settle` proceed (criterion 247).
pub fn compose_and_store_preview(
    snapshot: &VerifiedLedgerSnapshot,
    ledger_id: LedgerId,
    actor_id: MemberId,
    planner: &dyn SettlementPlanner,
    clock: &dyn Clock,
    nonce_provider: &dyn NonceProvider,
    preview_store: &PreviewStore,
) -> Result<PreviewAttemptOutcome, PreviewAttemptError> {
    let outcome = preview_settlement_from_snapshot(snapshot, actor_id, planner, clock)
        .map_err(PreviewAttemptError::Settlement)?;

    let (previewed, ledger_head_hash, created_at, expires_at) = match outcome {
        PreviewedSettlementOutcome::NoTransfersNeeded => {
            return Ok(PreviewAttemptOutcome::NoTransfersNeeded);
        }
        PreviewedSettlementOutcome::RecordablePreview {
            previewed,
            ledger_head_hash,
            created_at,
            expires_at,
            ..
        } => (previewed, ledger_head_hash, created_at, expires_at),
    };

    let preview_instance_id = nonce_provider.next_preview_instance_id();
    let binding = PreviewConfirmationBinding::capture(
        preview_instance_id,
        ledger_id,
        ledger_head_hash,
        actor_id,
        created_at,
        expires_at,
        &previewed,
    )
    .map_err(PreviewAttemptError::BindingCapture)?;

    let record = PreviewStoreRecord::new(previewed, binding);
    let key = PreviewStoreKey::new(ledger_id, actor_id);
    preview_store
        .transition(
            key,
            PreviewStoreTransition::Replace(Box::new(record.clone())),
        )
        .map_err(PreviewAttemptError::Store)?;

    Ok(PreviewAttemptOutcome::Stored {
        preview_instance_id,
        record: Box::new(record),
    })
}

/// Flip the stored preview's binding from `PendingDelivery` to `Delivered` after the
/// preview body successfully reached the actor (criterion 14 / 247).
pub fn mark_preview_delivered(
    preview_store: &PreviewStore,
    key: PreviewStoreKey,
    preview_instance_id: PreviewInstanceId,
) -> Result<PreviewStoreState, PreviewStoreError> {
    preview_store
        .transition(
            key,
            PreviewStoreTransition::MarkDelivered {
                preview_instance_id,
            },
        )?
        .ok_or(PreviewStoreError::NotFound)
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum SettleAttemptError {
    /// No preview is currently stored for this actor on this ledger; the actor must
    /// run `/review` first (criterion 136).
    #[error("no preview is stored for this actor on this ledger; run /review first")]
    NoPreviewStored,
    /// The preview binding's `ledger_head_hash` does not match the live snapshot
    /// head, meaning the ledger advanced since preview (criterion 18).
    #[error("ledger head is stale (stored: {stored_head:?}, observed: {observed_head:?})")]
    StaleHead {
        stored_head: EntryHash,
        observed_head: Option<EntryHash>,
    },
    /// `now >= expires_at` — preview lapsed (criterion 160).
    #[error("preview expired (now: {now:?}, expires_at: {expires_at:?})")]
    Expired {
        now: SystemTime,
        expires_at: SystemTime,
    },
    /// Underlying preview-store rejection (commit in progress, instance mismatch, etc.).
    #[error("preview store: {0}")]
    Store(#[from] PreviewStoreError),
    /// Settlement validation / digest check / etc. failed at application layer.
    #[error("settlement record: {0}")]
    Record(#[from] SettlementRecordError),
    /// Encoding the resulting envelope failed.
    #[error("settlement envelope encode: {0}")]
    EnvelopeEncode(#[from] LedgerCanonicalEncodeError),
}

/// Result of `/settle` composition. `RecordableEntry` carries the canonical settlement
/// entry the caller must append; `NoOp` means the preview itself was a no-op so
/// nothing should be appended (criterion 113 / 114).
#[derive(Debug, Clone, PartialEq)]
pub enum SettleAttemptOutcome {
    RecordableEntry {
        entry: Box<LedgerEntry>,
        envelope: Box<UnverifiedLedgerStoreEnvelope<()>>,
    },
    NoOp,
}

/// Pure composition: validate the stored preview against the live verified snapshot
/// and produce a canonical settlement entry + envelope ready for posting. **Does not
/// mutate the preview store.** The caller is responsible for the
/// `BeginCommit` → post → `FinishCommit` / `AbortCommit` orchestration after this
/// function returns `RecordableEntry`; that split prevents an early validation failure
/// (criterion 247 / digest mismatch / source descriptor / envelope encoding) from
/// leaving a preview stranded in `CommitInProgress`.
///
/// Validation order (each step short-circuits with a typed error):
/// 1. A preview exists for this `(ledger_id, actor_id)` (criterion 136).
/// 2. The stored binding is `Delivered` (criterion 247).
/// 3. Snapshot head hash matches the binding's `ledger_head_hash` (criterion 18).
/// 4. `now < binding.expires_at` (criterion 160).
/// 5. Application authoring records the previewed plan matching the digest.
/// 6. Encode the canonical envelope against the verified head.
pub fn compose_settlement_entry_from_preview(
    snapshot: &VerifiedLedgerSnapshot,
    ledger_id: LedgerId,
    actor_id: MemberId,
    new_entry_id: LedgerEntryId,
    source: DiscordLedgerSourceDescriptor,
    preview_store: &PreviewStore,
    clock: &dyn Clock,
) -> Result<SettleAttemptOutcome, SettleAttemptError> {
    let key = PreviewStoreKey::new(ledger_id, actor_id);
    let current = preview_store
        .current(key)
        .ok_or(SettleAttemptError::NoPreviewStored)?;
    let record = match &current {
        PreviewStoreState::Ready(record) | PreviewStoreState::CommitInProgress(record) => {
            record.clone()
        }
    };
    let binding = record.binding().clone();

    if !binding.is_delivered() {
        return Err(SettleAttemptError::Record(
            SettlementRecordError::PreviewNotDelivered,
        ));
    }

    let observed_head = snapshot.current_head_hash();
    if observed_head != Some(binding.ledger_head_hash()) {
        return Err(SettleAttemptError::StaleHead {
            stored_head: binding.ledger_head_hash(),
            observed_head,
        });
    }
    let now = clock.now();
    if now >= binding.expires_at() {
        return Err(SettleAttemptError::Expired {
            now,
            expires_at: binding.expires_at(),
        });
    }

    let entry = record_previewed_plan_matching(
        new_entry_id,
        actor_id,
        record.previewed().clone(),
        binding.clone(),
        source,
        clock,
    )
    .map_err(SettleAttemptError::Record)?;

    let entry = match entry {
        Some(entry) => entry,
        None => return Ok(SettleAttemptOutcome::NoOp),
    };

    let previous_hash = binding.ledger_head_hash();
    let envelope = make_unverified_envelope_sha256_v1(ledger_id, previous_hash, (), entry.clone())
        .map_err(SettleAttemptError::EnvelopeEncode)?;

    Ok(SettleAttemptOutcome::RecordableEntry {
        entry: Box::new(entry),
        envelope: Box::new(envelope),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ledger::{LedgerEffectiveDate, ledger_chain_genesis_sha256_v1},
        settle_up::PreviewedSettlement,
    };
    use std::time::{Duration, UNIX_EPOCH};
    use walicord_domain::{
        MemberBalances, Money, Settlement, SettlementContext, SettlementRoundingError, Transfer,
    };

    struct FixedClock {
        now: SystemTime,
    }

    impl Clock for FixedClock {
        fn now(&self) -> SystemTime {
            self.now
        }
        fn today_business_date(&self) -> LedgerEffectiveDate {
            LedgerEffectiveDate::new("2026-05-29").unwrap()
        }
    }

    struct TwoMemberPlanner;
    impl SettlementPlanner for TwoMemberPlanner {
        fn plan(
            &self,
            balances: MemberBalances,
            _settle_members: &[MemberId],
            _cash_members: &[MemberId],
            _context: SettlementContext,
        ) -> Result<Settlement, SettlementRoundingError> {
            let mut iter = balances.iter();
            let (&first, &first_balance) = iter.next().expect("two-member input");
            let (&second, &second_balance) = iter.next().expect("two-member input");
            let (from, to, amount) = if first_balance < Money::ZERO {
                (first, second, -first_balance)
            } else {
                (second, first, -second_balance)
            };
            let mut new_balances = MemberBalances::default();
            new_balances.insert(from, Money::ZERO);
            new_balances.insert(to, Money::ZERO);
            Ok(Settlement {
                new_balances,
                transfers: vec![Transfer { from, to, amount }],
            })
        }
    }

    fn previewed(amount: i64) -> PreviewedSettlement {
        let mut balances = MemberBalances::default();
        balances.insert(MemberId(1), Money::from_i64(-amount));
        balances.insert(MemberId(2), Money::from_i64(amount));
        crate::SettleUpPolicy::preview(
            &TwoMemberPlanner,
            &balances,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            SettlementContext::jpy_default(),
        )
        .expect("preview should build")
    }

    fn binding_for(
        actor: MemberId,
        ledger_id: LedgerId,
        head: EntryHash,
        previewed: &PreviewedSettlement,
        expires_at: SystemTime,
    ) -> PreviewConfirmationBinding {
        PreviewConfirmationBinding::capture(
            PreviewInstanceId::new(1).unwrap(),
            ledger_id,
            head,
            actor,
            UNIX_EPOCH,
            expires_at,
            previewed,
        )
        .expect("binding should capture")
    }

    fn store_with_preview(
        head: EntryHash,
        expires_at: SystemTime,
    ) -> (PreviewStore, PreviewStoreKey) {
        let ledger_id = LedgerId(77);
        let actor = MemberId(1);
        let preview = previewed(1000);
        let binding = binding_for(actor, ledger_id, head, &preview, expires_at);
        let record = PreviewStoreRecord::new(preview, binding);
        let store = PreviewStore::new();
        let key = PreviewStoreKey::new(ledger_id, actor);
        store
            .transition(key, PreviewStoreTransition::Replace(Box::new(record)))
            .unwrap();
        (store, key)
    }

    #[test]
    fn mark_preview_delivered_flips_binding_state_for_matching_instance() {
        let head = ledger_chain_genesis_sha256_v1(LedgerId(77));
        let (store, key) = store_with_preview(head, UNIX_EPOCH + Duration::from_secs(600));

        let state = mark_preview_delivered(&store, key, PreviewInstanceId::new(1).unwrap())
            .expect("delivered transition");

        assert!(state.record().binding().is_delivered());
    }

    #[test]
    fn mark_preview_delivered_rejects_when_instance_does_not_match() {
        let head = ledger_chain_genesis_sha256_v1(LedgerId(77));
        let (store, key) = store_with_preview(head, UNIX_EPOCH + Duration::from_secs(600));

        let actual = mark_preview_delivered(&store, key, PreviewInstanceId::new(99).unwrap());

        assert!(matches!(
            actual,
            Err(PreviewStoreError::InstanceMismatch { .. })
        ));
    }

    #[test]
    fn compose_settlement_entry_returns_no_preview_stored_when_store_is_empty() {
        let store = PreviewStore::new();
        let snapshot = crate::ledger::replay_verified_snapshot::<()>(&[]).expect("empty replay");
        let clock = FixedClock {
            now: UNIX_EPOCH + Duration::from_secs(30),
        };

        let actual = compose_settlement_entry_from_preview(
            &snapshot,
            LedgerId(77),
            MemberId(1),
            LedgerEntryId(1),
            DiscordLedgerSourceDescriptor::settle_thread_v1(),
            &store,
            &clock,
        );

        assert_eq!(actual.unwrap_err(), SettleAttemptError::NoPreviewStored);
    }

    #[test]
    fn compose_settlement_entry_rejects_undelivered_preview_without_mutating_store() {
        let head = ledger_chain_genesis_sha256_v1(LedgerId(77));
        let (store, key) = store_with_preview(head, UNIX_EPOCH + Duration::from_secs(600));
        // Intentionally skip `mark_preview_delivered`.
        let snapshot = crate::ledger::replay_verified_snapshot::<()>(&[]).expect("empty replay");
        let clock = FixedClock {
            now: UNIX_EPOCH + Duration::from_secs(30),
        };

        let actual = compose_settlement_entry_from_preview(
            &snapshot,
            LedgerId(77),
            MemberId(1),
            LedgerEntryId(1),
            DiscordLedgerSourceDescriptor::settle_thread_v1(),
            &store,
            &clock,
        );

        assert!(matches!(
            actual,
            Err(SettleAttemptError::Record(
                SettlementRecordError::PreviewNotDelivered
            ))
        ));
        // Store must remain Ready (no CommitInProgress leak) because the compose path
        // is pure.
        let state = store.current(key).expect("preview still stored");
        assert!(matches!(state, PreviewStoreState::Ready(_)));
    }
}
