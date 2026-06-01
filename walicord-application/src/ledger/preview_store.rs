use crate::{PreviewConfirmationBinding, PreviewInstanceId, PreviewedSettlement, ledger::LedgerId};
use std::{collections::HashMap, sync::Mutex, time::SystemTime};
use walicord_domain::model::MemberId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PreviewStoreKey {
    ledger_id: LedgerId,
    actor_id: MemberId,
}

impl PreviewStoreKey {
    pub fn new(ledger_id: LedgerId, actor_id: MemberId) -> Self {
        Self {
            ledger_id,
            actor_id,
        }
    }

    pub fn ledger_id(self) -> LedgerId {
        self.ledger_id
    }

    pub fn actor_id(self) -> MemberId {
        self.actor_id
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PreviewStoreRecord {
    previewed: PreviewedSettlement,
    binding: PreviewConfirmationBinding,
}

impl PreviewStoreRecord {
    pub fn new(previewed: PreviewedSettlement, binding: PreviewConfirmationBinding) -> Self {
        Self { previewed, binding }
    }

    pub fn previewed(&self) -> &PreviewedSettlement {
        &self.previewed
    }

    pub fn binding(&self) -> &PreviewConfirmationBinding {
        &self.binding
    }

    pub fn preview_instance_id(&self) -> PreviewInstanceId {
        self.binding.preview_instance_id()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PreviewStoreState {
    Ready(PreviewStoreRecord),
    CommitInProgress(PreviewStoreRecord),
}

impl PreviewStoreState {
    pub fn record(&self) -> &PreviewStoreRecord {
        match self {
            Self::Ready(record) | Self::CommitInProgress(record) => record,
        }
    }

    pub fn preview_instance_id(&self) -> PreviewInstanceId {
        self.record().preview_instance_id()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PreviewStoreTransition {
    /// Insert or replace a Ready record. Same-actor replacement supersedes the previous
    /// preview instance; rejected if the existing state is `CommitInProgress`.
    Replace(Box<PreviewStoreRecord>),
    /// `Ready` -> `CommitInProgress`, gated on `preview_instance_id` matching the stored
    /// record. Used by `/settle` before consuming the preview.
    BeginCommit {
        preview_instance_id: PreviewInstanceId,
    },
    /// `CommitInProgress` -> remove (instance must match). Used after a successful append.
    FinishCommit {
        preview_instance_id: PreviewInstanceId,
    },
    /// `CommitInProgress` -> `Ready` (instance must match). Used to roll back a commit
    /// that did not actually post.
    AbortCommit {
        preview_instance_id: PreviewInstanceId,
    },
    /// Clear only when the stored record's instance matches. Failure paths use this so a
    /// stale failed attempt cannot delete a newer preview created in the meantime.
    ClearMatching {
        preview_instance_id: PreviewInstanceId,
    },
    /// Clear when `now >= binding.expires_at`. Lets callers observe-or-expire in a single
    /// transition without a sweeper.
    ClearIfExpired { now: SystemTime },
    /// Transition the stored binding from `PendingDelivery` to `Delivered` after the
    /// preview body has actually reached the actor (criterion 14 / 247 / AC 10).
    /// Rejected if the stored record's instance does not match, if the state is
    /// `CommitInProgress`, or if the binding is already delivered.
    MarkDelivered {
        preview_instance_id: PreviewInstanceId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum PreviewStoreError {
    #[error("preview not found in store")]
    NotFound,
    #[error("preview commit is in progress (preview_instance_id: {preview_instance_id:?})")]
    CommitInProgress {
        preview_instance_id: PreviewInstanceId,
    },
    #[error("preview instance mismatch (actual: {actual:?}, expected: {expected:?})")]
    InstanceMismatch {
        actual: PreviewInstanceId,
        expected: PreviewInstanceId,
    },
    #[error("preview is not in CommitInProgress state")]
    NotInProgress,
    #[error("preview binding: {0}")]
    Binding(#[from] crate::PreviewBindingError),
}

pub struct PreviewStore {
    by_key: Mutex<HashMap<PreviewStoreKey, PreviewStoreState>>,
}

impl Default for PreviewStore {
    fn default() -> Self {
        Self::new()
    }
}

impl PreviewStore {
    pub fn new() -> Self {
        Self {
            by_key: Mutex::new(HashMap::new()),
        }
    }

    pub fn current(&self, key: PreviewStoreKey) -> Option<PreviewStoreState> {
        self.by_key
            .lock()
            .expect("PreviewStore mutex poisoned")
            .get(&key)
            .cloned()
    }

    pub fn transition(
        &self,
        key: PreviewStoreKey,
        transition: PreviewStoreTransition,
    ) -> Result<Option<PreviewStoreState>, PreviewStoreError> {
        let mut guard = self.by_key.lock().expect("PreviewStore mutex poisoned");
        let current = guard.get(&key).cloned();
        let (next, returned) = compute_next_state(current, transition)?;
        match next {
            Some(state) => {
                guard.insert(key, state);
            }
            None => {
                guard.remove(&key);
            }
        }
        Ok(returned)
    }
}

pub struct PreviewCommitGuard<'a> {
    preview_store: &'a PreviewStore,
    key: PreviewStoreKey,
    preview_instance_id: PreviewInstanceId,
    finished: bool,
}

impl<'a> PreviewCommitGuard<'a> {
    pub fn begin(
        preview_store: &'a PreviewStore,
        key: PreviewStoreKey,
        preview_instance_id: PreviewInstanceId,
    ) -> Result<Self, PreviewStoreError> {
        preview_store.transition(
            key,
            PreviewStoreTransition::BeginCommit {
                preview_instance_id,
            },
        )?;
        Ok(Self {
            preview_store,
            key,
            preview_instance_id,
            finished: false,
        })
    }

    pub fn finish(mut self) -> Result<(), PreviewStoreError> {
        self.preview_store.transition(
            self.key,
            PreviewStoreTransition::FinishCommit {
                preview_instance_id: self.preview_instance_id,
            },
        )?;
        self.finished = true;
        Ok(())
    }
}

impl Drop for PreviewCommitGuard<'_> {
    fn drop(&mut self) {
        if !self.finished {
            let _ = self.preview_store.transition(
                self.key,
                PreviewStoreTransition::AbortCommit {
                    preview_instance_id: self.preview_instance_id,
                },
            );
        }
    }
}

fn compute_next_state(
    current: Option<PreviewStoreState>,
    transition: PreviewStoreTransition,
) -> Result<(Option<PreviewStoreState>, Option<PreviewStoreState>), PreviewStoreError> {
    match (current, transition) {
        (None | Some(PreviewStoreState::Ready(_)), PreviewStoreTransition::Replace(record)) => {
            let state = PreviewStoreState::Ready(*record);
            Ok((Some(state.clone()), Some(state)))
        }
        (
            Some(PreviewStoreState::CommitInProgress(existing)),
            PreviewStoreTransition::Replace(_),
        ) => Err(PreviewStoreError::CommitInProgress {
            preview_instance_id: existing.preview_instance_id(),
        }),
        (
            Some(PreviewStoreState::Ready(existing)),
            PreviewStoreTransition::BeginCommit {
                preview_instance_id,
            },
        ) => {
            ensure_instance_matches(existing.preview_instance_id(), preview_instance_id)?;
            let state = PreviewStoreState::CommitInProgress(existing);
            Ok((Some(state.clone()), Some(state)))
        }
        (
            Some(PreviewStoreState::CommitInProgress(existing)),
            PreviewStoreTransition::BeginCommit { .. },
        ) => Err(PreviewStoreError::CommitInProgress {
            preview_instance_id: existing.preview_instance_id(),
        }),
        (None, PreviewStoreTransition::BeginCommit { .. }) => Err(PreviewStoreError::NotFound),
        (
            Some(PreviewStoreState::CommitInProgress(existing)),
            PreviewStoreTransition::FinishCommit {
                preview_instance_id,
            },
        ) => {
            ensure_instance_matches(existing.preview_instance_id(), preview_instance_id)?;
            Ok((None, None))
        }
        (None | Some(PreviewStoreState::Ready(_)), PreviewStoreTransition::FinishCommit { .. }) => {
            Err(PreviewStoreError::NotInProgress)
        }
        (
            Some(PreviewStoreState::CommitInProgress(existing)),
            PreviewStoreTransition::AbortCommit {
                preview_instance_id,
            },
        ) => {
            ensure_instance_matches(existing.preview_instance_id(), preview_instance_id)?;
            let state = PreviewStoreState::Ready(existing);
            Ok((Some(state.clone()), Some(state)))
        }
        (None | Some(PreviewStoreState::Ready(_)), PreviewStoreTransition::AbortCommit { .. }) => {
            Err(PreviewStoreError::NotInProgress)
        }
        (
            Some(state),
            PreviewStoreTransition::ClearMatching {
                preview_instance_id,
            },
        ) => {
            if state.preview_instance_id() == preview_instance_id {
                Ok((None, None))
            } else {
                Ok((Some(state.clone()), Some(state)))
            }
        }
        (None, PreviewStoreTransition::ClearMatching { .. }) => Ok((None, None)),
        (Some(state), PreviewStoreTransition::ClearIfExpired { now }) => {
            if state.record().binding().expires_at() <= now {
                Ok((None, None))
            } else {
                Ok((Some(state.clone()), Some(state)))
            }
        }
        (None, PreviewStoreTransition::ClearIfExpired { .. }) => Ok((None, None)),
        (
            Some(PreviewStoreState::Ready(existing)),
            PreviewStoreTransition::MarkDelivered {
                preview_instance_id,
            },
        ) => {
            ensure_instance_matches(existing.preview_instance_id(), preview_instance_id)?;
            let PreviewStoreRecord { previewed, binding } = existing;
            let binding = binding
                .mark_delivered(preview_instance_id)
                .map_err(PreviewStoreError::Binding)?;
            let state = PreviewStoreState::Ready(PreviewStoreRecord::new(previewed, binding));
            Ok((Some(state.clone()), Some(state)))
        }
        (
            Some(PreviewStoreState::CommitInProgress(existing)),
            PreviewStoreTransition::MarkDelivered { .. },
        ) => Err(PreviewStoreError::CommitInProgress {
            preview_instance_id: existing.preview_instance_id(),
        }),
        (None, PreviewStoreTransition::MarkDelivered { .. }) => Err(PreviewStoreError::NotFound),
    }
}

fn ensure_instance_matches(
    expected: PreviewInstanceId,
    actual: PreviewInstanceId,
) -> Result<(), PreviewStoreError> {
    if expected == actual {
        Ok(())
    } else {
        Err(PreviewStoreError::InstanceMismatch { actual, expected })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        PreviewBindingError, SettleUpPolicy, SettlementPlanner,
        ledger::ledger_chain_genesis_sha256_v1,
    };
    use rstest::rstest;
    use std::time::{Duration, UNIX_EPOCH};
    use walicord_domain::{
        MemberBalances, Money, Settlement, SettlementContext, SettlementRoundingError, Transfer,
    };

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
        SettleUpPolicy::preview(
            &TwoMemberPlanner,
            &balances,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            SettlementContext::jpy_default(),
        )
        .expect("preview should build")
    }

    fn ledger_id() -> LedgerId {
        walicord_ledger::test_fixtures::ledger_id(77)
    }

    fn actor() -> MemberId {
        MemberId(1)
    }

    fn key() -> PreviewStoreKey {
        PreviewStoreKey::new(ledger_id(), actor())
    }

    fn binding(
        instance: PreviewInstanceId,
        previewed: &PreviewedSettlement,
        expires_at: SystemTime,
    ) -> PreviewConfirmationBinding {
        PreviewConfirmationBinding::capture(
            instance,
            ledger_id(),
            ledger_chain_genesis_sha256_v1(ledger_id()),
            actor(),
            UNIX_EPOCH,
            expires_at,
            previewed,
        )
        .expect("binding should capture")
    }

    fn record(instance_id: u64, amount: i64) -> PreviewStoreRecord {
        let preview = previewed(amount);
        let instance = PreviewInstanceId::new(instance_id).expect("instance id non-zero");
        let binding = binding(instance, &preview, UNIX_EPOCH + Duration::from_secs(600));
        PreviewStoreRecord::new(preview, binding)
    }

    fn instance(value: u64) -> PreviewInstanceId {
        PreviewInstanceId::new(value).expect("instance id non-zero")
    }

    #[test]
    fn replace_into_empty_key_creates_ready_state() {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        let actual = store.transition(
            key(),
            PreviewStoreTransition::Replace(Box::new(rec.clone())),
        );
        assert_eq!(actual, Ok(Some(PreviewStoreState::Ready(rec))));
    }

    #[test]
    fn same_actor_replace_supersedes_previous_instance() {
        let store = PreviewStore::new();
        let first = record(1, 1000);
        let second = record(2, 2500);
        store
            .transition(key(), PreviewStoreTransition::Replace(Box::new(first)))
            .expect("first replace succeeds");

        let actual = store.transition(
            key(),
            PreviewStoreTransition::Replace(Box::new(second.clone())),
        );

        assert_eq!(actual, Ok(Some(PreviewStoreState::Ready(second))));
    }

    #[test]
    fn replace_is_rejected_while_commit_in_progress() {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        store
            .transition(key(), PreviewStoreTransition::Replace(Box::new(rec)))
            .expect("initial replace");
        store
            .transition(
                key(),
                PreviewStoreTransition::BeginCommit {
                    preview_instance_id: instance(1),
                },
            )
            .expect("begin commit");

        let actual = store.transition(
            key(),
            PreviewStoreTransition::Replace(Box::new(record(2, 2000))),
        );

        assert_eq!(
            actual,
            Err(PreviewStoreError::CommitInProgress {
                preview_instance_id: instance(1)
            })
        );
    }

    #[rstest]
    #[case::matching_instance(1, Ok(()))]
    #[case::mismatched_instance(99, Err(PreviewStoreError::InstanceMismatch { actual: instance(99), expected: instance(1) }))]
    fn begin_commit_requires_matching_instance(
        #[case] supplied_instance: u64,
        #[case] expected: Result<(), PreviewStoreError>,
    ) {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        store
            .transition(
                key(),
                PreviewStoreTransition::Replace(Box::new(rec.clone())),
            )
            .expect("initial replace");

        let actual = store
            .transition(
                key(),
                PreviewStoreTransition::BeginCommit {
                    preview_instance_id: instance(supplied_instance),
                },
            )
            .map(|state| {
                assert!(matches!(
                    state,
                    Some(PreviewStoreState::CommitInProgress(_))
                ));
            });

        assert_eq!(actual, expected);
    }

    #[test]
    fn begin_commit_on_missing_key_returns_not_found() {
        let store = PreviewStore::new();
        let actual = store.transition(
            key(),
            PreviewStoreTransition::BeginCommit {
                preview_instance_id: instance(1),
            },
        );
        assert_eq!(actual, Err(PreviewStoreError::NotFound));
    }

    #[test]
    fn finish_commit_removes_record_when_instance_matches() {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        store
            .transition(key(), PreviewStoreTransition::Replace(Box::new(rec)))
            .expect("replace");
        store
            .transition(
                key(),
                PreviewStoreTransition::BeginCommit {
                    preview_instance_id: instance(1),
                },
            )
            .expect("begin commit");

        let actual = store.transition(
            key(),
            PreviewStoreTransition::FinishCommit {
                preview_instance_id: instance(1),
            },
        );

        assert_eq!(actual, Ok(None));
        assert_eq!(store.current(key()), None);
    }

    #[test]
    fn abort_commit_returns_record_to_ready() {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        store
            .transition(
                key(),
                PreviewStoreTransition::Replace(Box::new(rec.clone())),
            )
            .expect("replace");
        store
            .transition(
                key(),
                PreviewStoreTransition::BeginCommit {
                    preview_instance_id: instance(1),
                },
            )
            .expect("begin commit");

        let actual = store.transition(
            key(),
            PreviewStoreTransition::AbortCommit {
                preview_instance_id: instance(1),
            },
        );

        assert_eq!(actual, Ok(Some(PreviewStoreState::Ready(rec))));
    }

    #[test]
    fn dropping_commit_guard_returns_record_to_ready() {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        store
            .transition(
                key(),
                PreviewStoreTransition::Replace(Box::new(rec.clone())),
            )
            .expect("replace");

        drop(PreviewCommitGuard::begin(&store, key(), instance(1)).expect("begin commit"));

        assert_eq!(store.current(key()), Some(PreviewStoreState::Ready(rec)));
    }

    #[test]
    fn finishing_commit_guard_removes_record() {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        store
            .transition(key(), PreviewStoreTransition::Replace(Box::new(rec)))
            .expect("replace");

        PreviewCommitGuard::begin(&store, key(), instance(1))
            .expect("begin commit")
            .finish()
            .expect("finish commit");

        assert_eq!(store.current(key()), None);
    }

    #[rstest]
    #[case::matching_instance_clears(1, None)]
    #[case::stale_instance_keeps_record(99, Some(()))]
    fn clear_matching_only_removes_matching_instance(
        #[case] supplied_instance: u64,
        #[case] expected_kept: Option<()>,
    ) {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        store
            .transition(
                key(),
                PreviewStoreTransition::Replace(Box::new(rec.clone())),
            )
            .expect("replace");

        store
            .transition(
                key(),
                PreviewStoreTransition::ClearMatching {
                    preview_instance_id: instance(supplied_instance),
                },
            )
            .expect("clear matching succeeds even on stale instance");

        let actual_kept = store.current(key()).map(|_| ());
        assert_eq!(actual_kept, expected_kept);
    }

    #[rstest]
    #[case::not_yet_expired(UNIX_EPOCH + Duration::from_secs(300), Some(()))]
    #[case::exactly_at_expiry(UNIX_EPOCH + Duration::from_secs(600), None)]
    #[case::past_expiry(UNIX_EPOCH + Duration::from_secs(601), None)]
    fn clear_if_expired_uses_binding_expires_at(
        #[case] now: SystemTime,
        #[case] expected_present: Option<()>,
    ) {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        store
            .transition(key(), PreviewStoreTransition::Replace(Box::new(rec)))
            .expect("replace");

        store
            .transition(key(), PreviewStoreTransition::ClearIfExpired { now })
            .expect("clear-if-expired never errors");

        let actual_present = store.current(key()).map(|_| ());
        assert_eq!(actual_present, expected_present);
    }

    #[test]
    fn finish_commit_on_ready_state_is_rejected() {
        let store = PreviewStore::new();
        let rec = record(1, 1000);
        store
            .transition(key(), PreviewStoreTransition::Replace(Box::new(rec)))
            .expect("replace");

        let actual = store.transition(
            key(),
            PreviewStoreTransition::FinishCommit {
                preview_instance_id: instance(1),
            },
        );

        assert_eq!(actual, Err(PreviewStoreError::NotInProgress));
    }

    #[test]
    fn binding_expiry_is_validated_at_capture() {
        let preview = previewed(1000);
        let actual = PreviewConfirmationBinding::capture(
            instance(1),
            ledger_id(),
            ledger_chain_genesis_sha256_v1(ledger_id()),
            actor(),
            UNIX_EPOCH,
            UNIX_EPOCH,
            &preview,
        );
        assert_eq!(
            actual,
            Err(PreviewBindingError::NonIncreasingLifetime {
                created_at: UNIX_EPOCH,
                expires_at: UNIX_EPOCH,
            })
        );
    }
}
