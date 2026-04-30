use crate::{
    ledger::{EntryHash, LedgerId},
    ports::SettlementPlanner,
};
use std::{borrow::Cow, time::SystemTime};
use walicord_domain::{
    MemberBalances, Money, Settlement, SettlementContext, SettlementRoundingError, Transfer,
    model::MemberId, services::quantize_balances_with_preferred_members,
};
use walicord_ledger::{NormalizedSettlementPlanRecorded, NormalizedSettlementPlanRecordedError};

/// Pure-application settle-up flow: quantize the balance map, ensure the explicitly named
/// settle members are represented, delegate transfer construction to a `SettlementPlanner`
/// port, and verify the planner's output against application-level invariants before
/// returning. The planner implementation lives in adapter crates so that the application
/// layer never imports a concrete solver. Use cases stay testable with an in-memory or
/// deterministic planner without pulling in HiGHS.
///
/// The post-planner validation runs in release builds too. It is the application's
/// defense-in-depth boundary: even if a planner adapter is buggy, replaced, or compromised,
/// the application refuses to apply a result that violates membership, transfer direction,
/// balance integrity, settle-member completion, or zero-sum invariants.
pub struct SettleUpPolicy;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SettleUpError {
    /// Quantization or solver-level failure surfaced by the planner adapter.
    Rounding(SettlementRoundingError),
    /// Application-level validation rejected the planner's output. Indicates a planner
    /// bug; the result is never applied.
    PlanInvalid(SettlementPlanValidationError),
    /// The digest of the previewed value passed to `record_previewed_plan_matching`
    /// does not match the expected digest the caller supplied. Indicates that the
    /// previewed value the user confirmed is not the value being recorded — either the
    /// in-memory `PreviewedSettlement` was mutated, or a different preview is being
    /// committed under the wrong preview-id binding. Either case is unsafe to record.
    PreviewDigestMismatch {
        expected: PreviewedSettlementDigest,
        actual: PreviewedSettlementDigest,
    },
}

impl From<SettlementRoundingError> for SettleUpError {
    fn from(err: SettlementRoundingError) -> Self {
        SettleUpError::Rounding(err)
    }
}

impl From<SettleUpError> for crate::error::SettlementOptimizationError {
    fn from(err: SettleUpError) -> Self {
        match err {
            SettleUpError::Rounding(rounding) => rounding.into(),
            SettleUpError::PlanInvalid(detail) => {
                crate::error::SettlementOptimizationError::PlannerOutputInvalid { detail }
            }
            SettleUpError::PreviewDigestMismatch { expected, actual } => {
                crate::error::SettlementOptimizationError::PreviewDigestMismatch {
                    expected,
                    actual,
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SettlementPlanValidationError {
    /// A transfer references a member that did not appear in the balances passed to the
    /// planner.
    UnknownMember { member: MemberId },
    /// A transfer where `from == to`, which the planner has no business producing.
    SelfTransfer { member: MemberId },
    /// A transfer with non-positive amount.
    NonPositiveTransfer { amount: Money },
    /// `from` was not a debtor (negative balance) at the start of the batch — the planner
    /// emitted a transfer in the wrong direction.
    NotADebtor { member: MemberId, balance: Money },
    /// `to` was not a creditor (positive balance) at the start of the batch — the planner
    /// emitted a transfer in the wrong direction.
    NotACreditor { member: MemberId, balance: Money },
    /// A transfer's amount exceeds the debtor's remaining debt at the moment the
    /// transfer would be applied. Without this check, a planner that emits a chain of
    /// transfers could overshoot one party's balance and silently flip them into the
    /// opposite role, even if the initial signs and the final `new_balances` look
    /// individually plausible. The replay-side projector enforces the same invariant
    /// (`SettlementTransferOverpaysDebt`); rejecting here keeps the application's commit
    /// boundary in lockstep with the ledger projector.
    TransferOverpaysDebt {
        member: MemberId,
        remaining_debt: Money,
        transfer_amount: Money,
    },
    /// A transfer's amount exceeds the creditor's remaining credit at the moment the
    /// transfer would be applied — the mirror of `TransferOverpaysDebt`.
    TransferOverpaysCredit {
        member: MemberId,
        remaining_credit: Money,
        transfer_amount: Money,
    },
    /// The planner's `new_balances` does not match `input_balances` after applying the
    /// returned transfers; one or more members differ.
    NewBalancesMismatch {
        member: MemberId,
        expected: Money,
        actual: Money,
    },
    /// A member listed as a settle target did not reach zero in `new_balances`.
    SettleMemberNotZero { member: MemberId, balance: Money },
    /// `new_balances` does not preserve the zero-sum invariant.
    ZeroSumViolation { total: Money },
    /// A non-empty plan passed all per-transfer / balance checks but still cannot be
    /// turned into a `NormalizedSettlementPlanRecorded` ledger event. The application
    /// validator is the trust boundary the ledger event constructor sits behind, so this
    /// is the final representability gate for recordable plans: every non-empty plan that
    /// survives `validate_settlement_plan` must be directly constructible as a ledger
    /// event with no further coercion. Without this gate the application checks and
    /// event-side checks could drift, letting a plan be "accepted at the boundary but
    /// rejected when written" — exactly the inconsistency that breaks audit guarantees.
    NotRepresentableAsLedgerEvent {
        detail: NormalizedSettlementPlanRecordedError,
    },
}

/// Result of mapping a validated settlement into the ledger event boundary.
///
/// Empty plans stay explicit as `NoTransfersNeeded` so future write adapters do not need
/// to rediscover the "don't call `NormalizedSettlementPlanRecorded::new(Vec::new())`"
/// rule on their own.
#[derive(Debug, Clone, PartialEq)]
pub enum SettlementLedgerEventOutcome {
    NoTransfersNeeded,
    Record(NormalizedSettlementPlanRecorded),
}

/// Canonical settlement plan that has already passed application validation and has been
/// reduced to the exact ledger-write shape. Holding one means:
///
/// * the contained `Settlement` is canonicalized to the transfers the ledger event would
///   store;
/// * the plan's ledger-write outcome is already decided (`NoTransfersNeeded` vs
///   `Record(...)`);
/// * preview / commit / ledger-write flows can share the same validated boundary instead
///   of each recomputing representability rules.
#[derive(Debug, Clone, PartialEq)]
pub struct ValidatedSettlementPlan {
    settlement: Settlement,
    ledger_event_outcome: SettlementLedgerEventOutcome,
}

impl ValidatedSettlementPlan {
    pub fn settlement(&self) -> &Settlement {
        &self.settlement
    }

    pub fn new_balances(&self) -> &MemberBalances {
        &self.settlement.new_balances
    }

    pub fn transfers(&self) -> &[Transfer] {
        &self.settlement.transfers
    }

    pub fn ledger_event_outcome(&self) -> &SettlementLedgerEventOutcome {
        &self.ledger_event_outcome
    }

    /// Borrow the recordable ledger event, if this validated plan actually needs one.
    /// Empty plans intentionally return `None` so write callers do not have to reproduce
    /// the constructor's empty-transfer rule themselves.
    pub fn recordable_event(&self) -> Option<&NormalizedSettlementPlanRecorded> {
        match &self.ledger_event_outcome {
            SettlementLedgerEventOutcome::NoTransfersNeeded => None,
            SettlementLedgerEventOutcome::Record(event) => Some(event),
        }
    }

    /// Extract the canonicalized `Settlement` for presentation or compatibility paths.
    /// Ledger write paths should keep using [`ValidatedSettlementPlan::ledger_event_outcome`]
    /// or [`ValidatedSettlementPlan::recordable_event`] so the no-op vs recordable
    /// distinction stays explicit at the boundary.
    pub fn into_settlement(self) -> Settlement {
        self.settlement
    }
}

/// A validated settlement plan plus the inputs it was validated against. Holding one is
/// structural proof that the contained plan was produced by `SettleUpPolicy::preview` and
/// passed `validate_settlement_plan`, so a commit path that consumes a
/// `PreviewedSettlement` records the **exact transfers the user previewed** rather than
/// re-running the planner at commit time. This is the application-level guarantee that
/// replaces the "planners must be deterministic" assumption: `SettlementPlanner::plan`
/// is allowed to return alternate optima, but the value the user confirmed is the value
/// that gets recorded.
#[derive(Debug, Clone, PartialEq)]
pub struct PreviewedSettlement {
    plan: ValidatedSettlementPlan,
    input_balances: MemberBalances,
    settle_members: Vec<MemberId>,
}

impl PreviewedSettlement {
    /// View the plan without consuming the `PreviewedSettlement`. This is the only
    /// borrow-form accessor; display / presentation flows clone what they need from the
    /// returned reference. There is intentionally **no** owned-extraction method other
    /// than [`SettleUpPolicy::record_previewed_plan`]: keeping the only validated-plan-by-
    /// value path through `record_previewed_plan` means a future caller cannot
    /// accidentally bypass the typed ledger-write outcome by reaching for a convenience
    /// `into_plan` accessor.
    pub fn plan(&self) -> &Settlement {
        self.plan.settlement()
    }

    /// View the commit-time ledger-write outcome already derived for this previewed plan.
    /// Confirm flows that proceed to a ledger write should use this typed outcome rather
    /// than reconstructing `NormalizedSettlementPlanRecorded` from raw transfers.
    pub fn ledger_event_outcome(&self) -> &SettlementLedgerEventOutcome {
        self.plan.ledger_event_outcome()
    }

    /// Borrow the recordable ledger event, if this preview actually needs one. Empty
    /// settlement plans intentionally return `None` so confirm/write callers cannot forget
    /// the no-op branch.
    pub fn recordable_event(&self) -> Option<&NormalizedSettlementPlanRecorded> {
        self.plan.recordable_event()
    }

    /// 32-byte canonical digest over the previewed plan's identity-bearing inputs:
    /// canonical transfers, input balances, and settle members. Two
    /// `PreviewedSettlement` values with the same canonical identity fields produce the
    /// same digest. Different canonical bytes are expected to produce different digests
    /// under SHA-256 collision resistance.
    ///
    /// **Why this exists.** Discord interaction flows (and any UI where preview and
    /// confirm are different requests / processes / workers) cannot rely on holding the
    /// `PreviewedSettlement` in process memory across the user-confirmation gap. The
    /// expected pattern is:
    ///
    /// ```text
    /// preview()  -> let previewed = …;
    ///                let binding = PreviewConfirmationBinding::capture(
    ///                    ledger_id,
    ///                    ledger_head_hash,
    ///                    actor_id,
    ///                    expires_at,
    ///                    &previewed,
    ///                );
    ///                external_store.put(preview_id, binding);  // outside this crate
    ///                send_confirm_button_to_user(preview_id);
    ///
    /// confirm(preview_id):
    ///                let stored = external_store.get(preview_id)?;
    ///                reject_if_expired(stored.expires_at)?;
    ///                reject_if_actor_changed(stored.actor_id, current_actor_id)?;
    ///                reject_if_head_changed(stored.ledger_head_hash, current_ledger_head_hash)?;
    ///                let previewed = reconstruct_previewed_settlement(...)?; // app-specific
    ///                SettleUpPolicy::record_previewed_plan_matching(
    ///                    previewed,
    ///                    stored.preview_digest,
    ///                )?;
    /// ```
    ///
    /// The digest is stable across runs because it is computed from a versioned,
    /// self-describing canonical byte encoding, not from any session-local state.
    ///
    /// **Important boundary note:** this digest binds the previewed settlement value
    /// itself, but it does **not** prove that the confirm request is still operating on
    /// the same ledger head / actor / scope that produced the preview. A cross-request
    /// confirm flow must persist and re-check its surrounding binding separately —
    /// typically a [`PreviewConfirmationBinding`] plus its own opaque `preview_id`. In
    /// particular, digest equality does not make a preview valid against a newer ledger
    /// head after more entries have been appended.
    pub fn digest(&self) -> PreviewedSettlementDigest {
        use sha2::{Digest as _, Sha256};
        let encoded = encode_previewed_settlement_v1(self);
        PreviewedSettlementDigest(Sha256::digest(encoded).into())
    }
}

/// 32-byte stable digest of a [`PreviewedSettlement`]. Reproducible across processes,
/// restarts, and rebuilds — see [`PreviewedSettlement::digest`] for the intended use.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PreviewedSettlementDigest(pub [u8; 32]);

/// Cross-request binding that an interaction/event-store layer should persist between
/// preview and confirm. The digest binds the previewed settlement value; the remaining
/// fields bind the authority/scope/freshness context that digest matching alone does not
/// cover.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreviewConfirmationBinding {
    pub ledger_id: LedgerId,
    pub ledger_head_hash: EntryHash,
    pub actor_id: MemberId,
    pub preview_digest: PreviewedSettlementDigest,
    pub expires_at: SystemTime,
}

impl PreviewConfirmationBinding {
    pub fn capture(
        ledger_id: LedgerId,
        ledger_head_hash: EntryHash,
        actor_id: MemberId,
        expires_at: SystemTime,
        previewed: &PreviewedSettlement,
    ) -> Self {
        Self {
            ledger_id,
            ledger_head_hash,
            actor_id,
            preview_digest: previewed.digest(),
            expires_at,
        }
    }
}

const PREVIEWED_SETTLEMENT_DIGEST_DOMAIN_SEPARATOR: &[u8] = b"walicord:previewed-settlement-digest";
const PREVIEWED_SETTLEMENT_DIGEST_SCHEMA_V1: u32 = 1;

fn encode_previewed_settlement_v1(previewed: &PreviewedSettlement) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(PREVIEWED_SETTLEMENT_DIGEST_DOMAIN_SEPARATOR);
    out.extend_from_slice(&PREVIEWED_SETTLEMENT_DIGEST_SCHEMA_V1.to_be_bytes());

    // Canonicalize the inputs so iteration order in HashMap-backed structures cannot
    // shift the digest. Sorting by `MemberId` keeps the digest reproducible across
    // processes, restarts, and rebuilds.
    let mut input_pairs: Vec<(MemberId, Money)> = previewed
        .input_balances
        .iter()
        .map(|(id, money)| (*id, *money))
        .collect();
    input_pairs.sort_by_key(|(id, _)| *id);
    encode_len_u32(&mut out, input_pairs.len());
    for (id, money) in &input_pairs {
        encode_member_id(&mut out, *id);
        encode_money(&mut out, *money);
    }

    // Settle members are already in the order the caller passed them; sort here so two
    // previews that name the same set in different orders share a digest.
    let mut settle_sorted = previewed.settle_members.clone();
    settle_sorted.sort();
    encode_len_u32(&mut out, settle_sorted.len());
    for id in &settle_sorted {
        encode_member_id(&mut out, *id);
    }

    // `plan.transfers` is already in canonical (`NormalizedSettlementPlanRecorded`)
    // order, set by `preview`, so iteration order is deterministic.
    encode_len_u32(&mut out, previewed.plan.transfers().len());
    for transfer in previewed.plan.transfers() {
        encode_member_id(&mut out, transfer.from);
        encode_member_id(&mut out, transfer.to);
        encode_money(&mut out, transfer.amount);
    }

    // `plan.new_balances` is implied by transfers + input_balances (the validator
    // enforces this), so omitting it from the digest is not a loss of identity — it
    // just keeps the digest input minimal.
    out
}

fn encode_len_u32(out: &mut Vec<u8>, len: usize) {
    let len = u32::try_from(len).expect("preview digest section length exceeds u32::MAX");
    out.extend_from_slice(&len.to_be_bytes());
}

fn encode_member_id(out: &mut Vec<u8>, member_id: MemberId) {
    out.extend_from_slice(&member_id.0.to_be_bytes());
}

fn encode_money(out: &mut Vec<u8>, money: Money) {
    out.extend_from_slice(&money.as_decimal().normalize().serialize());
}

impl SettleUpPolicy {
    /// Commit-path entry point. Quantizes, plans, validates, and returns the validated
    /// settlement boundary (`Settlement` + typed ledger-write outcome). Equivalent to
    /// `preview` followed by `record_previewed_plan` against the same planner output —
    /// provided as a single-call shortcut for callers that have no intermediate "user
    /// previews then confirms" step (e.g. scripted batch settle).
    ///
    /// Interactive review→confirm flows must instead use `preview` + `record_previewed_plan`
    /// so the value the user confirmed is the value recorded, independent of planner
    /// output stability.
    pub fn settle<C>(
        planner: &dyn SettlementPlanner,
        balances: &MemberBalances,
        settle_members: &[MemberId],
        cash_members: C,
        context: SettlementContext,
    ) -> Result<ValidatedSettlementPlan, SettleUpError>
    where
        C: IntoIterator<Item = MemberId>,
    {
        let previewed = Self::preview(planner, balances, settle_members, cash_members, context)?;
        Self::record_previewed_plan(previewed)
    }

    /// Preview-path entry point. Quantizes, plans, validates, and returns a
    /// `PreviewedSettlement` that captures both the validated plan and the inputs it was
    /// validated against. The `PreviewedSettlement` should then be passed to
    /// `record_previewed_plan` at commit time so the user-confirmed plan is exactly what
    /// gets recorded — preview/commit equivalence is enforced by passing the same
    /// `PreviewedSettlement` value through, *not* by re-running the planner.
    pub fn preview<C>(
        planner: &dyn SettlementPlanner,
        balances: &MemberBalances,
        settle_members: &[MemberId],
        cash_members: C,
        context: SettlementContext,
    ) -> Result<PreviewedSettlement, SettleUpError>
    where
        C: IntoIterator<Item = MemberId>,
    {
        let balances_with_members: Cow<'_, MemberBalances> = if settle_members
            .iter()
            .all(|member| balances.contains_key(member))
        {
            Cow::Borrowed(balances)
        } else {
            let mut owned = balances.clone();
            for member in settle_members {
                owned.entry(*member).or_insert(Money::ZERO);
            }
            Cow::Owned(owned)
        };
        let prepared_balances = quantize_balances_with_preferred_members(
            balances_with_members.as_ref(),
            context,
            settle_members,
        )?;
        let cash_members: Vec<MemberId> = cash_members.into_iter().collect();
        let plan_input = prepared_balances.clone();
        let plan = planner.plan(prepared_balances, settle_members, &cash_members, context)?;

        // Canonicalize transfers to the ledger event's representation now, so the
        // `PreviewedSettlement` the user confirms already has the byte-level shape that
        // `NormalizedSettlementPlanRecorded` would store. The aggregate balance effect
        // is unchanged (canonicalization only merges duplicate (from, to) pairs and
        // sorts), so `new_balances` carries through untouched.
        let validated_plan = validate_settlement_plan(&plan_input, settle_members, &plan)
            .map_err(SettleUpError::PlanInvalid)?;

        Ok(PreviewedSettlement {
            plan: validated_plan,
            input_balances: plan_input,
            settle_members: settle_members.to_vec(),
        })
    }

    /// Commit-time finalization of a previously previewed plan. **Re-validates** the plan
    /// against the inputs captured at preview time — this re-run is cheap and pure (no
    /// planner call), and guards against any in-memory mutation of the `PreviewedSettlement`
    /// between preview and commit. Callers that have a long-lived `PreviewedSettlement`
    /// (e.g. waiting for user confirmation across requests) get a defense-in-depth check
    /// at the commit boundary.
    ///
    /// This is the replacement for "call the planner again at commit time and hope for
    /// the same answer." Real-world solvers do not give that guarantee; recording the
    /// exact validated plan the user confirmed does.
    pub fn record_previewed_plan(
        previewed: PreviewedSettlement,
    ) -> Result<ValidatedSettlementPlan, SettleUpError> {
        let PreviewedSettlement {
            plan,
            input_balances,
            settle_members,
        } = previewed;
        // Re-run the canonical validator. The transfers stored on `plan` are already in
        // ledger-event canonical form (set by `preview`), so canonicalization here is
        // idempotent. Returning the freshly validated value — rather than the previously
        // stored one — keeps the commit boundary robust even if a future change lets a
        // non-canonical `Settlement` sneak into `PreviewedSettlement`.
        validate_settlement_plan(&input_balances, &settle_members, plan.settlement())
            .map_err(SettleUpError::PlanInvalid)
    }

    /// Commit-time finalization that *also* verifies the previewed plan's digest matches
    /// an `expected_digest` the caller carried across the user-confirmation gap. Use
    /// when preview and confirm are different requests / processes / workers and the
    /// caller persisted only a binding (e.g. keyed by an opaque `preview_id`) rather
    /// than the full `PreviewedSettlement`. The flow is:
    ///
    /// * preview returns `PreviewedSettlement`; caller persists a
    ///   [`PreviewConfirmationBinding`] under `preview_id`;
    /// * on confirm, caller reloads that binding, re-checks the non-digest fields against
    ///   the current ledger/actor/expiry state, reconstructs (or otherwise obtains) the
    ///   `PreviewedSettlement`, and then;
    /// * `record_previewed_plan_matching(previewed, stored.preview_digest)` records iff
    ///   the reconstructed value's digest matches what was stored at preview time.
    ///
    /// On mismatch the function returns `PreviewDigestMismatch` and does **not** record;
    /// callers must surface this to the user (typically: ask them to re-issue the
    /// settle command) rather than fall back to recording the unmatched value.
    ///
    /// Digest matching is only the value-identity check. Confirm flows that cross
    /// requests / workers must also bind the preview to the surrounding ledger state and
    /// authority context via [`PreviewConfirmationBinding`] and reject confirmation if
    /// the current ledger head no longer matches the previewed one.
    pub fn record_previewed_plan_matching(
        previewed: PreviewedSettlement,
        expected_digest: PreviewedSettlementDigest,
    ) -> Result<ValidatedSettlementPlan, SettleUpError> {
        let actual_digest = previewed.digest();
        if actual_digest != expected_digest {
            return Err(SettleUpError::PreviewDigestMismatch {
                expected: expected_digest,
                actual: actual_digest,
            });
        }
        Self::record_previewed_plan(previewed)
    }
}

/// Validates a planner's output against the application's commit-boundary invariants and
/// returns the canonicalized, ledger-ready settlement boundary that preview / commit /
/// write paths should all share.
fn validate_settlement_plan(
    input_balances: &MemberBalances,
    settle_members: &[MemberId],
    plan: &Settlement,
) -> Result<ValidatedSettlementPlan, SettlementPlanValidationError> {
    // Walk the transfers in order, mirroring the ledger projector's per-transfer checks so
    // a plan accepted here will also be accepted by `LedgerProjector` on replay. Each
    // transfer's debtor/creditor sign and overpayment is checked against the *current*
    // remaining balance, not the initial one — otherwise a chained plan could flip a
    // creditor into a debtor and still satisfy a final-state-only check.
    let mut remaining = input_balances.clone();
    for transfer in &plan.transfers {
        if transfer.from == transfer.to {
            return Err(SettlementPlanValidationError::SelfTransfer {
                member: transfer.from,
            });
        }
        if transfer.amount <= Money::ZERO {
            return Err(SettlementPlanValidationError::NonPositiveTransfer {
                amount: transfer.amount,
            });
        }
        let Some(&from_balance) = remaining.get(&transfer.from) else {
            return Err(SettlementPlanValidationError::UnknownMember {
                member: transfer.from,
            });
        };
        let Some(&to_balance) = remaining.get(&transfer.to) else {
            return Err(SettlementPlanValidationError::UnknownMember {
                member: transfer.to,
            });
        };
        if from_balance >= Money::ZERO {
            return Err(SettlementPlanValidationError::NotADebtor {
                member: transfer.from,
                balance: from_balance,
            });
        }
        if to_balance <= Money::ZERO {
            return Err(SettlementPlanValidationError::NotACreditor {
                member: transfer.to,
                balance: to_balance,
            });
        }
        let remaining_debt = -from_balance;
        if transfer.amount > remaining_debt {
            return Err(SettlementPlanValidationError::TransferOverpaysDebt {
                member: transfer.from,
                remaining_debt,
                transfer_amount: transfer.amount,
            });
        }
        if transfer.amount > to_balance {
            return Err(SettlementPlanValidationError::TransferOverpaysCredit {
                member: transfer.to,
                remaining_credit: to_balance,
                transfer_amount: transfer.amount,
            });
        }

        *remaining.entry(transfer.from).or_insert(Money::ZERO) += transfer.amount;
        *remaining.entry(transfer.to).or_insert(Money::ZERO) -= transfer.amount;
    }

    let expected_balances = remaining;
    for (member, expected) in &expected_balances {
        let actual = plan
            .new_balances
            .get(member)
            .copied()
            .unwrap_or(Money::ZERO);
        if actual != *expected {
            return Err(SettlementPlanValidationError::NewBalancesMismatch {
                member: *member,
                expected: *expected,
                actual,
            });
        }
    }
    for (member, actual) in &plan.new_balances {
        if !expected_balances.contains_key(member) {
            return Err(SettlementPlanValidationError::NewBalancesMismatch {
                member: *member,
                expected: Money::ZERO,
                actual: *actual,
            });
        }
    }

    for member in settle_members {
        let balance = plan
            .new_balances
            .get(member)
            .copied()
            .unwrap_or(Money::ZERO);
        if balance != Money::ZERO {
            return Err(SettlementPlanValidationError::SettleMemberNotZero {
                member: *member,
                balance,
            });
        }
    }

    let total: Money = plan.new_balances.values().copied().sum();
    if total != Money::ZERO {
        return Err(SettlementPlanValidationError::ZeroSumViolation { total });
    }

    // Final gate: the plan must be directly constructible as a ledger event, and the
    // returned settlement stores the **canonical form** of the transfers that the ledger
    // event would write — duplicates merged, ordered by `(from, to)`. Returning the
    // typed ledger-event outcome here is what makes preview / commit / ledger replay
    // agree byte-for-byte while also making the empty-plan branch explicit to callers.
    let (canonical_transfers, ledger_event_outcome) = if plan.transfers.is_empty() {
        (Vec::new(), SettlementLedgerEventOutcome::NoTransfersNeeded)
    } else {
        let normalized =
            NormalizedSettlementPlanRecorded::new(plan.transfers.clone()).map_err(|detail| {
                SettlementPlanValidationError::NotRepresentableAsLedgerEvent { detail }
            })?;
        (
            normalized.transfers().to_vec(),
            SettlementLedgerEventOutcome::Record(normalized),
        )
    };

    Ok(ValidatedSettlementPlan {
        settlement: Settlement {
            new_balances: plan.new_balances.clone(),
            transfers: canonical_transfers,
        },
        ledger_event_outcome,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use walicord_domain::Transfer;

    fn balances<const N: usize>(entries: [(u64, i64); N]) -> MemberBalances {
        entries
            .into_iter()
            .map(|(id, amount)| (MemberId(id), Money::from_i64(amount)))
            .collect()
    }

    fn transfer(from: u64, to: u64, amount: i64) -> Transfer {
        Transfer {
            from: MemberId(from),
            to: MemberId(to),
            amount: Money::from_i64(amount),
        }
    }

    fn recorded_outcome(transfers: Vec<Transfer>) -> SettlementLedgerEventOutcome {
        SettlementLedgerEventOutcome::Record(
            NormalizedSettlementPlanRecorded::new(transfers)
                .expect("transfers should be representable as a ledger event"),
        )
    }

    #[test]
    fn validates_a_correct_plan() {
        let input = balances([(1, 100), (2, -100)]);
        let plan = Settlement {
            new_balances: balances([(1, 0), (2, 0)]),
            transfers: vec![transfer(2, 1, 100)],
        };

        assert!(validate_settlement_plan(&input, &[MemberId(1), MemberId(2)], &plan).is_ok());
    }

    #[test]
    fn rejects_unknown_member_in_transfer() {
        let input = balances([(1, 100), (2, -100)]);
        let plan = Settlement {
            new_balances: balances([(1, 100), (2, -100)]),
            transfers: vec![transfer(3, 1, 50)],
        };

        assert_eq!(
            validate_settlement_plan(&input, &[], &plan),
            Err(SettlementPlanValidationError::UnknownMember {
                member: MemberId(3),
            })
        );
    }

    #[test]
    fn rejects_wrong_direction() {
        let input = balances([(1, 100), (2, -100)]);
        // 1 is creditor (+100), but the planner says 1 pays 2.
        let plan = Settlement {
            new_balances: balances([(1, 50), (2, -50)]),
            transfers: vec![transfer(1, 2, 50)],
        };

        assert_eq!(
            validate_settlement_plan(&input, &[], &plan),
            Err(SettlementPlanValidationError::NotADebtor {
                member: MemberId(1),
                balance: Money::from_i64(100),
            })
        );
    }

    #[test]
    fn rejects_inconsistent_new_balances() {
        let input = balances([(1, 100), (2, -100)]);
        // Transfer says 2→1 of 100 but new_balances claims 2 still owes 50.
        let plan = Settlement {
            new_balances: balances([(1, 0), (2, -50)]),
            transfers: vec![transfer(2, 1, 100)],
        };

        let actual = validate_settlement_plan(&input, &[], &plan);

        assert!(matches!(
            actual,
            Err(SettlementPlanValidationError::NewBalancesMismatch { .. })
        ));
    }

    #[test]
    fn rejects_settle_member_not_zero() {
        // Two creditors and two debtors, but only the first creditor/debtor pair is
        // resolved. Member 2 is named as a settle target but the plan leaves them at
        // -100, so the validator must reject with SettleMemberNotZero rather than passing
        // the rest of the checks (zero-sum, balances arithmetic, per-transfer signs/limits
        // are all individually fine here).
        let input = balances([(1, 100), (2, -100), (3, 100), (4, -100)]);
        let plan = Settlement {
            new_balances: balances([(1, 0), (2, -100), (3, 100), (4, 0)]),
            transfers: vec![transfer(4, 1, 100)],
        };

        let actual =
            validate_settlement_plan(&input, &[MemberId(1), MemberId(2)], &plan).unwrap_err();

        assert_eq!(
            actual,
            SettlementPlanValidationError::SettleMemberNotZero {
                member: MemberId(2),
                balance: Money::from_i64(-100),
            }
        );
    }

    #[test]
    fn rejects_zero_sum_violation() {
        let input = balances([(1, 100), (2, -100)]);
        let plan = Settlement {
            new_balances: balances([(1, 50), (2, -100)]),
            transfers: vec![transfer(2, 1, 50)],
        };

        let actual = validate_settlement_plan(&input, &[], &plan);

        assert!(matches!(
            actual,
            Err(SettlementPlanValidationError::NewBalancesMismatch { .. })
                | Err(SettlementPlanValidationError::ZeroSumViolation { .. })
        ));
    }

    #[test]
    fn rejects_transfer_that_overpays_creditor_through_chain() {
        // The motivating case from the spec review: A is the only debtor, B and C are
        // creditors of equal size. A bug where the planner emits `A -> B 100` would push
        // B from +50 to -50 — flipping a creditor into a debtor — yet still satisfy the
        // initial-sign-only check, the final-state-only check, and the settle-target zero
        // check. Per-transfer overpayment validation must reject this.
        let input = balances([(1, -100), (2, 50), (3, 50)]);
        let plan = Settlement {
            new_balances: balances([(1, 0), (2, -50), (3, 50)]),
            transfers: vec![transfer(1, 2, 100)],
        };

        let actual = validate_settlement_plan(&input, &[MemberId(1)], &plan);

        assert_eq!(
            actual,
            Err(SettlementPlanValidationError::TransferOverpaysCredit {
                member: MemberId(2),
                remaining_credit: Money::from_i64(50),
                transfer_amount: Money::from_i64(100),
            })
        );
    }

    #[test]
    fn rejects_transfer_that_overpays_debtor_through_chain() {
        // Mirror case: a chain of transfers leaves the same debtor still positive on debt
        // initially but the second transfer would consume more than the remaining debt,
        // pushing them into creditor territory.
        let input = balances([(1, -100), (2, 50), (3, 50)]);
        let plan = Settlement {
            new_balances: balances([(1, 50), (2, 0), (3, -50)]),
            transfers: vec![transfer(1, 2, 50), transfer(1, 3, 100)],
        };

        let actual = validate_settlement_plan(&input, &[], &plan);

        assert_eq!(
            actual,
            Err(SettlementPlanValidationError::TransferOverpaysDebt {
                member: MemberId(1),
                remaining_debt: Money::from_i64(50),
                transfer_amount: Money::from_i64(100),
            })
        );
    }

    /// Buggy planner that emits an overpaying transfer chain. Used to confirm that the
    /// preview / settle entry points reject planner output that would also be rejected by
    /// the ledger projector at replay time.
    struct OverpayingPlanner;

    impl SettlementPlanner for OverpayingPlanner {
        fn plan(
            &self,
            _balances: MemberBalances,
            _settle_members: &[MemberId],
            _cash_members: &[MemberId],
            _context: SettlementContext,
        ) -> Result<Settlement, walicord_domain::SettlementRoundingError> {
            // Input balances will be A:-100, B:+50, C:+50; one transfer A→B 100 overshoots
            // creditor B by 50. new_balances is internally consistent with the transfer
            // (A:0, B:-50, C:+50) and zero-sum; only per-transfer overpayment catches it.
            Ok(Settlement {
                new_balances: balances([(1, 0), (2, -50), (3, 50)]),
                transfers: vec![transfer(1, 2, 100)],
            })
        }
    }

    #[test]
    fn preview_rejects_buggy_planner_output_via_validation() {
        let input = balances([(1, -100), (2, 50), (3, 50)]);
        let context = SettlementContext::jpy_default();

        let err = SettleUpPolicy::preview(
            &OverpayingPlanner,
            &input,
            &[MemberId(1)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect_err("preview must reject overpaying planner output");

        assert!(matches!(
            err,
            SettleUpError::PlanInvalid(SettlementPlanValidationError::TransferOverpaysCredit {
                member,
                ..
            })
            if member == MemberId(2)
        ));
    }

    #[test]
    fn settle_and_preview_share_validation_surface() {
        let input = balances([(1, -100), (2, 50), (3, 50)]);
        let context = SettlementContext::jpy_default();

        let preview_err = SettleUpPolicy::preview(
            &OverpayingPlanner,
            &input,
            &[MemberId(1)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .unwrap_err();
        let settle_err = SettleUpPolicy::settle(
            &OverpayingPlanner,
            &input,
            &[MemberId(1)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .unwrap_err();

        assert_eq!(preview_err, settle_err);
    }

    #[test]
    fn accepts_chain_that_exactly_consumes_balances() {
        // A debtor of -100 paying two creditors of +50 each through two separate transfers
        // is legitimate and must remain accepted by the per-transfer validator.
        let input = balances([(1, -100), (2, 50), (3, 50)]);
        let plan = Settlement {
            new_balances: balances([(1, 0), (2, 0), (3, 0)]),
            transfers: vec![transfer(1, 2, 50), transfer(1, 3, 50)],
        };

        assert!(
            validate_settlement_plan(&input, &[MemberId(1), MemberId(2), MemberId(3)], &plan)
                .is_ok()
        );
    }

    /// Planner that returns one of two valid plans depending on a flip flag, simulating
    /// alternate-optimum drift between preview and commit calls. Real-world solvers
    /// (HiGHS with `time_limit_seconds` + `accept_feasible_on_limit`) are allowed to do
    /// exactly this, so the application boundary must not depend on planner determinism.
    struct AlternateOptimumPlanner {
        call_count: std::sync::atomic::AtomicU32,
    }

    impl AlternateOptimumPlanner {
        fn new() -> Self {
            Self {
                call_count: std::sync::atomic::AtomicU32::new(0),
            }
        }
    }

    impl SettlementPlanner for AlternateOptimumPlanner {
        fn plan(
            &self,
            _balances: MemberBalances,
            _settle_members: &[MemberId],
            _cash_members: &[MemberId],
            _context: SettlementContext,
        ) -> Result<Settlement, walicord_domain::SettlementRoundingError> {
            let n = self
                .call_count
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            // Two valid resolutions of (1:-100, 2:-100, 3:+100, 4:+100):
            //   call 0 -> 1->3 and 2->4
            //   call 1 -> 1->4 and 2->3
            // Both are 2-transfer plans of equal aggregate value; the canonical
            // (from, to)-sorted forms genuinely differ between them, so this is a real
            // alternate-optimum drift case rather than reordering of identical edges.
            let plan = if n.is_multiple_of(2) {
                Settlement {
                    new_balances: balances([(1, 0), (2, 0), (3, 0), (4, 0)]),
                    transfers: vec![transfer(1, 3, 100), transfer(2, 4, 100)],
                }
            } else {
                Settlement {
                    new_balances: balances([(1, 0), (2, 0), (3, 0), (4, 0)]),
                    transfers: vec![transfer(1, 4, 100), transfer(2, 3, 100)],
                }
            };
            Ok(plan)
        }
    }

    #[test]
    fn record_previewed_plan_records_exact_previewed_value_without_rerunning_planner() {
        // The exact failure mode this guards against: preview produces plan A, the user
        // confirms, and a naive commit path calls planner again — getting plan B, which
        // is also valid but differs from what the user saw. `record_previewed_plan`
        // consumes the captured plan instead of re-querying.
        let input = balances([(1, -100), (2, -100), (3, 100), (4, 100)]);
        let context = SettlementContext::jpy_default();
        let planner = AlternateOptimumPlanner::new();

        let previewed = SettleUpPolicy::preview(
            &planner,
            &input,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");

        let previewed_transfers = previewed.plan().transfers.clone();

        // Sanity: the planner does drift on a fresh call. If this assertion ever flips,
        // the test fixture (AlternateOptimumPlanner) has lost its drift behavior and the
        // record_previewed_plan property below is no longer being meaningfully tested.
        let drifted = planner
            .plan(input.clone(), &[MemberId(1), MemberId(2)], &[], context)
            .expect("planner re-call should succeed");
        assert_ne!(previewed_transfers, drifted.transfers);

        let recorded =
            SettleUpPolicy::record_previewed_plan(previewed).expect("record should succeed");
        assert_eq!(recorded.transfers(), previewed_transfers.as_slice());
    }

    #[test]
    fn previewed_settlement_digest_is_stable_for_equal_previews() {
        let input = balances([(1, 100), (2, -100)]);
        let context = SettlementContext::jpy_default();

        let one = SettleUpPolicy::preview(
            &CreditorReceivesAllPlanner,
            &input,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");
        let two = SettleUpPolicy::preview(
            &CreditorReceivesAllPlanner,
            &input,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");

        assert_eq!(
            one.digest(),
            two.digest(),
            "digest must be stable for equal previewed values"
        );
    }

    #[test]
    fn previewed_settlement_digest_uses_versioned_self_describing_encoding() {
        let input = balances([(1, 100), (2, -100)]);
        let context = SettlementContext::jpy_default();
        let previewed = SettleUpPolicy::preview(
            &CreditorReceivesAllPlanner,
            &input,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");

        let encoded = encode_previewed_settlement_v1(&previewed);
        let domain_len = PREVIEWED_SETTLEMENT_DIGEST_DOMAIN_SEPARATOR.len();
        let schema_start = domain_len;
        let input_count_start = schema_start + std::mem::size_of::<u32>();
        let schema =
            u32::from_be_bytes(encoded[schema_start..input_count_start].try_into().unwrap());
        let input_count = u32::from_be_bytes(
            encoded[input_count_start..input_count_start + std::mem::size_of::<u32>()]
                .try_into()
                .unwrap(),
        );

        assert_eq!(
            &encoded[..domain_len],
            PREVIEWED_SETTLEMENT_DIGEST_DOMAIN_SEPARATOR
        );
        assert_eq!(schema, PREVIEWED_SETTLEMENT_DIGEST_SCHEMA_V1);
        assert_eq!(input_count, 2);
    }

    #[test]
    fn previewed_settlement_digest_diverges_when_transfers_diverge() {
        // The same `input_balances` and `settle_members` but different planner output
        // must produce a different digest. Regression: if the digest collapsed inputs
        // into a constant, alternate-optimum drift would silently pass digest matching.
        let input = balances([(1, -100), (2, -100), (3, 100), (4, 100)]);
        let context = SettlementContext::jpy_default();

        let alt_planner = AlternateOptimumPlanner::new();
        let first = SettleUpPolicy::preview(
            &alt_planner,
            &input,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");
        let second = SettleUpPolicy::preview(
            &alt_planner,
            &input,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");

        // The fixture intentionally returns a different transfer ordering on the second
        // call, so the digests must differ.
        assert_ne!(first.plan().transfers, second.plan().transfers);
        assert_ne!(first.digest(), second.digest());
    }

    #[test]
    fn record_previewed_plan_matching_records_when_digest_matches() {
        let input = balances([(1, 100), (2, -100)]);
        let context = SettlementContext::jpy_default();
        let previewed = SettleUpPolicy::preview(
            &CreditorReceivesAllPlanner,
            &input,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");
        let stored_digest = previewed.digest();

        let recorded = SettleUpPolicy::record_previewed_plan_matching(previewed, stored_digest)
            .expect("matching digest should record");
        assert_eq!(recorded.transfers(), &[transfer(2, 1, 100)]);
    }

    #[test]
    fn record_previewed_plan_matching_rejects_on_digest_mismatch() {
        // Different inputs produce different digests; passing one digest while
        // recording a different previewed value must be rejected so a future Discord
        // confirm flow cannot silently commit a value the user did not see.
        let context = SettlementContext::jpy_default();
        let input_a = balances([(1, 100), (2, -100)]);
        let input_b = balances([(1, 200), (2, -200)]);
        let previewed_a = SettleUpPolicy::preview(
            &TwoMemberSquaringPlanner,
            &input_a,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview a should succeed");
        let previewed_b = SettleUpPolicy::preview(
            &TwoMemberSquaringPlanner,
            &input_b,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview b should succeed");

        let digest_a = previewed_a.digest();
        let actual = SettleUpPolicy::record_previewed_plan_matching(previewed_b, digest_a);

        assert!(matches!(
            actual,
            Err(SettleUpError::PreviewDigestMismatch { .. })
        ));
    }

    #[test]
    fn preview_digest_mismatch_details_survive_error_conversion() {
        let expected = PreviewedSettlementDigest([1; 32]);
        let actual = PreviewedSettlementDigest([2; 32]);

        let converted: crate::error::SettlementOptimizationError =
            SettleUpError::PreviewDigestMismatch { expected, actual }.into();

        assert_eq!(
            converted,
            crate::error::SettlementOptimizationError::PreviewDigestMismatch { expected, actual }
        );
    }

    #[test]
    fn preview_confirmation_binding_captures_context_and_digest() {
        let input = balances([(1, 100), (2, -100)]);
        let context = SettlementContext::jpy_default();
        let previewed = SettleUpPolicy::preview(
            &TwoMemberSquaringPlanner,
            &input,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");
        let expires_at =
            std::time::SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_700_000_000);

        let binding = PreviewConfirmationBinding::capture(
            LedgerId(42),
            EntryHash([9; 32]),
            MemberId(7),
            expires_at,
            &previewed,
        );

        assert_eq!(
            binding,
            PreviewConfirmationBinding {
                ledger_id: LedgerId(42),
                ledger_head_hash: EntryHash([9; 32]),
                actor_id: MemberId(7),
                preview_digest: previewed.digest(),
                expires_at,
            }
        );
    }

    /// Planner that zeros any two-member input by transferring the absolute value of
    /// the debtor's balance from the debtor to the creditor. Lets the digest test
    /// produce two valid-but-different previews from different inputs without juggling
    /// fixture members.
    struct TwoMemberSquaringPlanner;

    impl SettlementPlanner for TwoMemberSquaringPlanner {
        fn plan(
            &self,
            balances: MemberBalances,
            _settle_members: &[MemberId],
            _cash_members: &[MemberId],
            _context: SettlementContext,
        ) -> Result<Settlement, walicord_domain::SettlementRoundingError> {
            let mut iter = balances.iter();
            let (id_one, &money_one) = iter.next().expect("two-member input expected");
            let (id_two, &money_two) = iter.next().expect("two-member input expected");
            let (debtor, creditor, amount) = if money_one < Money::ZERO {
                (*id_one, *id_two, -money_one)
            } else {
                (*id_two, *id_one, -money_two)
            };
            let mut new_balances = MemberBalances::default();
            new_balances.insert(debtor, Money::ZERO);
            new_balances.insert(creditor, Money::ZERO);
            Ok(Settlement {
                new_balances,
                transfers: vec![Transfer {
                    from: debtor,
                    to: creditor,
                    amount,
                }],
            })
        }
    }

    #[test]
    fn record_previewed_plan_revalidates_against_captured_inputs() {
        // The PreviewedSettlement is structurally proof that validation already passed,
        // but `record_previewed_plan` re-runs the same validator — defense-in-depth for
        // long-lived previews that wait on user confirmation across requests. Pass a
        // valid preview through and confirm the recorded plan is identical to the
        // previewed plan (no spurious mutation, error wrapping, etc.).
        let input = balances([(1, 100), (2, -100)]);
        let context = SettlementContext::jpy_default();
        let planner = CreditorReceivesAllPlanner;

        let previewed = SettleUpPolicy::preview(
            &planner,
            &input,
            &[MemberId(1), MemberId(2)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");

        let plan_seen_by_user = previewed.plan().clone();
        let recorded = SettleUpPolicy::record_previewed_plan(previewed)
            .expect("record should pass re-validation");
        assert_eq!(recorded.settlement(), &plan_seen_by_user);
    }

    #[test]
    fn accepts_empty_plan_when_settle_members_are_already_zero() {
        // No transfers needed because every settle target is already at zero. The
        // representability gate must not fire here — validation should accept the
        // no-op case and surface it as `NoTransfersNeeded` at the typed write boundary.
        let input = balances([(1, 0), (2, 0)]);
        let plan = Settlement {
            new_balances: balances([(1, 0), (2, 0)]),
            transfers: Vec::new(),
        };

        assert!(
            validate_settlement_plan(&input, &[MemberId(1), MemberId(2)], &plan).is_ok(),
            "empty no-op plan should pass validation"
        );
    }

    #[test]
    fn empty_plan_maps_to_explicit_no_transfers_needed_outcome() {
        let input = balances([(1, 0), (2, 0)]);
        let plan = Settlement {
            new_balances: balances([(1, 0), (2, 0)]),
            transfers: Vec::new(),
        };

        let validated = validate_settlement_plan(&input, &[MemberId(1), MemberId(2)], &plan)
            .expect("empty no-op plan should validate");

        assert_eq!(
            validated.ledger_event_outcome(),
            &SettlementLedgerEventOutcome::NoTransfersNeeded
        );
        assert_eq!(validated.recordable_event(), None);
        assert!(validated.transfers().is_empty());
    }

    /// Planner that returns transfers in non-canonical (reverse-sorted) order. Used to
    /// confirm that `preview` canonicalizes the planner's output before storing it on
    /// the `PreviewedSettlement`, so the value the user confirmed is byte-for-byte
    /// identical to what the ledger event would record.
    struct NonCanonicalOrderPlanner;

    impl SettlementPlanner for NonCanonicalOrderPlanner {
        fn plan(
            &self,
            _balances: MemberBalances,
            _settle_members: &[MemberId],
            _cash_members: &[MemberId],
            _context: SettlementContext,
        ) -> Result<Settlement, walicord_domain::SettlementRoundingError> {
            // Inputs A:-100, B:+50, C:+50. Two valid transfers, but listed in reverse
            // (from, to) order so canonicalization changes the vector.
            Ok(Settlement {
                new_balances: balances([(1, 0), (2, 0), (3, 0)]),
                transfers: vec![transfer(1, 3, 50), transfer(1, 2, 50)],
            })
        }
    }

    #[test]
    fn preview_canonicalizes_planner_transfers_to_ledger_event_form() {
        let input = balances([(1, -100), (2, 50), (3, 50)]);
        let context = SettlementContext::jpy_default();

        let previewed = SettleUpPolicy::preview(
            &NonCanonicalOrderPlanner,
            &input,
            &[MemberId(1)],
            std::iter::empty::<MemberId>(),
            context,
        )
        .expect("preview should succeed");

        // The previewed transfers must equal exactly what
        // `NormalizedSettlementPlanRecorded::new` would store, regardless of the order
        // the planner emitted them in. Otherwise the user would see a different ordering
        // than what gets recorded — preview/commit/ledger replay drift.
        let canonical = walicord_ledger::NormalizedSettlementPlanRecorded::new(vec![
            transfer(1, 3, 50),
            transfer(1, 2, 50),
        ])
        .expect("ledger event should be representable");
        assert_eq!(previewed.plan().transfers, canonical.transfers());
        assert_eq!(
            previewed.ledger_event_outcome(),
            &recorded_outcome(canonical.transfers().to_vec())
        );
        assert_eq!(previewed.recordable_event(), Some(&canonical));
    }

    /// Minimal correct planner used by the round-trip test: zeros A and B with one
    /// transfer of 100 from B to A (A is creditor +100, B is debtor -100 in the input).
    struct CreditorReceivesAllPlanner;

    impl SettlementPlanner for CreditorReceivesAllPlanner {
        fn plan(
            &self,
            _balances: MemberBalances,
            _settle_members: &[MemberId],
            _cash_members: &[MemberId],
            _context: SettlementContext,
        ) -> Result<Settlement, walicord_domain::SettlementRoundingError> {
            Ok(Settlement {
                new_balances: balances([(1, 0), (2, 0)]),
                transfers: vec![transfer(2, 1, 100)],
            })
        }
    }
}
