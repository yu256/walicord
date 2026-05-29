use super::{
    projection::{
        ProjectionConsistencyError, VerifiedLedgerEntryView, project_recent_voidable_entries,
    },
    sessions::{
        VoidCandidateSelection, VoidSession, VoidSessionConstructionError, VoidSessionKey,
        VoidSessionStage,
    },
    store::VerifiedLedgerThreadLoad,
};
use walicord_application::{
    Clock, InteractionNonce, NonceProvider,
    ledger::{
        DiscordLedgerEntryError, DiscordLedgerSourceDescriptor, LedgerCanonicalEncodeError,
        LedgerEntry, LedgerEntryId, LedgerId, UnverifiedLedgerStoreEnvelope,
        build_discord_void_entry, make_unverified_envelope_sha256_v1,
    },
};
use walicord_domain::model::MemberId;

/// Maximum number of voidable candidates the `/void` and panel `取り消し` UI will
/// expose at once (criterion 21). Older entries fall back to the operator handoff
/// path (criterion 106).
pub const VOID_CANDIDATE_WINDOW: usize = 20;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VoidCandidateEnumerationError {
    Projection(ProjectionConsistencyError),
}

/// Enumerate the latest-20 voidable entries on the verified canonical thread; this is
/// the candidate set that `/void` / panel `取り消し` shows the actor. Entries older
/// than the window are intentionally excluded — the criterion-106 operator-handoff
/// path is responsible for them.
pub fn enumerate_void_candidates(
    load: &VerifiedLedgerThreadLoad,
) -> Result<Vec<VerifiedLedgerEntryView>, VoidCandidateEnumerationError> {
    project_recent_voidable_entries(load, VOID_CANDIDATE_WINDOW)
        .map_err(VoidCandidateEnumerationError::Projection)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VoidSessionBootstrapError {
    NoVoidableCandidates,
    Projection(ProjectionConsistencyError),
    ConstructionFailed(VoidSessionConstructionError),
}

/// Construct a fresh `VoidSession` in `SelectingCandidate` from the latest voidable
/// window. Returns `NoVoidableCandidates` if the window is empty so the caller can
/// surface the criterion-119 empty-state message (or criterion-106 older-than-window
/// handoff guidance, depending on context).
pub fn bootstrap_void_session(
    key: VoidSessionKey,
    load: &VerifiedLedgerThreadLoad,
    clock: &dyn Clock,
    nonce_provider: &dyn NonceProvider,
) -> Result<(VoidSession, InteractionNonce, Vec<VerifiedLedgerEntryView>), VoidSessionBootstrapError>
{
    let candidates = enumerate_void_candidates(load).map_err(|error| match error {
        VoidCandidateEnumerationError::Projection(err) => {
            VoidSessionBootstrapError::Projection(err)
        }
    })?;
    if candidates.is_empty() {
        return Err(VoidSessionBootstrapError::NoVoidableCandidates);
    }

    let nonce = nonce_provider.next_interaction_nonce();
    let session = VoidSession::new(
        key,
        VoidSessionStage::SelectingCandidate,
        None,
        nonce,
        clock.now(),
    )
    .map_err(VoidSessionBootstrapError::ConstructionFailed)?;
    Ok((session, nonce, candidates))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VoidConfirmTransitionError {
    NotInSelection,
    CandidateNotFound { target_entry_id: LedgerEntryId },
    ConstructionFailed(VoidSessionConstructionError),
}

/// Advance a `SelectingCandidate` session to `Confirming` once the actor selects a
/// target. The target must still be in the latest voidable window or the function
/// rejects with `CandidateNotFound` so the caller re-renders the selection (criterion
/// 112 / 135).
pub fn transition_to_confirm(
    session: VoidSession,
    target_entry_id: LedgerEntryId,
    candidates: &[VerifiedLedgerEntryView],
    clock: &dyn Clock,
) -> Result<VoidSession, VoidConfirmTransitionError> {
    if !matches!(session.stage(), VoidSessionStage::SelectingCandidate) {
        return Err(VoidConfirmTransitionError::NotInSelection);
    }
    let still_in_window = candidates
        .iter()
        .any(|view| view.entry().id == target_entry_id);
    if !still_in_window {
        return Err(VoidConfirmTransitionError::CandidateNotFound { target_entry_id });
    }

    VoidSession::new(
        session.key(),
        VoidSessionStage::Confirming,
        Some(VoidCandidateSelection::new(target_entry_id)),
        session.nonce(),
        clock.now(),
    )
    .map_err(VoidConfirmTransitionError::ConstructionFailed)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VoidComposeError {
    SessionNotConfirming,
    TargetNoLongerVoidable { target_entry_id: LedgerEntryId },
    EntryBuild(DiscordLedgerEntryError),
    EnvelopeEncode(LedgerCanonicalEncodeError),
}

/// Build the canonical void entry + envelope from a confirmation-stage session against
/// the live verified snapshot. The function re-validates that the target is still in
/// the voidable window at record time (criterion 112).
pub fn compose_void_entry(
    session: &VoidSession,
    load: &VerifiedLedgerThreadLoad,
    ledger_id: LedgerId,
    actor_id: MemberId,
    new_entry_id: LedgerEntryId,
    source: DiscordLedgerSourceDescriptor,
    clock: &dyn Clock,
) -> Result<(LedgerEntry, UnverifiedLedgerStoreEnvelope<()>), VoidComposeError> {
    let Some(selection) = session.selection() else {
        return Err(VoidComposeError::SessionNotConfirming);
    };
    if !matches!(session.stage(), VoidSessionStage::Confirming) {
        return Err(VoidComposeError::SessionNotConfirming);
    }

    let candidates =
        project_recent_voidable_entries(load, VOID_CANDIDATE_WINDOW).map_err(|_| {
            VoidComposeError::TargetNoLongerVoidable {
                target_entry_id: selection.target_entry_id(),
            }
        })?;
    let target = candidates
        .iter()
        .find(|view| view.entry().id == selection.target_entry_id())
        .ok_or(VoidComposeError::TargetNoLongerVoidable {
            target_entry_id: selection.target_entry_id(),
        })?;

    let entry = build_discord_void_entry(new_entry_id, actor_id, target.entry().id, source, clock)
        .map_err(VoidComposeError::EntryBuild)?;

    let previous_hash = load
        .snapshot()
        .current_head_hash()
        .expect("voidable window proves at least one entry exists, head must be set");
    let envelope = make_unverified_envelope_sha256_v1(ledger_id, previous_hash, (), entry.clone())
        .map_err(VoidComposeError::EnvelopeEncode)?;
    Ok((entry, envelope))
}

#[cfg(test)]
mod tests {
    use super::*;
    // No standalone tests here yet — the underlying projection helpers already cover
    // window selection, voided/sealed exclusion, and settlement candidacy via
    // `project_recent_voidable_entries` tests in projection.rs.

    #[test]
    fn void_candidate_window_matches_required_criterion_value() {
        assert_eq!(VOID_CANDIDATE_WINDOW, 20);
    }
}
