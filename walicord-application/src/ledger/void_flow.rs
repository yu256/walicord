use crate::{
    Clock, SessionNonce, SessionNonceProvider,
    ledger::{
        DiscordLedgerEntryError, DiscordLedgerSourceDescriptor, LedgerCanonicalEncodeError,
        LedgerEntry, LedgerEntryId, LedgerId, UnverifiedLedgerStoreEnvelope,
        build_discord_void_entry,
        expense_session::{
            VoidCandidateSelection, VoidSession, VoidSessionConstructionError, VoidSessionKey,
            VoidSessionStage,
        },
        make_unverified_envelope_sha256_v1,
        projection::{
            ProjectionConsistencyError, VerifiedLedgerEntryView, VerifiedLedgerThreadLoad,
            project_recent_voidable_entries,
        },
    },
};
use walicord_domain::model::MemberId;

/// Maximum number of voidable candidates the `/void` and panel `取り消し` UI will
/// expose at once. Matches `DISCORD_SELECT_OPTION_LIMIT` (25). Older entries fall
/// back to the operator handoff path (criterion 106).
pub const VOID_CANDIDATE_WINDOW: usize = 25;

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum VoidCandidateEnumerationError {
    #[error("projection consistency: {0}")]
    Projection(#[from] ProjectionConsistencyError),
}

/// Enumerate the latest voidable entries on the verified canonical thread; this is
/// the candidate set that `/void` / panel `取り消し` shows the actor. Entries older
/// than the window are intentionally excluded — the criterion-106 operator-handoff
/// path is responsible for them.
pub fn enumerate_void_candidates<ExternalId>(
    load: &VerifiedLedgerThreadLoad<ExternalId>,
) -> Result<Vec<VerifiedLedgerEntryView>, VoidCandidateEnumerationError> {
    project_recent_voidable_entries(load, VOID_CANDIDATE_WINDOW)
        .map_err(VoidCandidateEnumerationError::Projection)
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum VoidSessionBootstrapError {
    #[error("no voidable candidates in the latest window")]
    NoVoidableCandidates,
    #[error("projection consistency: {0}")]
    Projection(#[from] ProjectionConsistencyError),
    #[error("void session construction failed: {0}")]
    ConstructionFailed(#[from] VoidSessionConstructionError),
}

/// Construct a fresh `VoidSession` in `SelectingCandidate` from the latest voidable
/// window. Returns `NoVoidableCandidates` if the window is empty so the caller can
/// surface the criterion-119 empty-state message (or criterion-106 older-than-window
/// handoff guidance, depending on context).
pub fn bootstrap_void_session<ExternalId>(
    key: VoidSessionKey,
    load: &VerifiedLedgerThreadLoad<ExternalId>,
    clock: &dyn Clock,
    nonce_provider: &dyn SessionNonceProvider,
) -> Result<(VoidSession, SessionNonce, Vec<VerifiedLedgerEntryView>), VoidSessionBootstrapError> {
    let candidates = enumerate_void_candidates(load).map_err(|error| match error {
        VoidCandidateEnumerationError::Projection(err) => {
            VoidSessionBootstrapError::Projection(err)
        }
    })?;
    if candidates.is_empty() {
        return Err(VoidSessionBootstrapError::NoVoidableCandidates);
    }

    let nonce = nonce_provider.next_session_nonce();
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

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum VoidConfirmTransitionError {
    #[error("void session is not in SelectingCandidate")]
    NotInSelection,
    #[error("void candidate {target_entry_id:?} is no longer in the voidable window")]
    CandidateNotFound { target_entry_id: LedgerEntryId },
    #[error("void session construction failed: {0}")]
    ConstructionFailed(#[from] VoidSessionConstructionError),
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

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum VoidComposeError {
    #[error("void session is not in Confirming")]
    SessionNotConfirming,
    #[error("void target {target_entry_id:?} is no longer voidable")]
    TargetNoLongerVoidable { target_entry_id: LedgerEntryId },
    #[error("void entry build: {0}")]
    EntryBuild(#[from] DiscordLedgerEntryError),
    #[error("void envelope encode: {0}")]
    EnvelopeEncode(#[from] LedgerCanonicalEncodeError),
}

/// Build the canonical void entry + envelope from a confirmation-stage session against
/// the live verified snapshot. The function re-validates that the target is still in
/// the voidable window at record time (criterion 112).
pub fn compose_void_entry<ExternalId>(
    session: &VoidSession,
    load: &VerifiedLedgerThreadLoad<ExternalId>,
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

const _: () = assert!(VOID_CANDIDATE_WINDOW <= 25);
