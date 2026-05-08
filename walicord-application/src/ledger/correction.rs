use walicord_domain::model::MemberId;
use walicord_ledger::{
    AdjustmentReason, AdminCorrectionAuthority, BalanceAdjusted, BalanceAdjustedError,
    BalanceAdjustment, BalanceAdjustmentSource, LedgerEntryId, ProjectedEntryKind, ProjectedLedger,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StandardSealedEntryCorrectionError {
    /// `related_entry` is not present in the supplied `ProjectedLedger`'s entry index. It is
    /// either an unknown id or a marker entry (`LedgerHistorySealed`, `EntryVoided`,
    /// `BalanceAdjusted`) that has no event-payload participants.
    RelatedEntryNotApplied {
        related_entry: LedgerEntryId,
    },
    /// `related_entry` exists but was voided by a later `EntryVoided` event.
    RelatedEntryVoided {
        related_entry: LedgerEntryId,
    },
    /// `related_entry` exists but has not yet been sealed — this helper is for sealed-entry
    /// corrections only; unsealed entries should be amended via `EntryVoided` plus a new
    /// `ExpenseRecorded` / `NormalizedSettlementPlanRecorded`.
    RelatedEntryNotSealed {
        related_entry: LedgerEntryId,
    },
    NonParticipantMember {
        member_id: MemberId,
    },
    Adjustment(BalanceAdjustedError),
}

/// Builds a `BalanceAdjusted` event for the standard sealed-entry correction flow.
/// The core ledger only requires at least one adjustment member to participate in the
/// related sealed entry; this helper enforces the stricter standard-flow contract:
///
/// * the related entry must exist and be an expense / settlement transfer (not a marker);
/// * the related entry must already be inside sealed history;
/// * the related entry must not have been voided;
/// * every adjustment member must be a participant of the related entry.
///
/// All checks read from the supplied `ProjectedLedger` so callers cannot drift from the
/// canonical projection. Administrative flows that legitimately need to include
/// non-participants or to relate to an unsealed entry should bypass this helper and
/// construct `BalanceAdjusted` directly with prominent auditing.
pub fn standard_sealed_entry_correction(
    adjustments: Vec<BalanceAdjustment>,
    reason: AdjustmentReason,
    related_entry: LedgerEntryId,
    projected: &ProjectedLedger,
) -> Result<BalanceAdjusted, StandardSealedEntryCorrectionError> {
    let Some(related_info) = projected.entry(related_entry) else {
        return Err(StandardSealedEntryCorrectionError::RelatedEntryNotApplied { related_entry });
    };

    if !matches!(
        related_info.kind,
        ProjectedEntryKind::Expense | ProjectedEntryKind::SettlementTransfer
    ) {
        return Err(StandardSealedEntryCorrectionError::RelatedEntryNotApplied { related_entry });
    }

    if related_info.voided {
        return Err(StandardSealedEntryCorrectionError::RelatedEntryVoided { related_entry });
    }

    if !related_info.sealed {
        return Err(StandardSealedEntryCorrectionError::RelatedEntryNotSealed { related_entry });
    }

    if let Some(non_participant) = adjustments
        .iter()
        .find(|adjustment| !related_info.participants.contains(&adjustment.member_id))
    {
        return Err(StandardSealedEntryCorrectionError::NonParticipantMember {
            member_id: non_participant.member_id,
        });
    }

    BalanceAdjusted::new(
        adjustments,
        reason,
        BalanceAdjustmentSource::SealedEntryCorrection(related_entry),
    )
    .map_err(StandardSealedEntryCorrectionError::Adjustment)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StandardPriorAdjustmentCorrectionError {
    /// `related_adjustment_entry` is not present in the supplied `ProjectedLedger`'s entry
    /// index — it is either an unknown id or a marker entry that has no entry-info.
    RelatedEntryNotApplied {
        related_entry: LedgerEntryId,
    },
    /// `related_adjustment_entry` is in the index but is not a prior `BalanceAdjusted`
    /// entry. `PriorAdjustmentCorrection` only accepts another `BalanceAdjusted` as its
    /// target; for expense / settlement transfer entries, use
    /// [`standard_sealed_entry_correction`] instead.
    RelatedEntryNotAdjustment {
        related_entry: LedgerEntryId,
    },
    /// The related prior adjustment is itself voided. The initial model does not allow
    /// voiding `BalanceAdjusted`, so this should not be observable today, but the helper
    /// rejects defensively for forward compatibility.
    RelatedEntryVoided {
        related_entry: LedgerEntryId,
    },
    /// The related prior adjustment has not been sealed yet. Standard prior-adjustment
    /// correction is for fixing mistakes in already-sealed adjustments; if the prior
    /// adjustment is unsealed, the model has no first-class flow yet — surface the error
    /// rather than silently producing a `BalanceAdjusted` that core projection would
    /// reject.
    RelatedEntryNotSealed {
        related_entry: LedgerEntryId,
    },
    /// At least one adjustment member is not a participant of the prior adjustment, so
    /// the correction does not match the audit scope of the original adjustment.
    NonParticipantMember {
        member_id: MemberId,
    },
    Adjustment(BalanceAdjustedError),
}

/// Builds a `BalanceAdjusted` event whose source is
/// [`BalanceAdjustmentSource::PriorAdjustmentCorrection`], i.e. an adjustment that exists
/// to fix a mistake in an earlier `BalanceAdjusted`. Enforces the standard-flow contract
/// analogous to [`standard_sealed_entry_correction`]:
///
/// * the related entry must exist and be a `BalanceAdjusted` entry (not expense /
///   settlement / marker);
/// * the related entry must be inside sealed history;
/// * the related entry must not be voided;
/// * every adjustment member must be a participant of the prior adjustment.
///
/// Administrative flows that legitimately need to relax these constraints should bypass
/// the helper and construct `BalanceAdjusted` directly with prominent auditing.
pub fn standard_prior_adjustment_correction(
    adjustments: Vec<BalanceAdjustment>,
    reason: AdjustmentReason,
    related_adjustment_entry: LedgerEntryId,
    projected: &ProjectedLedger,
) -> Result<BalanceAdjusted, StandardPriorAdjustmentCorrectionError> {
    let Some(related_info) = projected.entry(related_adjustment_entry) else {
        return Err(
            StandardPriorAdjustmentCorrectionError::RelatedEntryNotApplied {
                related_entry: related_adjustment_entry,
            },
        );
    };

    if !matches!(related_info.kind, ProjectedEntryKind::BalanceAdjustment) {
        return Err(
            StandardPriorAdjustmentCorrectionError::RelatedEntryNotAdjustment {
                related_entry: related_adjustment_entry,
            },
        );
    }

    if related_info.voided {
        return Err(StandardPriorAdjustmentCorrectionError::RelatedEntryVoided {
            related_entry: related_adjustment_entry,
        });
    }

    if !related_info.sealed {
        return Err(
            StandardPriorAdjustmentCorrectionError::RelatedEntryNotSealed {
                related_entry: related_adjustment_entry,
            },
        );
    }

    if let Some(non_participant) = adjustments
        .iter()
        .find(|adjustment| !related_info.participants.contains(&adjustment.member_id))
    {
        return Err(
            StandardPriorAdjustmentCorrectionError::NonParticipantMember {
                member_id: non_participant.member_id,
            },
        );
    }

    BalanceAdjusted::new(
        adjustments,
        reason,
        BalanceAdjustmentSource::PriorAdjustmentCorrection(related_adjustment_entry),
    )
    .map_err(StandardPriorAdjustmentCorrectionError::Adjustment)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AdminExternalBalanceCorrectionError {
    /// `BalanceAdjusted` requires a sealed history segment to exist; the projector enforces
    /// this on replay (`BalanceAdjustmentWithoutSealedHistory`). Admin flows must surface
    /// the same precondition at command-construction time so the operator sees a clear
    /// reason for the refusal instead of producing an event that fails replay.
    NoSealedHistory,
    Adjustment(BalanceAdjustedError),
}

/// Builds a `BalanceAdjusted` event whose source is
/// [`BalanceAdjustmentSource::ExternalCorrection`] — the catch-all admin correction whose
/// audit trail does not point at any specific prior entry. The core ledger permits this
/// freely as long as a sealed history exists; this helper is the **intended audited
/// application entry point** for constructing `ExternalCorrection`, because:
///
/// * the helper name and dedicated error type make it obvious in code review that an admin
///   capability is being exercised, and
/// * direct construction at call sites bypasses the audit framing this helper provides
///   (the `admin_` prefix, the singular admin error type, the centralized doc).
///
/// This is still not a sealed authority boundary: code that imports `walicord-ledger`
/// can construct `ExternalCorrection` directly. The core model stays permissive because
/// it cannot know which callers are admin-authorized, so the meaningful discipline lives
/// at the application boundary and in code review. Regular use cases should stay on this
/// helper.
///
/// Standard correction flows (sealed-entry and prior-adjustment) should use
/// [`standard_sealed_entry_correction`] / [`standard_prior_adjustment_correction`]; only
/// reach for this helper when no specific related entry exists (e.g. balance reconciliation
/// against an external authority).
pub fn admin_external_balance_correction(
    adjustments: Vec<BalanceAdjustment>,
    reason: AdjustmentReason,
    projected: &ProjectedLedger,
) -> Result<BalanceAdjusted, AdminExternalBalanceCorrectionError> {
    if projected.state().sealed_history().is_none() {
        return Err(AdminExternalBalanceCorrectionError::NoSealedHistory);
    }

    BalanceAdjusted::new(
        adjustments,
        reason,
        BalanceAdjustmentSource::ExternalCorrection(
            AdminCorrectionAuthority::explicit_admin_capability(),
        ),
    )
    .map_err(AdminExternalBalanceCorrectionError::Adjustment)
}

/// Reconstructs the raw `ExternalCorrection` source when decoding previously-authored
/// canonical ledger data. This is intentionally separate from
/// [`admin_external_balance_correction`]: transport decode needs to round-trip existing
/// history faithfully, but new application writes should continue to go through the audited
/// admin helper above.
pub fn external_correction_source_for_transport_decode() -> BalanceAdjustmentSource {
    BalanceAdjustmentSource::ExternalCorrection(
        AdminCorrectionAuthority::explicit_admin_capability(),
    )
}
