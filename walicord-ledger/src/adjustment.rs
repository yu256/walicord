use std::collections::BTreeMap;
use walicord_domain::{Money, model::MemberId};

use crate::LedgerEntryId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BalanceAdjustment {
    pub member_id: MemberId,
    pub amount: Money,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdjustmentReason(String);

impl AdjustmentReason {
    pub fn new(reason: impl Into<String>) -> Result<Self, AdjustmentReasonError> {
        let reason = reason.into().trim().to_owned();
        if reason.is_empty() {
            return Err(AdjustmentReasonError::Empty);
        }

        Ok(Self(reason))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AdjustmentReasonError {
    Empty,
}

/// Explicit intent marker required to construct
/// [`BalanceAdjustmentSource::ExternalCorrection`]. This is **not** a hard authority or
/// permission boundary: any caller with access to `walicord-ledger` can still construct
/// the token. The real authorization boundary lives in application use cases. What this
/// type buys is review visibility — direct `ExternalCorrection` construction becomes
/// noisy and grep-able instead of looking like an ordinary enum variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdminCorrectionAuthority(());

impl AdminCorrectionAuthority {
    /// Construct the explicit marker required for an admin-style external correction.
    /// The verbose name is intentional: callers should have to opt into the fact that
    /// they are bypassing the standard related-entry correction flows. This remains a
    /// review-time signal, not a sealed authority object.
    pub fn explicit_admin_capability() -> Self {
        Self(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BalanceAdjustmentSource {
    /// Compensates a mistake found in a sealed expense or settlement transfer entry.
    SealedEntryCorrection(LedgerEntryId),
    /// Compensates a mistake found in a prior balance-adjustment entry — used when the
    /// previous correction itself was wrong. First-class instead of "another
    /// `ExternalCorrection`" so the audit chain stays explicit: the related entry id is
    /// the prior `BalanceAdjusted` whose effect is being adjusted, not an expense.
    PriorAdjustmentCorrection(LedgerEntryId),
    /// Current correction not attributable to any specific ledger entry. Reserved for
    /// administrative flows; construction requires an [`AdminCorrectionAuthority`]
    /// marker so ordinary call sites cannot stumble into this variant accidentally.
    /// The token field carries no data and is not a security boundary; it exists only to
    /// force explicit review-visible intent at construction sites.
    ExternalCorrection(AdminCorrectionAuthority),
}

impl BalanceAdjustmentSource {
    pub fn related_entry(&self) -> Option<LedgerEntryId> {
        match self {
            Self::SealedEntryCorrection(related_entry) => Some(*related_entry),
            Self::PriorAdjustmentCorrection(related_entry) => Some(*related_entry),
            Self::ExternalCorrection(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BalanceAdjusted {
    adjustments: Vec<BalanceAdjustment>,
    reason: AdjustmentReason,
    source: BalanceAdjustmentSource,
}

impl BalanceAdjusted {
    pub fn new(
        adjustments: Vec<BalanceAdjustment>,
        reason: AdjustmentReason,
        source: BalanceAdjustmentSource,
    ) -> Result<Self, BalanceAdjustedError> {
        if adjustments.is_empty() {
            return Err(BalanceAdjustedError::EmptyAdjustments);
        }

        let mut canonical = BTreeMap::new();

        for adjustment in adjustments {
            if adjustment.amount == Money::ZERO {
                return Err(BalanceAdjustedError::ZeroAmount {
                    member_id: adjustment.member_id,
                });
            }
            *canonical.entry(adjustment.member_id).or_insert(Money::ZERO) += adjustment.amount;
        }

        let adjustments = canonical
            .into_iter()
            .map(|(member_id, amount)| {
                if amount == Money::ZERO {
                    Err(BalanceAdjustedError::CancelledOutAdjustment { member_id })
                } else {
                    Ok(BalanceAdjustment { member_id, amount })
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        let total = adjustments
            .iter()
            .map(|adjustment| adjustment.amount)
            .sum::<Money>();

        if total != Money::ZERO {
            return Err(BalanceAdjustedError::Imbalanced { total });
        }

        Ok(Self {
            adjustments,
            reason,
            source,
        })
    }

    pub fn adjustments(&self) -> &[BalanceAdjustment] {
        &self.adjustments
    }

    pub fn reason(&self) -> &AdjustmentReason {
        &self.reason
    }

    pub fn source(&self) -> &BalanceAdjustmentSource {
        &self.source
    }

    pub fn related_entry(&self) -> Option<LedgerEntryId> {
        self.source.related_entry()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BalanceAdjustedError {
    EmptyAdjustments,
    ZeroAmount { member_id: MemberId },
    CancelledOutAdjustment { member_id: MemberId },
    Imbalanced { total: Money },
}
