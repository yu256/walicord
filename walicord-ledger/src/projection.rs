use std::collections::{BTreeMap, BTreeSet};

use walicord_domain::{MemberBalances, Money, Transfer, model::MemberId};

use crate::{
    AppendOrderedLedgerRecords, BalanceAdjusted, BalanceAdjustmentSource, ExpenseRecorded,
    LedgerEntryId, LedgerEvent, LedgerProjectionError, LedgerRecord,
    NormalizedSettlementPlanRecorded, validation::StructuralValidation,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SealedHistory {
    pub marker_entry_id: LedgerEntryId,
    pub through_entry_id: LedgerEntryId,
}

/// Projected ledger state. `balances` is a pure balance map with zero-balance entries pruned;
/// the canonical participant set lives in `participants` so review/presentation flows do not
/// need to interpret balance map keys as participation evidence.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LedgerState {
    balances: MemberBalances,
    participants: BTreeSet<MemberId>,
    voided_entry_ids: BTreeSet<LedgerEntryId>,
    voided_expense_entry_ids: BTreeSet<LedgerEntryId>,
    sealed_history: Option<SealedHistory>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectedEntryKind {
    /// Event-payload entry recording an expense.
    Expense,
    /// Event-payload entry recording fixed settlement transfers.
    SettlementTransfer,
    /// Marker entry sealing history through `target`. Marker `target` is the closed
    /// position; the marker itself is not closed by itself.
    HistorySealMarker { target: LedgerEntryId },
    /// Marker entry voiding `target`. The target is the entry whose effect was reversed.
    VoidMarker { target: LedgerEntryId },
    /// Manual zero-sum balance adjustment applied after history was sealed. The members
    /// touched are exposed via `participants` and the balance delta via `effect`, even
    /// though they are audit metadata rather than business event-payload data.
    BalanceAdjustment,
}

/// Per-entry projection result that is preserved for all ledger entries, not just expense
/// and settlement transfer entries. `participants` and `effect` are populated for entries
/// that touch members (Expense, SettlementTransfer, BalanceAdjustment); marker entries
/// (HistorySealMarker, VoidMarker) carry empty `participants`/`effect`. Voided expense or
/// settlement transfer entries remain in the index with `voided = true` so callers can
/// distinguish "not found" from "present but voided"; markers and balance adjustments
/// cannot be voided in the initial model and have `voided = false` always. `sealed` is true
/// for any entry whose append position lies inside sealed history — it applies to markers
/// and adjustments too, so audit/presentation flows can show "this adjustment is inside the
/// sealed range".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectedEntryInfo {
    pub kind: ProjectedEntryKind,
    pub participants: BTreeSet<MemberId>,
    pub effect: MemberBalances,
    pub append_position: usize,
    pub sealed: bool,
    pub voided: bool,
}

/// Per-entry index that survives projection. Use this index when a use case (such as
/// standard sealed-entry correction or presentation drill-down) needs to reason about a
/// specific entry's participants, kind, effect, sealed/voided status, or append position
/// without re-deriving them from the raw event log.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ProjectedEntryIndex {
    by_entry: BTreeMap<LedgerEntryId, ProjectedEntryInfo>,
}

impl ProjectedEntryIndex {
    pub fn get(&self, entry_id: LedgerEntryId) -> Option<&ProjectedEntryInfo> {
        self.by_entry.get(&entry_id)
    }

    pub fn participants_of(&self, entry_id: LedgerEntryId) -> Option<&BTreeSet<MemberId>> {
        self.by_entry.get(&entry_id).map(|info| &info.participants)
    }

    pub fn contains(&self, entry_id: LedgerEntryId) -> bool {
        self.by_entry.contains_key(&entry_id)
    }

    /// True iff the entry exists in the index (i.e. is an expense or settlement transfer)
    /// and the projection placed it inside sealed history.
    pub fn is_sealed(&self, entry_id: LedgerEntryId) -> bool {
        self.by_entry.get(&entry_id).is_some_and(|info| info.sealed)
    }

    /// True iff the entry exists in the index and was voided by a later `EntryVoided`.
    pub fn is_voided(&self, entry_id: LedgerEntryId) -> bool {
        self.by_entry.get(&entry_id).is_some_and(|info| info.voided)
    }

    /// Returns the latest entry of any kind in append order — markers (history seal, void,
    /// balance adjustment) are included alongside expense and settlement transfer entries.
    /// "Append tail" is the canonical position the next entry would be appended after; this
    /// is what ordinary seal/confirm flows use to seal through the reviewed range.
    pub fn latest_append_entry(&self) -> Option<EntryAppendPosition> {
        self.by_entry
            .iter()
            .max_by_key(|(_, info)| info.append_position)
            .map(|(entry_id, info)| EntryAppendPosition {
                entry_id: *entry_id,
                append_position: info.append_position,
            })
    }

    /// Returns the latest non-voided expense or settlement transfer entry in append order.
    /// Returns the latest expense or settlement-transfer entry that is not voided.
    /// `BalanceAdjustment` entries also affect balances and were once covered by the
    /// previous "latest_applied_entry" name, but the seal-target use case specifically
    /// wants to anchor on a business event (expense / settlement transfer), not on an
    /// audit-time adjustment. Distinct from `latest_append_entry`, which includes markers
    /// and adjustments.
    pub fn latest_unvoided_expense_or_settlement_entry(&self) -> Option<EntryAppendPosition> {
        self.by_entry
            .iter()
            .filter(|(_, info)| {
                matches!(
                    info.kind,
                    ProjectedEntryKind::Expense | ProjectedEntryKind::SettlementTransfer
                ) && !info.voided
            })
            .max_by_key(|(_, info)| info.append_position)
            .map(|(entry_id, info)| EntryAppendPosition {
                entry_id: *entry_id,
                append_position: info.append_position,
            })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EntryAppendPosition {
    pub entry_id: LedgerEntryId,
    pub append_position: usize,
}

/// Bundles the projected `LedgerState` with a `ProjectedEntryIndex`. Use cases that need
/// per-entry detail (correction flows, presentation drill-down) consume this; pure read
/// paths that only care about current balances can borrow `state()`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectedLedger {
    state: LedgerState,
    entry_index: ProjectedEntryIndex,
}

impl ProjectedLedger {
    pub fn state(&self) -> &LedgerState {
        &self.state
    }

    pub fn entry_index(&self) -> &ProjectedEntryIndex {
        &self.entry_index
    }

    pub fn entry(&self, entry_id: LedgerEntryId) -> Option<&ProjectedEntryInfo> {
        self.entry_index.get(entry_id)
    }

    pub fn participants_of(&self, entry_id: LedgerEntryId) -> Option<&BTreeSet<MemberId>> {
        self.entry_index.participants_of(entry_id)
    }

    pub fn is_entry_sealed(&self, entry_id: LedgerEntryId) -> bool {
        self.entry_index.is_sealed(entry_id)
    }

    pub fn is_entry_voided(&self, entry_id: LedgerEntryId) -> bool {
        self.entry_index.is_voided(entry_id)
    }

    pub fn into_state(self) -> LedgerState {
        self.state
    }

    pub fn into_parts(self) -> (LedgerState, ProjectedEntryIndex) {
        (self.state, self.entry_index)
    }
}

impl LedgerState {
    pub fn balances(&self) -> &MemberBalances {
        &self.balances
    }

    pub fn participants(&self) -> &BTreeSet<MemberId> {
        &self.participants
    }

    pub fn voided_entry_ids(&self) -> &BTreeSet<LedgerEntryId> {
        &self.voided_entry_ids
    }

    pub fn voided_expense_entry_ids(&self) -> &BTreeSet<LedgerEntryId> {
        &self.voided_expense_entry_ids
    }

    /// Entry id through which history is sealed; this is the seal target, not necessarily the marker entry.
    pub fn sealed_through(&self) -> Option<LedgerEntryId> {
        self.sealed_history
            .as_ref()
            .map(|history| history.through_entry_id)
    }

    pub fn sealed_history(&self) -> Option<&SealedHistory> {
        self.sealed_history.as_ref()
    }
}

#[derive(Debug, Default)]
pub struct LedgerProjector;

struct AppliedEntry {
    kind: ProjectedEntryKind,
    effect: MemberBalances,
    participants: BTreeSet<MemberId>,
    append_position: usize,
    voided: bool,
}

impl LedgerProjector {
    /// Replays entries in append order; entry ids are stable keys, not sort keys.
    pub fn replay(
        entries: &AppendOrderedLedgerRecords,
    ) -> Result<ProjectedLedger, LedgerProjectionError> {
        let validation = &entries.structural_validation;

        let mut balances = MemberBalances::default();
        let mut participants = BTreeSet::new();
        let mut sealed_history = None;
        let mut voided_entry_ids = BTreeSet::new();
        let mut voided_expense_entry_ids = BTreeSet::new();
        let mut applied_entries: BTreeMap<LedgerEntryId, AppliedEntry> = BTreeMap::new();

        for (position, entry) in entries.iter().enumerate() {
            match &entry.event {
                LedgerEvent::ExpenseRecorded(event) => {
                    let effect = expense_effect(event);
                    register_participants(&mut participants, effect.keys().copied());
                    apply_balance_delta(&mut balances, &effect);
                    let entry_participants = effect.keys().copied().collect();
                    applied_entries.insert(
                        entry.id,
                        AppliedEntry {
                            kind: ProjectedEntryKind::Expense,
                            effect,
                            participants: entry_participants,
                            append_position: position,
                            voided: false,
                        },
                    );
                }
                LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
                    let effect = apply_transfers(&mut balances, entry.id, event)?;
                    register_participants(&mut participants, effect.keys().copied());
                    let entry_participants = effect.keys().copied().collect();
                    applied_entries.insert(
                        entry.id,
                        AppliedEntry {
                            kind: ProjectedEntryKind::SettlementTransfer,
                            effect,
                            participants: entry_participants,
                            append_position: position,
                            voided: false,
                        },
                    );
                }
                LedgerEvent::LedgerHistorySealed(event) => {
                    let through_position = validation.entry_positions[&event.through()];
                    sealed_history = Some(latest_sealed_history(
                        sealed_history,
                        entry.id,
                        event.through(),
                        through_position,
                    ));
                    applied_entries.insert(
                        entry.id,
                        AppliedEntry {
                            kind: ProjectedEntryKind::HistorySealMarker {
                                target: event.through(),
                            },
                            effect: MemberBalances::default(),
                            participants: BTreeSet::new(),
                            append_position: position,
                            voided: false,
                        },
                    );
                }
                LedgerEvent::EntryVoided(event) => {
                    apply_void(
                        VoidContext {
                            entries: entries.as_slice(),
                            validation,
                            sealed_history,
                        },
                        &mut balances,
                        VoidState {
                            voided_entry_ids: &mut voided_entry_ids,
                            voided_expense_entry_ids: &mut voided_expense_entry_ids,
                            applied_entries: &mut applied_entries,
                        },
                        entry.id,
                        event.target(),
                    )?;
                    applied_entries.insert(
                        entry.id,
                        AppliedEntry {
                            kind: ProjectedEntryKind::VoidMarker {
                                target: event.target(),
                            },
                            effect: MemberBalances::default(),
                            participants: BTreeSet::new(),
                            append_position: position,
                            voided: false,
                        },
                    );
                }
                LedgerEvent::BalanceAdjusted(event) => {
                    apply_balance_adjustment(
                        &mut balances,
                        BalanceAdjustmentContext {
                            entries: entries.as_slice(),
                            validation,
                            sealed_history,
                            voided_entry_ids: &voided_entry_ids,
                            applied_entries: &applied_entries,
                        },
                        entry.id,
                        event,
                    )?;
                    register_participants(
                        &mut participants,
                        event.adjustments().iter().map(|a| a.member_id),
                    );

                    let mut adjustment_effect = MemberBalances::default();
                    for adjustment in event.adjustments() {
                        adjustment_effect.insert(adjustment.member_id, adjustment.amount);
                    }
                    let adjustment_participants = adjustment_effect.keys().copied().collect();
                    applied_entries.insert(
                        entry.id,
                        AppliedEntry {
                            kind: ProjectedEntryKind::BalanceAdjustment,
                            effect: adjustment_effect,
                            participants: adjustment_participants,
                            append_position: position,
                            voided: false,
                        },
                    );
                }
            }
        }

        let total = balances.values().sum::<Money>();
        if total != Money::ZERO {
            return Err(LedgerProjectionError::ImbalancedLedgerState { total });
        }

        prune_zero_balances(&mut balances);

        let sealed_through_position = sealed_history.map(|history| history.through_position);
        let by_entry = applied_entries
            .into_iter()
            .map(|(entry_id, applied)| {
                let sealed = sealed_through_position
                    .is_some_and(|through| applied.append_position <= through);
                (
                    entry_id,
                    ProjectedEntryInfo {
                        kind: applied.kind,
                        participants: applied.participants,
                        effect: applied.effect,
                        append_position: applied.append_position,
                        sealed,
                        voided: applied.voided,
                    },
                )
            })
            .collect();

        Ok(ProjectedLedger {
            state: LedgerState {
                balances,
                participants,
                voided_entry_ids,
                voided_expense_entry_ids,
                sealed_history: sealed_history.map(SealedHistoryState::into_public),
            },
            entry_index: ProjectedEntryIndex { by_entry },
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SealedHistoryState {
    marker_entry_id: LedgerEntryId,
    through_entry_id: LedgerEntryId,
    through_position: usize,
}

impl SealedHistoryState {
    fn into_public(self) -> SealedHistory {
        SealedHistory {
            marker_entry_id: self.marker_entry_id,
            through_entry_id: self.through_entry_id,
        }
    }
}

fn latest_sealed_history(
    current: Option<SealedHistoryState>,
    marker_entry_id: LedgerEntryId,
    through_entry_id: LedgerEntryId,
    through_position: usize,
) -> SealedHistoryState {
    match current {
        Some(current) if current.through_position > through_position => current,
        _ => SealedHistoryState {
            marker_entry_id,
            through_entry_id,
            through_position,
        },
    }
}

fn expense_effect(event: &ExpenseRecorded) -> MemberBalances {
    let mut effect = MemberBalances::default();

    for paid in event.paid_by() {
        *effect.entry(paid.member_id).or_insert(Money::ZERO) += paid.amount;
    }
    for owed in event.owed_by() {
        *effect.entry(owed.member_id).or_insert(Money::ZERO) -= owed.amount;
    }

    effect
}

fn apply_balance_delta(balances: &mut MemberBalances, delta: &MemberBalances) {
    for (member_id, amount) in delta {
        *balances.entry(*member_id).or_insert(Money::ZERO) += *amount;
    }
}

fn reverse_balance_delta(delta: &MemberBalances) -> MemberBalances {
    delta
        .iter()
        .map(|(member_id, amount)| (*member_id, -*amount))
        .collect()
}

fn register_participants(
    participants: &mut BTreeSet<MemberId>,
    members: impl IntoIterator<Item = MemberId>,
) {
    participants.extend(members);
}

fn prune_zero_balances(balances: &mut MemberBalances) {
    balances.retain(|_, amount| *amount != Money::ZERO);
}

struct VoidContext<'a> {
    entries: &'a [LedgerRecord],
    validation: &'a StructuralValidation,
    sealed_history: Option<SealedHistoryState>,
}

struct VoidState<'a> {
    voided_entry_ids: &'a mut BTreeSet<LedgerEntryId>,
    voided_expense_entry_ids: &'a mut BTreeSet<LedgerEntryId>,
    applied_entries: &'a mut BTreeMap<LedgerEntryId, AppliedEntry>,
}

fn apply_void(
    context: VoidContext<'_>,
    balances: &mut MemberBalances,
    state: VoidState<'_>,
    entry_id: LedgerEntryId,
    target: LedgerEntryId,
) -> Result<(), LedgerProjectionError> {
    let target_position = context.validation.entry_positions[&target];

    match context.entries[target_position].event {
        LedgerEvent::ExpenseRecorded(_) | LedgerEvent::NormalizedSettlementPlanRecorded(_) => {}
        LedgerEvent::LedgerHistorySealed(_) => {
            return Err(LedgerProjectionError::VoidTargetIsLedgerHistorySealed {
                entry_id,
                target,
            });
        }
        LedgerEvent::EntryVoided(_) => {
            return Err(LedgerProjectionError::VoidTargetIsVoid { entry_id, target });
        }
        LedgerEvent::BalanceAdjusted(_) => {
            return Err(LedgerProjectionError::VoidTargetIsBalanceAdjustment { entry_id, target });
        }
    }

    if context
        .sealed_history
        .is_some_and(|history| target_position <= history.through_position)
    {
        return Err(LedgerProjectionError::VoidTargetAlreadySealed { entry_id, target });
    }

    if !state.voided_entry_ids.insert(target) {
        return Err(LedgerProjectionError::DuplicateVoidTarget { entry_id, target });
    }

    let applied = state
        .applied_entries
        .get_mut(&target)
        .expect("void target was an applied expense or settlement transfer entry");
    if matches!(applied.kind, ProjectedEntryKind::Expense) {
        state.voided_expense_entry_ids.insert(target);
    }
    let reverse = reverse_balance_delta(&applied.effect);
    applied.voided = true;

    apply_balance_delta(balances, &reverse);

    Ok(())
}

fn apply_transfers(
    balances: &mut MemberBalances,
    entry_id: LedgerEntryId,
    event: &NormalizedSettlementPlanRecorded,
) -> Result<MemberBalances, LedgerProjectionError> {
    let mut next_balances = balances.clone();
    let mut effect = MemberBalances::default();

    for transfer in event.transfers() {
        validate_settlement_transfer(&next_balances, entry_id, transfer)?;
        *next_balances.entry(transfer.from).or_insert(Money::ZERO) += transfer.amount;
        *next_balances.entry(transfer.to).or_insert(Money::ZERO) -= transfer.amount;
        *effect.entry(transfer.from).or_insert(Money::ZERO) += transfer.amount;
        *effect.entry(transfer.to).or_insert(Money::ZERO) -= transfer.amount;
    }

    *balances = next_balances;

    Ok(effect)
}

fn validate_settlement_transfer(
    balances: &MemberBalances,
    entry_id: LedgerEntryId,
    transfer: &Transfer,
) -> Result<(), LedgerProjectionError> {
    let from_balance = balances.get(&transfer.from).copied().unwrap_or(Money::ZERO);
    if from_balance >= Money::ZERO {
        return Err(LedgerProjectionError::SettlementTransferWithoutDebt {
            entry_id,
            member_id: transfer.from,
            balance: from_balance,
        });
    }
    if transfer.amount > from_balance.abs() {
        return Err(LedgerProjectionError::SettlementTransferOverpaysDebt {
            entry_id,
            member_id: transfer.from,
            balance: from_balance,
            amount: transfer.amount,
        });
    }

    let to_balance = balances.get(&transfer.to).copied().unwrap_or(Money::ZERO);
    if to_balance <= Money::ZERO {
        return Err(LedgerProjectionError::SettlementTransferWithoutCredit {
            entry_id,
            member_id: transfer.to,
            balance: to_balance,
        });
    }
    if transfer.amount > to_balance {
        return Err(LedgerProjectionError::SettlementTransferOverpaysCredit {
            entry_id,
            member_id: transfer.to,
            balance: to_balance,
            amount: transfer.amount,
        });
    }

    Ok(())
}

fn apply_balance_adjustment(
    balances: &mut MemberBalances,
    context: BalanceAdjustmentContext<'_>,
    entry_id: LedgerEntryId,
    event: &BalanceAdjusted,
) -> Result<(), LedgerProjectionError> {
    let Some(sealed_history) = context.sealed_history else {
        return Err(LedgerProjectionError::BalanceAdjustmentWithoutSealedHistory { entry_id });
    };

    if let Some(related_entry) = event.related_entry() {
        let related_position = context.validation.entry_positions[&related_entry];
        if related_position > sealed_history.through_position {
            return Err(
                LedgerProjectionError::BalanceAdjustmentRelatedEntryNotSealed {
                    entry_id,
                    related_entry,
                },
            );
        }
        if context.voided_entry_ids.contains(&related_entry) {
            return Err(LedgerProjectionError::BalanceAdjustmentRelatedEntryVoided {
                entry_id,
                related_entry,
            });
        }

        let related_kind_ok = matches!(
            (event.source(), &context.entries[related_position].event),
            (
                BalanceAdjustmentSource::SealedEntryCorrection(_),
                LedgerEvent::ExpenseRecorded(_) | LedgerEvent::NormalizedSettlementPlanRecorded(_),
            ) | (
                BalanceAdjustmentSource::PriorAdjustmentCorrection(_),
                LedgerEvent::BalanceAdjusted(_),
            )
        );
        if !related_kind_ok {
            return Err(
                LedgerProjectionError::BalanceAdjustmentRelatedEntryUnsupported {
                    entry_id,
                    related_entry,
                },
            );
        }

        let related_effect = &context
            .applied_entries
            .get(&related_entry)
            .expect("related entry is a prior applied entry of a supported kind")
            .effect;

        let has_related_member = event
            .adjustments()
            .iter()
            .any(|adjustment| related_effect.contains_key(&adjustment.member_id));

        if !has_related_member {
            return Err(
                LedgerProjectionError::BalanceAdjustmentRelatedEntryUnrelated {
                    entry_id,
                    related_entry,
                },
            );
        }
    }

    for adjustment in event.adjustments() {
        *balances.entry(adjustment.member_id).or_insert(Money::ZERO) += adjustment.amount;
    }

    Ok(())
}

struct BalanceAdjustmentContext<'a> {
    entries: &'a [LedgerRecord],
    validation: &'a StructuralValidation,
    sealed_history: Option<SealedHistoryState>,
    voided_entry_ids: &'a BTreeSet<LedgerEntryId>,
    applied_entries: &'a BTreeMap<LedgerEntryId, AppliedEntry>,
}
