use walicord_ledger::{LedgerHistorySealed, ProjectedLedger};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SealThroughTailError {
    /// The projection has no entries at all to seal through.
    EmptyLedger,
    /// The selected tail is already inside the existing sealed range, so there is no new
    /// range to seal. Only `seal_through_latest_unvoided_expense_or_settlement_entry` can
    /// produce this; ordinary `seal_through_tail` always advances because the tail itself
    /// is by definition not inside the prior sealed range.
    AlreadySealed,
}

/// Builds a `LedgerHistorySealed` event targeting the actual append tail of the supplied
/// projection — the last entry of any kind, including markers (history seal, void, balance
/// adjustment) and voided expense/settlement transfers. This matches the doc-stated
/// ordinary flow ("seal through the current last append entry"): a balance adjustment or
/// void recorded after the previous seal becomes part of the next sealed range. The core
/// model permits sealing through any prior ledger position, but ordinary seal/confirm flows
/// should always advance to the current tail; this helper hides that policy from callers.
///
/// Calling this repeatedly with no new entries will keep producing seal markers that seal
/// the previous seal marker — that is a feature, not a bug, because the helper is purely
/// mechanical. Use [`seal_through_latest_unvoided_expense_or_settlement_entry`] instead
/// when the caller wants to refuse a re-seal that has no new business entries to cover.
pub fn seal_through_tail(
    projected: &ProjectedLedger,
) -> Result<LedgerHistorySealed, SealThroughTailError> {
    let tail = projected
        .entry_index()
        .latest_append_entry()
        .ok_or(SealThroughTailError::EmptyLedger)?;

    Ok(LedgerHistorySealed::new(tail.entry_id))
}

/// Idempotent variant of [`seal_through_tail`] for ordinary user-facing operations: like
/// `seal_through_tail`, it considers the entire append log (markers, adjustments, voids
/// included), but refuses with `AlreadySealed` when the most recent seal marker *is* the
/// current append tail — that is, nothing of any kind has been appended since the last
/// seal. This makes "user clicks seal twice" a safe no-op rather than producing a chain of
/// seal-the-seal-marker entries.
///
/// Use [`seal_through_tail`] when you want the purely mechanical behavior (always advance,
/// even if it just chains seal markers) — for example in batch / scripted seal flows. Use
/// [`seal_through_latest_unvoided_expense_or_settlement_entry`] when you specifically want
/// to refuse a re-seal unless a new business event (expense / settlement transfer) has
/// appeared since the last seal — markers and balance adjustments alone are not enough.
pub fn seal_through_tail_if_advances(
    projected: &ProjectedLedger,
) -> Result<LedgerHistorySealed, SealThroughTailError> {
    let tail = projected
        .entry_index()
        .latest_append_entry()
        .ok_or(SealThroughTailError::EmptyLedger)?;

    if let Some(existing) = projected.state().sealed_history()
        && existing.marker_entry_id == tail.entry_id
    {
        return Err(SealThroughTailError::AlreadySealed);
    }

    Ok(LedgerHistorySealed::new(tail.entry_id))
}

/// Variant of [`seal_through_tail`] that targets the latest non-voided expense or
/// settlement transfer, skipping markers and voided entries. Reserved for callers that
/// specifically want to refuse a re-seal when no new business event has been appended
/// since the last seal — `BalanceAdjustment` entries also affect balances but are
/// audit-time corrections, not new business activity, so this helper does not anchor on
/// them. If the existing seal range already covers the latest expense or settlement
/// transfer, there is nothing new to seal.
pub fn seal_through_latest_unvoided_expense_or_settlement_entry(
    projected: &ProjectedLedger,
) -> Result<LedgerHistorySealed, SealThroughTailError> {
    let tail = projected
        .entry_index()
        .latest_unvoided_expense_or_settlement_entry()
        .ok_or(SealThroughTailError::EmptyLedger)?;

    let tail_info = projected
        .entry(tail.entry_id)
        .expect("latest unvoided expense or settlement entry must be in the entry index");
    if tail_info.sealed {
        return Err(SealThroughTailError::AlreadySealed);
    }

    Ok(LedgerHistorySealed::new(tail.entry_id))
}
