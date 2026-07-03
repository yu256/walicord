use crate::discord_ledger::surfaces::{
    PageSection, ReadViewData, ReadViewDocument, ReadViewDocumentState, ReadViewPageModel,
    RequiredPageSection, Section,
};
use walicord_domain::NonEmptyVec;
use walicord_i18n as i18n;

pub const READ_VIEW_ITEMS_PER_PAGE: usize = 20;

impl ReadViewDocument {
    pub fn into_pages(self) -> NonEmptyVec<ReadViewPageModel> {
        match self.state() {
            ReadViewDocumentState::Empty { empty_state } => {
                NonEmptyVec::singleton(ReadViewPageModel::empty(&self, empty_state.clone()))
            }
            ReadViewDocumentState::Populated(data) => match data {
                ReadViewData::Ledger {
                    balances,
                    recent_entries,
                } => paginate_ledger(&self, balances, recent_entries),
                ReadViewData::Review {
                    balances,
                    transfers,
                } => paginate_review(&self, balances, transfers),
            },
        }
    }
}

struct PageWindow {
    page_indicator: Option<String>,
    snapshot_notice: Option<String>,
}

fn paginate_ledger(
    doc: &ReadViewDocument,
    balances: &Section<BalanceRow>,
    recent_entries: &Section<RecentEntryRow>,
) -> NonEmptyVec<ReadViewPageModel> {
    let bal_items = section_items(balances);
    let ent_items = section_items(recent_entries);
    let bal_count = bal_items.len().max(1);
    let ent_count = ent_items.len().max(1);
    let total = bal_count + ent_count;

    if total <= READ_VIEW_ITEMS_PER_PAGE {
        return NonEmptyVec::singleton(ReadViewPageModel::ledger_page(
            doc,
            section_to_page_section(balances),
            section_to_page_section(recent_entries),
            None,
            None,
        ));
    }

    let total_pages = total.div_ceil(READ_VIEW_ITEMS_PER_PAGE);
    let first_page = ledger_page_at(
        doc,
        balances,
        recent_entries,
        bal_count,
        total,
        total_pages,
        0,
    );
    let rest = (1..total_pages).map(|i| {
        ledger_page_at(
            doc,
            balances,
            recent_entries,
            bal_count,
            total,
            total_pages,
            i,
        )
    });
    NonEmptyVec::from_first_and_rest(first_page, rest)
}

use crate::discord_ledger::surfaces::{BalanceRow, RecentEntryRow, TransferRow};

fn paginate_review(
    doc: &ReadViewDocument,
    balances: &Section<BalanceRow>,
    transfers: &NonEmptyVec<TransferRow>,
) -> NonEmptyVec<ReadViewPageModel> {
    let bal_items = section_items(balances);
    let trans_items: &[TransferRow] = transfers;
    let bal_count = bal_items.len().max(1);
    let trans_count = trans_items.len();
    let total = bal_count + trans_count;

    if total <= READ_VIEW_ITEMS_PER_PAGE {
        return NonEmptyVec::singleton(ReadViewPageModel::review_page(
            doc,
            section_to_page_section(balances),
            RequiredPageSection::Rows(transfers.clone()),
            None,
            None,
        ));
    }

    let total_pages = total.div_ceil(READ_VIEW_ITEMS_PER_PAGE);
    let first_page = review_page_at(doc, balances, transfers, bal_count, total, total_pages, 0);
    let rest = (1..total_pages)
        .map(|i| review_page_at(doc, balances, transfers, bal_count, total, total_pages, i));
    NonEmptyVec::from_first_and_rest(first_page, rest)
}

fn ledger_page_at(
    doc: &ReadViewDocument,
    balances: &Section<BalanceRow>,
    recent_entries: &Section<RecentEntryRow>,
    bal_count: usize,
    total: usize,
    total_pages: usize,
    page_index: usize,
) -> ReadViewPageModel {
    let w = page_window(page_index, total_pages);
    let (start, end) = page_bounds(page_index, total);
    ReadViewPageModel::ledger_page(
        doc,
        slice_page_section(balances, start, end, 0),
        slice_page_section(recent_entries, start, end, bal_count),
        w.page_indicator,
        w.snapshot_notice,
    )
}

fn review_page_at(
    doc: &ReadViewDocument,
    balances: &Section<BalanceRow>,
    transfers: &NonEmptyVec<TransferRow>,
    bal_count: usize,
    total: usize,
    total_pages: usize,
    page_index: usize,
) -> ReadViewPageModel {
    let w = page_window(page_index, total_pages);
    let (start, end) = page_bounds(page_index, total);
    let bal_section = slice_page_section(balances, start, end, 0);
    let transfers = slice_required_page_section(transfers, start, end, bal_count);
    ReadViewPageModel::review_page(
        doc,
        bal_section,
        transfers,
        w.page_indicator,
        w.snapshot_notice,
    )
}

fn page_window(page_index: usize, total_pages: usize) -> PageWindow {
    PageWindow {
        page_indicator: Some(i18n::page_indicator(page_index + 1, total_pages).to_string()),
        snapshot_notice: Some(i18n::SNAPSHOT_NOTICE.to_owned()),
    }
}

fn section_items<T>(section: &Section<T>) -> &[T] {
    match section {
        Section::Empty => &[],
        Section::Rows(rows) => rows,
    }
}

fn section_to_page_section<T: Clone>(section: &Section<T>) -> PageSection<T> {
    match section {
        Section::Empty => PageSection::Empty,
        Section::Rows(rows) => PageSection::Rows(rows.clone()),
    }
}

fn slice_page_section<T: Clone>(
    section: &Section<T>,
    page_start: usize,
    page_end: usize,
    section_offset: usize,
) -> PageSection<T> {
    let virtual_len = section_virtual_len(section);
    let on_this_page = page_start < (section_offset + virtual_len) && page_end > section_offset;
    if !on_this_page {
        return PageSection::Hidden;
    }

    let Section::Rows(items) = section else {
        return PageSection::Empty;
    };

    let sliced = section_slice(items, page_start, page_end, section_offset);
    match NonEmptyVec::new(sliced) {
        Ok(non_empty) => PageSection::Rows(non_empty),
        Err(_) => PageSection::Hidden,
    }
}

fn slice_required_page_section<T: Clone>(
    items: &NonEmptyVec<T>,
    page_start: usize,
    page_end: usize,
    section_offset: usize,
) -> RequiredPageSection<T> {
    let sliced = section_slice(items, page_start, page_end, section_offset);
    match NonEmptyVec::new(sliced) {
        Ok(non_empty) => RequiredPageSection::Rows(non_empty),
        Err(_) => RequiredPageSection::Hidden,
    }
}

fn section_virtual_len<T>(section: &Section<T>) -> usize {
    match section {
        Section::Empty => 1,
        Section::Rows(rows) => rows.len(),
    }
}

fn section_slice<T: Clone>(
    items: &[T],
    page_start: usize,
    page_end: usize,
    section_offset: usize,
) -> Vec<T> {
    let Some((slice_start, slice_end)) =
        section_overlap(page_start, page_end, section_offset, items.len())
    else {
        return Vec::new();
    };
    items[slice_start..slice_end].to_vec()
}

fn page_bounds(page_index: usize, total_items: usize) -> (usize, usize) {
    let page_start = page_index * READ_VIEW_ITEMS_PER_PAGE;
    let page_end = ((page_index + 1) * READ_VIEW_ITEMS_PER_PAGE).min(total_items);
    (page_start, page_end)
}

fn section_overlap(
    page_start: usize,
    page_end: usize,
    section_offset: usize,
    section_len: usize,
) -> Option<(usize, usize)> {
    if section_len == 0 {
        return None;
    }
    let section_end = section_offset + section_len;
    let overlap_start = page_start.max(section_offset);
    let overlap_end = page_end.min(section_end);
    (overlap_start < overlap_end)
        .then(|| (overlap_start - section_offset, overlap_end - section_offset))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discord_ledger::{
        BalanceDirection, BalanceRow, ExpenseOrSettlementSummary, LedgerRoute, ReadViewPageData,
        ReadViewPageState, RecentEntryRow, RecoveryReference, ReviewRoute, SafeLiteralText,
        TransferRow,
    };
    use walicord_application::ledger::{LedgerEffectiveDate, LedgerEntryId};

    fn label(raw: &str) -> SafeLiteralText {
        SafeLiteralText::from_roster_label(raw).expect("label should sanitize")
    }

    fn balance(index: usize) -> BalanceRow {
        BalanceRow {
            display_name: label(&format!("member-{index}")),
            amount: index.to_string(),
            direction: BalanceDirection::Receive,
        }
    }

    fn transfer(index: usize) -> TransferRow {
        TransferRow {
            from_display_name: label(&format!("from-{index}")),
            to_display_name: label(&format!("to-{index}")),
            amount: index.to_string(),
        }
    }

    fn recent_entry(index: usize) -> RecentEntryRow {
        RecentEntryRow {
            entry_id: LedgerEntryId(index as u64),
            summary: ExpenseOrSettlementSummary::Expense {
                date: LedgerEffectiveDate::new("2026-01-01").expect("valid date"),
                payer_display_name: label(&format!("payer-{index}")),
                amount: (index * 100).to_string(),
                note: None,
            },
            recovery_reference: RecoveryReference {
                ledger_id_short: "abcd1234".to_owned(),
                entry_id: LedgerEntryId(index as u64),
                message_link: None,
            },
        }
    }

    fn make_balances(count: usize) -> Section<BalanceRow> {
        let rows: Vec<_> = (0..count).map(balance).collect();
        match NonEmptyVec::new(rows) {
            Ok(v) => Section::Rows(v),
            Err(_) => Section::Empty,
        }
    }

    fn make_transfers(count: usize) -> NonEmptyVec<TransferRow> {
        NonEmptyVec::new((1..=count).map(transfer).collect()).expect("non-empty transfers")
    }

    fn make_recent_entries(count: usize) -> Section<RecentEntryRow> {
        let rows: Vec<_> = (0..count).map(recent_entry).collect();
        match NonEmptyVec::new(rows) {
            Ok(v) => Section::Rows(v),
            Err(_) => Section::Empty,
        }
    }

    fn review_doc(balance_count: usize, transfer_count: usize) -> ReadViewDocument {
        ReadViewDocument::review(
            ReviewRoute::Thread,
            make_balances(balance_count),
            make_transfers(transfer_count),
            false,
            crate::discord_ledger::RecoveryAction::None,
            crate::discord_ledger::ReviewSettleAction::Hidden,
        )
    }

    fn ledger_doc(balance_count: usize, entry_count: usize) -> ReadViewDocument {
        ReadViewDocument::ledger(
            LedgerRoute::Command,
            make_balances(balance_count),
            make_recent_entries(entry_count),
            false,
        )
    }

    fn page_balance_count(page: &ReadViewPageModel) -> Option<usize> {
        match page.state() {
            ReadViewPageState::Populated(ReadViewPageData::Ledger { balances, .. })
            | ReadViewPageState::Populated(ReadViewPageData::Review { balances, .. }) => {
                match balances {
                    PageSection::Hidden => None,
                    PageSection::Empty => Some(0),
                    PageSection::Rows(rows) => Some(rows.len()),
                }
            }
            _ => None,
        }
    }

    #[test]
    fn review_single_page_returns_as_is() {
        let doc = review_doc(5, 10);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 1);
        assert_eq!(page_balance_count(pages.first()), Some(5));
    }

    #[test]
    fn review_multi_page_splits_balances_then_transfers() {
        let doc = review_doc(15, 15);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 2);
        assert_eq!(page_balance_count(pages.first()), Some(15));

        let ReadViewPageState::Populated(ReadViewPageData::Review { transfers, .. }) =
            pages.first().state()
        else {
            panic!("expected Review");
        };
        let RequiredPageSection::Rows(t) = transfers else {
            panic!("expected transfers")
        };
        assert_eq!(t.len(), 5);

        assert_eq!(page_balance_count(&pages[1]), None);
        let ReadViewPageState::Populated(ReadViewPageData::Review { transfers, .. }) =
            pages[1].state()
        else {
            panic!("expected Review");
        };
        let RequiredPageSection::Rows(t) = transfers else {
            panic!("expected transfers")
        };
        assert_eq!(t.len(), 10);
    }

    #[test]
    fn ledger_single_page_returns_as_is() {
        let doc = ledger_doc(3, 10);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 1);
    }

    #[test]
    fn ledger_multi_page_splits_balances_then_entries() {
        let doc = ledger_doc(5, 25);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 2);
        assert_eq!(page_balance_count(pages.first()), Some(5));
        let ReadViewPageState::Populated(ReadViewPageData::Ledger { recent_entries, .. }) =
            pages.first().state()
        else {
            panic!("expected Ledger");
        };
        let PageSection::Rows(e) = recent_entries else {
            panic!("expected entries")
        };
        assert_eq!(e.len(), 15);

        assert_eq!(page_balance_count(&pages[1]), None);
        let ReadViewPageState::Populated(ReadViewPageData::Ledger { recent_entries, .. }) =
            pages[1].state()
        else {
            panic!("expected Ledger");
        };
        let PageSection::Rows(e) = recent_entries else {
            panic!("expected entries")
        };
        assert_eq!(e.len(), 10);
    }

    #[test]
    fn empty_state_skips_pagination() {
        let doc = ReadViewDocument::ledger_empty(LedgerRoute::Command, false);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 1);
        assert!(matches!(
            pages.first().state(),
            ReadViewPageState::Empty { .. }
        ));
    }

    #[test]
    fn ledger_zero_balances_zero_entries_single_page() {
        let doc =
            ReadViewDocument::ledger(LedgerRoute::Command, Section::Empty, Section::Empty, false);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 1);
        assert!(matches!(
            pages.first().state(),
            ReadViewPageState::Empty { .. }
        ));
    }

    #[test]
    fn ledger_one_balance_one_entry_single_page() {
        let doc = ledger_doc(1, 1);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 1);
        assert_eq!(page_balance_count(pages.first()), Some(1));
    }

    #[test]
    fn ledger_exactly_20_items_single_page() {
        let doc = ledger_doc(10, 10);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 1);
    }

    #[test]
    fn ledger_21_items_splits_into_two_pages() {
        let doc = ledger_doc(10, 11);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 2);
        assert!(pages.first().page_indicator().is_some());
        assert!(pages[1].page_indicator().is_some());
    }

    #[test]
    fn review_exactly_20_items_single_page() {
        let doc = review_doc(10, 10);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 1);
        assert!(pages.first().page_indicator().is_none());
    }

    #[test]
    fn review_21_items_splits_into_two_pages() {
        let doc = review_doc(10, 11);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 2);
    }

    #[test]
    fn ledger_empty_balances_with_entries_uses_virtual_slot() {
        let doc = ledger_doc(0, 20);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 2);
        assert_eq!(page_balance_count(pages.first()), Some(0));
        assert_eq!(page_balance_count(&pages[1]), None);
    }

    #[test]
    fn ledger_empty_recent_entries_uses_virtual_slot() {
        let doc = ledger_doc(20, 0);
        let pages = doc.into_pages();
        assert_eq!(pages.len(), 2);

        let ReadViewPageState::Populated(ReadViewPageData::Ledger { recent_entries, .. }) =
            pages[1].state()
        else {
            panic!("expected Ledger");
        };
        assert_eq!(recent_entries, &PageSection::Empty);
    }

    #[test]
    fn stale_ledger_page_is_constructed_without_document_state() {
        let page = ReadViewPageModel::stale_ledger(LedgerRoute::Panel, true);
        assert!(matches!(
            page.state(),
            ReadViewPageState::Stale {
                missing_thread_note: true
            }
        ));
    }
}
