use crate::discord_ledger::{ReadViewContent, ReadViewPageModel};
use walicord_i18n as i18n;

pub const READ_VIEW_ITEMS_PER_PAGE: usize = 20;

pub fn paginate_read_view_model(model: ReadViewPageModel) -> Vec<ReadViewPageModel> {
    if model.empty_state.is_some() {
        return vec![model];
    }
    match &model.content {
        ReadViewContent::Review { .. } => paginate_review_model(model),
        ReadViewContent::Ledger { .. } => paginate_ledger_model(model),
    }
}

fn paginate_review_model(model: ReadViewPageModel) -> Vec<ReadViewPageModel> {
    let ReadViewContent::Review {
        transfers: Some(ref transfers),
    } = model.content
    else {
        return vec![model];
    };
    let balances = model.balances.as_deref().unwrap_or_default();
    let balances_item_count = balances.len().max(1);
    let total_items = balances_item_count + transfers.len();
    if total_items <= READ_VIEW_ITEMS_PER_PAGE {
        return vec![model];
    }

    let total_pages = total_items.div_ceil(READ_VIEW_ITEMS_PER_PAGE);
    let transfers_offset = balances_item_count;
    (0..total_pages)
        .map(|page_index| {
            let (page_start, page_end) = page_bounds(page_index, total_items);
            let mut page = model.clone();
            page.page_indicator =
                Some(i18n::page_indicator(page_index + 1, total_pages).to_string());
            page.snapshot_notice = Some(i18n::SNAPSHOT_NOTICE.to_owned());
            let sliced_balances = slice_section(balances, page_start, page_end, 0);
            let balances_on_this_page = page_start < balances_item_count && page_end > 0;
            page.balances = if balances_on_this_page {
                Some(sliced_balances)
            } else {
                None
            };
            let ReadViewContent::Review {
                transfers: Some(ref transfers),
            } = model.content
            else {
                unreachable!()
            };
            let sliced_transfers = slice_section(transfers, page_start, page_end, transfers_offset);
            page.content = ReadViewContent::Review {
                transfers: if sliced_transfers.is_empty() {
                    None
                } else {
                    Some(sliced_transfers)
                },
            };
            page
        })
        .collect()
}

fn paginate_ledger_model(model: ReadViewPageModel) -> Vec<ReadViewPageModel> {
    let ReadViewContent::Ledger {
        recent_entries: Some(ref recent_entries),
    } = model.content
    else {
        return vec![model];
    };
    let balances = model.balances.as_deref().unwrap_or_default();
    let balances_item_count = balances.len().max(1);
    let total_items = balances_item_count + recent_entries.len();
    if total_items <= READ_VIEW_ITEMS_PER_PAGE {
        return vec![model];
    }

    let total_pages = total_items.div_ceil(READ_VIEW_ITEMS_PER_PAGE);
    let entries_offset = balances_item_count;
    (0..total_pages)
        .map(|page_index| {
            let (page_start, page_end) = page_bounds(page_index, total_items);
            let mut page = model.clone();
            page.page_indicator =
                Some(i18n::page_indicator(page_index + 1, total_pages).to_string());
            page.snapshot_notice = Some(i18n::SNAPSHOT_NOTICE.to_owned());
            let sliced_balances = slice_section(balances, page_start, page_end, 0);
            let balances_on_this_page = page_start < balances_item_count && page_end > 0;
            page.balances = if balances_on_this_page {
                Some(sliced_balances)
            } else {
                None
            };
            let ReadViewContent::Ledger {
                recent_entries: Some(ref recent_entries),
            } = model.content
            else {
                unreachable!()
            };
            let sliced_entries =
                slice_section(recent_entries, page_start, page_end, entries_offset);
            page.content = ReadViewContent::Ledger {
                recent_entries: if sliced_entries.is_empty() {
                    None
                } else {
                    Some(sliced_entries)
                },
            };
            page
        })
        .collect()
}

fn page_bounds(page_index: usize, total_items: usize) -> (usize, usize) {
    let page_start = page_index * READ_VIEW_ITEMS_PER_PAGE;
    let page_end = ((page_index + 1) * READ_VIEW_ITEMS_PER_PAGE).min(total_items);
    (page_start, page_end)
}

fn slice_section<T: Clone>(
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
        BalanceDirection, BalanceRow, ExpenseOrSettlementSummary, ReadViewContent,
        ReadViewPageModel, ReadViewRoute, RecentEntryRow, RecoveryCta, RecoveryReference,
        SafeLiteralText, TransferRow,
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

    fn review_model(balances: usize, transfers: usize) -> ReadViewPageModel {
        ReadViewPageModel {
            route: ReadViewRoute::ReviewThread,
            title: std::borrow::Cow::Borrowed("review"),
            uncertain_write: false,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            balances: Some((0..balances).map(balance).collect()),
            footer_lines: Vec::new(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
            content: ReadViewContent::Review {
                transfers: Some((0..transfers).map(transfer).collect()),
            },
        }
    }

    fn ledger_model(balances: usize, entries: usize) -> ReadViewPageModel {
        ReadViewPageModel {
            route: ReadViewRoute::LedgerCommand,
            title: std::borrow::Cow::Borrowed("ledger"),
            uncertain_write: false,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            balances: Some((0..balances).map(balance).collect()),
            footer_lines: Vec::new(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
            content: ReadViewContent::Ledger {
                recent_entries: Some((0..entries).map(recent_entry).collect()),
            },
        }
    }

    #[test]
    fn review_single_page_returns_as_is() {
        let model = review_model(5, 10);
        let pages = paginate_read_view_model(model);
        assert_eq!(pages.len(), 1);
        assert_eq!(pages[0].balances.as_ref().unwrap().len(), 5);
    }

    #[test]
    fn review_multi_page_splits_balances_then_transfers() {
        let model = review_model(15, 15);
        let pages = paginate_read_view_model(model);
        assert_eq!(pages.len(), 2);
        assert_eq!(pages[0].balances.as_ref().unwrap().len(), 15);
        let ReadViewContent::Review {
            transfers: Some(ref transfers),
        } = pages[0].content
        else {
            panic!("expected Review content with transfers");
        };
        assert_eq!(transfers.len(), 5);
        assert!(pages[1].balances.is_none());
        let ReadViewContent::Review {
            transfers: Some(ref transfers),
        } = pages[1].content
        else {
            panic!("expected Review content with transfers");
        };
        assert_eq!(transfers.len(), 10);
    }

    #[test]
    fn ledger_single_page_returns_as_is() {
        let model = ledger_model(3, 10);
        let pages = paginate_read_view_model(model);
        assert_eq!(pages.len(), 1);
    }

    #[test]
    fn ledger_multi_page_splits_balances_then_entries() {
        let model = ledger_model(5, 25);
        let pages = paginate_read_view_model(model);
        assert_eq!(pages.len(), 2);
        assert_eq!(pages[0].balances.as_ref().unwrap().len(), 5);
        let ReadViewContent::Ledger {
            recent_entries: Some(ref entries),
        } = pages[0].content
        else {
            panic!("expected Ledger content with entries");
        };
        assert_eq!(entries.len(), 15);
        assert!(pages[1].balances.is_none());
        let ReadViewContent::Ledger {
            recent_entries: Some(ref entries),
        } = pages[1].content
        else {
            panic!("expected Ledger content with entries");
        };
        assert_eq!(entries.len(), 10);
    }

    #[test]
    fn empty_state_skips_pagination() {
        let mut model = ledger_model(0, 0);
        model.empty_state = Some(std::borrow::Cow::Borrowed("empty"));
        let pages = paginate_read_view_model(model);
        assert_eq!(pages.len(), 1);
    }
}
