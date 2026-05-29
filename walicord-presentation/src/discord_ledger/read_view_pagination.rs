//! Pagination of the read-view surface model. The pre-existing
//! `paginate_read_view_model` lived in the Discord adapter even though it operated
//! purely on presentation types (`ReadViewPageModel`, `BalanceRow`, `TransferRow`,
//! etc.) — no I/O, no application concepts, no Discord-side identifiers. Lives in
//! the presentation crate now so any read-view surface caller can paginate without
//! reaching across crate boundaries.

use crate::discord_ledger::{
    BalanceAdjustmentSummary, ReadViewKind, ReadViewPageModel, SealedRangeSummary,
};
use walicord_i18n as i18n;

/// Items per page for the read-view (`/review`, `/ledger`) surface. The constant was
/// historically in the adapter; it's a presentation concern (how big a page is for
/// Discord's message-size budget) so it moves with the pagination logic.
pub const READ_VIEW_ITEMS_PER_PAGE: usize = 20;

/// Split a fully-composed read-view surface model into pages, attaching the page
/// indicator and snapshot notice each page needs. Empty-state pages and pages that
/// fit in one page short-circuit to a single-page output.
pub fn paginate_read_view_model(model: ReadViewPageModel) -> Vec<ReadViewPageModel> {
    if model.empty_state.is_some() {
        return vec![model];
    }
    match model.kind {
        ReadViewKind::Review => paginate_review_model(model),
        ReadViewKind::Ledger => paginate_ledger_model(model),
    }
}

fn paginate_review_model(model: ReadViewPageModel) -> Vec<ReadViewPageModel> {
    let total_items = model.balances.len() + model.transfers.len();
    if total_items <= READ_VIEW_ITEMS_PER_PAGE {
        return vec![model];
    }

    let total_pages = total_items.div_ceil(READ_VIEW_ITEMS_PER_PAGE);
    (0..total_pages)
        .map(|page_index| {
            let (page_start, page_end) = page_bounds(page_index, total_items);
            let mut page = model.clone();
            page.page_indicator =
                Some(i18n::page_indicator(page_index + 1, total_pages).to_string());
            page.snapshot_notice = Some(i18n::snapshot_notice().to_owned());
            page.balances = slice_section(&model.balances, page_start, page_end, 0);
            page.transfers =
                slice_section(&model.transfers, page_start, page_end, model.balances.len());
            page.visible_sections.balances = !page.balances.is_empty();
            page.visible_sections.transfers = !page.transfers.is_empty();
            page.visible_sections.participants = false;
            page.visible_sections.voided_entries = false;
            page.visible_sections.confirmed = false;
            page
        })
        .collect()
}

fn paginate_ledger_model(model: ReadViewPageModel) -> Vec<ReadViewPageModel> {
    let balances_count = ledger_required_section_item_count(&model.balances);
    let voided_count = ledger_required_section_item_count(&model.voided_entries);
    let confirmed_count = confirmed_section_item_count(&model);
    let total_items = balances_count + model.participants.len() + voided_count + confirmed_count;
    if total_items <= READ_VIEW_ITEMS_PER_PAGE {
        return vec![model];
    }

    let total_pages = total_items.div_ceil(READ_VIEW_ITEMS_PER_PAGE);
    let participants_offset = balances_count;
    let voided_offset = participants_offset + model.participants.len();
    let confirmed_offset = voided_offset + voided_count;
    (0..total_pages)
        .map(|page_index| {
            let (page_start, page_end) = page_bounds(page_index, total_items);
            let mut page = model.clone();
            page.page_indicator =
                Some(i18n::page_indicator(page_index + 1, total_pages).to_string());
            page.snapshot_notice = Some(i18n::snapshot_notice().to_owned());
            page.balances = slice_section(&model.balances, page_start, page_end, 0);
            page.participants = slice_section(
                &model.participants,
                page_start,
                page_end,
                participants_offset,
            );
            page.voided_entries =
                slice_section(&model.voided_entries, page_start, page_end, voided_offset);
            let (sealed_range, balance_adjustments) =
                slice_confirmed_section(&model, page_start, page_end, confirmed_offset);
            page.sealed_range = sealed_range;
            page.balance_adjustments = balance_adjustments;
            page.visible_sections.balances =
                section_visible(page_start, page_end, 0, balances_count);
            page.visible_sections.transfers = false;
            page.visible_sections.participants = !page.participants.is_empty();
            page.visible_sections.voided_entries =
                section_visible(page_start, page_end, voided_offset, voided_count);
            page.visible_sections.confirmed =
                section_visible(page_start, page_end, confirmed_offset, confirmed_count);
            page
        })
        .collect()
}

fn slice_confirmed_section(
    model: &ReadViewPageModel,
    page_start: usize,
    page_end: usize,
    confirmed_offset: usize,
) -> (Option<SealedRangeSummary>, Vec<BalanceAdjustmentSummary>) {
    let confirmed_count = confirmed_section_item_count(model);
    let Some((slice_start, slice_end)) =
        section_overlap(page_start, page_end, confirmed_offset, confirmed_count)
    else {
        return (None, Vec::new());
    };
    if model.sealed_range.is_none() {
        let adjustment_start = slice_start.min(model.balance_adjustments.len());
        let adjustment_end = slice_end.min(model.balance_adjustments.len());
        return (
            None,
            model.balance_adjustments[adjustment_start..adjustment_end].to_vec(),
        );
    }
    let sealed_range = (slice_start == 0)
        .then(|| model.sealed_range.clone())
        .flatten();
    let adjustment_start = slice_start.saturating_sub(1);
    let adjustment_end = slice_end
        .saturating_sub(1)
        .min(model.balance_adjustments.len());
    (
        sealed_range,
        model.balance_adjustments[adjustment_start..adjustment_end].to_vec(),
    )
}

fn page_bounds(page_index: usize, total_items: usize) -> (usize, usize) {
    let page_start = page_index * READ_VIEW_ITEMS_PER_PAGE;
    let page_end = ((page_index + 1) * READ_VIEW_ITEMS_PER_PAGE).min(total_items);
    (page_start, page_end)
}

fn ledger_required_section_item_count<T>(items: &[T]) -> usize {
    items.len().max(1)
}

fn confirmed_section_item_count(model: &ReadViewPageModel) -> usize {
    (usize::from(model.sealed_range.is_some()) + model.balance_adjustments.len()).max(1)
}

fn section_visible(
    page_start: usize,
    page_end: usize,
    section_offset: usize,
    section_count: usize,
) -> bool {
    section_overlap(page_start, page_end, section_offset, section_count).is_some()
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
        BalanceDirection, BalanceRow, ReadViewKind, ReadViewPageModel, ReadViewRoute,
        ReadViewSectionVisibility, RecoveryCta, SafeLiteralText, TransferRow,
    };

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

    #[test]
    fn paginate_read_view_model_splits_review_rows_into_snapshot_bound_pages() {
        let pages = paginate_read_view_model(ReadViewPageModel {
            kind: ReadViewKind::Review,
            route: ReadViewRoute::ReviewThread,
            title: "清算確認".to_owned(),
            uncertain_write: false,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::ParentLink,
            recovery_url: Some("https://discord.com/channels/1/2".to_owned()),
            missing_thread_note: false,
            balances: (0..18).map(balance).collect(),
            transfers: (0..4).map(transfer).collect(),
            participants: Vec::new(),
            voided_entries: Vec::new(),
            sealed_range: None,
            balance_adjustments: Vec::new(),
            footer_lines: Vec::new(),
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        });

        assert_eq!(pages.len(), 2);
        assert_eq!(pages[0].page_indicator.as_deref(), Some("ページ 1/2"));
        assert_eq!(
            pages[0].snapshot_notice.as_deref(),
            Some(i18n::snapshot_notice())
        );
        assert_eq!(pages[0].balances.len(), 18);
        assert_eq!(pages[0].transfers.len(), 2);
        assert!(pages[0].visible_sections.balances);
        assert!(pages[0].visible_sections.transfers);
        assert_eq!(pages[1].balances.len(), 0);
        assert_eq!(pages[1].transfers.len(), 2);
        assert!(!pages[1].visible_sections.balances);
        assert!(pages[1].visible_sections.transfers);
    }

    #[test]
    fn paginate_read_view_model_paginates_ledger_as_one_whole_view_document() {
        let pages = paginate_read_view_model(ReadViewPageModel {
            kind: ReadViewKind::Ledger,
            route: ReadViewRoute::LedgerCommand,
            title: "台帳".to_owned(),
            uncertain_write: false,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            balances: (0..20).map(balance).collect(),
            transfers: Vec::new(),
            participants: vec![label("Alice"), label("Bob")],
            voided_entries: Vec::new(),
            sealed_range: None,
            balance_adjustments: Vec::new(),
            footer_lines: Vec::new(),
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        });

        assert_eq!(pages.len(), 2);
        assert_eq!(pages[0].page_indicator.as_deref(), Some("ページ 1/2"));
        assert_eq!(pages[0].balances.len(), 20);
        assert!(pages[0].visible_sections.balances);
        assert!(!pages[0].visible_sections.participants);
        assert_eq!(pages[1].balances.len(), 0);
        assert_eq!(pages[1].participants.len(), 2);
        assert!(!pages[1].visible_sections.balances);
        assert!(pages[1].visible_sections.participants);
        assert!(pages[1].visible_sections.voided_entries);
        assert!(pages[1].visible_sections.confirmed);
        assert_eq!(
            pages[1].snapshot_notice.as_deref(),
            Some(i18n::snapshot_notice())
        );
    }

    #[test]
    fn paginate_read_view_model_keeps_required_empty_ledger_sections_on_their_pages() {
        let pages = paginate_read_view_model(ReadViewPageModel {
            kind: ReadViewKind::Ledger,
            route: ReadViewRoute::LedgerCommand,
            title: "台帳".to_owned(),
            uncertain_write: false,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            route_guidance_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            balances: Vec::new(),
            transfers: Vec::new(),
            participants: (0..25)
                .map(|index| label(&format!("member-{index:02}")))
                .collect(),
            voided_entries: Vec::new(),
            sealed_range: None,
            balance_adjustments: Vec::new(),
            footer_lines: Vec::new(),
            visible_sections: ReadViewSectionVisibility::default(),
            empty_state: None,
            action_rows: Vec::new(),
            ephemeral: true,
        });

        assert_eq!(pages.len(), 2);
        assert!(pages[0].visible_sections.balances);
        assert!(pages[0].visible_sections.participants);
        assert!(!pages[0].visible_sections.voided_entries);
        assert!(!pages[0].visible_sections.confirmed);
        assert!(pages[1].visible_sections.participants);
        assert!(pages[1].visible_sections.voided_entries);
        assert!(pages[1].visible_sections.confirmed);
    }
}
