use super::{
    codec::AttachmentCodecError,
    store::{StoreLoadError, VerifiedLedgerThreadLoad},
};
use std::time::SystemTime;
use walicord_application::ledger::{
    LedgerEntry, LedgerEntryId, ProjectedEntryInfo, ProjectedEntryKind,
};
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    BalanceAdjustmentSummary, ReadViewKind, ReadViewPageModel, SealedRangeSummary,
};

pub const UNKNOWN_LEDGER_FORMAT_EVENT: &str = "ledger_unknown_variant";
pub const READ_VIEW_ITEMS_PER_PAGE: usize = 20;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalLoadRoute {
    Read,
    Preview,
    Refresh,
    WritePrelude,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalLoadFailure {
    Fetch {
        route: CanonicalLoadRoute,
    },
    FetchTimeout {
        route: CanonicalLoadRoute,
    },
    AttachmentCardinality {
        route: CanonicalLoadRoute,
    },
    OversizeAttachment {
        route: CanonicalLoadRoute,
    },
    Decode {
        route: CanonicalLoadRoute,
    },
    VersionMismatch {
        route: CanonicalLoadRoute,
        failing_entry_id: Option<LedgerEntryId>,
    },
    WriterLineage {
        route: CanonicalLoadRoute,
    },
    Chain {
        route: CanonicalLoadRoute,
    },
    Structure {
        route: CanonicalLoadRoute,
    },
    Projection {
        route: CanonicalLoadRoute,
    },
    MetadataCoherence {
        route: CanonicalLoadRoute,
    },
    DisplayDrift {
        route: CanonicalLoadRoute,
    },
    Permission {
        route: CanonicalLoadRoute,
    },
}

impl CanonicalLoadFailure {
    pub fn from_store_error(route: CanonicalLoadRoute, error: &StoreLoadError) -> Self {
        match error {
            StoreLoadError::Fetch(_) => Self::Fetch { route },
            StoreLoadError::FetchTimeout { .. } => Self::FetchTimeout { route },
            StoreLoadError::AttachmentCardinality { .. } => Self::AttachmentCardinality { route },
            StoreLoadError::OversizeAttachment { .. } => Self::OversizeAttachment { route },
            StoreLoadError::Decode {
                error: AttachmentCodecError::InvalidHashSuite(_),
                ..
            } => Self::Chain { route },
            StoreLoadError::Decode { error, .. } if is_schema_or_event_version_mismatch(error) => {
                Self::VersionMismatch {
                    route,
                    failing_entry_id: error.failing_entry_id(),
                }
            }
            StoreLoadError::Decode { .. } => Self::Decode { route },
            StoreLoadError::WriterLineage(_) => Self::WriterLineage { route },
            StoreLoadError::Chain(_) => Self::Chain { route },
            StoreLoadError::Structure(_) => Self::Structure { route },
            StoreLoadError::Projection(_) => Self::Projection { route },
            StoreLoadError::MetadataCoherence(_) => Self::MetadataCoherence { route },
            StoreLoadError::DisplayDrift { .. } => Self::DisplayDrift { route },
            StoreLoadError::Permission(_) => Self::Permission { route },
        }
    }

    pub fn route(self) -> CanonicalLoadRoute {
        match self {
            Self::Fetch { route }
            | Self::FetchTimeout { route }
            | Self::AttachmentCardinality { route }
            | Self::OversizeAttachment { route }
            | Self::Decode { route }
            | Self::VersionMismatch { route, .. }
            | Self::WriterLineage { route }
            | Self::Chain { route }
            | Self::Structure { route }
            | Self::Projection { route }
            | Self::MetadataCoherence { route }
            | Self::DisplayDrift { route }
            | Self::Permission { route } => route,
        }
    }

    pub fn failing_entry_id(self) -> Option<LedgerEntryId> {
        match self {
            Self::VersionMismatch {
                failing_entry_id, ..
            } => failing_entry_id,
            _ => None,
        }
    }

    pub fn user_message(self) -> Option<&'static str> {
        match self {
            Self::VersionMismatch { .. } => Some(i18n::unknown_ledger_format_message()),
            Self::Fetch { route } => Some(match route {
                CanonicalLoadRoute::WritePrelude => i18n::ledger_thread_prepare_failed_message(),
                CanonicalLoadRoute::Read
                | CanonicalLoadRoute::Preview
                | CanonicalLoadRoute::Refresh => i18n::ledger_retryable_load_message(),
            }),
            Self::FetchTimeout { route } => Some(match route {
                CanonicalLoadRoute::WritePrelude => i18n::ledger_thread_prepare_failed_message(),
                CanonicalLoadRoute::Read
                | CanonicalLoadRoute::Preview
                | CanonicalLoadRoute::Refresh => i18n::ledger_load_timeout_message(),
            }),
            Self::Permission { route } => Some(match route {
                CanonicalLoadRoute::WritePrelude => i18n::ledger_thread_prepare_failed_message(),
                CanonicalLoadRoute::Read
                | CanonicalLoadRoute::Preview
                | CanonicalLoadRoute::Refresh => i18n::ledger_permission_failed_message(),
            }),
            Self::AttachmentCardinality { .. }
            | Self::OversizeAttachment { .. }
            | Self::Decode { .. } => Some(i18n::ledger_attachment_decode_failed_message()),
            Self::WriterLineage { .. }
            | Self::Chain { .. }
            | Self::Structure { .. }
            | Self::Projection { .. }
            | Self::MetadataCoherence { .. }
            | Self::DisplayDrift { .. } => Some(i18n::ledger_integrity_failed_message()),
        }
    }

    pub fn observability_event(self) -> Option<&'static str> {
        match self {
            Self::VersionMismatch { .. } => Some(UNKNOWN_LEDGER_FORMAT_EVENT),
            _ => None,
        }
    }
}

impl CanonicalLoadRoute {
    pub fn label(self) -> &'static str {
        match self {
            Self::Read => "read",
            Self::Preview => "preview",
            Self::Refresh => "refresh",
            Self::WritePrelude => "write_prelude",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ProjectionConsistencyError {
    #[error("replayed entry {entry_id:?} is missing projected metadata")]
    MissingProjectedEntry { entry_id: LedgerEntryId },
    #[error("replayed entry {entry_id:?} is missing transport metadata")]
    MissingTransport { entry_id: LedgerEntryId },
}

#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedLedgerEntryView {
    entry: LedgerEntry,
    projected: ProjectedEntryInfo,
    message_link: String,
    recorded_at: SystemTime,
}

impl VerifiedLedgerEntryView {
    pub fn entry(&self) -> &LedgerEntry {
        &self.entry
    }

    pub fn projected(&self) -> &ProjectedEntryInfo {
        &self.projected
    }

    pub fn message_link(&self) -> &str {
        &self.message_link
    }

    pub fn recorded_at(&self) -> SystemTime {
        self.recorded_at
    }
}

pub fn project_verified_entries(
    load: &VerifiedLedgerThreadLoad,
) -> Result<Vec<VerifiedLedgerEntryView>, ProjectionConsistencyError> {
    let projected = load.snapshot().projected();
    let mut out = Vec::with_capacity(load.verified().len());

    for envelope in load.verified() {
        let entry = envelope.payload().entry.clone();
        let entry_id = entry.id;
        let projected_entry = projected
            .entry(entry_id)
            .cloned()
            .ok_or(ProjectionConsistencyError::MissingProjectedEntry { entry_id })?;
        let transport = load
            .transport_index()
            .get(entry_id)
            .ok_or(ProjectionConsistencyError::MissingTransport { entry_id })?;
        out.push(VerifiedLedgerEntryView {
            recorded_at: entry
                .metadata
                .recorded_at
                .unwrap_or_else(|| transport.recorded_at()),
            entry,
            projected: projected_entry,
            message_link: transport.message_link().to_owned(),
        });
    }

    Ok(out)
}

pub fn project_recent_voidable_entries(
    load: &VerifiedLedgerThreadLoad,
    limit: usize,
) -> Result<Vec<VerifiedLedgerEntryView>, ProjectionConsistencyError> {
    let all = project_verified_entries(load)?;
    let mut candidates = all
        .into_iter()
        .filter(|entry| {
            matches!(
                entry.projected().kind,
                ProjectedEntryKind::Expense | ProjectedEntryKind::SettlementTransfer
            ) && !entry.projected().voided
                && !entry.projected().sealed
        })
        .collect::<Vec<_>>();
    candidates.reverse();
    candidates.truncate(limit);
    Ok(candidates)
}

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

fn is_schema_or_event_version_mismatch(error: &AttachmentCodecError) -> bool {
    matches!(
        error,
        AttachmentCodecError::UnknownSchemaVersion { .. }
            | AttachmentCodecError::UnknownEventVariant { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discord::ledger::store::verified_thread_load_for_test;
    use serenity::all::MessageId;
    use walicord_application::ledger::{
        AllocationSnapshot, EntryVoided, ExpenseNote, ExpenseRecorded, LedgerEntry, LedgerEntryId,
        LedgerHistorySealed, MemberAmount, NormalizedSettlementPlanRecorded,
    };
    use walicord_domain::{Money, Transfer, model::MemberId};
    use walicord_presentation::{
        SafeLiteralText,
        discord_ledger::{
            BalanceDirection, BalanceRow, ReadViewRoute, ReadViewSectionVisibility, RecoveryCta,
            TransferRow,
        },
    };

    fn expense_entry(id: u64, payer: u64, owed_by: &[(u64, i64)]) -> LedgerEntry {
        LedgerEntry::expense(
            LedgerEntryId(id),
            ExpenseRecorded::new(
                vec![MemberAmount {
                    member_id: MemberId(payer),
                    amount: Money::from_i64(owed_by.iter().map(|(_, amount)| amount).sum()),
                }],
                owed_by
                    .iter()
                    .map(|(member_id, amount)| MemberAmount {
                        member_id: MemberId(*member_id),
                        amount: Money::from_i64(*amount),
                    })
                    .collect(),
                Some(ExpenseNote::new("fixture").expect("note should parse")),
            )
            .expect("expense should build"),
            AllocationSnapshot::Even,
        )
        .expect("entry should build")
    }

    fn settlement_entry(id: u64, from: u64, to: u64, amount: i64) -> LedgerEntry {
        LedgerEntry::non_expense(
            LedgerEntryId(id),
            NormalizedSettlementPlanRecorded::new(vec![Transfer {
                from: MemberId(from),
                to: MemberId(to),
                amount: Money::from_i64(amount),
            }])
            .expect("settlement should build"),
        )
    }

    fn void_entry(id: u64, target: u64) -> LedgerEntry {
        LedgerEntry::non_expense(LedgerEntryId(id), EntryVoided::new(LedgerEntryId(target)))
    }

    fn load(entries: Vec<LedgerEntry>) -> VerifiedLedgerThreadLoad {
        verified_thread_load_for_test(entries)
    }

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
    fn canonical_load_failure_maps_unknown_versions_to_update_required_outcome() {
        let actual = CanonicalLoadFailure::from_store_error(
            CanonicalLoadRoute::Preview,
            &StoreLoadError::Decode {
                message_id: MessageId::new(1),
                error: AttachmentCodecError::UnknownSchemaVersion {
                    version: 2,
                    entry_id: Some(LedgerEntryId(1)),
                },
            },
        );

        assert_eq!(
            actual,
            CanonicalLoadFailure::VersionMismatch {
                route: CanonicalLoadRoute::Preview,
                failing_entry_id: Some(LedgerEntryId(1)),
            }
        );
        assert_eq!(
            actual.user_message(),
            Some(i18n::unknown_ledger_format_message())
        );
        assert_eq!(
            actual.observability_event(),
            Some(UNKNOWN_LEDGER_FORMAT_EVENT)
        );
        assert_eq!(actual.failing_entry_id(), Some(LedgerEntryId(1)));
    }

    #[test]
    fn canonical_load_failure_maps_unknown_event_variants_to_update_required_outcome() {
        let actual = CanonicalLoadFailure::from_store_error(
            CanonicalLoadRoute::Read,
            &StoreLoadError::Decode {
                message_id: MessageId::new(2),
                error: AttachmentCodecError::UnknownEventVariant {
                    kind: "future_event".to_owned(),
                    entry_id: LedgerEntryId(9),
                },
            },
        );

        assert_eq!(
            actual,
            CanonicalLoadFailure::VersionMismatch {
                route: CanonicalLoadRoute::Read,
                failing_entry_id: Some(LedgerEntryId(9)),
            }
        );
        assert_eq!(
            actual.user_message(),
            Some(i18n::unknown_ledger_format_message())
        );
        assert_eq!(
            actual.observability_event(),
            Some(UNKNOWN_LEDGER_FORMAT_EVENT)
        );
        assert_eq!(actual.failing_entry_id(), Some(LedgerEntryId(9)));
    }

    #[test]
    fn canonical_load_failure_preserves_route_for_fetch_timeout() {
        let actual = CanonicalLoadFailure::from_store_error(
            CanonicalLoadRoute::WritePrelude,
            &StoreLoadError::FetchTimeout {
                elapsed: std::time::Duration::from_secs(31),
            },
        );

        assert_eq!(
            actual,
            CanonicalLoadFailure::FetchTimeout {
                route: CanonicalLoadRoute::WritePrelude,
            }
        );
    }

    #[test]
    fn canonical_load_failure_keeps_fetch_timeout_and_permission_messages_distinct() {
        let fetch = CanonicalLoadFailure::from_store_error(
            CanonicalLoadRoute::Read,
            &StoreLoadError::Fetch("transient".to_owned()),
        );
        let timeout = CanonicalLoadFailure::from_store_error(
            CanonicalLoadRoute::Read,
            &StoreLoadError::FetchTimeout {
                elapsed: std::time::Duration::from_secs(31),
            },
        );
        let permission = CanonicalLoadFailure::from_store_error(
            CanonicalLoadRoute::Read,
            &StoreLoadError::Permission("forbidden".to_owned()),
        );

        assert_eq!(
            fetch.user_message(),
            Some(i18n::ledger_retryable_load_message())
        );
        assert_eq!(
            timeout.user_message(),
            Some(i18n::ledger_load_timeout_message())
        );
        assert_eq!(
            permission.user_message(),
            Some(i18n::ledger_permission_failed_message())
        );
    }

    #[test]
    fn canonical_load_failure_keeps_unknown_transport_version_in_decode_family() {
        let actual = CanonicalLoadFailure::from_store_error(
            CanonicalLoadRoute::Read,
            &StoreLoadError::Decode {
                message_id: MessageId::new(1),
                error: AttachmentCodecError::UnsupportedTransportVersion(2),
            },
        );

        assert_eq!(
            actual,
            CanonicalLoadFailure::Decode {
                route: CanonicalLoadRoute::Read,
            }
        );
        assert_eq!(
            actual.user_message(),
            Some(i18n::ledger_attachment_decode_failed_message())
        );
        assert_eq!(actual.observability_event(), None);
    }

    #[test]
    fn canonical_load_failure_maps_remaining_route_taxonomy_cases() {
        let route = CanonicalLoadRoute::Refresh;
        let cases = vec![
            (
                StoreLoadError::AttachmentCardinality {
                    message_id: MessageId::new(1),
                    total_attachments: 2,
                    authoritative_attachments: 1,
                },
                CanonicalLoadFailure::AttachmentCardinality { route },
            ),
            (
                StoreLoadError::OversizeAttachment {
                    message_id: MessageId::new(1),
                    size_bytes: 65_537,
                },
                CanonicalLoadFailure::OversizeAttachment { route },
            ),
            (
                StoreLoadError::Decode {
                    message_id: MessageId::new(2),
                    error: AttachmentCodecError::UnreadableAttachment(
                        "attachment stream closed".to_owned(),
                    ),
                },
                CanonicalLoadFailure::Decode { route },
            ),
            (
                StoreLoadError::Decode {
                    message_id: MessageId::new(3),
                    error: AttachmentCodecError::InvalidHashSuite("sha512_v2".to_owned()),
                },
                CanonicalLoadFailure::Chain { route },
            ),
            (
                StoreLoadError::WriterLineage(
                    crate::discord::ledger::store::WriterLineageFailure::WebhookAuthor {
                        message_id: MessageId::new(4),
                    },
                ),
                CanonicalLoadFailure::WriterLineage { route },
            ),
            (
                StoreLoadError::MetadataCoherence(
                    crate::discord::ledger::store::MetadataCoherenceFailure::MissingGuildContext {
                        message_id: MessageId::new(5),
                    },
                ),
                CanonicalLoadFailure::MetadataCoherence { route },
            ),
            (
                StoreLoadError::DisplayDrift {
                    message_id: MessageId::new(6),
                },
                CanonicalLoadFailure::DisplayDrift { route },
            ),
            (
                StoreLoadError::Permission("forbidden".to_owned()),
                CanonicalLoadFailure::Permission { route },
            ),
            (
                StoreLoadError::Fetch("transient".to_owned()),
                CanonicalLoadFailure::Fetch { route },
            ),
            (
                StoreLoadError::Chain(walicord_application::ledger::LedgerLoadError::ChainVerification(
                    walicord_application::ledger::ChainPositionError {
                        position: 1,
                        error: walicord_application::ledger::LedgerHashChainError::EntryHashMismatch {
                            computed: walicord_application::ledger::EntryHash([1; 32]),
                            declared: walicord_application::ledger::EntryHash([2; 32]),
                        },
                    },
                )),
                CanonicalLoadFailure::Chain { route },
            ),
            (
                StoreLoadError::Structure(walicord_application::ledger::LedgerReplayError::Structure(
                    walicord_application::ledger::AppendOrderedLedgerEntriesError::Structure(
                        walicord_application::ledger::LedgerStructureError::DuplicateEntryId {
                            entry_id: LedgerEntryId(5),
                        },
                    ),
                )),
                CanonicalLoadFailure::Structure { route },
            ),
            (
                StoreLoadError::Projection(walicord_application::ledger::LedgerReplayError::Projection(
                    walicord_application::ledger::LedgerProjectionError::ImbalancedLedgerState {
                        total: Money::from_i64(1),
                    },
                )),
                CanonicalLoadFailure::Projection { route },
            ),
        ];

        for (error, expected) in cases {
            let actual = CanonicalLoadFailure::from_store_error(route, &error);
            assert_eq!(actual, expected);
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

    #[test]
    fn project_verified_entries_returns_transport_backed_views() {
        let actual = project_verified_entries(&load(vec![
            expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
            settlement_entry(2, 2, 1, 5_000),
        ]))
        .expect("projection should succeed");

        assert_eq!(actual.len(), 2);
        assert_eq!(actual[0].entry().id, LedgerEntryId(1));
        assert_eq!(
            actual[1].message_link(),
            "https://discord.com/channels/500/77/2"
        );
        assert_eq!(
            actual[1].recorded_at(),
            std::time::UNIX_EPOCH + std::time::Duration::from_secs(2)
        );
    }

    #[test]
    fn project_verified_entries_prefers_metadata_recorded_at_over_transport_fallback() {
        let mut settlement = settlement_entry(2, 2, 1, 5_000);
        settlement.metadata.recorded_at =
            Some(std::time::UNIX_EPOCH + std::time::Duration::from_secs(99));

        let actual = project_verified_entries(&load(vec![
            expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
            settlement,
        ]))
        .expect("projection should succeed");

        assert_eq!(
            actual[1].recorded_at(),
            std::time::UNIX_EPOCH + std::time::Duration::from_secs(99)
        );
    }

    #[test]
    fn project_recent_voidable_entries_uses_latest_unsealed_unvoided_business_entries() {
        let actual = project_recent_voidable_entries(
            &load(vec![
                expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
                expense_entry(2, 2, &[(1, 1_000), (2, 1_000)]),
                void_entry(3, 2),
                expense_entry(4, 1, &[(1, 2_000), (2, 2_000)]),
            ]),
            20,
        )
        .expect("projection should succeed");

        assert_eq!(
            actual
                .iter()
                .map(|entry| entry.entry().id)
                .collect::<Vec<_>>(),
            vec![LedgerEntryId(4), LedgerEntryId(1)]
        );
    }

    #[test]
    fn project_recent_voidable_entries_excludes_entries_inside_sealed_history() {
        let actual = project_recent_voidable_entries(
            &load(vec![
                expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
                LedgerEntry::non_expense(
                    LedgerEntryId(2),
                    LedgerHistorySealed::new(LedgerEntryId(1)),
                ),
                expense_entry(3, 1, &[(1, 2_000), (2, 2_000)]),
            ]),
            20,
        )
        .expect("projection should succeed");

        assert_eq!(
            actual
                .iter()
                .map(|entry| entry.entry().id)
                .collect::<Vec<_>>(),
            vec![LedgerEntryId(3)]
        );
    }

    #[test]
    fn project_recent_voidable_entries_includes_settlement_entries() {
        let actual = project_recent_voidable_entries(
            &load(vec![
                expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
                settlement_entry(2, 2, 1, 5_000),
            ]),
            20,
        )
        .expect("projection should succeed");

        assert_eq!(
            actual
                .iter()
                .map(|entry| entry.entry().id)
                .collect::<Vec<_>>(),
            vec![LedgerEntryId(2), LedgerEntryId(1)]
        );
    }

    #[test]
    fn project_recent_voidable_entries_applies_window_limit() {
        let actual = project_recent_voidable_entries(
            &load(vec![
                expense_entry(1, 1, &[(1, 1_000), (2, 1_000)]),
                expense_entry(2, 1, &[(1, 2_000), (2, 2_000)]),
                expense_entry(3, 1, &[(1, 3_000), (2, 3_000)]),
            ]),
            2,
        )
        .expect("projection should succeed");

        assert_eq!(
            actual
                .iter()
                .map(|entry| entry.entry().id)
                .collect::<Vec<_>>(),
            vec![LedgerEntryId(3), LedgerEntryId(2)]
        );
    }
}
