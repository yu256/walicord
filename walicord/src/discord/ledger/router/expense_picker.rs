//! Expense picker presentation helpers extracted from `router::mod`. The dispatcher
//! still owns the async handlers (because they reach into `self.deps`), but the
//! synchronous helpers — paging, snapshot id stamping, custom_id formatting,
//! search-modal rendering — live here so mod.rs's footer stays small.

use serenity::{
    all::{ActionRowComponent, ModalInteraction},
    builder::{CreateActionRow, CreateInputText, CreateInteractionResponse, CreateModal},
    model::application::InputTextStyle,
};
use walicord_application::ledger::expense_session::{
    ExpensePickerKind, ExpenseSelectionPhase, ExpenseSelectionState, PickerSnapshotId,
};
use walicord_i18n as i18n;
use walicord_presentation::discord_ledger::{
    SafeLiteralText, SurfaceActionRow, SurfaceButton, SurfaceInteractiveButtonStyle,
    SurfaceMemberLabels, SurfaceSelectOption, truncate_component_label, validate_custom_id,
    validate_modal_title, validate_text_input_label, validate_text_input_placeholder,
};

use super::{LedgerRouteError, RouterRosterSnapshot};

pub(super) const EXPENSE_PAYER_PICK_CUSTOM_ID_PREFIX: &str = "ledger:expense:payer-pick:";
pub(super) const EXPENSE_INDIVIDUAL_PICK_CUSTOM_ID_PREFIX: &str = "ledger:expense:individual-pick:";
pub(super) const EXPENSE_ROLE_PICK_CUSTOM_ID_PREFIX: &str = "ledger:expense:role-pick:";
pub(super) const EXPENSE_PICKER_PREV_CUSTOM_ID_PREFIX: &str = "ledger:expense:picker-prev:";
pub(super) const EXPENSE_PICKER_NEXT_CUSTOM_ID_PREFIX: &str = "ledger:expense:picker-next:";
pub(super) const EXPENSE_PICKER_SEARCH_CUSTOM_ID_PREFIX: &str = "ledger:expense:picker-search:";
pub(super) const EXPENSE_PICKER_CLEAR_CUSTOM_ID_PREFIX: &str = "ledger:expense:picker-clear:";
pub(super) const EXPENSE_PICKER_SEARCH_MODAL_CUSTOM_ID_PREFIX: &str =
    "ledger:expense:picker-search-modal:";
pub(super) const EXPENSE_PICKER_SEARCH_FIELD: &str = "query";
pub(super) const EXPENSE_PICKER_PAGE_SIZE: usize = 25;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ExpensePickerPage {
    pub snapshot_id: PickerSnapshotId,
    pub query: Option<String>,
    pub options: Vec<SurfaceSelectOption>,
    pub visible_values: Vec<u64>,
    pub detail_lines: Vec<String>,
    pub current_page: usize,
    pub total_pages: usize,
    pub total_items: usize,
    pub page_item_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ExpensePickerItem {
    value: u64,
    label: SafeLiteralText,
    selected: bool,
}

pub(super) fn picker_kind_for_phase(phase: &ExpenseSelectionPhase) -> Option<ExpensePickerKind> {
    match phase {
        ExpenseSelectionPhase::Payer => Some(ExpensePickerKind::Payer),
        ExpenseSelectionPhase::IndividualSelection => Some(ExpensePickerKind::Individuals),
        ExpenseSelectionPhase::Roles => Some(ExpensePickerKind::Roles),
        ExpenseSelectionPhase::ParticipantSource | ExpenseSelectionPhase::WeightEditor => None,
    }
}

pub(super) fn expense_picker_page(
    roster: &RouterRosterSnapshot,
    selection: &ExpenseSelectionState,
    kind: ExpensePickerKind,
) -> ExpensePickerPage {
    let query = selection
        .picker_states
        .get(&kind)
        .and_then(|state| state.query().map(str::to_owned));
    let snapshot_id = expense_picker_snapshot_id(roster, kind);
    let requested_page = selection
        .picker_states
        .get(&kind)
        .map_or(0, |state| state.current_page());
    let mut items = expense_picker_items(roster, selection, kind);
    items.sort_by(|left, right| {
        left.label
            .as_str()
            .cmp(right.label.as_str())
            .then_with(|| left.value.cmp(&right.value))
    });
    if let Some(query) = query.as_deref() {
        let normalized = query.to_lowercase();
        items.retain(|item| item.label.as_str().to_lowercase().contains(&normalized));
    }
    let total_items = items.len();
    let total_pages = total_items.div_ceil(EXPENSE_PICKER_PAGE_SIZE).max(1);
    let current_page = requested_page.min(total_pages.saturating_sub(1));
    let page_start = current_page * EXPENSE_PICKER_PAGE_SIZE;
    let page_end = (page_start + EXPENSE_PICKER_PAGE_SIZE).min(total_items);
    let page_items = if total_items == 0 {
        Vec::new()
    } else {
        items[page_start..page_end].to_vec()
    };
    let mut detail_lines = Vec::new();
    if let Some(query) = query.as_deref() {
        detail_lines.push(i18n::expense_search_line(query).to_string());
        if total_items == 0 {
            detail_lines.push(
                match kind {
                    ExpensePickerKind::Roles => i18n::ROLE_SEARCH_NOT_FOUND_ERROR,
                    ExpensePickerKind::Payer | ExpensePickerKind::Individuals => {
                        i18n::MEMBER_SEARCH_NOT_FOUND_ERROR
                    }
                }
                .to_owned(),
            );
        }
    }
    if total_pages > 1 {
        detail_lines.push(i18n::page_indicator(current_page + 1, total_pages).to_string());
        detail_lines
            .push(i18n::page_range_indicator(page_start + 1, page_end, total_items).to_string());
    }
    let visible_values = page_items.iter().map(|item| item.value).collect::<Vec<_>>();
    let options = page_items
        .into_iter()
        .map(|item| SurfaceSelectOption {
            value: item.value.to_string(),
            label: item.label,
            description: None,
            selected: item.selected,
        })
        .collect::<Vec<_>>();
    ExpensePickerPage {
        snapshot_id,
        query,
        page_item_count: options.len(),
        options,
        visible_values,
        detail_lines,
        current_page,
        total_pages,
        total_items,
    }
}

pub(super) fn expense_picker_query_matches(
    roster: &RouterRosterSnapshot,
    selection: &ExpenseSelectionState,
    kind: ExpensePickerKind,
    query: &str,
) -> bool {
    let normalized = query.to_lowercase();
    expense_picker_items(roster, selection, kind)
        .into_iter()
        .any(|item| item.label.as_str().to_lowercase().contains(&normalized))
}

pub(super) fn expense_picker_search_not_found_message(kind: ExpensePickerKind) -> &'static str {
    match kind {
        ExpensePickerKind::Roles => i18n::ROLE_SEARCH_NOT_FOUND_ERROR,
        ExpensePickerKind::Payer | ExpensePickerKind::Individuals => {
            i18n::MEMBER_SEARCH_NOT_FOUND_ERROR
        }
    }
}

fn expense_picker_snapshot_id(
    roster: &RouterRosterSnapshot,
    kind: ExpensePickerKind,
) -> PickerSnapshotId {
    let mut hash = 0xcbf2_9ce4_8422_2325u64;
    for item in expense_picker_items(roster, &ExpenseSelectionState::default(), kind) {
        for byte in item.value.to_be_bytes() {
            hash = hash
                .wrapping_mul(0x100_0000_01b3)
                .wrapping_add(u64::from(byte));
        }
        for byte in item.label.as_str().as_bytes() {
            hash = hash
                .wrapping_mul(0x100_0000_01b3)
                .wrapping_add(u64::from(*byte));
        }
    }
    PickerSnapshotId::new(hash)
}

fn expense_picker_items(
    roster: &RouterRosterSnapshot,
    selection: &ExpenseSelectionState,
    kind: ExpensePickerKind,
) -> Vec<ExpensePickerItem> {
    match kind {
        ExpensePickerKind::Payer | ExpensePickerKind::Individuals => {
            let labels = SurfaceMemberLabels::from_member_names(
                roster.roster.all_members.iter().map(|member_id| {
                    (
                        *member_id,
                        roster
                            .display_names
                            .get(member_id)
                            .map(|name| name.as_str()),
                    )
                }),
            );
            roster
                .roster
                .all_members
                .iter()
                .map(|member_id| ExpensePickerItem {
                    value: member_id.0,
                    label: labels.safe_member_label(*member_id),
                    selected: match kind {
                        ExpensePickerKind::Payer => selection.payer == Some(*member_id),
                        ExpensePickerKind::Individuals => {
                            selection.individual_members.contains(member_id)
                        }
                        ExpensePickerKind::Roles => false,
                    },
                })
                .collect()
        }
        ExpensePickerKind::Roles => roster
            .roster
            .role_members
            .keys()
            .map(|role_id| ExpensePickerItem {
                value: role_id.0,
                label: roster
                    .role_display_names
                    .get(role_id)
                    .and_then(|name| SafeLiteralText::from_roster_label(name.as_str()))
                    .unwrap_or_else(|| {
                        SafeLiteralText::from_roster_label(
                            &i18n::unknown_role_label(role_id.0).to_string(),
                        )
                        .expect("fallback role label should sanitize")
                    }),
                selected: selection.selected_roles.contains(role_id),
            })
            .collect(),
    }
}

pub(super) fn expense_picker_utility_row(
    kind: ExpensePickerKind,
    nonce: walicord_application::InteractionNonce,
    snapshot_id: PickerSnapshotId,
    current_page: usize,
    total_pages: usize,
) -> SurfaceActionRow {
    let mut buttons = Vec::new();
    if total_pages > 1 {
        buttons.push(SurfaceButton::Interactive {
            label: i18n::PICKER_PREVIOUS_PAGE_LABEL.to_owned(),
            custom_id: expense_picker_custom_id(
                EXPENSE_PICKER_PREV_CUSTOM_ID_PREFIX,
                kind,
                nonce,
                snapshot_id,
            ),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: current_page == 0,
        });
        buttons.push(SurfaceButton::Interactive {
            label: i18n::PICKER_NEXT_PAGE_LABEL.to_owned(),
            custom_id: expense_picker_custom_id(
                EXPENSE_PICKER_NEXT_CUSTOM_ID_PREFIX,
                kind,
                nonce,
                snapshot_id,
            ),
            style: SurfaceInteractiveButtonStyle::Secondary,
            disabled: current_page + 1 >= total_pages,
        });
    }
    buttons.push(SurfaceButton::Interactive {
        label: i18n::PICKER_SEARCH_LABEL.to_owned(),
        custom_id: expense_picker_custom_id(
            EXPENSE_PICKER_SEARCH_CUSTOM_ID_PREFIX,
            kind,
            nonce,
            snapshot_id,
        ),
        style: SurfaceInteractiveButtonStyle::Secondary,
        disabled: false,
    });
    buttons.push(SurfaceButton::Interactive {
        label: picker_clear_label(kind).to_owned(),
        custom_id: expense_picker_custom_id(
            EXPENSE_PICKER_CLEAR_CUSTOM_ID_PREFIX,
            kind,
            nonce,
            snapshot_id,
        ),
        style: SurfaceInteractiveButtonStyle::Secondary,
        disabled: false,
    });
    SurfaceActionRow::Buttons(buttons)
}

fn picker_clear_label(kind: ExpensePickerKind) -> &'static str {
    match kind {
        ExpensePickerKind::Payer => i18n::PAYER_CLEAR_LABEL,
        ExpensePickerKind::Individuals => i18n::INDIVIDUAL_CLEAR_LABEL,
        ExpensePickerKind::Roles => i18n::ROLE_CLEAR_LABEL,
    }
}

pub(super) fn expense_picker_custom_id(
    prefix: &str,
    kind: ExpensePickerKind,
    nonce: walicord_application::InteractionNonce,
    snapshot_id: PickerSnapshotId,
) -> String {
    format!(
        "{prefix}{}:{nonce}:{snapshot_id}",
        expense_picker_kind_slug(kind)
    )
}

pub(super) fn parse_expense_picker_custom_id(
    custom_id: &str,
    prefix: &str,
) -> Option<(
    ExpensePickerKind,
    walicord_application::InteractionNonce,
    PickerSnapshotId,
)> {
    let remainder = custom_id.strip_prefix(prefix)?;
    let (kind, remainder) = remainder.split_once(':')?;
    let (nonce, snapshot_id) = remainder.split_once(':')?;
    let kind = parse_expense_picker_kind(kind)?;
    let nonce = nonce
        .parse::<u64>()
        .ok()
        .and_then(|value| walicord_application::InteractionNonce::new(value).ok())?;
    let snapshot_id = snapshot_id.parse::<PickerSnapshotId>().ok()?;
    Some((kind, nonce, snapshot_id))
}

pub(super) fn expense_picker_selection_custom_id(
    prefix: &str,
    nonce: walicord_application::InteractionNonce,
    snapshot_id: PickerSnapshotId,
) -> String {
    format!("{prefix}{nonce}:{snapshot_id}")
}

pub(super) fn parse_expense_picker_selection_custom_id(
    custom_id: &str,
    prefix: &str,
) -> Option<(walicord_application::InteractionNonce, PickerSnapshotId)> {
    let remainder = custom_id.strip_prefix(prefix)?;
    let (nonce, snapshot_id) = remainder.split_once(':')?;
    let nonce = nonce
        .parse::<u64>()
        .ok()
        .and_then(|value| walicord_application::InteractionNonce::new(value).ok())?;
    let snapshot_id = snapshot_id.parse::<PickerSnapshotId>().ok()?;
    Some((nonce, snapshot_id))
}

fn expense_picker_kind_slug(kind: ExpensePickerKind) -> &'static str {
    match kind {
        ExpensePickerKind::Payer => "payer",
        ExpensePickerKind::Individuals => "individuals",
        ExpensePickerKind::Roles => "roles",
    }
}

fn parse_expense_picker_kind(value: &str) -> Option<ExpensePickerKind> {
    match value {
        "payer" => Some(ExpensePickerKind::Payer),
        "individuals" => Some(ExpensePickerKind::Individuals),
        "roles" => Some(ExpensePickerKind::Roles),
        _ => None,
    }
}

#[allow(clippy::result_large_err)] // LedgerRouteError is the router-wide error envelope.
pub(super) fn build_expense_picker_search_modal_response(
    kind: ExpensePickerKind,
    nonce: walicord_application::InteractionNonce,
    snapshot_id: PickerSnapshotId,
) -> Result<CreateInteractionResponse, LedgerRouteError> {
    let custom_id = expense_picker_custom_id(
        EXPENSE_PICKER_SEARCH_MODAL_CUSTOM_ID_PREFIX,
        kind,
        nonce,
        snapshot_id,
    );
    validate_custom_id(&custom_id)?;
    let (title, label, placeholder) = match kind {
        ExpensePickerKind::Payer | ExpensePickerKind::Individuals => (
            i18n::MEMBER_SEARCH_MODAL_TITLE,
            i18n::MEMBER_SEARCH_INPUT_LABEL,
            i18n::MEMBER_SEARCH_PLACEHOLDER,
        ),
        ExpensePickerKind::Roles => (
            i18n::ROLE_SEARCH_MODAL_TITLE,
            i18n::ROLE_SEARCH_INPUT_LABEL,
            i18n::ROLE_SEARCH_PLACEHOLDER,
        ),
    };
    let title = truncate_component_label(title);
    validate_modal_title(&title)?;
    let label = truncate_component_label(label);
    validate_text_input_label(&label)?;
    let placeholder = truncate_component_label(placeholder);
    validate_text_input_placeholder(&placeholder)?;
    let input = CreateInputText::new(InputTextStyle::Short, label, EXPENSE_PICKER_SEARCH_FIELD)
        .placeholder(placeholder)
        .required(true);
    Ok(CreateInteractionResponse::Modal(
        CreateModal::new(custom_id, title).components(vec![CreateActionRow::InputText(input)]),
    ))
}

pub(super) fn extract_expense_picker_search_query(modal: &ModalInteraction) -> &str {
    for row in &modal.data.components {
        for component in &row.components {
            if let ActionRowComponent::InputText(input) = component
                && input.custom_id == EXPENSE_PICKER_SEARCH_FIELD
            {
                return input.value.as_deref().unwrap_or_default();
            }
        }
    }
    ""
}

pub(super) fn merge_paged_selection(
    existing: &[u64],
    visible: &[u64],
    selected: &[u64],
) -> Vec<u64> {
    let mut merged = existing
        .iter()
        .copied()
        .filter(|value| !visible.contains(value))
        .chain(selected.iter().copied())
        .collect::<Vec<_>>();
    merged.sort_unstable();
    merged.dedup();
    merged
}
