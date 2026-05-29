use std::time::{Duration, SystemTime};

use crate::{
    Clock, SettlementPlanner,
    settle_up::{PreviewConfirmationBinding, PreviewedSettlement, SettleUpError, SettleUpPolicy},
};
use walicord_domain::{
    Money, SettlementContext,
    model::{MemberId, Weight},
};
use walicord_ledger::{EntryVoided, ExpenseNote, ExpenseRecorded, MemberAmount};

use super::{
    AllocationSnapshot, EntryHash, LedgerEffectiveDate, LedgerEntry, LedgerEntryId,
    LedgerEntryMetadata, LedgerSourceCanonical, LedgerSourceCanonicalError, MemberWeight,
    VerifiedLedgerSnapshot,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiscordLedgerSourceFamily {
    Expense,
    Review,
    Settle,
    Void,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiscordLedgerSourceDescriptor {
    family: DiscordLedgerSourceFamily,
    canonical: &'static str,
}

impl DiscordLedgerSourceDescriptor {
    pub fn expense_slash_modal_v1() -> Self {
        Self {
            family: DiscordLedgerSourceFamily::Expense,
            canonical: "expense/slash-modal/v1",
        }
    }

    pub fn expense_panel_modal_v1() -> Self {
        Self {
            family: DiscordLedgerSourceFamily::Expense,
            canonical: "expense/panel-modal/v1",
        }
    }

    pub fn review_thread_v1() -> Self {
        Self {
            family: DiscordLedgerSourceFamily::Review,
            canonical: "review/thread/v1",
        }
    }

    pub fn settle_thread_v1() -> Self {
        Self {
            family: DiscordLedgerSourceFamily::Settle,
            canonical: "settle/thread/v1",
        }
    }

    pub fn void_parent_v1() -> Self {
        Self {
            family: DiscordLedgerSourceFamily::Void,
            canonical: "void/parent/v1",
        }
    }

    fn require(self, expected: DiscordLedgerSourceFamily) -> Result<Self, WrongSourceDescriptor> {
        (self.family == expected)
            .then_some(self)
            .ok_or(WrongSourceDescriptor)
    }

    fn canonical(self) -> &'static str {
        self.canonical
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WrongSourceDescriptor;

const MAX_EXPENSE_NOTE_SCALARS: usize = 200;
const MAX_RESOLVED_EXPENSE_PARTICIPANTS: usize = 100;
const SETTLEMENT_PREVIEW_TTL: Duration = Duration::from_secs(600);

fn is_removed_note_format_char(ch: char) -> bool {
    matches!(
        ch,
        '\u{00AD}'
            | '\u{034F}'
            | '\u{061C}'
            | '\u{180E}'
            | '\u{FEFF}'
            | '\u{17B4}'
            | '\u{17B5}'
            | '\u{200B}'..='\u{200F}'
            | '\u{202A}'..='\u{202E}'
            | '\u{2060}'..='\u{206F}'
    )
}

fn url_like_host_candidate(token: &str) -> Option<&str> {
    let host = token
        .split(['/', ':', '?', '#'])
        .next()
        .unwrap_or(token)
        .trim_end_matches([',', '.', ';', '!', '?', ')', ']', '}']);
    (!host.is_empty()).then_some(host)
}

fn looks_like_host_token(token: &str) -> bool {
    let Some(host) = url_like_host_candidate(token) else {
        return false;
    };
    let segments: Vec<&str> = host.split('.').collect();
    let Some(tld) = segments.last() else {
        return false;
    };
    segments.len() >= 2
        && tld.len() >= 2
        && tld.chars().all(|ch| ch.is_ascii_alphabetic())
        && segments.iter().all(|segment| {
            !segment.is_empty()
                && segment
                    .chars()
                    .all(|ch| ch.is_ascii_alphanumeric() || ch == '-')
        })
}

fn contains_url_like_token(token: &str) -> bool {
    token
        .split(|ch: char| {
            !ch.is_ascii_alphanumeric() && !matches!(ch, '.' | '-' | ':' | '/' | '?' | '#')
        })
        .any(|candidate| candidate.contains("://") || looks_like_host_token(candidate))
}

fn neutralize_url_like_token(token: &str) -> String {
    token
        .chars()
        .map(|ch| match ch {
            ':' => '：',
            '/' => '／',
            '.' => '．',
            _ => ch,
        })
        .collect()
}

fn normalize_expense_note(raw: &str) -> Option<String> {
    let mut normalized = String::new();
    let mut last_was_space = false;

    for ch in raw.chars() {
        if is_removed_note_format_char(ch) {
            continue;
        }

        let ch = if ch.is_control() || matches!(ch, '\u{2028}' | '\u{2029}') {
            ' '
        } else {
            ch
        };

        if ch.is_whitespace() {
            if !normalized.is_empty() && !last_was_space {
                normalized.push(' ');
            }
            last_was_space = true;
            continue;
        }

        match ch {
            '@' => normalized.push('＠'),
            '<' => normalized.push('＜'),
            '>' => normalized.push('＞'),
            '\\' | '#' | '*' | '_' | '`' | '~' | '|' | '[' | ']' | '(' | ')' => {
                normalized.push('\\');
                normalized.push(ch);
            }
            _ => normalized.push(ch),
        }
        last_was_space = false;
    }

    let normalized = normalized
        .trim()
        .split(' ')
        .map(|token| {
            if contains_url_like_token(token) {
                neutralize_url_like_token(token)
            } else {
                token.to_owned()
            }
        })
        .collect::<Vec<_>>()
        .join(" ");
    (!normalized.is_empty()).then_some(normalized)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedExpenseAuthoringInput {
    payer: MemberId,
    amount: Money,
    participants: Vec<MemberWeight>,
    note: Option<ExpenseNote>,
    effective_date: LedgerEffectiveDate,
    recorded_by: MemberId,
}

impl ResolvedExpenseAuthoringInput {
    pub fn new(
        payer: MemberId,
        amount: Money,
        participants: impl IntoIterator<Item = MemberWeight>,
        note: Option<String>,
        effective_date: LedgerEffectiveDate,
        recorded_by: MemberId,
    ) -> Result<Self, ExpenseAuthoringError> {
        let mut participants: Vec<MemberWeight> = participants.into_iter().collect();
        if participants.is_empty() {
            return Err(ExpenseAuthoringError::EmptyParticipants);
        }
        participants.sort_by_key(|participant| participant.member_id);
        for window in participants.windows(2) {
            if window[0].member_id == window[1].member_id {
                return Err(ExpenseAuthoringError::DuplicateParticipant {
                    member_id: window[0].member_id,
                });
            }
        }
        if participants.len() > MAX_RESOLVED_EXPENSE_PARTICIPANTS {
            return Err(ExpenseAuthoringError::TooManyParticipants);
        }

        let note = note
            .map(|note| {
                (note.chars().count() <= MAX_EXPENSE_NOTE_SCALARS)
                    .then(|| normalize_expense_note(&note))
                    .ok_or(ExpenseAuthoringError::NoteTooLong)
            })
            .transpose()?
            .flatten()
            .map(ExpenseNote::new)
            .transpose()
            .map_err(|_| ExpenseAuthoringError::InvalidNote)?;

        Ok(Self {
            payer,
            amount,
            participants,
            note,
            effective_date,
            recorded_by,
        })
    }

    fn payer(&self) -> MemberId {
        self.payer
    }

    fn amount(&self) -> Money {
        self.amount
    }

    fn participants(&self) -> &[MemberWeight] {
        &self.participants
    }

    fn note(&self) -> Option<&ExpenseNote> {
        self.note.as_ref()
    }

    fn effective_date(&self) -> &LedgerEffectiveDate {
        &self.effective_date
    }

    fn recorded_by(&self) -> MemberId {
        self.recorded_by
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordableExpenseAuthoring {
    payer: MemberId,
    amount: Money,
    owed_by: Vec<MemberAmount>,
    note: Option<ExpenseNote>,
    effective_date: LedgerEffectiveDate,
    recorded_by: MemberId,
    allocation_snapshot: AllocationSnapshot,
}

impl RecordableExpenseAuthoring {
    pub fn new(input: ResolvedExpenseAuthoringInput) -> Result<Self, ExpenseAuthoringError> {
        if input.amount() <= Money::ZERO {
            return Err(ExpenseAuthoringError::InvalidAmount);
        }

        let allocation_snapshot = allocation_snapshot_for(input.participants(), input.amount())?;
        let owed_by = distribute_owed_amounts(input.participants(), input.amount())?;

        Ok(Self {
            payer: input.payer(),
            amount: input.amount(),
            owed_by,
            note: input.note().cloned(),
            effective_date: *input.effective_date(),
            recorded_by: input.recorded_by(),
            allocation_snapshot,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ExpenseAuthoringError {
    #[error("duplicate participant: member {member_id:?}")]
    DuplicateParticipant { member_id: MemberId },
    #[error("expense has no participants")]
    EmptyParticipants,
    #[error("invalid amount for expense")]
    InvalidAmount,
    #[error("invalid note for expense")]
    InvalidNote,
    #[error("expense note exceeds canonical length cap")]
    NoteTooLong,
    #[error("expense exceeds the participant cap")]
    TooManyParticipants,
    #[error("invalid weight configuration (e.g. all zero, negative)")]
    InvalidWeightConfiguration,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum DiscordLedgerEntryError {
    #[error("invalid ledger source descriptor: {0}")]
    InvalidSource(#[from] LedgerSourceCanonicalError),
    #[error("ledger entry construction failed")]
    LedgerConstruction,
    #[error("wrong source descriptor for this entry kind")]
    WrongSourceDescriptor,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum SettlementRecordError {
    #[error("settlement actor mismatch: observed {actual:?}, expected {expected:?}")]
    ActorMismatch {
        actual: MemberId,
        expected: MemberId,
    },
    #[error("invalid ledger source descriptor: {0}")]
    InvalidSource(#[from] LedgerSourceCanonicalError),
    #[error("settle-up preview rejected the record attempt: {0}")]
    Preview(#[from] SettleUpError),
    #[error("preview has not been marked delivered yet")]
    PreviewNotDelivered,
    #[error("wrong source descriptor for settlement record")]
    WrongSourceDescriptor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PreviewedSettlementOutcome {
    NoTransfersNeeded,
    RecordablePreview {
        previewed: PreviewedSettlement,
        actor_id: MemberId,
        ledger_head_hash: EntryHash,
        created_at: SystemTime,
        expires_at: SystemTime,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum SettlementPreviewError {
    #[error("ledger snapshot is missing its head hash")]
    MissingHeadHash,
    #[error("settle-up preview rejected: {0}")]
    Preview(#[from] SettleUpError),
    #[error("preview lifetime overflowed (created_at: {created_at:?})")]
    PreviewLifetimeOverflow { created_at: SystemTime },
}

pub fn preview_settlement_from_snapshot(
    snapshot: &VerifiedLedgerSnapshot,
    actor_id: MemberId,
    planner: &dyn SettlementPlanner,
    clock: &dyn Clock,
) -> Result<PreviewedSettlementOutcome, SettlementPreviewError> {
    let settle_members: Vec<MemberId> = snapshot
        .projected()
        .state()
        .participants()
        .iter()
        .copied()
        .collect();
    let previewed = SettleUpPolicy::preview(
        planner,
        snapshot.projected().state().balances(),
        &settle_members,
        std::iter::empty::<MemberId>(),
        SettlementContext::jpy_default(),
    )?;

    if previewed.recordable_event().is_none() {
        return Ok(PreviewedSettlementOutcome::NoTransfersNeeded);
    }

    let created_at = clock.now();
    let expires_at = created_at
        .checked_add(SETTLEMENT_PREVIEW_TTL)
        .ok_or(SettlementPreviewError::PreviewLifetimeOverflow { created_at })?;

    Ok(PreviewedSettlementOutcome::RecordablePreview {
        previewed,
        actor_id,
        ledger_head_hash: snapshot
            .current_head_hash()
            .ok_or(SettlementPreviewError::MissingHeadHash)?,
        created_at,
        expires_at,
    })
}

pub fn build_discord_expense_entry(
    entry_id: LedgerEntryId,
    authored: RecordableExpenseAuthoring,
    source: DiscordLedgerSourceDescriptor,
    clock: &dyn Clock,
) -> Result<LedgerEntry, DiscordLedgerEntryError> {
    let source = source
        .require(DiscordLedgerSourceFamily::Expense)
        .map_err(|_| DiscordLedgerEntryError::WrongSourceDescriptor)?;
    let event = ExpenseRecorded::new(
        vec![MemberAmount {
            member_id: authored.payer,
            amount: authored.amount,
        }],
        authored.owed_by,
        authored.note,
    )
    .map_err(|_| DiscordLedgerEntryError::LedgerConstruction)?;

    let mut entry = LedgerEntry::expense(entry_id, event, authored.allocation_snapshot)
        .map_err(|_| DiscordLedgerEntryError::LedgerConstruction)?;
    entry.metadata = LedgerEntryMetadata {
        recorded_by: Some(authored.recorded_by),
        source: Some(
            LedgerSourceCanonical::discord_ui(source.canonical())
                .map_err(DiscordLedgerEntryError::InvalidSource)?,
        ),
        effective_date: Some(authored.effective_date),
        recorded_at: Some(clock.now()),
        allocation_snapshot: entry.metadata.allocation_snapshot.clone(),
    };
    Ok(entry)
}

pub fn record_previewed_plan_matching(
    entry_id: LedgerEntryId,
    actor_id: MemberId,
    previewed: PreviewedSettlement,
    binding: PreviewConfirmationBinding,
    source: DiscordLedgerSourceDescriptor,
    clock: &dyn Clock,
) -> Result<Option<LedgerEntry>, SettlementRecordError> {
    let source = source
        .require(DiscordLedgerSourceFamily::Settle)
        .map_err(|_| SettlementRecordError::WrongSourceDescriptor)?;
    if binding.actor_id() != actor_id {
        return Err(SettlementRecordError::ActorMismatch {
            actual: actor_id,
            expected: binding.actor_id(),
        });
    }
    if !binding.is_delivered() {
        return Err(SettlementRecordError::PreviewNotDelivered);
    }

    let validated =
        SettleUpPolicy::record_previewed_plan_matching(previewed, binding.preview_digest())
            .map_err(SettlementRecordError::Preview)?;
    let Some(event) = validated.recordable_event().cloned() else {
        return Ok(None);
    };

    let mut entry = LedgerEntry::non_expense(entry_id, event);
    entry.metadata.recorded_by = Some(actor_id);
    entry.metadata.source = Some(
        LedgerSourceCanonical::discord_ui(source.canonical())
            .map_err(SettlementRecordError::InvalidSource)?,
    );
    entry.metadata.recorded_at = Some(clock.now());
    Ok(Some(entry))
}

pub fn build_discord_void_entry(
    entry_id: LedgerEntryId,
    actor_id: MemberId,
    target_entry_id: LedgerEntryId,
    source: DiscordLedgerSourceDescriptor,
    clock: &dyn Clock,
) -> Result<LedgerEntry, DiscordLedgerEntryError> {
    let source = source
        .require(DiscordLedgerSourceFamily::Void)
        .map_err(|_| DiscordLedgerEntryError::WrongSourceDescriptor)?;
    let mut entry = LedgerEntry::non_expense(entry_id, EntryVoided::new(target_entry_id));
    entry.metadata.recorded_by = Some(actor_id);
    entry.metadata.source = Some(
        LedgerSourceCanonical::discord_ui(source.canonical())
            .map_err(DiscordLedgerEntryError::InvalidSource)?,
    );
    entry.metadata.recorded_at = Some(clock.now());
    Ok(entry)
}

fn allocation_snapshot_for(
    participants: &[MemberWeight],
    amount: Money,
) -> Result<AllocationSnapshot, ExpenseAuthoringError> {
    if participants
        .iter()
        .all(|participant| participant.weight == Weight(1))
    {
        let total_units = SettlementContext::jpy_default()
            .to_atomic_units_i64(amount)
            .map_err(|_| ExpenseAuthoringError::InvalidAmount)?;
        let participant_count =
            i64::try_from(participants.len()).expect("participant count should fit into i64");
        if total_units >= participant_count {
            return Ok(AllocationSnapshot::Even);
        }
    }

    AllocationSnapshot::weighted(participants.iter().copied())
        .map_err(|_| ExpenseAuthoringError::InvalidWeightConfiguration)
}

/// Compute the canonical per-member owed amounts for a given expense `amount` and
/// resolved `participants` (`MemberWeight` pairs). This is the same distribution the
/// record path uses inside `RecordableExpenseAuthoring::new`, exposed publicly so the
/// confirmation / preview UIs can render the exact shares the actor will see on the
/// ledger — calling the same function eliminates divergence by construction.
pub fn compute_expense_owed_amounts(
    participants: &[MemberWeight],
    amount: Money,
) -> Result<Vec<MemberAmount>, ExpenseAuthoringError> {
    distribute_owed_amounts(participants, amount)
}

fn distribute_owed_amounts(
    participants: &[MemberWeight],
    amount: Money,
) -> Result<Vec<MemberAmount>, ExpenseAuthoringError> {
    let total_units = SettlementContext::jpy_default()
        .to_atomic_units_i64(amount)
        .map_err(|_| ExpenseAuthoringError::InvalidAmount)?;
    let total_units =
        u64::try_from(total_units).map_err(|_| ExpenseAuthoringError::InvalidAmount)?;

    let positive_indices: Vec<usize> = participants
        .iter()
        .enumerate()
        .filter_map(|(index, participant)| (participant.weight > Weight::ZERO).then_some(index))
        .collect();
    if positive_indices.is_empty() {
        return Err(ExpenseAuthoringError::InvalidWeightConfiguration);
    }

    let total_weight = positive_indices
        .iter()
        .map(|index| u128::from(participants[*index].weight.0))
        .sum::<u128>();
    if total_weight == 0 {
        return Err(ExpenseAuthoringError::InvalidWeightConfiguration);
    }

    let mut units_per_member = vec![0_u64; participants.len()];
    let total_units_u128 = u128::from(total_units);
    let mut assigned = 0_u64;
    for index in &positive_indices {
        let share = (total_units_u128 * u128::from(participants[*index].weight.0)) / total_weight;
        let share = u64::try_from(share).map_err(|_| ExpenseAuthoringError::InvalidAmount)?;
        units_per_member[*index] = share;
        assigned += share;
    }

    let remainder = total_units.saturating_sub(assigned);
    for index in positive_indices
        .into_iter()
        .take(usize::try_from(remainder).unwrap_or(0))
    {
        units_per_member[index] += 1;
    }

    Ok(participants
        .iter()
        .zip(units_per_member)
        .filter_map(|(participant, units)| {
            (units > 0).then_some(MemberAmount {
                member_id: participant.member_id,
                amount: Money::from_i64(i64::try_from(units).expect("u64 units should fit in i64")),
            })
        })
        .collect())
}
