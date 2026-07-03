use crate::ledger::{
    AllocationSnapshot, BalanceAdjusted, BalanceAdjustment, BalanceAdjustmentSource, EntryHash,
    EntryVoided, ExpenseNote, ExpenseRecorded, HashedLedgerPayload, LedgerEffectiveDate,
    LedgerEntry, LedgerEntryId, LedgerEntryMetadata, LedgerEvent, LedgerHashSuite,
    LedgerHistorySealed, LedgerSourceCanonical, LedgerSourceCanonicalKind, MemberAmount,
    MemberWeight, NormalizedSettlementPlanRecorded, SchemaVersion, UnverifiedLedgerStoreEnvelope,
    external_correction_source_for_transport_decode,
};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use sha2::{Digest as _, Sha256};
use std::{
    str::FromStr,
    time::{SystemTime, UNIX_EPOCH},
};
use walicord_domain::{
    Money, Transfer,
    model::{MemberId, Weight},
};
use walicord_ledger::CanonicalLedgerId;

const ATTACHMENT_SCHEMA_VERSION: u32 = 1;

pub struct CanonicalAttachmentCodec;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AttachmentTransportMetadata {
    pre_self_link_content_sha256: Option<String>,
}

impl AttachmentTransportMetadata {
    pub fn pre_self_link_content_sha256(&self) -> Option<&str> {
        self.pre_self_link_content_sha256.as_deref()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecodedCanonicalAttachment<ExternalId> {
    pub envelope: UnverifiedLedgerStoreEnvelope<ExternalId>,
    pub transport: AttachmentTransportMetadata,
}

impl CanonicalAttachmentCodec {
    pub fn encode(
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
    ) -> Result<Vec<u8>, AttachmentCodecError> {
        Self::encode_with_pre_self_link_content(envelope, None)
    }

    pub fn encode_with_pre_self_link_content(
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
        pre_self_link_content: Option<&str>,
    ) -> Result<Vec<u8>, AttachmentCodecError> {
        let dto = DiscordLedgerAttachmentDto {
            version: ATTACHMENT_SCHEMA_VERSION,
            transport: TransportMetadataDto {
                recorded_at_unix_ms: None,
                pre_self_link_content_sha256: pre_self_link_content
                    .map(pre_self_link_content_sha256),
            },
            envelope: EnvelopeDto::from_unverified(envelope)?,
        };

        serde_json::to_vec_pretty(&dto).map_err(AttachmentCodecError::JsonEncode)
    }

    pub fn decode<ExternalId>(
        bytes: &[u8],
        external_id: ExternalId,
    ) -> Result<UnverifiedLedgerStoreEnvelope<ExternalId>, AttachmentCodecError> {
        Ok(Self::decode_with_transport(bytes, external_id)?.envelope)
    }

    pub fn decode_with_transport<ExternalId>(
        bytes: &[u8],
        external_id: ExternalId,
    ) -> Result<DecodedCanonicalAttachment<ExternalId>, AttachmentCodecError> {
        let dto: RawDiscordLedgerAttachmentDto =
            serde_json::from_slice(bytes).map_err(AttachmentCodecError::JsonDecode)?;
        if dto.version != ATTACHMENT_SCHEMA_VERSION {
            return Err(AttachmentCodecError::UnsupportedTransportVersion(
                dto.version,
            ));
        }

        Ok(DecodedCanonicalAttachment {
            envelope: dto.envelope.into_unverified(external_id)?,
            transport: AttachmentTransportMetadata {
                pre_self_link_content_sha256: dto.transport.pre_self_link_content_sha256,
            },
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct DiscordLedgerAttachmentDto {
    version: u32,
    transport: TransportMetadataDto,
    envelope: EnvelopeDto,
}

#[derive(Debug, Deserialize)]
struct RawDiscordLedgerAttachmentDto {
    version: u32,
    transport: TransportMetadataDto,
    envelope: RawEnvelopeDto,
}

#[derive(Debug, Serialize, Deserialize)]
struct TransportMetadataDto {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    recorded_at_unix_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pre_self_link_content_sha256: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct EnvelopeDto {
    previous_hash: String,
    entry_hash: String,
    payload: PayloadDto,
}

impl EnvelopeDto {
    fn from_unverified(
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
    ) -> Result<Self, AttachmentCodecError> {
        Ok(Self {
            previous_hash: encode_hash(envelope.previous_hash),
            entry_hash: encode_hash(envelope.entry_hash),
            payload: PayloadDto::from_payload(&envelope.payload)?,
        })
    }
}

#[derive(Debug, Deserialize)]
struct RawEnvelopeDto {
    previous_hash: String,
    entry_hash: String,
    payload: RawPayloadDto,
}

impl RawEnvelopeDto {
    fn into_unverified<ExternalId>(
        self,
        external_id: ExternalId,
    ) -> Result<UnverifiedLedgerStoreEnvelope<ExternalId>, AttachmentCodecError> {
        Ok(UnverifiedLedgerStoreEnvelope {
            previous_hash: decode_hash(&self.previous_hash)?,
            entry_hash: decode_hash(&self.entry_hash)?,
            external_id,
            payload: self.payload.into_payload()?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct PayloadDto {
    ledger_id: CanonicalLedgerId,
    schema_version: u32,
    hash_suite: String,
    entry: EntryDto,
}

impl PayloadDto {
    fn from_payload(payload: &HashedLedgerPayload) -> Result<Self, AttachmentCodecError> {
        Ok(Self {
            ledger_id: payload.ledger_id.into(),
            schema_version: payload.schema_version.0,
            hash_suite: hash_suite_name(payload.hash_suite).into(),
            entry: EntryDto::from_entry(&payload.entry)?,
        })
    }
}

#[derive(Debug, Deserialize)]
struct RawPayloadDto {
    ledger_id: CanonicalLedgerId,
    schema_version: u32,
    hash_suite: String,
    entry: serde_json::Value,
}

impl RawPayloadDto {
    fn into_payload(self) -> Result<HashedLedgerPayload, AttachmentCodecError> {
        if self.schema_version != 1 {
            let entry_id = serde_json::from_value::<EntryIdProbe>(self.entry.clone())
                .ok()
                .map(|probe| LedgerEntryId(probe.id));
            return Err(AttachmentCodecError::UnknownSchemaVersion {
                version: self.schema_version,
                entry_id,
            });
        }
        let entry: RawEntryDto =
            serde_json::from_value(self.entry).map_err(AttachmentCodecError::JsonDecode)?;

        Ok(HashedLedgerPayload {
            ledger_id: self.ledger_id.into(),
            schema_version: SchemaVersion(self.schema_version),
            hash_suite: parse_hash_suite(&self.hash_suite)?,
            entry: entry.into_entry()?,
        })
    }
}

#[derive(Debug, Deserialize)]
struct EntryIdProbe {
    id: u64,
}

#[derive(Debug, Serialize, Deserialize)]
struct EntryDto {
    id: u64,
    metadata: MetadataDto,
    event: EventDto,
}

impl EntryDto {
    fn from_entry(entry: &LedgerEntry) -> Result<Self, AttachmentCodecError> {
        Ok(Self {
            id: entry.id.0,
            metadata: MetadataDto::from_metadata(&entry.metadata)?,
            event: EventDto::from_event(&entry.event),
        })
    }
}

#[derive(Debug, Deserialize)]
struct RawEntryDto {
    id: u64,
    metadata: MetadataDto,
    event: serde_json::Value,
}

impl RawEntryDto {
    fn into_entry(self) -> Result<LedgerEntry, AttachmentCodecError> {
        Ok(LedgerEntry {
            id: LedgerEntryId(self.id),
            metadata: self.metadata.into_metadata()?,
            event: decode_event(self.event, LedgerEntryId(self.id))?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct MetadataDto {
    recorded_by: Option<u64>,
    source: Option<SourceDto>,
    effective_date: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    recorded_at_unix_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    allocation_snapshot: Option<AllocationSnapshotDto>,
}

impl MetadataDto {
    fn from_metadata(metadata: &LedgerEntryMetadata) -> Result<Self, AttachmentCodecError> {
        Ok(Self {
            recorded_by: metadata.recorded_by.map(|member_id| member_id.0),
            source: metadata.source.as_ref().map(SourceDto::from_source),
            effective_date: metadata
                .effective_date
                .as_ref()
                .map(LedgerEffectiveDate::to_string),
            recorded_at_unix_ms: metadata
                .recorded_at
                .map(system_time_to_unix_ms)
                .transpose()?,
            allocation_snapshot: metadata
                .allocation_snapshot
                .as_ref()
                .map(AllocationSnapshotDto::from_snapshot),
        })
    }

    fn into_metadata(self) -> Result<LedgerEntryMetadata, AttachmentCodecError> {
        Ok(LedgerEntryMetadata {
            recorded_by: self.recorded_by.map(MemberId),
            source: self.source.map(SourceDto::into_source).transpose()?,
            effective_date: self
                .effective_date
                .map(LedgerEffectiveDate::new)
                .transpose()
                .map_err(|_| AttachmentCodecError::InvalidEffectiveDate)?,
            recorded_at: self
                .recorded_at_unix_ms
                .map(unix_ms_to_system_time)
                .transpose()?,
            allocation_snapshot: self
                .allocation_snapshot
                .map(AllocationSnapshotDto::into_snapshot)
                .transpose()?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct SourceDto {
    kind: String,
    canonical: String,
}

impl SourceDto {
    fn from_source(source: &LedgerSourceCanonical) -> Self {
        Self {
            kind: source_kind_name(source.kind()).into(),
            canonical: source.canonical_text().to_owned(),
        }
    }

    fn into_source(self) -> Result<LedgerSourceCanonical, AttachmentCodecError> {
        match self.kind.as_str() {
            "legacy_dsl" => LedgerSourceCanonical::legacy_dsl(self.canonical),
            "discord_ui" => LedgerSourceCanonical::discord_ui(self.canonical),
            _ => return Err(AttachmentCodecError::InvalidSourceKind(self.kind)),
        }
        .map_err(|_| AttachmentCodecError::InvalidSource)
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum AllocationSnapshotDto {
    Even,
    Weighted {
        resolved_weights: Vec<MemberWeightDto>,
    },
    LegacyUnknown,
}

impl AllocationSnapshotDto {
    fn from_snapshot(snapshot: &AllocationSnapshot) -> Self {
        match snapshot {
            AllocationSnapshot::Even => Self::Even,
            AllocationSnapshot::Weighted { resolved_weights } => Self::Weighted {
                resolved_weights: resolved_weights
                    .iter()
                    .map(|weight| MemberWeightDto {
                        member_id: weight.member_id.0,
                        weight: weight.weight.0,
                    })
                    .collect(),
            },
            AllocationSnapshot::LegacyUnknown => Self::LegacyUnknown,
        }
    }

    fn into_snapshot(self) -> Result<AllocationSnapshot, AttachmentCodecError> {
        match self {
            Self::Even => Ok(AllocationSnapshot::Even),
            Self::Weighted { resolved_weights } => {
                AllocationSnapshot::weighted(resolved_weights.into_iter().map(|weight| {
                    MemberWeight {
                        member_id: MemberId(weight.member_id),
                        weight: Weight(weight.weight),
                    }
                }))
                .map_err(|_| AttachmentCodecError::InvalidAllocationSnapshot)
            }
            Self::LegacyUnknown => Ok(AllocationSnapshot::LegacyUnknown),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct MemberWeightDto {
    member_id: u64,
    weight: u64,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum EventDto {
    ExpenseRecorded {
        paid_by: Vec<MemberAmountDto>,
        owed_by: Vec<MemberAmountDto>,
        note: Option<String>,
    },
    NormalizedSettlementPlanRecorded {
        transfers: Vec<TransferDto>,
    },
    LedgerHistorySealed {
        through: u64,
    },
    EntryVoided {
        target: u64,
    },
    BalanceAdjusted {
        adjustments: Vec<BalanceAdjustmentDto>,
        reason: String,
        source: BalanceAdjustmentSourceDto,
    },
}

impl EventDto {
    fn from_event(event: &LedgerEvent) -> Self {
        match event {
            LedgerEvent::ExpenseRecorded(event) => Self::ExpenseRecorded {
                paid_by: event
                    .paid_by()
                    .iter()
                    .map(MemberAmountDto::from_amount)
                    .collect(),
                owed_by: event
                    .owed_by()
                    .iter()
                    .map(MemberAmountDto::from_amount)
                    .collect(),
                note: event.note().map(|note| note.as_str().to_owned()),
            },
            LedgerEvent::NormalizedSettlementPlanRecorded(event) => {
                Self::NormalizedSettlementPlanRecorded {
                    transfers: event
                        .transfers()
                        .iter()
                        .map(TransferDto::from_transfer)
                        .collect(),
                }
            }
            LedgerEvent::LedgerHistorySealed(event) => Self::LedgerHistorySealed {
                through: event.through().0,
            },
            LedgerEvent::EntryVoided(event) => Self::EntryVoided {
                target: event.target().0,
            },
            LedgerEvent::BalanceAdjusted(event) => Self::BalanceAdjusted {
                adjustments: event
                    .adjustments()
                    .iter()
                    .map(BalanceAdjustmentDto::from_adjustment)
                    .collect(),
                reason: event.reason().as_str().to_owned(),
                source: BalanceAdjustmentSourceDto::from_source(event.source()),
            },
        }
    }
}

#[derive(Debug, Deserialize)]
struct EventKindProbe {
    kind: String,
}

#[derive(Debug, Deserialize)]
struct ExpenseRecordedEventDto {
    paid_by: Vec<MemberAmountDto>,
    owed_by: Vec<MemberAmountDto>,
    note: Option<String>,
}

#[derive(Debug, Deserialize)]
struct NormalizedSettlementPlanRecordedEventDto {
    transfers: Vec<TransferDto>,
}

#[derive(Debug, Deserialize)]
struct LedgerHistorySealedEventDto {
    through: u64,
}

#[derive(Debug, Deserialize)]
struct EntryVoidedEventDto {
    target: u64,
}

#[derive(Debug, Deserialize)]
struct BalanceAdjustedEventDto {
    adjustments: Vec<BalanceAdjustmentDto>,
    reason: String,
    source: BalanceAdjustmentSourceDto,
}

fn decode_event(
    value: serde_json::Value,
    entry_id: LedgerEntryId,
) -> Result<LedgerEvent, AttachmentCodecError> {
    let kind = serde_json::from_value::<EventKindProbe>(value.clone())
        .map_err(AttachmentCodecError::JsonDecode)?
        .kind;

    match kind.as_str() {
        "expense_recorded" => {
            let dto: ExpenseRecordedEventDto =
                serde_json::from_value(value).map_err(AttachmentCodecError::JsonDecode)?;
            Ok(LedgerEvent::ExpenseRecorded(
                ExpenseRecorded::new(
                    dto.paid_by
                        .into_iter()
                        .map(MemberAmountDto::into_amount)
                        .collect::<Result<_, _>>()?,
                    dto.owed_by
                        .into_iter()
                        .map(MemberAmountDto::into_amount)
                        .collect::<Result<_, _>>()?,
                    dto.note
                        .map(ExpenseNote::new)
                        .transpose()
                        .map_err(|_| AttachmentCodecError::InvalidNote)?,
                )
                .map_err(|_| AttachmentCodecError::InvalidExpenseEvent)?,
            ))
        }
        "normalized_settlement_plan_recorded" => {
            let dto: NormalizedSettlementPlanRecordedEventDto =
                serde_json::from_value(value).map_err(AttachmentCodecError::JsonDecode)?;
            Ok(LedgerEvent::NormalizedSettlementPlanRecorded(
                NormalizedSettlementPlanRecorded::new(
                    dto.transfers
                        .into_iter()
                        .map(TransferDto::into_transfer)
                        .collect::<Result<_, _>>()?,
                )
                .map_err(|_| AttachmentCodecError::InvalidSettlementEvent)?,
            ))
        }
        "ledger_history_sealed" => {
            let dto: LedgerHistorySealedEventDto =
                serde_json::from_value(value).map_err(AttachmentCodecError::JsonDecode)?;
            Ok(LedgerEvent::LedgerHistorySealed(LedgerHistorySealed::new(
                LedgerEntryId(dto.through),
            )))
        }
        "entry_voided" => {
            let dto: EntryVoidedEventDto =
                serde_json::from_value(value).map_err(AttachmentCodecError::JsonDecode)?;
            Ok(LedgerEvent::EntryVoided(EntryVoided::new(LedgerEntryId(
                dto.target,
            ))))
        }
        "balance_adjusted" => {
            let dto: BalanceAdjustedEventDto =
                serde_json::from_value(value).map_err(AttachmentCodecError::JsonDecode)?;
            Ok(LedgerEvent::BalanceAdjusted(
                BalanceAdjusted::new(
                    dto.adjustments
                        .into_iter()
                        .map(BalanceAdjustmentDto::into_adjustment)
                        .collect::<Result<_, _>>()?,
                    crate::ledger::AdjustmentReason::new(dto.reason)
                        .map_err(|_| AttachmentCodecError::InvalidAdjustmentReason)?,
                    dto.source.into_source()?,
                )
                .map_err(|_| AttachmentCodecError::InvalidAdjustmentEvent)?,
            ))
        }
        other => Err(AttachmentCodecError::UnknownEventVariant {
            kind: other.to_owned(),
            entry_id,
        }),
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct MemberAmountDto {
    member_id: u64,
    amount: String,
}

impl MemberAmountDto {
    fn from_amount(amount: &MemberAmount) -> Self {
        Self {
            member_id: amount.member_id.0,
            amount: encode_money(amount.amount),
        }
    }

    fn into_amount(self) -> Result<MemberAmount, AttachmentCodecError> {
        Ok(MemberAmount {
            member_id: MemberId(self.member_id),
            amount: parse_money(&self.amount)?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct TransferDto {
    from: u64,
    to: u64,
    amount: String,
}

impl TransferDto {
    fn from_transfer(transfer: &Transfer) -> Self {
        Self {
            from: transfer.from.0,
            to: transfer.to.0,
            amount: encode_money(transfer.amount),
        }
    }

    fn into_transfer(self) -> Result<Transfer, AttachmentCodecError> {
        Ok(Transfer {
            from: MemberId(self.from),
            to: MemberId(self.to),
            amount: parse_money(&self.amount)?,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct BalanceAdjustmentDto {
    member_id: u64,
    amount: String,
}

impl BalanceAdjustmentDto {
    fn from_adjustment(adjustment: &BalanceAdjustment) -> Self {
        Self {
            member_id: adjustment.member_id.0,
            amount: encode_money(adjustment.amount),
        }
    }

    fn into_adjustment(self) -> Result<BalanceAdjustment, AttachmentCodecError> {
        Ok(BalanceAdjustment {
            member_id: MemberId(self.member_id),
            amount: parse_money(&self.amount)?,
        })
    }
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum BalanceAdjustmentSourceDto {
    SealedEntryCorrection { entry_id: u64 },
    PriorAdjustmentCorrection { entry_id: u64 },
    ExternalCorrection,
}

impl BalanceAdjustmentSourceDto {
    fn from_source(source: &BalanceAdjustmentSource) -> Self {
        match source {
            BalanceAdjustmentSource::SealedEntryCorrection(entry_id) => {
                Self::SealedEntryCorrection {
                    entry_id: entry_id.0,
                }
            }
            BalanceAdjustmentSource::PriorAdjustmentCorrection(entry_id) => {
                Self::PriorAdjustmentCorrection {
                    entry_id: entry_id.0,
                }
            }
            BalanceAdjustmentSource::ExternalCorrection(_) => Self::ExternalCorrection,
        }
    }

    fn into_source(self) -> Result<BalanceAdjustmentSource, AttachmentCodecError> {
        match self {
            Self::SealedEntryCorrection { entry_id } => Ok(
                BalanceAdjustmentSource::SealedEntryCorrection(LedgerEntryId(entry_id)),
            ),
            Self::PriorAdjustmentCorrection { entry_id } => Ok(
                BalanceAdjustmentSource::PriorAdjustmentCorrection(LedgerEntryId(entry_id)),
            ),
            Self::ExternalCorrection => Ok(external_correction_source_for_transport_decode()),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum AttachmentCodecError {
    #[error("system clock error")]
    Clock,
    #[error("failed to encode canonical attachment: {0}")]
    JsonEncode(#[source] serde_json::Error),
    #[error("failed to decode canonical attachment: {0}")]
    JsonDecode(#[source] serde_json::Error),
    #[error("failed to read authoritative attachment: {0}")]
    UnreadableAttachment(String),
    #[error("unsupported canonical attachment version: {0}")]
    UnsupportedTransportVersion(u32),
    #[error("unknown canonical schema version: {version}{}", entry_id_suffix(*entry_id))]
    UnknownSchemaVersion {
        version: u32,
        entry_id: Option<LedgerEntryId>,
    },
    #[error("unknown ledger event variant: {kind} at entry {}", entry_id.0)]
    UnknownEventVariant {
        kind: String,
        entry_id: LedgerEntryId,
    },
    #[error("invalid hash length")]
    InvalidHashLength,
    #[error("invalid hash hex")]
    InvalidHashHex,
    #[error("invalid money value")]
    InvalidMoney,
    #[error("invalid source kind: {0}")]
    InvalidSourceKind(String),
    #[error("invalid source")]
    InvalidSource,
    #[error("invalid effective date")]
    InvalidEffectiveDate,
    #[error("invalid recorded_at timestamp")]
    InvalidRecordedAt,
    #[error("invalid allocation snapshot")]
    InvalidAllocationSnapshot,
    #[error("invalid note")]
    InvalidNote,
    #[error("invalid expense event")]
    InvalidExpenseEvent,
    #[error("invalid settlement event")]
    InvalidSettlementEvent,
    #[error("invalid adjustment reason")]
    InvalidAdjustmentReason,
    #[error("invalid adjustment event")]
    InvalidAdjustmentEvent,
    #[error("invalid hash suite: {0}")]
    InvalidHashSuite(String),
}

fn entry_id_suffix(entry_id: Option<LedgerEntryId>) -> String {
    match entry_id {
        Some(entry_id) => format!(" at entry {entry_id}"),
        None => String::new(),
    }
}

fn pre_self_link_content_sha256(content: &str) -> String {
    let digest = Sha256::digest(content.as_bytes());
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        use std::fmt::Write as _;
        let _ = write!(out, "{byte:02x}");
    }
    out
}

impl AttachmentCodecError {
    pub fn failing_entry_id(&self) -> Option<LedgerEntryId> {
        match self {
            Self::UnknownSchemaVersion { entry_id, .. } => *entry_id,
            Self::UnknownEventVariant { entry_id, .. } => Some(*entry_id),
            _ => None,
        }
    }
}

fn encode_hash(hash: EntryHash) -> String {
    hash.0.iter().map(|byte| format!("{byte:02x}")).collect()
}

fn decode_hash(hex: &str) -> Result<EntryHash, AttachmentCodecError> {
    if hex.len() != 64 {
        return Err(AttachmentCodecError::InvalidHashLength);
    }

    let mut out = [0_u8; 32];
    for (index, chunk) in hex.as_bytes().chunks_exact(2).enumerate() {
        let text = std::str::from_utf8(chunk).map_err(|_| AttachmentCodecError::InvalidHashHex)?;
        out[index] =
            u8::from_str_radix(text, 16).map_err(|_| AttachmentCodecError::InvalidHashHex)?;
    }
    Ok(EntryHash(out))
}

fn encode_money(money: Money) -> String {
    money.as_decimal().normalize().to_string()
}

fn parse_money(text: &str) -> Result<Money, AttachmentCodecError> {
    let decimal = Decimal::from_str(text).map_err(|_| AttachmentCodecError::InvalidMoney)?;
    Ok(Money::from_decimal(decimal))
}

fn system_time_to_unix_ms(value: SystemTime) -> Result<u64, AttachmentCodecError> {
    value
        .duration_since(UNIX_EPOCH)
        .map_err(|_| AttachmentCodecError::InvalidRecordedAt)?
        .as_millis()
        .try_into()
        .map_err(|_| AttachmentCodecError::InvalidRecordedAt)
}

fn unix_ms_to_system_time(value: u64) -> Result<SystemTime, AttachmentCodecError> {
    UNIX_EPOCH
        .checked_add(std::time::Duration::from_millis(value))
        .ok_or(AttachmentCodecError::InvalidRecordedAt)
}

fn hash_suite_name(hash_suite: LedgerHashSuite) -> &'static str {
    match hash_suite {
        LedgerHashSuite::Sha256V1 => "sha256_v1",
    }
}

fn parse_hash_suite(name: &str) -> Result<LedgerHashSuite, AttachmentCodecError> {
    match name {
        "sha256_v1" => Ok(LedgerHashSuite::Sha256V1),
        other => Err(AttachmentCodecError::InvalidHashSuite(other.to_owned())),
    }
}

fn source_kind_name(kind: LedgerSourceCanonicalKind) -> &'static str {
    match kind {
        LedgerSourceCanonicalKind::LegacyDsl => "legacy_dsl",
        LedgerSourceCanonicalKind::DiscordUi => "discord_ui",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{
        ledger_chain_genesis_sha256_v1, load_and_replay_verified_sha256_v1,
        make_unverified_envelope_sha256_v1,
    };
    use serde_json::Value;

    fn expense_entry() -> LedgerEntry {
        LedgerEntry::expense(
            LedgerEntryId(1),
            ExpenseRecorded::new(
                vec![MemberAmount {
                    member_id: MemberId(1),
                    amount: Money::from_i64(10_000),
                }],
                vec![MemberAmount {
                    member_id: MemberId(2),
                    amount: Money::from_i64(10_000),
                }],
                Some(ExpenseNote::new("ランチ").expect("note should parse")),
            )
            .expect("expense event should be valid"),
            AllocationSnapshot::Even,
        )
        .expect("entry should build")
    }

    #[test]
    fn canonical_attachment_round_trips_event_and_metadata() {
        let mut entry = expense_entry();
        entry.metadata.recorded_at = Some(UNIX_EPOCH + std::time::Duration::from_secs(42));
        let envelope = make_unverified_envelope_sha256_v1(
            walicord_ledger::test_fixtures::ledger_id(77),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            (),
            entry.clone(),
        )
        .expect("envelope should build");

        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let decoded =
            CanonicalAttachmentCodec::decode(&encoded, 123_u64).expect("attachment should decode");

        assert_eq!(decoded.previous_hash, envelope.previous_hash);
        assert_eq!(decoded.entry_hash, envelope.entry_hash);
        assert_eq!(decoded.payload.ledger_id, envelope.payload.ledger_id);
        assert_eq!(decoded.payload.entry, entry);
    }

    #[test]
    fn canonical_attachment_decode_uses_transport_fallback_shape_for_legacy_metadata() {
        let entry = expense_entry();
        let envelope = make_unverified_envelope_sha256_v1(
            walicord_ledger::test_fixtures::ledger_id(77),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            (),
            entry,
        )
        .expect("envelope should build");
        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let mut value: Value = serde_json::from_slice(&encoded).expect("json should parse");
        value["envelope"]["payload"]["entry"]["metadata"]
            .as_object_mut()
            .expect("metadata should be an object")
            .remove("recorded_at_unix_ms");

        let decoded = CanonicalAttachmentCodec::decode(
            &serde_json::to_vec(&value).expect("json should serialize"),
            123_u64,
        )
        .expect("legacy metadata should decode");

        assert_eq!(decoded.payload.entry.metadata.recorded_at, None);
    }

    #[test]
    fn canonical_attachment_encode_omits_recorded_at_when_metadata_does_not_have_it() {
        let entry = expense_entry();
        let envelope = make_unverified_envelope_sha256_v1(
            walicord_ledger::test_fixtures::ledger_id(77),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            (),
            entry,
        )
        .expect("envelope should build");

        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let value: Value = serde_json::from_slice(&encoded).expect("json should parse");

        assert!(
            value["envelope"]["payload"]["entry"]["metadata"]
                .as_object()
                .expect("metadata should be an object")
                .get("recorded_at_unix_ms")
                .is_none()
        );
    }

    #[test]
    fn canonical_attachment_replays_through_v1_load_path() {
        let ledger_id = walicord_ledger::test_fixtures::ledger_id(77);
        let entry = expense_entry();
        let envelope = make_unverified_envelope_sha256_v1(
            ledger_id,
            ledger_chain_genesis_sha256_v1(ledger_id),
            (),
            entry.clone(),
        )
        .expect("legacy envelope should build");

        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let decoded =
            CanonicalAttachmentCodec::decode(&encoded, 123_u64).expect("attachment should decode");
        let (verified, projected) = load_and_replay_verified_sha256_v1(vec![decoded], ledger_id)
            .expect("attachment should replay");

        assert_eq!(verified[0].payload().entry, entry);
        assert_eq!(
            projected.state().balances().get(&MemberId(1)),
            Some(&Money::from_i64(10_000))
        );
    }

    #[test]
    fn decode_rejects_unknown_transport_version() {
        let entry = expense_entry();
        let envelope = make_unverified_envelope_sha256_v1(
            walicord_ledger::test_fixtures::ledger_id(77),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            (),
            entry,
        )
        .expect("envelope should build");
        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let mut value: Value = serde_json::from_slice(&encoded).expect("json should parse");
        value["version"] = Value::from(2_u64);

        let actual = CanonicalAttachmentCodec::decode(
            &serde_json::to_vec(&value).expect("json should serialize"),
            123_u64,
        );

        assert!(matches!(
            actual,
            Err(AttachmentCodecError::UnsupportedTransportVersion(2))
        ));
    }

    #[test]
    fn decode_rejects_zero_ledger_id() {
        let entry = expense_entry();
        let envelope = make_unverified_envelope_sha256_v1(
            walicord_ledger::test_fixtures::ledger_id(77),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            (),
            entry,
        )
        .expect("envelope should build");
        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let mut value: Value = serde_json::from_slice(&encoded).expect("json should parse");
        value["envelope"]["payload"]["ledger_id"] = Value::from(0_u64);

        let actual = CanonicalAttachmentCodec::decode(
            &serde_json::to_vec(&value).expect("json should serialize"),
            123_u64,
        );

        assert!(matches!(actual, Err(AttachmentCodecError::JsonDecode(_))));
    }

    #[test]
    fn decode_rejects_unknown_schema_version() {
        let entry = expense_entry();
        let envelope = make_unverified_envelope_sha256_v1(
            walicord_ledger::test_fixtures::ledger_id(77),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            (),
            entry,
        )
        .expect("envelope should build");
        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let mut value: Value = serde_json::from_slice(&encoded).expect("json should parse");
        value["envelope"]["payload"]["schema_version"] = Value::from(2_u64);

        let actual = CanonicalAttachmentCodec::decode(
            &serde_json::to_vec(&value).expect("json should serialize"),
            123_u64,
        );

        assert!(matches!(
            actual,
            Err(AttachmentCodecError::UnknownSchemaVersion {
                version: 2,
                entry_id: Some(LedgerEntryId(1)),
            })
        ));
    }

    #[test]
    fn decode_rejects_unknown_schema_version_before_future_entry_shape_decode() {
        let entry = expense_entry();
        let envelope = make_unverified_envelope_sha256_v1(
            walicord_ledger::test_fixtures::ledger_id(77),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            (),
            entry,
        )
        .expect("envelope should build");
        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let mut value: Value = serde_json::from_slice(&encoded).expect("json should parse");
        value["envelope"]["payload"]["schema_version"] = Value::from(2_u64);
        value["envelope"]["payload"]["entry"]["metadata"] = Value::from("future-metadata-shape");

        let actual = CanonicalAttachmentCodec::decode(
            &serde_json::to_vec(&value).expect("json should serialize"),
            123_u64,
        );

        assert!(matches!(
            actual,
            Err(AttachmentCodecError::UnknownSchemaVersion {
                version: 2,
                entry_id: Some(LedgerEntryId(1)),
            })
        ));
    }

    #[test]
    fn decode_rejects_unknown_event_variant() {
        let entry = expense_entry();
        let envelope = make_unverified_envelope_sha256_v1(
            walicord_ledger::test_fixtures::ledger_id(77),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            (),
            entry,
        )
        .expect("envelope should build");
        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let mut value: Value = serde_json::from_slice(&encoded).expect("json should parse");
        value["envelope"]["payload"]["entry"]["event"]["kind"] = Value::from("future_event");

        let actual = CanonicalAttachmentCodec::decode(
            &serde_json::to_vec(&value).expect("json should serialize"),
            123_u64,
        );

        assert!(matches!(
            actual,
            Err(AttachmentCodecError::UnknownEventVariant { kind, entry_id })
                if kind == "future_event" && entry_id == LedgerEntryId(1)
        ));
    }

    #[test]
    fn decode_rejects_unknown_hash_suite() {
        let entry = expense_entry();
        let envelope = make_unverified_envelope_sha256_v1(
            walicord_ledger::test_fixtures::ledger_id(77),
            ledger_chain_genesis_sha256_v1(walicord_ledger::test_fixtures::ledger_id(77)),
            (),
            entry,
        )
        .expect("envelope should build");
        let encoded =
            CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let mut value: Value = serde_json::from_slice(&encoded).expect("json should parse");
        value["envelope"]["payload"]["hash_suite"] = Value::from("sha512_v2");

        let actual = CanonicalAttachmentCodec::decode(
            &serde_json::to_vec(&value).expect("json should serialize"),
            123_u64,
        );

        assert!(matches!(
            actual,
            Err(AttachmentCodecError::InvalidHashSuite(suite)) if suite == "sha512_v2"
        ));
    }
}
