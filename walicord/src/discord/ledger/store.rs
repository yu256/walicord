use super::{
    LEDGER_ATTACHMENT_FILENAME, fetch_all_channel_messages, response_writer::safe_create_message,
};
use serenity::{
    all::{ChannelId, GuildId, Message, MessageId, Permissions, UserId},
    builder::{EditThread, GetMessages},
    http::Http,
    prelude::Context,
};
use sha2::{Digest as _, Sha256};
use std::{
    collections::{BTreeMap, BTreeSet},
    future::Future,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
    time::{Duration, SystemTime, UNIX_EPOCH},
};
use walicord_application::ledger::{
    LedgerEntry, LedgerEntryId, LedgerEvent, LedgerId, LedgerLoadError, LedgerReplayError,
    UnverifiedLedgerStoreEnvelope, VerifiedLedgerStoreEnvelope,
    canonical_attachment::{AttachmentCodecError, CanonicalAttachmentCodec},
    observability::LedgerObservabilityEvent,
    projection::VerifiedEntryTransport,
    replay_verified_snapshot, verify_envelope_sha256_v1,
    verify_envelopes_in_append_order_sha256_v1,
    write_coordinator::{CanonicalMessageProbe, LAZY_RETRY_SCAN_WINDOW},
};

use super::observability::{
    DiscordLedgerObservability, DiscordLedgerObservabilityEvent, PermissionAction,
};

pub type VerifiedLedgerThreadLoad =
    walicord_application::ledger::projection::VerifiedLedgerThreadLoad<MessageId>;

pub const MAX_AUTHORITATIVE_ATTACHMENT_BYTES: u32 = 64 * 1024;
pub const CANONICAL_LOAD_TIMEOUT: Duration = Duration::from_secs(30);
pub const CANONICAL_LOAD_WARNING: Duration = Duration::from_secs(20);
pub const CANONICAL_WRITE_TIMEOUT: Duration = Duration::from_secs(30);
const IMMEDIATE_SELF_LINK_EDIT_WINDOW: Duration = Duration::from_secs(120);
const STABLE_SNAPSHOT_ATTEMPTS: usize = 3;

type DisplayDriftGuard = fn(&DisplayDriftObservation) -> Result<(), StoreLoadError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DisplayDriftObservation {
    pub ledger_id: LedgerId,
    pub entry_id: LedgerEntryId,
    pub message_id: MessageId,
    pub guild_id: Option<GuildId>,
    pub channel_id: ChannelId,
    pub recorded_at: SystemTime,
    pub edited_at: Option<SystemTime>,
    pub content: String,
    expected_pre_self_link_content_sha256: Option<String>,
    expected_first_line_prefixes: Vec<String>,
}

impl DisplayDriftObservation {
    fn from_record(
        ledger_id: LedgerId,
        envelope: &VerifiedLedgerStoreEnvelope<MessageId>,
        record: &CanonicalMessageRecord,
    ) -> Self {
        Self {
            ledger_id,
            entry_id: envelope.payload().entry.id,
            message_id: record.message_id,
            guild_id: record.guild_id,
            channel_id: record.channel_id,
            recorded_at: record.recorded_at,
            edited_at: record.edited_at,
            content: record.content.clone(),
            expected_pre_self_link_content_sha256: record
                .expected_pre_self_link_content_sha256
                .clone(),
            expected_first_line_prefixes: expected_canonical_message_prefixes(
                &envelope.payload().entry,
            ),
        }
    }
}

impl CanonicalMessageRecord {
    fn has_authoritative_attachment(&self) -> bool {
        self.attachments
            .iter()
            .any(|attachment| attachment.filename == LEDGER_ATTACHMENT_FILENAME)
    }

    fn should_validate_writer_lineage(&self) -> bool {
        self.webhook_id.is_some() || self.author_is_bot || self.has_authoritative_attachment()
    }
}

fn recovery_reference_ledger_id_short(ledger_id: LedgerId) -> String {
    format!("{ledger_id:08x}")
}

fn expected_canonical_message_prefixes(entry: &LedgerEntry) -> Vec<String> {
    match &entry.event {
        LedgerEvent::ExpenseRecorded(_) => {
            vec![format!(
                "{}",
                walicord_i18n::public_expense_header(entry.id.0)
            )]
        }
        LedgerEvent::NormalizedSettlementPlanRecorded(_) => vec![format!(
            "{}",
            walicord_i18n::public_settlement_header(entry.id.0)
        )],
        LedgerEvent::EntryVoided(_) => vec![format!("[#{}] ", entry.id.0)],
        LedgerEvent::LedgerHistorySealed(_) => vec![
            format!("{}", walicord_i18n::public_seal_header(entry.id.0))
                .split_once("[#")
                .map(|(prefix, _)| prefix.to_owned())
                .unwrap_or_else(|| format!("{}", walicord_i18n::public_seal_header(entry.id.0))),
        ],
        LedgerEvent::BalanceAdjusted(_) => vec![
            format!(
                "{}",
                walicord_i18n::public_balance_adjustment_header(entry.id.0)
            )
            .split_once("[#")
            .map(|(prefix, _)| prefix.to_owned())
            .unwrap_or_else(|| {
                format!(
                    "{}",
                    walicord_i18n::public_balance_adjustment_header(entry.id.0)
                )
            }),
        ],
    }
}

impl PendingCanonicalMessageRecord {
    fn has_authoritative_attachment(&self) -> bool {
        self.attachments
            .iter()
            .any(|attachment| attachment.filename == LEDGER_ATTACHMENT_FILENAME)
    }

    fn should_validate_writer_lineage(&self) -> bool {
        self.webhook_id.is_some() || self.author_is_bot || self.has_authoritative_attachment()
    }
}

trait LineageRecord {
    fn message_id(&self) -> MessageId;
    fn author_id(&self) -> UserId;
    fn webhook_id(&self) -> Option<u64>;
}

fn build_transport_entries(
    canonical_thread_id: ChannelId,
    verified: &[VerifiedLedgerStoreEnvelope<MessageId>],
    records: &[CanonicalMessageRecord],
) -> Result<BTreeMap<LedgerEntryId, VerifiedEntryTransport>, StoreLoadError> {
    let records_by_message_id: BTreeMap<MessageId, &CanonicalMessageRecord> = records
        .iter()
        .map(|record| (record.message_id, record))
        .collect();
    let mut by_entry_id = BTreeMap::new();

    for envelope in verified {
        let message_id = *envelope.external_id();
        let entry_id = envelope.payload().entry.id;
        let record =
            records_by_message_id
                .get(&message_id)
                .ok_or(StoreLoadError::MetadataCoherence(
                    MetadataCoherenceFailure::MissingTransportMetadata {
                        entry_id,
                        message_id,
                    },
                ))?;
        let guild_id = record.guild_id.ok_or(StoreLoadError::MetadataCoherence(
            MetadataCoherenceFailure::MissingGuildContext { message_id },
        ))?;
        if record.channel_id != canonical_thread_id {
            return Err(StoreLoadError::MetadataCoherence(
                MetadataCoherenceFailure::MismatchedChannel {
                    message_id,
                    expected: canonical_thread_id,
                    actual: record.channel_id,
                },
            ));
        }

        by_entry_id.insert(
            entry_id,
            VerifiedEntryTransport::new(
                canonical_message_link(guild_id, canonical_thread_id, message_id),
                record.recorded_at,
            ),
        );
    }

    Ok(by_entry_id)
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum MetadataCoherenceFailure {
    #[error("verified entry {entry_id:?} is missing transport metadata for message {message_id}")]
    MissingTransportMetadata {
        entry_id: LedgerEntryId,
        message_id: MessageId,
    },
    #[error("message {message_id} is missing guild context for a recovery link")]
    MissingGuildContext { message_id: MessageId },
    #[error(
        "message {message_id} was loaded from channel {actual} but expected canonical thread {expected}"
    )]
    MismatchedChannel {
        message_id: MessageId,
        expected: ChannelId,
        actual: ChannelId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum WriterLineagePolicyError {
    #[error("writer lineage allowlist is unavailable")]
    MissingAllowlist,
    #[error("writer lineage active writer is unavailable")]
    MissingActiveWriter,
    #[error("writer lineage allowlist is empty")]
    EmptyAllowlist,
    #[error("active writer {active_writer} is not present in the approved writer lineage")]
    ActiveWriterNotApproved { active_writer: UserId },
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum WriterLineageFailure {
    #[error("{0}")]
    PolicyUnavailable(#[from] WriterLineagePolicyError),
    #[error("webhook-authored canonical candidate found at message {message_id}")]
    WebhookAuthor {
        message_id: MessageId,
        author_id: UserId,
    },
    #[error("author {author_id} is not approved for canonical history at message {message_id}")]
    UnapprovedAuthor {
        message_id: MessageId,
        author_id: UserId,
    },
    #[error(
        "historical writer {author_id} reappeared after active writer {active_writer} at message {message_id}"
    )]
    NonActiveWriterAfterCutover {
        message_id: MessageId,
        author_id: UserId,
        active_writer: UserId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WriterLineagePolicy {
    active_writer: UserId,
    approved_writers: BTreeSet<UserId>,
}

impl WriterLineagePolicy {
    pub fn load<I>(
        active_writer: Option<UserId>,
        approved_writers: Option<I>,
    ) -> Result<Self, WriterLineagePolicyError>
    where
        I: IntoIterator<Item = UserId>,
    {
        let active_writer = active_writer.ok_or(WriterLineagePolicyError::MissingActiveWriter)?;
        let approved_writers: BTreeSet<UserId> = approved_writers
            .ok_or(WriterLineagePolicyError::MissingAllowlist)?
            .into_iter()
            .collect();
        if approved_writers.is_empty() {
            return Err(WriterLineagePolicyError::EmptyAllowlist);
        }
        if !approved_writers.contains(&active_writer) {
            return Err(WriterLineagePolicyError::ActiveWriterNotApproved { active_writer });
        }

        Ok(Self {
            active_writer,
            approved_writers,
        })
    }

    fn validate_records<R>(&self, records: &[R]) -> Result<(), WriterLineageFailure>
    where
        R: LineageRecord,
    {
        let mut seen_active_writer = false;
        for record in records {
            if record.webhook_id().is_some() {
                return Err(WriterLineageFailure::WebhookAuthor {
                    message_id: record.message_id(),
                    author_id: record.author_id(),
                });
            }
            if !self.approved_writers.contains(&record.author_id()) {
                return Err(WriterLineageFailure::UnapprovedAuthor {
                    message_id: record.message_id(),
                    author_id: record.author_id(),
                });
            }
            if record.author_id() == self.active_writer {
                seen_active_writer = true;
                continue;
            }
            if seen_active_writer {
                return Err(WriterLineageFailure::NonActiveWriterAfterCutover {
                    message_id: record.message_id(),
                    author_id: record.author_id(),
                    active_writer: self.active_writer,
                });
            }
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum StoreLoadError {
    #[error("failed to fetch canonical thread data: {0}")]
    Fetch(String),
    #[error("canonical thread load timed out after {elapsed:?}")]
    FetchTimeout { elapsed: Duration },
    #[error(
        "message {message_id} has invalid attachment cardinality: total={total_attachments}, authoritative={authoritative_attachments}"
    )]
    AttachmentCardinality {
        message_id: MessageId,
        total_attachments: usize,
        authoritative_attachments: usize,
    },
    #[error(
        "message {message_id} advertises an oversize authoritative attachment: {size_bytes} bytes"
    )]
    OversizeAttachment {
        message_id: MessageId,
        size_bytes: u32,
    },
    #[error("message {message_id} authoritative attachment failed to decode: {error}")]
    Decode {
        message_id: MessageId,
        error: AttachmentCodecError,
    },
    #[error("canonical writer lineage rejected history: {0}")]
    WriterLineage(WriterLineageFailure),
    #[error("canonical chain verification failed: {0:?}")]
    Chain(LedgerLoadError),
    #[error("canonical entry structure failed replay: {0:?}")]
    Structure(LedgerReplayError),
    #[error("canonical projection failed replay: {0:?}")]
    Projection(LedgerReplayError),
    #[error("canonical message metadata is inconsistent with replayed history: {0}")]
    MetadataCoherence(MetadataCoherenceFailure),
    #[error("canonical display drift detected at message {message_id}")]
    DisplayDrift { message_id: MessageId },
    #[error("canonical thread permissions are not sufficient: {0}")]
    Permission(String),
}

impl StoreLoadError {
    fn from_replay(error: LedgerReplayError) -> Self {
        match error {
            LedgerReplayError::Structure(_) => Self::Structure(error),
            LedgerReplayError::Projection(_) => Self::Projection(error),
        }
    }
}

pub(crate) fn serenity_error_is_read_denied(error: &serenity::Error) -> bool {
    matches!(
        error,
        serenity::Error::Http(http_error) if read_denied_status(http_error.status_code())
    )
}

fn read_denied_status(status_code: Option<serenity::http::StatusCode>) -> bool {
    matches!(
        status_code,
        Some(
            serenity::http::StatusCode::FORBIDDEN
                | serenity::http::StatusCode::UNAUTHORIZED
                | serenity::http::StatusCode::NOT_FOUND
        )
    )
}

fn classify_send_error(error: serenity::Error) -> StoreWriteError {
    if serenity_error_is_read_denied(&error) {
        if matches!(
            &error,
            serenity::Error::Http(serenity::all::HttpError::UnsuccessfulRequest(response))
                if response.status_code == serenity::all::StatusCode::FORBIDDEN
        ) {
            StoreWriteError::Permission(format!("send forbidden: {error}"))
        } else if matches!(
            &error,
            serenity::Error::Http(serenity::all::HttpError::UnsuccessfulRequest(response))
                if response.status_code == serenity::all::StatusCode::NOT_FOUND
        ) {
            StoreWriteError::Permission(format!("send target hidden or missing: {error}"))
        } else {
            StoreWriteError::Permission(format!("send unauthorized: {error}"))
        }
    } else {
        StoreWriteError::Transport(format!("send failed: {error}"))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AppendThreadPreparation {
    Ready,
    Unarchive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AppendThreadPreparationError {
    NotThreadOrLocked,
}

fn append_thread_preparation(
    thread_metadata: Option<(bool, bool)>,
) -> Result<AppendThreadPreparation, AppendThreadPreparationError> {
    match thread_metadata {
        Some((_, true)) | None => Err(AppendThreadPreparationError::NotThreadOrLocked),
        Some((true, false)) => Ok(AppendThreadPreparation::Unarchive),
        Some((false, false)) => Ok(AppendThreadPreparation::Ready),
    }
}

fn missing_append_permission(current: Permissions) -> Option<PermissionAction> {
    [
        (Permissions::VIEW_CHANNEL, PermissionAction::ViewChannel),
        (
            Permissions::READ_MESSAGE_HISTORY,
            PermissionAction::ReadMessageHistory,
        ),
        (
            Permissions::SEND_MESSAGES_IN_THREADS,
            PermissionAction::SendMessageInChannel,
        ),
        (Permissions::ATTACH_FILES, PermissionAction::AttachFiles),
    ]
    .into_iter()
    .find_map(|(required, action)| (!current.contains(required)).then_some(action))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ReadBackAttachmentBytesError {
    Drift,
}

fn verify_read_back_attachment_bytes(
    expected: &[u8],
    actual: &[u8],
) -> Result<(), ReadBackAttachmentBytesError> {
    if actual == expected {
        Ok(())
    } else {
        Err(ReadBackAttachmentBytesError::Drift)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum StoreWriteError {
    /// Canonical attachment encoding failed (codec rejected the envelope).
    #[error("canonical attachment encode failed: {0}")]
    Prepare(AttachmentCodecError),
    /// Bot or actor permissions are missing for posting / attaching files /
    /// unarchiving. The detail string is the Discord-native diagnostic.
    #[error("canonical thread permissions are not sufficient: {0}")]
    Permission(String),
    /// The canonical thread is archived or locked and unarchive is not possible.
    #[error("canonical thread is archived or locked")]
    ArchivedOrLocked,
    /// Generic Discord transport / rate-limit failure during the post call.
    #[error("canonical thread post failed: {0}")]
    Transport(String),
    /// Send appeared to succeed but the read-back fetch returned a payload that does
    /// not match the canonical bytes we just posted.
    #[error("canonical thread read-back verification failed: {0}")]
    ReadBack(String),
    /// Operation exceeded the write timeout budget.
    #[error("canonical thread write timed out after {elapsed:?}")]
    WriteTimeout { elapsed: Duration },
}

#[derive(Clone)]
pub struct DiscordCanonicalLedgerStore {
    writer_lineage: WriterLineagePolicy,
    display_drift_guard: DisplayDriftGuard,
    observability: Arc<dyn DiscordLedgerObservability>,
}

impl DiscordCanonicalLedgerStore {
    pub fn new(
        writer_lineage: WriterLineagePolicy,
        observability: Arc<dyn DiscordLedgerObservability>,
    ) -> Self {
        Self {
            writer_lineage,
            display_drift_guard: allow_immediate_self_link_completion_edit_only,
            observability,
        }
    }

    #[cfg(test)]
    pub fn new_with_display_drift_guard(
        writer_lineage: WriterLineagePolicy,
        display_drift_guard: DisplayDriftGuard,
        observability: Arc<dyn DiscordLedgerObservability>,
    ) -> Self {
        Self {
            writer_lineage,
            display_drift_guard,
            observability,
        }
    }

    fn validate_writer_lineage<R>(
        &self,
        ledger_id: Option<LedgerId>,
        records: &[R],
    ) -> Result<(), StoreLoadError>
    where
        R: LineageRecord,
    {
        self.writer_lineage
            .validate_records(records)
            .map_err(|failure| {
                let observed_writer = match &failure {
                    WriterLineageFailure::UnapprovedAuthor { author_id, .. }
                    | WriterLineageFailure::NonActiveWriterAfterCutover { author_id, .. } => {
                        Some(*author_id)
                    }
                    WriterLineageFailure::WebhookAuthor { author_id, .. } => Some(*author_id),
                    WriterLineageFailure::PolicyUnavailable(_) => None,
                };
                if let Some(observed_writer) = observed_writer {
                    self.observability.emit_discord(
                        DiscordLedgerObservabilityEvent::ActiveActiveMisconfiguration {
                            ledger_id,
                            observed_writer,
                            expected_writer: self.writer_lineage.active_writer,
                        },
                    );
                }
                StoreLoadError::WriterLineage(failure)
            })
    }

    pub(crate) fn observe_permission_failure(
        &self,
        ledger_id: Option<LedgerId>,
        channel_id: ChannelId,
        action: PermissionAction,
    ) {
        self.observability
            .emit_discord(DiscordLedgerObservabilityEvent::PermissionFailure {
                ledger_id,
                guild_id: None,
                channel_id,
                action,
            });
    }

    fn observe_load_permission_result<T>(
        &self,
        ledger_id: Option<LedgerId>,
        channel_id: ChannelId,
        result: Result<T, StoreLoadError>,
    ) -> Result<T, StoreLoadError> {
        if matches!(result, Err(StoreLoadError::Permission(_))) {
            self.observe_permission_failure(
                ledger_id,
                channel_id,
                PermissionAction::ReadMessageHistory,
            );
        }
        result
    }

    /// Post a canonical entry to the bound thread, then read the just-posted message
    /// back to verify the authoritative attachment survived intact (criterion 54-55,
    /// 148, 209-212). The function does no locking — the caller is expected to hold
    /// the per-`LedgerId` mutex from `WriteCoordinator` for the duration. On a
    /// successful post + verify, it returns the verified envelope keyed by the
    /// Discord `MessageId`.
    pub async fn append_authoritative(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
        prepared_body: &str,
    ) -> Result<VerifiedLedgerStoreEnvelope<MessageId>, StoreWriteError> {
        let result = with_write_timeout(
            CANONICAL_WRITE_TIMEOUT,
            self.append_authoritative_inner(ctx, canonical_thread_id, envelope, prepared_body),
        )
        .await;
        if matches!(result, Err(StoreWriteError::Permission(_))) {
            self.observe_permission_failure(
                Some(envelope.payload.ledger_id),
                canonical_thread_id,
                PermissionAction::AppendCanonicalMessage,
            );
        }
        result
    }

    async fn append_authoritative_inner(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        envelope: &UnverifiedLedgerStoreEnvelope<()>,
        prepared_body: &str,
    ) -> Result<VerifiedLedgerStoreEnvelope<MessageId>, StoreWriteError> {
        let attachment_bytes = CanonicalAttachmentCodec::encode_with_pre_self_link_content(
            envelope,
            Some(prepared_body),
        )
        .map_err(StoreWriteError::Prepare)?;
        self.prepare_thread_for_append(ctx, canonical_thread_id, envelope.payload.ledger_id)
            .await?;

        let send_outcome = canonical_thread_id
            .send_message(
                &ctx.http,
                safe_create_message().content(prepared_body).add_file(
                    serenity::all::CreateAttachment::bytes(
                        attachment_bytes.clone(),
                        LEDGER_ATTACHMENT_FILENAME,
                    ),
                ),
            )
            .await
            .map_err(classify_send_error)?;

        let read_back = canonical_thread_id
            .message(&ctx.http, send_outcome.id)
            .await
            .map_err(|error| {
                if serenity_error_is_read_denied(&error) {
                    self.observe_permission_failure(
                        Some(envelope.payload.ledger_id),
                        canonical_thread_id,
                        PermissionAction::ReadMessageHistory,
                    );
                    StoreWriteError::Permission(format!("fetch read-back forbidden: {error}"))
                } else {
                    StoreWriteError::ReadBack(format!("fetch read-back failed: {error}"))
                }
            })?;

        let mut matching_attachments = read_back
            .attachments
            .iter()
            .filter(|attachment| attachment.filename == LEDGER_ATTACHMENT_FILENAME);
        let authoritative = matching_attachments.next().ok_or_else(|| {
            StoreWriteError::ReadBack("authoritative attachment missing on read-back".to_owned())
        })?;
        if matching_attachments.next().is_some() {
            return Err(StoreWriteError::ReadBack(
                "more than one authoritative attachment on read-back".to_owned(),
            ));
        }
        if (authoritative.size as usize) != attachment_bytes.len() {
            return Err(StoreWriteError::ReadBack(format!(
                "attachment size drift: expected {expected}, got {actual}",
                expected = attachment_bytes.len(),
                actual = authoritative.size,
            )));
        }
        let read_back_attachment_bytes = authoritative.download().await.map_err(|error| {
            StoreWriteError::ReadBack(format!(
                "authoritative attachment download failed on read-back: {error}"
            ))
        })?;
        verify_read_back_attachment_bytes(&attachment_bytes, &read_back_attachment_bytes).map_err(
            |_| {
                StoreWriteError::ReadBack(
                    "authoritative attachment bytes drifted on read-back".to_owned(),
                )
            },
        )?;
        if read_back.content != prepared_body {
            return Err(StoreWriteError::ReadBack(
                "message body drift between send and read-back".to_owned(),
            ));
        }

        let UnverifiedLedgerStoreEnvelope {
            previous_hash,
            entry_hash,
            payload,
            ..
        } = envelope.clone();
        let envelope_with_message_id = UnverifiedLedgerStoreEnvelope {
            previous_hash,
            entry_hash,
            external_id: send_outcome.id,
            payload: payload.clone(),
        };
        let verified = verify_envelope_sha256_v1(envelope_with_message_id, payload.ledger_id)
            .map_err(|error| {
                StoreWriteError::ReadBack(format!("envelope verification failed: {error:?}"))
            })?;
        Ok(verified)
    }

    async fn prepare_thread_for_append(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
    ) -> Result<(), StoreWriteError> {
        let channel = canonical_thread_id
            .to_channel(&ctx.http)
            .await
            .map_err(classify_send_error)?
            .guild()
            .ok_or(StoreWriteError::ArchivedOrLocked)?;
        let preparation = append_thread_preparation(
            channel
                .thread_metadata
                .map(|metadata| (metadata.archived, metadata.locked)),
        )
        .map_err(|_| StoreWriteError::ArchivedOrLocked)?;
        let current_permissions = {
            let guild = ctx.cache.guild(channel.guild_id).ok_or_else(|| {
                StoreWriteError::Permission("guild permission cache is unavailable".to_owned())
            })?;
            let bot_user_id = ctx.cache.current_user().id;
            let member = guild.members.get(&bot_user_id).ok_or_else(|| {
                StoreWriteError::Permission("bot member permission cache is unavailable".to_owned())
            })?;
            guild.user_permissions_in(&channel, member)
        };
        if let Some(missing) = missing_append_permission(current_permissions) {
            return Err(StoreWriteError::Permission(format!(
                "missing Discord permission for canonical append: {missing:?}"
            )));
        }
        if preparation == AppendThreadPreparation::Unarchive {
            canonical_thread_id
                .edit_thread(&ctx.http, EditThread::new().archived(false))
                .await
                .map_err(|error| {
                    if serenity_error_is_read_denied(&error) {
                        self.observe_permission_failure(
                            Some(ledger_id),
                            canonical_thread_id,
                            PermissionAction::UnarchiveThread,
                        );
                        StoreWriteError::Permission(format!(
                            "canonical thread unarchive forbidden: {error}"
                        ))
                    } else {
                        StoreWriteError::ArchivedOrLocked
                    }
                })?;
        }
        Ok(())
    }

    pub async fn load_verified_thread(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
        route_label: &'static str,
    ) -> Result<VerifiedLedgerThreadLoad, StoreLoadError> {
        let fetched_entry_count = Arc::new(AtomicUsize::new(0));
        let warning_count = Arc::clone(&fetched_entry_count);
        let timeout_count = Arc::clone(&fetched_entry_count);
        let warning_observability = Arc::clone(&self.observability);
        let timeout_observability = Arc::clone(&self.observability);
        let result = with_load_timeout(
            CANONICAL_LOAD_TIMEOUT,
            CANONICAL_LOAD_WARNING,
            move || {
                warning_observability.emit(LedgerObservabilityEvent::LoadTimeoutWarning {
                    ledger_id,
                    elapsed: CANONICAL_LOAD_WARNING,
                    route_label,
                    fetched_entry_count: warning_count.load(Ordering::Relaxed),
                });
            },
            move || {
                timeout_observability.emit(LedgerObservabilityEvent::LoadTimeout {
                    ledger_id,
                    elapsed: CANONICAL_LOAD_TIMEOUT,
                    route_label,
                    fetched_entry_count: timeout_count.load(Ordering::Relaxed),
                });
            },
            async {
                let messages = fetch_stable_channel_messages(ctx, canonical_thread_id).await?;
                fetched_entry_count.store(messages.len(), Ordering::Relaxed);
                let mut pending_records = Vec::with_capacity(messages.len());
                for message in messages {
                    pending_records.push(pending_canonical_message_record(message));
                }
                let pending_records: Vec<PendingCanonicalMessageRecord> = pending_records
                    .into_iter()
                    .filter(PendingCanonicalMessageRecord::should_validate_writer_lineage)
                    .collect();
                self.validate_writer_lineage(Some(ledger_id), &pending_records)?;

                let mut records = Vec::with_capacity(pending_records.len());
                for record in pending_records {
                    records.push(download_canonical_message_record(record).await?);
                    fetched_entry_count.store(records.len(), Ordering::Relaxed);
                }

                let display_drift_guard = self.display_drift_guard;
                self.load_verified_thread_from_candidate_records_with_guard(
                    canonical_thread_id,
                    ledger_id,
                    records,
                    move |envelope, record| {
                        let observation =
                            DisplayDriftObservation::from_record(ledger_id, envelope, record);
                        display_drift_guard(&observation)
                    },
                )
            },
        )
        .await;
        self.observe_load_permission_result(Some(ledger_id), canonical_thread_id, result)
    }

    pub async fn scan_recent_canonical_messages(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
    ) -> Result<Vec<CanonicalMessageProbe>, StoreLoadError> {
        let result = self
            .scan_recent_canonical_messages_inner(ctx, canonical_thread_id, ledger_id)
            .await;
        self.observe_load_permission_result(Some(ledger_id), canonical_thread_id, result)
    }

    async fn scan_recent_canonical_messages_inner(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
    ) -> Result<Vec<CanonicalMessageProbe>, StoreLoadError> {
        let mut pending_records =
            fetch_recent_canonical_pending_records(ctx, canonical_thread_id).await?;
        pending_records.reverse();
        self.validate_writer_lineage(Some(ledger_id), &pending_records)?;
        pending_records.reverse();

        let mut probes = Vec::new();
        for pending in pending_records.into_iter().take(LAZY_RETRY_SCAN_WINDOW) {
            let record = download_canonical_message_record(pending).await?;
            let authoritative: Vec<&CanonicalAttachmentCandidate> = record
                .attachments
                .iter()
                .filter(|attachment| attachment.filename == LEDGER_ATTACHMENT_FILENAME)
                .collect();
            if record.attachments.len() != 1 || authoritative.len() != 1 {
                return Err(StoreLoadError::AttachmentCardinality {
                    message_id: record.message_id,
                    total_attachments: record.attachments.len(),
                    authoritative_attachments: authoritative.len(),
                });
            }
            let attachment = authoritative[0];
            let decoded = CanonicalAttachmentCodec::decode(&attachment.bytes, record.message_id)
                .map_err(|error| StoreLoadError::Decode {
                    message_id: record.message_id,
                    error,
                })?;
            probes.push(CanonicalMessageProbe {
                entry_id: decoded.payload.entry.id,
                envelope_bytes: attachment.bytes.clone(),
            });
        }
        Ok(probes)
    }

    pub async fn scan_all_canonical_messages(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
    ) -> Result<Vec<CanonicalMessageProbe>, StoreLoadError> {
        let result = self
            .scan_all_canonical_messages_inner(ctx, canonical_thread_id, ledger_id)
            .await;
        self.observe_load_permission_result(Some(ledger_id), canonical_thread_id, result)
    }

    async fn scan_all_canonical_messages_inner(
        &self,
        ctx: &Context,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
    ) -> Result<Vec<CanonicalMessageProbe>, StoreLoadError> {
        let messages = fetch_stable_channel_messages(ctx, canonical_thread_id).await?;
        let pending_records: Vec<PendingCanonicalMessageRecord> = messages
            .into_iter()
            .map(pending_canonical_message_record)
            .filter(PendingCanonicalMessageRecord::should_validate_writer_lineage)
            .collect();
        self.validate_writer_lineage(Some(ledger_id), &pending_records)?;

        let mut probes = Vec::with_capacity(pending_records.len());
        for pending in pending_records {
            let record = download_canonical_message_record(pending).await?;
            let authoritative: Vec<&CanonicalAttachmentCandidate> = record
                .attachments
                .iter()
                .filter(|attachment| attachment.filename == LEDGER_ATTACHMENT_FILENAME)
                .collect();
            if record.attachments.len() != 1 || authoritative.len() != 1 {
                return Err(StoreLoadError::AttachmentCardinality {
                    message_id: record.message_id,
                    total_attachments: record.attachments.len(),
                    authoritative_attachments: authoritative.len(),
                });
            }
            let attachment = authoritative[0];
            let decoded = CanonicalAttachmentCodec::decode(&attachment.bytes, record.message_id)
                .map_err(|error| StoreLoadError::Decode {
                    message_id: record.message_id,
                    error,
                })?;
            probes.push(CanonicalMessageProbe {
                entry_id: decoded.payload.entry.id,
                envelope_bytes: attachment.bytes.clone(),
            });
        }
        Ok(probes)
    }

    pub async fn load_verified_thread_discovering_id(
        &self,
        http: &Http,
        canonical_thread_id: ChannelId,
    ) -> Result<Option<(LedgerId, VerifiedLedgerThreadLoad)>, StoreLoadError> {
        let result = self
            .load_verified_thread_discovering_id_inner(http, canonical_thread_id)
            .await;
        self.observe_load_permission_result(None, canonical_thread_id, result)
    }

    async fn load_verified_thread_discovering_id_inner(
        &self,
        http: &Http,
        canonical_thread_id: ChannelId,
    ) -> Result<Option<(LedgerId, VerifiedLedgerThreadLoad)>, StoreLoadError> {
        let messages = fetch_stable_channel_messages_with_http(http, canonical_thread_id).await?;
        let pending_records: Vec<PendingCanonicalMessageRecord> = messages
            .into_iter()
            .map(pending_canonical_message_record)
            .filter(PendingCanonicalMessageRecord::should_validate_writer_lineage)
            .collect();
        self.validate_writer_lineage(None, &pending_records)?;

        let mut records = Vec::with_capacity(pending_records.len());
        for record in pending_records {
            records.push(download_canonical_message_record(record).await?);
        }
        let Some(ledger_id) = discover_ledger_id_from_records(&records)? else {
            return Ok(None);
        };
        let load =
            self.load_verified_thread_from_records(canonical_thread_id, ledger_id, records)?;
        Ok(Some((ledger_id, load)))
    }

    fn load_verified_thread_from_records(
        &self,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
        records: Vec<CanonicalMessageRecord>,
    ) -> Result<VerifiedLedgerThreadLoad, StoreLoadError> {
        let display_drift_guard = self.display_drift_guard;
        self.load_verified_thread_from_records_with_guard(
            canonical_thread_id,
            ledger_id,
            records,
            move |envelope, record| {
                let observation = DisplayDriftObservation::from_record(ledger_id, envelope, record);
                display_drift_guard(&observation)
            },
        )
    }

    fn load_verified_thread_from_records_with_guard<F>(
        &self,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
        records: Vec<CanonicalMessageRecord>,
        display_guard: F,
    ) -> Result<VerifiedLedgerThreadLoad, StoreLoadError>
    where
        F: FnMut(
            &VerifiedLedgerStoreEnvelope<MessageId>,
            &CanonicalMessageRecord,
        ) -> Result<(), StoreLoadError>,
    {
        let records: Vec<CanonicalMessageRecord> = records
            .into_iter()
            .filter(CanonicalMessageRecord::should_validate_writer_lineage)
            .collect();
        self.validate_writer_lineage(Some(ledger_id), &records)?;

        self.load_verified_thread_from_candidate_records_with_guard(
            canonical_thread_id,
            ledger_id,
            records,
            display_guard,
        )
    }

    fn load_verified_thread_from_candidate_records_with_guard<F>(
        &self,
        canonical_thread_id: ChannelId,
        ledger_id: LedgerId,
        records: Vec<CanonicalMessageRecord>,
        mut display_guard: F,
    ) -> Result<VerifiedLedgerThreadLoad, StoreLoadError>
    where
        F: FnMut(
            &VerifiedLedgerStoreEnvelope<MessageId>,
            &CanonicalMessageRecord,
        ) -> Result<(), StoreLoadError>,
    {
        let mut records = records;
        let mut decoded = Vec::with_capacity(records.len());
        for record in &mut records {
            let authoritative: Vec<&CanonicalAttachmentCandidate> = record
                .attachments
                .iter()
                .filter(|attachment| attachment.filename == LEDGER_ATTACHMENT_FILENAME)
                .collect();
            if record.attachments.len() != 1 || authoritative.len() != 1 {
                return Err(StoreLoadError::AttachmentCardinality {
                    message_id: record.message_id,
                    total_attachments: record.attachments.len(),
                    authoritative_attachments: authoritative.len(),
                });
            }

            let attachment = authoritative[0];
            if attachment.size_bytes > MAX_AUTHORITATIVE_ATTACHMENT_BYTES {
                return Err(StoreLoadError::OversizeAttachment {
                    message_id: record.message_id,
                    size_bytes: attachment.size_bytes,
                });
            }

            let decoded_attachment = CanonicalAttachmentCodec::decode_with_transport(
                &attachment.bytes,
                record.message_id,
            )
            .map_err(|error| StoreLoadError::Decode {
                message_id: record.message_id,
                error,
            })?;
            record.expected_pre_self_link_content_sha256 = decoded_attachment
                .transport
                .pre_self_link_content_sha256()
                .map(str::to_owned);
            decoded.push(decoded_attachment.envelope);
        }

        let verified = verify_envelopes_in_append_order_sha256_v1(decoded, ledger_id)
            .map_err(|error| StoreLoadError::Chain(LedgerLoadError::ChainVerification(error)))?;
        let snapshot = replay_verified_snapshot(&verified).map_err(StoreLoadError::from_replay)?;
        if snapshot.should_emit_growth_warning() {
            self.observability
                .emit(LedgerObservabilityEvent::GrowthWarning {
                    ledger_id,
                    entry_count: snapshot.canonical_entry_count() as u64,
                });
        }
        let transport_entries = build_transport_entries(canonical_thread_id, &verified, &records)?;
        let records_by_message_id: BTreeMap<MessageId, &CanonicalMessageRecord> = records
            .iter()
            .map(|record| (record.message_id, record))
            .collect();

        for envelope in &verified {
            let message_id = *envelope.external_id();
            let record =
                records_by_message_id
                    .get(&message_id)
                    .ok_or(StoreLoadError::MetadataCoherence(
                        MetadataCoherenceFailure::MissingTransportMetadata {
                            entry_id: envelope.payload().entry.id,
                            message_id,
                        },
                    ))?;
            display_guard(envelope, record)?;
        }

        Ok(
            VerifiedLedgerThreadLoad::new(snapshot, verified, transport_entries).expect(
                "build_transport_entries inserts a transport entry for every verified envelope",
            ),
        )
    }
}

async fn fetch_all_channel_messages_with_http(
    http: &Http,
    channel_id: ChannelId,
) -> serenity::Result<Vec<Message>> {
    let mut all_messages = Vec::new();
    let mut last_message_id = None;
    loop {
        let mut builder = GetMessages::new().limit(100);
        if let Some(before) = last_message_id {
            builder = builder.before(before);
        }
        let messages = channel_id.messages(http, builder).await?;
        if messages.is_empty() {
            break;
        }
        last_message_id = messages.last().map(|message| message.id);
        all_messages.extend(messages);
    }
    all_messages.reverse();
    Ok(all_messages)
}

async fn fetch_stable_channel_messages(
    ctx: &Context,
    channel_id: ChannelId,
) -> Result<Vec<Message>, StoreLoadError> {
    for _ in 0..STABLE_SNAPSHOT_ATTEMPTS {
        let messages = fetch_all_channel_messages(ctx, channel_id)
            .await
            .map_err(classify_thread_fetch_error)?;
        let observed_head = fetch_channel_head(&ctx.http, channel_id).await?;
        if snapshot_head_matches(messages.last().map(|message| message.id), observed_head) {
            return Ok(messages);
        }
    }
    Err(StoreLoadError::Fetch(
        "canonical thread changed while loading stable snapshot".to_owned(),
    ))
}

async fn fetch_stable_channel_messages_with_http(
    http: &Http,
    channel_id: ChannelId,
) -> Result<Vec<Message>, StoreLoadError> {
    for _ in 0..STABLE_SNAPSHOT_ATTEMPTS {
        let messages = fetch_all_channel_messages_with_http(http, channel_id)
            .await
            .map_err(classify_thread_fetch_error)?;
        let observed_head = fetch_channel_head(http, channel_id).await?;
        if snapshot_head_matches(messages.last().map(|message| message.id), observed_head) {
            return Ok(messages);
        }
    }
    Err(StoreLoadError::Fetch(
        "canonical thread changed while loading stable snapshot".to_owned(),
    ))
}

async fn fetch_channel_head(
    http: &Http,
    channel_id: ChannelId,
) -> Result<Option<MessageId>, StoreLoadError> {
    channel_id
        .messages(http, GetMessages::new().limit(1))
        .await
        .map_err(classify_thread_fetch_error)
        .map(|messages| messages.first().map(|message| message.id))
}

fn snapshot_head_matches(loaded_head: Option<MessageId>, observed_head: Option<MessageId>) -> bool {
    loaded_head == observed_head
}

async fn fetch_recent_canonical_pending_records(
    ctx: &Context,
    channel_id: ChannelId,
) -> Result<Vec<PendingCanonicalMessageRecord>, StoreLoadError> {
    let mut pending_records = Vec::new();
    let mut last_message_id = None;
    loop {
        let mut builder = GetMessages::new().limit(100);
        if let Some(before) = last_message_id {
            builder = builder.before(before);
        }
        let messages = channel_id
            .messages(&ctx.http, builder)
            .await
            .map_err(classify_thread_fetch_error)?;
        if messages.is_empty() {
            break;
        }
        last_message_id = messages.last().map(|message| message.id);
        pending_records.extend(
            messages
                .into_iter()
                .map(pending_canonical_message_record)
                .filter(PendingCanonicalMessageRecord::should_validate_writer_lineage),
        );
        if pending_records.len() >= LAZY_RETRY_SCAN_WINDOW {
            break;
        }
    }
    Ok(pending_records)
}

fn discover_ledger_id_from_records(
    records: &[CanonicalMessageRecord],
) -> Result<Option<LedgerId>, StoreLoadError> {
    let Some(record) = records.first() else {
        return Ok(None);
    };
    let authoritative: Vec<&CanonicalAttachmentCandidate> = record
        .attachments
        .iter()
        .filter(|attachment| attachment.filename == LEDGER_ATTACHMENT_FILENAME)
        .collect();
    if record.attachments.len() != 1 || authoritative.len() != 1 {
        return Err(StoreLoadError::AttachmentCardinality {
            message_id: record.message_id,
            total_attachments: record.attachments.len(),
            authoritative_attachments: authoritative.len(),
        });
    }
    let attachment = authoritative[0];
    if attachment.size_bytes > MAX_AUTHORITATIVE_ATTACHMENT_BYTES {
        return Err(StoreLoadError::OversizeAttachment {
            message_id: record.message_id,
            size_bytes: attachment.size_bytes,
        });
    }
    let decoded = CanonicalAttachmentCodec::decode(&attachment.bytes, record.message_id).map_err(
        |error| StoreLoadError::Decode {
            message_id: record.message_id,
            error,
        },
    )?;
    Ok(Some(decoded.payload.ledger_id))
}

#[derive(Debug, Clone)]
struct CanonicalMessageRecord {
    message_id: MessageId,
    author_id: UserId,
    author_is_bot: bool,
    webhook_id: Option<u64>,
    guild_id: Option<GuildId>,
    channel_id: ChannelId,
    recorded_at: SystemTime,
    edited_at: Option<SystemTime>,
    content: String,
    expected_pre_self_link_content_sha256: Option<String>,
    attachments: Vec<CanonicalAttachmentCandidate>,
}

impl LineageRecord for CanonicalMessageRecord {
    fn message_id(&self) -> MessageId {
        self.message_id
    }

    fn author_id(&self) -> UserId {
        self.author_id
    }

    fn webhook_id(&self) -> Option<u64> {
        self.webhook_id
    }
}

#[derive(Debug, Clone)]
struct CanonicalAttachmentCandidate {
    filename: String,
    size_bytes: u32,
    bytes: Vec<u8>,
}

#[derive(Debug, Clone)]
struct PendingCanonicalMessageRecord {
    message_id: MessageId,
    author_id: UserId,
    author_is_bot: bool,
    webhook_id: Option<u64>,
    guild_id: Option<GuildId>,
    channel_id: ChannelId,
    recorded_at: SystemTime,
    edited_at: Option<SystemTime>,
    content: String,
    attachments: Vec<PendingCanonicalAttachmentCandidate>,
}

impl LineageRecord for PendingCanonicalMessageRecord {
    fn message_id(&self) -> MessageId {
        self.message_id
    }

    fn author_id(&self) -> UserId {
        self.author_id
    }

    fn webhook_id(&self) -> Option<u64> {
        self.webhook_id
    }
}

#[derive(Debug, Clone)]
struct PendingCanonicalAttachmentCandidate {
    filename: String,
    size_bytes: u32,
    authoritative_attachment: Option<serenity::all::Attachment>,
}

fn pending_canonical_message_record(message: Message) -> PendingCanonicalMessageRecord {
    let mut attachments = Vec::with_capacity(message.attachments.len());

    for attachment in message.attachments {
        let is_authoritative = attachment.filename == LEDGER_ATTACHMENT_FILENAME;
        attachments.push(PendingCanonicalAttachmentCandidate {
            filename: attachment.filename.clone(),
            size_bytes: attachment.size,
            authoritative_attachment: is_authoritative.then_some(attachment),
        });
    }

    PendingCanonicalMessageRecord {
        message_id: message.id,
        author_id: message.author.id,
        author_is_bot: message.author.bot,
        webhook_id: message.webhook_id.map(|webhook_id| webhook_id.get()),
        guild_id: message.guild_id,
        channel_id: message.channel_id,
        recorded_at: timestamp_to_system_time(message.timestamp.unix_timestamp()),
        edited_at: message
            .edited_timestamp
            .map(|timestamp| timestamp_to_system_time(timestamp.unix_timestamp())),
        content: message.content,
        attachments,
    }
}

fn validate_authoritative_attachment_url(
    message_id: MessageId,
    attachment_url: &str,
) -> Result<(), StoreLoadError> {
    let parsed = url::Url::parse(attachment_url).map_err(|error| StoreLoadError::Decode {
        message_id,
        error: AttachmentCodecError::UnreadableAttachment(error.to_string()),
    })?;
    if matches!(parsed.scheme(), "http" | "https") && parsed.has_host() {
        return Ok(());
    }
    Err(StoreLoadError::Decode {
        message_id,
        error: AttachmentCodecError::UnreadableAttachment(attachment_url.to_owned()),
    })
}

async fn download_canonical_message_record(
    record: PendingCanonicalMessageRecord,
) -> Result<CanonicalMessageRecord, StoreLoadError> {
    let authoritative_attachment_count = record
        .attachments
        .iter()
        .filter(|attachment| attachment.filename == LEDGER_ATTACHMENT_FILENAME)
        .count();
    let should_download_authoritative =
        record.attachments.len() == 1 && authoritative_attachment_count == 1;
    let mut attachments = Vec::with_capacity(record.attachments.len());

    for attachment in record.attachments {
        let should_download = should_download_authoritative
            && attachment.filename == LEDGER_ATTACHMENT_FILENAME
            && attachment.size_bytes <= MAX_AUTHORITATIVE_ATTACHMENT_BYTES;
        let bytes = if should_download {
            let authoritative_attachment = attachment
                .authoritative_attachment
                .expect("authoritative attachment should remain available until download");
            validate_authoritative_attachment_url(
                record.message_id,
                &authoritative_attachment.url,
            )?;
            authoritative_attachment
                .download()
                .await
                .map_err(|error| unreadable_attachment_error(record.message_id, error))?
        } else {
            Vec::new()
        };
        attachments.push(CanonicalAttachmentCandidate {
            filename: attachment.filename,
            size_bytes: attachment.size_bytes,
            bytes,
        });
    }

    Ok(CanonicalMessageRecord {
        message_id: record.message_id,
        author_id: record.author_id,
        author_is_bot: record.author_is_bot,
        webhook_id: record.webhook_id,
        guild_id: record.guild_id,
        channel_id: record.channel_id,
        recorded_at: record.recorded_at,
        edited_at: record.edited_at,
        content: record.content,
        expected_pre_self_link_content_sha256: None,
        attachments,
    })
}

fn canonical_message_link(
    guild_id: GuildId,
    channel_id: ChannelId,
    message_id: MessageId,
) -> String {
    format!(
        "https://discord.com/channels/{}/{}/{}",
        guild_id.get(),
        channel_id.get(),
        message_id.get()
    )
}

fn timestamp_to_system_time(unix_seconds: i64) -> SystemTime {
    UNIX_EPOCH + Duration::from_secs(unix_seconds.max(0) as u64)
}

fn classify_fetch_status(
    status_code: Option<serenity::http::StatusCode>,
    error_text: String,
) -> StoreLoadError {
    match status_code {
        Some(serenity::http::StatusCode::FORBIDDEN | serenity::http::StatusCode::UNAUTHORIZED) => {
            StoreLoadError::Permission(error_text)
        }
        _ => StoreLoadError::Fetch(error_text),
    }
}

fn classify_thread_fetch_status(
    status_code: Option<serenity::http::StatusCode>,
    error_text: String,
) -> StoreLoadError {
    match status_code {
        Some(
            serenity::http::StatusCode::FORBIDDEN
            | serenity::http::StatusCode::UNAUTHORIZED
            | serenity::http::StatusCode::NOT_FOUND,
        ) => StoreLoadError::Permission(error_text),
        _ => StoreLoadError::Fetch(error_text),
    }
}

fn classify_thread_fetch_error(error: serenity::Error) -> StoreLoadError {
    match &error {
        serenity::Error::Http(http_error) => {
            classify_thread_fetch_status(http_error.status_code(), error.to_string())
        }
        _ => StoreLoadError::Fetch(error.to_string()),
    }
}

fn unreadable_attachment_error(message_id: MessageId, error: serenity::Error) -> StoreLoadError {
    match &error {
        serenity::Error::Http(http_error) => {
            classify_fetch_status(http_error.status_code(), error.to_string())
        }
        _ => StoreLoadError::Decode {
            message_id,
            error: AttachmentCodecError::UnreadableAttachment(error.to_string()),
        },
    }
}

#[cfg(test)]
fn reject_edited_messages(observation: &DisplayDriftObservation) -> Result<(), StoreLoadError> {
    if observation.edited_at.is_some() {
        return Err(StoreLoadError::DisplayDrift {
            message_id: observation.message_id,
        });
    }
    Ok(())
}

fn allow_immediate_self_link_completion_edit_only(
    observation: &DisplayDriftObservation,
) -> Result<(), StoreLoadError> {
    let Some(edited_at) = observation.edited_at else {
        return Ok(());
    };
    let Some(guild_id) = observation.guild_id else {
        return Err(StoreLoadError::DisplayDrift {
            message_id: observation.message_id,
        });
    };
    let Ok(edit_age) = edited_at.duration_since(observation.recorded_at) else {
        return Err(StoreLoadError::DisplayDrift {
            message_id: observation.message_id,
        });
    };
    if edit_age > IMMEDIATE_SELF_LINK_EDIT_WINDOW {
        return Err(StoreLoadError::DisplayDrift {
            message_id: observation.message_id,
        });
    }
    let Some(expected_pre_self_link_content_sha256) =
        observation.expected_pre_self_link_content_sha256.as_deref()
    else {
        return Err(StoreLoadError::DisplayDrift {
            message_id: observation.message_id,
        });
    };
    let expected_link =
        canonical_message_link(guild_id, observation.channel_id, observation.message_id);
    let expected_line = format!(
        "{}ledger:{}/entry:{} | <{expected_link}>",
        walicord_i18n::RECOVERY_REFERENCE_PREFIX,
        recovery_reference_ledger_id_short(observation.ledger_id),
        observation.entry_id.0
    );
    let expected_unlinked_line = format!(
        "{}ledger:{}/entry:{}",
        walicord_i18n::RECOVERY_REFERENCE_PREFIX,
        recovery_reference_ledger_id_short(observation.ledger_id),
        observation.entry_id.0
    );
    let lines = observation.content.lines().collect::<Vec<_>>();
    let recovery_reference_lines = lines
        .iter()
        .filter(|line| line.starts_with(walicord_i18n::RECOVERY_REFERENCE_PREFIX))
        .count();
    if lines.len() >= 2
        && observation
            .expected_first_line_prefixes
            .iter()
            .any(|prefix| lines[0].starts_with(prefix))
        && recovery_reference_lines == 1
        && lines.last().copied() == Some(expected_line.as_str())
        && pre_self_link_content_sha256(&replace_final_recovery_reference_line(
            &lines,
            &expected_unlinked_line,
        )) == expected_pre_self_link_content_sha256
    {
        return Ok(());
    }

    Err(StoreLoadError::DisplayDrift {
        message_id: observation.message_id,
    })
}

fn replace_final_recovery_reference_line(lines: &[&str], replacement: &str) -> String {
    let mut normalized = lines
        .iter()
        .map(|line| (*line).to_owned())
        .collect::<Vec<_>>();
    if let Some(last) = normalized.last_mut() {
        *last = replacement.to_owned();
    }
    normalized.join("\n")
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

async fn with_load_timeout<F, T, W, X>(
    timeout: Duration,
    warning_after: Duration,
    on_warning: W,
    on_timeout: X,
    future: F,
) -> Result<T, StoreLoadError>
where
    F: Future<Output = Result<T, StoreLoadError>>,
    W: FnOnce(),
    X: FnOnce(),
{
    let warning_sleep = tokio::time::sleep(warning_after);
    let timeout_sleep = tokio::time::sleep(timeout);
    tokio::pin!(future);
    tokio::pin!(warning_sleep);
    tokio::pin!(timeout_sleep);
    let mut on_warning = Some(on_warning);
    let mut on_timeout = Some(on_timeout);
    loop {
        tokio::select! {
            result = &mut future => return result,
            _ = &mut warning_sleep, if on_warning.is_some() => {
                if let Some(on_warning) = on_warning.take() {
                    on_warning();
                }
            }
            _ = &mut timeout_sleep => {
                if let Some(on_timeout) = on_timeout.take() {
                    on_timeout();
                }
                return Err(StoreLoadError::FetchTimeout { elapsed: timeout });
            }
        }
    }
}

async fn with_write_timeout<F, T>(timeout: Duration, future: F) -> Result<T, StoreWriteError>
where
    F: Future<Output = Result<T, StoreWriteError>>,
{
    tokio::time::timeout(timeout, future)
        .await
        .map_err(|_| StoreWriteError::WriteTimeout { elapsed: timeout })?
}

#[cfg(test)]
use super::observability::CapturingLedgerObservability;

#[cfg(test)]
pub(super) fn verified_thread_load_for_test(
    entries: Vec<walicord_application::ledger::LedgerEntry>,
) -> VerifiedLedgerThreadLoad {
    let ledger_id = walicord_ledger::test_fixtures::ledger_id(77);
    let channel_id = ChannelId::new(77);
    let mut previous_hash = walicord_application::ledger::ledger_chain_genesis_sha256_v1(ledger_id);
    let mut records = Vec::with_capacity(entries.len());
    for (index, entry) in entries.into_iter().enumerate() {
        let message_id = (index + 1) as u64;
        let envelope = walicord_application::ledger::make_unverified_envelope_sha256_v1(
            ledger_id,
            previous_hash,
            (),
            entry,
        )
        .expect("envelope should build");
        previous_hash = envelope.entry_hash;
        let bytes = CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        records.push(CanonicalMessageRecord {
            message_id: MessageId::new(message_id),
            author_id: UserId::new(900),
            author_is_bot: true,
            webhook_id: None,
            guild_id: Some(GuildId::new(500)),
            channel_id,
            recorded_at: UNIX_EPOCH + Duration::from_secs(message_id),
            edited_at: None,
            content: "canonical".to_owned(),
            expected_pre_self_link_content_sha256: None,
            attachments: vec![CanonicalAttachmentCandidate {
                filename: LEDGER_ATTACHMENT_FILENAME.to_owned(),
                size_bytes: u32::try_from(bytes.len()).expect("attachment size should fit"),
                bytes,
            }],
        });
    }

    DiscordCanonicalLedgerStore::new_with_display_drift_guard(
        WriterLineagePolicy::load(Some(UserId::new(900)), Some([UserId::new(900)]))
            .expect("lineage should build"),
        reject_edited_messages,
        Arc::new(CapturingLedgerObservability::new()),
    )
    .load_verified_thread_from_records(channel_id, ledger_id, records)
    .expect("load should succeed")
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use serde_json::Value;
    use tokio::runtime::Builder as RuntimeBuilder;
    use walicord_application::ledger::{
        AdjustmentReason, AllocationSnapshot, BalanceAdjusted, BalanceAdjustment, EntryVoided,
        ExpenseNote, ExpenseRecorded, LedgerEntry, LedgerHistorySealed, MemberAmount,
        NormalizedSettlementPlanRecorded, external_correction_source_for_transport_decode,
        ledger_chain_genesis_sha256_v1, make_unverified_envelope_sha256_v1,
    };
    use walicord_domain::{Money, Transfer, model::MemberId};

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

    fn seal_entry(id: u64, through: u64) -> LedgerEntry {
        LedgerEntry::non_expense(
            LedgerEntryId(id),
            LedgerHistorySealed::new(LedgerEntryId(through)),
        )
    }

    fn adjustment_entry(id: u64) -> LedgerEntry {
        LedgerEntry::non_expense(
            LedgerEntryId(id),
            BalanceAdjusted::new(
                vec![
                    BalanceAdjustment {
                        member_id: MemberId(1),
                        amount: Money::from_i64(-100),
                    },
                    BalanceAdjustment {
                        member_id: MemberId(2),
                        amount: Money::from_i64(100),
                    },
                ],
                AdjustmentReason::new("補正").expect("reason should parse"),
                external_correction_source_for_transport_decode(),
            )
            .expect("adjustment should build"),
        )
    }

    fn canonical_record(
        message_id: u64,
        author_id: u64,
        guild_id: u64,
        channel_id: u64,
        recorded_at: u64,
        envelope_bytes: Vec<u8>,
    ) -> CanonicalMessageRecord {
        CanonicalMessageRecord {
            message_id: MessageId::new(message_id),
            author_id: UserId::new(author_id),
            author_is_bot: true,
            webhook_id: None,
            guild_id: Some(GuildId::new(guild_id)),
            channel_id: ChannelId::new(channel_id),
            recorded_at: UNIX_EPOCH + Duration::from_secs(recorded_at),
            edited_at: None,
            content: "canonical".to_owned(),
            expected_pre_self_link_content_sha256: None,
            attachments: vec![CanonicalAttachmentCandidate {
                filename: LEDGER_ATTACHMENT_FILENAME.to_owned(),
                size_bytes: u32::try_from(envelope_bytes.len())
                    .expect("attachment size should fit"),
                bytes: envelope_bytes,
            }],
        }
    }

    #[test]
    fn ledger_id_discovery_uses_canonical_attachment_instead_of_thread_id() {
        let ledger_id = walicord_ledger::test_fixtures::ledger_id(77);
        let envelope = make_unverified_envelope_sha256_v1(
            ledger_id,
            ledger_chain_genesis_sha256_v1(ledger_id),
            (),
            expense_entry(1, 1, &[(1, 100)]),
        )
        .expect("envelope should build");
        let bytes = CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
        let records = vec![canonical_record(1, 900, 500, 42, 1, bytes)];

        assert!(matches!(
            discover_ledger_id_from_records(&records),
            Ok(Some(actual)) if actual == ledger_id
        ));
    }

    fn unrelated_attachment_record(
        message_id: u64,
        author_id: u64,
        channel_id: u64,
    ) -> CanonicalMessageRecord {
        CanonicalMessageRecord {
            message_id: MessageId::new(message_id),
            author_id: UserId::new(author_id),
            author_is_bot: true,
            webhook_id: None,
            guild_id: Some(GuildId::new(700)),
            channel_id: ChannelId::new(channel_id),
            recorded_at: UNIX_EPOCH,
            edited_at: None,
            content: "canonical".to_owned(),
            expected_pre_self_link_content_sha256: None,
            attachments: vec![CanonicalAttachmentCandidate {
                filename: "not-ledger.json".to_owned(),
                size_bytes: 10,
                bytes: Vec::new(),
            }],
        }
    }

    fn user_chatter_record(message_id: u64, channel_id: u64) -> CanonicalMessageRecord {
        CanonicalMessageRecord {
            message_id: MessageId::new(message_id),
            author_id: UserId::new(42),
            author_is_bot: false,
            webhook_id: None,
            guild_id: Some(GuildId::new(700)),
            channel_id: ChannelId::new(channel_id),
            recorded_at: UNIX_EPOCH,
            edited_at: None,
            content: "hello".to_owned(),
            expected_pre_self_link_content_sha256: None,
            attachments: Vec::new(),
        }
    }

    fn user_authored_authoritative_record(
        message_id: u64,
        channel_id: u64,
        envelope_bytes: Vec<u8>,
    ) -> CanonicalMessageRecord {
        let mut record =
            canonical_record(message_id, 42, 700, channel_id, message_id, envelope_bytes);
        record.author_is_bot = false;
        record
    }

    fn encode_entries_as_records(entries: Vec<LedgerEntry>) -> Vec<CanonicalMessageRecord> {
        let ledger_id = walicord_ledger::test_fixtures::ledger_id(77);
        let channel_id = ChannelId::new(77);
        let mut previous_hash = ledger_chain_genesis_sha256_v1(ledger_id);
        let mut records = Vec::with_capacity(entries.len());
        for (index, entry) in entries.into_iter().enumerate() {
            let message_id = (index + 1) as u64;
            let envelope = make_unverified_envelope_sha256_v1(ledger_id, previous_hash, (), entry)
                .expect("envelope should build");
            previous_hash = envelope.entry_hash;
            let bytes =
                CanonicalAttachmentCodec::encode(&envelope).expect("attachment should encode");
            records.push(canonical_record(
                message_id,
                900,
                500,
                channel_id.get(),
                message_id,
                bytes,
            ));
        }
        records
    }

    fn attach_pre_self_link_fingerprint(record: &mut CanonicalMessageRecord, content: &str) {
        let decoded =
            CanonicalAttachmentCodec::decode(&record.attachments[0].bytes, record.message_id)
                .expect("attachment should decode");
        let envelope = walicord_application::ledger::UnverifiedLedgerStoreEnvelope {
            previous_hash: decoded.previous_hash,
            entry_hash: decoded.entry_hash,
            external_id: (),
            payload: decoded.payload,
        };
        let bytes =
            CanonicalAttachmentCodec::encode_with_pre_self_link_content(&envelope, Some(content))
                .expect("attachment should encode");
        record.attachments[0].size_bytes =
            u32::try_from(bytes.len()).expect("attachment size should fit");
        record.attachments[0].bytes = bytes;
    }

    fn store() -> DiscordCanonicalLedgerStore {
        DiscordCanonicalLedgerStore::new_with_display_drift_guard(
            WriterLineagePolicy::load(
                Some(UserId::new(900)),
                Some([UserId::new(800), UserId::new(900)]),
            )
            .expect("lineage should build"),
            reject_edited_messages,
            Arc::new(CapturingLedgerObservability::new()),
        )
    }

    fn pending_record_with_authoritative_attachment_url(
        message_id: u64,
        channel_id: u64,
        url: &str,
    ) -> PendingCanonicalMessageRecord {
        PendingCanonicalMessageRecord {
            message_id: MessageId::new(message_id),
            author_id: UserId::new(900),
            author_is_bot: true,
            webhook_id: None,
            guild_id: Some(GuildId::new(500)),
            channel_id: ChannelId::new(channel_id),
            recorded_at: UNIX_EPOCH + Duration::from_secs(message_id),
            edited_at: None,
            content: "canonical".to_owned(),
            attachments: vec![PendingCanonicalAttachmentCandidate {
                filename: LEDGER_ATTACHMENT_FILENAME.to_owned(),
                size_bytes: 128,
                authoritative_attachment: Some(
                    serde_json::from_value(serde_json::json!({
                        "id": 1_u64,
                        "filename": LEDGER_ATTACHMENT_FILENAME,
                        "description": null,
                        "height": null,
                        "proxy_url": url,
                        "size": 128_u32,
                        "url": url,
                        "width": null,
                        "content_type": "application/json",
                        "ephemeral": false,
                        "duration_secs": null,
                        "waveform": null
                    }))
                    .expect("attachment should deserialize"),
                ),
            }],
        }
    }

    #[test]
    fn classify_fetch_status_maps_permission_status_codes() {
        let forbidden = classify_fetch_status(
            Some(serenity::http::StatusCode::FORBIDDEN),
            "forbidden".to_owned(),
        );
        let unauthorized = classify_fetch_status(
            Some(serenity::http::StatusCode::UNAUTHORIZED),
            "unauthorized".to_owned(),
        );
        let not_found = classify_fetch_status(
            Some(serenity::http::StatusCode::NOT_FOUND),
            "missing attachment".to_owned(),
        );
        let other = classify_fetch_status(None, "other".to_owned());

        assert!(matches!(forbidden, StoreLoadError::Permission(_)));
        assert!(matches!(unauthorized, StoreLoadError::Permission(_)));
        assert!(matches!(not_found, StoreLoadError::Fetch(_)));
        assert!(matches!(other, StoreLoadError::Fetch(_)));
    }

    #[test]
    fn classify_thread_fetch_status_maps_not_found_to_permission() {
        let not_found = classify_thread_fetch_status(
            Some(serenity::http::StatusCode::NOT_FOUND),
            "unknown channel".to_owned(),
        );

        assert!(matches!(not_found, StoreLoadError::Permission(_)));
    }

    #[rstest]
    #[case::forbidden(Some(serenity::http::StatusCode::FORBIDDEN), true)]
    #[case::unauthorized(Some(serenity::http::StatusCode::UNAUTHORIZED), true)]
    #[case::not_found(Some(serenity::http::StatusCode::NOT_FOUND), true)]
    #[case::other(Some(serenity::http::StatusCode::INTERNAL_SERVER_ERROR), false)]
    #[case::missing(None, false)]
    fn read_denied_status_matches_hidden_thread_responses(
        #[case] status_code: Option<serenity::http::StatusCode>,
        #[case] expected: bool,
    ) {
        assert_eq!(read_denied_status(status_code), expected);
    }

    #[test]
    fn classify_thread_fetch_error_maps_non_http_errors_to_fetch() {
        let actual = classify_thread_fetch_error(serenity::Error::Other("boom"));

        assert!(matches!(actual, StoreLoadError::Fetch(_)));
    }

    #[rstest]
    #[case(None, None, true)]
    #[case(Some(1), Some(1), true)]
    #[case(None, Some(1), false)]
    #[case(Some(1), Some(2), false)]
    fn stable_snapshot_requires_the_same_head_before_verification(
        #[case] loaded_head: Option<u64>,
        #[case] observed_head: Option<u64>,
        #[case] expected: bool,
    ) {
        assert_eq!(
            snapshot_head_matches(
                loaded_head.map(MessageId::new),
                observed_head.map(MessageId::new)
            ),
            expected
        );
    }

    #[test]
    fn writer_lineage_policy_rejects_missing_allowlist() {
        let actual = WriterLineagePolicy::load::<Vec<UserId>>(Some(UserId::new(900)), None);

        assert_eq!(actual, Err(WriterLineagePolicyError::MissingAllowlist));
    }

    #[test]
    fn writer_lineage_policy_rejects_missing_active_writer() {
        let actual = WriterLineagePolicy::load::<Vec<UserId>>(None, Some(vec![UserId::new(900)]));

        assert_eq!(actual, Err(WriterLineagePolicyError::MissingActiveWriter));
    }

    #[test]
    fn writer_lineage_policy_rejects_empty_allowlist() {
        let actual = WriterLineagePolicy::load(Some(UserId::new(900)), Some(Vec::<UserId>::new()));

        assert_eq!(actual, Err(WriterLineagePolicyError::EmptyAllowlist));
    }

    #[test]
    fn writer_lineage_policy_rejects_active_writer_outside_allowlist() {
        let actual = WriterLineagePolicy::load(Some(UserId::new(900)), Some([UserId::new(800)]));

        assert_eq!(
            actual,
            Err(WriterLineagePolicyError::ActiveWriterNotApproved {
                active_writer: UserId::new(900),
            })
        );
    }

    #[test]
    fn writer_lineage_policy_rejects_non_active_writer_after_cutover() {
        let policy = WriterLineagePolicy::load(
            Some(UserId::new(900)),
            Some([UserId::new(800), UserId::new(900)]),
        )
        .expect("lineage should build");
        let records = vec![
            canonical_record(1, 800, 500, 77, 1, Vec::new()),
            canonical_record(2, 900, 500, 77, 2, Vec::new()),
            canonical_record(3, 800, 500, 77, 3, Vec::new()),
        ];

        let actual = policy.validate_records(&records);

        assert_eq!(
            actual,
            Err(WriterLineageFailure::NonActiveWriterAfterCutover {
                message_id: MessageId::new(3),
                author_id: UserId::new(800),
                active_writer: UserId::new(900),
            })
        );
    }

    #[test]
    fn store_observes_non_active_writer_after_cutover() {
        let observability = Arc::new(CapturingLedgerObservability::new());
        let store = DiscordCanonicalLedgerStore::new(
            WriterLineagePolicy::load(
                Some(UserId::new(900)),
                Some([UserId::new(800), UserId::new(900)]),
            )
            .expect("lineage should build"),
            observability.clone(),
        );
        let records = vec![
            canonical_record(1, 900, 500, 77, 1, Vec::new()),
            canonical_record(2, 800, 500, 77, 2, Vec::new()),
        ];

        let _ = store.validate_writer_lineage(
            Some(walicord_ledger::test_fixtures::ledger_id(77)),
            &records,
        );

        assert_eq!(
            observability.snapshot(),
            vec![
                super::super::observability::CapturedLedgerObservabilityEvent::Discord(
                    DiscordLedgerObservabilityEvent::ActiveActiveMisconfiguration {
                        ledger_id: Some(walicord_ledger::test_fixtures::ledger_id(77)),
                        observed_writer: UserId::new(800),
                        expected_writer: UserId::new(900),
                    }
                )
            ]
        );
    }

    #[test]
    fn store_observes_lineage_failure_before_ledger_id_discovery() {
        let observability = Arc::new(CapturingLedgerObservability::new());
        let store = DiscordCanonicalLedgerStore::new(
            WriterLineagePolicy::load(Some(UserId::new(900)), Some([UserId::new(900)]))
                .expect("lineage should build"),
            observability.clone(),
        );
        let records = vec![canonical_record(1, 800, 500, 77, 1, Vec::new())];

        let _ = store.validate_writer_lineage(None, &records);

        assert_eq!(
            observability.snapshot(),
            vec![
                super::super::observability::CapturedLedgerObservabilityEvent::Discord(
                    DiscordLedgerObservabilityEvent::ActiveActiveMisconfiguration {
                        ledger_id: None,
                        observed_writer: UserId::new(800),
                        expected_writer: UserId::new(900),
                    }
                )
            ]
        );
    }

    #[test]
    fn store_observes_webhook_authored_candidate() {
        let observability = Arc::new(CapturingLedgerObservability::new());
        let store = DiscordCanonicalLedgerStore::new(
            WriterLineagePolicy::load(Some(UserId::new(900)), Some([UserId::new(900)]))
                .expect("lineage should build"),
            observability.clone(),
        );
        let mut records = vec![canonical_record(1, 900, 500, 77, 1, Vec::new())];
        records[0].webhook_id = Some(7);

        let _ = store.validate_writer_lineage(
            Some(walicord_ledger::test_fixtures::ledger_id(77)),
            &records,
        );

        assert_eq!(
            observability.snapshot(),
            vec![
                super::super::observability::CapturedLedgerObservabilityEvent::Discord(
                    DiscordLedgerObservabilityEvent::ActiveActiveMisconfiguration {
                        ledger_id: Some(walicord_ledger::test_fixtures::ledger_id(77)),
                        observed_writer: UserId::new(900),
                        expected_writer: UserId::new(900),
                    }
                )
            ]
        );
    }

    #[test]
    fn store_observes_permission_failure_with_transport_context() {
        let observability = Arc::new(CapturingLedgerObservability::new());
        let store = DiscordCanonicalLedgerStore::new(
            WriterLineagePolicy::load(Some(UserId::new(900)), Some([UserId::new(900)]))
                .expect("lineage should build"),
            observability.clone(),
        );

        store.observe_permission_failure(
            Some(walicord_ledger::test_fixtures::ledger_id(77)),
            ChannelId::new(30),
            PermissionAction::ReadMessageHistory,
        );

        assert_eq!(
            observability.snapshot(),
            vec![
                super::super::observability::CapturedLedgerObservabilityEvent::Discord(
                    DiscordLedgerObservabilityEvent::PermissionFailure {
                        ledger_id: Some(walicord_ledger::test_fixtures::ledger_id(77)),
                        guild_id: None,
                        channel_id: ChannelId::new(30),
                        action: PermissionAction::ReadMessageHistory,
                    }
                )
            ]
        );
    }

    #[test]
    fn load_verified_thread_builds_snapshot_transport_index_and_verified_history() {
        let records = encode_entries_as_records(vec![
            expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
            settlement_entry(2, 2, 1, 5_000),
        ]);

        let actual = store()
            .load_verified_thread_from_records(
                ChannelId::new(77),
                walicord_ledger::test_fixtures::ledger_id(77),
                records,
            )
            .expect("load should succeed");

        assert_eq!(actual.snapshot().canonical_entry_count(), 2);
        assert_eq!(actual.verified().len(), 2);
        assert_eq!(
            actual
                .transport_index()
                .get(LedgerEntryId(2))
                .expect("transport should exist")
                .message_link(),
            "https://discord.com/channels/500/77/2"
        );
    }

    #[test]
    fn load_verified_thread_rejects_missing_authoritative_attachment() {
        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            vec![unrelated_attachment_record(1, 900, 77)],
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::AttachmentCardinality {
                message_id,
                total_attachments: 1,
                authoritative_attachments: 0,
            }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_multiple_authoritative_attachments() {
        let mut record = unrelated_attachment_record(1, 900, 77);
        record.attachments = vec![
            CanonicalAttachmentCandidate {
                filename: LEDGER_ATTACHMENT_FILENAME.to_owned(),
                size_bytes: 1,
                bytes: vec![1],
            },
            CanonicalAttachmentCandidate {
                filename: LEDGER_ATTACHMENT_FILENAME.to_owned(),
                size_bytes: 1,
                bytes: vec![2],
            },
        ];

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            vec![record],
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::AttachmentCardinality {
                message_id,
                total_attachments: 2,
                authoritative_attachments: 2,
            }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_mixed_authoritative_and_non_authoritative_attachments() {
        let mut record = unrelated_attachment_record(1, 900, 77);
        record.attachments = vec![
            CanonicalAttachmentCandidate {
                filename: LEDGER_ATTACHMENT_FILENAME.to_owned(),
                size_bytes: 1,
                bytes: vec![1],
            },
            CanonicalAttachmentCandidate {
                filename: "extra.txt".to_owned(),
                size_bytes: 4,
                bytes: b"note".to_vec(),
            },
        ];

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            vec![record],
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::AttachmentCardinality {
                message_id,
                total_attachments: 2,
                authoritative_attachments: 1,
            }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_oversize_authoritative_attachment() {
        let mut record = unrelated_attachment_record(1, 900, 77);
        record.attachments = vec![CanonicalAttachmentCandidate {
            filename: LEDGER_ATTACHMENT_FILENAME.to_owned(),
            size_bytes: MAX_AUTHORITATIVE_ATTACHMENT_BYTES + 1,
            bytes: Vec::new(),
        }];

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            vec![record],
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::OversizeAttachment {
                message_id,
                size_bytes,
            }) if message_id == MessageId::new(1)
                && size_bytes == MAX_AUTHORITATIVE_ATTACHMENT_BYTES + 1
        ));
    }

    #[test]
    fn load_verified_thread_rejects_unapproved_writer() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        records[0].author_id = UserId::new(901);

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::WriterLineage(WriterLineageFailure::UnapprovedAuthor {
                message_id,
                author_id,
            })) if message_id == MessageId::new(1) && author_id == UserId::new(901)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_webhook_authored_candidates() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        records[0].webhook_id = Some(7);

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::WriterLineage(WriterLineageFailure::WebhookAuthor {
                message_id,
                ..
            })) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_accepts_historical_writers_before_active_cutover() {
        let mut records = encode_entries_as_records(vec![
            expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
            expense_entry(2, 1, &[(1, 1_000), (2, 1_000)]),
        ]);
        records[0].author_id = UserId::new(800);

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert_eq!(
            actual
                .expect("load should succeed")
                .snapshot()
                .canonical_entry_count(),
            2
        );
    }

    #[test]
    fn load_verified_thread_rejects_user_authored_authoritative_attachment() {
        let mut records = encode_entries_as_records(vec![
            expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
            expense_entry(2, 1, &[(1, 1_000), (2, 1_000)]),
        ]);
        let user_authored =
            user_authored_authoritative_record(99, 77, records[0].attachments[0].bytes.clone());
        records.insert(1, user_authored);

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::WriterLineage(WriterLineageFailure::UnapprovedAuthor {
                message_id,
                author_id,
            })) if message_id == MessageId::new(99) && author_id == UserId::new(42)
        ));
    }

    #[test]
    fn load_verified_thread_ignores_ordinary_user_chatter() {
        let mut records = encode_entries_as_records(vec![
            expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
            expense_entry(2, 1, &[(1, 1_000), (2, 1_000)]),
        ]);
        records.insert(1, user_chatter_record(99, 77));

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert_eq!(
            actual
                .expect("load should succeed")
                .snapshot()
                .canonical_entry_count(),
            2
        );
    }

    #[test]
    fn load_verified_thread_rejects_chain_failures() {
        let mut records = encode_entries_as_records(vec![
            expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
            expense_entry(2, 1, &[(1, 1_000), (2, 1_000)]),
        ]);
        let mut value: Value = serde_json::from_slice(&records[1].attachments[0].bytes)
            .expect("attachment should parse as json");
        value["envelope"]["payload"]["ledger_id"] = Value::from(78_u64);
        records[1].attachments[0].bytes =
            serde_json::to_vec(&value).expect("tampered attachment should serialize");

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(actual, Err(StoreLoadError::Chain(_))));
    }

    #[test]
    fn load_verified_thread_rejects_decode_failures() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        records[0].attachments[0].bytes = b"{".to_vec();

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::Decode { message_id, .. }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_unknown_transport_version_attachments() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        let mut value: Value = serde_json::from_slice(&records[0].attachments[0].bytes)
            .expect("attachment should parse as json");
        value["version"] = Value::from(2_u64);
        records[0].attachments[0].bytes =
            serde_json::to_vec(&value).expect("tampered attachment should serialize");

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::Decode {
                message_id,
                error: AttachmentCodecError::UnsupportedTransportVersion(2),
            }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_unknown_schema_version_attachments() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        let mut value: Value = serde_json::from_slice(&records[0].attachments[0].bytes)
            .expect("attachment should parse as json");
        value["envelope"]["payload"]["schema_version"] = Value::from(2_u64);
        records[0].attachments[0].bytes =
            serde_json::to_vec(&value).expect("tampered attachment should serialize");

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::Decode {
                message_id,
                error: AttachmentCodecError::UnknownSchemaVersion {
                    version: 2,
                    entry_id: Some(LedgerEntryId(1)),
                },
            }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_unknown_event_variant_attachments() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        let mut value: Value = serde_json::from_slice(&records[0].attachments[0].bytes)
            .expect("attachment should parse as json");
        value["envelope"]["payload"]["entry"]["event"]["kind"] = Value::from("future_event");
        records[0].attachments[0].bytes =
            serde_json::to_vec(&value).expect("tampered attachment should serialize");

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::Decode {
                message_id,
                error: AttachmentCodecError::UnknownEventVariant { kind, entry_id },
            }) if message_id == MessageId::new(1)
                && kind == "future_event"
                && entry_id == LedgerEntryId(1)
        ));
    }

    #[test]
    fn download_canonical_message_record_classifies_unreadable_authoritative_attachment_as_decode()
    {
        let record = pending_record_with_authoritative_attachment_url(1, 77, "not a url");
        let runtime = RuntimeBuilder::new_current_thread()
            .enable_all()
            .build()
            .expect("runtime should build");

        let actual = runtime.block_on(download_canonical_message_record(record));

        assert!(matches!(
            actual,
            Err(StoreLoadError::Decode {
                message_id,
                error: AttachmentCodecError::UnreadableAttachment(_),
            }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_structure_failures() {
        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            encode_entries_as_records(vec![
                expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
                void_entry(2, 2),
            ]),
        );

        assert!(matches!(actual, Err(StoreLoadError::Structure(_))));
    }

    #[test]
    fn load_verified_thread_rejects_projection_failures() {
        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            encode_entries_as_records(vec![settlement_entry(1, 1, 2, 100)]),
        );

        assert!(matches!(actual, Err(StoreLoadError::Projection(_))));
    }

    #[test]
    fn load_verified_thread_rejects_missing_guild_transport_metadata() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        records[0].guild_id = None;

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::MetadataCoherence(
                MetadataCoherenceFailure::MissingGuildContext { message_id }
            )) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_display_drift() {
        let records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);

        let actual = store().load_verified_thread_from_records_with_guard(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
            |envelope, record| {
                if envelope.payload().entry.id == LedgerEntryId(1) && record.content == "canonical"
                {
                    return Err(StoreLoadError::DisplayDrift {
                        message_id: record.message_id,
                    });
                }
                Ok(())
            },
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::DisplayDrift { message_id }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_allows_immediate_self_link_completion_edit() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        let ledger_id_short =
            recovery_reference_ledger_id_short(walicord_ledger::test_fixtures::ledger_id(77));
        let message_link =
            canonical_message_link(GuildId::new(500), ChannelId::new(77), MessageId::new(1));
        attach_pre_self_link_fingerprint(
            &mut records[0],
            &format!("支出 [#1]\n復旧用の参照: ledger:{ledger_id_short}/entry:1"),
        );
        records[0].edited_at = Some(UNIX_EPOCH + Duration::from_secs(90));
        records[0].content =
            format!("支出 [#1]\n復旧用の参照: ledger:{ledger_id_short}/entry:1 | <{message_link}>");

        let actual = store().load_verified_thread_from_records_with_guard(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
            |_, record| {
                let expected_link = canonical_message_link(
                    GuildId::new(500),
                    ChannelId::new(77),
                    record.message_id,
                );
                if record.edited_at.is_some()
                    && record.content
                        == format!(
                            "支出 [#1]\n復旧用の参照: ledger:{ledger_id_short}/entry:1 | <{expected_link}>"
                        )
                {
                    return Ok(());
                }
                if record.edited_at.is_some() {
                    return Err(StoreLoadError::DisplayDrift {
                        message_id: record.message_id,
                    });
                }
                Ok(())
            },
        );

        assert!(actual.is_ok());
    }

    #[test]
    fn load_verified_thread_allows_immediate_self_link_completion_edit_for_compact_seal() {
        let mut records = encode_entries_as_records(vec![
            expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
            seal_entry(2, 1),
        ]);
        let ledger_id_short =
            recovery_reference_ledger_id_short(walicord_ledger::test_fixtures::ledger_id(77));
        let message_link =
            canonical_message_link(GuildId::new(500), ChannelId::new(77), MessageId::new(2));
        attach_pre_self_link_fingerprint(
            &mut records[1],
            &format!(
                "確認 花子 が 2026-05-25 18:55 まで確認\n復旧用の参照: ledger:{ledger_id_short}/entry:2"
            ),
        );
        records[1].edited_at = Some(UNIX_EPOCH + Duration::from_secs(90));
        records[1].content = format!(
            "確認 花子 が 2026-05-25 18:55 まで確認\n復旧用の参照: ledger:{ledger_id_short}/entry:2 | <{message_link}>"
        );

        let actual = store().load_verified_thread_from_records_with_guard(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
            |_, record| {
                let expected_link = canonical_message_link(
                    GuildId::new(500),
                    ChannelId::new(77),
                    record.message_id,
                );
                if record.edited_at.is_some()
                    && record.content
                        == format!(
                            "確認 花子 が 2026-05-25 18:55 まで確認\n復旧用の参照: ledger:{ledger_id_short}/entry:2 | <{expected_link}>"
                        )
                {
                    return Ok(());
                }
                if record.edited_at.is_some() {
                    return Err(StoreLoadError::DisplayDrift {
                        message_id: record.message_id,
                    });
                }
                Ok(())
            },
        );

        assert!(actual.is_ok());
    }

    #[test]
    fn load_verified_thread_allows_immediate_self_link_completion_edit_for_compact_adjustment() {
        let mut records = encode_entries_as_records(vec![
            expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
            seal_entry(2, 1),
            adjustment_entry(3),
        ]);
        let ledger_id_short =
            recovery_reference_ledger_id_short(walicord_ledger::test_fixtures::ledger_id(77));
        let message_link =
            canonical_message_link(GuildId::new(500), ChannelId::new(77), MessageId::new(3));
        attach_pre_self_link_fingerprint(
            &mut records[2],
            &format!(
                "残高補正 記録者 理由: 補正 / 太郎 支払い 100円, 花子 受け取り 100円\n復旧用の参照: ledger:{ledger_id_short}/entry:3"
            ),
        );
        records[2].edited_at = Some(UNIX_EPOCH + Duration::from_secs(90));
        records[2].content = format!(
            "残高補正 記録者 理由: 補正 / 太郎 支払い 100円, 花子 受け取り 100円\n復旧用の参照: ledger:{ledger_id_short}/entry:3 | <{message_link}>"
        );

        let actual = store().load_verified_thread_from_records_with_guard(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
            |_, record| {
                let expected_link = canonical_message_link(
                    GuildId::new(500),
                    ChannelId::new(77),
                    record.message_id,
                );
                if record.edited_at.is_some()
                    && record.content
                        == format!(
                            "残高補正 記録者 理由: 補正 / 太郎 支払い 100円, 花子 受け取り 100円\n復旧用の参照: ledger:{ledger_id_short}/entry:3 | <{expected_link}>"
                        )
                {
                    return Ok(());
                }
                if record.edited_at.is_some() {
                    return Err(StoreLoadError::DisplayDrift {
                        message_id: record.message_id,
                    });
                }
                Ok(())
            },
        );

        assert!(actual.is_ok());
    }

    #[test]
    fn load_verified_thread_rejects_single_line_self_link_wipes() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        let ledger_id_short =
            recovery_reference_ledger_id_short(walicord_ledger::test_fixtures::ledger_id(77));
        let message_link =
            canonical_message_link(GuildId::new(500), ChannelId::new(77), MessageId::new(1));
        attach_pre_self_link_fingerprint(
            &mut records[0],
            &format!("支出 [#1]\n復旧用の参照: ledger:{ledger_id_short}/entry:1"),
        );
        records[0].edited_at = Some(UNIX_EPOCH + Duration::from_secs(90));
        records[0].content =
            format!("復旧用の参照: ledger:{ledger_id_short}/entry:1 | <{message_link}>");

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::DisplayDrift { message_id }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_edited_messages_with_extra_content_around_self_link() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        let ledger_id_short =
            recovery_reference_ledger_id_short(walicord_ledger::test_fixtures::ledger_id(77));
        let message_link =
            canonical_message_link(GuildId::new(500), ChannelId::new(77), MessageId::new(1));
        attach_pre_self_link_fingerprint(
            &mut records[0],
            &format!("支出 [#1]\n復旧用の参照: ledger:{ledger_id_short}/entry:1"),
        );
        records[0].edited_at = Some(UNIX_EPOCH + Duration::from_secs(90));
        records[0].content =
            format!("tampered\n復旧用の参照: ledger:{ledger_id_short}/entry:1 | <{message_link}>");

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::DisplayDrift { message_id }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_unrecognized_edited_message() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        let ledger_id_short =
            recovery_reference_ledger_id_short(walicord_ledger::test_fixtures::ledger_id(77));
        attach_pre_self_link_fingerprint(
            &mut records[0],
            &format!("支出 [#1]\n復旧用の参照: ledger:{ledger_id_short}/entry:1"),
        );
        records[0].edited_at = Some(UNIX_EPOCH + Duration::from_secs(90));

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::DisplayDrift { message_id }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_rejects_self_link_edits_without_pre_edit_fingerprint() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        let ledger_id_short =
            recovery_reference_ledger_id_short(walicord_ledger::test_fixtures::ledger_id(77));
        let message_link =
            canonical_message_link(GuildId::new(500), ChannelId::new(77), MessageId::new(1));
        records[0].edited_at = Some(UNIX_EPOCH + Duration::from_secs(90));
        records[0].content =
            format!("支出 [#1]\n復旧用の参照: ledger:{ledger_id_short}/entry:1 | <{message_link}>");

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::DisplayDrift { message_id }) if message_id == MessageId::new(1)
        ));
    }

    #[test]
    fn load_verified_thread_keeps_external_message_ids_outside_ledger_entry_ids() {
        let actual = store()
            .load_verified_thread_from_records(
                ChannelId::new(77),
                walicord_ledger::test_fixtures::ledger_id(77),
                encode_entries_as_records(vec![expense_entry(10, 1, &[(1, 5_000), (2, 5_000)])]),
            )
            .expect("load should succeed");

        assert_eq!(actual.verified()[0].payload().entry.id, LedgerEntryId(10));
        assert_eq!(*actual.verified()[0].external_id(), MessageId::new(1));
    }

    #[test]
    fn verified_entry_transport_index_rejects_channel_mismatch() {
        let mut records =
            encode_entries_as_records(vec![expense_entry(1, 1, &[(1, 5_000), (2, 5_000)])]);
        records[0].channel_id = ChannelId::new(78);

        let actual = store().load_verified_thread_from_records(
            ChannelId::new(77),
            walicord_ledger::test_fixtures::ledger_id(77),
            records,
        );

        assert!(matches!(
            actual,
            Err(StoreLoadError::MetadataCoherence(
                MetadataCoherenceFailure::MismatchedChannel {
                    message_id,
                    expected,
                    actual,
                }
            )) if message_id == MessageId::new(1)
                && expected == ChannelId::new(77)
                && actual == ChannelId::new(78)
        ));
    }

    #[test]
    fn replayed_void_window_fixture_keeps_recent_business_entries_addressable() {
        let actual = store()
            .load_verified_thread_from_records(
                ChannelId::new(77),
                walicord_ledger::test_fixtures::ledger_id(77),
                encode_entries_as_records(vec![
                    expense_entry(1, 1, &[(1, 5_000), (2, 5_000)]),
                    expense_entry(2, 2, &[(1, 1_000), (2, 1_000)]),
                    void_entry(3, 2),
                    seal_entry(4, 1),
                ]),
            )
            .expect("load should succeed");

        assert!(actual.transport_index().get(LedgerEntryId(1)).is_some());
        assert!(actual.transport_index().get(LedgerEntryId(2)).is_some());
        assert!(actual.transport_index().get(LedgerEntryId(3)).is_some());
        assert!(actual.transport_index().get(LedgerEntryId(4)).is_some());
    }

    #[tokio::test]
    async fn load_timeout_helper_returns_fetch_timeout() {
        let actual = with_load_timeout(
            Duration::from_millis(2),
            Duration::from_millis(1),
            || {},
            || {},
            async {
                tokio::time::sleep(Duration::from_millis(5)).await;
                Ok::<(), StoreLoadError>(())
            },
        )
        .await;

        assert!(matches!(
            actual,
            Err(StoreLoadError::FetchTimeout { elapsed }) if elapsed == Duration::from_millis(2)
        ));
    }

    #[rstest]
    #[case::active_thread(Some((false, false)), Ok(AppendThreadPreparation::Ready))]
    #[case::archived_thread(Some((true, false)), Ok(AppendThreadPreparation::Unarchive))]
    #[case::non_thread(None, Err(AppendThreadPreparationError::NotThreadOrLocked))]
    #[case::locked_thread(Some((false, true)), Err(AppendThreadPreparationError::NotThreadOrLocked))]
    #[case::locked_archived_thread(
        Some((true, true)),
        Err(AppendThreadPreparationError::NotThreadOrLocked)
    )]
    fn append_thread_preparation_rejects_locked_and_unarchives_archived_threads(
        #[case] metadata: Option<(bool, bool)>,
        #[case] expected: Result<AppendThreadPreparation, AppendThreadPreparationError>,
    ) {
        assert_eq!(append_thread_preparation(metadata), expected);
    }

    #[rstest]
    #[case::all_required(
        Permissions::VIEW_CHANNEL
            | Permissions::READ_MESSAGE_HISTORY
            | Permissions::SEND_MESSAGES_IN_THREADS
            | Permissions::ATTACH_FILES,
        None
    )]
    #[case::missing_view_channel(
        Permissions::READ_MESSAGE_HISTORY
            | Permissions::SEND_MESSAGES_IN_THREADS
            | Permissions::ATTACH_FILES,
        Some(PermissionAction::ViewChannel)
    )]
    #[case::missing_history(
        Permissions::VIEW_CHANNEL
            | Permissions::SEND_MESSAGES_IN_THREADS
            | Permissions::ATTACH_FILES,
        Some(PermissionAction::ReadMessageHistory)
    )]
    #[case::missing_thread_send(
        Permissions::VIEW_CHANNEL
            | Permissions::READ_MESSAGE_HISTORY
            | Permissions::ATTACH_FILES,
        Some(PermissionAction::SendMessageInChannel)
    )]
    #[case::missing_attach_files(
        Permissions::VIEW_CHANNEL
            | Permissions::READ_MESSAGE_HISTORY
            | Permissions::SEND_MESSAGES_IN_THREADS,
        Some(PermissionAction::AttachFiles)
    )]
    fn canonical_append_preflight_reports_first_missing_permission(
        #[case] permissions: Permissions,
        #[case] expected: Option<PermissionAction>,
    ) {
        assert_eq!(missing_append_permission(permissions), expected);
    }

    #[rstest]
    #[case::exact_bytes(b"canonical", b"canonical", Ok(()))]
    #[case::drifted_bytes(b"canonical", b"tampered", Err(ReadBackAttachmentBytesError::Drift))]
    fn canonical_append_read_back_requires_exact_authoritative_attachment_bytes(
        #[case] expected_bytes: &[u8],
        #[case] actual_bytes: &[u8],
        #[case] expected: Result<(), ReadBackAttachmentBytesError>,
    ) {
        assert_eq!(
            verify_read_back_attachment_bytes(expected_bytes, actual_bytes),
            expected
        );
    }

    #[tokio::test]
    async fn write_timeout_helper_returns_ambiguous_write_timeout() {
        let actual = with_write_timeout(Duration::from_millis(2), async {
            tokio::time::sleep(Duration::from_millis(5)).await;
            Ok::<(), StoreWriteError>(())
        })
        .await;

        assert!(matches!(
            actual,
            Err(StoreWriteError::WriteTimeout { elapsed })
                if elapsed == Duration::from_millis(2)
        ));
    }
}
