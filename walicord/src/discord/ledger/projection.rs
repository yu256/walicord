use super::store::StoreLoadError;
use walicord_application::ledger::{LedgerEntryId, canonical_attachment::AttachmentCodecError};
use walicord_i18n as i18n;

pub use walicord_application::ledger::observability::CanonicalLoadRoute;

pub const UNKNOWN_LEDGER_FORMAT_EVENT: &str = "ledger_unknown_variant";

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
            Self::VersionMismatch { .. } => Some(i18n::UNKNOWN_LEDGER_FORMAT_MESSAGE),
            Self::Fetch { route } => Some(match route {
                CanonicalLoadRoute::WritePrelude => i18n::LEDGER_THREAD_PREPARE_FAILED_MESSAGE,
                CanonicalLoadRoute::Read
                | CanonicalLoadRoute::Preview
                | CanonicalLoadRoute::Refresh => i18n::LEDGER_RETRYABLE_LOAD_MESSAGE,
            }),
            Self::FetchTimeout { route } => Some(match route {
                CanonicalLoadRoute::WritePrelude => i18n::LEDGER_THREAD_PREPARE_FAILED_MESSAGE,
                CanonicalLoadRoute::Read
                | CanonicalLoadRoute::Preview
                | CanonicalLoadRoute::Refresh => i18n::LEDGER_LOAD_TIMEOUT_MESSAGE,
            }),
            Self::Permission { route } => Some(match route {
                CanonicalLoadRoute::WritePrelude => i18n::LEDGER_THREAD_PREPARE_FAILED_MESSAGE,
                CanonicalLoadRoute::Read
                | CanonicalLoadRoute::Preview
                | CanonicalLoadRoute::Refresh => i18n::LEDGER_PERMISSION_FAILED_MESSAGE,
            }),
            Self::AttachmentCardinality { .. }
            | Self::OversizeAttachment { .. }
            | Self::Decode { .. } => Some(i18n::LEDGER_ATTACHMENT_DECODE_FAILED_MESSAGE),
            Self::WriterLineage { .. }
            | Self::Chain { .. }
            | Self::Structure { .. }
            | Self::Projection { .. }
            | Self::MetadataCoherence { .. }
            | Self::DisplayDrift { .. } => Some(i18n::LEDGER_INTEGRITY_FAILED_MESSAGE),
        }
    }

    pub fn observability_event(self) -> Option<&'static str> {
        match self {
            Self::VersionMismatch { .. } => Some(UNKNOWN_LEDGER_FORMAT_EVENT),
            _ => None,
        }
    }
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
    use crate::discord::ledger::store::{VerifiedLedgerThreadLoad, verified_thread_load_for_test};
    use serenity::all::MessageId;
    use walicord_application::ledger::{
        AllocationSnapshot, EntryVoided, ExpenseNote, ExpenseRecorded, LedgerEntry, LedgerEntryId,
        LedgerHistorySealed, MemberAmount, NormalizedSettlementPlanRecorded,
        projection::{project_recent_voidable_entries, project_verified_entries},
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

    fn load(entries: Vec<LedgerEntry>) -> VerifiedLedgerThreadLoad {
        verified_thread_load_for_test(entries)
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
            Some(i18n::UNKNOWN_LEDGER_FORMAT_MESSAGE)
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
            Some(i18n::UNKNOWN_LEDGER_FORMAT_MESSAGE)
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
            Some(i18n::LEDGER_RETRYABLE_LOAD_MESSAGE)
        );
        assert_eq!(
            timeout.user_message(),
            Some(i18n::LEDGER_LOAD_TIMEOUT_MESSAGE)
        );
        assert_eq!(
            permission.user_message(),
            Some(i18n::LEDGER_PERMISSION_FAILED_MESSAGE)
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
            Some(i18n::LEDGER_ATTACHMENT_DECODE_FAILED_MESSAGE)
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
                        author_id: serenity::all::UserId::new(9),
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
                .map(|entry| entry.entry_id())
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
                .map(|entry| entry.entry_id())
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
                .map(|entry| entry.entry_id())
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
                .map(|entry| entry.entry_id())
                .collect::<Vec<_>>(),
            vec![LedgerEntryId(3), LedgerEntryId(2)]
        );
    }
}
