use super::locator::LocatorRecoveryReference;
use serenity::{
    all::{GatewayIntents, Permissions},
    builder::{CreateActionRow, CreateButton, CreateCommand},
};
use std::collections::HashSet;
use walicord_i18n as i18n;

pub(crate) const REQUIRED_OAUTH_SCOPES: &[&str] = &["bot", "applications.commands"];

const REQUIRED_GATEWAY_INTENTS: &[(GatewayIntents, &str)] = &[
    (GatewayIntents::GUILDS, "GUILDS"),
    (GatewayIntents::GUILD_MESSAGES, "GUILD_MESSAGES"),
    (GatewayIntents::MESSAGE_CONTENT, "MESSAGE_CONTENT"),
    (GatewayIntents::GUILD_MEMBERS, "GUILD_MEMBERS"),
    (
        GatewayIntents::GUILD_MESSAGE_REACTIONS,
        "GUILD_MESSAGE_REACTIONS",
    ),
];

const CANONICAL_SURFACE_PERMISSIONS: &[(Permissions, &str)] = &[
    (Permissions::VIEW_CHANNEL, "View Channel"),
    (Permissions::READ_MESSAGE_HISTORY, "Read Message History"),
    (Permissions::SEND_MESSAGES, "Send Messages"),
    (
        Permissions::SEND_MESSAGES_IN_THREADS,
        "Send Messages in Threads",
    ),
    (Permissions::ATTACH_FILES, "Attach Files"),
    (Permissions::CREATE_PUBLIC_THREADS, "Create Public Threads"),
    (Permissions::MANAGE_THREADS, "Manage Threads"),
];

const LEGACY_REACTION_PERMISSIONS: &[(Permissions, &str)] = &[
    (Permissions::VIEW_CHANNEL, "View Channel"),
    (Permissions::READ_MESSAGE_HISTORY, "Read Message History"),
    (Permissions::ADD_REACTIONS, "Add Reactions"),
];

const TOPIC_CHANGE_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::ManageChannels,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

const CHANNEL_OVERRIDE_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::ManageChannels,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

const ROLE_BASED_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::ManageRoles,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

const THREAD_COMMAND_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::UseApplicationCommandsOrEquivalent,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

const THREAD_MAINTENANCE_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::ManageThreads,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AdminTermMeaning {
    DiscordNativePermissionHolder,
}

pub(crate) fn admin_term_meaning() -> AdminTermMeaning {
    AdminTermMeaning::DiscordNativePermissionHolder
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AccessControlMode {
    ChannelOverride,
    RoleBased,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NativeAdminOperation {
    TrackedStateTopicChange,
    BotAccessRepair(AccessControlMode),
    UserAccessRepair(AccessControlMode),
    ThreadCommandExecution,
    CanonicalThreadUnarchive,
    DuplicateThreadDeletion,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NativeAdminCapability {
    ManageChannels,
    ManageRoles,
    ManageThreads,
    UseApplicationCommandsOrEquivalent,
    Administrator,
    ServerOwner,
}

pub(crate) fn native_admin_capabilities(
    operation: NativeAdminOperation,
) -> &'static [NativeAdminCapability] {
    match operation {
        NativeAdminOperation::TrackedStateTopicChange => TOPIC_CHANGE_CAPABILITIES,
        NativeAdminOperation::BotAccessRepair(AccessControlMode::ChannelOverride)
        | NativeAdminOperation::UserAccessRepair(AccessControlMode::ChannelOverride) => {
            CHANNEL_OVERRIDE_CAPABILITIES
        }
        NativeAdminOperation::BotAccessRepair(AccessControlMode::RoleBased)
        | NativeAdminOperation::UserAccessRepair(AccessControlMode::RoleBased) => {
            ROLE_BASED_CAPABILITIES
        }
        NativeAdminOperation::ThreadCommandExecution => THREAD_COMMAND_CAPABILITIES,
        NativeAdminOperation::CanonicalThreadUnarchive
        | NativeAdminOperation::DuplicateThreadDeletion => THREAD_MAINTENANCE_CAPABILITIES,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RuntimePermissionScope {
    CanonicalSurface,
    LegacyReactionPath,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ParentAdminSurfaceAuthorization {
    Allowed,
    MissingNativeAdminPermission,
}

pub(crate) fn authorize_parent_admin_surface(
    has_required_native_permission: bool,
) -> ParentAdminSurfaceAuthorization {
    if has_required_native_permission {
        ParentAdminSurfaceAuthorization::Allowed
    } else {
        ParentAdminSurfaceAuthorization::MissingNativeAdminPermission
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ThreadCommandReadiness {
    Ready,
    MissingThreadAccess,
    MissingThreadCommandPermission,
}

pub(crate) fn thread_command_readiness(
    can_open_thread: bool,
    can_use_thread_commands: bool,
) -> ThreadCommandReadiness {
    if !can_open_thread {
        ThreadCommandReadiness::MissingThreadAccess
    } else if !can_use_thread_commands {
        ThreadCommandReadiness::MissingThreadCommandPermission
    } else {
        ThreadCommandReadiness::Ready
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LedgerRefreshAcknowledgement {
    Unauthorized,
    ReadyNoThread,
    Ready,
}

#[derive(Debug, Clone)]
pub(crate) struct RecoveryOutcomeMessage {
    body: String,
    components: Vec<CreateActionRow>,
}

impl RecoveryOutcomeMessage {
    pub(crate) fn from_body(body: impl Into<String>) -> Self {
        Self {
            body: body.into(),
            components: Vec::new(),
        }
    }

    pub(crate) fn new(body: impl Into<String>, components: Vec<CreateActionRow>) -> Self {
        Self {
            body: body.into(),
            components,
        }
    }

    pub(crate) fn body(&self) -> &str {
        &self.body
    }

    pub(crate) fn components(&self) -> &[CreateActionRow] {
        &self.components
    }

    pub(crate) fn into_parts(self) -> (String, Vec<CreateActionRow>) {
        (self.body, self.components)
    }
}

pub(crate) fn recovery_reference_components(
    recovery_reference: &LocatorRecoveryReference,
) -> Vec<CreateActionRow> {
    match recovery_reference {
        LocatorRecoveryReference::Ledger {
            thread_link: Some(thread_link),
            ..
        } => vec![CreateActionRow::Buttons(vec![
            CreateButton::new_link(thread_link.clone()).label(i18n::open_ledger_thread_label()),
        ])],
        LocatorRecoveryReference::Channel {
            parent_channel_link: Some(parent_channel_link),
            ..
        } => vec![CreateActionRow::Buttons(vec![
            CreateButton::new_link(parent_channel_link.clone())
                .label(i18n::open_parent_channel_label()),
        ])],
        LocatorRecoveryReference::Ledger {
            thread_link: None, ..
        }
        | LocatorRecoveryReference::Channel {
            parent_channel_link: None,
            ..
        } => Vec::new(),
    }
}

pub(crate) fn render_parent_channel_access_failure_message(
    recovery_reference: &LocatorRecoveryReference,
) -> RecoveryOutcomeMessage {
    RecoveryOutcomeMessage::new(
        [
            i18n::bot_cannot_operate_parent_channel_message().to_owned(),
            recovery_reference.render_line(),
        ]
        .join("\n"),
        recovery_reference_components(recovery_reference),
    )
}

pub(crate) fn render_archived_thread_recovery_message(
    recovery_reference: &LocatorRecoveryReference,
) -> RecoveryOutcomeMessage {
    RecoveryOutcomeMessage::new(
        [
            i18n::archived_thread_recovery_message().to_owned(),
            recovery_reference.render_line(),
        ]
        .join("\n"),
        recovery_reference_components(recovery_reference),
    )
}

pub(crate) fn render_ledger_refresh_acknowledgement(
    acknowledgement: LedgerRefreshAcknowledgement,
) -> String {
    match acknowledgement {
        LedgerRefreshAcknowledgement::Unauthorized => {
            i18n::ledger_refresh_admin_only_message().to_owned()
        }
        LedgerRefreshAcknowledgement::ReadyNoThread => {
            i18n::ledger_refresh_no_thread_message().to_owned()
        }
        LedgerRefreshAcknowledgement::Ready => i18n::ledger_refresh_ready_message().to_owned(),
    }
}

pub(crate) fn render_ledger_refresh_uncertain_write_message(
    recovery_reference: &LocatorRecoveryReference,
) -> RecoveryOutcomeMessage {
    RecoveryOutcomeMessage::new(
        [
            i18n::ledger_refresh_uncertain_write_message().to_owned(),
            recovery_reference.render_line(),
        ]
        .join("\n"),
        recovery_reference_components(recovery_reference),
    )
}

#[cfg_attr(not(test), allow(dead_code))]
pub(crate) fn ledger_refresh_command() -> CreateCommand {
    CreateCommand::new("ledger-refresh").description(i18n::slash_ledger_refresh_description())
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error(
    "startup readiness failed: missing oauth_scopes={missing_oauth_scopes:?}, missing gateway_intents={missing_gateway_intents:?}"
)]
pub(crate) struct StartupReadinessFailure {
    pub missing_oauth_scopes: Vec<&'static str>,
    pub missing_gateway_intents: Vec<&'static str>,
}

pub(crate) fn required_gateway_intents() -> GatewayIntents {
    REQUIRED_GATEWAY_INTENTS
        .iter()
        .fold(GatewayIntents::empty(), |acc, (intent, _)| acc | *intent)
}

pub(crate) fn missing_required_oauth_scopes<I, S>(oauth_scopes: I) -> Vec<&'static str>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let present = oauth_scopes
        .into_iter()
        .map(|scope| scope.as_ref().trim().to_ascii_lowercase())
        .collect::<HashSet<_>>();

    REQUIRED_OAUTH_SCOPES
        .iter()
        .copied()
        .filter(|scope| !present.contains(*scope))
        .collect()
}

pub(crate) fn missing_required_gateway_intents(intents: GatewayIntents) -> Vec<&'static str> {
    REQUIRED_GATEWAY_INTENTS
        .iter()
        .filter_map(|(required, name)| (!intents.contains(*required)).then_some(*name))
        .collect()
}

pub(crate) fn validate_startup_readiness<I, S>(
    oauth_scopes: I,
    intents: GatewayIntents,
) -> Result<(), StartupReadinessFailure>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let failure = StartupReadinessFailure {
        missing_oauth_scopes: missing_required_oauth_scopes(oauth_scopes),
        missing_gateway_intents: missing_required_gateway_intents(intents),
    };

    if failure.missing_oauth_scopes.is_empty() && failure.missing_gateway_intents.is_empty() {
        Ok(())
    } else {
        Err(failure)
    }
}

pub(crate) fn missing_runtime_permissions_for(
    scope: RuntimePermissionScope,
    current: Permissions,
) -> Vec<&'static str> {
    let required = match scope {
        RuntimePermissionScope::CanonicalSurface => CANONICAL_SURFACE_PERMISSIONS,
        RuntimePermissionScope::LegacyReactionPath => LEGACY_REACTION_PERMISSIONS,
    };

    required
        .iter()
        .filter_map(|(permission, name)| (!current.contains(*permission)).then_some(*name))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use serenity::all::ChannelId;

    #[test]
    fn admin_term_maps_only_to_discord_permissions() {
        assert_eq!(
            admin_term_meaning(),
            AdminTermMeaning::DiscordNativePermissionHolder
        );
    }

    #[rstest]
    #[case::topic_change(
        NativeAdminOperation::TrackedStateTopicChange,
        TOPIC_CHANGE_CAPABILITIES
    )]
    #[case::bot_access_channel_override(
        NativeAdminOperation::BotAccessRepair(AccessControlMode::ChannelOverride),
        CHANNEL_OVERRIDE_CAPABILITIES
    )]
    #[case::bot_access_role_based(
        NativeAdminOperation::BotAccessRepair(AccessControlMode::RoleBased),
        ROLE_BASED_CAPABILITIES
    )]
    #[case::user_access_channel_override(
        NativeAdminOperation::UserAccessRepair(AccessControlMode::ChannelOverride),
        CHANNEL_OVERRIDE_CAPABILITIES
    )]
    #[case::user_access_role_based(
        NativeAdminOperation::UserAccessRepair(AccessControlMode::RoleBased),
        ROLE_BASED_CAPABILITIES
    )]
    #[case::thread_command(
        NativeAdminOperation::ThreadCommandExecution,
        THREAD_COMMAND_CAPABILITIES
    )]
    #[case::unarchive(
        NativeAdminOperation::CanonicalThreadUnarchive,
        THREAD_MAINTENANCE_CAPABILITIES
    )]
    #[case::duplicate_delete(
        NativeAdminOperation::DuplicateThreadDeletion,
        THREAD_MAINTENANCE_CAPABILITIES
    )]
    fn permission_mapping_matrix_matches_native_discord_permissions(
        #[case] operation: NativeAdminOperation,
        #[case] expected: &[NativeAdminCapability],
    ) {
        assert_eq!(native_admin_capabilities(operation), expected);
    }

    #[rstest]
    #[case::ready(
        vec!["bot", "applications.commands"],
        required_gateway_intents(),
        Ok(())
    )]
    #[case::missing_scope_and_intent(
        vec!["bot"],
        GatewayIntents::GUILDS
            | GatewayIntents::GUILD_MESSAGES
            | GatewayIntents::GUILD_MEMBERS
            | GatewayIntents::GUILD_MESSAGE_REACTIONS,
        Err(StartupReadinessFailure {
            missing_oauth_scopes: vec!["applications.commands"],
            missing_gateway_intents: vec!["MESSAGE_CONTENT"],
        })
    )]
    fn missing_scopes_or_intents_fail_startup_or_readiness(
        #[case] oauth_scopes: Vec<&str>,
        #[case] intents: GatewayIntents,
        #[case] expected: Result<(), StartupReadinessFailure>,
    ) {
        assert_eq!(validate_startup_readiness(oauth_scopes, intents), expected);
    }

    #[test]
    fn runtime_permission_matrix_and_add_reactions_scope() {
        let canonical_missing = missing_runtime_permissions_for(
            RuntimePermissionScope::CanonicalSurface,
            Permissions::VIEW_CHANNEL
                | Permissions::READ_MESSAGE_HISTORY
                | Permissions::SEND_MESSAGES
                | Permissions::SEND_MESSAGES_IN_THREADS
                | Permissions::ATTACH_FILES
                | Permissions::CREATE_PUBLIC_THREADS
                | Permissions::MANAGE_THREADS,
        );
        let reaction_missing = missing_runtime_permissions_for(
            RuntimePermissionScope::LegacyReactionPath,
            Permissions::VIEW_CHANNEL | Permissions::READ_MESSAGE_HISTORY,
        );

        assert_eq!(canonical_missing, Vec::<&'static str>::new());
        assert_eq!(reaction_missing, vec!["Add Reactions"]);
    }

    #[rstest]
    #[case::admin_required(true, ParentAdminSurfaceAuthorization::Allowed)]
    #[case::admin_missing(false, ParentAdminSurfaceAuthorization::MissingNativeAdminPermission)]
    fn parent_admin_surfaces_require_only_native_discord_admin_state(
        #[case] has_required_native_permission: bool,
        #[case] expected: ParentAdminSurfaceAuthorization,
    ) {
        assert_eq!(
            authorize_parent_admin_surface(has_required_native_permission),
            expected
        );
    }

    #[rstest]
    #[case::ready(true, true, ThreadCommandReadiness::Ready)]
    #[case::missing_thread_access(false, false, ThreadCommandReadiness::MissingThreadAccess)]
    #[case::missing_thread_command_permission(
        true,
        false,
        ThreadCommandReadiness::MissingThreadCommandPermission
    )]
    fn thread_command_readiness_rechecks_current_access_and_command_eligibility(
        #[case] can_open_thread: bool,
        #[case] can_use_thread_commands: bool,
        #[case] expected: ThreadCommandReadiness,
    ) {
        assert_eq!(
            thread_command_readiness(can_open_thread, can_use_thread_commands),
            expected
        );
    }

    fn channel_recovery_reference() -> LocatorRecoveryReference {
        LocatorRecoveryReference::channel(
            ChannelId::new(10),
            Some("https://discord.example/channels/1/10"),
        )
    }

    fn ledger_recovery_reference() -> LocatorRecoveryReference {
        LocatorRecoveryReference::ledger("abcd1234", Some("https://discord.example/channels/1/20"))
    }

    #[test]
    fn parent_channel_access_failure_uses_fixed_setup_copy_and_channel_reference() {
        let message = render_parent_channel_access_failure_message(&channel_recovery_reference());

        assert_eq!(
            message.body(),
            [
                i18n::bot_cannot_operate_parent_channel_message().to_owned(),
                "復旧用の参照: channel:10 | <https://discord.example/channels/1/10>".to_owned(),
            ]
            .join("\n")
        );
        assert_eq!(
            serde_json::to_value(message.components()).expect("components should serialize")[0]["components"]
                [0]["label"],
            i18n::open_parent_channel_label()
        );
    }

    #[test]
    fn archived_thread_recovery_uses_fixed_copy_and_thread_reference() {
        let message = render_archived_thread_recovery_message(&ledger_recovery_reference());

        assert_eq!(
            message.body(),
            [
                i18n::archived_thread_recovery_message().to_owned(),
                "復旧用の参照: ledger:abcd1234 | <https://discord.example/channels/1/20>"
                    .to_owned(),
            ]
            .join("\n")
        );
        assert_eq!(
            serde_json::to_value(message.components()).expect("components should serialize")[0]["components"]
                [0]["label"],
            i18n::open_ledger_thread_label()
        );
    }

    #[rstest]
    #[case::unauthorized(
        LedgerRefreshAcknowledgement::Unauthorized,
        i18n::ledger_refresh_admin_only_message()
    )]
    #[case::no_thread(
        LedgerRefreshAcknowledgement::ReadyNoThread,
        i18n::ledger_refresh_no_thread_message()
    )]
    #[case::ready(
        LedgerRefreshAcknowledgement::Ready,
        i18n::ledger_refresh_ready_message()
    )]
    fn ledger_refresh_acknowledgements_use_fixed_copy(
        #[case] acknowledgement: LedgerRefreshAcknowledgement,
        #[case] expected: &str,
    ) {
        assert_eq!(
            render_ledger_refresh_acknowledgement(acknowledgement),
            expected
        );
    }

    #[rstest]
    #[case::ledger(
        ledger_recovery_reference(),
        "復旧用の参照: ledger:abcd1234 | <https://discord.example/channels/1/20>"
    )]
    #[case::channel(
        channel_recovery_reference(),
        "復旧用の参照: channel:10 | <https://discord.example/channels/1/10>"
    )]
    fn ledger_refresh_uncertain_write_message_uses_step_4_recovery_reference_rules(
        #[case] recovery_reference: LocatorRecoveryReference,
        #[case] expected_reference_line: &str,
    ) {
        let message = render_ledger_refresh_uncertain_write_message(&recovery_reference);

        assert_eq!(
            message.body(),
            [
                i18n::ledger_refresh_uncertain_write_message().to_owned(),
                expected_reference_line.to_owned(),
            ]
            .join("\n")
        );
    }

    #[test]
    fn ledger_refresh_command_builder_uses_fixed_name_and_description() {
        let command =
            serde_json::to_value(ledger_refresh_command()).expect("command should serialize");

        assert_eq!(command["name"], "ledger-refresh");
        assert_eq!(
            command["description"],
            i18n::slash_ledger_refresh_description()
        );
    }
}
