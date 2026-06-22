use super::locator::LocatorRecoveryReference;
use serenity::{
    all::Permissions,
    builder::{CreateActionRow, CreateButton, CreateCommand},
};
use walicord_i18n as i18n;

const CANONICAL_SURFACE_PERMISSIONS: &[(Permissions, &str)] =
    &[(Permissions::VIEW_CHANNEL, "View Channel")];

#[cfg(test)]
const TOPIC_CHANGE_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::ManageChannels,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

#[cfg(test)]
const CHANNEL_OVERRIDE_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::ManageChannels,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

#[cfg(test)]
const ROLE_BASED_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::ManageRoles,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

#[cfg(test)]
const THREAD_COMMAND_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::UseApplicationCommandsOrEquivalent,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

#[cfg(test)]
const THREAD_MAINTENANCE_CAPABILITIES: &[NativeAdminCapability] = &[
    NativeAdminCapability::ManageThreads,
    NativeAdminCapability::Administrator,
    NativeAdminCapability::ServerOwner,
];

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AdminTermMeaning {
    DiscordNativePermissionHolder,
}

#[cfg(test)]
pub(crate) fn admin_term_meaning() -> AdminTermMeaning {
    AdminTermMeaning::DiscordNativePermissionHolder
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AccessControlMode {
    ChannelOverride,
    RoleBased,
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NativeAdminOperation {
    TrackedStateTopicChange,
    BotAccessRepair(AccessControlMode),
    UserAccessRepair(AccessControlMode),
    ThreadCommandExecution,
    CanonicalThreadUnarchive,
    DuplicateThreadDeletion,
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NativeAdminCapability {
    ManageChannels,
    ManageRoles,
    ManageThreads,
    UseApplicationCommandsOrEquivalent,
    Administrator,
    ServerOwner,
}

#[cfg(test)]
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
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ParentAdminSurfaceAuthorization {
    Allowed,
    MissingNativeAdminPermission,
}

#[cfg(test)]
pub(crate) fn authorize_parent_admin_surface(
    has_required_native_permission: bool,
) -> ParentAdminSurfaceAuthorization {
    if has_required_native_permission {
        ParentAdminSurfaceAuthorization::Allowed
    } else {
        ParentAdminSurfaceAuthorization::MissingNativeAdminPermission
    }
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ThreadCommandReadiness {
    Ready,
    MissingThreadAccess,
    MissingThreadCommandPermission,
}

#[cfg(test)]
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

    #[cfg(test)]
    pub(crate) fn body(&self) -> &str {
        &self.body
    }

    #[cfg(test)]
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
            CreateButton::new_link(thread_link.clone()).label(i18n::OPEN_LEDGER_THREAD_LABEL),
        ])],
        LocatorRecoveryReference::Channel {
            channel_link: Some(channel_link),
            ..
        } => vec![CreateActionRow::Buttons(vec![
            CreateButton::new_link(channel_link.clone()).label(i18n::OPEN_PARENT_CHANNEL_LABEL),
        ])],
        LocatorRecoveryReference::Ledger {
            thread_link: None, ..
        }
        | LocatorRecoveryReference::Channel {
            channel_link: None, ..
        } => Vec::new(),
    }
}

#[cfg(test)]
pub(crate) fn render_parent_channel_access_failure_message(
    recovery_reference: &LocatorRecoveryReference,
) -> RecoveryOutcomeMessage {
    RecoveryOutcomeMessage::new(
        [
            i18n::BOT_CANNOT_OPERATE_PARENT_CHANNEL_MESSAGE.to_owned(),
            recovery_reference.render_line(),
        ]
        .join("\n"),
        recovery_reference_components(recovery_reference),
    )
}

#[cfg(test)]
pub(crate) fn render_archived_thread_recovery_message(
    recovery_reference: &LocatorRecoveryReference,
) -> RecoveryOutcomeMessage {
    RecoveryOutcomeMessage::new(
        [
            i18n::ARCHIVED_THREAD_RECOVERY_MESSAGE.to_owned(),
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
            i18n::LEDGER_REFRESH_ADMIN_ONLY_MESSAGE.to_owned()
        }
        LedgerRefreshAcknowledgement::ReadyNoThread => {
            i18n::LEDGER_REFRESH_NO_THREAD_MESSAGE.to_owned()
        }
        LedgerRefreshAcknowledgement::Ready => i18n::LEDGER_REFRESH_READY_MESSAGE.to_owned(),
    }
}

pub(crate) fn render_ledger_refresh_uncertain_write_message(
    recovery_reference: &LocatorRecoveryReference,
) -> RecoveryOutcomeMessage {
    RecoveryOutcomeMessage::new(
        [
            i18n::LEDGER_REFRESH_UNCERTAIN_WRITE_MESSAGE.to_owned(),
            recovery_reference.render_line(),
        ]
        .join("\n"),
        recovery_reference_components(recovery_reference),
    )
}

pub(crate) fn ledger_refresh_command() -> CreateCommand {
    CreateCommand::new("ledger-refresh").description(i18n::SLASH_LEDGER_REFRESH_DESCRIPTION)
}

pub(crate) fn missing_runtime_permissions_for(
    scope: RuntimePermissionScope,
    current: Permissions,
) -> Vec<&'static str> {
    let required = match scope {
        RuntimePermissionScope::CanonicalSurface => CANONICAL_SURFACE_PERMISSIONS,
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

    #[test]
    fn canonical_runtime_permission_matrix_requires_only_parent_visibility() {
        let canonical_missing = missing_runtime_permissions_for(
            RuntimePermissionScope::CanonicalSurface,
            Permissions::VIEW_CHANNEL,
        );

        assert_eq!(canonical_missing, Vec::<&'static str>::new());
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
                i18n::BOT_CANNOT_OPERATE_PARENT_CHANNEL_MESSAGE.to_owned(),
                "復旧用の参照: channel:10 | <https://discord.example/channels/1/10>".to_owned(),
            ]
            .join("\n")
        );
        assert_eq!(
            serde_json::to_value(message.components()).expect("components should serialize")[0]["components"]
                [0]["label"],
            i18n::OPEN_PARENT_CHANNEL_LABEL
        );
    }

    #[test]
    fn archived_thread_recovery_uses_fixed_copy_and_thread_reference() {
        let message = render_archived_thread_recovery_message(&ledger_recovery_reference());

        assert_eq!(
            message.body(),
            [
                i18n::ARCHIVED_THREAD_RECOVERY_MESSAGE.to_owned(),
                "復旧用の参照: ledger:abcd1234 | <https://discord.example/channels/1/20>"
                    .to_owned(),
            ]
            .join("\n")
        );
        assert_eq!(
            serde_json::to_value(message.components()).expect("components should serialize")[0]["components"]
                [0]["label"],
            i18n::OPEN_LEDGER_THREAD_LABEL
        );
    }

    #[rstest]
    #[case::unauthorized(
        LedgerRefreshAcknowledgement::Unauthorized,
        i18n::LEDGER_REFRESH_ADMIN_ONLY_MESSAGE
    )]
    #[case::no_thread(
        LedgerRefreshAcknowledgement::ReadyNoThread,
        i18n::LEDGER_REFRESH_NO_THREAD_MESSAGE
    )]
    #[case::ready(
        LedgerRefreshAcknowledgement::Ready,
        i18n::LEDGER_REFRESH_READY_MESSAGE
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
                i18n::LEDGER_REFRESH_UNCERTAIN_WRITE_MESSAGE.to_owned(),
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
            i18n::SLASH_LEDGER_REFRESH_DESCRIPTION
        );
    }
}
