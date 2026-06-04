use serenity::{
    all::{ChannelId, Message},
    prelude::Context,
};

mod adapters;
mod expense_modal_open;
mod locator;
mod observability;
mod panel;
mod permissions;
mod projection;
mod response_writer;
mod route_guard;
mod router;
mod store;

#[cfg(test)]
pub(crate) use self::route_guard::startup_channel_is_track_target;
pub(crate) use self::{
    adapters::{
        DiscordLedgerCanonicalThreadCreator, DiscordLedgerThreadLoader, DiscordRouterRosterFetcher,
        discord_canonical_thread_locator,
    },
    observability::TracingLedgerObservability,
    permissions::ledger_refresh_command,
    response_writer::{safe_edit_interaction_response, safe_interaction_response_message},
    route_guard::{
        ChannelFlagAction, SlashScopeError, channel_flag_action, slash_scope_channel_id,
        startup_track_targets,
    },
    router::{InteractionDispatch, LedgerRouter, LedgerRouterDependencies},
    store::{DiscordCanonicalLedgerStore, WriterLineagePolicy},
};
pub(crate) use walicord_application::ledger::{
    expense_session::{
        ExpenseModalSubmissionBindingStore, ExpenseSessionStore, ModalRetryBindingStore,
        VoidSessionStore,
    },
    preview_store::PreviewStore,
    write_coordinator::{UncertainWriteRegistry, WriteCoordinator},
};
pub(crate) use walicord_infrastructure::{ProcessNonceProvider, SystemClock};

pub(crate) const LEDGER_ATTACHMENT_FILENAME: &str = "walicord-ledger-entry.json";

async fn fetch_all_channel_messages(
    ctx: &Context,
    channel_id: ChannelId,
) -> serenity::Result<Vec<Message>> {
    use serenity::builder::GetMessages;

    let mut all_messages = Vec::new();
    let mut last_message_id = None;
    loop {
        let mut builder = GetMessages::new().limit(100);
        if let Some(before) = last_message_id {
            builder = builder.before(before);
        }
        let messages = channel_id.messages(&ctx.http, builder).await?;
        if messages.is_empty() {
            break;
        }
        last_message_id = messages.last().map(|message| message.id);
        all_messages.extend(messages);
    }
    all_messages.reverse();
    Ok(all_messages)
}
