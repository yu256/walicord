use crate::{
    channel::ChannelManager,
    discord::{roster::MemberRosterProvider, service::DiscordChannelService},
    handler::BotHandler,
    message_cache::MessageCache,
};
use serenity::{Client, all::GatewayIntents};
use std::{env, path::PathBuf};
use walicord_application::MessageProcessor;
use walicord_infrastructure::{
    HighsSettlementPlanner, WalicordProgramParser, acquire_instance_lock,
};

const GATEWAY_INTENTS: GatewayIntents = GatewayIntents::GUILDS
    .union(GatewayIntents::GUILD_MESSAGES)
    .union(GatewayIntents::MESSAGE_CONTENT)
    .union(GatewayIntents::GUILD_MEMBERS)
    .union(GatewayIntents::GUILD_MESSAGE_REACTIONS);

pub struct AppConfig {
    pub token: String,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum AppConfigError {
    #[error("Discord bot token is missing from configuration")]
    MissingToken,
}

impl AppConfig {
    pub fn from_env() -> Result<Self, AppConfigError> {
        let _ = dotenvy::dotenv();
        let token = env::var("DISCORD_TOKEN").map_err(|_| AppConfigError::MissingToken)?;
        Ok(Self { token })
    }
}

/// Builds and configures the Discord client with all dependencies
pub struct AppBuilder;

impl AppBuilder {
    pub async fn build(config: AppConfig) -> Result<Client, serenity::Error> {
        let processor = MessageProcessor::new(&WalicordProgramParser, &HighsSettlementPlanner);
        let channel_service = DiscordChannelService;
        let roster_provider = MemberRosterProvider::new(channel_service);
        let message_cache = MessageCache::new();
        let channel_manager = ChannelManager::new();

        let handler: BotHandler<'_, DiscordChannelService, MemberRosterProvider> = BotHandler::new(
            message_cache,
            channel_service,
            roster_provider,
            processor,
            channel_manager,
        );

        Client::builder(&config.token, GATEWAY_INTENTS)
            .event_handler(handler)
            .await
    }
}

/// Initialize logging and tracing
pub fn init_logging() {
    tracing_subscriber::fmt::init();
}

fn runtime_instance_lock_path() -> PathBuf {
    std::env::temp_dir()
        .join("walicord-ledger-locks")
        .join("runtime-instance.lock")
}

/// Run the application with proper error handling
pub async fn run() {
    init_logging();

    let _instance_lock = match acquire_instance_lock(runtime_instance_lock_path()) {
        Ok(lock) => lock,
        Err(error) => {
            tracing::error!(%error, "failed to acquire startup instance lock");
            std::process::exit(1);
        }
    };

    let config = match AppConfig::from_env() {
        Ok(config) => config,
        Err(AppConfigError::MissingToken) => {
            tracing::error!("DISCORD_TOKEN is not set");
            std::process::exit(1);
        }
    };

    let mut client = match AppBuilder::build(config).await {
        Ok(client) => client,
        Err(e) => {
            tracing::error!("Failed to create client: {:?}", e);
            std::process::exit(1);
        }
    };

    if let Err(why) = client.start().await {
        tracing::error!("Client error: {:?}", why);
    }
}
