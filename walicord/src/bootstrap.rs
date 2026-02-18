use crate::{
    channel::ChannelManager,
    discord::{roster::MemberRosterProvider, service::DiscordChannelService},
    handler::BotHandler,
    message_cache::MessageCache,
};
use serenity::{Client, all::GatewayIntents};
use std::env;
use walicord_application::MessageProcessor;
use walicord_infrastructure::{WalicordProgramParser, WalicordSettlementOptimizer};

/// Application configuration and dependency injection container
pub struct AppConfig {
    pub token: String,
    pub intents: GatewayIntents,
}

impl AppConfig {
    pub fn from_env() -> Self {
        let _ = dotenvy::dotenv();
        let token = env::var("DISCORD_TOKEN").expect("DISCORD_TOKEN is not set");
        let intents = GatewayIntents::GUILD_MESSAGES
            | GatewayIntents::MESSAGE_CONTENT
            | GatewayIntents::GUILDS
            | GatewayIntents::GUILD_MEMBERS
            | GatewayIntents::GUILD_MESSAGE_REACTIONS;

        Self { token, intents }
    }
}

/// Builds and configures the Discord client with all dependencies
pub struct AppBuilder;

impl AppBuilder {
    pub async fn build(config: AppConfig) -> Result<Client, serenity::Error> {
        let processor = MessageProcessor::new(&WalicordProgramParser, &WalicordSettlementOptimizer);
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

        Client::builder(&config.token, config.intents)
            .event_handler(handler)
            .await
    }
}

/// Initialize logging and tracing
pub fn init_logging() {
    tracing_subscriber::fmt::init();
}

/// Run the application with proper error handling
pub async fn run() {
    init_logging();

    let config = AppConfig::from_env();

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
