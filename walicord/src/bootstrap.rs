use crate::{
    channel::ChannelManager,
    discord::{
        ledger::{StartupReadinessFailure, required_gateway_intents, validate_startup_readiness},
        roster::MemberRosterProvider,
        service::DiscordChannelService,
    },
    handler::BotHandler,
    message_cache::MessageCache,
};
use serenity::{Client, all::GatewayIntents};
use std::env;
use walicord_application::MessageProcessor;
use walicord_infrastructure::{HighsSettlementPlanner, WalicordProgramParser};

/// Application configuration and dependency injection container
pub struct AppConfig {
    pub token: String,
    pub intents: GatewayIntents,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum AppConfigError {
    #[error("Discord bot token is missing from configuration")]
    MissingToken,
    #[error("required OAuth scopes are missing from the bot install")]
    MissingOAuthScopes,
    #[error("startup readiness validation failed: {0}")]
    StartupReadiness(#[from] StartupReadinessFailure),
}

impl AppConfig {
    pub fn default_gateway_intents() -> GatewayIntents {
        required_gateway_intents()
    }

    pub fn validate_deployment_readiness<I, S>(
        oauth_scopes: I,
        intents: GatewayIntents,
    ) -> Result<(), StartupReadinessFailure>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        validate_startup_readiness(oauth_scopes, intents)
    }

    fn declared_oauth_scopes_from_env() -> Result<Vec<String>, AppConfigError> {
        let declared_scopes =
            env::var("DISCORD_OAUTH_SCOPES").map_err(|_| AppConfigError::MissingOAuthScopes)?;
        Ok(declared_scopes
            .split_whitespace()
            .map(str::to_owned)
            .collect())
    }

    pub fn from_env() -> Result<Self, AppConfigError> {
        let _ = dotenvy::dotenv();
        let token = env::var("DISCORD_TOKEN").map_err(|_| AppConfigError::MissingToken)?;
        let intents = Self::default_gateway_intents();
        let oauth_scopes = Self::declared_oauth_scopes_from_env()?;

        Self::validate_deployment_readiness(oauth_scopes.iter().map(String::as_str), intents)
            .map_err(AppConfigError::StartupReadiness)?;

        Ok(Self { token, intents })
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

    let config = match AppConfig::from_env() {
        Ok(config) => config,
        Err(AppConfigError::MissingToken) => {
            tracing::error!("DISCORD_TOKEN is not set");
            std::process::exit(1);
        }
        Err(AppConfigError::MissingOAuthScopes) => {
            tracing::error!("DISCORD_OAUTH_SCOPES is not set; startup readiness failed closed");
            std::process::exit(1);
        }
        Err(AppConfigError::StartupReadiness(failure)) => {
            tracing::error!(
                missing_oauth_scopes = ?failure.missing_oauth_scopes,
                missing_gateway_intents = ?failure.missing_gateway_intents,
                "discord-ledger startup readiness failed closed"
            );
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

#[cfg(test)]
mod tests {
    use super::AppConfig;
    use crate::discord::ledger::{StartupReadinessFailure, required_gateway_intents};

    #[test]
    fn bootstrap_requests_required_gateway_intents() {
        assert_eq!(
            AppConfig::default_gateway_intents(),
            required_gateway_intents()
        );
    }

    #[test]
    fn bootstrap_readiness_accepts_required_scopes_and_intents() {
        assert_eq!(
            AppConfig::validate_deployment_readiness(
                vec!["bot", "applications.commands"],
                AppConfig::default_gateway_intents()
            ),
            Ok(())
        );
    }

    #[test]
    fn bootstrap_readiness_fails_closed_when_scopes_are_missing() {
        assert_eq!(
            AppConfig::validate_deployment_readiness(
                vec!["bot"],
                AppConfig::default_gateway_intents()
            ),
            Err(StartupReadinessFailure {
                missing_oauth_scopes: vec!["applications.commands"],
                missing_gateway_intents: Vec::new(),
            })
        );
    }
}
