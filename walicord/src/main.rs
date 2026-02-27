#![warn(clippy::uninlined_format_args)]

mod bootstrap;
mod channel;
mod discord;
mod handler;
mod message_cache;
mod reaction;
mod role_visibility_feedback;
mod settlement;
#[cfg(test)]
mod test_utils;

use bootstrap::run;

#[tokio::main]
async fn main() {
    run().await;
}
