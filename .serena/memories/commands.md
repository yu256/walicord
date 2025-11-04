### Running the bot

To run the bot, you need to set the `DISCORD_TOKEN` and `TARGET_CHANNEL_IDS` environment variables. You can create a `.env` file in the root of the project to store these variables.

```sh
cargo run -p walicord
```

### Bot Commands

- `!variables`: Displays the current state of variables and groups.
- `!evaluate`: Calculates and displays the final settlement of debts.

### Running the interpreter

To run the interpreter, you can use the following command:

```sh
cargo run --package walicord_interpreter -- <file.walicord>
```

### Development Commands

- **Testing:** `cargo test`
- **Formatting:** `cargo +nightly fmt`
- **Linting:** `cargo +nightly clippy --fix --allow-dirty`