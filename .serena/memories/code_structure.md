The project is a Rust workspace with five crates:

- `walicord`: The main application, which is a Discord bot. It is responsible for handling Discord events and interacting with the Discord API.
- `walicord_core`: The core logic of the application. It contains the application services, domain model, and infrastructure for parsing and processing the walicord language.
- `walicord-parser`: A crate responsible for parsing a custom language used for tracking expenses.
- `walicord-calc`: A crate that handles the logic for calculating the optimal debt settlement using linear programming.
- `walicord_interpreter`: A command-line tool for running `.walicord` scripts.