The project is a Rust workspace with three crates:

- `walicord`: The main application, which is a Discord bot. It's structured into `application`, `domain`, and `infrastructure` layers.
- `walicord-parser`: A crate responsible for parsing a custom language used for tracking expenses.
- `walicord-calc`: A crate that handles the logic for calculating the optimal debt settlement using linear programming.