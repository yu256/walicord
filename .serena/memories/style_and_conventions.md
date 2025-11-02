The project follows standard Rust conventions.

- **Formatting:** Code is formatted with `rustfmt` (run with `cargo +nightly fmt`).
- **Linting:** `clippy` is used for linting (run with `cargo +nightly clippy --fix --allow-dirty`).
- **Naming:** Standard Rust naming conventions are used (e.g., `snake_case` for variables and functions, `PascalCase` for types).
- **Modularity:** The code is organized into modules and crates to separate concerns. The main `walicord` crate follows a layered architecture (`application`, `domain`, `infrastructure`).