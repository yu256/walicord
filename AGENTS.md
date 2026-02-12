# Agent Guidelines

## First Step

- Before thinking about the solution to a request, first read relevant project files to understand the context

## Code Comments Guidelines

**Avoid redundant comments:**

- Do NOT write comments that simply restate what the code obviously does
- Do NOT write comments that describe the structure (e.g., `{/* Header */}`, `// Footer section`)
- Do NOT write changelog-style comments (e.g., `// Added 2024-01-01`, `// Fixed bug`)

**Write comments only when:**

- Explaining WHY something is done (not WHAT is done)
- Documenting non-obvious business logic or algorithms
- Clarifying complex or counterintuitive code
- Providing context that cannot be expressed in code

**Prefer self-documenting code:**

- Use descriptive variable and function names
- Extract complex logic into well-named functions
- Structure code to be readable without comments

## Testing Rule

- Use t-wada style TDD for tests

## Default Commands

- Always run the following commands after code changes and before committing:
  - PowerShell: `cargo +nightly clippy --fix --allow-dirty --all-targets; cargo +nightly fmt; cargo test`
  - Bash/sh: `cargo +nightly clippy --fix --allow-dirty --all-targets && cargo +nightly fmt && cargo test`

## Architecture Rule

- The project adopts Tactical DDD
