---
name: test-strategy
description: "Guides test planning and case design with intent-driven coverage and clear rstest case naming."
---
# Test Strategy Skill

This skill helps you plan and implement tests with a focus on intent, coverage, and maintainability.

## Principles

- Test intent, not implementation details
- Prefer small, deterministic tests over large end-to-end tests
- Make case names explain the behavior and edge condition
- Add tests first when requirements are unclear

## Process

### Step 1: Clarify the Behavior

Identify the observable outcomes:
- Inputs, outputs, and errors
- Boundary and edge cases
- Existing constraints in the codebase

If any key behavior is ambiguous, ask the user for clarification.

### Step 2: Design Cases

Use equivalence classes and edge cases:
- Normal path
- Boundary values
- Empty or missing inputs
- Error or invalid inputs
- Interaction patterns (ordering, duplicates, whitespace, etc.)

### Step 3: Use Clear Case Names

Prefer `rstest` case labels for clarity:

```rust
#[rstest]
#[case::inline_comment_adjacent("1000円/*ランチ*/<@456>")]
#[case::line_comment_only("// メモ")]
fn test_accepts_line_comments(#[case] input: &str) { ... }
```

Naming guidelines:
- Use behavior + context: `case::line_comment_only`
- Avoid redundant words like "test" in case names
- Keep names short but precise

### Step 4: Assert Intent

Avoid `is_ok()` only tests for behavior that has structure.
Prefer declarative `assert_eq!` on structured outputs, domain results, or error types.
For sequences, compare slices when possible.

### Step 5: Keep Tests Focused

- One reason to fail per test
- Avoid multiple behavior checks in one case
- Use helpers for repeated construction

## Output Format

When proposing tests, provide:
- A short list of cases with intent
- The chosen test level
- The planned assertions
