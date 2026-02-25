---
name: test-strategy
description: "CRITICAL: Use for any testing request (TDD, rstest, fixture, case design, test refactor). Enforces t-wada style: declarative cases, no bool-driven branching in test bodies, and no rstest when it does not improve readability."
---
# Test Strategy Skill

This skill guides test planning and implementation with t-wada style TDD and high readability.

## Trigger Signals

Use this skill when the user asks about any of:
- test design, test refactor, or test readability
- TDD / red-green-refactor
- `rstest`, `fixture`, `case` / `values` usage
- coverage, regression tests, edge cases, flaky tests

If this skill is triggered, follow it even when the task is mainly implementation.

## Principles

- Test intent, not implementation details
- Prefer small, deterministic tests over large end-to-end tests
- Red -> Green -> Refactor when adding/changing behavior
- Keep tests declarative: case data describes behavior; test body should not branch by flags
- Do not branch on `Result`/case type inside tests (`if`/`match` for scenario routing is forbidden)
- Prefer injecting `Result<Success, Error>` as case data and comparing `actual` vs `expected` directly when the assertion shape is identical
- Do not extract errors with `.err().expect(...)` / `expect_err(...)` when direct `Result` equality is enough
- If `rstest` does not improve readability, use `#[test]` instead
- Keep fixtures when they reduce setup noise and improve intent clarity

## Rstest Policy

Use `rstest` when:
- Multiple examples share one behavior and assertion shape
- `fixture` injection removes repetitive setup
- `#[values]` or `#[case]` makes variation explicit and readable

Avoid `rstest` when:
- There is only one meaningful case and no fixture/value benefit
- Case inputs are control flags (`true/false`, mode switches) that cause `if` branching in the test body
- Replacing clear `#[values]` with artificial case labels makes intent worse

Case design rules:
- Prefer expected outcome objects/enums over boolean control parameters
- One assertion path per case
- Case names should encode behavior + context only when names are actually informative
- Keep normal path and error path in one parameterized test by default when both can be validated by direct `Result` equality
- Split normal path and error path only when assertion shapes differ or readability improves

## Process

### Step 1: Clarify the Behavior

Identify the observable outcomes:
- Inputs, outputs, and errors
- Boundary and edge cases
- Existing constraints in the codebase

If any key behavior is ambiguous, ask the user for clarification.

### Step 2: Write the First Failing Test (Red)

Pick the smallest externally visible behavior change and make it fail first.

### Step 3: Design Declarative Cases

Use equivalence classes and edge cases:
- Normal path
- Boundary values
- Empty or missing inputs
- Error or invalid inputs
- Interaction patterns (ordering, duplicates, whitespace, etc.)

Avoid "flag case" tests like:

```rust
#[case(true)]
#[case(false)]
fn test_x(#[case] enabled: bool) {
    if enabled { ... } else { ... } // avoid
}
```

Prefer:

```rust
enum Expected { Success(...), Error(...) }
#[case(input_a, Expected::Success(...))]
#[case(input_b, Expected::Error(...))]
fn test_x(#[case] input: ..., #[case] expected: Expected) { ... }
```

For simple success/error validation, prefer `Result` equality (no branching in test body):

```rust
#[derive(Debug, PartialEq, Eq)]
enum MyError { ... }

#[rstest]
#[case(input_ok, Ok(ExpectedValue::A))]
#[case(input_ng, Err(MyError::InvalidToken))]
fn test_x(#[case] input: &str, #[case] expected: Result<ExpectedValue, MyError>) {
    let actual = parse(input);
    assert_eq!(actual, expected);
}
```

Avoid:

```rust
let err = parse(input).err().expect("should fail");
assert_eq!(err, expected_error);
```

If an error type lacks equality:
- Add `PartialEq, Eq` to the error type when reasonable, or
- Add a small test-only assertion helper that compares semantic fields without control-flow branching

### Step 4: Choose `#[test]` vs `rstest`

- Keep `#[test]` for single-scenario tests
- Use `rstest` only when parameterization or fixtures materially improve readability
- Preserve existing fixtures unless there is a clear simplification gain

### Step 5: Use Clear Case Names

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

### Step 6: Assert Intent

Avoid `is_ok()` only tests for behavior that has structure.
Prefer declarative `assert_eq!` on structured outputs, domain results, or error types.
For sequences, compare slices when possible.
Prefer direct `Result` comparison with case-injected `expected` when possible.

### Step 7: Keep Tests Focused

- One reason to fail per test
- Avoid multiple behavior checks in one case
- Use helpers for repeated construction
