# Transaction Construction

This document defines how Walicord constructs settlement transfers from final balances produced by settlement mathematics.

Unlike a design note, this page is written as a product-facing specification: it defines behavior, guarantees, and command semantics in terms that can be exposed in README-level documentation.

## Overview

Settlement in Walicord has two layers:

1. **Settlement mathematics** computes final balances on the atomic-unit grid with exact zero-sum.
2. **Transaction construction** converts those balances into concrete transfers `(from, to, amount)`.

This document covers layer (2) only.

## Background

Historically, split-payment products often focused on one of two goals:

- keep the number of transfers small,
- keep transfer amounts human-friendly for cash handling.

In practice, these goals can conflict. A transfer plan with fewer edges can still create awkward payer-side amounts (for example, many non-`100`/non-`1000` units), while a cash-friendly plan can increase counterpart involvement.

Walicord therefore defines an explicit objective order instead of relying on implicit heuristics.

## Inputs and Outputs

### Inputs

- `b[m]: Money` — quantized final balance per member.
- `settle_members: Set<MemberId>` — members explicitly requested to settle now.
- `cash_members: Set<MemberId>` — members whose transfers should avoid loose change (payer or payee side).

Input contract for this layer:

- `b[m]` must already be on the settlement atomic-unit grid and convertible to i64 atomic units.
- This contract is guaranteed by the settlement mathematics (quantization) layer before
  transaction construction is invoked.

### Sign Convention (Authoritative for this layer)

- `b[m] > 0` => `m` is a **payer**.
- `b[m] < 0` => `m` is a **receiver**.

### Outputs

- `transfers: List<(from, to, amount)>`
- `new_balances: Map<MemberId, Money>` after applying `transfers`.

## Core Guarantees

For valid input balances, transaction construction guarantees:

1. **No rebalance of input balances**: the input `b[m]` is treated as fixed.
2. **Atomic-unit transfers**: every transfer amount is on the settlement atomic-unit grid.
3. **Conservation**: transfers preserve total money flow exactly.
4. **Partial settlement**:
   - every member in `settle_members` is settled to exactly zero,
   - members outside `settle_members` may remain non-zero.
5. **Determinism**: identical inputs produce identical transfer content and ordering.

## Complexity Guardrail

The current implementation solves lexicographic objectives by repeated MILP calls.
To bound latency for chat-bot usage, the solver rejects oversized bipartite edge sets.

- Let `E = #payers * #receivers` in the transfer model.
- If `E > 120`, transfer construction is rejected as `ModelTooLarge` (operational limit).

This bound is an operational safety guard, not a mathematical infeasibility condition.

## Error Modes and User Guidance

Transaction construction may return the following operational errors:

- `InvalidGrid`: cash-grid configuration is invalid (`g1 <= 0`, `g2 <= 0`, or `g1 % g2 != 0`).
  - Operator action: fix configuration values first.
- `ModelTooLarge`: transfer graph exceeds the operational edge budget (`E > 120`).
  - User/operator action:
    1. split settle-up into smaller member subsets,
    2. reduce `cash_members` scope when possible,
    3. retry with fewer active payer/receiver combinations.
- `NoSolution`: solver failed to produce an optimal solution under valid model constraints.
  - Operator action: retry, then inspect runtime/solver environment if persistent.
- `RoundingMismatch`: post-solve rounding consistency check failed.
  - Operator action: treat as integrity failure; retry and investigate numerics/runtime.

## Optimization Policy

Walicord uses exact lexicographic optimization (not greedy matching).

Current runtime profile uses fixed JPY-style cash grids: `G1=1000`, `G2=100`.
Cash-friendliness is evaluated for transfers that touch `cash_members` on either side.

Future extension note: when non-JPY settlement profiles are introduced end-to-end,
`G1/G2` should be derived from settlement context instead of fixed constants.

The solver minimizes, in this strict order:

1. **OBJ1**: number of cash-member transfers not divisible by `G1`.
2. **OBJ2**: among OBJ1-optimal solutions, number of cash-member transfers not divisible by `G2`.
3. **OBJW**: among OBJ1/OBJ2-optimal solutions, number of transfers involving non-`settle_members` counterparts.
4. **OBJ3**: among OBJ1/OBJ2/OBJW-optimal solutions, total transfer count.
5. **OBJ4**: among OBJ1/OBJ2/OBJW/OBJ3-optimal solutions, minimize the maximum transfer amount.
6. **Tie-break**: deterministic lexicographic minimization of edge amounts in fixed `(from asc, to asc)` order.

## Why this approach

- **Cash intent is explicit**: users can mark cash-oriented members, and the solver optimizes divisibility for transfers that touch them first.
- **Settle intent is preserved**: after cash objectives, `OBJW` prefers plans that avoid pulling in non-`settle_members` counterparts when possible.
- **Determinism for trust and UX**: the same input always yields the same transfer list and ordering, which is important for chat-based workflows and audits.
- **Exactness over heuristics**: lexicographic optimization provides globally optimal outcomes under the declared objective order.

## Transfer Extraction Rules

After optimization:

- emit one transfer per positive edge (`amount > 0`),
- do not emit duplicate `(from, to)` pairs,
- sort output by `(from, to)` ascending member ID,
- compute `new_balances` by applying transfers exactly once.

## Example

**Balances**

- A: `+1200` (payer)
- B: `-1000` (receiver)
- C: `-200` (receiver)
- `settle_members = {A, B, C}`
- `cash_members = {A}`

**Result shape**

- A pays B `1000`
- A pays C `200`

This is preferred over splits that increase non-`G1`/non-`G2` cash transfers or introduce non-settle counterparts.

## Command Surface

Walicord exposes cash preference in two script-level forms.

- Script-local persisted flag (effective while scanning the current script in order):
  - `!member set <set_expr> cash`
  - `!cash` (self)
- Per-command option:
  - `!settleup <set_expr> [--cash <set_expr>]`

Effective cash set at execution time:

- `effective_cash_members = script_local_cash_members ∪ command_cash_members`

Here, "script-local persisted" means state is carried across earlier commands in the same
evaluated script sequence. It is not durable storage across independent executions.

Cash membership affects transaction construction only. It does not alter settlement mathematics.

## Relationship to Settlement Mathematics

Settlement mathematics and transaction construction intentionally separate responsibilities:

- settlement mathematics guarantees valid final balances,
- transaction construction guarantees transfer realization under cash/settle objectives.

For quantization and repair details, see `docs/design/settlement-mathematics.md`.
