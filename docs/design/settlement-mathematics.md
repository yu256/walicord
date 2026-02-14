# Settlement Mathematics

This document describes the mathematical foundations of Walicord's settlement system, focusing on rounding and adjustment algorithms that produce deterministic, bounded-impact, and zero-sum settlement outputs.

## Overview

Walicord uses a **deferred rounding** strategy for monetary calculations. Instead of rounding after each transaction, we maintain high-precision decimal amounts throughout intermediate calculations and only round at settlement time. This approach minimizes accumulated rounding errors and resolves remainders with a deterministic settlement rule.

**Atomic unit `u`** (settlement quantization step): output balances are constrained to `u * Z`.
Formally, for currency `scale`, `u = 10^{-scale}` (for example, JPY: `scale=0, u=1`; USD: `scale=2, u=0.01`).
In common currency settings this matches the smallest currency unit, but this document treats `u` as the settlement grid step by definition.

## Implementation Status

Implementation snapshot note: current settle-up runtime defaults to `SettlementContext::jpy_default()` (scale = 0, HalfUp). This is an implementation default and may change by release/profile.

Current settle-up note: for partial settle-up commands, quantization remains global (all balances), but adjustment ranking prioritizes settle-up members first. This reduces incidental quantization deltas on non-settling members while preserving global zero-sum in adjusted outputs.
**Important**: preferred members are not "rewarded"; they are prioritized to absorb repair impact so non-preferred members are less affected.

## Scope

- Quantization and repair are defined on the full group balance vector.
- Partial settle-up uses the same global quantization/repair and only changes candidate priority via `preferred_set`.
- **Subset-level settlement completion is produced by transfer construction, not by quantization itself.**
- For details on how transfers are generated from valid balances, see [Transaction Construction](transaction-construction.md).

## Reading Guide

- **Normative** sections define required behavior (input/reject conditions, quantization requirements, selection rule, diagnostics contract).
- **Informative** sections explain rationale, intuition, and operational interpretation.
- For repair behavior, `Selection Rule (Normative)` is the primary algorithm source; equivalent formulations are subordinate.

## Core Concepts

Terminology used in this document:
- **quantization**: per-participant rounding to atomic-unit grid (`q_i`, `rounded[i]`)
- **rounding**: used as a synonym for quantization unless explicitly stated otherwise
- **repair**: post-quantization zero-sum restoration (`adjusted[i]`)
- **balance sign convention**: positive = debt (net pay), negative = credit (net receive)
- **transfer-direction semantics**: positive side pays negative side

### 1. Deferred Rounding

Traditional split-payment systems often distribute remainders sequentially (e.g., allocating `+1000` across three credits as `+334, +333, +333`). This creates a deterministic order bias where earlier participants systematically absorb the larger remainder.
In deferred rounding, this burden is not fixed at transaction time; it is resolved once at settlement under a global deterministic rule.

Walicord's approach:
- **During transactions**: Split `100 / 3` into three high-precision shares of `33.333...` (stored in finite-precision decimal fixed-point representation)
- **During aggregation**: Accumulate these high-precision balances
- **At settlement**: Round to atomic units (e.g., 1 yen) with zero-sum adjustment

Split-stage residuals are not force-absorbed per transaction; they are resolved once in settlement by the global zero-sum adjustment.

### 2. Zero-Sum Constraint

A valid settlement must satisfy:

```
Σ balances = 0
```

This ensures no money is created or destroyed during settlement.

Scope of guarantee: quantization is per-member and can introduce zero-sum violation; the subsequent repair step enforces exact zero-sum on the full balance vector. In partial settle-up workflows, subset-level zeroing is achieved by the settlement transfer algorithm, not by quantization alone.

When rounding high-precision balances to atomic units, this constraint may be violated:

```
Original:  A: +666.666..., B: -333.333..., C: -333.333...
Rounded:   A: +667,        B: -333,        C: -333
Sum:       667 - 333 - 333 = 1 ≠ 0  ← Violation!
```

### 3. Minimal Adjustment Algorithm

To restore the zero-sum constraint with bounded one-step impact, we use the **Zero-Sum Minimal Adjustment** algorithm.

#### Processing Flow

```text
High-precision balances
        |
        |  input validation: |Σ original| <= epsilon
        v
Per-member rounding to atomic unit
        |
        v
Compute V_amt = Σ rounded[i] and V_int = V_amt / u
        |
        |-- V_int = 0 --> done
        |
        v
Select adjustment candidates (diff + stable_key)
        |
        v
Apply ±u one-step adjustments
        |
        v
Adjusted balances with exact zero-sum
```

#### Function Contract

```text
input:
  original[i]: Decimal
  u: atomic unit
  rounding_mode: {HalfUp, HalfEven}
  preferred_set: optional subset of participant IDs
  stable_key_profile: implementation-defined versioned profile
  epsilon: imbalance tolerance

output:
  adjusted[i]: Decimal in u * Z
  diagnostics (emitted via logs): {V_int, selected_ids, bound flags}
```

#### Core Invariants (Normative)

- Precondition: input must satisfy `|Σ original| <= ε` (otherwise rejected).
- `V_int = Σ q_i ∈ Z`
- `adjusted[i] ∈ u * Z`
- `Σ adjusted[i] = 0`
- for one-step repair, `Σ |adjusted[i] - rounded[i]| = |V_amt|`

#### Definitions (Normative)

Given:
- `original[i]` — high-precision balance for participant *i*
- `q_i = quantize_to_int(original[i], u, mode)` — integer quantized balance for participant *i*
- `rounded[i] = q_i * u` — quantized balance in currency unit
- `diff[i] = rounded[i] - original[i]` — rounding displacement for participant *i*
- `u` — atomic unit (`u > 0` by definition)

Definition lock (normative terminology):
- `V_int := Σ q_i ∈ Z` (atomic-unit zero-sum violation)
- `V_amt := Σ rounded[i] = V_int * u` (amount-domain zero-sum violation)
- in this document, "zero-sum violation" always refers to `V_amt`/`V_int` by domain

Consequence: since `V_amt ∈ u * Z`, `V_int` is well-defined as the unique integer satisfying `V_amt = V_int * u`.
Implementation note: branch decisions and adjustment count are driven by `V_int`.

Rounding mode is part of the specification (`quantize_to_int`):
- `HalfUp`: midpoint (`.5`) rounds away from zero
- `HalfEven`: midpoint (`.5`) rounds to nearest even integer

`quantize_to_int` is defined as nearest integer quantization of `x = original[i] / u` with explicit tie handling:
- HalfUp: nearest with ties away from zero
- HalfEven: nearest with ties to even

#### Quantization Requirements (Normative)

- Evaluability target: implementations SHOULD choose `u` and accepted input ranges so `x = original / u` is evaluable within decimal-engine constraints under the documented operating envelope.
- Envelope definition: in this document, "supported operating envelope" means implementation-accepted maxima for absolute balance magnitude, participant count, and `scale`; these bounds MUST be documented as implementation constants (values may live in this document or a referenced implementation profile).
- Quantization rule: implementation MUST apply nearest-integer quantization with explicit tie handling (HalfUp or HalfEven) exactly once at integer quantization.
- Failure behavior: if representability/overflow/precision assumptions required for correct `q_i` are violated, implementation MUST reject with diagnostics (never silently emit incorrect `q_i`).
- Validation policy: implementations SHOULD keep a debug/test cross-check path (for example, scale-shift quantization) and reject on mismatch.

Minimum reject conditions for quantization paths:
- MUST reject if the implementation's chosen quantization evaluation path cannot be evaluated under decimal-engine constraints (precision/exponent/engine limits).
- MUST reject if midpoint-rounded value cannot be converted to integer (`q_i`) after integer quantization.
- MUST reject in debug/test cross-check mode when division path and scale-shift path disagree.
- MUST reject if `q_i` exceeds the implementation-defined integer range. (Current implementation: `i128`.)

#### Implementation facts (Informative)

Implementation fact: `quantize_to_int` is defined in one canonical function and reused by settlement paths.
Implementation fact (Rust): `HalfUp` maps to `rust_decimal::RoundingStrategy::MidpointAwayFromZero`, and `HalfEven` maps to `rust_decimal::RoundingStrategy::MidpointNearestEven`.
Implementation note: in the `rust_decimal` model, `original / u` and midpoint handling are evaluated in Decimal arithmetic, and midpoint rounding is applied exactly once at integer quantization (`round_dp_with_strategy(0, ...)`).
Implementation note: an alternative implementation path is scale-shift quantization (`original * 10^scale` then integer midpoint rounding) with explicit overflow bounds.
Typical failure diagnostics include: division/scale overflow, non-integer conversion failure after midpoint rounding, or detected mismatch against validation path in test/debug builds.
Implementation fact: candidate ranking uses `diff = rounded - original` numeric comparison directly (not `original/u` division-derived ordering).
Implementation fact: Decimal comparisons are numeric-value based (scale-insensitive, for example `1.0 == 1.00`), and equality/tie checks use strict numeric equality (no tolerance band).
Implementation note: if the decimal engine allows multiple internal scale forms, values used in `diff` computation typically follow one canonical numeric path so equal amounts compare equal.
Implementation fact: `rounded` is computed as `q_i * u`.
By construction, each `rounded[i]` lies in `u * Z`.

**Sign interpretation**:
- `diff[i] > 0` ⇔ participant balance increased by rounding (favorable to participant)
- `diff[i] < 0` ⇔ participant balance decreased by rounding (unfavorable to participant)

| Original | Rounded | diff | Interpretation |
|:--------:|:-------:|:----:|:---------------|
| +10.6 | +11 | +0.4 | Gained (rounded up) |
| +10.4 | +10 | -0.4 | Lost (rounded down) |
| -10.6 | -11 | -0.4 | Lost (more debt) |
| -10.4 | -10 | +0.4 | Gained (less debt) |

> **Key insight for negative balances**: For debts (negative values), a numerically larger value (for example, `-10.4 -> -10`) means less debt and is favorable. A numerically smaller value (for example, `-10.6 -> -11`) means more debt and is unfavorable.

**Formal properties**:
- `|diff[i]|` — absolute displacement caused by rounding (magnitude of change)
- `sign(diff[i])` — direction of participant-level gain/loss: positive = favorable, negative = unfavorable
- Integer-valued quantities are `q_i` and `V_int`

Find adjusted values `adjusted[i]` such that (one-step repair contract):
1. `Σ adjusted[i] = 0` (zero-sum constraint)
2. `adjusted[i] ∈ u * Z` for all *i* (atomic grid output)
3. `Δ[i] := adjusted[i] - rounded[i] ∈ {0, ±u}` for all *i* (one-step repair)
4. exactly `|V_int|` participants receive non-zero `Δ[i]`

Minimal-intervention identity (`Σ |Δ[i]| = |V_amt|`) is treated as an algorithmic guarantee (Theorem 1), not as an independent contract input.

#### Guarantees (Normative)

This document specifies the current Walicord implementation behavior and is not a portability or backward-compatibility contract.

Walicord uses one-step repair with the following normative guarantees:

1. **Zero-sum output**: satisfy `Σ adjusted = 0`.
2. **Atomic-grid output**: satisfy `adjusted[i] ∈ u * Z` for all participants.
3. **Repair shape**: satisfy `Δ[i] = adjusted[i] - rounded[i] ∈ {0, ±u}` with exactly `|V_int|` non-zero repairs.
4. **Deterministic selection**: apply case-based ranking with `preferred_rank` first, then directional `diff`, then deterministic tie-break.

Given (1)-(3), total intervention is structurally fixed to `Σ |Δ[i]| = |V_amt|`.

#### Policy Intent (Informative)

Given the fixed one-step repair shape, the remaining degree of freedom is candidate selection. Under one-step repair, intervention volume is structural (`Σ |Δ| = |V_amt|`) and does not require solving a separate optimizer; selection determines *which participants* absorb those adjustments. `preferred_rank` may override local diff-order by policy to reduce spillover to non-preferred members; no additional optimality is claimed beyond the normative guarantees above.

#### Selection Rule (Normative)

The repair algorithm is defined directly (not by a generic optimizer). This section is the authoritative algorithm source.

- `k = |V_int|`, `a = -sign(V_int) * u`
- if `preferred_set` is omitted, treat `preferred_set = ∅`
- `preferred_rank(i) := 0` if `i ∈ preferred_set`, else `1`
- rank by `(preferred_rank asc, directional_diff_order, stable_key asc, member_id asc)` where:
  - if `V_int > 0`: `diff` descending
  - if `V_int < 0`: `diff` ascending
- apply `+a` to the first `k` ranked participants

Normative pseudocode:

```text
if V_int == 0: return rounded
k = abs(V_int)
a = -sign(V_int) * u
if preferred_set is omitted: preferred_set = ∅
preferred_rank(i) = 0 if i in preferred_set else 1
order by preferred_rank asc, then:
  if V_int > 0: diff desc
  if V_int < 0: diff asc
then stable_key asc, member_id asc
apply +a to first k entries
```

#### Ordering Semantics (Normative)

- `diff` ordering MUST use numeric comparison only (never string form or scale metadata).
- `rounded` and `original` used in `diff` MUST be produced via the same canonical numeric-construction path.
- Tie detection MUST use strict numeric equality (no tolerance band).
- `stable_key` ordering MUST be lexicographic ascending on the profile-defined byte sequence.
- Final tie-break MUST be `member_id` ascending to preserve total order.

#### Rationale (Informative)

**One-step repair fixes intervention volume**: In this problem, the required number of unit adjustments is fixed by the zero-sum constraint (`|V_int|`). Given the one-step repair constraint, total intervention is fixed to `|V_amt|` without solving a separate optimization problem. Selection determines *who* receives those adjustments. We prioritize participants who benefited most from rounding in the correction direction, which reduces reversals of prior rounding outcomes. Alternative norms may induce different allocation characteristics depending on admissible adjustment constraints:
- L2 minimization: may favor more uneven allocation in some formulations
- L∞ minimization: targets worst-case spread but may over-constrain feasible selections

This one-step contract limits individual impact to at most one atomic unit.

**Uniqueness**: The feasible one-step candidate set generally contains multiple outputs. We select a unique output with deterministic ranking (directional `diff`, then `stable_key`, then `member_id`).

Implementation note (equivalent compact form): `score[i] = diff[i] * sign(V_int)`, then rank by `(preferred_rank asc, score desc, stable_key asc, member_id asc)`.

Equivalence note: case-based ranking and score-based ranking are identical.

`V_int = 0` implies `Σ rounded = u * Σ q_i = 0`, so `return rounded` already satisfies exact zero-sum.

#### Reject Conditions (Normative)

- Input validation (single entry point): require `|Σ original| <= ε`; otherwise reject
- Defensive bound: require `k <= n`; otherwise reject (implementation safety guard against corrupted pipelines / extreme integer states)
- Implementation health condition: a `k > n` reject MUST be treated as a pipeline/number-state anomaly (not a normal selection outcome), and diagnostics MUST preserve enough context to distinguish input imbalance from quantization-state inconsistency.

#### Diagnostic Bounds (Informative)

- Exact-model reference bound: `k <= floor(n/2)` (light signal)
- Operational tolerance bound: `k <= floor(n/2) + ceil(ε/u)` (anomaly signal)
- `ceil(ε/u)` uses mathematical ceiling on Decimal arithmetic: compute `ε / u` in Decimal, apply `ceil`, then convert to integer diagnostics domain.
- `epsilon_over_u` diagnostics value is represented in an integer diagnostics domain and treated as non-negative in atomic-unit units (current implementation uses `i128` with a non-negative invariant).

Operational alert policy (recommended):
- `k > floor(n/2)`: info (light reference signal)
- `k > floor(n/2) + ceil(ε/u)`: warn (heavy, likely input/pipeline anomaly)
- `k > n`: error + reject

Reason for `floor(n/2)`: nearest-rounding residuals are in `[-0.5, 0.5]`, so total residual magnitude is at most `n/2`; integer `V_int` then gives `|V_int| <= floor(n/2)` in exact-model reasoning.

Assumptions behind the exact-model reference bound:
- `Σ original = 0` exactly
- nearest quantization semantics are applied exactly to `x_i = original[i] / u`
- per-participant residuals correspond to nearest-rounding residuals

Common reasons this reference bound can be exceeded in practice:
- epsilon-tolerant inputs (`|Σ original| <= ε` but not exactly zero)
- finite-precision representation/evaluation effects in decimal arithmetic
- upstream input pipelines that are not mathematically idealized
- rounding-mode/tie distributions can change boundary-near frequency (for example, HalfUp-heavy midpoint populations); this is primarily a frequency effect, not by itself a correctness failure

Implementation note: in the current API, `q_i` is always computed internally.
Bound intuition: under nearest rounding, each conceptual residual term is in `[-0.5, 0.5]`, so the sum magnitude is at most `n/2`. Since `V_int` is integer, the exact-model bound is `|V_int| <= floor(n/2)`; with implementation tolerance this expands to `floor(n/2) + ceil(ε/u)`.
Boundary assumptions for this bound:
- `x_i := original[i] / u` is evaluated as a numeric value (not string/scale representation)
- quantization is `q_i = quantize_to_int(original[i], u, mode)` with nearest-rounding semantics

This ranking is total over all valid inputs. The rule is: select the directional extreme of `diff` required by repair direction.

Design note: participants are not filtered by diff sign. All participants remain eligible; ranking enforces policy priority and avoids candidate-shortage branches.
Principle: adjustments are taken from the favorable side first. Opposite-sign selections are rare and appear mainly in heavily skewed distributions or large-`k` cases near boundary conditions; these cases should be observable via diagnostics.
Condition sketch: when `V_int > 0`, if fewer than `k` candidates have sufficiently positive `diff`, candidates with `diff <= 0` may still be selected; for `V_int < 0`, the condition is symmetric.

Partial settle-up extension: preferred members are the target set specified by the current settle-up command (for example, `settle_members`). Ranking applies the same diff ordering but places preferred members before non-preferred members. Non-preferred members remain eligible adjustment candidates. If preferred members are insufficient, remaining adjustments are assigned by the global ranking order.
Preferred priority is a policy override for candidate selection; under this algorithm's current constraints (each participant adjusted by at most one unit, total adjusted units fixed by one-step repair), it does not change feasibility or intervention volume, only which solution is chosen.
Preferred-set semantics: preferred members are the candidates that preferentially absorb adjustment impact, to reduce spillover onto non-preferred members. Being preferred is not a reward; it means higher priority for being assigned repair deltas (`±u`).
Policy effect clarification: preferred priority reduces changes on non-preferred members; ordering *within* the preferred block still follows directional `diff` (then tie-break).
Partial settle-up clarification: even in partial commands, `original` is defined on the full group balance vector. Quantization performs global projection to the atomic grid plus global zero-sum repair; subset-level zeroing is handled by transfer construction, not by quantization itself.

Reference pseudocode:

```text
V_int = sum(q_i)
require abs(sum(original)) <= epsilon
if V_int == 0: return rounded
k = abs(V_int)
if k > floor(n/2): warn("nearest-rounding theoretical bound violated")
if k > floor(n/2) + ceil(epsilon/u): warn("operational tolerance bound exceeded")
require k <= n                              # defensive guard

a = -sign(V_int) * u
for each i:
  diff[i] = rounded[i] - original[i]

order by preferred_rank asc, then:
  if V_int > 0: diff desc
  if V_int < 0: diff asc
then stable_key asc, member_id asc
where preferred_rank := 0 if preferred else 1  # smaller is better
apply +a to first k entries
```

Equivalent implementation form: compute `score = diff * sign(V_int)` and order by `score desc` under the same tie-break keys.

Note: participants are not filtered by diff sign. All participants remain eligible; ranking enforces policy priority.

**Intuition on diff sign**:
- `diff > 0` → Participant **gained** from rounding
- `diff < 0` → Participant **lost** from rounding

Principle: cancel rounding advantage in the direction required by global zero-sum repair.

#### Example

```
Original:              A: +666.666..., B: -333.333..., C: -333.333...
Rounded:               A: +667 (diff: +0.333...), B: -333 (diff: +0.333...), C: -333 (diff: +0.333...)
Zero-sum violation:    +1

Adjustment:            All diffs tie; stable_key tie-break selects A, then decrement A by 1
Result:                A: +666, B: -333, C: -333
Sum:                   0 ✓
```

Note: after adjustment, individual error relative to original can exceed `0.5 * u` in edge cases. This is an unavoidable trade-off when enforcing exact zero-sum with atomic-unit constraints.

#### Example (`V_int < 0`, debt-heavy side, HalfUp)

```text
Original:              A: -0.6, B: -0.6, C: +1.2
Rounded:               A: -1 (diff: -0.4), B: -1 (diff: -0.4), C: +1 (diff: -0.2)
Zero-sum violation:    -1   (V_int < 0)

Adjustment rule:       add +u to the smallest diff first
Adjustment:            stable_key tie-break selects one of {A, B}, then +1
Result (one outcome):  A: 0, B: -1, C: +1
Sum:                   0 ✓
```

Score intuition: `score = diff * sign(V_int)`. With `V_int < 0`, `sign(V_int) = -1`, so smaller `diff` yields larger `score` and is prioritized for `+u` repair.
Repair-direction note: `V_int < 0` means the post-quantization sum is too negative, so repair applies `+u` to selected participants to raise the total toward zero (`+u` reduces debt for negative balances and increases credit for positive balances).

#### Example (preferred overrides score order)

```text
u = 1, V_int = +1, so repair applies -1
P (preferred):     diff = +0.1  => score = +0.1
X (non-preferred): diff = +0.4  => score = +0.4
Ranking key: (preferred_rank, score, stable_key, member_id)
Result: P is selected before X by policy override
```

This behavior is intentional: preferred-set policy can override score-local ordering to reduce spillover to non-preferred members.

#### Deterministic Tie-Breaking

When multiple participants have the same score (equivalently the same `diff` for fixed `sign(V_int)`), we use `stable_key` as a secondary sort key. This ensures the same inputs always produce the same output, preventing non-deterministic behavior. `score` comparisons use exact decimal ordering; near-equal but non-equal values are ordered by numeric value before tie-break.
For one quantization/repair execution, `sign(V_int)` is global and constant across all participants during sorting.
Tie condition is strict numeric equality of `score` (equivalently `diff` for fixed `sign(V_int)`); no tolerance band is used for tie detection.
If `stable_key` is also equal, implementations MUST apply a final deterministic tie-break (`member_id` ascending in current implementation) to preserve total ordering.
Normative requirement: `stable_key` MUST be derived from a versioned profile and provide deterministic ordering; the active profile identifier and bound field set MUST be logged.

**Formal definition**:
```
stable_key(member) := (hash_key(member), member_tie_break(member))
```
where `hash_key(member) := hash(canonical_member_identifier, settlement_context)` and `member_tie_break(member)` is the deterministic final key (`member_id` ascending in current implementation).
Current uniqueness assumption: `member_id` is unique within a settlement scope; if this assumption is violated, the input is invalid.

Implementation profile snapshot (Informative, current): `stable_key_v1_sha256_member_context_be`
- input fields are `{member_id, scale, rounding_mode, fairness_policy}`
- `settlement_context` contribution is `{scale, rounding_mode, fairness_policy}`
- framing is fixed-width binary fields in that order, then SHA-256
- profile changes are versioned as new stable-key profile identifiers

Current profile details (Informative):
- hash algorithm is SHA-256
- input byte layout is fixed (field order and byte widths)
- input fields are fixed to `{member_id, scale, rounding_mode, fairness_policy}`
- `hash_key` output type is a 32-byte array
- comparison is lexicographic ascending on `(hash_key, member_tie_break)`

Current implementation uses a deterministic 32-byte `hash_key` derived from `(member_id, settlement_context)` and compares `(hash_key, member_id)` lexicographically. This keeps ordering deterministic and avoids direct raw-ID ordering bias at primary tie-break level.
Operational note: SHA-256 collision risk is treated as practically negligible; if a collision occurs, fallback comparison by `member_id` still guarantees total order.

`stable_key` is computed at runtime, not persisted, and only required to be deterministic within this implementation.

Audit logging fields (current implementation):
- `stable_key_profile/version`
- `stable_key_profile_fields` (fixed field-set descriptor for the active profile)
- `u` and `scale`
- `rounding_mode`
- `fairness_policy`
- `epsilon`
- required bound-context diagnostics (`member_count`, `sum_original`, `sum_rounded`, `epsilon_over_u`)
- selection/bound diagnostics (`V_int`, `selected_ids`, `selected_diffs`, `preferred_hit_count`, `kth_score`, `next_score`, `exceeds_theoretical`, `exceeds_operational`)

Reject-path logging contract (normative):
- on `k > n` reject, implementation MUST log at least `{sum_original, sum_rounded, member_count, u, epsilon, rounding_mode}` in the reject event.

Operational recommendation: include a machine-readable `reject_reason` key (for example, `input_imbalance`, `quantize_failure`, `crosscheck_mismatch`, `k_gt_n`) to accelerate incident triage.

Operational cutover note: when `k` is large, operators should reconstruct alert roots with `(member_count, sum_original, sum_rounded, epsilon_over_u)` first, then inspect ranking diagnostics.

Audit note: if raw `member_id` ordering is used, long-term issuance-order bias can appear at tie boundaries.

## Mathematical Guarantees

### Theorem 1: Bounded Adjustment

Assuming nearest-rounding quantization (HalfUp/HalfEven) and the one-step repair contract (`Δ ∈ {0, ±u}` with exactly `|V_int|` non-zero entries), for *n* participants:

```
|adjusted[i] - rounded[i]| ≤ u for all i
Σ |adjusted[i] - rounded[i]| = |V_amt|
```

Lemma (minimal-intervention lower bound): for any feasible repair `Δ[i] = adjusted[i] - rounded[i]` with `Σ (rounded[i] + Δ[i]) = 0`, triangle inequality gives `Σ |Δ[i]| >= |V_amt|` because `Σ Δ[i] = -V_amt`.

Algorithmic identity: one-step repair applies exactly `|V_int|` unit changes, so `Σ |Δ[i]| = |V_int| * u = |V_amt|`, attaining the lower bound. Under the one-step repair constraint, total intervention is fixed to `|V_amt|`; candidate ranking only decides which participants receive the intervention.

Model assumption: exact arithmetic has `Σ original_balances = 0`.
Implementation guard: accept inputs when `|Σ original_balances| <= ε`; otherwise reject.

Supplementary nearest-rounding diagnostic bounds:

These bounds are not required for correctness; they are used for diagnostics and sanity checks.

- Exact-model reference bound (`Σ original = 0`): `|V_amt| ≤ floor(n/2) * u`.
- Implementation-model bound (`|Σ original| <= ε`): `|V_amt| ≤ n * 0.5 * u + ε`.

The first is a compact exact-model reference (especially informative for symmetric tie behavior such as HalfEven); the second is the operational bound under finite-precision tolerance.

#### Important Note: Adjustment Error vs Rounding Error

While `|adjusted[i] - rounded[i]| ≤ u`, the **total error from original** can exceed the typical rounding error bound:

```
Example: "Unlucky Winner"
- Original: 0.4
- Rounded: 0 (diff = -0.4, rounded down)
- Selected for adjustment: +1
- Adjusted: 1
- Final error: |1 - 0.4| = 0.6 > 0.5
```

This can occur when global zero-sum repair requires assigning a unit adjustment to a participant whose local rounding error had opposite sign. The final error can exceed `0.5 * u`, and this is the unavoidable cost of restoring exact zero-sum under atomic-unit constraints. Formal trigger: if the selected top-`k` set in repair direction contains any participant with `diff[i] * sign(V_int) < 0` (opposite-sign candidate), `0.5 * u` exceedance is possible for selected participants. Even with diff-priority ranking, this can occur when `k` is large or `diff` distribution is heavily skewed.

### Theorem 2: Zero-Sum Preservation

After adjustment:

```
Σ adjusted[i] = 0
```

**Proof**: Since each `rounded[i] = q_i * u`, we have `V_amt = Σ rounded[i] = u * Σ q_i`. Define `V_int := Σ q_i ∈ ℤ`; equivalently, `V_amt = V_int * u`. We adjust `|V_int|` participants by `-sign(V_int) * u`. The new sum is:

```
Σ adjusted[i] = Σ rounded[i] + |V_int| × (-sign(V_int) * u)
              = V_amt - |V_int| * u * sign(V_int)
              = V_amt - V_amt
              = 0
```

Note: this proof operates in the rounded/adjusted domain; `|Σ original| <= ε` affects acceptance, not the exact zero-sum property of `adjusted`.

### Theorem 3A: Rounded Error Bound (Before Adjustment)

With deferred rounding, rounding error originates in the **settlement quantization step** (not from individual transactions). Let `r(x)` be the rounding function and `δ = sup |r(x) - x|` be the maximum rounding error per participant. For standard rounding modes:
r maps amount to amount on the atomic grid by quantizing `x / u` to a nearest integer and mapping back by `* u`.

- **Half Up**: `δ = 0.5 × u` (maximum displacement at midpoint inputs `x = (k + 0.5) * u`)
- **Half Even**: `δ = 0.5 × u` (same bound, though error distribution differs)

For the rounded vector (before zero-sum adjustment), the violation is bounded by:

```
|V_amt| ≤ n × δ
```

where *n* is the number of participants.

Assuming `Σ original = 0`, equivalently `V_amt = Σ (rounded - original)`, so the bound follows from summing per-participant rounding deviations bounded by `δ`.

**Proof**: Each participant's balance rounds to the nearest atomic unit, contributing at most `δ` error. Summing over *n* participants gives `n × δ`.

This theorem applies to `rounded`, not `adjusted`.

### Theorem 3B: Adjusted Error Bound (This Algorithm, nearest-rounding modes)

Preconditions:
- nearest-rounding quantization (HalfUp or HalfEven), so `|rounded[i] - original[i]| <= 0.5 * u`
- one-step adjustment rule, so each participant has `Δ[i] = adjusted[i] - rounded[i] ∈ {0, ±u}`
- repair is applied to the `rounded` vector only; `original` is never directly modified
- this repair specification is limited to one-step repair (`Δ[i] ∈ {0, ±u}`); multi-step repair is out of scope

Therefore:

`|adjusted[i] - original[i]| <= 1.5 * u`.

Reachable boundary example (`1.5 * u`): under HalfUp midpoint rounding, `original = +0.5 * u` gives `rounded = +1.0 * u` (`|diff| = 0.5 * u`). If zero-sum repair also applies `+u` to that participant, `adjusted = +2.0 * u`, so `|adjusted - original| = 1.5 * u`.
This selection can occur when repair-direction candidate pressure is high (for example, large `k` or skewed score distribution), including cases where an "unlucky" participant is selected by score/tie-break order.
HalfEven has different midpoint destinations, but the same `1.5 * u` upper bound remains reachable under one-step repair.

Scope note: this bound relies on the current one-step-per-participant adjustment rule. If future variants allow multi-step or weighted per-participant adjustments, this bound must be re-derived.
Interpretation note: `1.5 * u` is a worst-case bound. In many small-`|V_int|` cases, realized errors are closer to `0.5 * u`, but no tighter universal guarantee is claimed.

**Contrast with immediate rounding**:
- Theoretical model (exact arithmetic): final output error bound is O(1), independent of transaction count.
- Implementation model (finite precision): pre-settlement drift is absorbed by `ε`, and `ε` may be configured from operation-count budget `N`.

### Corollary: Original-Sum Epsilon Tolerance (`|Σ original| <= ε`)

We accept balances where:

```
|Σ balances| ≤ ε
```

This check is applied to the **original high-precision balances before rounding**.

**Rationale for ε**:
- `ε` is always defined in amount units (same unit as `original`, `rounded`, and `V_amt`)
- `ε = u / 10^6` (for JPY: `ε = 10^-6`)
- Error source: finite-precision arithmetic drift in aggregate updates before settlement quantization
- Bound model: per-operation drift upper bound × operation count (conservative), then apply safety margin
- Operational assumption: up to 10^6 aggregate updates per settlement period (well above typical usage)
- Aggregation depth is bounded by participant count (typically < 100), while operation count tracks ledger events

This accommodates microscopic drift from finite-precision decimal operations while detecting actual data corruption.

Note: `ε` is an implementation parameter, not a theorem constant. The mathematical model assumes exact arithmetic; implementation chooses `ε` to safely bound finite-precision drift.

Operational note: when rounding mode, currency scale, maximum event count, or balance magnitude envelope changes, `ε` should be re-evaluated. A practical policy can use `ε = max(u / 10^6, ε_min)` to avoid over-strict rejects in very fine scales, where `ε_min` is derived from implementation error budget (worst-case decimal ULP × maximum operation count × safety factor; for example, decimal minimum step × operation count × safety factor).
Here, operation count means the maximum number of balance updates in one settlement period.
Template estimate (rust_decimal): `ε_min = safety_factor * N * 1e-28` (amount units), where `N` is the period operation-count budget.
Here `1e-28` is a conservative per-operation ULP-scale unit based on rust_decimal's 28 decimal places of precision.
Example calibration: with `N = 10^6` and `safety_factor = 100`, `ε_min = 10^-20`.
In that setting, JPY (`u=1`) gives baseline `u/10^6 = 10^-6`, and USD (`u=10^-2`) gives baseline `10^-8`; both dominate `ε_min`.

Current operational defaults:
- Baseline tolerance: `u / 10^6`
- Effective tolerance: `max(u / 10^6, ε_min)`

Current formula (fixed in implementation guidance):
- `ε = max(u / 10^6, safety_factor * N * 1e-28)`
- current parameters: `N = 10^6`, `safety_factor = 100`

Re-evaluation triggers:
- currency scale changes
- rounding mode changes
- maximum balance-update count per settlement period changes
- balance magnitude envelope changes

### Edge Case: Single Participant (`n=1`)

For a single participant, global zero-sum requires `|original[0]| <= ε` at input validation.
Under this condition, quantization yields `0` after repair; otherwise input is rejected by the imbalance guard.

## Algorithm Complexity

### Time Complexity: O(n log n)

- Rounding: O(n)
- Sorting for adjustment selection: O(n log n)
- Adjustment application: O(n)

### Space Complexity: O(n)

- Storage for balances, diffs, and candidate lists: O(n)

## Rounding Modes

Walicord supports two rounding strategies:

### Half Up (implementation default in the current JPY profile)

These examples apply to integer quantization of `x = original / u`.

Rounds halves away from zero:
- `0.5 → 1`
- `-0.5 → -1`

As of the current implementation snapshot, this is the default behavior in `SettlementContext::jpy_default()`.

### Half Even (Banker's Rounding)

These examples apply to integer quantization of `x = original / u`.

Rounds halves to the nearest even number:
- `0.5 → 0`
- `1.5 → 2`
- `-0.5 → 0`
- `-1.5 → -2`

**Why Half Up is preferred over Half Even**:

1. **System behavior is dominated by repair policy, not midpoint rule alone**: Half Even minimizes expected-value bias for standalone repeated rounding. In this system, final outputs are produced by the composed procedure (quantization + exact zero-sum repair + deterministic ranking). The sum is fixed by repair, and practical differences are mainly driven by candidate selection policy.

2. **Predictability and user intuition dominate**: Half Even introduces counter-intuitive midpoint behavior (`0.5 → 0` while `0.51 → 1`) for many users. Half Up is easier to explain in product behavior and support operations.

3. **Zero-sum repair constrains aggregate effects**: The repair step enforces exact conservation (`Σ adjusted = 0`) and fixes total intervention under the one-step contract, so remaining differences between HalfUp and HalfEven are mostly participant-allocation details under the ranking policy.

**Conclusion**: Because settlement outputs are determined by quantization + exact zero-sum repair + deterministic selection, practical midpoint-rule differences are mainly in candidate ordering near ties; UX consistency is prioritized in the current default profile (Half Up).

## Comparison with Alternative Approaches

### Sequential Remainder Distribution

**Approach**: First *k* participants get `base + 1`, others get `base`.

**Problem**: Earlier participants systematically receive larger amounts, creating order-dependent bias.

**Example**: Split 100 by 3:
- Sequential: [34, 33, 33] — participant 1 always pays more
- Deferred: [33.333..., 33.333..., 33.333...] → rounded at settlement

### Immediate Rounding per Transaction

**Approach**: Round after each split/operation.

**Problem**: Rounding errors accumulate across many transactions.

**Example**: 10,000 splits of 100 by 3:
- Immediate rounding: error accumulates linearly
- Deferred rounding: error bounded by ε

## Practical Considerations

### Scale flexibility

The algorithm works for any currency scale:

| Currency | scale | Atomic Unit |
|----------|-------|-------------|
| JPY      | 0     | 1 yen       |
| USD      | 2     | 0.01 dollar |
| BTC      | 8     | 0.00000001 BTC |

### Overflow Prevention

Aggregate sums use checked arithmetic to prevent overflow:

```rust
// Pseudocode
fn add_balance(acc: &mut Balance, value: Money) -> Result<(), OverflowError> {
    acc.sum = acc.sum.checked_add(value)?;
    acc.count = acc.count.checked_add(1)?;
    Ok(())
}
```

### Audit Trail

If `|Σ balances| > ε` (indicating data corruption), the system:
1. Rejects the settlement
2. Logs the discrepancy
3. Requires manual investigation

This maintains ledger integrity and provides clear accountability.

### Recommended Test Vectors

- `|Σ original|` near `ε` on both positive and negative sides
- `V_amt = ± floor(n/2) * u` neighborhood cases
- all participants with equal `diff` (tie-break reproducibility)
- negative midpoint cases (for example, `-x.5`) for HalfUp and HalfEven
- `diff` values that are very close but not equal (ordering stability)
- skewed sign populations (mostly debtors or mostly creditors)
- diagnostic transition cases (`k` crossing theoretical/operational bounds)
- very large-magnitude originals near midpoint boundaries after scaling (`original / u`)
- division-representation edge cases near decimal precision limits (for example `scale=8` with large magnitudes)
- numerically equal values with different decimal representations (for example `1.0` vs `1.00`) to confirm identical diff ordering
- preferred set variants: empty / all / partial with both `V_int > 0` and `V_int < 0`
- preferred tie case: within preferred set, equal score resolved by `stable_key`
- `n=2` boundary cases with `V_int = ±1` (minimal one-step repair)
- deterministic-key collision fallback behavior (injectable key provider in tests)
- minimal participant counts (`n=1`, `n=2`)
- sign-boundary cases where `V_int` crosses `0` and `±1`
- preferred-priority cases where preferred members are locally less favorable by `diff`

## Theoretical Model vs Implementation Model

This document distinguishes between two layers:

### Theoretical Model (Platform Independent)
- Assumes infinite-precision real numbers
- Defines mathematical invariants (zero-sum, one-step intervention identity)
- Specifies algorithm behavior independent of implementation details
- Suitable for formal verification and audit specifications

### Implementation Model (rust_decimal)
- 28-digit decimal precision (96-bit mantissa)
- Finite but bounded error accumulation
- `ε = u / 10^6` tolerance threshold
- Concrete tie-breaking using a deterministic 32-byte `stable_key` and lexicographic byte-order comparison

**Portability**: The theoretical model can be implemented with alternative decimal libraries (e.g., `bigdecimal`, fixed-point integers) by adjusting precision parameters while preserving algorithmic correctness.

## References

- [rust_decimal](https://docs.rs/rust_decimal/) — 28-digit decimal arithmetic
- [rust_decimal RoundingStrategy](https://docs.rs/rust_decimal/latest/rust_decimal/enum.RoundingStrategy.html) — midpoint rounding behavior used by quantization
- "What Every Computer Scientist Should Know About Floating-Point Arithmetic" — Goldberg, 1991
- IEEE 754-2008 Standard for Floating-Point Arithmetic (background reference for rounding terminology)
