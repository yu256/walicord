# Transaction Construction

This document describes how Walicord constructs the set of transfers (transactions) to resolve optimized balances derived from the settlement process.

## Overview

After "Settlement Mathematics" logic determines the *final net balances* for all participants (ensuring zero-sum and atomic units), the **Transaction Construction** algorithm determines *who pays whom* to reach those states from the current states.

Specifically, in Walicord's current architecture (`SettlementCalculator`), the logic simplifies the debt graph by matching creditors and debtors greedily, with a preference for settling members within the requested subset.

## Algorithm Description

The algorithm takes:
1. **Current Balances**: The starting state of all members.
2. **Target Settlement Members**: A subset of members who explicitly requested to settle up (e.g., via `!settleup @A @B`).

It produces:
- A list of **Transfers** (Person A pays Person B amount X).
- A **New Balance** map (updated balances after transfers are applied).

### The Process

The core logic (`SettlementCalculator::calculate`) works as follows:

1. **Identify Settle Candidates**: Iterate through the provided `settle_members`.
2. **Determine Direction**: For each member, check if they have a non-zero balance.
   - If `balance > 0`: They are a **payer** (have a surplus/debt to the pool).
   - If `balance < 0`: They are a **receiver** (have a deficit/credit from the pool).
   - Target direction is the opposite of their sign (e.g., a payer needs to transfer money out to reduce their positive balance to zero).

3. **Priority Matching (Within Settle Group)**:
   - The algorithm first attempts to match the current member's balance against other members *within the same `settle_members` list*.
   - It looks for members with the opposite sign (e.g., if A is +100, look for B with -50).
   - **Greedy Transfer**: It transfers the minimum of `|current_balance|` and `|target_balance|` to satisfy as much as possible.
   - This "clears" debts between the people who explicitly asked to settle.

4. **Global Matching (Outside Settle Group)**:
   - If the current member *still* has a remaining balance after step 3 (and they are in the `settle_members` list), it implies their net position cannot be cleared solely by other settling members.
   - The algorithm then searches the **entire participant list** (all members with balances).
   - It repeats the greedy matching process against any available participant with an opposite sign.

5. **Result**:
   - The generated transfers ensure that the members in `settle_members` reach a zero balance (or as close as possible if the system wasn't zero-sum to begin with, though the Settlement Mathematics layer guarantees zero-sum inputs).
   - Non-settling members may see their balances shift (e.g., their credit is transferred from A to B), but the global net zero is preserved.

## Example

**Scenario**:
- A: +100 (Payer/Debtor)
- B: +50 (Payer/Debtor)
- C: -150 (Receiver/Creditor)

**Command**: `!settleup @A`

**Execution**:
1. Focus on **A**. Balance +100. Needs to pay 100 to clear.
2. **Priority Match**: Look for other names in command. None.
3. **Global Match**: Look at all members.
   - B is +50 (Same sign, skip).
   - C is -150 (Opposite sign, valid).
4. **Transfer**:
   - Amount = min(|100|, |-150|) = 100.
   - Construct Transfer: **A pays C 100**.
   - Update A: +100 -> 0.
   - Update C: -150 -> -50.
5. **Result**: 
   - A is settled (0).
   - B (+50) and C (-50) remain.

## Why this approach?

- **Local Optimality**: By prioritizing the `settle_members` group, we avoid involving unrelated third parties if the debt can be resolved internally.
- **Completeness**: Falling back to the global list ensures that a member *can* settle up even if their specific debtor isn't in the command (e.g., "I want to leave the group, clear my balance").
- **Determinism**: The iteration order dictates the graph structure. While not explicitly minimizing the *number* of transactions (which would require a min-cost max-flow solver or similar), the greedy approach is computationally cheap (O(N^2)) and practically sufficient for small groups.
