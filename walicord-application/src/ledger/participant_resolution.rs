use super::expense_session::{
    ExpenseConfirmationSnapshot, ExpenseParticipantSelection, ExpenseSelectionState,
};
use std::collections::{BTreeMap, BTreeSet};
use walicord_domain::model::{MemberId, RoleId, Weight};

/// Roster snapshot used at confirmation rebuild and append time. The adapter populates
/// this from Discord's roster cache (criterion 4 / 81). Roles map to current member
/// sets; `all_members` is the resolved set of every selectable member (the `MEMBERS`
/// virtual group from criterion 214).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RosterSnapshot {
    pub all_members: BTreeSet<MemberId>,
    pub role_members: BTreeMap<RoleId, BTreeSet<MemberId>>,
}

impl RosterSnapshot {
    pub fn members_of(&self, role: RoleId) -> impl Iterator<Item = MemberId> + '_ {
        self.role_members
            .get(&role)
            .into_iter()
            .flat_map(|members| members.iter().copied())
    }
}

/// Richer outcome from resolving a selection state against a roster snapshot.
/// `defaulted_members` lists every resolved member whose final weight came from the
/// `Weight(1)` default (no override applied), driving the confirmation `既定値 1` cue
/// (criterion 216). `dropped_overrides` records every weight override whose member is
/// no longer present in the resolution; the caller is expected to log them (criterion
/// 158) and remove them from the session's selection state so a later shrink-and-readd
/// does not silently resurrect a stale weight.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolutionOutcome {
    pub resolved: Vec<ExpenseParticipantSelection>,
    pub defaulted_members: Vec<MemberId>,
    pub dropped_overrides: Vec<(MemberId, Weight)>,
}

/// Resolve the actor's current selection state against the live roster (criterion 81 /
/// criterion 4). Newly-added participants default to `Weight(1)` (criterion 157);
/// overrides referencing members no longer present are dropped and reported back as
/// `dropped_overrides` so the caller can update session state and emit the criterion
/// 158 observability event.
///
/// The output is sorted by `MemberId` for canonical encoding and deterministic display.
pub fn resolve_selection_against_roster(
    selection_state: &ExpenseSelectionState,
    roster: &RosterSnapshot,
) -> ResolutionOutcome {
    let mut resolved: BTreeMap<MemberId, Weight> = BTreeMap::new();

    for member_id in &selection_state.individual_members {
        if roster.all_members.contains(member_id) {
            resolved.insert(*member_id, Weight(1));
        }
    }

    for role_id in &selection_state.selected_roles {
        for member_id in roster.members_of(*role_id) {
            resolved.insert(member_id, Weight(1));
        }
    }

    if selection_state.include_members_group {
        for member_id in &roster.all_members {
            resolved.insert(*member_id, Weight(1));
        }
    }

    let mut dropped_overrides: Vec<(MemberId, Weight)> = Vec::new();
    let mut overridden: BTreeSet<MemberId> = BTreeSet::new();
    for (member_id, weight) in &selection_state.weight_overrides {
        if resolved.contains_key(member_id) {
            resolved.insert(*member_id, *weight);
            overridden.insert(*member_id);
        } else {
            dropped_overrides.push((*member_id, *weight));
        }
    }

    let resolved_rows: Vec<ExpenseParticipantSelection> = resolved
        .iter()
        .map(|(member_id, weight)| ExpenseParticipantSelection {
            member_id: *member_id,
            weight: *weight,
        })
        .collect();

    let defaulted_members: Vec<MemberId> = resolved
        .keys()
        .copied()
        .filter(|member_id| !overridden.contains(member_id))
        .collect();

    ResolutionOutcome {
        resolved: resolved_rows,
        defaulted_members,
        dropped_overrides,
    }
}

/// One entry of detected drift between a confirmation snapshot and the current resolved
/// participants (criterion 111). Drift triggers a confirmation rebuild that explicitly
/// surfaces the added / removed lines and any defaulted weights to the actor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParticipantDrift {
    Added { member_id: MemberId, weight: Weight },
    Removed { member_id: MemberId, weight: Weight },
}

/// Diff a confirmation snapshot against the currently-resolved participant set. Empty
/// `drift` means no participant change between the two; the caller may still need to
/// surface weight changes separately by comparing weights on overlapping rows.
pub fn drift_between_snapshot_and_resolution(
    snapshot: &ExpenseConfirmationSnapshot,
    resolved: &[ExpenseParticipantSelection],
) -> Vec<ParticipantDrift> {
    let snapshot_index: BTreeMap<MemberId, Weight> = snapshot
        .participants
        .iter()
        .map(|row| (row.member_id, row.weight))
        .collect();
    let resolved_index: BTreeMap<MemberId, Weight> = resolved
        .iter()
        .map(|row| (row.member_id, row.weight))
        .collect();

    let mut drift = Vec::new();
    for (member_id, weight) in &resolved_index {
        if !snapshot_index.contains_key(member_id) {
            drift.push(ParticipantDrift::Added {
                member_id: *member_id,
                weight: *weight,
            });
        }
    }
    for (member_id, weight) in &snapshot_index {
        if !resolved_index.contains_key(member_id) {
            drift.push(ParticipantDrift::Removed {
                member_id: *member_id,
                weight: *weight,
            });
        }
    }
    drift
}

#[cfg(test)]
mod tests {
    use super::*;

    fn roster_with_role_a(members_in_a: &[u64], all_members: &[u64]) -> RosterSnapshot {
        let mut role_members = BTreeMap::new();
        role_members.insert(
            RoleId(1),
            members_in_a.iter().map(|id| MemberId(*id)).collect(),
        );
        RosterSnapshot {
            all_members: all_members.iter().map(|id| MemberId(*id)).collect(),
            role_members,
        }
    }

    #[test]
    fn individual_members_are_resolved_to_weight_one_by_default() {
        let selection = ExpenseSelectionState {
            individual_members: vec![MemberId(1), MemberId(2)],
            ..Default::default()
        };
        let roster = roster_with_role_a(&[], &[1, 2]);

        let outcome = resolve_selection_against_roster(&selection, &roster);

        assert_eq!(
            outcome.resolved,
            vec![
                ExpenseParticipantSelection {
                    member_id: MemberId(1),
                    weight: Weight(1)
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(1)
                },
            ]
        );
        assert_eq!(outcome.defaulted_members, vec![MemberId(1), MemberId(2)]);
        assert!(outcome.dropped_overrides.is_empty());
    }

    #[test]
    fn individual_member_no_longer_in_roster_is_dropped() {
        let selection = ExpenseSelectionState {
            individual_members: vec![MemberId(1), MemberId(999)],
            ..Default::default()
        };
        let roster = roster_with_role_a(&[], &[1, 2]);

        let outcome = resolve_selection_against_roster(&selection, &roster);

        assert_eq!(outcome.resolved.len(), 1);
        assert_eq!(outcome.resolved[0].member_id, MemberId(1));
    }

    #[test]
    fn roles_resolve_to_current_role_members_with_default_weight() {
        let selection = ExpenseSelectionState {
            selected_roles: vec![RoleId(1)],
            ..Default::default()
        };
        let roster = roster_with_role_a(&[1, 2, 3], &[1, 2, 3, 4]);

        let outcome = resolve_selection_against_roster(&selection, &roster);

        assert_eq!(
            outcome.resolved,
            vec![
                ExpenseParticipantSelection {
                    member_id: MemberId(1),
                    weight: Weight(1)
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(1)
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(3),
                    weight: Weight(1)
                },
            ]
        );
    }

    #[test]
    fn members_group_expands_to_all_members() {
        let selection = ExpenseSelectionState {
            include_members_group: true,
            ..Default::default()
        };
        let roster = roster_with_role_a(&[], &[10, 20, 30]);

        let outcome = resolve_selection_against_roster(&selection, &roster);

        assert_eq!(outcome.resolved.len(), 3);
        assert_eq!(outcome.resolved[0].member_id, MemberId(10));
        assert_eq!(outcome.resolved[2].member_id, MemberId(30));
    }

    #[test]
    fn duplicate_member_through_individual_role_and_members_group_collapses_to_one_row() {
        let selection = ExpenseSelectionState {
            individual_members: vec![MemberId(1)],
            selected_roles: vec![RoleId(1)],
            include_members_group: true,
            ..Default::default()
        };
        let roster = roster_with_role_a(&[1], &[1]);

        let outcome = resolve_selection_against_roster(&selection, &roster);

        assert_eq!(outcome.resolved.len(), 1);
        assert_eq!(outcome.resolved[0].member_id, MemberId(1));
        assert_eq!(outcome.resolved[0].weight, Weight(1));
    }

    #[test]
    fn weight_overrides_apply_only_to_members_that_remain_in_resolution() {
        let mut overrides = BTreeMap::new();
        overrides.insert(MemberId(1), Weight(3));
        overrides.insert(MemberId(999), Weight(5));
        let selection = ExpenseSelectionState {
            individual_members: vec![MemberId(1), MemberId(2)],
            weight_overrides: overrides,
            ..Default::default()
        };
        let roster = roster_with_role_a(&[], &[1, 2]);

        let outcome = resolve_selection_against_roster(&selection, &roster);

        assert_eq!(
            outcome.resolved,
            vec![
                ExpenseParticipantSelection {
                    member_id: MemberId(1),
                    weight: Weight(3)
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(1)
                },
            ]
        );
        assert_eq!(outcome.defaulted_members, vec![MemberId(2)]);
        assert_eq!(outcome.dropped_overrides, vec![(MemberId(999), Weight(5))]);
    }

    #[test]
    fn role_membership_change_appears_as_added_and_removed_drift_versus_snapshot() {
        let snapshot = ExpenseConfirmationSnapshot {
            participants: vec![
                ExpenseParticipantSelection {
                    member_id: MemberId(1),
                    weight: Weight(1),
                },
                ExpenseParticipantSelection {
                    member_id: MemberId(2),
                    weight: Weight(1),
                },
            ],
        };
        let resolved = vec![
            ExpenseParticipantSelection {
                member_id: MemberId(1),
                weight: Weight(1),
            },
            ExpenseParticipantSelection {
                member_id: MemberId(3),
                weight: Weight(1),
            },
        ];

        let actual = drift_between_snapshot_and_resolution(&snapshot, &resolved);

        assert_eq!(
            actual,
            vec![
                ParticipantDrift::Added {
                    member_id: MemberId(3),
                    weight: Weight(1)
                },
                ParticipantDrift::Removed {
                    member_id: MemberId(2),
                    weight: Weight(1)
                },
            ]
        );
    }

    #[test]
    fn no_drift_is_reported_when_resolution_matches_snapshot() {
        let snapshot = ExpenseConfirmationSnapshot {
            participants: vec![ExpenseParticipantSelection {
                member_id: MemberId(1),
                weight: Weight(1),
            }],
        };
        let resolved = vec![ExpenseParticipantSelection {
            member_id: MemberId(1),
            weight: Weight(1),
        }];

        let actual = drift_between_snapshot_and_resolution(&snapshot, &resolved);

        assert!(actual.is_empty());
    }

    #[test]
    fn override_for_absent_member_is_reported_as_dropped_so_caller_can_purge_session_state() {
        let mut overrides = BTreeMap::new();
        overrides.insert(MemberId(7), Weight(5));
        let selection = ExpenseSelectionState {
            individual_members: vec![MemberId(1)],
            weight_overrides: overrides,
            ..Default::default()
        };
        let roster = roster_with_role_a(&[], &[1]);

        let outcome = resolve_selection_against_roster(&selection, &roster);

        assert_eq!(outcome.dropped_overrides, vec![(MemberId(7), Weight(5))]);
        assert!(
            outcome
                .resolved
                .iter()
                .all(|row| row.member_id != MemberId(7))
        );
    }
}
