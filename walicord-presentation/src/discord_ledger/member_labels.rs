use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    sync::OnceLock,
};

use super::sanitizer::SafeLiteralText;
use icu::{
    collator::{Collator, CollatorBorrowed, options::CollatorOptions},
    locale::locale,
};
use walicord_application::ledger::LedgerEntry;
use walicord_domain::model::{MemberId, RoleId};
use walicord_i18n as i18n;

pub fn unknown_member_label() -> SafeLiteralText {
    SafeLiteralText::from_generated_roster_label(i18n::UNKNOWN_DISPLAY_LABEL)
}

fn unknown_user_safe_label(member_id: MemberId) -> SafeLiteralText {
    let label = i18n::unknown_user_label(member_id.0).to_string();
    SafeLiteralText::from_generated_roster_label(label)
}

pub fn unknown_role_safe_label(role_id: RoleId) -> SafeLiteralText {
    let label = i18n::unknown_role_label(role_id.0).to_string();
    SafeLiteralText::from_generated_roster_label(label)
}

fn disambiguated_safe_label(
    base_visible: &SafeLiteralText,
    member_id: MemberId,
) -> SafeLiteralText {
    let label = i18n::disambiguated_visible_label(base_visible.as_str(), member_id.0).to_string();
    SafeLiteralText::from_generated_roster_label(label)
}

fn japanese_collator() -> &'static CollatorBorrowed<'static> {
    static COLLATOR: OnceLock<CollatorBorrowed<'static>> = OnceLock::new();
    COLLATOR.get_or_init(|| {
        Collator::try_new(locale!("ja").into(), CollatorOptions::default())
            .expect("compiled ICU data should include the Japanese collator")
    })
}

fn case_folded(text: &str) -> String {
    text.chars().flat_map(char::to_lowercase).collect()
}

fn compare_display_text(lhs: &str, rhs: &str) -> Ordering {
    let lhs = case_folded(lhs);
    let rhs = case_folded(rhs);
    japanese_collator().compare(&lhs, &rhs)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceMemberLabel {
    visible: SafeLiteralText,
    sort_key: String,
}

impl SurfaceMemberLabel {
    pub fn visible(&self) -> &SafeLiteralText {
        &self.visible
    }

    pub fn sort_key(&self) -> &str {
        &self.sort_key
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SurfaceMemberLabels {
    members: BTreeMap<MemberId, SurfaceMemberLabel>,
    roles: BTreeMap<RoleId, SafeLiteralText>,
}

impl SurfaceMemberLabels {
    fn member_sort_text(&self, member_id: MemberId) -> Cow<'_, str> {
        self.member(member_id)
            .map(|label| Cow::Borrowed(label.sort_key()))
            .unwrap_or_else(|| Cow::Owned(i18n::unknown_user_label(member_id.0).to_string()))
    }

    pub fn from_member_names<'a, I>(members: I) -> Self
    where
        I: IntoIterator<Item = (MemberId, Option<&'a str>)>,
    {
        let base_labels: Vec<(MemberId, SafeLiteralText)> = members
            .into_iter()
            .map(|(member_id, display_name)| {
                let visible = match display_name.and_then(SafeLiteralText::from_roster_label) {
                    Some(label) => label,
                    None => unknown_user_safe_label(member_id),
                };
                (member_id, visible)
            })
            .collect();

        let collisions = base_labels
            .iter()
            .fold(HashMap::new(), |mut counts, (_, label)| {
                *counts.entry(label.as_str().to_owned()).or_insert(0usize) += 1;
                counts
            });

        let members = base_labels
            .into_iter()
            .map(|(member_id, base_visible)| {
                let sort_key = base_visible.as_str().to_owned();
                let collision_count = match collisions.get(base_visible.as_str()) {
                    Some(count) => *count,
                    None => 0,
                };
                let visible = if collision_count > 1 {
                    disambiguated_safe_label(&base_visible, member_id)
                } else {
                    base_visible
                };
                (member_id, SurfaceMemberLabel { visible, sort_key })
            })
            .collect();

        Self {
            members,
            roles: BTreeMap::new(),
        }
    }

    pub fn insert_role_name(&mut self, role_id: RoleId, role_name: &str) {
        if let Some(label) = SafeLiteralText::from_roster_label(role_name) {
            self.roles.insert(role_id, label);
        }
    }

    pub fn member(&self, member_id: MemberId) -> Option<&SurfaceMemberLabel> {
        self.members.get(&member_id)
    }

    pub fn role(&self, role_id: RoleId) -> Option<&SafeLiteralText> {
        self.roles.get(&role_id)
    }

    pub fn sorted_member_ids(&self) -> Vec<MemberId> {
        let mut rows: Vec<_> = self.members.keys().copied().collect();
        rows.sort_by(|lhs, rhs| self.compare_members(*lhs, *rhs));
        rows
    }

    pub fn compare_members(&self, lhs: MemberId, rhs: MemberId) -> Ordering {
        let lhs_label = self.member_sort_text(lhs);
        let rhs_label = self.member_sort_text(rhs);
        Self::compare_display_text(lhs_label.as_ref(), rhs_label.as_ref()).then(lhs.cmp(&rhs))
    }

    pub fn compare_display_text(lhs: &str, rhs: &str) -> Ordering {
        compare_display_text(lhs, rhs)
    }

    pub fn safe_member_label(&self, member_id: MemberId) -> SafeLiteralText {
        match self.member(member_id) {
            Some(label) => label.visible().clone(),
            None => unknown_user_safe_label(member_id),
        }
    }

    pub fn safe_actor_label(&self, entry: &LedgerEntry) -> SafeLiteralText {
        match entry.metadata.recorded_by {
            Some(member_id) => self.safe_member_label(member_id),
            None => unknown_member_label(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SurfaceMemberLabels;
    use std::cmp::Ordering;
    use walicord_domain::model::{MemberId, RoleId};
    use walicord_i18n as i18n;

    #[test]
    fn missing_member_names_use_the_fixed_unknown_user_fallback() {
        let labels = SurfaceMemberLabels::from_member_names([(MemberId(42), None)]);

        assert_eq!(
            labels
                .member(MemberId(42))
                .expect("member label should exist")
                .visible()
                .as_str(),
            "不明なユーザー (ID: 42)"
        );
    }

    #[test]
    fn colliding_visible_names_are_disambiguated_deterministically() {
        let labels = SurfaceMemberLabels::from_member_names([
            (MemberId(2), Some("Alice")),
            (MemberId(1), Some("Alice")),
            (MemberId(3), Some("Bob")),
        ]);

        assert_eq!(
            labels
                .member(MemberId(1))
                .expect("member label should exist")
                .visible()
                .as_str(),
            "Alice (ID: 1)"
        );
        assert_eq!(
            labels
                .member(MemberId(2))
                .expect("member label should exist")
                .visible()
                .as_str(),
            "Alice (ID: 2)"
        );
        assert_eq!(
            labels.sorted_member_ids(),
            vec![MemberId(1), MemberId(2), MemberId(3)]
        );
    }

    #[test]
    fn member_sort_order_uses_locale_aware_display_names_before_member_id_tie_breaks() {
        let labels = SurfaceMemberLabels::from_member_names([
            (MemberId(2), Some("ao")),
            (MemberId(1), Some("Ao")),
            (MemberId(3), Some("Bob")),
        ]);

        assert_eq!(
            labels.sorted_member_ids(),
            vec![MemberId(1), MemberId(2), MemberId(3)]
        );
    }

    #[test]
    fn display_text_comparison_uses_locale_aware_collation() {
        assert_eq!(
            SurfaceMemberLabels::compare_display_text("ao", "Ao"),
            Ordering::Equal
        );
    }

    #[test]
    fn compare_members_uses_unknown_user_fallback_for_missing_ids() {
        let labels = SurfaceMemberLabels::from_member_names([(MemberId(2), Some("Alice"))]);
        let expected = SurfaceMemberLabels::compare_display_text(
            &i18n::unknown_user_label(1).to_string(),
            labels
                .member(MemberId(2))
                .expect("member label should exist")
                .sort_key(),
        )
        .then(MemberId(1).cmp(&MemberId(2)));

        assert_eq!(labels.compare_members(MemberId(1), MemberId(2)), expected);
    }

    #[test]
    fn role_labels_are_safely_normalized_before_storage() {
        let mut labels = SurfaceMemberLabels::default();
        labels.insert_role_name(RoleId(1), "開発");
        labels.insert_role_name(RoleId(2), " @everyone <@123> ");

        assert_eq!(
            labels
                .role(RoleId(1))
                .expect("role label should exist")
                .as_str(),
            "開発"
        );
        assert_eq!(
            labels
                .role(RoleId(2))
                .expect("role label should exist")
                .as_str(),
            "＠everyone ＜＠123＞"
        );
    }
}
