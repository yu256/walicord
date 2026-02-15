use crate::model::{MemberId, MemberSet, MemberSetExpr};
use fxhash::{FxHashMap, FxHashSet};

/// Resolves group names to sets of member IDs
pub struct MemberSetResolver<'a> {
    // Groups map group names to sets of member IDs
    groups: FxHashMap<&'a str, FxHashSet<MemberId>>,
    default_members: Option<FxHashSet<MemberId>>,
}

impl<'a> MemberSetResolver<'a> {
    pub fn new() -> Self {
        Self::new_with_members(std::iter::empty())
    }

    pub fn new_with_members<I>(members: I) -> Self
    where
        I: IntoIterator<Item = MemberId>,
    {
        let default_members: FxHashSet<MemberId> = members.into_iter().collect();
        Self {
            groups: FxHashMap::default(),
            default_members: if default_members.is_empty() {
                None
            } else {
                Some(default_members)
            },
        }
    }

    pub fn evaluate_and_register_group(
        &mut self,
        name: &'a str,
        expr: &MemberSetExpr<'a>,
    ) -> Option<MemberSet> {
        let members = self.evaluate_members(expr)?;
        self.register_group_members(name, members.iter());
        Some(members)
    }

    pub fn register_group_members<I>(&mut self, name: &'a str, members: I)
    where
        I: IntoIterator<Item = MemberId>,
    {
        self.groups.insert(name, members.into_iter().collect());
    }

    pub fn evaluate_members(&self, expr: &MemberSetExpr<'a>) -> Option<MemberSet> {
        let set = expr.evaluate(&|name| {
            if name == "MEMBERS" {
                self.default_members.as_ref()
            } else {
                self.groups.get(name)
            }
        })?;
        let mut ordered: Vec<MemberId> = set.iter().copied().collect();
        ordered.sort_unstable();
        Some(MemberSet::new(ordered))
    }

    pub fn is_defined(&self, name: &str) -> bool {
        if name == "MEMBERS" {
            self.default_members.is_some()
        } else {
            self.groups.contains_key(name)
        }
    }

    pub fn is_group_defined(&self, name: &str) -> bool {
        self.groups.contains_key(name)
    }
}

impl<'a> Default for MemberSetResolver<'a> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{MemberSetExpr, MemberSetOp};
    use rstest::rstest;

    #[test]
    fn evaluate_members_uses_default_members_for_members_keyword() {
        let resolver = MemberSetResolver::new_with_members([MemberId(2), MemberId(1)]);
        let expr = MemberSetExpr::new([MemberSetOp::PushGroup("MEMBERS")]);

        let members = resolver
            .evaluate_members(&expr)
            .expect("members should resolve");

        assert_eq!(members.members(), [MemberId(1), MemberId(2)]);
    }

    #[test]
    fn evaluate_members_sorts_group_members() {
        let mut resolver = MemberSetResolver::new();
        resolver.register_group_members("team", [MemberId(3), MemberId(1), MemberId(2)]);
        let expr = MemberSetExpr::new([MemberSetOp::PushGroup("team")]);

        let members = resolver
            .evaluate_members(&expr)
            .expect("members should resolve");

        assert_eq!(members.members(), [MemberId(1), MemberId(2), MemberId(3)]);
    }

    #[rstest]
    #[case::union(
        vec![
            MemberSetOp::PushGroup("A"),
            MemberSetOp::PushGroup("B"),
            MemberSetOp::Union,
        ],
        vec![MemberId(1), MemberId(2), MemberId(3)]
    )]
    #[case::intersection(
        vec![
            MemberSetOp::PushGroup("A"),
            MemberSetOp::PushGroup("B"),
            MemberSetOp::Intersection,
        ],
        vec![MemberId(2)]
    )]
    #[case::difference(
        vec![
            MemberSetOp::PushGroup("A"),
            MemberSetOp::PushGroup("B"),
            MemberSetOp::Difference,
        ],
        vec![MemberId(1)]
    )]
    fn evaluate_members_supports_set_operations(
        #[case] ops: Vec<MemberSetOp<'static>>,
        #[case] expected: Vec<MemberId>,
    ) {
        let mut resolver = MemberSetResolver::new();
        resolver.register_group_members("A", [MemberId(1), MemberId(2)]);
        resolver.register_group_members("B", [MemberId(2), MemberId(3)]);

        let expr = MemberSetExpr::new(ops);
        let members = resolver
            .evaluate_members(&expr)
            .expect("members should resolve");

        assert_eq!(members.members(), expected.as_slice());
    }

    #[test]
    fn evaluate_members_returns_none_for_unknown_group() {
        let resolver = MemberSetResolver::new();
        let expr = MemberSetExpr::new([MemberSetOp::PushGroup("missing")]);

        assert!(resolver.evaluate_members(&expr).is_none());
    }
}
