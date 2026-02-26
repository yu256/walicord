use crate::model::{MemberId, MemberSet, MemberSetExpr, RoleId, RoleMembers};
use fxhash::{FxHashMap, FxHashSet};
use std::sync::OnceLock;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemberSetResolutionError<'a> {
    UndefinedGroup {
        name: &'a str,
    },
    UndefinedRole {
        id: RoleId,
    },
    ExpressionTooLarge {
        member_count: usize,
        max_supported: usize,
    },
    InvalidExpression,
}

/// Resolves group names to sets of member IDs
pub struct MemberSetResolver<'a> {
    // Groups map group names to sets of member IDs
    groups: FxHashMap<&'a str, FxHashSet<MemberId>>,
    roles: &'a RoleMembers,
    default_members: Option<FxHashSet<MemberId>>,
}

impl<'a> MemberSetResolver<'a> {
    fn empty_roles() -> &'static RoleMembers {
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(RoleMembers::default)
    }

    pub fn new() -> Self {
        Self::new_with_members(std::iter::empty())
    }

    pub fn new_with_members<I>(members: I) -> Self
    where
        I: IntoIterator<Item = MemberId>,
    {
        Self::new_with_context(members, Self::empty_roles())
    }

    pub fn new_with_context<I>(members: I, roles: &'a RoleMembers) -> Self
    where
        I: IntoIterator<Item = MemberId>,
    {
        let default_members: FxHashSet<MemberId> = members.into_iter().collect();
        Self {
            groups: FxHashMap::default(),
            roles,
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
        let members = self.try_evaluate_members(expr).ok()?;
        self.register_group_members(name, members.iter());
        Some(members)
    }

    pub fn try_evaluate_and_register_group(
        &mut self,
        name: &'a str,
        expr: &MemberSetExpr<'a>,
    ) -> Result<MemberSet, MemberSetResolutionError<'a>> {
        let members = self.try_evaluate_members(expr)?;
        self.register_group_members(name, members.iter());
        Ok(members)
    }

    pub fn register_group_members<I>(&mut self, name: &'a str, members: I)
    where
        I: IntoIterator<Item = MemberId>,
    {
        self.groups.insert(name, members.into_iter().collect());
    }

    pub fn evaluate_members(&self, expr: &MemberSetExpr<'a>) -> Option<MemberSet> {
        self.try_evaluate_members(expr).ok()
    }

    pub fn try_evaluate_members(
        &self,
        expr: &MemberSetExpr<'a>,
    ) -> Result<MemberSet, MemberSetResolutionError<'a>> {
        if let Some(set) = expr.evaluate(
            &|name| {
                if name == "MEMBERS" {
                    self.default_members.as_ref()
                } else {
                    self.groups.get(name)
                }
            },
            &|role_id| self.roles.get(&role_id),
        ) {
            let mut ordered: Vec<MemberId> = set.iter().copied().collect();
            ordered.sort_unstable();
            return Ok(MemberSet::new(ordered));
        }

        let mut referenced_members: FxHashSet<MemberId> = FxHashSet::default();

        for member_id in expr.referenced_ids() {
            referenced_members.insert(member_id);
        }
        for group_name in expr.referenced_groups() {
            let members = if group_name == "MEMBERS" {
                self.default_members.as_ref()
            } else {
                self.groups.get(group_name)
            }
            .ok_or(MemberSetResolutionError::UndefinedGroup { name: group_name })?;
            referenced_members.extend(members.iter().copied());
        }
        for role_id in expr.referenced_role_ids() {
            let members = self
                .roles
                .get(&role_id)
                .ok_or(MemberSetResolutionError::UndefinedRole { id: role_id })?;
            referenced_members.extend(members.iter().copied());
        }

        let max_supported = u128::BITS as usize;
        if referenced_members.len() > max_supported {
            return Err(MemberSetResolutionError::ExpressionTooLarge {
                member_count: referenced_members.len(),
                max_supported,
            });
        }

        let set = expr
            .evaluate(
                &|name| {
                    if name == "MEMBERS" {
                        self.default_members.as_ref()
                    } else {
                        self.groups.get(name)
                    }
                },
                &|role_id| self.roles.get(&role_id),
            )
            .ok_or(MemberSetResolutionError::InvalidExpression)?;
        let mut ordered: Vec<MemberId> = set.iter().copied().collect();
        ordered.sort_unstable();
        Ok(MemberSet::new(ordered))
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

    pub fn is_role_defined(&self, role_id: RoleId) -> bool {
        self.roles.contains_key(&role_id)
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
    use crate::model::{MemberSetExpr, MemberSetOp, RoleId, RoleMembers};
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

    #[rstest]
    #[case::unknown_group(
        MemberSetExpr::new([MemberSetOp::PushGroup("missing")]),
        Err(MemberSetResolutionError::UndefinedGroup { name: "missing" })
    )]
    #[case::unknown_role(
        MemberSetExpr::new([MemberSetOp::PushRole(RoleId(999))]),
        Err(MemberSetResolutionError::UndefinedRole { id: RoleId(999) })
    )]
    #[case::invalid_expression(
        MemberSetExpr::new([MemberSetOp::Union]),
        Err(MemberSetResolutionError::InvalidExpression)
    )]
    fn try_evaluate_members_error_cases(
        #[case] expr: MemberSetExpr<'static>,
        #[case] expected: Result<MemberSet, MemberSetResolutionError<'static>>,
    ) {
        let resolver = MemberSetResolver::new();
        assert_eq!(resolver.try_evaluate_members(&expr), expected);
    }

    #[test]
    fn evaluate_members_returns_none_for_unknown_group() {
        let resolver = MemberSetResolver::new();
        let expr = MemberSetExpr::new([MemberSetOp::PushGroup("missing")]);

        assert!(resolver.evaluate_members(&expr).is_none());
    }

    #[test]
    fn evaluate_members_resolves_role_reference() {
        let roles = RoleMembers::from_iter([(
            RoleId(10),
            FxHashSet::from_iter([MemberId(2), MemberId(1)]),
        )]);
        let resolver = MemberSetResolver::new_with_context(std::iter::empty(), &roles);
        let expr = MemberSetExpr::new([MemberSetOp::PushRole(RoleId(10))]);

        let members = resolver
            .evaluate_members(&expr)
            .expect("role members should resolve");
        assert_eq!(members.members(), [MemberId(1), MemberId(2)]);
    }

    #[test]
    fn evaluate_members_returns_none_for_unknown_role() {
        let resolver = MemberSetResolver::new();
        let expr = MemberSetExpr::new([MemberSetOp::PushRole(RoleId(999))]);
        assert!(resolver.evaluate_members(&expr).is_none());
    }

    #[test]
    fn evaluate_members_supports_union_between_group_and_role() {
        let roles = RoleMembers::from_iter([(
            RoleId(10),
            FxHashSet::from_iter([MemberId(2), MemberId(3)]),
        )]);
        let mut resolver = MemberSetResolver::new_with_context(std::iter::empty(), &roles);
        resolver.register_group_members("A", [MemberId(1), MemberId(2)]);

        let expr = MemberSetExpr::new([
            MemberSetOp::PushGroup("A"),
            MemberSetOp::PushRole(RoleId(10)),
            MemberSetOp::Union,
        ]);
        let members = resolver
            .evaluate_members(&expr)
            .expect("members should resolve");
        assert_eq!(members.members(), [MemberId(1), MemberId(2), MemberId(3)]);
    }

    #[test]
    fn try_evaluate_members_returns_expression_too_large() {
        let mut resolver = MemberSetResolver::new();
        resolver.register_group_members("A", (1..=64).map(MemberId));
        resolver.register_group_members("B", (65..=129).map(MemberId));
        let expr = MemberSetExpr::new([
            MemberSetOp::PushGroup("A"),
            MemberSetOp::PushGroup("B"),
            MemberSetOp::Union,
        ]);

        assert_eq!(
            resolver.try_evaluate_members(&expr),
            Err(MemberSetResolutionError::ExpressionTooLarge {
                member_count: 129,
                max_supported: 128,
            })
        );
    }

    #[test]
    fn try_evaluate_members_allows_large_single_group_reference() {
        let mut resolver = MemberSetResolver::new();
        resolver.register_group_members("all", (1..=129).map(MemberId));
        let expr = MemberSetExpr::new([MemberSetOp::PushGroup("all")]);

        let members = resolver
            .try_evaluate_members(&expr)
            .expect("single group reference should resolve even when large");

        assert_eq!(members.members().len(), 129);
        assert_eq!(members.members().first().copied(), Some(MemberId(1)));
        assert_eq!(members.members().last().copied(), Some(MemberId(129)));
    }

    #[test]
    fn try_evaluate_members_allows_large_single_role_reference() {
        let roles =
            RoleMembers::from_iter([(RoleId(10), FxHashSet::from_iter((1..=129).map(MemberId)))]);
        let resolver = MemberSetResolver::new_with_context(std::iter::empty(), &roles);
        let expr = MemberSetExpr::new([MemberSetOp::PushRole(RoleId(10))]);

        let members = resolver
            .try_evaluate_members(&expr)
            .expect("single role reference should resolve even when large");

        assert_eq!(members.members().len(), 129);
        assert_eq!(members.members().first().copied(), Some(MemberId(1)));
        assert_eq!(members.members().last().copied(), Some(MemberId(129)));
    }
}
