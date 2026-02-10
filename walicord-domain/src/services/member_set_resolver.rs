use crate::model::{MemberId, MemberSet, MemberSetExpr};
use fxhash::{FxHashMap, FxHashSet};

/// Resolves group names to sets of member IDs
pub struct MemberSetResolver<'a> {
    // Groups map group names to sets of member IDs
    groups: FxHashMap<&'a str, FxHashSet<MemberId>>,
}

impl<'a> MemberSetResolver<'a> {
    pub fn new() -> Self {
        Self {
            groups: FxHashMap::default(),
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
        let set = expr.evaluate(&|name| self.groups.get(name).cloned())?;
        let mut ordered: Vec<MemberId> = set.into_iter().collect();
        ordered.sort_unstable();
        Some(MemberSet::new(ordered))
    }

    pub fn is_defined(&self, name: &str) -> bool {
        // For the new design, we check if it's a registered group
        self.groups.contains_key(name)
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
