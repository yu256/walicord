use crate::model::{MemberSet, MemberSetExpr};
use std::collections::{HashMap, HashSet};

pub struct MemberSetResolver<'a> {
    order: &'a [&'a str],
    order_lookup: HashSet<&'a str>,
    base_sets: HashMap<&'a str, HashSet<&'a str>>,
    groups: HashMap<&'a str, HashSet<&'a str>>,
}

impl<'a> MemberSetResolver<'a> {
    pub fn new(order: &'a [&'a str]) -> Self {
        let order_lookup: HashSet<&str> = order.iter().copied().collect();

        let mut base_sets: HashMap<&str, HashSet<&str>> = order
            .iter()
            .copied()
            .map(|member| (member, HashSet::from([member])))
            .collect();
        base_sets.insert("MEMBERS", order_lookup.clone());

        Self {
            order,
            order_lookup,
            base_sets,
            groups: HashMap::new(),
        }
    }

    pub fn evaluate_and_register_group(
        &mut self,
        name: &'a str,
        expr: &MemberSetExpr<'a>,
    ) -> Option<MemberSet<'a>> {
        let members = self.evaluate_members(expr)?;
        self.register_group_members(name, members.iter());
        Some(members)
    }

    pub fn register_group_members<I>(&mut self, name: &'a str, members: I)
    where
        I: IntoIterator<Item = &'a str>,
    {
        self.groups.insert(name, members.into_iter().collect());
    }

    pub fn evaluate_members(&self, expr: &MemberSetExpr<'a>) -> Option<MemberSet<'a>> {
        let set = self.evaluate_set(expr)?;
        Some(MemberSet::new(self.order_members(&set)))
    }

    pub fn is_defined(&self, name: &str) -> bool {
        self.groups.contains_key(name) || self.base_sets.contains_key(name)
    }

    fn evaluate_set(&self, expr: &MemberSetExpr<'a>) -> Option<HashSet<&'a str>> {
        expr.evaluate(&|name| self.groups.get(name).or_else(|| self.base_sets.get(name)))
            .map(|cow| cow.into_owned())
    }

    fn order_members(&self, set: &HashSet<&'a str>) -> Vec<&'a str> {
        let mut ordered: Vec<&str> = self
            .order
            .iter()
            .copied()
            .filter(|member| set.contains(member))
            .collect();

        if ordered.len() == set.len() {
            return ordered;
        }

        let mut extras: Vec<&str> = set
            .iter()
            .copied()
            .filter(|member| !self.order_lookup.contains(member))
            .collect();
        extras.sort_unstable();
        ordered.extend(extras);
        ordered
    }
}
