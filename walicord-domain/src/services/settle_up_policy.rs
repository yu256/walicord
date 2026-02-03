use crate::{
    model::{Money, Settlement},
    services::SettlementCalculator,
};
use std::collections::{HashMap, HashSet};

pub struct SettleUpPolicy;

impl SettleUpPolicy {
    pub fn settle<'a>(
        balances: HashMap<&'a str, Money>,
        members: &'a [&'a str],
        settle_members: &[&'a str],
    ) -> Settlement<'a> {
        let participants = build_participant_order(members, &balances);
        let calculator = SettlementCalculator;
        calculator.calculate(balances, &participants, settle_members)
    }
}

fn build_participant_order<'a>(
    members: &'a [&'a str],
    balances: &HashMap<&'a str, Money>,
) -> Vec<&'a str> {
    let mut order: Vec<&'a str> = Vec::with_capacity(balances.len());
    let mut seen: HashSet<&'a str> = HashSet::with_capacity(balances.len());

    for &member in members {
        if balances.contains_key(member) {
            order.push(member);
            seen.insert(member);
        }
    }

    let mut extras: Vec<&'a str> = balances
        .keys()
        .copied()
        .filter(|member| seen.insert(*member))
        .collect();
    extras.sort_unstable();
    order.extend(extras);
    order
}
