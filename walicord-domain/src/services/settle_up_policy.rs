use crate::{
    model::{MemberId, Money, Settlement},
    services::SettlementCalculator,
};
use fxhash::FxHashMap;

pub struct SettleUpPolicy;

impl SettleUpPolicy {
    pub fn settle(balances: FxHashMap<MemberId, Money>, settle_members: &[MemberId]) -> Settlement {
        let calculator = SettlementCalculator;
        let mut all_members: Vec<MemberId> = balances.keys().copied().collect();
        all_members.sort_unstable();
        calculator.calculate(balances, &all_members, settle_members)
    }
}
