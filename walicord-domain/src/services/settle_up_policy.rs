use crate::{
    model::{MemberBalances, MemberId, Money, Settlement},
    services::SettlementCalculator,
};

pub struct SettleUpPolicy;

impl SettleUpPolicy {
    pub fn settle<I>(
        mut balances: MemberBalances,
        all_members: I,
        settle_members: &[MemberId],
    ) -> Settlement
    where
        I: IntoIterator<Item = MemberId>,
    {
        let calculator = SettlementCalculator;
        for member in all_members {
            balances.entry(member).or_insert(Money::zero());
        }
        calculator.calculate(balances, settle_members)
    }
}
