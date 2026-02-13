use crate::{
    model::{MemberBalances, MemberId, Money, Settlement},
    services::{
        SettlementCalculator, SettlementContext, SettlementRoundingError,
        quantize_balances_with_preferred_members,
    },
};

pub struct SettleUpPolicy;

impl SettleUpPolicy {
    pub fn settle<I>(
        mut balances: MemberBalances,
        all_members: I,
        settle_members: &[MemberId],
        context: SettlementContext,
    ) -> Result<Settlement, SettlementRoundingError>
    where
        I: IntoIterator<Item = MemberId>,
    {
        let calculator = SettlementCalculator;
        for member in all_members {
            balances.entry(member).or_insert(Money::ZERO);
        }
        // Partial settle-up still quantizes the full group; preferred members only
        // influence who absorbs zero-sum repair so non-target members are affected less.
        let balances =
            quantize_balances_with_preferred_members(&balances, context, settle_members)?;
        Ok(calculator.calculate(balances, settle_members))
    }
}
