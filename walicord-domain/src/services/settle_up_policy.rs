use crate::{
    model::{MemberBalances, MemberId, Money, Settlement},
    services::{
        SettlementContext, SettlementRoundingError, TransferConstructor,
        quantize_balances_with_preferred_members,
    },
};
use std::borrow::Cow;

pub struct SettleUpPolicy;

impl SettleUpPolicy {
    pub fn settle<C>(
        balances: &MemberBalances,
        settle_members: &[MemberId],
        cash_members: C,
        context: SettlementContext,
    ) -> Result<Settlement, SettlementRoundingError>
    where
        C: IntoIterator<Item = MemberId>,
    {
        let constructor = TransferConstructor;
        let balances_with_members: Cow<'_, MemberBalances> = if settle_members
            .iter()
            .all(|member| balances.contains_key(member))
        {
            Cow::Borrowed(balances)
        } else {
            let mut owned = balances.clone();
            for member in settle_members {
                owned.entry(*member).or_insert(Money::ZERO);
            }
            Cow::Owned(owned)
        };
        // Partial settle-up still quantizes the full group; preferred members only
        // influence who absorbs zero-sum repair so non-target members are affected less.
        let balances = quantize_balances_with_preferred_members(
            balances_with_members.as_ref(),
            context,
            settle_members,
        )?;
        let cash_members: Vec<MemberId> = cash_members.into_iter().collect();
        constructor.calculate(balances, settle_members, &cash_members, context)
    }
}
