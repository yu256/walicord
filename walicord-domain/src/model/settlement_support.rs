use super::{
    MemberBalances, MemberId, MemberSet, Money, RemainderPolicy, ResolvedAllocationStrategy,
    SplitError,
};
use rust_decimal::Decimal;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Transfer {
    pub from: MemberId,
    pub to: MemberId,
    pub amount: Money,
}

#[derive(Debug, PartialEq)]
pub struct Settlement {
    pub new_balances: MemberBalances,
    pub transfers: Vec<Transfer>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BalanceDeltaDirection {
    Increase,
    Decrease,
}

impl BalanceDeltaDirection {
    fn factor(self) -> Decimal {
        match self {
            Self::Increase => Decimal::ONE,
            Self::Decrease => -Decimal::ONE,
        }
    }
}

/// Distributes an amount among members, updating their balances.
///
/// # Arguments
/// * `balances` - Map of member balances to update
/// * `members` - Set of members to distribute among
/// * `amount` - Total amount to distribute
/// * `direction` - Whether distributed shares increase or decrease balances
/// * `allocation` - The resolved strategy used to split the amount
///
/// # Returns
/// * `Ok(())` if distribution succeeded
///
/// `allocation` is already validated and resolved, so the current implementation
/// has no runtime error path. The `Result` return type is retained for API
/// compatibility with earlier versions of this helper.
pub fn distribute_balances(
    balances: &mut MemberBalances,
    members: &MemberSet,
    amount: Money,
    direction: BalanceDeltaDirection,
    allocation: &ResolvedAllocationStrategy,
) -> Result<(), SplitError> {
    if members.is_empty() {
        return Ok(());
    }

    let shares: Vec<Money> = match allocation {
        ResolvedAllocationStrategy::Even => amount
            .split_even(members.members().len(), RemainderPolicy::FrontLoad)
            .collect(),
        ResolvedAllocationStrategy::Weighted(ratios) => {
            amount.split_ratio(ratios, RemainderPolicy::FrontLoad)
        }
    };

    for (member, share) in members.iter().zip(shares) {
        let signed = share * direction.factor();
        *balances.entry(member).or_insert(Money::ZERO) += signed;
    }

    Ok(())
}
