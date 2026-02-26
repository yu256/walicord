use super::{
    AllocationStrategy, MemberBalances, MemberId, MemberSet, Money, Ratios, RemainderPolicy,
    SplitError, Weight,
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
/// * `allocation` - The strategy used to split the amount (even or weighted)
///
/// # Returns
/// * `Ok(())` if distribution succeeded
/// * `Err(SplitError::EmptyRatios)` if members set is empty (should not happen due to early return)
/// * `Err(SplitError::ZeroTotalRatio)` if weighted distribution has zero total weight
pub fn distribute_balances(
    balances: &mut MemberBalances,
    members: &MemberSet,
    amount: Money,
    direction: BalanceDeltaDirection,
    allocation: &AllocationStrategy,
) -> Result<(), SplitError> {
    if members.is_empty() {
        return Ok(());
    }

    let shares: Vec<Money> = match allocation {
        AllocationStrategy::Even => amount
            .split_even(members.members().len(), RemainderPolicy::FrontLoad)
            .collect(),
        AllocationStrategy::Weighted(weights) => {
            let weight_vec: Vec<Weight> = members
                .iter()
                .map(|id| weights.get(&id).copied().unwrap_or(Weight(1)))
                .collect();
            let ratios = Ratios::try_new(weight_vec)?;
            amount.split_ratio(&ratios, RemainderPolicy::FrontLoad)
        }
    };

    for (member, share) in members.iter().zip(shares) {
        let signed = share * direction.factor();
        *balances.entry(member).or_insert(Money::ZERO) += signed;
    }

    Ok(())
}
