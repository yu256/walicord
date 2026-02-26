use super::*;
use std::sync::OnceLock;

impl<'a> Default for BalanceAccumulator<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> BalanceAccumulator<'a> {
    fn empty_roles() -> &'static RoleMembers {
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(RoleMembers::default)
    }

    pub fn new() -> Self {
        Self::new_with_members(&[])
    }

    pub fn new_with_members(member_ids: &[MemberId]) -> Self {
        Self::new_with_context(member_ids, Self::empty_roles())
    }

    pub fn new_with_context(member_ids: &[MemberId], role_members: &'a RoleMembers) -> Self {
        let balances: MemberBalances = MemberBalances::default();
        let resolver =
            MemberSetResolver::new_with_context(member_ids.iter().copied(), role_members);

        Self { balances, resolver }
    }

    pub fn apply(&mut self, statement: &Statement<'a>) -> Result<(), BalanceError> {
        match statement {
            Statement::Declaration(decl) => {
                for member_id in decl.expression.referenced_ids() {
                    self.balances.entry(member_id).or_insert(Money::ZERO);
                }
                let Ok(members_vec) = self.resolver.try_evaluate_members(&decl.expression) else {
                    return Ok(());
                };
                for member in members_vec.iter() {
                    self.balances.entry(member).or_insert(Money::ZERO);
                }
                self.resolver
                    .register_group_members(decl.name, members_vec.iter());
            }
            Statement::Payment(payment) => {
                for member_id in payment
                    .payer
                    .referenced_ids()
                    .chain(payment.payee.referenced_ids())
                {
                    self.balances.entry(member_id).or_insert(Money::ZERO);
                }
                let Ok(payer_members) = self.resolver.try_evaluate_members(&payment.payer) else {
                    return Ok(());
                };
                let Ok(payee_members) = self.resolver.try_evaluate_members(&payment.payee) else {
                    return Ok(());
                };

                // Validate weighted distribution before mutating any balances
                // to preserve zero-sum invariant
                if let AllocationStrategy::Weighted(weights) = &payment.allocation {
                    let total_weight: Option<Weight> =
                        payee_members.iter().try_fold(Weight::ZERO, |acc, id| {
                            let w = weights.get(&id).copied().unwrap_or(Weight(1));
                            acc.checked_add(w)
                        });
                    match total_weight {
                        None => return Err(BalanceError::WeightOverflow),
                        Some(Weight::ZERO) => return Err(BalanceError::ZeroTotalWeight),
                        Some(_) => {}
                    }
                }

                distribute_balances(
                    &mut self.balances,
                    &payer_members,
                    payment.amount,
                    BalanceDeltaDirection::Increase,
                    &AllocationStrategy::Even,
                )
                .expect("even distribution should never fail");

                distribute_balances(
                    &mut self.balances,
                    &payee_members,
                    payment.amount,
                    BalanceDeltaDirection::Decrease,
                    &payment.allocation,
                )
                .expect("weighted distribution validated above");
            }
        }
        Ok(())
    }

    pub fn balances(&self) -> &MemberBalances {
        &self.balances
    }

    pub fn into_balances(self) -> MemberBalances {
        self.balances
    }

    pub fn set_balances(&mut self, balances: MemberBalances) {
        self.balances = balances;
    }

    pub fn evaluate_members(&self, expr: &MemberSetExpr<'a>) -> Option<MemberSet> {
        self.resolver.evaluate_members(expr)
    }

    pub fn try_evaluate_members(
        &self,
        expr: &MemberSetExpr<'a>,
    ) -> Result<MemberSet, MemberSetResolutionError<'a>> {
        self.resolver.try_evaluate_members(expr)
    }
}
