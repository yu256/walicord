use proptest::prelude::*;
use walicord_domain::{
    MemberSetResolver, Program, SettleUpPolicy, StatementWithLine,
    model::{MemberSetExpr, MemberSetOp, Money, Payment, Statement},
};

proptest! {
    #[test]
    fn balances_sum_to_zero(
        member_count in 1usize..=6,
        payment_count in 0usize..=30,
        amounts in prop::collection::vec(0u64..=10_000, 0..=30),
        payer_indexes in prop::collection::vec(0usize..=5, 0..=30),
        payee_indexes in prop::collection::vec(0usize..=5, 0..=30),
    ) {
        let available = ["A", "B", "C", "D", "E", "F"];
        let members = &available[..member_count];

        let mut statements = Vec::with_capacity(payment_count);
        for idx in 0..payment_count {
            let amount = *amounts.get(idx).unwrap_or(&0);
            let payer_idx = payer_indexes.get(idx).copied().unwrap_or(0) % member_count;
            let payee_idx = payee_indexes.get(idx).copied().unwrap_or(0) % member_count;
            let payer = members[payer_idx];
            let payee = members[payee_idx];

            statements.push(StatementWithLine {
                line: idx + 1,
                statement: Statement::Payment(Payment {
                    amount: Money::from_u64(amount),
                    payer: MemberSetExpr::new(vec![MemberSetOp::Push(payer)]),
                    payee: MemberSetExpr::new(vec![MemberSetOp::Push(payee)]),
                }),
            });
        }

        let program = Program::try_new(members, statements).expect("program build failed");
        let balances = program.calculate_balances();
        let total: i64 = balances.values().map(|money| money.amount()).sum();
        prop_assert_eq!(total, 0);
    }
}

proptest! {
    #[test]
    fn settle_members_balance_zero(
        member_count in 1usize..=6,
        payment_count in 0usize..=30,
        amounts in prop::collection::vec(0u64..=10_000, 0..=30),
        payer_indexes in prop::collection::vec(0usize..=5, 0..=30),
        payee_indexes in prop::collection::vec(0usize..=5, 0..=30),
        settle_mask in 1usize..=63,
    ) {
        let available = ["A", "B", "C", "D", "E", "F"];
        let members = &available[..member_count];

        let mut statements = Vec::with_capacity(payment_count);
        for idx in 0..payment_count {
            let amount = *amounts.get(idx).unwrap_or(&0);
            let payer_idx = payer_indexes.get(idx).copied().unwrap_or(0) % member_count;
            let payee_idx = payee_indexes.get(idx).copied().unwrap_or(0) % member_count;
            let payer = members[payer_idx];
            let payee = members[payee_idx];

            statements.push(StatementWithLine {
                line: idx + 1,
                statement: Statement::Payment(Payment {
                    amount: Money::from_u64(amount),
                    payer: MemberSetExpr::new(vec![MemberSetOp::Push(payer)]),
                    payee: MemberSetExpr::new(vec![MemberSetOp::Push(payee)]),
                }),
            });
        }

        let mut ops = Vec::new();
        let mut selected = 0;
        for (idx, &member) in members.iter().enumerate() {
            if (settle_mask & (1 << idx)) == 0 {
                continue;
            }
            ops.push(MemberSetOp::Push(member));
            if selected > 0 {
                ops.push(MemberSetOp::Union);
            }
            selected += 1;
        }

        if selected == 0 {
            ops.push(MemberSetOp::Push(members[0]));
        }

        let program = Program::try_new(members, statements).expect("program build failed");
        let balances = program.calculate_balances();

        let settle_expr = MemberSetExpr::new(ops);
        let resolver = MemberSetResolver::new(members);
        let Some(settle_members) = resolver.evaluate_members(&settle_expr) else {
            return Ok(());
        };
        if settle_members.is_empty() {
            return Ok(());
        }

        let settlement = SettleUpPolicy::settle(balances, members, settle_members.members());
        let balances = settlement.new_balances;

        for (idx, &member) in members.iter().enumerate() {
            if (settle_mask & (1 << idx)) == 0 {
                continue;
            }
            let balance = balances.get(member).copied().unwrap_or_else(Money::zero);
            prop_assert_eq!(balance.amount(), 0);
        }
    }
}
