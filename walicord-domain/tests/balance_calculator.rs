use proptest::prelude::*;
use rust_decimal::Decimal;
use std::collections::BTreeMap;
use walicord_domain::{
    MemberSetResolver, Program, SettleUpPolicy, SettlementContext, StatementWithLine,
    model::{MemberId, MemberSetExpr, MemberSetOp, Money, Payment, Statement, Weight},
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
        let mut statements = Vec::with_capacity(payment_count);
        for idx in 0..payment_count {
            let amount = *amounts.get(idx).unwrap_or(&0);
            let payer_idx = payer_indexes.get(idx).copied().unwrap_or(0) % member_count;
            let payee_idx = payee_indexes.get(idx).copied().unwrap_or(0) % member_count;
            let payer = MemberId(payer_idx as u64 + 1);
            let payee = MemberId(payee_idx as u64 + 1);

            statements.push(StatementWithLine {
                line: idx + 1,
                statement: Statement::Payment(Payment::even(
                    Money::try_from(amount).expect("amount should fit in i64"),
                    MemberSetExpr::new([MemberSetOp::Push(payer)]),
                    MemberSetExpr::new([MemberSetOp::Push(payee)]),
                )),
            });
        }

        let program = Program::try_new(statements, &[]).expect("program build failed");
        let balances = program.calculate_balances();
        let total: Money = balances.values().sum();
        let epsilon = Decimal::new(1, 6);
        prop_assert!(total.as_decimal().abs() <= epsilon);
    }

    #[test]
    fn weighted_balances_sum_to_zero(
        member_count in 1usize..=6,
        payment_count in 0usize..=30,
        amounts in prop::collection::vec(0u64..=10_000, 0..=30),
        payer_indexes in prop::collection::vec(0usize..=5, 0..=30),
        // Generate up to 3 payees per payment
        payee_groups in prop::collection::vec(
            prop::collection::vec((0usize..=5, 0u64..=5), 1..=3),
            0..=30
        ),
    ) {
        let mut statements = Vec::with_capacity(payment_count);
        for idx in 0..payment_count {
            let amount = *amounts.get(idx).unwrap_or(&0);
            let payer_idx = payer_indexes.get(idx).copied().unwrap_or(0) % member_count;
            let payer = MemberId(payer_idx as u64 + 1);

            let payees = payee_groups.get(idx).cloned().unwrap_or_else(|| vec![(0, 1)]);

            let mut payee_ops = Vec::new();
            let mut payee_weights = BTreeMap::new();

            for (i, (p_idx, weight)) in payees.iter().enumerate() {
                let payee_id = MemberId((p_idx % member_count) as u64 + 1);
                payee_ops.push(MemberSetOp::Push(payee_id));
                if i > 0 {
                    payee_ops.push(MemberSetOp::Union);
                }
                // Mirror production behavior by inserting all weights (including zeros)
                payee_weights.insert(payee_id, Weight(*weight));
            }

            // If all weights are zero, add a default fallback to avoid division by zero error in test
            if payee_weights.values().all(|&w| w == Weight::ZERO)
                && let Some(first_payee) = payee_ops.first()
                    && let MemberSetOp::Push(id) = first_payee {
                        payee_weights.insert(*id, Weight(1));
                    }

            statements.push(StatementWithLine {
                line: idx + 1,
                statement: Statement::Payment(Payment::weighted(
                    Money::try_from(amount).expect("amount should fit in i64"),
                    MemberSetExpr::new([MemberSetOp::Push(payer)]),
                    MemberSetExpr::new(payee_ops),
                    payee_weights,
                )),
            });
        }

        let program = Program::try_new(statements, &[]).expect("program build failed");
        let balances = program.calculate_balances();
        let total: Money = balances.values().sum();
        let epsilon = Decimal::new(1, 6);
        prop_assert!(total.as_decimal().abs() <= epsilon);
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
        let mut statements = Vec::with_capacity(payment_count);
        for idx in 0..payment_count {
            let amount = *amounts.get(idx).unwrap_or(&0);
            let payer_idx = payer_indexes.get(idx).copied().unwrap_or(0) % member_count;
            let payee_idx = payee_indexes.get(idx).copied().unwrap_or(0) % member_count;
            let payer = MemberId(payer_idx as u64 + 1);
            let payee = MemberId(payee_idx as u64 + 1);

            statements.push(StatementWithLine {
                line: idx + 1,
                statement: Statement::Payment(Payment::even(
                    Money::try_from(amount).expect("amount should fit in i64"),
                    MemberSetExpr::new([MemberSetOp::Push(payer)]),
                    MemberSetExpr::new([MemberSetOp::Push(payee)]),
                )),
            });
        }

        let mut ops = Vec::new();
        let mut selected = 0;
        for idx in 0..member_count {
            if (settle_mask & (1 << idx)) == 0 {
                continue;
            }
            ops.push(MemberSetOp::Push(MemberId(idx as u64 + 1)));
            if selected > 0 {
                ops.push(MemberSetOp::Union);
            }
            selected += 1;
        }

        if selected == 0 {
            ops.push(MemberSetOp::Push(MemberId(1)));
        }

        let program = Program::try_new(statements, &[]).expect("program build failed");
        let balances = program.calculate_balances();

        let settle_expr = MemberSetExpr::new(ops);
        let resolver = MemberSetResolver::new();
        let Some(settle_members) = resolver.evaluate_members(&settle_expr) else {
            return Ok(());
        };
        if settle_members.is_empty() {
            return Ok(());
        }

        let settlement = SettleUpPolicy::settle(
            &balances,
            settle_members.members(),
            std::iter::empty(),
            SettlementContext::jpy_default(),
        )
        .expect("settle should succeed");
        let balances = settlement.new_balances;

        for idx in 0..member_count {
            if (settle_mask & (1 << idx)) == 0 {
                continue;
            }
            let member = MemberId(idx as u64 + 1);
            let balance = balances.get(&member).copied().unwrap_or(Money::ZERO);
            prop_assert!(balance.is_zero());
        }
    }
}
