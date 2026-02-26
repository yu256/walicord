use proptest::prelude::*;
use rust_decimal::Decimal;
use std::collections::BTreeMap;
use walicord_domain::{
    BalanceError, Program, SettleUpPolicy, SettlementContext, StatementWithLine,
    model::{MemberId, MemberSetExpr, MemberSetOp, Money, Payment, Statement, Weight},
};

#[derive(Debug)]
struct WeightedPaymentInput {
    amount: u64,
    payer_idx: usize,
    payees: Vec<(usize, u64)>,
}

#[derive(Debug)]
struct EvenPaymentInput {
    amount: u64,
    payer_idx: usize,
    payee_idx: usize,
}

#[derive(Debug)]
struct SettleCaseInput {
    payments: Vec<EvenPaymentInput>,
    settle_members: Vec<MemberId>,
}

fn weighted_payee_group_strategy(member_count: usize) -> impl Strategy<Value = Vec<(usize, u64)>> {
    let max_len = member_count.min(3);
    prop::sample::subsequence((0..member_count).collect::<Vec<_>>(), 1..=max_len)
        .prop_flat_map(|member_indexes| {
            let len = member_indexes.len();
            (
                Just(member_indexes),
                prop::collection::vec(0u64..=5, len),
                0..len,
                1u64..=5,
            )
        })
        .prop_map(
            |(member_indexes, mut weights, positive_idx, positive_weight)| {
                weights[positive_idx] = positive_weight;
                member_indexes.into_iter().zip(weights).collect()
            },
        )
}

fn zero_weight_payee_group_strategy(
    member_count: usize,
) -> impl Strategy<Value = Vec<(usize, u64)>> {
    let max_len = member_count.min(3);
    prop::sample::subsequence((0..member_count).collect::<Vec<_>>(), 1..=max_len)
        .prop_map(|member_indexes| member_indexes.into_iter().map(|idx| (idx, 0)).collect())
}

fn weighted_payment_inputs_strategy() -> impl Strategy<Value = Vec<WeightedPaymentInput>> {
    (1usize..=6).prop_flat_map(|member_count| {
        (0usize..=30).prop_flat_map(move |payment_count| {
            (
                prop::collection::vec(0u64..=10_000, payment_count),
                prop::collection::vec(0usize..member_count, payment_count),
                prop::collection::vec(weighted_payee_group_strategy(member_count), payment_count),
            )
                .prop_map(move |(amounts, payer_indexes, payee_groups)| {
                    (0..payment_count)
                        .map(|idx| WeightedPaymentInput {
                            amount: amounts[idx],
                            payer_idx: payer_indexes[idx],
                            payees: payee_groups[idx].clone(),
                        })
                        .collect()
                })
        })
    })
}

fn all_zero_weight_payment_input_strategy() -> impl Strategy<Value = WeightedPaymentInput> {
    (1usize..=6).prop_flat_map(|member_count| {
        (
            0u64..=10_000,
            0usize..member_count,
            zero_weight_payee_group_strategy(member_count),
        )
            .prop_map(|(amount, payer_idx, payees)| WeightedPaymentInput {
                amount,
                payer_idx,
                payees,
            })
    })
}

fn even_payment_inputs_strategy() -> impl Strategy<Value = Vec<EvenPaymentInput>> {
    (1usize..=6).prop_flat_map(|member_count| {
        (0usize..=30).prop_flat_map(move |payment_count| {
            (
                prop::collection::vec(0u64..=10_000, payment_count),
                prop::collection::vec(0usize..member_count, payment_count),
                prop::collection::vec(0usize..member_count, payment_count),
            )
                .prop_map(move |(amounts, payer_indexes, payee_indexes)| {
                    (0..payment_count)
                        .map(|idx| EvenPaymentInput {
                            amount: amounts[idx],
                            payer_idx: payer_indexes[idx],
                            payee_idx: payee_indexes[idx],
                        })
                        .collect()
                })
        })
    })
}

fn settle_case_inputs_strategy() -> impl Strategy<Value = SettleCaseInput> {
    (1usize..=6).prop_flat_map(|member_count| {
        let members = (0..member_count).collect::<Vec<_>>();
        (0usize..=30).prop_flat_map(move |payment_count| {
            (
                prop::collection::vec(0u64..=10_000, payment_count),
                prop::collection::vec(0usize..member_count, payment_count),
                prop::collection::vec(0usize..member_count, payment_count),
                prop::sample::subsequence(members.clone(), 1..=member_count),
            )
                .prop_map(
                    move |(amounts, payer_indexes, payee_indexes, settle_indexes)| {
                        let payments = (0..payment_count)
                            .map(|idx| EvenPaymentInput {
                                amount: amounts[idx],
                                payer_idx: payer_indexes[idx],
                                payee_idx: payee_indexes[idx],
                            })
                            .collect();
                        let settle_members = settle_indexes
                            .into_iter()
                            .map(|idx| MemberId(idx as u64 + 1))
                            .collect();

                        SettleCaseInput {
                            payments,
                            settle_members,
                        }
                    },
                )
        })
    })
}

proptest! {
    #[test]
    fn balances_sum_to_zero(
        payment_inputs in even_payment_inputs_strategy(),
    ) {
        let mut statements = Vec::with_capacity(payment_inputs.len());
        for (idx, payment) in payment_inputs.iter().enumerate() {
            let amount = payment.amount;
            let payer_idx = payment.payer_idx;
            let payee_idx = payment.payee_idx;
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
        let balances = program.calculate_balances().expect("balance calculation should succeed");
        let total: Money = balances.values().sum();
        let epsilon = Decimal::new(1, 6);
        prop_assert!(total.as_decimal().abs() <= epsilon);
    }

    #[test]
    fn weighted_balances_sum_to_zero(
        payment_inputs in weighted_payment_inputs_strategy(),
    ) {
        let mut statements = Vec::with_capacity(payment_inputs.len());
        for (idx, payment_input) in payment_inputs.iter().enumerate() {
            let amount = payment_input.amount;
            let payer_idx = payment_input.payer_idx;
            let payer = MemberId(payer_idx as u64 + 1);

            let mut payee_ops = Vec::new();
            let mut payee_weights = BTreeMap::new();

            for (i, (p_idx, weight)) in payment_input.payees.iter().enumerate() {
                let payee_id = MemberId(*p_idx as u64 + 1);
                payee_ops.push(MemberSetOp::Push(payee_id));
                if i > 0 {
                    payee_ops.push(MemberSetOp::Union);
                }
                payee_weights.insert(payee_id, Weight(*weight));
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
        let balances = program.calculate_balances().expect("balance calculation should succeed");
        let total: Money = balances.values().sum();
        let epsilon = Decimal::new(1, 6);
        prop_assert!(total.as_decimal().abs() <= epsilon);
    }

    #[test]
    fn weighted_payment_with_all_zero_payee_weights_returns_zero_total_weight(
        payment_input in all_zero_weight_payment_input_strategy(),
    ) {
        let payer = MemberId(payment_input.payer_idx as u64 + 1);
        let mut payee_ops = Vec::new();
        let mut payee_weights = BTreeMap::new();

        for (i, (p_idx, weight)) in payment_input.payees.iter().enumerate() {
            let payee_id = MemberId(*p_idx as u64 + 1);
            payee_ops.push(MemberSetOp::Push(payee_id));
            if i > 0 {
                payee_ops.push(MemberSetOp::Union);
            }
            payee_weights.insert(payee_id, Weight(*weight));
        }

        let program = Program::try_new(
            vec![StatementWithLine {
                line: 1,
                statement: Statement::Payment(Payment::weighted(
                    Money::try_from(payment_input.amount).expect("amount should fit in i64"),
                    MemberSetExpr::new([MemberSetOp::Push(payer)]),
                    MemberSetExpr::new(payee_ops),
                    payee_weights,
                )),
            }],
            &[],
        )
        .expect("program build failed");

        prop_assert_eq!(program.calculate_balances(), Err(BalanceError::ZeroTotalWeight));
    }
}

proptest! {
    #[test]
    fn settle_members_balance_zero(
        case in settle_case_inputs_strategy(),
    ) {
        let SettleCaseInput {
            payments,
            settle_members,
        } = case;

        let mut statements = Vec::with_capacity(payments.len());
        for (idx, payment) in payments.iter().enumerate() {
            let amount = payment.amount;
            let payer_idx = payment.payer_idx;
            let payee_idx = payment.payee_idx;
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
        let balances = program.calculate_balances().expect("balance calculation should succeed");

        let settlement = SettleUpPolicy::settle(
            &balances,
            &settle_members,
            std::iter::empty(),
            SettlementContext::jpy_default(),
        )
        .expect("settle should succeed");
        let balances = settlement.new_balances;

        for member in settle_members {
            let balance = balances.get(&member).copied().unwrap_or(Money::ZERO);
            prop_assert!(balance.is_zero());
        }
    }
}
