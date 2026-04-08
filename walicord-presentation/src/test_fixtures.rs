use std::collections::HashMap;
use walicord_application::{MemberDirectory, PersonBalance, SettleUpContext, SettlementResult};
use walicord_domain::{Money, Transfer, model::MemberId};

pub struct TestMemberDirectory {
    pub names: HashMap<MemberId, &'static str>,
}

impl MemberDirectory for TestMemberDirectory {
    fn display_name(&self, member_id: MemberId) -> Option<&str> {
        self.names.get(&member_id).copied()
    }
}

pub fn member_directory() -> TestMemberDirectory {
    TestMemberDirectory {
        names: HashMap::from([
            (MemberId(1), "Alice"),
            (MemberId(2), "Bob"),
            (MemberId(3), "Carol"),
            (MemberId(4), "Dave"),
        ]),
    }
}

pub fn empty_member_directory() -> TestMemberDirectory {
    TestMemberDirectory {
        names: HashMap::new(),
    }
}

pub fn review_result() -> SettlementResult {
    SettlementResult {
        balances: vec![
            PersonBalance {
                id: MemberId(1),
                balance: Money::from_i64(100),
            },
            PersonBalance {
                id: MemberId(2),
                balance: Money::from_i64(-100),
            },
        ],
        optimized_transfers: vec![Transfer {
            from: MemberId(2), // debtor pays
            to: MemberId(1),   // creditor receives
            amount: Money::from_i64(100),
        }],
        settle_up: None,
        quantization_scale: 0,
        effective_cash_members: vec![MemberId(1)],
        pre_quantization_sum: Money::ZERO,
    }
}

pub fn review_incomplete_result() -> SettlementResult {
    SettlementResult {
        balances: vec![
            PersonBalance {
                id: MemberId(1),
                balance: Money::from_i64(100),
            },
            PersonBalance {
                id: MemberId(2),
                balance: Money::from_i64(-100),
            },
        ],
        optimized_transfers: vec![],
        settle_up: None,
        quantization_scale: 0,
        effective_cash_members: vec![],
        pre_quantization_sum: Money::ZERO,
    }
}

pub fn complex_review_result() -> SettlementResult {
    SettlementResult {
        balances: vec![
            PersonBalance {
                id: MemberId(1),
                balance: Money::new(12345, 2),
            },
            PersonBalance {
                id: MemberId(2),
                balance: Money::new(-5000, 2),
            },
            PersonBalance {
                id: MemberId(3),
                balance: Money::new(-3025, 2),
            },
            PersonBalance {
                id: MemberId(4),
                balance: Money::new(-2020, 2),
            },
            PersonBalance {
                id: MemberId(5),
                balance: Money::new(-2300, 2),
            },
        ],
        // Debtors (2,3,4,5) pay creditor (1)
        optimized_transfers: vec![
            Transfer {
                from: MemberId(2),
                to: MemberId(1),
                amount: Money::new(5000, 2),
            },
            Transfer {
                from: MemberId(3),
                to: MemberId(1),
                amount: Money::new(3025, 2),
            },
            Transfer {
                from: MemberId(4),
                to: MemberId(1),
                amount: Money::new(2020, 2),
            },
            Transfer {
                from: MemberId(5),
                to: MemberId(1),
                amount: Money::new(2300, 2),
            },
        ],
        settle_up: None,
        quantization_scale: 2,
        effective_cash_members: vec![MemberId(1)],
        pre_quantization_sum: Money::ZERO,
    }
}

pub fn complex_settleup_result() -> SettlementResult {
    SettlementResult {
        balances: vec![
            PersonBalance {
                id: MemberId(1),
                balance: Money::new(2500, 2),
            },
            PersonBalance {
                id: MemberId(2),
                balance: Money::new(-1000, 2),
            },
            PersonBalance {
                id: MemberId(3),
                balance: Money::new(1000, 2),
            },
            PersonBalance {
                id: MemberId(4),
                balance: Money::new(-2000, 2),
            },
            PersonBalance {
                id: MemberId(5),
                balance: Money::new(-500, 2),
            },
        ],
        optimized_transfers: vec![
            Transfer {
                from: MemberId(5), // debtor pays creditor
                to: MemberId(1),
                amount: Money::new(500, 2),
            },
            Transfer {
                from: MemberId(4), // debtor pays creditor
                to: MemberId(3),
                amount: Money::new(1000, 2),
            },
        ],
        settle_up: Some(SettleUpContext {
            settle_members: vec![MemberId(3), MemberId(5)],
            immediate_transfers: vec![Transfer {
                from: MemberId(2), // debtor pays creditor
                to: MemberId(1),
                amount: Money::new(500, 2),
            }],
        }),
        quantization_scale: 2,
        effective_cash_members: vec![MemberId(1)],
        pre_quantization_sum: Money::ZERO,
    }
}
