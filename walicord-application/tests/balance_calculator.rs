use rstest::{fixture, rstest};
use std::collections::HashMap;
use walicord_application::{
    MessageProcessor, PersonBalance, ProgramParseError, ProgramParser, Script,
    SettlementOptimizationError, SettlementOptimizer,
};
use walicord_domain::{Money, Transfer};
use walicord_infrastructure::WalicordProgramParser;

struct NoopOptimizer;

impl SettlementOptimizer for NoopOptimizer {
    fn optimize<'a>(
        &self,
        _balances: &[PersonBalance<'a>],
    ) -> Result<Vec<Transfer<'a>>, SettlementOptimizationError> {
        Ok(Vec::new())
    }
}

static TEST_PARSER: WalicordProgramParser = WalicordProgramParser;
static TEST_OPTIMIZER: NoopOptimizer = NoopOptimizer;

#[fixture]
fn processor() -> MessageProcessor<'static> {
    MessageProcessor::new(&TEST_PARSER, &TEST_OPTIMIZER)
}

fn parse_program_from_content<'a>(members: &'a [&'a str], content: &'a str) -> Script<'a> {
    let parser = WalicordProgramParser;
    match parser.parse(members, content) {
        Ok(program) => program,
        Err(err) => match err {
            ProgramParseError::MissingMembersDeclaration => {
                panic!("parse failed: missing members declaration")
            }
            ProgramParseError::UndefinedMember { name, line } => {
                panic!("parse failed: undefined member {name} at line {line}")
            }
            ProgramParseError::FailedToEvaluateGroup { name } => {
                panic!("parse failed: failed to evaluate group {name}")
            }
            ProgramParseError::SyntaxError(message) => {
                panic!("parse failed: {message}")
            }
        },
    }
}

fn assert_balances(balances: &HashMap<&str, Money>, expected: &[(&str, i64)]) {
    for (name, balance) in expected {
        assert_eq!(
            balances.get(name).map(|money| money.amount()),
            Some(*balance)
        );
    }
}

fn balances_from_result<'a>(balances: &[PersonBalance<'a>]) -> HashMap<&'a str, Money> {
    balances
        .iter()
        .map(|balance| (balance.name, balance.balance))
        .collect()
}

#[rstest]
#[case::simple_settle(
    &["A", "B"],
    "A lent 100 to B\n!settleup A",
    1,
    &[("A", 100), ("B", -100)],
    &[("A", 0), ("B", 0)],
)]
#[case::keep_others(
    &["A", "B", "C"],
    "A lent 60 to C\nB lent 100 to C\n!settleup A",
    2,
    &[("A", 60), ("B", 100), ("C", -160)],
    &[("A", 0), ("B", 100), ("C", -100)],
)]
#[case::empty_settle(
    &["A", "B"],
    "A lent 100 to B\n!settleup MEMBERS - MEMBERS",
    1,
    &[("A", 100), ("B", -100)],
    &[("A", 100), ("B", -100)],
)]
#[case::settle_all_no_transfers(
    &["A", "B"],
    "A lent 100 to B\n!settleup MEMBERS",
    1,
    &[("A", 100), ("B", -100)],
    &[("A", 0), ("B", 0)],
)]
#[case::settle_unknown_group_no_change(
    &["A", "B"],
    "A lent 100 to B\n!settleup UNKNOWN",
    1,
    &[("A", 100), ("B", -100)],
    &[("A", 100), ("B", -100)],
)]
#[case::settle_group_subset(
    &["A", "B", "C"],
    "group1 := A, B\nA lent 100 to C\n!settleup group1",
    2,
    &[("A", 100), ("B", 0), ("C", -100)],
    &[("A", 0), ("B", 0), ("C", 0)],
)]
fn settle_up_pre_and_post_balances(
    processor: MessageProcessor<'_>,
    #[case] members: &'static [&'static str],
    #[case] content: &'static str,
    #[case] prefix_len: usize,
    #[case] expected_pre: &'static [(&'static str, i64)],
    #[case] expected_post: &'static [(&'static str, i64)],
) {
    let program = parse_program_from_content(members, content);

    let pre_balances = processor.calculate_balances_for_prefix(&program, prefix_len);
    assert_balances(&pre_balances, expected_pre);

    let result = processor
        .build_settlement_result(&program)
        .expect("result generation failed");
    let post_balances = balances_from_result(&result.balances);
    assert_balances(&post_balances, expected_post);
}

#[rstest]
#[case::negative_balance(
    &["A", "B", "C"],
    "B lent 100 to A\nC lent 50 to A\n!settleup A",
    &[("A", 0)],
)]
#[case::multiple_members(
    &["A", "B", "C", "D"],
    "A lent 100 to C\nB lent 100 to C\nD lent 50 to A\n!settleup A, B",
    &[("A", 0), ("B", 0)],
)]
#[case::cross_group(
    &["A", "B", "C", "D"],
    "A lent 100 to C\nB lent 100 to D\n!settleup A, B",
    &[("A", 0), ("B", 0)],
)]
#[case::partial_within_group(
    &["A", "B", "C"],
    "A lent 100 to B\nC lent 50 to A\n!settleup A, B",
    &[("A", 0), ("B", 0)],
)]
#[case::exact_match(
    &["A", "B", "C"],
    "A lent 100 to B\nB lent 100 to C\n!settleup A, B, C",
    &[("A", 0), ("B", 0), ("C", 0)],
)]
#[case::settle_with_group_and_member(
    &["A", "B", "C"],
    "group1 := A, B\nC lent 90 to group1\n!settleup group1, C",
    &[("A", 0), ("B", 0), ("C", 0)],
)]
#[case::settle_after_multiple_payments(
    &["A", "B", "C"],
    "A lent 100 to B\nB lent 30 to C\nC lent 10 to A\n!settleup A",
    &[("A", 0)],
)]
#[case::settle_complex_set_expr(
    &["A", "B", "C", "D"],
    "A lent 100 to B\nC lent 50 to D\n!settleup (MEMBERS - A) ∪ B",
    &[("A", 0), ("B", 0), ("C", 0), ("D", 0)],
)]
fn settle_up_post_balances(
    processor: MessageProcessor<'_>,
    #[case] members: &'static [&'static str],
    #[case] content: &'static str,
    #[case] expected_post: &'static [(&'static str, i64)],
) {
    let program = parse_program_from_content(members, content);
    let result = processor
        .build_settlement_result(&program)
        .expect("result generation failed");
    let post_balances = balances_from_result(&result.balances);
    assert_balances(&post_balances, expected_post);
}

#[rstest]
#[case::remainder_distribution(
    &["A", "B", "C"],
    "A lent 100 to A, B, C",
    &[("A", 66), ("B", -33), ("C", -33)],
)]
#[case::remainder_distribution_four(
    &["A", "B", "C", "D"],
    "A lent 10 to A, B, C, D",
    &[("A", 7), ("B", -3), ("C", -2), ("D", -2)],
)]
#[case::complex_set_expr_payee(
    &["A", "B", "C"],
    "A lent 90 to (B ∪ C) - C",
    &[("A", 90), ("B", -90), ("C", 0)],
)]
#[case::fullwidth_comma_union(
    &["A", "B", "C"],
    "A lent 90 to B，C",
    &[("A", 90), ("B", -45), ("C", -45)],
)]
#[case::members_as_payee(
    &["A", "B", "C"],
    "A lent 90 to MEMBERS",
    &[("A", 60), ("B", -30), ("C", -30)],
)]
#[case::nested_set_expr_payee(
    &["A", "B", "C", "D"],
    "A lent 120 to ((A ∪ B) - A) ∪ (C ∩ MEMBERS)",
    &[("A", 120), ("B", -60), ("C", -60), ("D", 0)],
)]
#[case::fullwidth_spaces(
    &["A", "B"],
    "A　lent　100　to　B",
    &[("A", 100), ("B", -100)],
)]
#[case::japanese_lent(
    &["A", "B"],
    "A が B に 100 貸した",
    &[("A", 100), ("B", -100)],
)]
#[case::japanese_borrowed(
    &["A", "B"],
    "B が A から 100 借りた",
    &[("A", 100), ("B", -100)],
)]
#[case::same_payer_payee_group(
    &["A", "B", "C"],
    "group1 := A, B\nA lent 50 to group1",
    &[("A", 25), ("B", -25), ("C", 0)],
)]
#[case::zero_amount_no_change(
    &["A", "B"],
    "A lent 0 to B",
    &[("A", 0), ("B", 0)],
)]
#[case::single_member_no_op(
    &["A"],
    "A lent 100 to A",
    &[("A", 0)],
)]
#[case::multi_settlement_commands(
    &["A", "B", "C"],
    "A lent 60 to B\n!settleup A\nC lent 40 to B\n!settleup A",
    &[("A", 0), ("B", -40), ("C", 40)],
)]
fn payment_distribution_balances(
    processor: MessageProcessor<'_>,
    #[case] members: &'static [&'static str],
    #[case] content: &'static str,
    #[case] expected_post: &'static [(&'static str, i64)],
) {
    let program = parse_program_from_content(members, content);
    let result = processor
        .build_settlement_result(&program)
        .expect("result generation failed");
    let post_balances = balances_from_result(&result.balances);
    assert_balances(&post_balances, expected_post);
}
