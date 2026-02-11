use rstest::{fixture, rstest};
use walicord_application::{
    MessageProcessor, PersonBalance, ProgramParseError, ProgramParser, Script,
    SettlementOptimizationError, SettlementOptimizer,
};
use walicord_domain::{MemberBalances, Transfer, model::MemberId};
use walicord_infrastructure::WalicordProgramParser;

struct NoopOptimizer;

impl SettlementOptimizer for NoopOptimizer {
    fn optimize(
        &self,
        _balances: &[PersonBalance],
    ) -> Result<Vec<Transfer>, SettlementOptimizationError> {
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
            ProgramParseError::FailedToEvaluateGroup { name, line } => {
                panic!("parse failed: failed to evaluate group {name} at line {line}")
            }
            ProgramParseError::UndefinedGroup { name, line } => {
                panic!("parse failed: undefined group {name} at line {line}")
            }
            ProgramParseError::UndefinedMember { id, line } => {
                panic!("parse failed: undefined member <@{id}> at line {line}")
            }
            ProgramParseError::SyntaxError(message) => {
                panic!("parse failed: {message}")
            }
        },
    }
}

fn assert_balances(balances: &MemberBalances, expected: &[(u64, i64)]) {
    for (id, balance) in expected {
        assert_eq!(
            balances.get(&MemberId(*id)).map(|money| money.amount()),
            Some(*balance)
        );
    }
}

fn balances_from_result(balances: &[PersonBalance]) -> MemberBalances {
    balances
        .iter()
        .map(|balance| (balance.id, balance.balance))
        .collect()
}

#[rstest]
#[case::simple_settle(
    &[],
    "<@1> lent 100 to <@2>\n!settleup <@1>",
    1,
    &[(1, 100), (2, -100)],
    &[(1, 0), (2, 0)],
)]
#[case::keep_others(
    &[],
    "<@1> lent 60 to <@3>\n<@2> lent 100 to <@3>\n!settleup <@1>",
    2,
    &[(1, 60), (2, 100), (3, -160)],
    &[(1, 0), (2, 100), (3, -100)],
)]
#[case::settle_all_no_transfers(
    &[],
    "all := <@1>, <@2>\n<@1> lent 100 to <@2>\n!settleup all",
    2,
    &[(1, 100), (2, -100)],
    &[(1, 0), (2, 0)],
)]
#[case::empty_settle(
    &[],
    "<@1> lent 100 to <@2>\n!settleup <@1> - <@1>",
    1,
    &[(1, 100), (2, -100)],
    &[(1, 100), (2, -100)],
)]
#[case::settle_group_subset(
    &[],
    "group1 := <@1>, <@2>\n<@1> lent 100 to <@3>\n!settleup group1",
    2,
    &[(1, 100), (2, 0), (3, -100)],
    &[(1, 0), (2, 0), (3, 0)],
)]
fn settle_up_pre_and_post_balances(
    processor: MessageProcessor<'_>,
    #[case] members: &'static [&'static str],
    #[case] content: &'static str,
    #[case] prefix_len: usize,
    #[case] expected_pre: &'static [(u64, i64)],
    #[case] expected_post: &'static [(u64, i64)],
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
    &[],
    "<@2> lent 100 to <@1>\n<@3> lent 50 to <@1>\n!settleup <@1>",
    &[(1, 0)],
)]
#[case::zero_balance_member_no_change(
    &[],
    "<@1> lent 100 to <@2>\n!settleup <@3>",
    &[(1, 100), (2, -100), (3, 0)],
)]
#[case::multiple_members(
    &[],
    "<@1> lent 100 to <@3>\n<@2> lent 100 to <@3>\n<@4> lent 50 to <@1>\n!settleup <@1>, <@2>",
    &[(1, 0), (2, 0)],
)]
#[case::cross_group(
    &[],
    "<@1> lent 100 to <@3>\n<@2> lent 100 to <@4>\n!settleup <@1>, <@2>",
    &[(1, 0), (2, 0)],
)]
#[case::partial_within_group(
    &[],
    "<@1> lent 100 to <@2>\n<@3> lent 50 to <@1>\n!settleup <@1>, <@2>",
    &[(1, 0), (2, 0)],
)]
#[case::exact_match(
    &[],
    "<@1> lent 100 to <@2>\n<@2> lent 100 to <@3>\n!settleup <@1>, <@2>, <@3>",
    &[(1, 0), (2, 0), (3, 0)],
)]
#[case::settle_with_group_and_member(
    &[],
    "group1 := <@1>, <@2>\n<@3> lent 90 to group1\n!settleup group1, <@3>",
    &[(1, 0), (2, 0), (3, 0)],
)]
#[case::settle_after_multiple_payments(
    &[],
    "<@1> lent 100 to <@2>\n<@2> lent 30 to <@3>\n<@3> lent 10 to <@1>\n!settleup <@1>",
    &[(1, 0)],
)]
#[case::settle_complex_set_expr(
    &[],
    "all := <@1>, <@2>, <@3>, <@4>\n<@1> lent 100 to <@2>\n<@3> lent 50 to <@4>\n!settleup (all - <@1>) ∪ <@2>",
    &[(1, 0), (2, 0), (3, 0), (4, 0)],
)]
fn settle_up_post_balances(
    processor: MessageProcessor<'_>,
    #[case] members: &'static [&'static str],
    #[case] content: &'static str,
    #[case] expected_post: &'static [(u64, i64)],
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
    &[],
    "<@1> lent 100 to <@1>, <@2>, <@3>",
    &[(1, 66), (2, -33), (3, -33)],
)]
#[case::remainder_distribution_four(
    &[],
    "<@1> lent 10 to <@1>, <@2>, <@3>, <@4>",
    &[(1, 7), (2, -3), (3, -2), (4, -2)],
)]
#[case::complex_set_expr_payee(
    &[],
    "<@1> lent 90 to (<@2> ∪ <@3>) - <@3>",
    &[(1, 90), (2, -90), (3, 0)],
)]
#[case::fullwidth_comma_union(
    &[],
    "<@1> lent 90 to <@2>，<@3>",
    &[(1, 90), (2, -45), (3, -45)],
)]
#[case::members_as_payee(
    &[],
    "all := <@1>, <@2>, <@3>\n<@1> lent 90 to all",
    &[(1, 60), (2, -30), (3, -30)],
)]
#[case::nested_set_expr_payee(
    &[],
    "all := <@1>, <@2>, <@3>, <@4>\n<@1> lent 120 to ((<@1> ∪ <@2>) - <@1>) ∪ (<@3> ∩ all)",
    &[(1, 120), (2, -60), (3, -60), (4, 0)],
)]
#[case::fullwidth_spaces(
    &[],
    "<@1>　lent　100　to　<@2>",
    &[(1, 100), (2, -100)],
)]
#[case::japanese_lent(
    &[],
    "<@1> が <@2> に 100 貸した",
    &[(1, 100), (2, -100)],
)]
#[case::japanese_borrowed(
    &[],
    "<@2> が <@1> から 100 借りた",
    &[(1, 100), (2, -100)],
)]
#[case::same_payer_payee_group(
    &[],
    "group1 := <@1>, <@2>\n<@1> lent 50 to group1",
    &[(1, 25), (2, -25)],
)]
#[case::zero_amount_no_change(
    &[],
    "<@1> lent 0 to <@2>",
    &[(1, 0), (2, 0)],
)]
#[case::single_member_no_op(
    &[],
    "<@1> lent 100 to <@1>",
    &[(1, 0)],
)]
#[case::multi_settlement_commands(
    &[],
    "<@1> lent 60 to <@2>\n!settleup <@1>\n<@3> lent 40 to <@2>\n!settleup <@1>",
    &[(1, 0), (2, -40), (3, 40)],
)]
fn payment_distribution_balances(
    processor: MessageProcessor<'_>,
    #[case] members: &'static [&'static str],
    #[case] content: &'static str,
    #[case] expected_post: &'static [(u64, i64)],
) {
    let program = parse_program_from_content(members, content);
    let result = processor
        .build_settlement_result(&program)
        .expect("result generation failed");
    let post_balances = balances_from_result(&result.balances);
    assert_balances(&post_balances, expected_post);
}
