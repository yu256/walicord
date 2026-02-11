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
const MEMBERS_1_2_3: [MemberId; 3] = [MemberId(1), MemberId(2), MemberId(3)];
const EMPTY_MEMBERS: [MemberId; 0] = [];

#[fixture]
fn processor() -> MessageProcessor<'static> {
    MessageProcessor::new(&TEST_PARSER, &TEST_OPTIMIZER)
}

fn parse_program_from_content<'a>(members: &'a [MemberId], content: &'a str) -> Script<'a> {
    let parser = WalicordProgramParser;
    match parser.parse(members, content, None) {
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
            ProgramParseError::SyntaxError { line, detail } => {
                panic!("parse failed at line {line}: {detail}")
            }
            ProgramParseError::MissingContextForImplicitPayment { line } => {
                panic!("parse failed: implicit payer without author at line {line}")
            }
        },
    }
}

fn assert_parse_undefined_group(members: &[MemberId], content: &str, name: &str, line: usize) {
    let parser = WalicordProgramParser;
    match parser.parse(members, content, None) {
        Ok(_) => panic!("expected undefined group error"),
        Err(ProgramParseError::UndefinedGroup {
            name: actual_name,
            line: actual_line,
        }) => {
            assert_eq!(actual_name.as_ref(), name);
            assert_eq!(actual_line, line);
        }
        Err(err) => panic!("unexpected parse error: {err:?}"),
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
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n!settleup <@1>",
    1,
    &[(1, 100), (2, -100)],
    &[(1, 0), (2, 0)],
)]
#[case::keep_others(
    &EMPTY_MEMBERS,
    "<@1> paid 60 to <@3>\n<@2> paid 100 to <@3>\n!settleup <@1>",
    2,
    &[(1, 60), (2, 100), (3, -160)],
    &[(1, 0), (2, 100), (3, -100)],
)]
#[case::settle_all_no_transfers(
    &EMPTY_MEMBERS,
    "all := <@1> <@2>\n<@1> paid 100 to <@2>\n!settleup all",
    2,
    &[(1, 100), (2, -100)],
    &[(1, 0), (2, 0)],
)]
#[case::empty_settle(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n!settleup <@1> - <@1>",
    1,
    &[(1, 100), (2, -100)],
    &[(1, 100), (2, -100)],
)]
#[case::settle_group_subset(
    &EMPTY_MEMBERS,
    "group1 := <@1> <@2>\n<@1> paid 100 to <@3>\n!settleup group1",
    2,
    &[(1, 100), (2, 0), (3, -100)],
    &[(1, 0), (2, 0), (3, 0)],
)]
fn settle_up_pre_and_post_balances(
    processor: MessageProcessor<'_>,
    #[case] members: &'static [MemberId],
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
    &EMPTY_MEMBERS,
    "<@2> paid 100 to <@1>\n<@3> paid 50 to <@1>\n!settleup <@1>",
    &[(1, 0)],
)]
#[case::zero_balance_member_no_change(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n!settleup <@3>",
    &[(1, 100), (2, -100), (3, 0)],
)]
#[case::multiple_members(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@3>\n<@2> paid 100 to <@3>\n<@4> paid 50 to <@1>\n!settleup <@1> <@2>",
    &[(1, 0), (2, 0)],
)]
#[case::cross_group(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@3>\n<@2> paid 100 to <@4>\n!settleup <@1> <@2>",
    &[(1, 0), (2, 0)],
)]
#[case::partial_within_group(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n<@3> paid 50 to <@1>\n!settleup <@1> <@2>",
    &[(1, 0), (2, 0)],
)]
#[case::exact_match(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n<@2> paid 100 to <@3>\n!settleup <@1> <@2> <@3>",
    &[(1, 0), (2, 0), (3, 0)],
)]
#[case::settle_with_group_and_member(
    &EMPTY_MEMBERS,
    "group1 := <@1> <@2>\n<@3> paid 90 to group1\n!settleup group1 ∪ <@3>",
    &[(1, 0), (2, 0), (3, 0)],
)]
#[case::settle_space_separated_mentions(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n<@2> paid 50 to <@3>\n!settleup <@1> <@2> <@3>",
    &[(1, 0), (2, 0), (3, 0)],
)]
#[case::settle_after_multiple_payments(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n<@2> paid 30 to <@3>\n<@3> paid 10 to <@1>\n!settleup <@1>",
    &[(1, 0)],
)]
#[case::settle_complex_set_expr(
    &EMPTY_MEMBERS,
    "all := <@1> <@2> <@3> <@4>\n<@1> paid 100 to <@2>\n<@3> paid 50 to <@4>\n!settleup (all - <@1>) ∪ <@2>",
    &[(1, 0), (2, 0), (3, 0), (4, 0)],
)]
fn settle_up_post_balances(
    processor: MessageProcessor<'_>,
    #[case] members: &'static [MemberId],
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
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@1> <@2> <@3>",
    &[(1, 66), (2, -33), (3, -33)],
)]
#[case::remainder_distribution_four(
    &EMPTY_MEMBERS,
    "<@1> paid 10 to <@1> <@2> <@3> <@4>",
    &[(1, 7), (2, -3), (3, -2), (4, -2)],
)]
#[case::complex_set_expr_payee(
    &EMPTY_MEMBERS,
    "<@1> paid 90 to (<@2> ∪ <@3>) - <@3>",
    &[(1, 90), (2, -90), (3, 0)],
)]
#[case::fullwidth_comma_union(
    &EMPTY_MEMBERS,
    "<@1> paid 90 to <@2> <@3>",
    &[(1, 90), (2, -45), (3, -45)],
)]
#[case::group_as_payee(
    &EMPTY_MEMBERS,
    "all := <@1> <@2> <@3>\n<@1> paid 90 to all",
    &[(1, 60), (2, -30), (3, -30)],
)]
#[case::members_as_payee(
    &MEMBERS_1_2_3,
    "<@1> paid 90 to MEMBERS",
    &[(1, 60), (2, -30), (3, -30)],
)]
#[case::nested_set_expr_payee(
    &EMPTY_MEMBERS,
    "all := <@1> <@2> <@3> <@4>\n<@1> paid 120 to ((<@1> ∪ <@2>) - <@1>) ∪ (<@3> ∩ all)",
    &[(1, 120), (2, -60), (3, -60), (4, 0)],
)]
#[case::fullwidth_spaces(
    &EMPTY_MEMBERS,
    "<@1>　paid　100　to　<@2>",
    &[(1, 100), (2, -100)],
)]
#[case::japanese_tatekae(
    &EMPTY_MEMBERS,
    "<@1> が <@2> に 100 立て替えた",
    &[(1, 100), (2, -100)],
)]
#[case::japanese_tatekae_alt(
    &EMPTY_MEMBERS,
    "<@1> が <@2> に 100 立て替えた",
    &[(1, 100), (2, -100)],
)]
#[case::same_payer_payee_group(
    &EMPTY_MEMBERS,
    "group1 := <@1> <@2>\n<@1> paid 50 to group1",
    &[(1, 25), (2, -25)],
)]
#[case::zero_amount_no_change(
    &EMPTY_MEMBERS,
    "<@1> paid 0 to <@2>",
    &[(1, 0), (2, 0)],
)]
#[case::single_member_no_op(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@1>",
    &[(1, 0)],
)]
#[case::multi_settlement_commands(
    &EMPTY_MEMBERS,
    "<@1> paid 60 to <@2>\n!settleup <@1>\n<@3> paid 40 to <@2>\n!settleup <@1>",
    &[(1, 0), (2, -40), (3, 40)],
)]
fn payment_distribution_balances(
    processor: MessageProcessor<'_>,
    #[case] members: &'static [MemberId],
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

#[test]
fn members_reference_requires_roster() {
    assert_parse_undefined_group(&EMPTY_MEMBERS, "<@1> paid 90 to MEMBERS", "MEMBERS", 1);
}
