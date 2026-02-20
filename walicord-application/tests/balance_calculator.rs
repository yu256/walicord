use rstest::{fixture, rstest};
use walicord_application::{
    MessageProcessor, PersonBalance, ProgramParseError, ProgramParser, Script,
    SettlementOptimizationError, SettlementOptimizer,
};
use walicord_domain::{MemberBalances, Money, SettlementContext, Transfer, model::MemberId};
use walicord_infrastructure::WalicordProgramParser;

struct NoopOptimizer;

impl SettlementOptimizer for NoopOptimizer {
    fn optimize(
        &self,
        _balances: &[PersonBalance],
        _settle_members: &[MemberId],
        _cash_members: &[MemberId],
        _context: SettlementContext,
    ) -> Result<Vec<Transfer>, SettlementOptimizationError> {
        Ok(Vec::new())
    }
}

static TEST_PARSER: WalicordProgramParser = WalicordProgramParser;
static TEST_OPTIMIZER: NoopOptimizer = NoopOptimizer;
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum TestMember {
    Alice,
    Bob,
    Carol,
    Dave,
}

impl TestMember {
    const fn id(self) -> MemberId {
        match self {
            TestMember::Alice => MemberId(1),
            TestMember::Bob => MemberId(2),
            TestMember::Carol => MemberId(3),
            TestMember::Dave => MemberId(4),
        }
    }
}

const MEMBERS_1_2_3: [MemberId; 3] = [
    TestMember::Alice.id(),
    TestMember::Bob.id(),
    TestMember::Carol.id(),
];
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
            ProgramParseError::MissingContextForImplicitAuthor { line } => {
                panic!("parse failed: implicit author without context at line {line}")
            }
            ProgramParseError::InvalidAmountExpression { line, detail } => {
                panic!("parse failed: invalid amount expression at line {line}: {detail}")
            }
            ProgramParseError::AllZeroWeights { line } => {
                panic!("parse failed: all weights are zero at line {line}")
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

fn assert_balances(balances: &MemberBalances, expected: &[(TestMember, i64)]) {
    for (member, balance) in expected {
        assert_eq!(balances.get(&member.id()), Some(&Money::from_i64(*balance)));
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
    &[(TestMember::Alice, 100), (TestMember::Bob, -100)],
    &[(TestMember::Alice, 0), (TestMember::Bob, 0)],
)]
#[case::keep_others(
    &EMPTY_MEMBERS,
    "<@1> paid 60 to <@3>\n<@2> paid 100 to <@3>\n!settleup <@1>",
    2,
    &[
        (TestMember::Alice, 60),
        (TestMember::Bob, 100),
        (TestMember::Carol, -160),
    ],
    &[
        (TestMember::Alice, 0),
        (TestMember::Bob, 100),
        (TestMember::Carol, -100),
    ],
)]
#[case::settle_all_no_transfers(
    &EMPTY_MEMBERS,
    "all := <@1> <@2>\n<@1> paid 100 to <@2>\n!settleup all",
    2,
    &[(TestMember::Alice, 100), (TestMember::Bob, -100)],
    &[(TestMember::Alice, 0), (TestMember::Bob, 0)],
)]
#[case::empty_settle(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n!settleup <@1> - <@1>",
    1,
    &[(TestMember::Alice, 100), (TestMember::Bob, -100)],
    &[(TestMember::Alice, 100), (TestMember::Bob, -100)],
)]
#[case::settle_group_subset(
    &EMPTY_MEMBERS,
    "group1 := <@1> <@2>\n<@1> paid 100 to <@3>\n!settleup group1",
    2,
    &[
        (TestMember::Alice, 100),
        (TestMember::Bob, 0),
        (TestMember::Carol, -100),
    ],
    &[
        (TestMember::Alice, 0),
        (TestMember::Bob, 0),
        (TestMember::Carol, 0),
    ],
)]
#[tokio::test(flavor = "multi_thread")]
async fn settle_up_pre_and_post_balances(
    #[case] members: &'static [MemberId],
    #[case] content: &'static str,
    #[case] prefix_len: usize,
    #[case] expected_pre: &'static [(TestMember, i64)],
    #[case] expected_post: &'static [(TestMember, i64)],
) {
    let processor = MessageProcessor::new(&TEST_PARSER, &TEST_OPTIMIZER);
    let program = parse_program_from_content(members, content);

    let pre_balances = processor
        .calculate_balances_for_prefix(&program, prefix_len)
        .await;
    assert_balances(&pre_balances, expected_pre);

    let result = processor
        .build_settlement_result(&program)
        .await
        .expect("result generation failed");
    let post_balances = balances_from_result(&result.balances);
    assert_balances(&post_balances, expected_post);
}

#[rstest]
#[case::negative_balance(
    &EMPTY_MEMBERS,
    "<@2> paid 100 to <@1>\n<@3> paid 50 to <@1>\n!settleup <@1>",
    &[(TestMember::Alice, 0)],
)]
#[case::zero_balance_member_no_change(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n!settleup <@3>",
    &[
        (TestMember::Alice, 100),
        (TestMember::Bob, -100),
        (TestMember::Carol, 0),
    ],
)]
#[case::multiple_members(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@3>\n<@2> paid 100 to <@3>\n<@4> paid 50 to <@1>\n!settleup <@1> <@2>",
    &[(TestMember::Alice, 0), (TestMember::Bob, 0)],
)]
#[case::cross_group(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@3>\n<@2> paid 100 to <@4>\n!settleup <@1> <@2>",
    &[(TestMember::Alice, 0), (TestMember::Bob, 0)],
)]
#[case::partial_within_group(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n<@3> paid 50 to <@1>\n!settleup <@1> <@2>",
    &[(TestMember::Alice, 0), (TestMember::Bob, 0)],
)]
#[case::exact_match(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n<@2> paid 100 to <@3>\n!settleup <@1> <@2> <@3>",
    &[
        (TestMember::Alice, 0),
        (TestMember::Bob, 0),
        (TestMember::Carol, 0),
    ],
)]
#[case::settle_with_group_and_member(
    &EMPTY_MEMBERS,
    "group1 := <@1> <@2>\n<@3> paid 90 to group1\n!settleup group1 ∪ <@3>",
    &[
        (TestMember::Alice, 0),
        (TestMember::Bob, 0),
        (TestMember::Carol, 0),
    ],
)]
#[case::settle_space_separated_mentions(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n<@2> paid 50 to <@3>\n!settleup <@1> <@2> <@3>",
    &[
        (TestMember::Alice, 0),
        (TestMember::Bob, 0),
        (TestMember::Carol, 0),
    ],
)]
#[case::settle_after_multiple_payments(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@2>\n<@2> paid 30 to <@3>\n<@3> paid 10 to <@1>\n!settleup <@1>",
    &[(TestMember::Alice, 0)],
)]
#[case::settle_complex_set_expr(
    &EMPTY_MEMBERS,
    "all := <@1> <@2> <@3> <@4>\n<@1> paid 100 to <@2>\n<@3> paid 50 to <@4>\n!settleup (all - <@1>) ∪ <@2>",
    &[
        (TestMember::Alice, 0),
        (TestMember::Bob, 0),
        (TestMember::Carol, 0),
        (TestMember::Dave, 0),
    ],
)]
#[tokio::test(flavor = "multi_thread")]
async fn settle_up_post_balances(
    #[case] members: &'static [MemberId],
    #[case] content: &'static str,
    #[case] expected_post: &'static [(TestMember, i64)],
) {
    let processor = MessageProcessor::new(&TEST_PARSER, &TEST_OPTIMIZER);
    let program = parse_program_from_content(members, content);
    let result = processor
        .build_settlement_result(&program)
        .await
        .expect("result generation failed");
    let post_balances = balances_from_result(&result.balances);
    assert_balances(&post_balances, expected_post);
}

#[rstest]
#[case::remainder_distribution(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@1> <@2> <@3>",
    &[
        (TestMember::Alice, 67),
        (TestMember::Bob, -34),
        (TestMember::Carol, -33),
    ],
)]
#[case::remainder_distribution_four(
    &EMPTY_MEMBERS,
    "<@1> paid 10 to <@1> <@2> <@3> <@4>",
    &[
        (TestMember::Alice, 8),
        (TestMember::Bob, -3),
        (TestMember::Carol, -3),
        (TestMember::Dave, -2),
    ],
)]
#[case::complex_set_expr_payee(
    &EMPTY_MEMBERS,
    "<@1> paid 90 to (<@2> ∪ <@3>) - <@3>",
    &[
        (TestMember::Alice, 90),
        (TestMember::Bob, -90),
        (TestMember::Carol, 0),
    ],
)]
#[case::fullwidth_comma_union(
    &EMPTY_MEMBERS,
    "<@1> paid 90 to <@2> <@3>",
    &[
        (TestMember::Alice, 90),
        (TestMember::Bob, -45),
        (TestMember::Carol, -45),
    ],
)]
#[case::group_as_payee(
    &EMPTY_MEMBERS,
    "all := <@1> <@2> <@3>\n<@1> paid 90 to all",
    &[
        (TestMember::Alice, 60),
        (TestMember::Bob, -30),
        (TestMember::Carol, -30),
    ],
)]
#[case::members_as_payee(
    &MEMBERS_1_2_3,
    "<@1> paid 90 to MEMBERS",
    &[
        (TestMember::Alice, 60),
        (TestMember::Bob, -30),
        (TestMember::Carol, -30),
    ],
)]
#[case::nested_set_expr_payee(
    &EMPTY_MEMBERS,
    "all := <@1> <@2> <@3> <@4>\n<@1> paid 120 to ((<@1> ∪ <@2>) - <@1>) ∪ (<@3> ∩ all)",
    &[
        (TestMember::Alice, 120),
        (TestMember::Bob, -60),
        (TestMember::Carol, -60),
        (TestMember::Dave, 0),
    ],
)]
#[case::fullwidth_spaces(
    &EMPTY_MEMBERS,
    "<@1>　paid　100　to　<@2>",
    &[(TestMember::Alice, 100), (TestMember::Bob, -100)],
)]
#[case::japanese_tatekae(
    &EMPTY_MEMBERS,
    "<@1> が <@2> に 100 立て替えた",
    &[(TestMember::Alice, 100), (TestMember::Bob, -100)],
)]
#[case::japanese_tatekae_alt(
    &EMPTY_MEMBERS,
    "<@1> が <@2> に 100 たてかえた",
    &[(TestMember::Alice, 100), (TestMember::Bob, -100)],
)]
#[case::same_payer_payee_group(
    &EMPTY_MEMBERS,
    "group1 := <@1> <@2>\n<@1> paid 50 to group1",
    &[(TestMember::Alice, 25), (TestMember::Bob, -25)],
)]
#[case::zero_amount_no_change(
    &EMPTY_MEMBERS,
    "<@1> paid 0 to <@2>",
    &[(TestMember::Alice, 0), (TestMember::Bob, 0)],
)]
#[case::single_member_no_op(
    &EMPTY_MEMBERS,
    "<@1> paid 100 to <@1>",
    &[(TestMember::Alice, 0)],
)]
#[case::multi_settlement_commands(
    &EMPTY_MEMBERS,
    "<@1> paid 60 to <@2>\n!settleup <@1>\n<@3> paid 40 to <@2>\n!settleup <@1>",
    &[
        (TestMember::Alice, 0),
        (TestMember::Bob, -40),
        (TestMember::Carol, 40),
    ],
)]
#[tokio::test(flavor = "multi_thread")]
async fn payment_distribution_balances(
    #[case] members: &'static [MemberId],
    #[case] content: &'static str,
    #[case] expected_post: &'static [(TestMember, i64)],
) {
    let processor = MessageProcessor::new(&TEST_PARSER, &TEST_OPTIMIZER);
    let program = parse_program_from_content(members, content);
    let result = processor
        .build_settlement_result(&program)
        .await
        .expect("result generation failed");
    let post_balances = balances_from_result(&result.balances);
    assert_balances(&post_balances, expected_post);
}

#[test]
fn members_reference_requires_roster() {
    assert_parse_undefined_group(&EMPTY_MEMBERS, "<@1> paid 90 to MEMBERS", "MEMBERS", 1);
}
