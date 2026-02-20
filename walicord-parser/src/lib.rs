#![warn(clippy::uninlined_format_args)]

mod i18n;

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_till, take_until, take_while, take_while1},
    character::complete::{char, digit1, multispace1, u64},
    combinator::{map_res, opt, recognize},
    multi::many0,
    sequence::delimited,
};
use rust_decimal::Decimal;
use smallvec::{SmallVec, smallvec};
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub enum SetOp<'a> {
    Push(u64),              // Discord user ID (mention)
    PushWeighted(u64, u64), // Discord user ID with weight (mention*weight)
    PushGroup(&'a str),     // Group name reference
    Union,
    Intersection,
    Difference,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SetExpr<'a> {
    ops: SmallVec<[SetOp<'a>; 3]>,
}

impl<'a> SetExpr<'a> {
    pub fn new() -> Self {
        Self {
            ops: SmallVec::new(),
        }
    }

    fn push(&mut self, op: SetOp<'a>) {
        self.ops.push(op);
    }

    pub fn ops(&self) -> &[SetOp<'a>] {
        &self.ops
    }

    pub fn referenced_ids(&self) -> impl Iterator<Item = u64> + '_ {
        self.ops.iter().filter_map(|op| match op {
            SetOp::Push(id) | SetOp::PushWeighted(id, _) => Some(*id),
            _ => None,
        })
    }

    pub fn referenced_groups(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.ops.iter().filter_map(|op| match op {
            SetOp::PushGroup(name) => Some(*name),
            _ => None,
        })
    }

    pub fn referenced_weighted(&self) -> impl Iterator<Item = (u64, u64)> + '_ {
        self.ops.iter().filter_map(|op| match op {
            SetOp::PushWeighted(id, weight) => Some((*id, *weight)),
            _ => None,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration<'a> {
    pub name: &'a str,
    pub expression: SetExpr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PayerSpec<'a> {
    Explicit(SetExpr<'a>),
    Implicit,
}

impl<'a> PayerSpec<'a> {
    pub fn is_implicit(&self) -> bool {
        matches!(self, Self::Implicit)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AmountExpr {
    ops: SmallVec<[AmountOp; 1]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AmountOp {
    Literal(Decimal),
    Add,
    Sub,
    Mul,
    Div,
}

impl AmountExpr {
    pub fn literal(value: Decimal) -> Self {
        Self {
            ops: smallvec![AmountOp::Literal(value)],
        }
    }

    pub fn ops(&self) -> &[AmountOp] {
        &self.ops
    }

    fn push_ops(&mut self, mut other: AmountExpr) {
        self.ops.append(&mut other.ops);
    }

    fn push_op(&mut self, op: AmountOp) {
        self.ops.push(op);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Payment<'a> {
    pub amount: AmountExpr,
    pub payer: PayerSpec<'a>,
    pub payee: SetExpr<'a>,
}

impl<'a> Payment<'a> {
    pub fn is_payer_implicit(&self) -> bool {
        self.payer.is_implicit()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Command<'a> {
    Variables,
    Review,
    CashSelf,
    MemberAddCash {
        members: SetExpr<'a>,
    },
    SettleUp {
        members: SetExpr<'a>,
        cash_members: Option<SetExpr<'a>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Payment(Payment<'a>),
    Command(Command<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StatementWithLine<'a> {
    pub line: usize,
    pub statement: Statement<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    pub statements: Vec<StatementWithLine<'a>>,
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum ParseError {
    #[error("Undefined member ID '<@{id}>' at line {line}.")]
    UndefinedMember { id: u64, line: usize },
    #[error("Undefined group '{name}' at line {line}.")]
    UndefinedGroup { name: String, line: usize },
    #[error("Syntax error at line {line}: {detail}")]
    SyntaxError { line: usize, detail: String },
}

fn mention(input: &str) -> IResult<&str, u64> {
    let (input, _) = tag("<@")(input)?;
    let (input, _) = opt(char('!')).parse(input)?;
    let (input, id) = u64(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, id))
}

fn mention_with_weight(input: &str) -> IResult<&str, SetOp<'static>> {
    let (input, id) = mention(input)?;
    let (input, weight) = opt((char('*'), u64).map(|(_, w)| w)).parse(input)?;
    let op = match weight {
        Some(w) => SetOp::PushWeighted(id, w),
        None => SetOp::Push(id),
    };
    Ok((input, op))
}

fn mention_sequence(input: &str) -> IResult<&str, SetExpr<'_>> {
    use nom::multi::many1;

    let (input, ops) = many1((mention_with_weight, sp).map(|(op, _)| op)).parse(input)?;

    let mut expr = SetExpr::new();
    let len = ops.len();
    for op in ops {
        expr.push(op);
    }
    for _ in 0..len - 1 {
        expr.push(SetOp::Union);
    }
    Ok((input, expr))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize((
        take_while1(|c: char| c.is_alphanumeric() || c == '_' || is_japanese_char(c)),
        take_while(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || is_japanese_char(c)),
    ))
    .parse(input)
}

fn is_japanese_char(c: char) -> bool {
    matches!(c,
        '\u{3040}'..='\u{309F}' | // Hiragana
        '\u{30A0}'..='\u{30FF}' | // Katakana
        '\u{4E00}'..='\u{9FFF}' | // Kanji
        '\u{3400}'..='\u{4DBF}'   // Kanji extension
    )
}

fn sp(input: &str) -> IResult<&str, &str> {
    fn fullwidth_space(input: &str) -> IResult<&str, &str> {
        take_while1(|c: char| c == '\u{3000}')(input)
    }

    fn comment(input: &str) -> IResult<&str, &str> {
        delimited(tag("/*"), take_until("*/"), tag("*/")).parse(input)
    }

    fn line_comment(input: &str) -> IResult<&str, &str> {
        recognize((tag("//"), take_till(|c| c == '\n'))).parse(input)
    }

    recognize(many0(alt((
        multispace1,
        fullwidth_space,
        comment,
        line_comment,
    ))))
    .parse(input)
}

fn set_primary(input: &str) -> IResult<&str, SetExpr<'_>> {
    alt((
        (char('('), sp, set_expr, sp, char(')')).map(|(_, _, expr, _, _)| expr),
        mention_sequence,
        identifier.map(|name| {
            let mut expr = SetExpr::new();
            expr.push(SetOp::PushGroup(name));
            expr
        }),
    ))
    .parse(input)
}

fn set_difference(input: &str) -> IResult<&str, SetExpr<'_>> {
    (
        set_primary,
        nom::multi::many0((sp, tag("-"), sp, set_primary)),
    )
        .map(|(first, ops)| {
            ops.into_iter().fold(first, |mut acc, (_, _, _, right)| {
                acc.ops.extend(right.ops);
                acc.push(SetOp::Difference);
                acc
            })
        })
        .parse(input)
}

fn set_intersection(input: &str) -> IResult<&str, SetExpr<'_>> {
    (
        set_difference,
        nom::multi::many0((sp, tag("∩"), sp, set_difference)),
    )
        .map(|(first, ops)| {
            ops.into_iter().fold(first, |mut acc, (_, _, _, right)| {
                acc.ops.extend(right.ops);
                acc.push(SetOp::Intersection);
                acc
            })
        })
        .parse(input)
}

// Parse union operations (lowest precedence)
fn set_expr(input: &str) -> IResult<&str, SetExpr<'_>> {
    (
        set_intersection,
        nom::multi::many0((sp, union_token, sp, set_intersection)),
    )
        .map(|(first, ops)| {
            ops.into_iter().fold(first, |mut acc, (_, _, _, right)| {
                acc.ops.extend(right.ops);
                acc.push(SetOp::Union);
                acc
            })
        })
        .parse(input)
}

fn union_token(input: &str) -> IResult<&str, &str> {
    alt((tag("∪"), tag(","), tag("，"))).parse(input)
}

// name := expression (e.g., name := (A ∪ B) ∩ C)
fn declaration(input: &str) -> IResult<&str, Declaration<'_>> {
    (identifier, sp, tag(":="), sp, set_expr)
        .map(|(name, _, _, _, expression)| Declaration { name, expression })
        .parse(input)
}

fn paid(input: &str) -> IResult<&str, &str> {
    alt((tag("立て替えた"), tag("たてかえた"), tag_no_case("paid"))).parse(input)
}

fn decimal_literal(input: &str) -> IResult<&str, Decimal> {
    map_res(
        recognize((digit1, opt((char('.'), digit1)))),
        Decimal::from_str,
    )
    .parse(input)
}

fn amount_literal(input: &str) -> IResult<&str, AmountExpr> {
    (
        opt(char('¥')),
        decimal_literal,
        opt(alt((tag("円"), tag("えん"), tag_no_case("yen")))),
    )
        .map(|(_, amount, _)| AmountExpr::literal(amount))
        .parse(input)
}

fn amount_primary(input: &str) -> IResult<&str, AmountExpr> {
    alt((
        (char('('), sp, amount_expr, sp, char(')')).map(|(_, _, value, _, _)| value),
        amount_literal,
    ))
    .parse(input)
}

fn amount_term(input: &str) -> IResult<&str, AmountExpr> {
    (
        amount_primary,
        many0((sp, alt((char('*'), char('/'))), sp, amount_primary)),
    )
        .map(|(first, rest)| {
            rest.into_iter().fold(first, |mut acc, (_, op, _, rhs)| {
                acc.push_ops(rhs);
                acc.push_op(match op {
                    '*' => AmountOp::Mul,
                    '/' => AmountOp::Div,
                    _ => unreachable!("parser should only produce * or /"),
                });
                acc
            })
        })
        .parse(input)
}

fn amount_expr(input: &str) -> IResult<&str, AmountExpr> {
    (
        amount_term,
        many0((sp, alt((char('+'), char('-'))), sp, amount_term)),
    )
        .map(|(first, rest)| {
            rest.into_iter().fold(first, |mut acc, (_, op, _, rhs)| {
                acc.push_ops(rhs);
                acc.push_op(match op {
                    '+' => AmountOp::Add,
                    '-' => AmountOp::Sub,
                    _ => {
                        unreachable!("parser should only produce + or -")
                    }
                });
                acc
            })
        })
        .parse(input)
}

fn ga(input: &str) -> IResult<&str, &str> {
    tag("が").parse(input)
}

fn ni(input: &str) -> IResult<&str, &str> {
    tag("に").parse(input)
}

fn to(input: &str) -> IResult<&str, &str> {
    tag_no_case("to").parse(input)
}

// {payer} ga {payee} ni {amount} paid (Japanese grammar pattern)
fn payment_lender_subject_ja(input: &str) -> IResult<&str, Payment<'_>> {
    (
        set_expr, // payer
        sp,
        ga,
        sp,
        set_expr, // payee
        sp,
        ni,
        sp,
        amount_expr, // amount
        sp,
        paid,
    )
        .map(|(payer, _, _, _, payee, _, _, _, amount, _, _)| Payment {
            amount,
            payer: PayerSpec::Explicit(payer),
            payee,
        })
        .parse(input)
}

// {payer} paid {amount} to {payee}
fn payment_lender_subject_en(input: &str) -> IResult<&str, Payment<'_>> {
    (
        set_expr, // payer
        sp,
        paid,
        sp,
        amount_expr, // amount
        sp,
        to,
        sp,
        set_expr, // payee
    )
        .map(|(payer, _, _, _, amount, _, _, _, payee)| Payment {
            amount,
            payer: PayerSpec::Explicit(payer),
            payee,
        })
        .parse(input)
}

// {amount} {payee}
fn payment_implicit_simple(input: &str) -> IResult<&str, Payment<'_>> {
    (amount_expr, sp, set_expr)
        .map(|(amount, _, payee)| Payment {
            amount,
            payer: PayerSpec::Implicit,
            payee,
        })
        .parse(input)
}

// {amount} to {payee}
fn payment_implicit_with_to(input: &str) -> IResult<&str, Payment<'_>> {
    (amount_expr, sp, to, sp, set_expr)
        .map(|(amount, _, _, _, payee)| Payment {
            amount,
            payer: PayerSpec::Implicit,
            payee,
        })
        .parse(input)
}

fn payment(input: &str) -> IResult<&str, Payment<'_>> {
    alt((
        payment_lender_subject_ja,
        payment_lender_subject_en,
        payment_implicit_with_to,
        payment_implicit_simple,
    ))
    .parse(input)
}

fn command(input: &str) -> IResult<&str, Command<'_>> {
    alt((
        tag_no_case("!variables").map(|_| Command::Variables),
        tag_no_case("!review").map(|_| Command::Review),
        tag("!清算確認").map(|_| Command::Review),
        tag_no_case("!cash").map(|_| Command::CashSelf),
        (
            tag_no_case("!member"),
            sp,
            tag_no_case("set"),
            sp,
            set_expr,
            sp,
            tag_no_case("cash"),
        )
            .map(|(_, _, _, _, members, _, _)| Command::MemberAddCash { members }),
        (
            alt((tag_no_case("!settleup"), tag_no_case("!確定"))),
            sp,
            set_expr,
            opt((sp, tag_no_case("--cash"), sp, set_expr)),
        )
            .map(|(_, _, members, cash)| Command::SettleUp {
                members,
                cash_members: cash.map(|(_, _, _, set)| set),
            }),
    ))
    .parse(input)
}

fn statement(input: &str) -> IResult<&str, Statement<'_>> {
    alt((
        declaration.map(Statement::Declaration),
        payment.map(Statement::Payment),
        command.map(Statement::Command),
    ))
    .parse(input)
}

fn statement_with_sp(input: &str) -> IResult<&str, Statement<'_>> {
    (sp, statement, sp).map(|(_, stmt, _)| stmt).parse(input)
}

// Parse the entire program
// NOTE: MEMBERS declaration is no longer required - members are referenced via Discord mentions
pub fn parse_program<'a>(input: &'a str) -> Result<Program<'a>, ParseError> {
    let mut statements = Vec::new();

    for (idx, line) in input.lines().enumerate() {
        let (rest, _) = sp(line).map_err(|e| ParseError::SyntaxError {
            line: idx + 1,
            detail: i18n::syntax_error_detail(e),
        })?;
        if rest.trim().is_empty() {
            continue;
        }
        match statement_with_sp(rest) {
            Ok((rest, stmt)) => {
                if !rest.trim().is_empty() {
                    return Err(ParseError::SyntaxError {
                        line: idx + 1,
                        detail: i18n::syntax_error_unparsed_detail(rest.trim()),
                    });
                }
                statements.push(StatementWithLine {
                    line: idx + 1,
                    statement: stmt,
                });
            }
            Err(e) => {
                return Err(ParseError::SyntaxError {
                    line: idx + 1,
                    detail: i18n::syntax_error_detail(e),
                });
            }
        }
    }

    Ok(Program { statements })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use std::str::FromStr;

    fn mention_expr(id: u64) -> SetExpr<'static> {
        let mut expr = SetExpr::new();
        expr.push(SetOp::Push(id));
        expr
    }

    fn eval_amount(expr: &AmountExpr) -> Option<Decimal> {
        let mut stack: Vec<Decimal> = Vec::with_capacity(expr.ops().len());
        for op in expr.ops() {
            match op {
                AmountOp::Literal(value) => stack.push(*value),
                AmountOp::Add => apply_bin(&mut stack, |a, b| a.checked_add(b))?,
                AmountOp::Sub => apply_bin(&mut stack, |a, b| a.checked_sub(b))?,
                AmountOp::Mul => apply_bin(&mut stack, |a, b| a.checked_mul(b))?,
                AmountOp::Div => apply_bin(&mut stack, |a, b| a.checked_div(b))?,
            }
        }
        if stack.len() == 1 { stack.pop() } else { None }
    }

    fn apply_bin(
        stack: &mut Vec<Decimal>,
        op: fn(Decimal, Decimal) -> Option<Decimal>,
    ) -> Option<()> {
        let rhs = stack.pop()?;
        let lhs = stack.pop()?;
        stack.push(op(lhs, rhs)?);
        Some(())
    }

    #[rstest]
    #[case::members("MEMBERS", &[SetOp::PushGroup("MEMBERS")])]
    #[case::mention("<@65>", &[SetOp::Push(65)])] // 'A' = 65 in ASCII
    #[case::union_symbol(
        "<@65> ∪ <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Union]
    )]
    #[case::union_space(
        "<@65> <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Union]
    )]
    #[case::intersection(
        "<@65> ∩ <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Intersection]
    )]
    #[case::difference(
        "<@65> - <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Difference]
    )]
    #[case::nested(
        "(<@65> ∪ <@66>) ∩ <@67>",
        &[
            SetOp::Push(65),
            SetOp::Push(66),
            SetOp::Union,
            SetOp::Push(67),
            SetOp::Intersection
        ]
    )]
    fn test_set_expr_ops(#[case] input: &str, #[case] expected: &'static [SetOp]) {
        let (_, expr) = set_expr(input).expect("set expression should parse");
        assert_eq!(expr.ops(), expected);
    }

    #[rstest]
    #[case::comma_union("<@65>, <@66> paid 1000 to <@67>")]
    #[case::mixed_union("<@65> <@66>, <@67> paid 1000 to <@68>")]
    fn test_accepts_union_separators(#[case] input: &str) {
        let program = parse_program(input).expect("program with union separators should parse");
        assert!(!program.statements.is_empty());
    }

    #[rstest]
    #[case::variables("!variables", Statement::Command(Command::Variables))]
    #[case::review_en("!review", Statement::Command(Command::Review))]
    #[case::review_ja("!清算確認", Statement::Command(Command::Review))]
    fn test_parse_simple_commands<'a>(#[case] input: &'a str, #[case] expected: Statement<'a>) {
        let (_, stmt) = statement(input).expect("simple command should parse");
        assert_eq!(stmt, expected);
    }

    #[rstest]
    #[case::settleup_difference("!settleup <@999> - <@111>")]
    #[case::kakutei_difference("!確定 <@999> - <@111>")]
    fn test_parse_kakutei_command(#[case] input: &str) {
        let (_, stmt) = statement(input).expect("settleup command should parse");
        let mut expected_expr = SetExpr::new();
        expected_expr.push(SetOp::Push(999));
        expected_expr.push(SetOp::Push(111));
        expected_expr.push(SetOp::Difference);
        assert_eq!(
            stmt,
            Statement::Command(Command::SettleUp {
                members: expected_expr,
                cash_members: None,
            })
        );
    }

    #[rstest]
    #[case::settleup_alias("!settleup <@1> <@2> --cash <@3> ∪ <@4>")]
    #[case::kakutei_alias("!確定 <@1> <@2> --cash <@3> ∪ <@4>")]
    fn test_parse_settleup_with_cash_option(#[case] input: &str) {
        let (_, stmt) = statement(input).expect("settleup with cash should parse");

        let mut members = SetExpr::new();
        members.push(SetOp::Push(1));
        members.push(SetOp::Push(2));
        members.push(SetOp::Union);

        let mut cash = SetExpr::new();
        cash.push(SetOp::Push(3));
        cash.push(SetOp::Push(4));
        cash.push(SetOp::Union);

        assert_eq!(
            stmt,
            Statement::Command(Command::SettleUp {
                members,
                cash_members: Some(cash),
            })
        );
    }

    #[test]
    fn test_parse_member_cash_command() {
        let (_, stmt) =
            statement("!member set <@10> ∪ <@11> cash").expect("member cash command should parse");

        let mut members = SetExpr::new();
        members.push(SetOp::Push(10));
        members.push(SetOp::Push(11));
        members.push(SetOp::Union);

        assert_eq!(stmt, Statement::Command(Command::MemberAddCash { members }));
    }

    #[test]
    fn test_parse_cash_self_command() {
        let (_, stmt) = statement("!cash").expect("cash command should parse");
        assert_eq!(stmt, Statement::Command(Command::CashSelf));
    }

    #[rstest]
    #[case::member_cash_invalid_state("!member set <@1> cash maybe")]
    #[case::member_cash_legacy_on("!member set <@1> cash on")]
    #[case::member_cash_legacy_off("!member set <@1> cash off")]
    #[case::settleup_cash_missing_expr("!settleup <@1> --cash")]
    fn test_reject_invalid_command_syntax(#[case] input: &str) {
        let result = parse_program(input);
        assert!(matches!(result, Err(ParseError::SyntaxError { .. })));
    }

    #[rstest]
    #[case::identifier_internal_hyphen("team-alpha := <@1>", "team-alpha")]
    #[case::identifier_underscore_prefix("_ops-team := <@1>", "_ops-team")]
    fn test_declaration_identifier_accepts_valid_patterns(
        #[case] input: &str,
        #[case] expected_name: &str,
    ) {
        let program = parse_program(input).expect("valid declaration should parse");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0].statement {
            Statement::Declaration(decl) => assert_eq!(decl.name, expected_name),
            _ => panic!("expected declaration statement"),
        }
    }

    #[rstest]
    #[case::leading_hyphen("-team := <@1>")]
    #[case::hyphen_only("- := <@1>")]
    fn test_declaration_identifier_rejects_invalid_patterns(#[case] input: &str) {
        let result = parse_program(input);
        assert!(matches!(result, Err(ParseError::SyntaxError { .. })));
    }

    // Tests for mention-based parsing (Discord ID)
    #[rstest]
    #[case::standard("<@123456789>", 123456789)]
    #[case::nickname("<@!123456789>", 123456789)] // nickname mention
    fn test_parse_mention(#[case] input: &str, #[case] expected: u64) {
        let (_, id) = mention(input).expect("mention should parse");
        assert_eq!(id, expected);
    }

    #[rstest]
    #[case::ja("<@123> が <@456> に 1000 立て替えた")]
    #[case::en("<@123> paid 1000 to <@456>")]
    fn test_mention_based_payment(#[case] input: &str) {
        let (_, payment) = payment(input).expect("mention-based payment should parse");
        assert_eq!(eval_amount(&payment.amount), Some(Decimal::from(1000)));
        assert!(matches!(payment.payer, PayerSpec::Explicit(_)));
    }

    #[rstest]
    #[case::sum("100+200", "300")]
    #[case::mul("100*3", "300")]
    #[case::mixed("100+200*3", "700")]
    #[case::paren("(100+200)*3", "900")]
    #[case::yen_literals("100円+200円", "300")]
    #[case::spaces("100 + 200", "300")]
    #[case::decimal("10.5+0.5", "11.0")]
    fn test_amount_expr(#[case] input: &str, #[case] expected: &str) {
        let (rest, amount) = amount_expr(input).expect("amount expression should parse");
        assert!(rest.is_empty());
        let expected = Decimal::from_str(expected).expect("expected decimal literal");
        assert_eq!(eval_amount(&amount), Some(expected));
    }

    #[rstest]
    #[case::trailing_op("10+")]
    #[case::missing_paren("(10+20")]
    fn test_amount_expr_rejects_invalid_syntax(#[case] input: &str) {
        let result = amount_expr(input);
        assert!(
            result.is_err() || matches!(result, Ok((rest, _)) if !rest.is_empty()),
            "invalid syntax must not fully parse"
        );
    }

    #[rstest]
    #[case::names_as_groups("Alice が Bob に 1000 立て替えた", "Alice", "Bob")]
    fn test_name_based_parsed_as_group_refs(
        #[case] input: &str,
        #[case] payer_name: &str,
        #[case] payee_name: &str,
    ) {
        let (_, payment) = payment(input).expect("name-based payment should parse");
        let PayerSpec::Explicit(payer) = payment.payer else {
            panic!("payer should be explicit");
        };
        assert_eq!(payer.ops(), &[SetOp::PushGroup(payer_name)]);
        assert_eq!(payment.payee.ops(), &[SetOp::PushGroup(payee_name)]);
    }

    #[rstest]
    #[case::implicit_simple("1000 <@456>")]
    #[case::implicit_to("1000 to <@456>")]
    fn test_implicit_payment(#[case] input: &str) {
        let (_, payment) = payment(input).expect("implicit payment should parse");
        assert_eq!(eval_amount(&payment.amount), Some(Decimal::from(1000)));
        assert!(payment.is_payer_implicit());
    }

    #[rstest]
    #[case::inline_comment_payment(
        "1000円 /*ランチ*/ <@456>",
        Statement::Payment(Payment {
            amount: AmountExpr::literal(Decimal::from(1000)),
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::inline_comment_paid_ja(
        "<@123> /*交通費*/ が <@456> に 1000 立て替えた",
        Statement::Payment(Payment {
            amount: AmountExpr::literal(Decimal::from(1000)),
            payer: PayerSpec::Explicit(mention_expr(123)),
            payee: mention_expr(456),
        })
    )]
    #[case::inline_comment_declaration(
        "team := <@111> /*主担当*/ <@222>",
        Statement::Declaration(Declaration {
            name: "team",
            expression: {
                let mut expr = SetExpr::new();
                expr.push(SetOp::Push(111));
                expr.push(SetOp::Push(222));
                expr.push(SetOp::Union);
                expr
            },
        })
    )]
    #[case::inline_comment_settleup(
        "!settleup /*除外*/ <@999> - <@111>",
        Statement::Command(Command::SettleUp {
            members: {
            let mut expr = SetExpr::new();
            expr.push(SetOp::Push(999));
            expr.push(SetOp::Push(111));
            expr.push(SetOp::Difference);
            expr
            },
            cash_members: None,
        })
    )]
    #[case::inline_comment_member_cash(
        "!member set <@10> /*メモ*/ cash",
        Statement::Command(Command::MemberAddCash {
            members: {
            let mut expr = SetExpr::new();
            expr.push(SetOp::Push(10));
            expr
            },
        })
    )]
    #[case::inline_comment_adjacent(
        "1000円/*ランチ*/<@456>",
        Statement::Payment(Payment {
            amount: AmountExpr::literal(Decimal::from(1000)),
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::inline_comment_multiple(
        "1000円 /*A*/ <@456> /*B*/",
        Statement::Payment(Payment {
            amount: AmountExpr::literal(Decimal::from(1000)),
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::inline_comment_empty(
        "1000円 /**/ <@456>",
        Statement::Payment(Payment {
            amount: AmountExpr::literal(Decimal::from(1000)),
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    fn test_accepts_inline_comments(
        #[case] input: &'static str,
        #[case] expected: Statement<'static>,
    ) {
        let program = parse_program(input).expect("Should accept inline comments");
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].statement, expected);
    }

    #[rstest]
    #[case::line_comment_trailing(
        "1000円 <@456> // ランチ",
        Statement::Payment(Payment {
            amount: AmountExpr::literal(Decimal::from(1000)),
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::line_comment_adjacent(
        "1000円 <@456>// ランチ",
        Statement::Payment(Payment {
            amount: AmountExpr::literal(Decimal::from(1000)),
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::line_comment_settleup(
        "!settleup <@999> // 除外",
        Statement::Command(Command::SettleUp {
            members: {
            let mut expr = SetExpr::new();
            expr.push(SetOp::Push(999));
            expr
            },
            cash_members: None,
        })
    )]
    fn test_accepts_line_comments(
        #[case] input: &'static str,
        #[case] expected: Statement<'static>,
    ) {
        let program = parse_program(input).expect("Should accept line comments");
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].statement, expected);
    }

    #[rstest]
    #[case::line_comment_only("// メモ")]
    #[case::line_comment_leading_spaces("   // メモ")]
    fn test_line_comment_only_is_ignored(#[case] input: &str) {
        let program = parse_program(input).expect("Should accept line comments");
        assert!(program.statements.is_empty());
    }

    #[rstest]
    #[case::unterminated_inline("1000円 /*ランチ <@456>")]
    #[case::unterminated_multiline("1000円 /*メモ\n<@456>")]
    fn test_unterminated_comment_returns_error(#[case] input: &str) {
        let result = parse_program(input);
        assert!(matches!(result, Err(ParseError::SyntaxError { .. })));
    }

    #[rstest]
    #[case::single_payment("<@123> が <@456> に 1000 立て替えた")]
    #[case::declaration_and_payment("team := <@111> <@222>\nteam が <@333> に 1000 立て替えた")]
    fn test_parse_program_variants(#[case] input: &str) {
        let program = parse_program(input).expect("program variant should parse");
        assert!(!program.statements.is_empty());
    }

    #[rstest]
    #[case::single_weighted(
        "<@65>*2",
        &[SetOp::PushWeighted(65, 2)]
    )]
    #[case::weighted_and_unweighted(
        "<@65>*2 <@66>",
        &[SetOp::PushWeighted(65, 2), SetOp::Push(66), SetOp::Union]
    )]
    #[case::multiple_weighted(
        "<@65>*3 <@66>*0 <@67>",
        &[SetOp::PushWeighted(65, 3), SetOp::PushWeighted(66, 0), SetOp::Push(67), SetOp::Union, SetOp::Union]
    )]
    fn test_weighted_set_expr_ops(#[case] input: &str, #[case] expected: &'static [SetOp]) {
        let (_, expr) = set_expr(input).expect("weighted set expression should parse");
        assert_eq!(expr.ops(), expected);
    }

    #[rstest]
    #[case::weighted_payment_implicit("3000 <@65>*2 <@66>*0 <@67>")]
    #[case::weighted_payment_to("3000 to <@65>*2 <@66>*0 <@67>")]
    #[case::weighted_payment_ja("<@10> が <@65>*2 <@66>*0 <@67> に 3000 立て替えた")]
    #[case::weighted_payment_en("<@10> paid 3000 to <@65>*2 <@66>*0 <@67>")]
    fn test_weighted_payment_parses(#[case] input: &str) {
        let program = parse_program(input).expect("weighted payment should parse");
        assert!(!program.statements.is_empty());
        let stmt = &program.statements[0].statement;
        let Statement::Payment(payment) = stmt else {
            panic!("expected payment statement");
        };
        let payee = &payment.payee;
        let weighted: Vec<_> = payee.referenced_weighted().collect();
        assert_eq!(weighted, vec![(65, 2), (66, 0)]);
    }

    #[rstest]
    #[case::no_weight("<@65>", &[SetOp::Push(65)])]
    fn test_unweighted_backward_compat(#[case] input: &str, #[case] expected: &'static [SetOp]) {
        let (_, expr) = set_expr(input).expect("unweighted should still parse");
        assert_eq!(expr.ops(), expected);
    }
}
