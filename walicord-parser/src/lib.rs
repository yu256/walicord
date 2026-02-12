#![warn(clippy::uninlined_format_args)]

mod i18n;

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_till, take_until, take_while1},
    character::complete::{char, multispace1, u64},
    combinator::{opt, recognize},
    multi::many0,
    sequence::delimited,
};

#[derive(Debug, Clone, PartialEq)]
pub enum SetOp<'a> {
    Push(u64),          // Discord user ID (mention)
    PushGroup(&'a str), // Group name reference
    Union,
    Intersection,
    Difference,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SetExpr<'a> {
    ops: Vec<SetOp<'a>>,
}

impl<'a> SetExpr<'a> {
    pub fn new() -> Self {
        Self { ops: Vec::new() }
    }

    fn push(&mut self, op: SetOp<'a>) {
        self.ops.push(op);
    }

    pub fn ops(&self) -> &[SetOp<'a>] {
        &self.ops
    }

    pub fn referenced_ids(&self) -> impl Iterator<Item = u64> + '_ {
        self.ops.iter().filter_map(|op| match op {
            SetOp::Push(id) => Some(*id),
            _ => None,
        })
    }

    pub fn referenced_groups(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.ops.iter().filter_map(|op| match op {
            SetOp::PushGroup(name) => Some(*name),
            _ => None,
        })
    }

    pub fn to_infix_string(&self) -> Option<String> {
        let mut stack: Vec<String> = Vec::new();

        for op in &self.ops {
            match op {
                SetOp::Push(id) => stack.push(format!("<@{id}>")),
                SetOp::PushGroup(name) => stack.push((*name).to_string()),
                SetOp::Union => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(format!("({a} ∪ {b})"));
                }
                SetOp::Intersection => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(format!("({a} ∩ {b})"));
                }
                SetOp::Difference => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(format!("({a} - {b})"));
                }
            }
        }

        if stack.len() == 1 {
            let mut result = stack.pop()?;
            if is_fully_wrapped(&result) {
                result = result[1..result.len() - 1].to_string();
            }
            Some(result)
        } else {
            None
        }
    }
}

fn is_fully_wrapped(value: &str) -> bool {
    if !value.starts_with('(') || !value.ends_with(')') || value.len() <= 1 {
        return false;
    }

    let mut depth = 0;
    for (i, c) in value.char_indices() {
        if c == '(' {
            depth += 1;
        } else if c == ')' {
            depth -= 1;
            if depth == 0 && i != value.len() - 1 {
                return false;
            }
        }
    }

    depth == 0
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
pub struct Payment<'a> {
    pub amount: u64,
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
    SettleUp(SetExpr<'a>),
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

fn mention_sequence(input: &str) -> IResult<&str, SetExpr<'_>> {
    use nom::multi::many1;

    let (input, mentions) = many1((mention, sp).map(|(id, _)| id)).parse(input)?;

    let mut expr = SetExpr::new();
    let len = mentions.len();
    for id in mentions {
        expr.push(SetOp::Push(id));
    }
    for _ in 0..len - 1 {
        expr.push(SetOp::Union);
    }
    Ok((input, expr))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || is_japanese_char(c))(input)
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

fn yen(input: &str) -> IResult<&str, u64> {
    (
        opt(char('¥')),
        u64,
        opt(alt((tag("円"), tag("えん"), tag_no_case("yen")))),
    )
        .map(|(_, amount, _)| amount)
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
        sp, ga, sp, set_expr, // payee
        sp, ni, sp, yen, // amount
        sp, paid,
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
        sp, paid, sp, yen, // amount
        sp, to, sp, set_expr, // payee
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
    (yen, sp, set_expr)
        .map(|(amount, _, payee)| Payment {
            amount,
            payer: PayerSpec::Implicit,
            payee,
        })
        .parse(input)
}

// {amount} to {payee}
fn payment_implicit_with_to(input: &str) -> IResult<&str, Payment<'_>> {
    (yen, sp, to, sp, set_expr)
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
        (
            alt((tag_no_case("!settleup"), tag_no_case("!確定"))),
            sp,
            set_expr,
        )
            .map(|(_, _, expr)| Command::SettleUp(expr)),
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

    fn mention_expr(id: u64) -> SetExpr<'static> {
        let mut expr = SetExpr::new();
        expr.push(SetOp::Push(id));
        expr
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
        let (_, expr) = set_expr(input).unwrap();
        assert_eq!(&expr.ops, expected);
    }

    #[rstest]
    #[case::comma_union("<@65>, <@66> paid 1000 to <@67>")]
    #[case::mixed_union("<@65> <@66>, <@67> paid 1000 to <@68>")]
    fn test_accepts_union_separators(#[case] input: &str) {
        let result = parse_program(input);
        assert!(result.is_ok(), "Should accept union separators");
    }

    #[rstest]
    #[case::nested_intersection("(<@65> ∪ <@66>) ∩ <@67>", "(<@65> ∪ <@66>) ∩ <@67>")]
    #[case::difference_union("<@65> - <@66> ∪ <@67>", "(<@65> - <@66>) ∪ <@67>")]
    #[case::single("<@65>", "<@65>")]
    #[case::union_with_intersection("<@65> ∪ (<@66> ∩ <@67>)", "<@65> ∪ (<@66> ∩ <@67>)")]
    fn test_infix_string(#[case] input: &str, #[case] expected: &str) {
        let (_, expr) = set_expr(input).unwrap();
        let infix = expr.to_infix_string().unwrap();
        assert_eq!(infix, expected);
    }

    #[rstest]
    #[case::variables("!variables", Statement::Command(Command::Variables))]
    #[case::review_en("!review", Statement::Command(Command::Review))]
    #[case::review_ja("!清算確認", Statement::Command(Command::Review))]
    fn test_parse_simple_commands(#[case] input: &str, #[case] expected: Statement<'_>) {
        let (_, stmt) = statement(input).unwrap();
        assert_eq!(stmt, expected);
    }

    #[rstest]
    #[case::settleup_difference("!settleup <@999> - <@111>")]
    fn test_parse_kakutei_command(#[case] input: &str) {
        let (_, stmt) = statement(input).unwrap();
        let mut expected_expr = SetExpr::new();
        expected_expr.push(SetOp::Push(999));
        expected_expr.push(SetOp::Push(111));
        expected_expr.push(SetOp::Difference);
        assert_eq!(stmt, Statement::Command(Command::SettleUp(expected_expr)));
    }

    // Tests for mention-based parsing (Discord ID)
    #[rstest]
    #[case::standard("<@123456789>", 123456789)]
    #[case::nickname("<@!123456789>", 123456789)] // nickname mention
    fn test_parse_mention(#[case] input: &str, #[case] expected: u64) {
        let (_, id) = mention(input).unwrap();
        assert_eq!(id, expected);
    }

    #[rstest]
    #[case::ja("<@123> が <@456> に 1000 立て替えた")]
    #[case::en("<@123> paid 1000 to <@456>")]
    fn test_mention_based_payment(#[case] input: &str) {
        let result = payment(input);
        assert!(result.is_ok(), "Failed to parse: {input}");
        let (_, payment) = result.unwrap();
        assert_eq!(payment.amount, 1000);
        assert!(matches!(payment.payer, PayerSpec::Explicit(_)));
    }

    #[rstest]
    #[case::names_as_groups("Alice が Bob に 1000 立て替えた", "Alice", "Bob")]
    fn test_name_based_parsed_as_group_refs(
        #[case] input: &str,
        #[case] payer_name: &str,
        #[case] payee_name: &str,
    ) {
        // Names are parsed as group references, not member references
        let result = payment(input);
        assert!(result.is_ok(), "Names should parse as group references");

        let (_, payment) = result.unwrap();
        // payer should be a group reference
        let PayerSpec::Explicit(payer) = payment.payer else {
            panic!("Expected explicit payer");
        };
        assert_eq!(payer.ops, vec![SetOp::PushGroup(payer_name)]);
        // payee should be a group reference
        assert_eq!(payment.payee.ops, vec![SetOp::PushGroup(payee_name)]);
    }

    #[rstest]
    #[case::implicit_simple("1000 <@456>")]
    #[case::implicit_to("1000 to <@456>")]
    fn test_implicit_payment(#[case] input: &str) {
        let result = payment(input);
        assert!(result.is_ok(), "Failed to parse implicit payment");
        let (_, payment) = result.unwrap();
        assert_eq!(payment.amount, 1000);
        assert!(payment.is_payer_implicit());
    }

    #[rstest]
    #[case::inline_comment_payment(
        "1000円 /*ランチ*/ <@456>",
        Statement::Payment(Payment {
            amount: 1000,
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::inline_comment_paid_ja(
        "<@123> /*交通費*/ が <@456> に 1000 立て替えた",
        Statement::Payment(Payment {
            amount: 1000,
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
        Statement::Command(Command::SettleUp({
            let mut expr = SetExpr::new();
            expr.push(SetOp::Push(999));
            expr.push(SetOp::Push(111));
            expr.push(SetOp::Difference);
            expr
        }))
    )]
    #[case::inline_comment_adjacent(
        "1000円/*ランチ*/<@456>",
        Statement::Payment(Payment {
            amount: 1000,
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::inline_comment_multiple(
        "1000円 /*A*/ <@456> /*B*/",
        Statement::Payment(Payment {
            amount: 1000,
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::inline_comment_empty(
        "1000円 /**/ <@456>",
        Statement::Payment(Payment {
            amount: 1000,
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    fn test_accepts_inline_comments(#[case] input: &str, #[case] expected: Statement<'_>) {
        let program = parse_program(input).expect("Should accept inline comments");
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].statement, expected);
    }

    #[rstest]
    #[case::line_comment_trailing(
        "1000円 <@456> // ランチ",
        Statement::Payment(Payment {
            amount: 1000,
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::line_comment_adjacent(
        "1000円 <@456>// ランチ",
        Statement::Payment(Payment {
            amount: 1000,
            payer: PayerSpec::Implicit,
            payee: mention_expr(456),
        })
    )]
    #[case::line_comment_settleup(
        "!settleup <@999> // 除外",
        Statement::Command(Command::SettleUp({
            let mut expr = SetExpr::new();
            expr.push(SetOp::Push(999));
            expr
        }))
    )]
    fn test_accepts_line_comments(#[case] input: &str, #[case] expected: Statement<'_>) {
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
        let result = parse_program(input);
        assert!(result.is_ok(), "Program should parse: {result:?}");
    }
}
