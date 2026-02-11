#![warn(clippy::uninlined_format_args)]

mod i18n;

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while, take_while1},
    character::complete::{char, u64},
    combinator::opt,
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
pub struct Payment<'a> {
    pub amount: u64,
    pub payer: SetExpr<'a>,
    pub payee: SetExpr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Command<'a> {
    Variables,
    Evaluate,
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
    #[error("Syntax error: {0}")]
    SyntaxError(String),
}

// Parse Discord mention: <@123456789> or <@!123456789>
fn mention(input: &str) -> IResult<&str, u64> {
    let (input, _) = tag("<@")(input)?;
    let (input, _) = opt(char('!')).parse(input)?;
    let (input, id) = u64(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, id))
}

// Parse a sequence of space-separated mentions as a union
// e.g., "<@123> <@456> <@789>" becomes Push(123), Push(456), Push(789), Union, Union
fn mention_sequence(input: &str) -> IResult<&str, SetExpr<'_>> {
    use nom::multi::many1;
    
    let (input, mentions) = many1((mention, sp).map(|(id, _)| id)).parse(input)?;
    
    if mentions.len() == 1 {
        // Single mention, just push it
        let mut expr = SetExpr::new();
        expr.push(SetOp::Push(mentions[0]));
        Ok((input, expr))
    } else {
        // Multiple mentions, create union operations
        let mut expr = SetExpr::new();
        let len = mentions.len();
        for id in mentions {
            expr.push(SetOp::Push(id));
        }
        // Add Union operations for n-1 times (to combine n mentions)
        for _ in 0..len - 1 {
            expr.push(SetOp::Union);
        }
        Ok((input, expr))
    }
}

// Parse group name (identifier) - only for group declarations like "team := ..."
fn identifier(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || is_japanese_char(c))(input)
}

// Check for Japanese characters
fn is_japanese_char(c: char) -> bool {
    matches!(c,
        '\u{3040}'..='\u{309F}' | // Hiragana
        '\u{30A0}'..='\u{30FF}' | // Katakana
        '\u{4E00}'..='\u{9FFF}' | // Kanji
        '\u{3400}'..='\u{4DBF}'   // Kanji extension
    )
}

// Custom whitespace parser that includes full-width spaces
fn sp(input: &str) -> IResult<&str, &str> {
    take_while(|c: char| c.is_whitespace() || c == '\u{3000}')(input)
}

// Parse a primary expression: mention sequence, identifier (group name), or parenthesized expression
fn set_primary(input: &str) -> IResult<&str, SetExpr<'_>> {
    alt((
        (char('('), sp, set_expr, sp, char(')')).map(|(_, _, expr, _, _)| expr),
        // Try parsing as mention sequence (one or more space-separated Discord IDs)
        mention_sequence,
        // Fall back to identifier (group name)
        identifier.map(|name| {
            let mut expr = SetExpr::new();
            expr.push(SetOp::PushGroup(name));
            expr
        }),
    ))
    .parse(input)
}

// Parse difference operations (highest precedence after primary)
fn set_difference(input: &str) -> IResult<&str, SetExpr<'_>> {
    (
        set_primary,
        nom::multi::many0((sp, tag("-"), sp, set_primary)),
    )
        .map(|(first, ops)| {
            ops.into_iter().fold(first, |mut acc, (_, _, _, right)| {
                // Merge right's ops into acc, then add Difference op
                acc.ops.extend(right.ops);
                acc.push(SetOp::Difference);
                acc
            })
        })
        .parse(input)
}

// Parse intersection operations (middle precedence)
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
    tag("∪").parse(input)
}

// name := expression (e.g., name := (A ∪ B) ∩ C)
fn declaration(input: &str) -> IResult<&str, Declaration<'_>> {
    (identifier, sp, tag(":="), sp, set_expr)
        .map(|(name, _, _, _, expression)| Declaration { name, expression })
        .parse(input)
}

fn borrowed(input: &str) -> IResult<&str, &str> {
    alt((
        tag("かりた"),
        tag("借りた"),
        tag("borrowed"),
        tag("BORROWED"),
    ))
    .parse(input)
}

fn from(input: &str) -> IResult<&str, &str> {
    alt((tag("from"), tag("FROM"), tag("から"))).parse(input)
}

fn yen(input: &str) -> IResult<&str, u64> {
    (
        opt(char('¥')),
        u64,
        opt(alt((tag("円"), tag("えん"), tag("yen")))),
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

fn lent(input: &str) -> IResult<&str, &str> {
    alt((tag("貸した"), tag("かした"), tag("lent"), tag("LENT"))).parse(input)
}

fn to(input: &str) -> IResult<&str, &str> {
    alt((tag("to"), tag("TO"))).parse(input)
}

// {payer} ga {payee} ni {amount} lent (Japanese grammar pattern)
fn payment_lender_subject_ja(input: &str) -> IResult<&str, Payment<'_>> {
    (
        set_expr, // payer
        sp, ga, sp, set_expr, // payee
        sp, ni, sp, yen, // amount
        sp, lent,
    )
        .map(|(payer, _, _, _, payee, _, _, _, amount, _, _)| Payment {
            amount,
            payer,
            payee,
        })
        .parse(input)
}

// {payer} lent {amount} to {payee}
fn payment_lender_subject_en(input: &str) -> IResult<&str, Payment<'_>> {
    (
        set_expr, // payer
        sp, lent, sp, yen, // amount
        sp, to, sp, set_expr, // payee
    )
        .map(|(payer, _, _, _, amount, _, _, _, payee)| Payment {
            amount,
            payer,
            payee,
        })
        .parse(input)
}

// {payee} ga {payer} kara {amount} borrowed (Japanese grammar pattern)
fn payment_borrower_subject_ja(input: &str) -> IResult<&str, Payment<'_>> {
    (
        set_expr, // payee
        sp, ga, sp, set_expr, // payer
        sp, from, // "kara" in Japanese
        sp, yen, // amount
        sp, borrowed,
    )
        .map(|(payee, _, _, _, payer, _, _, _, amount, _, _)| Payment {
            amount,
            payer,
            payee,
        })
        .parse(input)
}

// payee borrowed amount from payer
fn payment_borrower_subject_en(input: &str) -> IResult<&str, Payment<'_>> {
    (set_expr, sp, borrowed, sp, yen, sp, from, sp, set_expr)
        .map(|(payee, _, _, _, amount, _, _, _, payer)| Payment {
            amount,
            payer,
            payee,
        })
        .parse(input)
}

fn payment(input: &str) -> IResult<&str, Payment<'_>> {
    alt((
        payment_lender_subject_ja,
        payment_lender_subject_en,
        payment_borrower_subject_ja,
        payment_borrower_subject_en,
    ))
    .parse(input)
}

fn command(input: &str) -> IResult<&str, Command<'_>> {
    alt((
        tag_no_case("!variables").map(|_| Command::Variables),
        tag_no_case("!evaluate").map(|_| Command::Evaluate),
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

// Parse the entire program
// NOTE: MEMBERS declaration is no longer required - members are referenced via Discord mentions
pub fn parse_program<'a>(input: &'a str) -> Result<Program<'a>, ParseError> {
    let mut statements = Vec::new();

    for (idx, line) in input.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        match statement(trimmed) {
            Ok((rest, stmt)) => {
                if !rest.trim().is_empty() {
                    return Err(ParseError::SyntaxError(i18n::line_syntax_error_unparsed(
                        idx + 1,
                        rest.trim(),
                    )));
                }
                statements.push(StatementWithLine {
                    line: idx + 1,
                    statement: stmt,
                });
            }
            Err(e) => {
                return Err(ParseError::SyntaxError(i18n::line_syntax_error(idx + 1, e)));
            }
        }
    }

    Ok(Program { statements })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("MEMBERS", &[SetOp::PushGroup("MEMBERS")])]
    #[case("<@65>", &[SetOp::Push(65)])] // 'A' = 65 in ASCII
    #[case(
        "<@65> ∪ <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Union]
    )]
    #[case(
        "<@65> <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Union]
    )]
    #[case(
        "<@65> ∩ <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Intersection]
    )]
    #[case(
        "<@65> - <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Difference]
    )]
    #[case(
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
    #[case("(<@65> ∪ <@66>) ∩ <@67>", "(<@65> ∪ <@66>) ∩ <@67>")]
    #[case("<@65> - <@66> ∪ <@67>", "(<@65> - <@66>) ∪ <@67>")]
    #[case("<@65>", "<@65>")]
    #[case("<@65> ∪ (<@66> ∩ <@67>)", "<@65> ∪ (<@66> ∩ <@67>)")]
    fn test_infix_string(#[case] input: &str, #[case] expected: &str) {
        let (_, expr) = set_expr(input).unwrap();
        let infix = expr.to_infix_string().unwrap();
        assert_eq!(infix, expected);
    }

    #[test]
    fn test_parse_variables_command() {
        let input = "!variables";
        let (_, stmt) = statement(input).unwrap();
        assert_eq!(stmt, Statement::Command(Command::Variables));
    }

    #[test]
    fn test_parse_evaluate_command() {
        let input = "!evaluate";
        let (_, stmt) = statement(input).unwrap();
        assert_eq!(stmt, Statement::Command(Command::Evaluate));
    }

    #[test]
    fn test_parse_kakutei_command() {
        let input = "!settleup <@999> - <@111>";
        let (_, stmt) = statement(input).unwrap();
        let mut expected_expr = SetExpr::new();
        expected_expr.push(SetOp::Push(999));
        expected_expr.push(SetOp::Push(111));
        expected_expr.push(SetOp::Difference);
        assert_eq!(stmt, Statement::Command(Command::SettleUp(expected_expr)));
    }

    // Tests for mention-based parsing (Discord ID)
    #[rstest]
    #[case("<@123456789>", 123456789)]
    #[case("<@!123456789>", 123456789)] // nickname mention
    fn test_parse_mention(#[case] input: &str, #[case] expected: u64) {
        let (_, id) = mention(input).unwrap();
        assert_eq!(id, expected);
    }

    #[rstest]
    #[case("<@123> が <@456> に 1000 貸した")]
    #[case("<@123> lent 1000 to <@456>")]
    fn test_mention_based_payment(#[case] input: &str) {
        let result = payment(input);
        assert!(result.is_ok(), "Failed to parse: {input}");
        let (_, payment) = result.unwrap();
        assert_eq!(payment.amount, 1000);
    }

    #[test]
    fn test_name_based_parsed_as_group_refs() {
        // Names are parsed as group references, not member references
        let result = payment("Alice が Bob に 1000 貸した");
        assert!(result.is_ok(), "Names should parse as group references");

        let (_, payment) = result.unwrap();
        // payer should be a group reference "Alice"
        assert_eq!(payment.payer.ops, vec![SetOp::PushGroup("Alice")]);
        // payee should be a group reference "Bob"
        assert_eq!(payment.payee.ops, vec![SetOp::PushGroup("Bob")]);
    }

    #[test]
    fn test_no_members_declaration() {
        // Works without a MEMBERS declaration
        let input = "<@123> が <@456> に 1000 貸した";
        let result = parse_program(input);
        assert!(result.is_ok(), "Should work without MEMBERS declaration");
    }

    #[test]
    fn test_group_definition_with_mentions() {
        // Define groups using space-separated mentions
        let input = "team := <@111> <@222>\nteam が <@333> に 1000 貸した";
        let result = parse_program(input);
        assert!(
            result.is_ok(),
            "Should support group definitions: {result:?}"
        );
    }
}
