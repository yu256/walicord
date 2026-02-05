#![warn(clippy::uninlined_format_args)]

mod i18n;

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while, take_while1},
    character::complete::{char, u64},
    combinator::opt,
    multi::separated_list1,
};

#[derive(Debug, Clone, PartialEq)]
pub enum SetOp<'a> {
    Push(&'a str),
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

    pub fn referenced_names(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.ops.iter().filter_map(|op| match op {
            SetOp::Push(name) => Some(*name),
            _ => None,
        })
    }

    pub fn to_infix_string(&self) -> Option<String> {
        let mut stack: Vec<String> = Vec::new();

        for op in &self.ops {
            match op {
                SetOp::Push(name) => stack.push((*name).to_string()),
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
pub enum AmountExpr {
    Literal(u64),
    ReceiptRef { index: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Payment<'a> {
    pub amount: AmountExpr,
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
    pub members_decl: &'a [&'a str],
    pub statements: Vec<StatementWithLine<'a>>,
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
#[error("Parse error: {0}")]
pub enum ParseError<'a> {
    NoMembersDeclaration(#[from] NoMembersDeclarationError),
    #[error("Undefined member '{name}' at line {line}.")]
    UndefinedMember {
        name: &'a str,
        line: usize,
    },
    SyntaxError(String),
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
#[error("No MEMBERS declaration found in channel topic.")]
pub struct NoMembersDeclarationError;

// Parse name (identifier)
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

// Parse MEMBERS list: name1, name2, name3
fn members_list(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list1((sp, char(','), sp), identifier).parse(input)
}

// MEMBERS := name1, name2, name3
fn members_declaration(input: &str) -> IResult<&str, Vec<&str>> {
    (tag("MEMBERS"), sp, tag(":="), sp, members_list)
        .map(|(_, _, _, _, members)| members)
        .parse(input)
}

// Parse a primary expression: either an identifier or a parenthesized expression
fn set_primary(input: &str) -> IResult<&str, SetExpr<'_>> {
    alt((
        (char('('), sp, set_expr, sp, char(')')).map(|(_, _, expr, _, _)| expr),
        identifier.map(|name| {
            let mut expr = SetExpr::new();
            expr.push(SetOp::Push(name));
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
    alt((tag("∪"), tag(","), tag("，"))).parse(input)
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

fn receipt_amount(input: &str) -> IResult<&str, AmountExpr> {
    let (rest, _) = tag_no_case("receipt").parse(input)?;
    let mut rest = rest;
    let mut index = None;
    if let Some(after_hash) = rest.strip_prefix('#') {
        let digit_len = after_hash
            .as_bytes()
            .iter()
            .take_while(|b| b.is_ascii_digit())
            .count();
        if digit_len == 0 {
            return Err(nom::Err::Error(nom::error::Error::new(
                rest,
                nom::error::ErrorKind::Digit,
            )));
        }
        let digits = &after_hash[..digit_len];
        rest = &after_hash[digit_len..];
        index = Some(digits);
    }

    let index = match index {
        Some(digits) => {
            let parsed = digits.parse::<usize>().unwrap_or(0);
            if parsed == 0 {
                return Err(nom::Err::Error(nom::error::Error::new(
                    rest,
                    nom::error::ErrorKind::Digit,
                )));
            }
            parsed - 1
        }
        None => 0,
    };

    Ok((rest, AmountExpr::ReceiptRef { index }))
}

fn amount_expr(input: &str) -> IResult<&str, AmountExpr> {
    alt((receipt_amount, yen.map(AmountExpr::Literal))).parse(input)
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
        sp,
        ga,
        sp,
        set_expr, // payee
        sp,
        ni,
        sp,
        amount_expr, // amount
        sp,
        lent,
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
        sp,
        lent,
        sp,
        amount_expr, // amount
        sp,
        to,
        sp,
        set_expr, // payee
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
        sp,
        ga,
        sp,
        set_expr, // payer
        sp,
        from, // "kara" in Japanese
        sp,
        amount_expr, // amount
        sp,
        borrowed,
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
    (
        set_expr,
        sp,
        borrowed,
        sp,
        amount_expr,
        sp,
        from,
        sp,
        set_expr,
    )
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

// Parse the entire program (MEMBERS declaration is provided separately)
pub fn parse_program<'a>(
    members_decl: &'a [&'a str],
    input: &'a str,
) -> Result<Program<'a>, ParseError<'a>> {
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

    Ok(Program {
        members_decl,
        statements,
    })
}

// Extract MEMBERS declaration from the channel topic
pub fn extract_members_from_topic(topic: &str) -> Result<Vec<&str>, NoMembersDeclarationError> {
    // Find the line "MEMBERS := ..." from a multi-line topic
    for line in topic.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("MEMBERS") {
            match members_declaration(trimmed) {
                Ok((_, members)) => return Ok(members),
                Err(_) => continue,
            }
        }
    }
    Err(NoMembersDeclarationError)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("A", &[SetOp::Push("A")])]
    #[case(
        "A ∪ B",
        &[SetOp::Push("A"), SetOp::Push("B"), SetOp::Union]
    )]
    #[case(
        "A, B",
        &[SetOp::Push("A"), SetOp::Push("B"), SetOp::Union]
    )]
    #[case(
        "A ∩ B",
        &[SetOp::Push("A"), SetOp::Push("B"), SetOp::Intersection]
    )]
    #[case(
        "A - B",
        &[SetOp::Push("A"), SetOp::Push("B"), SetOp::Difference]
    )]
    #[case(
        "(A ∪ B) ∩ C",
        &[
            SetOp::Push("A"),
            SetOp::Push("B"),
            SetOp::Union,
            SetOp::Push("C"),
            SetOp::Intersection
        ]
    )]
    fn test_set_expr_ops(#[case] input: &str, #[case] expected: &'static [SetOp<'static>]) {
        let (_, expr) = set_expr(input).unwrap();
        assert_eq!(&expr.ops, expected);
    }

    #[rstest]
    #[case("(A ∪ B) ∩ C", "(A ∪ B) ∩ C")]
    #[case("A - B ∪ C", "(A - B) ∪ C")]
    #[case("A", "A")]
    #[case("A ∪ (B ∩ C)", "A ∪ (B ∩ C)")]
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
        let input = "!settleup MEMBERS - A";
        let (_, stmt) = statement(input).unwrap();
        let mut expected_expr = SetExpr::new();
        expected_expr.push(SetOp::Push("MEMBERS"));
        expected_expr.push(SetOp::Push("A"));
        expected_expr.push(SetOp::Difference);
        assert_eq!(stmt, Statement::Command(Command::SettleUp(expected_expr)));
    }

    #[test]
    fn test_parse_receipt_amount() {
        let input = "A lent receipt to B";
        let (_, stmt) = statement(input).unwrap();
        let Statement::Payment(payment) = stmt else {
            panic!("expected payment statement");
        };
        assert_eq!(payment.amount, AmountExpr::ReceiptRef { index: 0 });
    }

    #[test]
    fn test_parse_receipt_amount_index() {
        let input = "A lent receipt#2 to B";
        let (_, stmt) = statement(input).unwrap();
        let Statement::Payment(payment) = stmt else {
            panic!("expected payment statement");
        };
        assert_eq!(payment.amount, AmountExpr::ReceiptRef { index: 1 });
    }
}
