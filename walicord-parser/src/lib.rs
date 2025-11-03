#![warn(clippy::uninlined_format_args)]

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, u64},
    combinator::opt,
    multi::separated_list1,
};
use std::{borrow::Cow, collections::HashSet};

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

    pub fn push(&mut self, op: SetOp<'a>) {
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
            if result.starts_with('(') && result.ends_with(')') && result.len() > 1 {
                result.pop();
                result.remove(0);
            }
            Some(result)
        } else {
            None
        }
    }

    // Evaluate the RPN expression given a function to resolve names to sets
    pub fn evaluate<'b, F>(&self, resolver: &F) -> Option<Cow<'b, HashSet<&'a str>>>
    where
        'a: 'b,
        F: Fn(&str) -> Option<&'b HashSet<&'a str>>,
    {
        let mut stack: Vec<Cow<'b, HashSet<&'a str>>> = Vec::with_capacity(self.ops.len());

        for op in &self.ops {
            match op {
                SetOp::Push(name) => {
                    let set = resolver(name)?;
                    stack.push(Cow::Borrowed(set));
                }
                SetOp::Union => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(Cow::Owned(a.union(&b).copied().collect()));
                }
                SetOp::Intersection => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(Cow::Owned(a.intersection(&b).copied().collect()));
                }
                SetOp::Difference => {
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    stack.push(Cow::Owned(a.difference(&b).copied().collect()));
                }
            }
        }

        if stack.len() == 1 { stack.pop() } else { None }
    }
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
pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Payment(Payment<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    pub members_decl: &'a [&'a str],
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
#[error("Parse error: {0}")]
pub enum ParseError {
    NoMembersDeclaration(#[from] NoMembersDeclarationError),
    #[error("Undefined member '{name}' at line {line}.")]
    UndefinedMember {
        name: String,
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

// {payer} が {payee} に {amount}貸した
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

// {payee} が {payer} から {amount}借りた
fn payment_borrower_subject_ja(input: &str) -> IResult<&str, Payment<'_>> {
    (
        set_expr, // payee
        sp, ga, sp, set_expr, // payer
        sp, from, // "から"
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

fn statement(input: &str) -> IResult<&str, Statement<'_>> {
    alt((
        declaration.map(Statement::Declaration),
        payment.map(Statement::Payment),
    ))
    .parse(input)
}

// Parse the entire program (MEMBERS declaration is provided separately)
pub fn parse_program<'a>(
    members_decl: &'a [&'a str],
    input: &'a str,
) -> Result<Program<'a>, ParseError> {
    let defined_members: HashSet<&str> = members_decl.iter().copied().collect();
    let mut defined_groups: HashSet<&str> = HashSet::new();

    let mut statements = Vec::new();

    for (idx, line) in input.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        match statement(trimmed) {
            Ok((rest, stmt)) => {
                if !rest.trim().is_empty() {
                    return Err(ParseError::SyntaxError(format!(
                        "行 {}: 構文エラー - 解析されていない入力: {}",
                        idx + 1,
                        rest.trim()
                    )));
                }
                match &stmt {
                    Statement::Declaration(decl) => {
                        // Check if all referenced names in the declaration are defined
                        for name in decl.expression.referenced_names() {
                            if name == "MEMBERS" {
                                continue;
                            }
                            if !defined_members.contains(name) && !defined_groups.contains(name) {
                                return Err(ParseError::UndefinedMember {
                                    name: name.to_string(),
                                    line: idx + 1,
                                });
                            }
                        }
                        defined_groups.insert(decl.name);
                    }
                    Statement::Payment(p) => {
                        for name in p
                            .payer
                            .referenced_names()
                            .chain(p.payee.referenced_names())
                        {
                            if name == "MEMBERS" {
                                continue;
                            }
                            if !defined_members.contains(name)
                                && !defined_groups.contains(name)
                            {
                                return Err(ParseError::UndefinedMember {
                                    name: name.to_string(),
                                    line: idx + 1,
                                });
                            }
                        }
                    }
                }
                statements.push(stmt);
            }
            Err(e) => {
                return Err(ParseError::SyntaxError(format!(
                    "行 {}: 構文エラー - {e}",
                    idx + 1
                )));
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

    #[test]
    fn test_simple_name() {
        let input = "A";
        let (_, expr) = set_expr(input).unwrap();
        assert_eq!(expr.ops, vec![SetOp::Push("A")]);
    }

    #[test]
    fn test_union() {
        let input = "A ∪ B";
        let (_, expr) = set_expr(input).unwrap();
        assert_eq!(
            expr.ops,
            vec![SetOp::Push("A"), SetOp::Push("B"), SetOp::Union]
        );
    }

    #[test]
    fn test_union_with_comma() {
        let input = "A, B";
        let (_, expr) = set_expr(input).unwrap();
        assert_eq!(
            expr.ops,
            vec![SetOp::Push("A"), SetOp::Push("B"), SetOp::Union]
        );
    }

    #[test]
    fn test_intersection() {
        let input = "A ∩ B";
        let (_, expr) = set_expr(input).unwrap();
        assert_eq!(
            expr.ops,
            vec![SetOp::Push("A"), SetOp::Push("B"), SetOp::Intersection]
        );
    }

    #[test]
    fn test_difference() {
        let input = "A - B";
        let (_, expr) = set_expr(input).unwrap();
        assert_eq!(
            expr.ops,
            vec![SetOp::Push("A"), SetOp::Push("B"), SetOp::Difference]
        );
    }

    #[test]
    fn test_complex_expression() {
        // (A ∪ B) ∩ C
        let input = "(A ∪ B) ∩ C";
        let (_, expr) = set_expr(input).unwrap();
        assert_eq!(
            expr.ops,
            vec![
                SetOp::Push("A"),
                SetOp::Push("B"),
                SetOp::Union,
                SetOp::Push("C"),
                SetOp::Intersection
            ]
        );
    }

    #[test]
    fn test_infix_string() {
        let input = "(A ∪ B) ∩ C";
        let (_, expr) = set_expr(input).unwrap();
        let infix = expr.to_infix_string().unwrap();
        assert_eq!(infix, "(A ∪ B) ∩ C");

        let input2 = "A - B ∪ C";
        let (_, expr2) = set_expr(input2).unwrap();
        let infix2 = expr2.to_infix_string().unwrap();
        assert_eq!(infix2, "(A - B) ∪ C");
    }

    #[test]
    fn test_evaluation() {
        use std::collections::HashMap;

        let mut sets = HashMap::new();
        sets.insert("A", HashSet::from(["1", "2", "3"]));
        sets.insert("B", HashSet::from(["2", "3", "4"]));
        sets.insert("C", HashSet::from(["3", "4", "5"]));

        let resolver = |name: &str| sets.get(name);

        // A ∪ B = {1, 2, 3, 4}
        let input = "A ∪ B";
        let (_, expr) = set_expr(input).unwrap();
        let result = expr.evaluate(&resolver).unwrap();
        assert_eq!(result, Cow::Owned(HashSet::from(["1", "2", "3", "4"])));

        // (A ∪ B) ∩ C = {3, 4}
        let input = "(A ∪ B) ∩ C";
        let (_, expr) = set_expr(input).unwrap();
        let result = expr.evaluate(&resolver).unwrap();
        assert_eq!(result, Cow::Owned(HashSet::from(["3", "4"])));

        // A - B = {1}
        let input = "A - B";
        let (_, expr) = set_expr(input).unwrap();
        let result = expr.evaluate(&resolver).unwrap();
        assert_eq!(result, Cow::Owned(HashSet::from(["1"])));
    }
}
