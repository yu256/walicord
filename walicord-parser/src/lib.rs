#![warn(clippy::uninlined_format_args)]

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, u64},
    combinator::opt,
    multi::separated_list1,
};
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration<'a> {
    pub name: &'a str,
    pub members: Vec<&'a str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Payment<'a> {
    pub amount: u64,
    pub payer: &'a str,
    pub payee: &'a str,
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

// name := member1, member2
fn declaration(input: &str) -> IResult<&str, Declaration<'_>> {
    (identifier, sp, tag(":="), sp, members_list)
        .map(|(name, _, _, _, members)| Declaration { name, members })
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
        identifier, // payer
        sp, ga, sp, identifier, // payee
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
        identifier, // payer
        sp, lent, sp, yen, // amount
        sp, to, sp, identifier, // payee
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
        identifier, // payee
        sp, ga, sp, identifier, // payer
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
    (identifier, sp, borrowed, sp, yen, sp, from, sp, identifier)
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
            Ok((_, stmt)) => {
                match &stmt {
                    Statement::Declaration(decl) => {
                        // Check if all members in the declaration are defined
                        for member in &decl.members {
                            if !defined_members.contains(member) && !defined_groups.contains(member)
                            {
                                return Err(ParseError::UndefinedMember {
                                    name: member.to_string(),
                                    line: idx + 1,
                                });
                            }
                        }
                        defined_groups.insert(decl.name);
                    }
                    Statement::Payment(p) => {
                        if !defined_members.contains(p.payee) && !defined_groups.contains(p.payee) {
                            return Err(ParseError::UndefinedMember {
                                name: p.payee.to_string(),
                                line: idx + 1,
                            });
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
