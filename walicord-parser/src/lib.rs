#![warn(clippy::uninlined_format_args)]

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_till, take_until, take_while, take_while1},
    character::complete::{char, digit1, multispace1, u64},
    combinator::{cut, map_res, opt, recognize},
    error::{ContextError, ErrorKind, ParseError as NomParseError, context},
    multi::many0,
    sequence::delimited,
};
use rust_decimal::Decimal;
use smallvec::{SmallVec, smallvec};
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SetOp<'a> {
    Push(u64),                       // Discord user ID (mention)
    PushWeighted(u64, u64),          // Discord user ID with weight (mention*weight)
    PushRole(u64),                   // Discord role ID (role mention)
    PushWeightedRole(u64, u64),      // Discord role ID with weight (role mention*weight)
    PushGroup(&'a str),              // Group name reference
    PushWeightedGroup(&'a str, u64), // Group name reference with weight (group*weight)
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
            SetOp::PushGroup(name) | SetOp::PushWeightedGroup(name, _) => Some(*name),
            _ => None,
        })
    }

    pub fn referenced_roles(&self) -> impl Iterator<Item = u64> + '_ {
        self.ops.iter().filter_map(|op| match op {
            SetOp::PushRole(id) | SetOp::PushWeightedRole(id, _) => Some(*id),
            _ => None,
        })
    }

    pub fn referenced_weighted(&self) -> impl Iterator<Item = (u64, u64)> + '_ {
        self.ops.iter().filter_map(|op| match op {
            SetOp::PushWeighted(id, weight) => Some((*id, *weight)),
            _ => None,
        })
    }

    /// Returns true if there are any unweighted push operations (Push without weight).
    pub fn has_unweighted_push(&self) -> bool {
        self.ops.iter().any(|op| matches!(op, SetOp::Push(_)))
    }

    /// Returns true if there are any runtime-resolved references.
    /// Group and role references (both weighted and unweighted) resolve to members at runtime.
    /// Unweighted references may contribute default weight 1 members, so parser-side
    /// all-zero checks must treat them as runtime-dependent.
    pub fn has_group_reference(&self) -> bool {
        self.ops.iter().any(|op| {
            matches!(
                op,
                SetOp::PushGroup(_)
                    | SetOp::PushWeightedGroup(_, _)
                    | SetOp::PushRole(_)
                    | SetOp::PushWeightedRole(_, _)
            )
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedElement {
    Amount,
    MemberOrGroup,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxErrorKind {
    ParseFailure {
        attempted_form: Option<&'static str>,
        expected: ExpectedElement,
        near: String,
    },
    TrailingInput {
        text: String,
    },
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum ParseError {
    #[error("Undefined member ID '<@{id}>' at line {line}.")]
    UndefinedMember { id: u64, line: usize },
    #[error("Undefined group '{name}' at line {line}.")]
    UndefinedGroup { name: String, line: usize },
    #[error("Syntax error at line {line}: {kind:?}")]
    SyntaxError { line: usize, kind: SyntaxErrorKind },
}

// `context` sets the outermost "form" label.
// `with_expected` sets the innermost "expected element" label.
#[derive(Debug)]
struct NomSyntaxError<'a> {
    input: &'a str,
    expected: Option<&'static str>,
    form: Option<&'static str>,
}

impl<'a> NomParseError<&'a str> for NomSyntaxError<'a> {
    fn from_error_kind(input: &'a str, _kind: ErrorKind) -> Self {
        NomSyntaxError {
            input,
            expected: None,
            form: None,
        }
    }

    fn append(_input: &'a str, _kind: ErrorKind, other: Self) -> Self {
        other
    }

    fn or(self, other: Self) -> Self {
        if self.input.len() <= other.input.len() {
            self
        } else {
            other
        }
    }
}

impl<'a> ContextError<&'a str> for NomSyntaxError<'a> {
    fn add_context(_input: &'a str, ctx: &'static str, mut other: Self) -> Self {
        other.form = Some(ctx);
        other
    }
}

impl<'a, E> nom::error::FromExternalError<&'a str, E> for NomSyntaxError<'a> {
    fn from_external_error(input: &'a str, _kind: ErrorKind, _e: E) -> Self {
        NomSyntaxError {
            input,
            expected: None,
            form: None,
        }
    }
}

fn with_expected<'a, O, P>(
    label: &'static str,
    mut parser: P,
) -> impl FnMut(&'a str) -> PResult<'a, O>
where
    P: FnMut(&'a str) -> PResult<'a, O>,
{
    move |input| {
        parser(input).map_err(|e| match e {
            nom::Err::Error(mut err) => {
                if err.expected.is_none() {
                    err.expected = Some(label);
                }
                nom::Err::Error(err)
            }
            nom::Err::Failure(mut err) => {
                if err.expected.is_none() {
                    err.expected = Some(label);
                }
                nom::Err::Failure(err)
            }
            other => other,
        })
    }
}

type PResult<'a, T> = IResult<&'a str, T, NomSyntaxError<'a>>;

fn mention(input: &str) -> PResult<'_, u64> {
    let (input, _) = tag("<@")(input)?;
    let (input, _) = opt(char('!')).parse(input)?;
    let (input, id) = u64(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, id))
}

fn role_mention(input: &str) -> PResult<'_, u64> {
    let (input, _) = tag("<@&")(input)?;
    let (input, id) = u64(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, id))
}

fn mention_with_weight(input: &str) -> PResult<'_, SetOp<'_>> {
    let (input, id) = mention(input)?;
    let (input, weight) = opt((char('*'), u64).map(|(_, w)| w)).parse(input)?;
    let op = match weight {
        Some(w) => SetOp::PushWeighted(id, w),
        None => SetOp::Push(id),
    };
    Ok((input, op))
}

fn role_mention_with_weight(input: &str) -> PResult<'_, SetOp<'_>> {
    let (input, id) = role_mention(input)?;
    let (input, weight) = opt((char('*'), u64).map(|(_, w)| w)).parse(input)?;
    let op = match weight {
        Some(w) => SetOp::PushWeightedRole(id, w),
        None => SetOp::PushRole(id),
    };
    Ok((input, op))
}

fn everyone_mention(input: &str) -> PResult<'_, SetOp<'_>> {
    let (input, _) = tag_no_case("@everyone")(input)?;
    Ok((input, SetOp::PushGroup("MEMBERS")))
}

fn everyone_mention_with_weight(input: &str) -> PResult<'_, SetOp<'_>> {
    let (input, _) = tag_no_case("@everyone")(input)?;
    let (input, weight) = opt((char('*'), u64).map(|(_, w)| w)).parse(input)?;
    let op = match weight {
        Some(w) => SetOp::PushWeightedGroup("MEMBERS", w),
        None => SetOp::PushGroup("MEMBERS"),
    };
    Ok((input, op))
}

fn mention_or_role(input: &str) -> PResult<'_, SetOp<'_>> {
    alt((
        mention.map(SetOp::Push),
        role_mention.map(SetOp::PushRole),
        everyone_mention,
    ))
    .parse(input)
}

fn mention_or_role_weighted(input: &str) -> PResult<'_, SetOp<'_>> {
    alt((
        mention_with_weight,
        role_mention_with_weight,
        everyone_mention_with_weight,
    ))
    .parse(input)
}

fn mention_sequence_generic<'a, P>(parser: P, input: &'a str) -> PResult<'a, SetExpr<'a>>
where
    P: Fn(&'a str) -> PResult<'a, SetOp<'a>>,
{
    use nom::multi::many1;

    let (input, ops) = many1((parser, sp).map(|(op, _)| op)).parse(input)?;

    let mut expr = SetExpr::new();
    let len = ops.len();
    expr.ops.extend(ops);
    expr.ops.extend(std::iter::repeat_n(SetOp::Union, len - 1));
    Ok((input, expr))
}

fn mention_sequence(input: &str) -> PResult<'_, SetExpr<'_>> {
    mention_sequence_generic(mention_or_role, input)
}

fn mention_sequence_weighted(input: &str) -> PResult<'_, SetExpr<'_>> {
    mention_sequence_generic(mention_or_role_weighted, input)
}

fn is_reserved_keyword(word: &str) -> bool {
    paid_keyword(word).is_ok_and(|(rest, _)| rest.is_empty())
        || to_keyword(word).is_ok_and(|(rest, _)| rest.is_empty())
        || ga(word).is_ok_and(|(rest, _)| rest.is_empty())
        || ni(word).is_ok_and(|(rest, _)| rest.is_empty())
}

fn identifier(input: &str) -> PResult<'_, &str> {
    let (rest, ident) = recognize((
        take_while1(|c: char| c.is_alphanumeric() || c == '_' || is_japanese_char(c)),
        take_while(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || is_japanese_char(c)),
    ))
    .parse(input)?;
    if is_reserved_keyword(ident) {
        return Err(nom::Err::Error(NomSyntaxError::from_error_kind(
            input,
            ErrorKind::Tag,
        )));
    }
    Ok((rest, ident))
}

fn is_japanese_char(c: char) -> bool {
    matches!(c,
        '\u{3040}'..='\u{309F}' | // Hiragana
        '\u{30A0}'..='\u{30FF}' | // Katakana
        '\u{4E00}'..='\u{9FFF}' | // Kanji
        '\u{3400}'..='\u{4DBF}'   // Kanji extension
    )
}

fn sp(input: &str) -> PResult<'_, &str> {
    fn fullwidth_space(input: &str) -> PResult<'_, &str> {
        take_while1(|c: char| c == '\u{3000}')(input)
    }

    fn comment(input: &str) -> PResult<'_, &str> {
        delimited(tag("/*"), take_until("*/"), tag("*/")).parse(input)
    }

    fn line_comment(input: &str) -> PResult<'_, &str> {
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

fn set_primary(input: &str) -> PResult<'_, SetExpr<'_>> {
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

fn set_primary_weighted(input: &str) -> PResult<'_, SetExpr<'_>> {
    fn identifier_with_weight(input: &str) -> PResult<'_, SetExpr<'_>> {
        let (input, name) = identifier(input)?;
        let (input, weight) = opt((char('*'), u64).map(|(_, w)| w)).parse(input)?;
        let mut expr = SetExpr::new();
        match weight {
            Some(w) => expr.push(SetOp::PushWeightedGroup(name, w)),
            None => expr.push(SetOp::PushGroup(name)),
        }
        Ok((input, expr))
    }

    alt((
        (char('('), sp, set_expr_weighted, sp, char(')')).map(|(_, _, expr, _, _)| expr),
        mention_sequence_weighted,
        identifier_with_weight,
    ))
    .parse(input)
}

fn diff_token(input: &str) -> PResult<'_, &str> {
    tag("-")(input)
}

fn intersect_token(input: &str) -> PResult<'_, &str> {
    tag("∩")(input)
}

fn union_token(input: &str) -> PResult<'_, &str> {
    alt((tag("∪"), tag(","), tag("，"))).parse(input)
}

fn set_op_expr<'a>(
    primary: fn(&'a str) -> PResult<'a, SetExpr<'a>>,
    op_token: fn(&'a str) -> PResult<'a, &'a str>,
    set_op: SetOp<'a>,
    input: &'a str,
) -> PResult<'a, SetExpr<'a>> {
    (primary, nom::multi::many0((sp, op_token, sp, primary)))
        .map(|(first, ops)| {
            ops.into_iter().fold(first, |mut acc, (_, _, _, right)| {
                acc.ops.extend(right.ops);
                acc.push(set_op);
                acc
            })
        })
        .parse(input)
}

fn set_difference(input: &str) -> PResult<'_, SetExpr<'_>> {
    set_op_expr(set_primary, diff_token, SetOp::Difference, input)
}

fn set_difference_weighted(input: &str) -> PResult<'_, SetExpr<'_>> {
    set_op_expr(set_primary_weighted, diff_token, SetOp::Difference, input)
}

fn set_intersection(input: &str) -> PResult<'_, SetExpr<'_>> {
    set_op_expr(set_difference, intersect_token, SetOp::Intersection, input)
}

fn set_intersection_weighted(input: &str) -> PResult<'_, SetExpr<'_>> {
    set_op_expr(
        set_difference_weighted,
        intersect_token,
        SetOp::Intersection,
        input,
    )
}

fn set_expr(input: &str) -> PResult<'_, SetExpr<'_>> {
    set_op_expr(set_intersection, union_token, SetOp::Union, input)
}

fn set_expr_weighted(input: &str) -> PResult<'_, SetExpr<'_>> {
    set_op_expr(set_intersection_weighted, union_token, SetOp::Union, input)
}

// name := expression (e.g., name := (A ∪ B) ∩ C)
fn declaration(input: &str) -> PResult<'_, Declaration<'_>> {
    let (input, name) = identifier(input)?;
    let (input, _) = sp(input)?;
    let (input, _) = tag(":=")(input)?;
    let (input, _) = sp(input)?;
    let (input, expression) = cut(context(
        "<NAME> := <SET>",
        with_expected("member/group", set_expr),
    ))
    .parse(input)?;
    Ok((input, Declaration { name, expression }))
}

fn is_word_boundary_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-' || is_japanese_char(c)
}

fn paid_keyword(input: &str) -> PResult<'_, &str> {
    let (rest, matched) =
        alt((tag("立て替えた"), tag("たてかえた"), tag_no_case("paid"))).parse(input)?;
    if rest.starts_with(is_word_boundary_char) {
        return Err(nom::Err::Error(NomSyntaxError::from_error_kind(
            input,
            ErrorKind::Tag,
        )));
    }
    Ok((rest, matched))
}

fn to_keyword(input: &str) -> PResult<'_, &str> {
    let (rest, matched) = tag_no_case("to")(input)?;
    if rest.starts_with(is_word_boundary_char) {
        return Err(nom::Err::Error(NomSyntaxError::from_error_kind(
            input,
            ErrorKind::Tag,
        )));
    }
    Ok((rest, matched))
}

fn decimal_literal(input: &str) -> PResult<'_, Decimal> {
    map_res(
        recognize((digit1, opt((char('.'), digit1)))),
        Decimal::from_str,
    )
    .parse(input)
}

fn amount_literal(input: &str) -> PResult<'_, AmountExpr> {
    (
        opt(char('¥')),
        decimal_literal,
        opt(alt((tag("円"), tag("えん"), tag_no_case("yen")))),
    )
        .map(|(_, amount, _)| AmountExpr::literal(amount))
        .parse(input)
}

fn amount_primary(input: &str) -> PResult<'_, AmountExpr> {
    alt((
        (char('('), sp, amount_expr, sp, char(')')).map(|(_, _, value, _, _)| value),
        amount_literal,
    ))
    .parse(input)
}

fn amount_term(input: &str) -> PResult<'_, AmountExpr> {
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

fn amount_expr(input: &str) -> PResult<'_, AmountExpr> {
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

fn ga(input: &str) -> PResult<'_, &str> {
    tag("が").parse(input)
}

fn ni(input: &str) -> PResult<'_, &str> {
    tag("に").parse(input)
}

// {payer} ga {payee} ni {amount} paid (Japanese grammar pattern)
fn payment_lender_subject_ja(input: &str) -> PResult<'_, Payment<'_>> {
    let (input, payer) = set_expr(input)?;
    let (input, _) = sp(input)?;
    let (input, _) = ga(input)?;
    let (input, _) = sp(input)?;
    let (input, (payee, _, _, _, amount, _, _)) = cut(context(
        "<PAYER> が <PAYEE> に <AMOUNT> 立て替えた",
        (
            with_expected("member/group", set_expr_weighted),
            sp,
            ni,
            sp,
            with_expected("amount", amount_expr),
            sp,
            paid_keyword,
        ),
    ))
    .parse(input)?;
    Ok((
        input,
        Payment {
            amount,
            payer: PayerSpec::Explicit(payer),
            payee,
        },
    ))
}

// {payer} paid {amount} to {payee}
fn payment_lender_subject_en(input: &str) -> PResult<'_, Payment<'_>> {
    let (input, payer) = set_expr(input)?;
    let (input, _) = sp(input)?;
    let (input, _) = paid_keyword(input)?;
    let (input, _) = sp(input)?;
    let (input, (amount, _, _, _, payee)) = cut(context(
        "<PAYER> paid <AMOUNT> to <PAYEE>",
        (
            with_expected("amount", amount_expr),
            sp,
            to_keyword,
            sp,
            with_expected("member/group", set_expr_weighted),
        ),
    ))
    .parse(input)?;
    Ok((
        input,
        Payment {
            amount,
            payer: PayerSpec::Explicit(payer),
            payee,
        },
    ))
}

// {amount} {payee}
fn payment_implicit_simple(input: &str) -> PResult<'_, Payment<'_>> {
    (amount_expr, sp, set_expr_weighted)
        .map(|(amount, _, payee)| Payment {
            amount,
            payer: PayerSpec::Implicit,
            payee,
        })
        .parse(input)
}

// {amount} to {payee}
fn payment_implicit_with_to(input: &str) -> PResult<'_, Payment<'_>> {
    let (input, amount) = amount_expr(input)?;
    let (input, _) = sp(input)?;
    let (input, _) = to_keyword(input)?;
    let (input, _) = sp(input)?;
    let (input, payee) = cut(context(
        "<AMOUNT> to <PAYEE>",
        with_expected("member/group", set_expr_weighted),
    ))
    .parse(input)?;
    Ok((
        input,
        Payment {
            amount,
            payer: PayerSpec::Implicit,
            payee,
        },
    ))
}

fn payment(input: &str) -> PResult<'_, Payment<'_>> {
    alt((
        payment_lender_subject_ja,
        payment_lender_subject_en,
        payment_implicit_with_to,
        payment_implicit_simple,
    ))
    .parse(input)
}

fn command(input: &str) -> PResult<'_, Command<'_>> {
    alt((
        tag_no_case("!variables").map(|_| Command::Variables),
        tag_no_case("!review").map(|_| Command::Review),
        tag("!清算確認").map(|_| Command::Review),
        tag_no_case("!cash").map(|_| Command::CashSelf),
        |input| {
            let (input, _) = tag_no_case("!member")(input)?;
            let (input, _) = sp(input)?;
            let (input, (_, _, members, _, _)) = cut(context(
                "!command",
                (
                    tag_no_case("set"),
                    sp,
                    with_expected("member/group", set_expr),
                    sp,
                    tag_no_case("cash"),
                ),
            ))
            .parse(input)?;
            Ok((input, Command::MemberAddCash { members }))
        },
        |input| {
            let (input, _) = alt((tag_no_case("!settleup"), tag_no_case("!確定"))).parse(input)?;
            let (input, _) = sp(input)?;
            let (input, (members, cash)) = cut(context(
                "!command",
                (
                    with_expected("member/group", set_expr),
                    opt((
                        sp,
                        tag_no_case("--cash"),
                        sp,
                        with_expected("member/group", set_expr),
                    )),
                ),
            ))
            .parse(input)?;
            Ok((
                input,
                Command::SettleUp {
                    members,
                    cash_members: cash.map(|(_, _, _, set)| set),
                },
            ))
        },
    ))
    .parse(input)
}

fn statement(input: &str) -> PResult<'_, Statement<'_>> {
    alt((
        declaration.map(Statement::Declaration),
        payment.map(Statement::Payment),
        command.map(Statement::Command),
    ))
    .parse(input)
}

fn statement_with_sp(input: &str) -> PResult<'_, Statement<'_>> {
    (sp, statement, sp).map(|(_, stmt, _)| stmt).parse(input)
}

fn truncate_str(s: &str, max_bytes: usize) -> &str {
    if s.len() <= max_bytes {
        return s;
    }
    let mut end = max_bytes;
    while end > 0 && !s.is_char_boundary(end) {
        end -= 1;
    }
    &s[..end]
}

fn expected_label_to_element(label: Option<&str>) -> ExpectedElement {
    match label {
        Some("amount") => ExpectedElement::Amount,
        Some("member/group") => ExpectedElement::MemberOrGroup,
        _ => ExpectedElement::Unknown,
    }
}

fn extract_syntax_error(err: nom::Err<NomSyntaxError<'_>>) -> SyntaxErrorKind {
    const NEAR_MAX_BYTES: usize = 30;

    fn extract(e: NomSyntaxError<'_>) -> SyntaxErrorKind {
        SyntaxErrorKind::ParseFailure {
            attempted_form: e.form,
            expected: expected_label_to_element(e.expected),
            near: truncate_str(e.input.trim(), NEAR_MAX_BYTES).to_string(),
        }
    }

    match err {
        nom::Err::Failure(e) => extract(e),
        nom::Err::Error(e) => extract(e),
        nom::Err::Incomplete(_) => SyntaxErrorKind::ParseFailure {
            attempted_form: None,
            expected: ExpectedElement::Unknown,
            near: String::new(),
        },
    }
}

// Parse the entire program
// NOTE: MEMBERS declaration is no longer required - members are referenced via Discord mentions
pub fn parse_program<'a>(input: &'a str) -> Result<Program<'a>, ParseError> {
    let mut statements = Vec::new();

    for (idx, line) in input.lines().enumerate() {
        let (rest, _) = sp(line).map_err(|e| ParseError::SyntaxError {
            line: idx + 1,
            kind: extract_syntax_error(e),
        })?;
        if rest.trim().is_empty() {
            continue;
        }
        match statement_with_sp(rest) {
            Ok((rest, stmt)) => {
                if !rest.trim().is_empty() {
                    return Err(ParseError::SyntaxError {
                        line: idx + 1,
                        kind: SyntaxErrorKind::TrailingInput {
                            text: truncate_str(rest.trim(), 30).to_string(),
                        },
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
                    kind: extract_syntax_error(e),
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
    #[case::role("<@&42>", &[SetOp::PushRole(42)])]
    #[case::union_symbol(
        "<@65> ∪ <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Union]
    )]
    #[case::union_space(
        "<@65> <@66>",
        &[SetOp::Push(65), SetOp::Push(66), SetOp::Union]
    )]
    #[case::union_space_with_role(
        "<@&42> <@66>",
        &[SetOp::PushRole(42), SetOp::Push(66), SetOp::Union]
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
    #[case::smallest_nonzero("<@&1>", 1)]
    #[case::basic("<@&987654321>", 987654321)]
    fn test_parse_role_mention(#[case] input: &str, #[case] expected: u64) {
        let (_, id) = role_mention(input).expect("role mention should parse");
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
    #[case::role_and_weighted_member(
        "<@&70> <@65>*2",
        &[SetOp::PushRole(70), SetOp::PushWeighted(65, 2), SetOp::Union]
    )]
    #[case::weighted_role(
        "<@&70>*2 <@65>",
        &[SetOp::PushWeightedRole(70, 2), SetOp::Push(65), SetOp::Union]
    )]
    #[case::weighted_group(
        "team*3, <@65>",
        &[SetOp::PushWeightedGroup("team", 3), SetOp::Push(65), SetOp::Union]
    )]
    fn test_weighted_set_expr_ops(#[case] input: &str, #[case] expected: &'static [SetOp]) {
        let (_, expr) = set_expr_weighted(input).expect("weighted set expression should parse");
        assert_eq!(expr.ops(), expected);
    }

    #[rstest]
    #[case::weighted_payment_implicit(
        "3000 <@65>*2 <@66>*0 <@67>",
        vec![(65, 2), (66, 0)]
    )]
    #[case::weighted_payment_to(
        "3000 to <@65>*2 <@66>*0 <@67>",
        vec![(65, 2), (66, 0)]
    )]
    #[case::weighted_payment_ja(
        "<@10> が <@65>*2 <@66>*0 <@67> に 3000 立て替えた",
        vec![(65, 2), (66, 0)]
    )]
    #[case::weighted_payment_en(
        "<@10> paid 3000 to <@65>*2 <@66>*0 <@67>",
        vec![(65, 2), (66, 0)]
    )]
    #[case::weighted_with_role("3000 to <@&70> <@65>*2", vec![(65, 2)])]
    #[case::weighted_with_weighted_role("3000 to <@&70>*2 <@65>", vec![])]
    #[case::weighted_with_weighted_group("3000 to team*2, <@65>", vec![])]
    fn test_weighted_payment_parses(#[case] input: &str, #[case] expected: Vec<(u64, u64)>) {
        let program = parse_program(input).expect("weighted payment should parse");
        assert!(!program.statements.is_empty());
        let stmt = &program.statements[0].statement;
        let Statement::Payment(payment) = stmt else {
            panic!("expected payment statement");
        };
        let payee = &payment.payee;
        let weighted: Vec<_> = payee.referenced_weighted().collect();
        assert_eq!(weighted, expected);
    }

    #[test]
    fn test_weighted_payment_with_role_includes_role_reference() {
        let program = parse_program("3000 to <@&70> <@65>*2")
            .expect("weighted payment with role should parse");
        let Statement::Payment(payment) = &program.statements[0].statement else {
            panic!("expected payment statement");
        };

        let roles: Vec<_> = payment.payee.referenced_roles().collect();
        assert_eq!(roles, vec![70]);
    }

    #[test]
    fn test_weighted_payment_with_weighted_group_includes_group_reference() {
        let program = parse_program("3000 to team*2, <@65>")
            .expect("weighted payment with group should parse");
        let Statement::Payment(payment) = &program.statements[0].statement else {
            panic!("expected payment statement");
        };

        let groups: Vec<_> = payment.payee.referenced_groups().collect();
        assert_eq!(groups, vec!["team"]);
    }

    #[test]
    fn test_unweighted_backward_compat() {
        let (_, expr) = set_expr("<@65>").expect("unweighted should still parse");
        assert_eq!(expr.ops(), &[SetOp::Push(65)]);
    }

    #[rstest]
    #[case::declaration("team := <@1>*2 <@2>")]
    #[case::settleup_members("!settleup <@1>*2 <@2>")]
    #[case::settleup_cash("!settleup <@1> --cash <@2>*2")]
    #[case::member_cash("!member set <@1>*2 cash")]
    #[case::weighted_payer("<@10>*2 paid 3000 to <@20>")]
    fn test_reject_weighted_outside_payment_payee(#[case] input: &str) {
        let result = parse_program(input);
        assert!(matches!(result, Err(ParseError::SyntaxError { .. })));
    }

    #[test]
    fn test_accept_weighted_role_mention_in_payee() {
        let result = parse_program("3000 to <@&20>*2");
        assert!(result.is_ok());
    }

    #[test]
    fn test_accept_weighted_group_reference_in_payee() {
        let result = parse_program("3000 to team*2");
        assert!(result.is_ok());
    }

    // --- @everyone mention tests ---

    #[rstest]
    #[case::lowercase("@everyone", &[SetOp::PushGroup("MEMBERS")])]
    #[case::uppercase("@Everyone", &[SetOp::PushGroup("MEMBERS")])]
    #[case::allcaps("@EVERYONE", &[SetOp::PushGroup("MEMBERS")])]
    fn test_everyone_in_set_expr(#[case] input: &str, #[case] expected: &[SetOp]) {
        let (_, expr) = set_expr(input).expect("@everyone should parse as set expression");
        assert_eq!(expr.ops(), expected);
    }

    #[test]
    fn test_everyone_with_weight() {
        let (_, expr) =
            set_expr_weighted("@everyone*2").expect("@everyone with weight should parse");
        assert_eq!(expr.ops(), &[SetOp::PushWeightedGroup("MEMBERS", 2)]);
    }

    #[test]
    fn test_everyone_in_difference() {
        let (_, expr) = set_expr("@everyone - <@123>").expect("@everyone - mention should parse");
        assert_eq!(
            expr.ops(),
            &[
                SetOp::PushGroup("MEMBERS"),
                SetOp::Push(123),
                SetOp::Difference,
            ]
        );
    }

    #[test]
    fn test_everyone_as_payee() {
        let program = parse_program("1000 <@10> @everyone").expect("payment to @everyone");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0].statement {
            Statement::Payment(p) => {
                let groups: Vec<_> = p.payee.referenced_groups().collect();
                assert_eq!(groups, vec!["MEMBERS"]);
            }
            _ => panic!("expected payment statement"),
        }
    }

    #[test]
    fn test_everyone_as_explicit_payer_ja() {
        let program = parse_program("@everyone が <@20> に 1000 立て替えた")
            .expect("payment from @everyone (ja)");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0].statement {
            Statement::Payment(p) => match &p.payer {
                PayerSpec::Explicit(expr) => {
                    let groups: Vec<_> = expr.referenced_groups().collect();
                    assert_eq!(groups, vec!["MEMBERS"]);
                }
                PayerSpec::Implicit => panic!("expected explicit payer"),
            },
            _ => panic!("expected payment statement"),
        }
    }

    #[test]
    fn test_everyone_as_explicit_payer_en() {
        let program =
            parse_program("@everyone paid 1000 to <@20>").expect("payment from @everyone (en)");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0].statement {
            Statement::Payment(p) => match &p.payer {
                PayerSpec::Explicit(expr) => {
                    let groups: Vec<_> = expr.referenced_groups().collect();
                    assert_eq!(groups, vec!["MEMBERS"]);
                }
                PayerSpec::Implicit => panic!("expected explicit payer"),
            },
            _ => panic!("expected payment statement"),
        }
    }

    #[test]
    fn test_everyone_in_declaration() {
        let program =
            parse_program("team := @everyone - <@99>").expect("declaration with @everyone");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0].statement {
            Statement::Declaration(d) => {
                assert_eq!(d.name, "team");
                assert_eq!(
                    d.expression.ops(),
                    &[
                        SetOp::PushGroup("MEMBERS"),
                        SetOp::Push(99),
                        SetOp::Difference,
                    ]
                );
            }
            _ => panic!("expected declaration statement"),
        }
    }

    #[test]
    fn test_everyone_in_settleup() {
        let (_, stmt) =
            statement("!settleup @everyone").expect("settleup with @everyone should parse");
        assert_eq!(
            stmt,
            Statement::Command(Command::SettleUp {
                members: {
                    let mut e = SetExpr::new();
                    e.push(SetOp::PushGroup("MEMBERS"));
                    e
                },
                cash_members: None,
            })
        );
    }

    #[test]
    fn test_everyone_mixed_with_mentions() {
        let (_, expr) = set_expr("<@1> @everyone").expect("mention + @everyone should parse");
        assert_eq!(
            expr.ops(),
            &[SetOp::Push(1), SetOp::PushGroup("MEMBERS"), SetOp::Union,]
        );
    }

    #[test]
    fn test_reject_weighted_everyone_outside_payee() {
        let result = parse_program("!settleup @everyone*2");
        assert!(matches!(result, Err(ParseError::SyntaxError { .. })));
    }

    // `to`, `paid`, and Japanese payment keywords are effectively reserved.
    #[rstest]
    #[case::to("1000 to")]
    #[case::paid("1000 paid")]
    fn test_grammar_keywords_are_reserved_in_implicit_payment(#[case] input: &str) {
        let result = parse_program(input);
        assert!(matches!(result, Err(ParseError::SyntaxError { .. })));
    }

    #[rstest]
    #[case::ja_payment_missing_amount(
        "<@123> が <@456> に 立て替えた",
        Err(ParseError::SyntaxError {
            line: 1,
            kind: SyntaxErrorKind::ParseFailure {
                attempted_form: Some("<PAYER> が <PAYEE> に <AMOUNT> 立て替えた"),
                expected: ExpectedElement::Amount,
                near: "立て替えた".to_string(),
            },
        })
    )]
    #[case::ja_payment_missing_payee(
        "<@123> が 立て替えた",
        Err(ParseError::SyntaxError {
            line: 1,
            kind: SyntaxErrorKind::ParseFailure {
                attempted_form: Some("<PAYER> が <PAYEE> に <AMOUNT> 立て替えた"),
                expected: ExpectedElement::MemberOrGroup,
                near: "立て替えた".to_string(),
            },
        })
    )]
    #[case::en_payment_missing_amount(
        "<@123> paid to <@456>",
        Err(ParseError::SyntaxError {
            line: 1,
            kind: SyntaxErrorKind::ParseFailure {
                attempted_form: Some("<PAYER> paid <AMOUNT> to <PAYEE>"),
                expected: ExpectedElement::Amount,
                near: "to <@456>".to_string(),
            },
        })
    )]
    #[case::implicit_to_missing_payee(
        "1000 to",
        Err(ParseError::SyntaxError {
            line: 1,
            kind: SyntaxErrorKind::ParseFailure {
                attempted_form: Some("<AMOUNT> to <PAYEE>"),
                expected: ExpectedElement::MemberOrGroup,
                near: String::new(),
            },
        })
    )]
    #[case::settleup_missing_members(
        "!settleup",
        Err(ParseError::SyntaxError {
            line: 1,
            kind: SyntaxErrorKind::ParseFailure {
                attempted_form: Some("!command"),
                expected: ExpectedElement::MemberOrGroup,
                near: String::new(),
            },
        })
    )]
    #[case::trailing_text(
        "1000 <@123> xyz",
        Err(ParseError::SyntaxError {
            line: 1,
            kind: SyntaxErrorKind::TrailingInput {
                text: "xyz".to_string(),
            },
        })
    )]
    #[case::unrecognized_input(
        "hello world",
        Err(ParseError::SyntaxError {
            line: 1,
            kind: SyntaxErrorKind::ParseFailure {
                attempted_form: None,
                expected: ExpectedElement::Unknown,
                near: "world".to_string(),
            },
        })
    )]
    #[case::error_on_second_line(
        "<@1> が <@2> に 1000 立て替えた\nbad line",
        Err(ParseError::SyntaxError {
            line: 2,
            kind: SyntaxErrorKind::ParseFailure {
                attempted_form: None,
                expected: ExpectedElement::Unknown,
                near: "line".to_string(),
            },
        })
    )]
    fn test_syntax_error_diagnostics(
        #[case] input: &str,
        #[case] expected: Result<(), ParseError>,
    ) {
        let actual = parse_program(input).map(|_| ());
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::to_prefix_in_group_name("1000 tokyo", "tokyo")]
    #[case::to_hyphenated_group("1000 to-team", "to-team")]
    fn test_to_prefix_group_names_parse_as_implicit_payment(
        #[case] input: &str,
        #[case] expected_group: &str,
    ) {
        let program = parse_program(input).expect("should parse as implicit payment");
        let Statement::Payment(p) = &program.statements[0].statement else {
            panic!("expected payment");
        };
        assert!(p.is_payer_implicit());
        assert_eq!(p.payee.ops(), &[SetOp::PushGroup(expected_group)]);
    }
}
