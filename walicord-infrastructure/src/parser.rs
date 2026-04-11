use std::borrow::Cow;
use walicord_application::{
    Command, ProgramParseError, ProgramParser, Script, ScriptStatement, ScriptStatementWithLine,
    SyntaxErrorKind,
};
use walicord_domain::{
    AmountExpr, AmountOp, Declaration, Payment, Program as DomainProgram,
    Statement as DomainStatement, StatementWithLine as DomainStatementWithLine,
    model::{
        MemberId, MemberSetExpr, MemberSetOp, RoleId, RoleMembers, Weight, WeightOverride,
        WeightOverrides,
    },
};
use walicord_parser::{
    AmountExpr as ParserAmountExpr, AmountOp as ParserAmountOp, Command as ParserCommand,
    ParseError, PayerSpec, SetOp, Statement as ParserStatement, parse_program,
};

#[derive(Default)]
pub struct WalicordProgramParser;

fn map_expected_element(
    t: walicord_parser::ExpectedElement,
) -> walicord_application::ExpectedElement {
    match t {
        walicord_parser::ExpectedElement::Amount => walicord_application::ExpectedElement::Amount,
        walicord_parser::ExpectedElement::MemberOrGroup => {
            walicord_application::ExpectedElement::MemberOrGroup
        }
        walicord_parser::ExpectedElement::Unknown => walicord_application::ExpectedElement::Unknown,
    }
}

fn map_syntax_error_kind(kind: walicord_parser::SyntaxErrorKind) -> SyntaxErrorKind {
    match kind {
        walicord_parser::SyntaxErrorKind::ParseFailure {
            attempted_form,
            expected,
            near,
        } => SyntaxErrorKind::ParseFailure {
            attempted_form,
            expected: map_expected_element(expected),
            near,
        },
        walicord_parser::SyntaxErrorKind::TrailingInput { text } => {
            SyntaxErrorKind::TrailingInput { text }
        }
    }
}

fn map_parse_error<'a>(err: ParseError) -> ProgramParseError<'a> {
    match err {
        ParseError::SyntaxError { line, kind } => ProgramParseError::SyntaxError {
            line,
            kind: map_syntax_error_kind(kind),
        },
        ParseError::UndefinedGroup { name, line } => ProgramParseError::UndefinedGroup {
            name: Cow::Owned(name),
            line,
        },
        ParseError::UndefinedMember { id, line } => ProgramParseError::UndefinedMember { id, line },
    }
}

impl ProgramParser for WalicordProgramParser {
    fn parse<'a>(
        &self,
        member_ids: &'a [MemberId],
        role_members: &'a RoleMembers,
        content: &'a str,
        author_id: Option<MemberId>,
    ) -> Result<Script<'a>, ProgramParseError<'a>> {
        match parse_program(content) {
            Ok(program) => {
                let walicord_parser::Program { statements, .. } = program;

                let mut app_statements = Vec::with_capacity(statements.len());
                let mut domain_statements = Vec::with_capacity(statements.len());

                for stmt in statements {
                    let walicord_parser::StatementWithLine { line, statement } = stmt;
                    match statement {
                        ParserStatement::Declaration(decl) => {
                            let expression = to_member_set_expr_no_weight(decl.expression, line)?;
                            let app_decl = Declaration {
                                name: decl.name,
                                expression: expression.clone(),
                            };
                            let domain_decl = Declaration {
                                name: decl.name,
                                expression,
                            };
                            let app_statement = DomainStatement::Declaration(app_decl);
                            app_statements.push(ScriptStatementWithLine {
                                line,
                                statement: ScriptStatement::Domain(app_statement),
                            });
                            domain_statements.push(DomainStatementWithLine {
                                line,
                                statement: DomainStatement::Declaration(domain_decl),
                            });
                        }
                        ParserStatement::Payment(parser_payment) => {
                            let walicord_parser::Payment {
                                amount,
                                payer,
                                payee,
                            } = parser_payment;
                            let amount_expr = to_amount_expr(amount);
                            let amount_money = amount_expr.evaluate().map_err(|err| {
                                ProgramParseError::InvalidAmountExpression {
                                    line,
                                    detail: err.to_string(),
                                }
                            })?;
                            let payer_expr = match payer {
                                PayerSpec::Explicit(expr) => {
                                    to_member_set_expr_no_weight(expr, line)?
                                }
                                PayerSpec::Implicit => {
                                    let Some(author) = author_id else {
                                        return Err(
                                            ProgramParseError::MissingContextForImplicitAuthor {
                                                line,
                                            },
                                        );
                                    };
                                    MemberSetExpr::new([MemberSetOp::Push(author)])
                                }
                            };
                            let payee_expr = to_member_set_expr_allow_weighted(payee.clone());
                            let payee_overrides = extract_payee_weight_overrides(&payee);
                            let has_explicit_weights = !payee_overrides.is_empty();

                            // Validate that not all weights are zero.
                            // This only applies when ALL members have explicit zero weights
                            // (no unweighted members, no group references, and all weighted members have weight 0).
                            // Unweighted members default to weight 1, so total would be > 0.
                            // Group/role references are skipped because their members are resolved at runtime.
                            if has_explicit_weights
                                && final_explicit_overrides_are_all_zero(&payee_overrides)
                                && !payee.has_unweighted_push()
                                && !payee.has_group_reference()
                            {
                                return Err(ProgramParseError::AllZeroWeights { line });
                            }

                            let (app_payment, domain_payment) = if !has_explicit_weights {
                                (
                                    Payment::even(
                                        amount_money,
                                        payer_expr.clone(),
                                        payee_expr.clone(),
                                    ),
                                    Payment::even(amount_money, payer_expr, payee_expr),
                                )
                            } else {
                                (
                                    Payment::weighted_with_overrides(
                                        amount_money,
                                        payer_expr.clone(),
                                        payee_expr.clone(),
                                        payee_overrides.clone(),
                                    ),
                                    Payment::weighted_with_overrides(
                                        amount_money,
                                        payer_expr,
                                        payee_expr,
                                        payee_overrides,
                                    ),
                                )
                            };
                            let app_statement = DomainStatement::Payment(app_payment);
                            app_statements.push(ScriptStatementWithLine {
                                line,
                                statement: ScriptStatement::Domain(app_statement),
                            });
                            domain_statements.push(DomainStatementWithLine {
                                line,
                                statement: DomainStatement::Payment(domain_payment),
                            });
                        }
                        ParserStatement::Command(parser_command) => {
                            let command = match parser_command {
                                ParserCommand::Variables => Command::Variables,
                                ParserCommand::Review => Command::Review,
                                ParserCommand::MemberAddCash { members } => {
                                    Command::MemberAddCash {
                                        members: to_member_set_expr_no_weight(members, line)?,
                                    }
                                }
                                ParserCommand::CashSelf => {
                                    let Some(author) = author_id else {
                                        return Err(
                                            ProgramParseError::MissingContextForImplicitAuthor {
                                                line,
                                            },
                                        );
                                    };
                                    Command::MemberAddCash {
                                        members: MemberSetExpr::new([MemberSetOp::Push(author)]),
                                    }
                                }
                                ParserCommand::SettleUp {
                                    members,
                                    cash_members,
                                } => Command::SettleUp {
                                    members: to_member_set_expr_no_weight(members, line)?,
                                    cash_members: cash_members
                                        .map(|set| to_member_set_expr_no_weight(set, line))
                                        .transpose()?,
                                },
                            };
                            app_statements.push(ScriptStatementWithLine {
                                line,
                                statement: ScriptStatement::Command(command),
                            });
                        }
                    }
                }

                DomainProgram::try_new_with_roles(domain_statements, member_ids, role_members)
                    .map_err(ProgramParseError::from)?;
                Ok(Script::new(member_ids, role_members, app_statements))
            }
            Err(err) => Err(map_parse_error(err)),
        }
    }
}

fn to_member_set_expr_allow_weighted<'a>(expr: walicord_parser::SetExpr<'a>) -> MemberSetExpr<'a> {
    let ops = expr.ops().iter().map(|op| match op {
        SetOp::Push(id) | SetOp::PushWeighted(id, _) => MemberSetOp::Push(MemberId(*id)),
        SetOp::PushRole(id) | SetOp::PushWeightedRole(id, _) => MemberSetOp::PushRole(RoleId(*id)),
        SetOp::PushGroup(name) | SetOp::PushWeightedGroup(name, _) => MemberSetOp::PushGroup(name),
        SetOp::Union => MemberSetOp::Union,
        SetOp::Intersection => MemberSetOp::Intersection,
        SetOp::Difference => MemberSetOp::Difference,
    });
    MemberSetExpr::new(ops)
}

fn to_member_set_expr_no_weight<'a>(
    expr: walicord_parser::SetExpr<'a>,
    line: usize,
) -> Result<MemberSetExpr<'a>, ProgramParseError<'a>> {
    if expr.ops().iter().any(|op| {
        matches!(
            op,
            SetOp::PushWeighted(_, _)
                | SetOp::PushWeightedRole(_, _)
                | SetOp::PushWeightedGroup(_, _)
        )
    }) {
        return Err(ProgramParseError::WeightedReferenceOutsidePayee { line });
    }

    Ok(to_member_set_expr_allow_weighted(expr))
}

fn extract_payee_weight_overrides(expr: &walicord_parser::SetExpr<'_>) -> WeightOverrides {
    WeightOverrides::new(expr.ops().iter().filter_map(|op| match op {
        SetOp::PushWeighted(id, w) => Some(WeightOverride::member(MemberId(*id), Weight(*w))),
        SetOp::PushWeightedRole(id, w) => Some(WeightOverride::role(RoleId(*id), Weight(*w))),
        SetOp::PushWeightedGroup(name, w) => Some(WeightOverride::group(*name, Weight(*w))),
        _ => None,
    }))
}

fn final_explicit_overrides_are_all_zero(overrides: &WeightOverrides) -> bool {
    let entries = overrides.entries();
    entries.iter().enumerate().all(|(idx, entry)| {
        let overwritten = entries[idx + 1..]
            .iter()
            .any(|later| later.target == entry.target);
        overwritten || entry.weight == Weight::ZERO
    })
}

fn to_amount_expr(expr: ParserAmountExpr) -> AmountExpr {
    let ops = expr.ops().iter().map(|op| match op {
        ParserAmountOp::Literal(value) => AmountOp::Literal(*value),
        ParserAmountOp::Add => AmountOp::Add,
        ParserAmountOp::Sub => AmountOp::Sub,
        ParserAmountOp::Mul => AmountOp::Mul,
        ParserAmountOp::Div => AmountOp::Div,
    });
    AmountExpr::new(ops)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use walicord_application::{Command, ProgramParser, ScriptStatement};
    use walicord_domain::{
        Statement,
        model::{RoleId, RoleMembers},
    };

    fn empty_roles() -> &'static RoleMembers {
        use std::sync::OnceLock;
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(RoleMembers::default)
    }

    const EMPTY_MEMBERS: [MemberId; 0] = [];

    fn parse_payment_payer_ids(
        input: &'static str,
        author: Option<MemberId>,
    ) -> Result<Vec<MemberId>, ProgramParseError<'static>> {
        let parser = WalicordProgramParser;
        parser
            .parse(&EMPTY_MEMBERS, empty_roles(), input, author)
            .map(|script| {
                let statement = &script.statements()[0].statement;
                let ScriptStatement::Domain(Statement::Payment(payment)) = statement else {
                    panic!("expected payment statement");
                };
                payment.payer.referenced_ids().collect()
            })
    }

    fn parse_payment_payee_role_ids(
        input: &'static str,
        author: Option<MemberId>,
        roles: &'static RoleMembers,
    ) -> Result<Vec<RoleId>, ProgramParseError<'static>> {
        let parser = WalicordProgramParser;
        parser
            .parse(&EMPTY_MEMBERS, roles, input, author)
            .map(|script| {
                let statement = &script.statements()[0].statement;
                let ScriptStatement::Domain(Statement::Payment(payment)) = statement else {
                    panic!("expected payment statement");
                };
                payment.payee.referenced_role_ids().collect()
            })
    }

    fn parse_payment_is_even_allocation(
        input: &'static str,
        author: Option<MemberId>,
        roles: &'static RoleMembers,
    ) -> Result<bool, ProgramParseError<'static>> {
        let parser = WalicordProgramParser;
        parser
            .parse(&EMPTY_MEMBERS, roles, input, author)
            .map(|script| {
                let Some(payment) = script.statements().iter().find_map(|stmt| {
                    let ScriptStatement::Domain(Statement::Payment(payment)) = &stmt.statement
                    else {
                        return None;
                    };
                    Some(payment)
                }) else {
                    panic!("expected payment statement");
                };
                payment.allocation.is_even()
            })
    }

    fn roles_with_role_10() -> &'static RoleMembers {
        use std::sync::OnceLock;
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(|| {
            RoleMembers::from_iter([(RoleId(10), [MemberId(2), MemberId(3)].into_iter().collect())])
        })
    }

    #[rstest]
    #[case::implicit_without_to_with_author(
        "1000 <@2>",
        Some(MemberId(1)),
        Ok(vec![MemberId(1)])
    )]
    #[case::implicit_with_to_with_author(
        "1000 to <@2>",
        Some(MemberId(1)),
        Ok(vec![MemberId(1)])
    )]
    #[case::implicit_without_to_missing_author(
        "1000 <@2>",
        None,
        Err(ProgramParseError::MissingContextForImplicitAuthor { line: 1 })
    )]
    #[case::implicit_with_to_missing_author(
        "1000 to <@2>",
        None,
        Err(ProgramParseError::MissingContextForImplicitAuthor { line: 1 })
    )]
    fn parse_implicit_payer(
        #[case] input: &'static str,
        #[case] author: Option<MemberId>,
        #[case] expected: Result<Vec<MemberId>, ProgramParseError<'static>>,
    ) {
        let actual = parse_payment_payer_ids(input, author);
        assert_eq!(actual, expected);
    }

    #[test]
    fn parse_settleup_with_cash_maps_to_command_model() {
        let parser = WalicordProgramParser;
        let script = parser
            .parse(
                &EMPTY_MEMBERS,
                empty_roles(),
                "!settleup <@1> --cash <@2>",
                None,
            )
            .expect("parse should succeed");

        let statement = &script.statements()[0].statement;
        let ScriptStatement::Command(Command::SettleUp {
            members,
            cash_members,
        }) = statement
        else {
            panic!("expected settleup command");
        };

        let settle_ids: Vec<_> = members.referenced_ids().collect();
        assert_eq!(settle_ids, vec![MemberId(1)]);
        let cash_ids: Vec<_> = cash_members
            .as_ref()
            .expect("cash should exist")
            .referenced_ids()
            .collect();
        assert_eq!(cash_ids, vec![MemberId(2)]);
    }

    #[test]
    fn parse_member_cash_command_maps_to_command_model() {
        let parser = WalicordProgramParser;
        let script = parser
            .parse(
                &EMPTY_MEMBERS,
                empty_roles(),
                "!member set <@1> <@2> cash",
                None,
            )
            .expect("parse should succeed");

        let statement = &script.statements()[0].statement;
        let ScriptStatement::Command(Command::MemberAddCash { members }) = statement else {
            panic!("expected member cash command");
        };

        let ids: Vec<_> = members.referenced_ids().collect();
        assert_eq!(ids, vec![MemberId(1), MemberId(2)]);
    }

    #[test]
    fn parse_cash_self_command_maps_to_member_cash() {
        let parser = WalicordProgramParser;
        let script = parser
            .parse(&EMPTY_MEMBERS, empty_roles(), "!cash", Some(MemberId(7)))
            .expect("parse should succeed");

        let statement = &script.statements()[0].statement;
        let ScriptStatement::Command(Command::MemberAddCash { members }) = statement else {
            panic!("expected member cash command");
        };

        let ids: Vec<_> = members.referenced_ids().collect();
        assert_eq!(ids, vec![MemberId(7)]);
    }

    #[rstest]
    #[case::all_zero_weights(
        "3000 <@1>*0 <@2>*0",
        MemberId(3),
        Err(ProgramParseError::AllZeroWeights { line: 1 })
    )]
    #[case::later_zero_override_makes_total_zero(
        "3000 <@1>*1 <@1>*0",
        MemberId(3),
        Err(ProgramParseError::AllZeroWeights { line: 1 })
    )]
    #[case::later_non_zero_override_avoids_false_positive(
        "3000 <@1>*0 <@1>*1",
        MemberId(3),
        Ok(())
    )]
    #[case::mixed_zero_and_unweighted(
        "3000 <@1>*0 <@2>*0 <@3>",
        MemberId(4),
        Ok(())
    )]
    #[case::group_reference_with_zero_weights(
        "team := <@2>\n1000 team, <@1>*0",
        MemberId(3),
        Ok(())
    )]
    fn parse_weighted_payment_paths(
        #[case] input: &'static str,
        #[case] author: MemberId,
        #[case] expected: Result<(), ProgramParseError<'static>>,
    ) {
        let parser = WalicordProgramParser;
        let actual = parser
            .parse(&EMPTY_MEMBERS, empty_roles(), input, Some(author))
            .map(|_| ());
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::defined_role_with_author(
        "1000 to <@&10>",
        Some(MemberId(1)),
        roles_with_role_10(),
        Ok(vec![RoleId(10)])
    )]
    #[case::defined_weighted_role_with_author(
        "1000 to <@&10>*2",
        Some(MemberId(1)),
        roles_with_role_10(),
        Ok(vec![RoleId(10)])
    )]
    #[case::undefined_role_with_author(
        "1000 to <@&10>",
        Some(MemberId(1)),
        empty_roles(),
        Err(ProgramParseError::UndefinedRole { id: 10, line: 1 })
    )]
    #[case::undefined_role_without_author(
        "<@1> paid 1000 to <@&10>",
        None,
        empty_roles(),
        Err(ProgramParseError::UndefinedRole { id: 10, line: 1 })
    )]
    fn parse_role_reference(
        #[case] input: &'static str,
        #[case] author: Option<MemberId>,
        #[case] roles: &'static RoleMembers,
        #[case] expected: Result<Vec<RoleId>, ProgramParseError<'static>>,
    ) {
        let actual = parse_payment_payee_role_ids(input, author, roles);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::weighted_role_only("1000 to <@&10>*2", roles_with_role_10(), Ok(false))]
    #[case::weighted_group_only("team := <@2>\n1000 team*2", empty_roles(), Ok(false))]
    fn parse_weighted_runtime_reference_creates_weighted_allocation(
        #[case] input: &'static str,
        #[case] roles: &'static RoleMembers,
        #[case] expected: Result<bool, ProgramParseError<'static>>,
    ) {
        let actual = parse_payment_is_even_allocation(input, Some(MemberId(1)), roles);
        assert_eq!(actual, expected);
    }
}
