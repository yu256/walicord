use std::{borrow::Cow, collections::BTreeMap};
use walicord_application::{
    Command, ProgramParseError, ProgramParser, Script, ScriptStatement, ScriptStatementWithLine,
};
use walicord_domain::{
    AmountExpr, AmountOp, Declaration, Payment, Program as DomainProgram,
    Statement as DomainStatement, StatementWithLine as DomainStatementWithLine,
    model::{MemberId, MemberSetExpr, MemberSetOp, Weight},
};
use walicord_parser::{
    AmountExpr as ParserAmountExpr, AmountOp as ParserAmountOp, Command as ParserCommand,
    ParseError, PayerSpec, SetOp, Statement as ParserStatement, parse_program,
};

#[derive(Default)]
pub struct WalicordProgramParser;

fn map_parse_error<'a>(err: ParseError) -> ProgramParseError<'a> {
    match err {
        ParseError::SyntaxError { line, detail } => ProgramParseError::SyntaxError { line, detail },
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
                            let expression = to_member_set_expr(decl.expression);
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
                                PayerSpec::Explicit(expr) => to_member_set_expr(expr),
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
                            let payee_expr = to_member_set_expr(payee.clone());
                            let payee_weights = extract_payee_weights(&payee);

                            let (app_payment, domain_payment) = if payee_weights.is_empty() {
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
                                    Payment::weighted(
                                        amount_money,
                                        payer_expr.clone(),
                                        payee_expr.clone(),
                                        payee_weights.clone(),
                                    ),
                                    Payment::weighted(
                                        amount_money,
                                        payer_expr,
                                        payee_expr,
                                        payee_weights,
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
                                        members: to_member_set_expr(members),
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
                                    members: to_member_set_expr(members),
                                    cash_members: cash_members.map(to_member_set_expr),
                                },
                            };
                            app_statements.push(ScriptStatementWithLine {
                                line,
                                statement: ScriptStatement::Command(command),
                            });
                        }
                    }
                }

                DomainProgram::try_new(domain_statements, member_ids)
                    .map_err(ProgramParseError::from)?;
                Ok(Script::new(member_ids, app_statements))
            }
            Err(err) => Err(map_parse_error(err)),
        }
    }
}

fn to_member_set_expr<'a>(expr: walicord_parser::SetExpr<'a>) -> MemberSetExpr<'a> {
    let ops = expr.ops().iter().map(|op| match op {
        SetOp::Push(id) | SetOp::PushWeighted(id, _) => MemberSetOp::Push(MemberId(*id)),
        SetOp::PushGroup(name) => MemberSetOp::PushGroup(name),
        SetOp::Union => MemberSetOp::Union,
        SetOp::Intersection => MemberSetOp::Intersection,
        SetOp::Difference => MemberSetOp::Difference,
    });
    MemberSetExpr::new(ops)
}

fn extract_payee_weights(expr: &walicord_parser::SetExpr<'_>) -> BTreeMap<MemberId, Weight> {
    expr.referenced_weighted()
        .map(|(id, w)| (MemberId(id), Weight(w)))
        .collect()
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
    use walicord_domain::Statement;

    #[rstest]
    #[case("1000 <@2>")]
    #[case("1000 to <@2>")]
    fn parse_implicit_payer_injects_author(#[case] input: &str) {
        let parser = WalicordProgramParser;
        let members: [MemberId; 0] = [];
        let script = parser
            .parse(&members, input, Some(MemberId(1)))
            .expect("parse should succeed");

        let statement = &script.statements()[0].statement;
        let ScriptStatement::Domain(Statement::Payment(payment)) = statement else {
            panic!("expected payment statement");
        };

        let payer_ids: Vec<_> = payment.payer.referenced_ids().collect();
        assert_eq!(payer_ids, vec![MemberId(1)]);
    }

    #[rstest]
    #[case("1000 <@2>")]
    #[case("1000 to <@2>")]
    fn parse_implicit_payer_without_author_returns_error(#[case] input: &str) {
        let parser = WalicordProgramParser;
        let members: [MemberId; 0] = [];
        let result = parser.parse(&members, input, None);
        match result {
            Err(ProgramParseError::MissingContextForImplicitAuthor { line }) => {
                assert_eq!(line, 1);
            }
            _ => panic!("expected implicit payer without author error"),
        }
    }

    #[test]
    fn parse_settleup_with_cash_maps_to_command_model() {
        let parser = WalicordProgramParser;
        let members: [MemberId; 0] = [];
        let script = parser
            .parse(&members, "!settleup <@1> --cash <@2>", None)
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
        let members: [MemberId; 0] = [];
        let script = parser
            .parse(&members, "!member set <@1> <@2> cash", None)
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
        let members: [MemberId; 0] = [];
        let script = parser
            .parse(&members, "!cash", Some(MemberId(7)))
            .expect("parse should succeed");

        let statement = &script.statements()[0].statement;
        let ScriptStatement::Command(Command::MemberAddCash { members }) = statement else {
            panic!("expected member cash command");
        };

        let ids: Vec<_> = members.referenced_ids().collect();
        assert_eq!(ids, vec![MemberId(7)]);
    }
}
