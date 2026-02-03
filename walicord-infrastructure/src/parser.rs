use walicord_application::{
    Command, ProgramParseError, ProgramParser, Script, ScriptStatement, ScriptStatementWithLine,
};
use walicord_domain::{
    model::{MemberSetExpr, MemberSetOp, Money},
    Declaration, Payment, Program as DomainProgram, Statement as DomainStatement,
    StatementWithLine as DomainStatementWithLine,
};
use walicord_parser::{
    parse_program, Command as ParserCommand, ParseError, SetOp, Statement as ParserStatement,
};

#[derive(Default)]
pub struct WalicordProgramParser;

impl ProgramParser for WalicordProgramParser {
    fn parse<'a>(
        &self,
        members: &'a [&'a str],
        content: &'a str,
    ) -> Result<Script<'a>, ProgramParseError<'a>> {
        match parse_program(members, content) {
            Ok(program) => {
                let walicord_parser::Program {
                    members_decl,
                    statements,
                } = program;

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
                            let payer_expr = to_member_set_expr(payer);
                            let payee_expr = to_member_set_expr(payee);
                            let app_payment = Payment {
                                amount: Money::from_u64(amount),
                                payer: payer_expr.clone(),
                                payee: payee_expr.clone(),
                            };
                            let domain_payment = Payment {
                                amount: Money::from_u64(amount),
                                payer: payer_expr,
                                payee: payee_expr,
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
                                ParserCommand::Evaluate => Command::Evaluate,
                                ParserCommand::SettleUp(expr) => {
                                    Command::SettleUp(to_member_set_expr(expr))
                                }
                            };
                            app_statements.push(ScriptStatementWithLine {
                                line,
                                statement: ScriptStatement::Command(command),
                            });
                        }
                    }
                }

                DomainProgram::try_new(members_decl, domain_statements)
                    .map_err(ProgramParseError::from)?;
                Ok(Script::new(members_decl, app_statements))
            }
            Err(err) => Err(match err {
                ParseError::NoMembersDeclaration(_) => ProgramParseError::MissingMembersDeclaration,
                ParseError::SyntaxError(details) => ProgramParseError::SyntaxError(details),
                ParseError::UndefinedMember { name, line } => {
                    ProgramParseError::UndefinedMember { name, line }
                }
            }),
        }
    }
}

fn to_member_set_expr<'a>(expr: walicord_parser::SetExpr<'a>) -> MemberSetExpr<'a> {
    let ops = expr
        .ops()
        .iter()
        .map(|op| match op {
            SetOp::Push(name) => MemberSetOp::Push(name),
            SetOp::Union => MemberSetOp::Union,
            SetOp::Intersection => MemberSetOp::Intersection,
            SetOp::Difference => MemberSetOp::Difference,
        })
        .collect();
    MemberSetExpr::new(ops)
}
