use crate::domain::{
    Declaration, Program, ProgramParseError, ProgramParser,
    model::{Payment, Statement},
};
use std::collections::{HashMap, HashSet};
use walicord_parser::{ParseError, Statement as ParserStatement, parse_program};

#[derive(Default)]
pub struct WalicordProgramParser;

impl ProgramParser for WalicordProgramParser {
    fn parse<'a>(
        &self,
        members: &'a [&'a str],
        content: &'a str,
    ) -> Result<Program<'a>, ProgramParseError> {
        match parse_program(members, content) {
            Ok(program) => {
                let walicord_parser::Program {
                    members_decl,
                    statements,
                } = program;

                let mut base_sets: HashMap<&'a str, HashSet<&'a str>> = HashMap::new();
                for &member in members {
                    base_sets.insert(member, HashSet::from([member]));
                }
                base_sets.insert("MEMBERS", members.iter().copied().collect::<HashSet<_>>());

                let mut evaluated_groups: HashMap<&'a str, HashSet<&'a str>> = HashMap::new();
                let mut domain_statements = Vec::with_capacity(statements.len());

                for stmt in statements {
                    match stmt {
                        ParserStatement::Declaration(decl) => {
                            let Some(result_set_cow) = decl.expression.evaluate(&|name| {
                                evaluated_groups.get(name).or_else(|| base_sets.get(name))
                            }) else {
                                return Err(ProgramParseError::SyntaxError(format!(
                                    "グループ '{}' の評価に失敗しました",
                                    decl.name
                                )));
                            };

                            let result_set_owned = result_set_cow.into_owned();

                            let mut members_vec: Vec<&str> = members
                                .iter()
                                .copied()
                                .filter(|m| result_set_owned.contains(m))
                                .collect();

                            let mut extras: Vec<&str> = result_set_owned
                                .iter()
                                .copied()
                                .filter(|m| !members_vec.contains(m))
                                .collect();
                            extras.sort_unstable();
                            members_vec.extend(extras);

                            evaluated_groups.insert(decl.name, result_set_owned);

                            domain_statements.push(Statement::Declaration(Declaration {
                                name: decl.name,
                                members: members_vec,
                            }));
                        }
                        ParserStatement::Payment(parser_payment) => {
                            let walicord_parser::Payment {
                                amount,
                                payer,
                                payee,
                            } = parser_payment;
                            domain_statements.push(Statement::Payment(Payment {
                                amount,
                                payer,
                                payee,
                            }));
                        }
                    }
                }

                Ok(Program {
                    members: members_decl,
                    statements: domain_statements,
                })
            }
            Err(err) => Err(match err {
                ParseError::NoMembersDeclaration(_) => ProgramParseError::MissingMembersDeclaration,
                ParseError::UndefinedMember { name, line } => {
                    ProgramParseError::UndefinedMember { name, line }
                }
                ParseError::SyntaxError(details) => ProgramParseError::SyntaxError(details),
            }),
        }
    }
}
