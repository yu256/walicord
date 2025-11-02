use crate::domain::{
    Declaration, Program, ProgramParseError, ProgramParser,
    model::{Payment, Statement},
};
use walicord_parser::{ParseError, parse_program};

#[derive(Default)]
pub struct WalicordProgramParser;

impl ProgramParser for WalicordProgramParser {
    fn parse<'a>(
        &self,
        members: &'a [&'a str],
        content: &'a str,
    ) -> Result<Program<'a>, ProgramParseError> {
        match parse_program(members, content) {
            Ok(program) => Ok(Program {
                members: program.members_decl,
                statements: program
                    .statements
                    .into_iter()
                    .map(|stmt| match stmt {
                        walicord_parser::Statement::Declaration(decl) => {
                            Statement::Declaration(Declaration {
                                name: decl.name,
                                members: decl.members,
                            })
                        }
                        walicord_parser::Statement::Payment(p) => Statement::Payment(Payment {
                            amount: p.amount,
                            payer: p.payer,
                            payee: p.payee,
                        }),
                    })
                    .collect(),
            }),
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
