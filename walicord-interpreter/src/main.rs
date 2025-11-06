use std::{borrow::Cow, env, fs, process};

use walicord_core::{
    application::{MessageProcessor, ProcessingOutcome},
    domain::model::{Command as ProgramCommand, Program, Statement},
    infrastructure::parser::WalicordProgramParser,
};
use walicord_parser::extract_members_from_topic;

type CliResult<T> = Result<T, Cow<'static, str>>;

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {err}");
        process::exit(1);
    }
}

fn run() -> CliResult<()> {
    let Some(path) = env::args().nth(1) else {
        return Err("Usage: walicord_interpreter <file.walicord>".into());
    };

    let source =
        fs::read_to_string(&path).map_err(|err| format!("Failed to read '{path}': {err}"))?;

    let (members, program_content) = parse_members_first_line(&source)?;

    let processor = MessageProcessor::new(&WalicordProgramParser);

    let program = match processor.parse_program(&members, program_content) {
        ProcessingOutcome::Success(program) => program,
        ProcessingOutcome::MissingMembersDeclaration => {
            return Err("Program is missing MEMBERS declaration".into());
        }
        ProcessingOutcome::UndefinedMember { name, line } => {
            return Err(format!("Undefined member '{name}' at line {line}").into());
        }
        ProcessingOutcome::SyntaxError { message } => {
            return Err(format!("Syntax error: {message}").into());
        }
    };

    print_program_output(&processor, &program)
}

fn print_program_output<'a>(
    processor: &MessageProcessor<'a>,
    program: &Program<'a>,
) -> CliResult<()> {
    let mut printed = false;

    for (index, statement) in program.statements.iter().enumerate() {
        if let Statement::Command(command) = statement {
            match command {
                ProgramCommand::Variables => {
                    let output = processor.format_variables_response_for_prefix(program, index);
                    println!("{output}");
                    printed = true;
                }
                ProgramCommand::Evaluate => {
                    match processor.format_settlement_response_for_prefix(program, index) {
                        Ok(output) => {
                            println!("{output}");
                            printed = true;
                        }
                        Err(message) => return Err(message.into()),
                    }
                }
                ProgramCommand::SettleUp(_) => {
                    match processor.format_settlement_response_for_prefix(program, index) {
                        Ok(output) => {
                            println!("{output}");
                            printed = true;
                        }
                        Err(message) => return Err(message.into()),
                    }
                }
            }
        }
    }

    if !printed {
        match processor.format_settlement_response(program) {
            Ok(output) => println!("{output}"),
            Err(message) => return Err(message.into()),
        }
    }

    Ok(())
}

fn parse_members_first_line(source: &str) -> CliResult<(Vec<&str>, &str)> {
    if source.is_empty() {
        return Err("File is empty; expected `MEMBERS := ...` on the first line".into());
    }

    let (members_line_raw, rest) = match source.find('\n') {
        Some(idx) => (&source[..idx], &source[idx + 1..]),
        None => (source, ""),
    };

    let members_line = members_line_raw
        .strip_suffix('\r')
        .unwrap_or(members_line_raw);

    if !members_line.trim_start().starts_with("MEMBERS") {
        return Err("First line must start with `MEMBERS := ...`".into());
    }

    let members = extract_members_from_topic(members_line)
        .map_err(|_| "Failed to parse `MEMBERS := ...` declaration on the first line")?;

    Ok((members, rest))
}
