use std::{borrow::Cow, env, fs, process};

use walicord_application::{
    Command as ProgramCommand, MessageProcessor, ProcessingOutcome, Script, ScriptStatement,
    SettlementOptimizationError,
};
use walicord_domain::model::MemberId;
use walicord_infrastructure::{WalicordProgramParser, WalicordSettlementOptimizer};
use walicord_presentation::{SettlementPresenter, SettlementView, VariablesPresenter};

type CliResult<T> = Result<T, Cow<'static, str>>;

fn format_settlement_error(err: SettlementOptimizationError) -> Cow<'static, str> {
    match err {
        SettlementOptimizationError::ImbalancedTotal(total) => format!(
            "{} (total: {total})",
            walicord_i18n::SETTLEMENT_CALCULATION_FAILED
        )
        .into(),
        SettlementOptimizationError::NoSolution => {
            walicord_i18n::SETTLEMENT_CALCULATION_FAILED.into()
        }
    }
}

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

    let (member_ids, program_content) = parse_members_first_line(&source)?;

    let processor = MessageProcessor::new(&WalicordProgramParser, &WalicordSettlementOptimizer);

    let program = match processor.parse_program(&member_ids, program_content) {
        ProcessingOutcome::Success(program) => program,
        ProcessingOutcome::FailedToEvaluateGroup { name, line } => {
            return Err(format!(
                "{} (line {})",
                walicord_i18n::failed_to_evaluate_group(name),
                line
            )
            .into());
        }
        ProcessingOutcome::UndefinedGroup { name, line } => {
            return Err(format!("{} (line {})", walicord_i18n::undefined_group(name), line).into());
        }
        ProcessingOutcome::UndefinedMember { id, line } => {
            return Err(format!("{} (line {})", walicord_i18n::undefined_member(id), line).into());
        }
        ProcessingOutcome::SyntaxError { message } => {
            return Err(format!("Syntax error: {message}").into());
        }
    };

    print_program_output(&processor, &program)
}

fn print_settlement_view(response: &SettlementView) {
    println!("\n--- Balance Table SVG ---");
    println!("{}", response.balance_table_svg);

    if let Some(ref svg) = response.transfer_table_svg {
        println!("\n--- Transfer Table SVG ---");
        println!("{svg}");
    }
}

fn print_program_output<'a>(
    processor: &MessageProcessor<'a>,
    program: &Script<'a>,
) -> CliResult<()> {
    let mut printed = false;

    for (index, statement) in program.statements().iter().enumerate() {
        if let ScriptStatement::Command(command) = &statement.statement {
            match command {
                ProgramCommand::Variables => {
                    let output = VariablesPresenter::render_for_prefix(program, index);
                    println!("{output}");
                    printed = true;
                }
                ProgramCommand::Evaluate => {
                    match processor.build_settlement_result_for_prefix(program, index) {
                        Ok(response) => {
                            let view = SettlementPresenter::render(&response);
                            print_settlement_view(&view);
                            printed = true;
                        }
                        Err(err) => return Err(format_settlement_error(err)),
                    }
                }
                ProgramCommand::SettleUp(_) => {
                    match processor.build_settlement_result_for_prefix(program, index) {
                        Ok(response) => {
                            let view = SettlementPresenter::render(&response);
                            print_settlement_view(&view);
                            printed = true;
                        }
                        Err(err) => return Err(format_settlement_error(err)),
                    }
                }
            }
        }
    }

    if !printed {
        match processor.build_settlement_result(program) {
            Ok(response) => {
                let view = SettlementPresenter::render(&response);
                print_settlement_view(&view);
            }
            Err(err) => return Err(format_settlement_error(err)),
        }
    }

    Ok(())
}

fn parse_members_first_line(source: &str) -> CliResult<(Vec<MemberId>, &str)> {
    Ok((Vec::new(), source))
}
