use std::{borrow::Cow, env, fs, process};

use walicord_application::{
    Command as ProgramCommand, MessageProcessor, ProcessingOutcome, ReceiptResolveError, Script,
    ScriptStatement, SettlementBuildError,
};
use walicord_infrastructure::{WalicordProgramParser, WalicordSettlementOptimizer};
use walicord_parser::extract_members_from_topic;
use walicord_presentation::{SettlementPresenter, SettlementView, VariablesPresenter};

type CliResult<T> = Result<T, Cow<'static, str>>;

fn format_settlement_error(err: SettlementBuildError) -> Cow<'static, str> {
    match err {
        SettlementBuildError::Optimization(err) => match err {
            walicord_application::SettlementOptimizationError::ImbalancedTotal(total) => format!(
                "{} (total: {total})",
                walicord_i18n::SETTLEMENT_CALCULATION_FAILED
            )
            .into(),
            walicord_application::SettlementOptimizationError::NoSolution => {
                walicord_i18n::SETTLEMENT_CALCULATION_FAILED.into()
            }
        },
        SettlementBuildError::Receipt(err) => format_receipt_error(err),
    }
}

fn format_receipt_error(err: ReceiptResolveError) -> Cow<'static, str> {
    match err {
        ReceiptResolveError::OcrUnavailable { .. } => walicord_i18n::RECEIPT_OCR_UNAVAILABLE.into(),
        ReceiptResolveError::MissingAttachment { .. } => {
            walicord_i18n::RECEIPT_ATTACHMENT_MISSING.into()
        }
        ReceiptResolveError::OcrFailed { source } => {
            format!("{} ({source})", walicord_i18n::RECEIPT_OCR_FAILED).into()
        }
        ReceiptResolveError::TotalNotFound { .. } => walicord_i18n::RECEIPT_TOTAL_NOT_FOUND.into(),
        ReceiptResolveError::TotalAmbiguous { .. } => walicord_i18n::RECEIPT_TOTAL_AMBIGUOUS.into(),
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

    let (members, program_content) = parse_members_first_line(&source)?;

    let processor = MessageProcessor::new(&WalicordProgramParser, &WalicordSettlementOptimizer);

    let program = match processor.parse_program(&members, program_content) {
        ProcessingOutcome::Success(program) => program,
        ProcessingOutcome::MissingMembersDeclaration => {
            return Err("Program is missing MEMBERS declaration".into());
        }
        ProcessingOutcome::UndefinedMember { name, line } => {
            return Err(format!("Undefined member '{name}' at line {line}").into());
        }
        ProcessingOutcome::FailedToEvaluateGroup { name } => {
            return Err(walicord_i18n::failed_to_evaluate_group(name).into());
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
