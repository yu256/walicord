use std::{env, path::PathBuf, process::ExitCode};
use walicord::discord_ledger_maintenance::{
    DiscordLedgerMaintenanceRequestEmitter, MaintenanceReadinessConfig,
    default_runtime_instance_lock_path, ensure_artifact_directory,
};
use walicord_application::ledger::{
    LedgerEntryId,
    maintenance::{
        MaintenanceCommand, OlderThanTwentyReplacement, OlderThanTwentyVoidRecoveryRequest,
        TransportChannelId,
    },
};
use walicord_domain::model::MemberId;

const RUNBOOK_ENV: &str = "WALICORD_LEDGER_MAINTENANCE_RUNBOOK";
const ARTIFACT_DIRECTORY_ENV: &str = "WALICORD_LEDGER_MAINTENANCE_ARTIFACT_DIR";
const CONFIRM_QUIESCED: &str = "--confirm-quiesced-single-writer-window";

#[tokio::main]
async fn main() -> ExitCode {
    tracing_subscriber::fmt::init();
    match run(env::args().skip(1).collect()).await {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("{message}");
            ExitCode::FAILURE
        }
    }
}

async fn run(args: Vec<String>) -> Result<(), String> {
    let (confirmed, args) = take_confirmation(args);
    let maintenance = DiscordLedgerMaintenanceRequestEmitter::new(config_from_env(confirmed)?);
    let Some((subcommand, arguments)) = args.split_first() else {
        return Err(usage());
    };
    if subcommand == "readiness" {
        if !arguments.is_empty() {
            return Err(usage());
        }
        let readiness = maintenance.readiness().map_err(|error| error.to_string())?;
        println!(
            "ready: runbook={} artifacts={} lock={}",
            readiness.runbook_path.display(),
            readiness.artifact_directory.display(),
            readiness.runtime_instance_lock_path.display()
        );
        return Ok(());
    }
    let command = parse_command(subcommand, arguments)?;
    let outcome = maintenance
        .run(command)
        .await
        .map_err(|error| error.to_string())?;
    println!(
        "maintenance request persisted: operation={} artifact={}",
        outcome.operation,
        outcome.artifact_path.display()
    );
    Ok(())
}

fn config_from_env(confirmed: bool) -> Result<MaintenanceReadinessConfig, String> {
    let runbook_path = required_path(RUNBOOK_ENV)?;
    let artifact_directory = required_path(ARTIFACT_DIRECTORY_ENV)?;
    ensure_artifact_directory(&artifact_directory)
        .map_err(|error| format!("failed to prepare {ARTIFACT_DIRECTORY_ENV}: {error}"))?;
    Ok(MaintenanceReadinessConfig {
        artifact_directory,
        runbook_path,
        runtime_instance_lock_path: default_runtime_instance_lock_path(),
        quiesced_single_writer_window_confirmed: confirmed,
    })
}

fn required_path(name: &str) -> Result<PathBuf, String> {
    env::var_os(name)
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
        .ok_or_else(|| format!("{name} is not set"))
}

fn take_confirmation(args: Vec<String>) -> (bool, Vec<String>) {
    let mut confirmed = false;
    let mut remaining = Vec::with_capacity(args.len());
    for argument in args {
        if argument == CONFIRM_QUIESCED {
            confirmed = true;
        } else {
            remaining.push(argument);
        }
    }
    (confirmed, remaining)
}

fn parse_command(subcommand: &str, arguments: &[String]) -> Result<MaintenanceCommand, String> {
    match (subcommand, arguments) {
        ("older-than-twenty-void", [ledger_id, target_entry_id, actor_id, replacement_actor]) => {
            Ok(MaintenanceCommand::OlderThanTwentyVoidRecovery(
                OlderThanTwentyVoidRecoveryRequest {
                    ledger_id: parse(ledger_id, "ledger id")?,
                    target_entry_id: LedgerEntryId(parse(target_entry_id, "target entry id")?),
                    actor_id: MemberId(parse(actor_id, "actor id")?),
                    replacement: Box::new(OlderThanTwentyReplacement {
                        replacement_recorded_by: MemberId(parse(
                            replacement_actor,
                            "replacement actor id",
                        )?),
                    }),
                },
            ))
        }
        ("duplicate-thread-resolution", [ledger_id, keep, retired]) => {
            Ok(MaintenanceCommand::DuplicateThreadResolution {
                ledger_id: parse(ledger_id, "ledger id")?,
                authoritative_thread_keep: channel(keep)?,
                retired_thread_ids: retired
                    .split(',')
                    .map(channel)
                    .collect::<Result<Vec<_>, _>>()?,
            })
        }
        ("damaged-thread-replacement", [retired_ledger_id, new_parent_channel_id]) => {
            Ok(MaintenanceCommand::DamagedThreadReplacement {
                retired_ledger_id: parse(retired_ledger_id, "retired ledger id")?,
                new_parent_channel_id: channel(new_parent_channel_id)?,
            })
        }
        ("ordinary-seal", [ledger_id]) => Ok(MaintenanceCommand::OrdinarySeal {
            ledger_id: parse(ledger_id, "ledger id")?,
        }),
        ("sealed-entry-correction", [ledger_id, target_entry_id]) => {
            Ok(MaintenanceCommand::StandardSealedEntryCorrection {
                ledger_id: parse(ledger_id, "ledger id")?,
                target_entry_id: LedgerEntryId(parse(target_entry_id, "target entry id")?),
            })
        }
        ("prior-adjustment-correction", [ledger_id, target_entry_id]) => {
            Ok(MaintenanceCommand::StandardPriorAdjustmentCorrection {
                ledger_id: parse(ledger_id, "ledger id")?,
                target_entry_id: LedgerEntryId(parse(target_entry_id, "target entry id")?),
            })
        }
        _ => Err(usage()),
    }
}

fn channel(value: &str) -> Result<TransportChannelId, String> {
    TransportChannelId::new(parse(value, "channel id")?).map_err(|error| error.to_string())
}

fn parse<T>(value: &str, name: &str) -> Result<T, String>
where
    T: std::str::FromStr,
    T::Err: std::fmt::Display,
{
    value
        .parse()
        .map_err(|error| format!("invalid {name} `{value}`: {error}"))
}

fn usage() -> String {
    format!(
        "usage: discord_ledger_maintenance {CONFIRM_QUIESCED} <readiness|older-than-twenty-void|duplicate-thread-resolution|damaged-thread-replacement|ordinary-seal|sealed-entry-correction|prior-adjustment-correction> [arguments]"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use walicord_ledger::test_fixtures::ledger_id;

    #[test]
    fn parse_ordinary_seal_builds_typed_command() {
        assert_eq!(
            parse_command("ordinary-seal", &["77".to_owned()]),
            Ok(MaintenanceCommand::OrdinarySeal {
                ledger_id: ledger_id(77)
            })
        );
    }

    #[test]
    fn parse_duplicate_resolution_rejects_zero_channel_id() {
        assert_eq!(
            parse_command(
                "duplicate-thread-resolution",
                &["77".to_owned(), "0".to_owned(), "2".to_owned()]
            ),
            Err("transport channel id must be non-zero".to_owned())
        );
    }

    #[test]
    fn take_confirmation_removes_gate_flag_before_command_parsing() {
        assert_eq!(
            take_confirmation(vec![
                "ordinary-seal".to_owned(),
                CONFIRM_QUIESCED.to_owned(),
                "77".to_owned(),
            ]),
            (true, vec!["ordinary-seal".to_owned(), "77".to_owned()])
        );
    }
}
