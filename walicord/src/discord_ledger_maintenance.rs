use serde_json::{Value, json};
use std::{
    fs::{OpenOptions, create_dir_all},
    io::Write as _,
    path::{Path, PathBuf},
    sync::atomic::{AtomicU64, Ordering},
    time::{SystemTime, UNIX_EPOCH},
};
use walicord_application::ledger::maintenance::{MaintenanceCommand, TransportChannelId};
use walicord_infrastructure::{InstanceLockError, acquire_instance_lock};

static ARTIFACT_SEQUENCE: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaintenanceReadinessConfig {
    pub artifact_directory: PathBuf,
    pub runbook_path: PathBuf,
    pub runtime_instance_lock_path: PathBuf,
    pub quiesced_single_writer_window_confirmed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaintenanceReadiness {
    pub artifact_directory: PathBuf,
    pub runbook_path: PathBuf,
    pub runtime_instance_lock_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum MaintenanceReadinessError {
    #[error("quiesced single-writer window was not confirmed")]
    QuiescedSingleWriterWindowNotConfirmed,
    #[error("maintenance runbook is unavailable: {path}")]
    RunbookUnavailable { path: PathBuf },
    #[error("maintenance artifact directory is unavailable: {path}")]
    ArtifactDirectoryUnavailable { path: PathBuf },
}

#[derive(Debug, thiserror::Error)]
pub enum MaintenanceError {
    #[error(transparent)]
    Readiness(#[from] MaintenanceReadinessError),
    #[error("runtime instance lock is unavailable: {path}: {source}")]
    RuntimeInstanceLockUnavailable {
        path: PathBuf,
        #[source]
        source: InstanceLockError,
    },
    #[error("failed to persist maintenance artifact at {path}: {source}")]
    PersistArtifact {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaintenanceRequestArtifact {
    pub artifact_path: PathBuf,
    pub operation: &'static str,
}

pub struct DiscordLedgerMaintenanceRequestEmitter {
    config: MaintenanceReadinessConfig,
}

impl DiscordLedgerMaintenanceRequestEmitter {
    pub fn new(config: MaintenanceReadinessConfig) -> Self {
        Self { config }
    }

    pub fn readiness(&self) -> Result<MaintenanceReadiness, MaintenanceReadinessError> {
        if !self.config.quiesced_single_writer_window_confirmed {
            return Err(MaintenanceReadinessError::QuiescedSingleWriterWindowNotConfirmed);
        }
        if !self.config.runbook_path.is_file() {
            return Err(MaintenanceReadinessError::RunbookUnavailable {
                path: self.config.runbook_path.clone(),
            });
        }
        if !self.config.artifact_directory.is_dir() {
            return Err(MaintenanceReadinessError::ArtifactDirectoryUnavailable {
                path: self.config.artifact_directory.clone(),
            });
        }
        Ok(MaintenanceReadiness {
            artifact_directory: self.config.artifact_directory.clone(),
            runbook_path: self.config.runbook_path.clone(),
            runtime_instance_lock_path: self.config.runtime_instance_lock_path.clone(),
        })
    }

    pub async fn run(
        &self,
        command: MaintenanceCommand,
    ) -> Result<MaintenanceRequestArtifact, MaintenanceError> {
        let readiness = self.readiness()?;
        let _runtime_lock = acquire_instance_lock(readiness.runtime_instance_lock_path.clone())
            .map_err(|source| MaintenanceError::RuntimeInstanceLockUnavailable {
                path: readiness.runtime_instance_lock_path.clone(),
                source,
            })?;
        let operation = maintenance_operation(&command);
        let artifact_path = persist_artifact(&readiness, &command, operation)?;
        tracing::warn!(
            event = "discord_ledger_maintenance_request",
            operation,
            artifact_path = %artifact_path.display(),
            runbook_path = %readiness.runbook_path.display(),
            "persisted trusted-operator maintenance request"
        );
        Ok(MaintenanceRequestArtifact {
            artifact_path,
            operation,
        })
    }
}

fn persist_artifact(
    readiness: &MaintenanceReadiness,
    command: &MaintenanceCommand,
    operation: &'static str,
) -> Result<PathBuf, MaintenanceError> {
    let sequence = ARTIFACT_SEQUENCE.fetch_add(1, Ordering::Relaxed);
    let issued_at = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let path = readiness.artifact_directory.join(format!(
        "discord-ledger-maintenance-{issued_at}-{}-{sequence}.json",
        std::process::id()
    ));
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .map_err(|source| MaintenanceError::PersistArtifact {
            path: path.clone(),
            source,
        })?;
    let body = json!({
        "schema_version": 1,
        "status": "operator_action_required",
        "operation": operation,
        "runbook_path": readiness.runbook_path,
        "command": maintenance_command_json(command),
    });
    serde_json::to_writer_pretty(&mut file, &body).map_err(|source| {
        MaintenanceError::PersistArtifact {
            path: path.clone(),
            source: std::io::Error::other(source),
        }
    })?;
    file.write_all(b"\n")
        .and_then(|()| file.sync_all())
        .map_err(|source| MaintenanceError::PersistArtifact {
            path: path.clone(),
            source,
        })?;
    Ok(path)
}

fn maintenance_operation(command: &MaintenanceCommand) -> &'static str {
    match command {
        MaintenanceCommand::OlderThanTwentyVoidRecovery(_) => "older_than_twenty_void_recovery",
        MaintenanceCommand::DuplicateThreadResolution { .. } => "duplicate_thread_resolution",
        MaintenanceCommand::DamagedThreadReplacement { .. } => "damaged_thread_replacement",
        MaintenanceCommand::OrdinarySeal { .. } => "ordinary_seal",
        MaintenanceCommand::StandardSealedEntryCorrection { .. } => {
            "standard_sealed_entry_correction"
        }
        MaintenanceCommand::StandardPriorAdjustmentCorrection { .. } => {
            "standard_prior_adjustment_correction"
        }
    }
}

fn maintenance_command_json(command: &MaintenanceCommand) -> Value {
    match command {
        MaintenanceCommand::OlderThanTwentyVoidRecovery(request) => json!({
            "ledger_id": request.ledger_id.to_string(),
            "target_entry_id": request.target_entry_id.0,
            "actor_id": request.actor_id.0,
            "replacement_recorded_by": request.replacement.replacement_recorded_by.0,
        }),
        MaintenanceCommand::DuplicateThreadResolution {
            ledger_id,
            authoritative_thread_keep,
            retired_thread_ids,
        } => json!({
            "ledger_id": ledger_id.to_string(),
            "authoritative_thread_keep": authoritative_thread_keep.to_string(),
            "retired_thread_ids": transport_channel_ids(retired_thread_ids),
        }),
        MaintenanceCommand::DamagedThreadReplacement {
            retired_ledger_id,
            new_parent_channel_id,
        } => json!({
            "retired_ledger_id": retired_ledger_id.to_string(),
            "new_parent_channel_id": new_parent_channel_id.to_string(),
        }),
        MaintenanceCommand::OrdinarySeal { ledger_id } => json!({
            "ledger_id": ledger_id.to_string(),
        }),
        MaintenanceCommand::StandardSealedEntryCorrection {
            ledger_id,
            target_entry_id,
        }
        | MaintenanceCommand::StandardPriorAdjustmentCorrection {
            ledger_id,
            target_entry_id,
        } => json!({
            "ledger_id": ledger_id.to_string(),
            "target_entry_id": target_entry_id.0,
        }),
    }
}

fn transport_channel_ids(ids: &[TransportChannelId]) -> Vec<String> {
    ids.iter().map(ToString::to_string).collect()
}

pub fn default_runtime_instance_lock_path() -> PathBuf {
    std::env::temp_dir()
        .join("walicord-ledger-locks")
        .join("runtime-instance.lock")
}

pub fn ensure_artifact_directory(path: &Path) -> Result<(), std::io::Error> {
    create_dir_all(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        fs::{create_dir_all, read_to_string, write},
        sync::atomic::{AtomicU64, Ordering},
    };
    use walicord_application::ledger::maintenance::MaintenanceCommand;
    use walicord_ledger::test_fixtures::ledger_id;

    static TEST_SEQUENCE: AtomicU64 = AtomicU64::new(0);

    fn test_directory(name: &str) -> PathBuf {
        let sequence = TEST_SEQUENCE.fetch_add(1, Ordering::Relaxed);
        std::env::temp_dir().join(format!(
            "walicord-maintenance-{name}-{}-{sequence}",
            std::process::id()
        ))
    }

    fn ready_config(name: &str) -> MaintenanceReadinessConfig {
        let directory = test_directory(name);
        create_dir_all(&directory).expect("artifact directory should be created");
        let runbook_path = directory.join("runbook.md");
        write(&runbook_path, "# Maintenance\n").expect("runbook should be written");
        MaintenanceReadinessConfig {
            artifact_directory: directory.clone(),
            runbook_path,
            runtime_instance_lock_path: directory.join("runtime.lock"),
            quiesced_single_writer_window_confirmed: true,
        }
    }

    #[test]
    fn readiness_rejects_unconfirmed_quiesced_window() {
        let mut config = ready_config("unconfirmed");
        config.quiesced_single_writer_window_confirmed = false;
        let actual = DiscordLedgerMaintenanceRequestEmitter::new(config).readiness();
        assert_eq!(
            actual,
            Err(MaintenanceReadinessError::QuiescedSingleWriterWindowNotConfirmed)
        );
    }

    #[test]
    fn readiness_rejects_missing_runbook() {
        let mut config = ready_config("missing-runbook");
        config.runbook_path = config.artifact_directory.join("missing.md");
        let expected = Err(MaintenanceReadinessError::RunbookUnavailable {
            path: config.runbook_path.clone(),
        });
        let actual = DiscordLedgerMaintenanceRequestEmitter::new(config).readiness();
        assert_eq!(actual, expected);
    }

    #[tokio::test]
    async fn run_persists_auditable_operator_handoff_after_readiness_gate() {
        let config = ready_config("persist");
        let outcome = DiscordLedgerMaintenanceRequestEmitter::new(config)
            .run(MaintenanceCommand::OrdinarySeal {
                ledger_id: ledger_id(77),
            })
            .await
            .expect("maintenance request should persist");
        let artifact = read_to_string(&outcome.artifact_path).expect("artifact should be readable");
        let artifact: Value = serde_json::from_str(&artifact).expect("artifact should be json");

        assert_eq!(outcome.operation, "ordinary_seal");
        assert_eq!(artifact["schema_version"], 1);
        assert_eq!(artifact["status"], "operator_action_required");
        assert_eq!(artifact["operation"], "ordinary_seal");
        assert_eq!(artifact["command"]["ledger_id"], "77");
    }

    #[tokio::test]
    async fn run_rejects_parallel_bot_or_maintenance_process() {
        let config = ready_config("locked");
        let _lock = acquire_instance_lock(config.runtime_instance_lock_path.clone())
            .expect("first lock should succeed");

        let actual = DiscordLedgerMaintenanceRequestEmitter::new(config)
            .run(MaintenanceCommand::OrdinarySeal {
                ledger_id: ledger_id(77),
            })
            .await;

        assert!(matches!(
            actual,
            Err(MaintenanceError::RuntimeInstanceLockUnavailable { .. })
        ));
    }
}
