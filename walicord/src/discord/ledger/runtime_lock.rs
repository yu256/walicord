use fs2::FileExt;
use std::{
    fs::{File, OpenOptions},
    io,
    path::{Path, PathBuf},
};

/// Cross-process startup lock. Held for the lifetime of the bot process to enforce the
/// single-process single-writer model required by criteria 53, 115, 155, and 182. The
/// file descriptor is retained inside the [`InstanceLock`] value; dropping the value
/// releases the OS-level lock.
///
/// This is intentionally the **only** cross-process lock retained by the new Discord
/// ledger implementation. Per-append cross-process locks are forbidden by criterion 182
/// because the application now relies on per-`LedgerId` in-process async mutexes plus
/// this startup lock for write serialization.
pub struct InstanceLock {
    _file: File,
    path: PathBuf,
}

impl InstanceLock {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

#[derive(Debug)]
pub enum InstanceLockError {
    EnsureLockDir(io::Error),
    OpenLockFile(io::Error),
    AcquireExclusive(io::Error),
}

impl std::fmt::Display for InstanceLockError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EnsureLockDir(error) => {
                write!(f, "ロック用ディレクトリを作成できませんでした: {error}")
            }
            Self::OpenLockFile(error) => write!(f, "ロックファイルを開けませんでした: {error}"),
            Self::AcquireExclusive(error) => {
                write!(
                    f,
                    "起動時のインスタンスロックを取得できませんでした: {error}"
                )
            }
        }
    }
}

impl std::error::Error for InstanceLockError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::EnsureLockDir(error)
            | Self::OpenLockFile(error)
            | Self::AcquireExclusive(error) => Some(error),
        }
    }
}

pub fn acquire_instance_lock(path: PathBuf) -> Result<InstanceLock, InstanceLockError> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(InstanceLockError::EnsureLockDir)?;
    }
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(&path)
        .map_err(InstanceLockError::OpenLockFile)?;
    file.try_lock_exclusive()
        .map_err(InstanceLockError::AcquireExclusive)?;
    Ok(InstanceLock { _file: file, path })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static COUNTER: AtomicU64 = AtomicU64::new(0);

    fn fresh_lock_path() -> PathBuf {
        let id = COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!(
            "walicord-runtime-lock-test-{}-{id}.lock",
            std::process::id()
        ))
    }

    #[test]
    fn acquire_succeeds_when_no_other_holder() {
        let path = fresh_lock_path();
        let actual = acquire_instance_lock(path.clone());
        assert!(actual.is_ok(), "first acquire should succeed");
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn second_acquire_fails_while_first_is_alive() {
        let path = fresh_lock_path();
        let first = acquire_instance_lock(path.clone()).expect("first acquire should succeed");

        let second = acquire_instance_lock(path.clone());

        assert!(
            matches!(second, Err(InstanceLockError::AcquireExclusive(_))),
            "second acquire should fail while first is held"
        );
        drop(first);
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn acquire_succeeds_after_previous_lock_is_dropped() {
        let path = fresh_lock_path();
        let first = acquire_instance_lock(path.clone()).expect("first acquire");
        drop(first);

        let actual = acquire_instance_lock(path.clone());

        assert!(actual.is_ok(), "acquire after drop should succeed");
        let _ = std::fs::remove_file(&path);
    }
}
