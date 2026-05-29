use std::{
    collections::HashMap,
    hash::Hash,
    sync::{Arc, Mutex},
};
use tokio::sync::watch;

/// Coalesces concurrent same-key read operations into a single in-flight execution.
/// Satisfies criterion 243 ("`same_ledger_reads_singleflight`"): when multiple
/// `/review`, `/ledger`, panel `清算確認`, or panel `台帳` requests target the same
/// verified-load key concurrently, exactly one underlying load runs and every waiter
/// receives the same outcome (success, failure, or leader-cancelled).
///
/// The shared result is wrapped in `Arc` so the inner `Result<V, E>` does not need to
/// be `Clone`.
type SharedResult<V, E> = Arc<Result<V, E>>;
type ResultReceiver<V, E> = watch::Receiver<Option<SharedResult<V, E>>>;
type ResultSender<V, E> = watch::Sender<Option<SharedResult<V, E>>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SingleflightOutcome<V, E> {
    /// Leader's `work()` completed and published its result; all callers receive this.
    Loaded(Arc<Result<V, E>>),
    /// The leader task was cancelled / panicked before publishing a result. Waiters
    /// receive this outcome rather than panicking, and the key is cleaned up so the
    /// next caller starts fresh.
    LeaderCancelled,
}

pub struct ReadSingleflight<K, V, E> {
    in_flight: Mutex<HashMap<K, ResultReceiver<V, E>>>,
}

impl<K, V, E> Default for ReadSingleflight<K, V, E>
where
    K: Eq + Hash + Clone + Send + Sync + 'static,
    V: Send + Sync + 'static,
    E: Send + Sync + 'static,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V, E> ReadSingleflight<K, V, E>
where
    K: Eq + Hash + Clone + Send + Sync + 'static,
    V: Send + Sync + 'static,
    E: Send + Sync + 'static,
{
    pub fn new() -> Self {
        Self {
            in_flight: Mutex::new(HashMap::new()),
        }
    }

    pub async fn do_or_wait<F, Fut>(self: &Arc<Self>, key: K, work: F) -> SingleflightOutcome<V, E>
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = Result<V, E>>,
    {
        enum Slot<V, E> {
            Leader(ResultSender<V, E>),
            Waiter(ResultReceiver<V, E>),
        }

        let slot: Slot<V, E> = {
            let mut map = self
                .in_flight
                .lock()
                .expect("ReadSingleflight mutex poisoned");
            if let Some(rx) = map.get(&key) {
                Slot::Waiter(rx.clone())
            } else {
                let (tx, rx) = watch::channel(None);
                map.insert(key.clone(), rx);
                Slot::Leader(tx)
            }
        };

        match slot {
            Slot::Leader(tx) => {
                let guard = LeaderGuard {
                    sf: Arc::clone(self),
                    key: Some(key),
                };
                let result = Arc::new(work().await);
                let _ = tx.send(Some(Arc::clone(&result)));
                drop(guard);
                SingleflightOutcome::Loaded(result)
            }
            Slot::Waiter(mut rx) => loop {
                if let Some(result) = rx.borrow().clone() {
                    return SingleflightOutcome::Loaded(result);
                }
                if rx.changed().await.is_err() {
                    return SingleflightOutcome::LeaderCancelled;
                }
            },
        }
    }

    fn remove(&self, key: &K) {
        self.in_flight
            .lock()
            .expect("ReadSingleflight mutex poisoned")
            .remove(key);
    }
}

/// RAII guard that cleans the in-flight map when the leader scope exits, including
/// abort/panic paths. If the leader publishes a result and then drops the guard
/// normally, the key is also removed so the next caller starts fresh.
struct LeaderGuard<K, V, E>
where
    K: Eq + Hash + Clone + Send + Sync + 'static,
    V: Send + Sync + 'static,
    E: Send + Sync + 'static,
{
    sf: Arc<ReadSingleflight<K, V, E>>,
    key: Option<K>,
}

impl<K, V, E> Drop for LeaderGuard<K, V, E>
where
    K: Eq + Hash + Clone + Send + Sync + 'static,
    V: Send + Sync + 'static,
    E: Send + Sync + 'static,
{
    fn drop(&mut self) {
        if let Some(key) = self.key.take() {
            self.sf.remove(&key);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use tokio::sync::Barrier;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct Loaded(u64);

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum LoadError {
        Boom,
    }

    fn singleflight() -> Arc<ReadSingleflight<u64, Loaded, LoadError>> {
        Arc::new(ReadSingleflight::new())
    }

    #[tokio::test]
    async fn single_caller_runs_work_once_and_returns_result() {
        let sf = singleflight();
        let actual = sf.do_or_wait(1, || async { Ok(Loaded(42)) }).await;
        assert_eq!(
            actual,
            SingleflightOutcome::Loaded(Arc::new(Ok(Loaded(42))))
        );
    }

    #[tokio::test]
    async fn concurrent_callers_share_one_execution_and_one_result() {
        let sf = singleflight();
        let invocations = Arc::new(AtomicUsize::new(0));
        let barrier = Arc::new(Barrier::new(3));

        let mut tasks = Vec::new();
        for _ in 0..3 {
            let sf = Arc::clone(&sf);
            let counter = Arc::clone(&invocations);
            let gate = Arc::clone(&barrier);
            tasks.push(tokio::spawn(async move {
                gate.wait().await;
                sf.do_or_wait(7, || async {
                    counter.fetch_add(1, Ordering::SeqCst);
                    tokio::time::sleep(std::time::Duration::from_millis(5)).await;
                    Ok(Loaded(99))
                })
                .await
            }));
        }

        let mut results = Vec::new();
        for task in tasks {
            results.push(task.await.expect("task should join"));
        }

        assert_eq!(invocations.load(Ordering::SeqCst), 1);
        for result in results {
            assert_eq!(
                result,
                SingleflightOutcome::Loaded(Arc::new(Ok(Loaded(99))))
            );
        }
    }

    #[tokio::test]
    async fn waiters_receive_failure_when_leader_returns_err() {
        let sf = singleflight();
        let barrier = Arc::new(Barrier::new(2));

        let sf_a = Arc::clone(&sf);
        let gate_a = Arc::clone(&barrier);
        let task_a = tokio::spawn(async move {
            gate_a.wait().await;
            sf_a.do_or_wait(1, || async {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
                Err(LoadError::Boom)
            })
            .await
        });

        let sf_b = Arc::clone(&sf);
        let gate_b = Arc::clone(&barrier);
        let task_b = tokio::spawn(async move {
            gate_b.wait().await;
            tokio::time::sleep(std::time::Duration::from_millis(1)).await;
            sf_b.do_or_wait(1, || async { Ok(Loaded(0)) }).await
        });

        let (a, b) = tokio::join!(task_a, task_b);
        assert_eq!(
            a.expect("a joins"),
            SingleflightOutcome::Loaded(Arc::new(Err(LoadError::Boom)))
        );
        assert_eq!(
            b.expect("b joins"),
            SingleflightOutcome::Loaded(Arc::new(Err(LoadError::Boom)))
        );
    }

    #[tokio::test]
    async fn waiters_receive_leader_cancelled_when_leader_task_is_aborted() {
        let sf = singleflight();

        let sf_leader = Arc::clone(&sf);
        let leader = tokio::spawn(async move {
            let _ = sf_leader
                .do_or_wait(1, || async {
                    tokio::time::sleep(std::time::Duration::from_secs(60)).await;
                    Ok(Loaded(0))
                })
                .await;
        });

        tokio::time::sleep(std::time::Duration::from_millis(20)).await;

        let sf_waiter = Arc::clone(&sf);
        let waiter =
            tokio::spawn(async move { sf_waiter.do_or_wait(1, || async { Ok(Loaded(0)) }).await });

        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        leader.abort();
        let _ = leader.await;

        let outcome = waiter.await.expect("waiter joins");

        assert_eq!(outcome, SingleflightOutcome::LeaderCancelled);
    }

    #[tokio::test]
    async fn key_is_cleaned_up_after_leader_cancellation_so_next_caller_runs_fresh() {
        let sf = singleflight();

        let sf_leader = Arc::clone(&sf);
        let leader = tokio::spawn(async move {
            let _ = sf_leader
                .do_or_wait(1, || async {
                    tokio::time::sleep(std::time::Duration::from_secs(60)).await;
                    Ok(Loaded(0))
                })
                .await;
        });
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        leader.abort();
        let _ = leader.await;

        let invocations = Arc::new(AtomicUsize::new(0));
        let counter = Arc::clone(&invocations);
        let actual = sf
            .do_or_wait(1, move || {
                let counter = Arc::clone(&counter);
                async move {
                    counter.fetch_add(1, Ordering::SeqCst);
                    Ok(Loaded(7))
                }
            })
            .await;

        assert_eq!(invocations.load(Ordering::SeqCst), 1);
        assert_eq!(actual, SingleflightOutcome::Loaded(Arc::new(Ok(Loaded(7)))));
    }

    #[tokio::test]
    async fn separate_keys_run_in_parallel() {
        let sf = singleflight();
        let invocations = Arc::new(AtomicUsize::new(0));

        let mut tasks = Vec::new();
        for key in 0..3 {
            let sf = Arc::clone(&sf);
            let counter = Arc::clone(&invocations);
            tasks.push(tokio::spawn(async move {
                sf.do_or_wait(key, || async {
                    counter.fetch_add(1, Ordering::SeqCst);
                    Ok(Loaded(key))
                })
                .await
            }));
        }

        let mut payloads = Vec::new();
        for task in tasks {
            match task.await.expect("task joins") {
                SingleflightOutcome::Loaded(result) => match result.as_ref() {
                    Ok(Loaded(value)) => payloads.push(*value),
                    Err(_) => unreachable!("loads should succeed in this test"),
                },
                SingleflightOutcome::LeaderCancelled => unreachable!("leader did not cancel"),
            }
        }

        assert_eq!(invocations.load(Ordering::SeqCst), 3);
        payloads.sort_unstable();
        assert_eq!(payloads, vec![0, 1, 2]);
    }

    #[tokio::test]
    async fn second_round_after_first_completes_executes_fresh_work() {
        let sf = singleflight();

        let first = sf.do_or_wait(1, || async { Ok(Loaded(10)) }).await;
        let second = sf.do_or_wait(1, || async { Ok(Loaded(20)) }).await;

        assert_eq!(first, SingleflightOutcome::Loaded(Arc::new(Ok(Loaded(10)))));
        assert_eq!(
            second,
            SingleflightOutcome::Loaded(Arc::new(Ok(Loaded(20))))
        );
    }
}
