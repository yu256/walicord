use std::time::{Duration, SystemTime};

pub(super) fn non_negative_elapsed_since(now: SystemTime, earlier: SystemTime) -> Duration {
    match now.duration_since(earlier) {
        Ok(duration) => duration,
        Err(_clock_moved_back) => Duration::ZERO,
    }
}
