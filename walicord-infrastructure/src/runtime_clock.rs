use chrono::Utc;
use std::{
    sync::atomic::{AtomicU64, Ordering},
    time::SystemTime,
};
use walicord_application::{
    Clock, InteractionNonce, NonceProvider, business_calendar::business_timezone,
    ledger::LedgerEffectiveDate, settle_up::PreviewInstanceId,
};

/// Production [`Clock`] implementation. `now` returns wall-clock time; the business
/// date is the calendar date at `BUSINESS_TIMEZONE_OFFSET_SECONDS` east of UTC.
pub struct SystemClock;

impl Clock for SystemClock {
    fn now(&self) -> SystemTime {
        SystemTime::now()
    }

    fn today_business_date(&self) -> LedgerEffectiveDate {
        let today = Utc::now().with_timezone(&business_timezone()).date_naive();
        LedgerEffectiveDate::from_naive_date(today)
    }
}

/// Production [`NonceProvider`]. Interaction nonces are restart-distinct by salting
/// the process start time (criterion 193).
pub struct ProcessNonceProvider {
    salt: u64,
    interaction_counter: AtomicU64,
    preview_counter: AtomicU64,
}

impl Default for ProcessNonceProvider {
    fn default() -> Self {
        Self::new()
    }
}

impl ProcessNonceProvider {
    pub fn new() -> Self {
        let salt = system_time_nanos_since_epoch();
        Self {
            salt,
            interaction_counter: AtomicU64::new(1),
            preview_counter: AtomicU64::new(1),
        }
    }
}

impl NonceProvider for ProcessNonceProvider {
    fn next_interaction_nonce(&self) -> InteractionNonce {
        let counter = self.interaction_counter.fetch_add(1, Ordering::Relaxed);
        InteractionNonce::new(mix(self.salt, counter)).expect("non-zero by construction")
    }

    fn next_preview_instance_id(&self) -> PreviewInstanceId {
        let counter = self.preview_counter.fetch_add(1, Ordering::Relaxed);
        PreviewInstanceId::new(mix(self.salt, counter)).expect("non-zero by construction")
    }
}

fn mix(salt: u64, counter: u64) -> u64 {
    let combined = salt.wrapping_add(counter).wrapping_mul(0x9E3779B97F4A7C15);
    combined.max(1)
}

fn system_time_nanos_since_epoch() -> u64 {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|duration| duration.as_nanos() as u64)
        .unwrap_or(1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn system_clock_today_business_date_is_iso_yyyy_mm_dd() {
        let s = SystemClock.today_business_date().to_string();
        assert_eq!(s.len(), 10);
        assert!(s.as_bytes()[4] == b'-' && s.as_bytes()[7] == b'-');
    }

    #[test]
    fn nonce_provider_returns_non_zero_distinct_values_across_calls() {
        let provider = ProcessNonceProvider::new();
        let first = provider.next_interaction_nonce();
        let second = provider.next_interaction_nonce();
        assert_ne!(first, second);
    }

    #[test]
    fn nonce_provider_returns_non_zero_distinct_preview_instance_ids() {
        let provider = ProcessNonceProvider::new();
        let first = provider.next_preview_instance_id();
        let second = provider.next_preview_instance_id();
        assert_ne!(first, second);
    }
}
