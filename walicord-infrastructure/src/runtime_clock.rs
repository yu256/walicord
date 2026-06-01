use chrono::Utc;
use std::{
    num::NonZeroU64,
    sync::atomic::{AtomicU64, Ordering},
    time::SystemTime,
};
use walicord_application::{
    Clock, InteractionNonce, LedgerIdProvider, NonceProvider,
    business_calendar::business_timezone,
    ledger::{LedgerEffectiveDate, LedgerId},
    settle_up::PreviewInstanceId,
};
use walicord_ledger::LedgerIdIssuer;

/// Production [`Clock`] implementation. `now` returns wall-clock time; the business
/// date is the calendar date at `BUSINESS_TIMEZONE_OFFSET_SECONDS` east of UTC.
pub struct SystemClock;

impl Clock for SystemClock {
    fn now(&self) -> SystemTime {
        SystemTime::now()
    }

    fn today_business_date(&self) -> LedgerEffectiveDate {
        let today = Utc::now().with_timezone(&business_timezone()).date_naive();
        LedgerEffectiveDate::new(today.format("%Y-%m-%d").to_string())
            .expect("today_business_date should always produce a valid YYYY-MM-DD")
    }
}

/// Production [`NonceProvider`]. Interaction nonces are restart-distinct by salting
/// the process start time (criterion 193). Ledger ids use OS entropy because they
/// remain canonical beyond the lifetime of one process.
pub struct ProcessNonceProvider {
    salt: u64,
    interaction_counter: AtomicU64,
    preview_counter: AtomicU64,
    ledger_id_issuer: LedgerIdIssuer,
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
            ledger_id_issuer: LedgerIdIssuer::from_entropy(random_non_zero_u64()),
        }
    }
}

impl LedgerIdProvider for ProcessNonceProvider {
    fn next_ledger_id(&self) -> LedgerId {
        self.ledger_id_issuer.issue()
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

fn random_non_zero_u64() -> NonZeroU64 {
    loop {
        let mut bytes = [0_u8; std::mem::size_of::<u64>()];
        getrandom::fill(&mut bytes).expect("OS entropy should be available");
        if let Some(value) = NonZeroU64::new(u64::from_ne_bytes(bytes)) {
            return value;
        }
    }
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

    #[test]
    fn nonce_provider_returns_distinct_ledger_ids_across_calls() {
        let provider = ProcessNonceProvider::new();
        assert_ne!(provider.next_ledger_id(), provider.next_ledger_id());
    }
}
