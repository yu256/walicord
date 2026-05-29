use chrono::{TimeZone, Utc};
use std::time::SystemTime;
use walicord_infrastructure::business_timezone;
use walicord_presentation::discord_ledger::BusinessDateTime;

/// Format a `SystemTime` as a `BusinessDateTime` (`YYYY-MM-DD HH:MM`) in the business
/// timezone. The two `expect`s here are total functions over valid Unix timestamps:
/// `FixedOffset::east_opt` is checked once at module load via `business_timezone`, and
/// every `SystemTime` we get from the runtime clock resolves to a single local time at
/// this offset (no fold / gap because the offset is fixed, not a DST-bearing IANA zone).
pub fn business_datetime_from_system_time(recorded_at: SystemTime) -> BusinessDateTime {
    let recorded_at = chrono::DateTime::<Utc>::from(recorded_at);
    let formatted = business_timezone()
        .timestamp_opt(
            recorded_at.timestamp(),
            recorded_at.timestamp_subsec_nanos(),
        )
        .single()
        .expect("fixed-offset timezone has no fold / gap; single() is total")
        .format("%Y-%m-%d %H:%M")
        .to_string();
    BusinessDateTime::parse(formatted)
        .expect("`%Y-%m-%d %H:%M` is BusinessDateTime's accepted form by construction")
}
