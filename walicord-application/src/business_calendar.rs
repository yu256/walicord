use chrono::FixedOffset;

/// Business date boundaries use fixed JST (UTC+09:00), as required by criterion 258
/// for date / timestamp rendering.
pub const BUSINESS_TIMEZONE_OFFSET_SECONDS: i32 = 9 * 60 * 60;

pub fn business_timezone() -> FixedOffset {
    FixedOffset::east_opt(BUSINESS_TIMEZONE_OFFSET_SECONDS)
        .expect("business timezone offset should stay valid")
}
