use chrono::FixedOffset;

/// JST. Business date boundaries align with Asia/Tokyo because the only deployment
/// today targets that audience; criterion 258 fixes the business timezone choice for
/// every date / timestamp render.
pub const BUSINESS_TIMEZONE_OFFSET_SECONDS: i32 = 9 * 60 * 60;

pub fn business_timezone() -> FixedOffset {
    FixedOffset::east_opt(BUSINESS_TIMEZONE_OFFSET_SECONDS)
        .expect("business timezone offset should stay valid")
}
