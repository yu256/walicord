use std::{fmt, time::SystemTime};

use chrono::{NaiveDateTime, Utc};
use walicord_application::business_calendar::business_timezone;

use super::budgets::DISCORD_COMPONENT_LABEL_LIMIT;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SafeLiteralText(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum SafeLiteralTextError {
    #[error("note text normalizes to an empty literal")]
    EmptyNote,
}

impl SafeLiteralText {
    pub fn from_note(raw: &str) -> Option<Self> {
        normalize_note_like(raw).map(Self)
    }

    pub fn parse_note(raw: &str) -> Result<Self, SafeLiteralTextError> {
        Self::from_note(raw).ok_or(SafeLiteralTextError::EmptyNote)
    }

    pub fn from_roster_label(raw: &str) -> Option<Self> {
        normalize_roster_label(raw).map(Self)
    }

    /// Use for roster labels assembled from fixed copy, numeric ids, and existing
    /// `SafeLiteralText` values. Untrusted user / Discord text must go through
    /// `from_roster_label`.
    pub fn from_generated_roster_label(raw: impl Into<String>) -> Self {
        Self(raw.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn into_inner(self) -> String {
        self.0
    }

    pub fn truncate_for_component_label(&self) -> Self {
        let actual = self.0.chars().count();
        if actual <= DISCORD_COMPONENT_LABEL_LIMIT {
            return self.clone();
        }

        let keep = DISCORD_COMPONENT_LABEL_LIMIT.saturating_sub(1);
        let prefix: String = self.0.chars().take(keep).collect();
        Self(format!("{prefix}…"))
    }
}

impl fmt::Display for SafeLiteralText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// Business-timezone wall clock minute, stored as a typed `NaiveDateTime` so callers
/// neither round-trip through formatted strings nor depend on the rendered shape.
/// `Display` is the single place that materialises the `YYYY-MM-DD HH:MM` form, and
/// `parse` is the only place that accepts that shape from untrusted input.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BusinessDateTime(NaiveDateTime);

impl BusinessDateTime {
    const FORMAT: &'static str = "%Y-%m-%d %H:%M";

    pub fn parse(raw: &str) -> Option<Self> {
        NaiveDateTime::parse_from_str(raw, Self::FORMAT)
            .ok()
            .map(Self)
    }

    pub fn from_system_time(recorded_at: SystemTime) -> Self {
        let recorded_at = chrono::DateTime::<Utc>::from(recorded_at);
        let local = recorded_at.with_timezone(&business_timezone());
        Self(local.naive_local())
    }
}

impl fmt::Display for BusinessDateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.format(Self::FORMAT).fmt(f)
    }
}

fn is_removed_format_char(ch: char) -> bool {
    matches!(
        ch,
        '\u{00AD}'
            | '\u{034F}'
            | '\u{061C}'
            | '\u{180E}'
            | '\u{FEFF}'
            | '\u{17B4}'
            | '\u{17B5}'
            | '\u{200B}'..='\u{200F}'
            | '\u{202A}'..='\u{202E}'
            | '\u{2060}'..='\u{206F}'
    )
}

fn url_like_host_candidate(token: &str) -> Option<&str> {
    let host =
        first_url_host_segment(token).trim_end_matches([',', '.', ';', '!', '?', ')', ']', '}']);
    (!host.is_empty()).then_some(host)
}

fn first_url_host_segment(token: &str) -> &str {
    match token.find(['/', ':', '?', '#']) {
        Some(index) => &token[..index],
        None => token,
    }
}

fn looks_like_host_token(token: &str) -> bool {
    let Some(host) = url_like_host_candidate(token) else {
        return false;
    };
    let segments: Vec<&str> = host.split('.').collect();
    let Some(tld) = segments.last() else {
        return false;
    };
    segments.len() >= 2
        && tld.len() >= 2
        && tld.chars().all(|ch| ch.is_ascii_alphabetic())
        && segments.iter().all(|segment| {
            !segment.is_empty()
                && segment
                    .chars()
                    .all(|ch| ch.is_ascii_alphanumeric() || ch == '-')
        })
}

fn contains_url_like_token(token: &str) -> bool {
    token
        .split(|ch: char| {
            !ch.is_ascii_alphanumeric() && !matches!(ch, '.' | '-' | ':' | '/' | '?' | '#')
        })
        .any(|candidate| candidate.contains("://") || looks_like_host_token(candidate))
}

fn neutralize_url_like_token(token: &str) -> String {
    token
        .chars()
        .map(|ch| match ch {
            ':' => '：',
            '/' => '／',
            '.' => '．',
            _ => ch,
        })
        .collect()
}

fn normalize_plain_text(
    raw: &str,
    escape_equals: bool,
    escape_parentheses: bool,
    neutralize_urls: bool,
) -> Option<String> {
    let mut normalized = String::new();
    let mut last_was_space = false;

    for ch in raw.chars() {
        if is_removed_format_char(ch) {
            continue;
        }

        let ch = if ch.is_control() || matches!(ch, '\u{2028}' | '\u{2029}') {
            ' '
        } else {
            ch
        };

        if ch.is_whitespace() {
            if !normalized.is_empty() && !last_was_space {
                normalized.push(' ');
            }
            last_was_space = true;
            continue;
        }

        match ch {
            '@' => normalized.push('＠'),
            '<' => normalized.push('＜'),
            '>' => normalized.push('＞'),
            '=' if escape_equals => normalized.push('＝'),
            '(' | ')' if escape_parentheses => {
                normalized.push('\\');
                normalized.push(ch);
            }
            '\\' | '#' | '*' | '_' | '`' | '~' | '|' | '[' | ']' => {
                normalized.push('\\');
                normalized.push(ch);
            }
            _ => normalized.push(ch),
        }
        last_was_space = false;
    }

    let normalized = normalized.trim();
    if normalized.is_empty() {
        return None;
    }

    let normalized = normalized
        .split(' ')
        .map(|token| {
            if neutralize_urls && contains_url_like_token(token) {
                neutralize_url_like_token(token)
            } else {
                token.to_owned()
            }
        })
        .collect::<Vec<_>>()
        .join(" ");

    (!normalized.is_empty()).then_some(normalized)
}

fn normalize_note_like(raw: &str) -> Option<String> {
    normalize_plain_text(raw, false, true, true)
}

fn normalize_roster_label(raw: &str) -> Option<String> {
    normalize_plain_text(raw, true, false, true)
}

#[cfg(test)]
mod tests {
    use super::{BusinessDateTime, SafeLiteralText};

    #[test]
    fn note_normalization_prevents_ping_and_spoofing() {
        let actual = SafeLiteralText::from_note(
            "  @everyone\n# 見出し <@123> https://example.com/a?b=1\u{202E}\u{200B}  ",
        )
        .expect("normalized note should remain non-blank");

        assert_eq!(
            actual.as_str(),
            "＠everyone \\# 見出し ＜＠123＞ https：／／example．com／a?b=1"
        );
    }

    #[test]
    fn blank_normalized_note_is_omitted() {
        let actual = SafeLiteralText::from_note("\u{200B}\n\t ");

        assert_eq!(actual, None);
    }

    #[test]
    fn roster_label_sanitization_keeps_plain_strings_single_line_and_parse_safe() {
        let actual = SafeLiteralText::from_roster_label(" \nA=@everyone <@123> `x`\u{202E} ")
            .expect("normalized label should remain non-blank");

        assert_eq!(actual.as_str(), "A＝＠everyone ＜＠123＞ \\`x\\`");
    }

    #[test]
    fn roster_label_sanitization_neutralizes_url_like_tokens() {
        let actual = SafeLiteralText::from_roster_label("Alice https://example.com/dev")
            .expect("normalized label should remain non-blank");

        assert_eq!(actual.as_str(), "Alice https：／／example．com／dev");
    }

    #[test]
    fn note_normalization_neutralizes_hosts_wrapped_in_markdown_punctuation() {
        let actual = SafeLiteralText::from_note("foo(example.com)")
            .expect("normalized note should remain non-blank");

        assert_eq!(actual.as_str(), "foo\\(example．com\\)");
    }

    #[test]
    fn roster_label_sanitization_neutralizes_hosts_wrapped_in_punctuation() {
        let actual = SafeLiteralText::from_roster_label("foo(example.com)")
            .expect("normalized label should remain non-blank");

        assert_eq!(actual.as_str(), "foo(example．com)");
    }

    #[rstest::rstest]
    #[case::canonical_form("2026-05-25 18:55", Some("2026-05-25 18:55"))]
    #[case::wrong_date_separator("2026/05/25 18:55", None)]
    #[case::missing_space_between_date_and_time("2026-05-25T18:55", None)]
    #[case::hour_out_of_range("2026-05-25 24:00", None)]
    #[case::minute_out_of_range("2026-05-25 18:60", None)]
    #[case::nonexistent_calendar_day("2026-02-30 18:55", None)]
    fn business_date_time_parses_only_the_business_timestamp_format(
        #[case] input: &str,
        #[case] expected_display: Option<&str>,
    ) {
        let actual = BusinessDateTime::parse(input).map(|x| x.to_string());

        assert_eq!(actual.as_deref(), expected_display);
    }
}
