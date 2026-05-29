use std::fmt;

use walicord_application::ledger::LedgerEffectiveDate;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SafeLiteralText(String);

impl SafeLiteralText {
    pub fn from_note(raw: &str) -> Option<Self> {
        normalize_note_like(raw).map(Self)
    }

    pub fn from_roster_label(raw: &str) -> Option<Self> {
        normalize_roster_label(raw).map(Self)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn into_inner(self) -> String {
        self.0
    }
}

impl fmt::Display for SafeLiteralText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BusinessDateTime(String);

impl BusinessDateTime {
    pub fn parse(raw: impl Into<String>) -> Option<Self> {
        let raw = raw.into();
        let (date, time) = raw.split_once(' ')?;
        LedgerEffectiveDate::new(date.to_owned()).ok()?;
        let (hour, minute) = time.split_once(':')?;
        if hour.len() != 2 || minute.len() != 2 {
            return None;
        }
        let hour: u32 = hour.parse().ok()?;
        let minute: u32 = minute.parse().ok()?;
        if hour > 23 || minute > 59 {
            return None;
        }
        Some(Self(raw))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for BusinessDateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
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
    let host = token
        .split(['/', ':', '?', '#'])
        .next()
        .unwrap_or(token)
        .trim_end_matches([',', '.', ';', '!', '?', ')', ']', '}']);
    (!host.is_empty()).then_some(host)
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

    #[test]
    fn business_date_time_requires_the_fixed_business_timestamp_format() {
        let actual = BusinessDateTime::parse("2026-05-25 18:55");

        assert_eq!(
            actual.expect("timestamp should parse").as_str(),
            "2026-05-25 18:55"
        );
    }

    #[test]
    fn business_date_time_rejects_invalid_shapes_and_ranges() {
        assert_eq!(BusinessDateTime::parse("2026/05/25 18:55"), None);
        assert_eq!(BusinessDateTime::parse("2026-05-25T18:55"), None);
        assert_eq!(BusinessDateTime::parse("2026-05-25 24:00"), None);
        assert_eq!(BusinessDateTime::parse("2026-05-25 18:60"), None);
        assert_eq!(BusinessDateTime::parse("2026-02-30 18:55"), None);
    }
}
