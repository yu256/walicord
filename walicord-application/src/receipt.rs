use std::collections::HashMap;

use walicord_domain::Money;

use crate::{
    error::{ReceiptOcrError, ReceiptResolveError},
    model::AmountExpr,
    ports::ReceiptOcr,
};

#[derive(Debug, Clone)]
pub struct ReceiptAttachment {
    pub bytes: Vec<u8>,
    pub filename: Option<String>,
    pub content_type: Option<String>,
}

#[derive(Debug, Default)]
pub struct ReceiptContext {
    line_attachments: HashMap<usize, Vec<ReceiptAttachment>>,
}

impl ReceiptContext {
    pub fn insert_line_attachments(&mut self, line: usize, attachments: Vec<ReceiptAttachment>) {
        if attachments.is_empty() {
            return;
        }
        self.line_attachments.insert(line, attachments);
    }

    pub fn attachments_for_line(&self, line: usize) -> Option<&[ReceiptAttachment]> {
        self.line_attachments.get(&line).map(|list| list.as_slice())
    }
}

pub struct ReceiptImage<'a> {
    pub bytes: &'a [u8],
    pub filename: Option<&'a str>,
    pub content_type: Option<&'a str>,
}

pub struct OcrText {
    pub text: String,
    pub confidence: Option<i32>,
}

pub fn resolve_amount(
    amount: &AmountExpr,
    line: usize,
    context: &ReceiptContext,
    ocr: Option<&dyn ReceiptOcr>,
) -> Result<Money, ReceiptResolveError> {
    match amount {
        AmountExpr::Literal(value) => Ok(*value),
        AmountExpr::ReceiptRef { index } => {
            let Some(ocr) = ocr else {
                return Err(ReceiptResolveError::OcrUnavailable { line });
            };
            let attachments = context.attachments_for_line(line).ok_or(
                ReceiptResolveError::MissingAttachment {
                    line,
                    index: *index,
                },
            )?;
            let attachment =
                attachments
                    .get(*index)
                    .ok_or(ReceiptResolveError::MissingAttachment {
                        line,
                        index: *index,
                    })?;
            let image = ReceiptImage {
                bytes: &attachment.bytes,
                filename: attachment.filename.as_deref(),
                content_type: attachment.content_type.as_deref(),
            };
            let ocr_text = ocr.extract_text(&image)?;
            resolve_total_from_text(&ocr_text.text, line)
        }
    }
}

fn resolve_total_from_text(text: &str, line: usize) -> Result<Money, ReceiptResolveError> {
    let normalized = normalize_text(text);
    let candidates = extract_candidates(&normalized);
    match select_total(&candidates) {
        Ok(candidate) => Ok(Money::from_i64(candidate.value)),
        Err(TotalSelectionError::NotFound) => Err(ReceiptResolveError::TotalNotFound { line }),
        Err(TotalSelectionError::Ambiguous) => Err(ReceiptResolveError::TotalAmbiguous { line }),
    }
}

fn normalize_text(text: &str) -> String {
    let mut output = String::with_capacity(text.len());
    let chars: Vec<char> = text.chars().collect();

    for (idx, c) in chars.iter().copied().enumerate() {
        let normalized = match c {
            '０'..='９' => char::from_u32((c as u32) - ('０' as u32) + ('0' as u32)).unwrap(),
            '，' => ',',
            '．' => '.',
            '￥' => '¥',
            '\t' | '\r' => ' ',
            '\u{000b}' | '\u{000c}' => ' ',
            'Ｏ' => 'O',
            'ｏ' => 'o',
            'ｌ' => 'l',
            'Ｉ' => 'I',
            _ => c,
        };

        let normalized =
            if matches!(normalized, 'O' | 'o' | 'I' | 'l') && is_adjacent_to_digit(&chars, idx) {
                match normalized {
                    'O' | 'o' => '0',
                    'I' | 'l' => '1',
                    _ => normalized,
                }
            } else {
                normalized
            };

        output.push(normalized);
    }

    output
        .lines()
        .map(|line| line.split_whitespace().collect::<Vec<_>>().join(" "))
        .collect::<Vec<_>>()
        .join("\n")
}

fn is_adjacent_to_digit(chars: &[char], idx: usize) -> bool {
    let prev = idx.checked_sub(1).and_then(|i| chars.get(i)).copied();
    let next = chars.get(idx + 1).copied();
    prev.is_some_and(|c| c.is_ascii_digit()) || next.is_some_and(|c| c.is_ascii_digit())
}

#[derive(Clone, Copy)]
struct AmountCandidate {
    value: i64,
    line_index: usize,
    score: i32,
}

fn extract_candidates(text: &str) -> Vec<AmountCandidate> {
    let mut candidates = Vec::new();

    for (line_index, line) in text.lines().enumerate() {
        let keyword_score = keyword_score(line);
        let currency_bonus = if has_currency_marker(line) { 10 } else { 0 };
        let score = keyword_score + currency_bonus;

        let numbers = scan_numbers(line);
        for value in numbers {
            candidates.push(AmountCandidate {
                value,
                line_index,
                score,
            });
        }
    }

    candidates
}

fn keyword_score(line: &str) -> i32 {
    let upper = line.to_ascii_uppercase();
    if line.contains("総合計") || line.contains("合計") || upper.contains("TOTAL") {
        100
    } else {
        0
    }
}

fn has_currency_marker(line: &str) -> bool {
    let upper = line.to_ascii_uppercase();
    line.contains('¥') || line.contains('円') || upper.contains("JPY")
}

fn scan_numbers(line: &str) -> Vec<i64> {
    let mut values = Vec::new();
    let mut buffer = String::new();

    for c in line.chars().chain(std::iter::once(' ')) {
        if c.is_ascii_digit() || c == ',' {
            buffer.push(c);
        } else if !buffer.is_empty() {
            if let Some(value) = parse_amount_token(&buffer) {
                values.push(value);
            }
            buffer.clear();
        }
    }

    values
}

fn parse_amount_token(token: &str) -> Option<i64> {
    let normalized = token.replace(',', "");
    normalized.parse::<i64>().ok()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TotalSelectionError {
    NotFound,
    Ambiguous,
}

fn select_total(candidates: &[AmountCandidate]) -> Result<AmountCandidate, TotalSelectionError> {
    if candidates.is_empty() {
        return Err(TotalSelectionError::NotFound);
    }

    let mut sorted = candidates.to_vec();
    sorted.sort_by(|a, b| {
        b.score
            .cmp(&a.score)
            .then_with(|| b.line_index.cmp(&a.line_index))
            .then_with(|| b.value.cmp(&a.value))
    });

    let top = sorted[0];
    let ambiguous = sorted.iter().skip(1).any(|candidate| {
        candidate.score == top.score
            && candidate.line_index == top.line_index
            && candidate.value != top.value
    });

    if ambiguous {
        return Err(TotalSelectionError::Ambiguous);
    }

    Ok(top)
}

impl From<ReceiptOcrError> for ReceiptResolveError {
    fn from(err: ReceiptOcrError) -> Self {
        ReceiptResolveError::OcrFailed { source: err }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalize_fullwidth_digits() {
        let text = "１２３４５";
        assert_eq!(normalize_text(text), "12345");
    }

    #[test]
    fn select_total_prefers_keyword_line() {
        let text = "小計 800\n合計 1000";
        let total = resolve_total_from_text(text, 1).expect("total selection failed");
        assert_eq!(total, Money::from_i64(1000));
    }

    #[test]
    fn ambiguous_total_returns_error() {
        let text = "合計 1000 1200";
        let err = resolve_total_from_text(text, 1).expect_err("expected ambiguity error");
        assert_eq!(err, ReceiptResolveError::TotalAmbiguous { line: 1 });
    }
}
