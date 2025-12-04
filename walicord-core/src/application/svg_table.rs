use std::{borrow::Cow, fmt::Write};

const FONT_SIZE: u32 = 14;
const CELL_PADDING: u32 = 10;
const LINE_HEIGHT: u32 = FONT_SIZE + CELL_PADDING * 2;
const HEADER_BG: &str = "#4a5568";
const HEADER_TEXT: &str = "#ffffff";
const ROW_BG_EVEN: &str = "#f7fafc";
const ROW_BG_ODD: &str = "#edf2f7";
const ROW_TEXT: &str = "#1a202c";
const BORDER_COLOR: &str = "#cbd5e0";
const FONT_FAMILY: &str = "system-ui, -apple-system, sans-serif";
const CHAR_WIDTH: f32 = 8.5;

#[derive(Default)]
pub struct SvgTableBuilder<'a, Seq> {
    headers: &'a [Cow<'a, str>],
    rows: Vec<Seq>,
    alignments: Cow<'a, [Alignment]>,
}

#[derive(Clone, Copy, Default)]
pub enum Alignment {
    #[default]
    Left,
    Center,
    Right,
}

impl<'a, Seq> SvgTableBuilder<'a, Seq>
where
    Seq: AsRef<[Cow<'a, str>]> + Default,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alignments(mut self, alignments: &'a [Alignment]) -> Self {
        self.alignments = Cow::Borrowed(alignments);
        self
    }

    pub fn headers(mut self, headers: &'a [Cow<'a, str>]) -> Self {
        self.headers = headers;
        if self.alignments.is_empty() {
            self.alignments = Cow::Owned(vec![Alignment::default(); self.headers.len()]);
        }
        self
    }

    pub fn row(mut self, row: Seq) -> Self {
        self.rows.push(row);
        self
    }

    pub fn rows(mut self, rows: impl IntoIterator<Item = Seq>) -> Self {
        self.rows.extend(rows);
        self
    }

    pub fn build(self) -> String {
        let col_count = self.headers.len();
        if col_count == 0 {
            return String::new();
        }

        let mut col_widths: Vec<u32> = self
            .headers
            .iter()
            .map(|h| estimate_text_width(h))
            .collect();

        for row in &self.rows {
            for (i, cell) in row.as_ref().iter().enumerate() {
                if i < col_widths.len() {
                    col_widths[i] = col_widths[i].max(estimate_text_width(cell));
                }
            }
        }

        let total_width: u32 =
            col_widths.iter().sum::<u32>() + (col_count as u32 + 1) * CELL_PADDING;
        let total_height: u32 = LINE_HEIGHT * (1 + self.rows.len() as u32) + 2;

        let mut svg = String::with_capacity(4096);
        let _ = writeln!(
            &mut svg,
            r#"<svg xmlns="http://www.w3.org/2000/svg" width="{total_width}" height="{total_height}" viewBox="0 0 {total_width} {total_height}">"#
        );
        let _ = writeln!(
            &mut svg,
            r#"<style>text {{ font-family: {FONT_FAMILY}; font-size: {FONT_SIZE}px; }}</style>"#
        );

        let _ = writeln!(
            &mut svg,
            r#"<rect width="{total_width}" height="{total_height}" fill="{BORDER_COLOR}" rx="4" />"#
        );

        let _ = writeln!(
            &mut svg,
            r#"<rect x="1" y="1" width="{}" height="{LINE_HEIGHT}" fill="{HEADER_BG}" rx="3" />"#,
            total_width - 2
        );

        let mut x = CELL_PADDING;
        for (i, header) in self.headers.iter().enumerate() {
            let width = col_widths[i];
            let text_x = compute_text_x(
                x,
                width,
                self.alignments.get(i).copied().unwrap_or_default(),
            );
            let anchor = alignment_anchor(self.alignments.get(i).copied().unwrap_or_default());
            let _ = writeln!(
                &mut svg,
                r#"<text x="{text_x}" y="{}" fill="{HEADER_TEXT}" text-anchor="{anchor}">{}</text>"#,
                LINE_HEIGHT / 2 + FONT_SIZE / 2 - 2,
                escape_xml(header)
            );
            x += width + CELL_PADDING;
        }

        for (row_idx, row) in self.rows.iter().enumerate() {
            let y = LINE_HEIGHT * (1 + row_idx as u32) + 1;
            let bg = if row_idx % 2 == 0 {
                ROW_BG_EVEN
            } else {
                ROW_BG_ODD
            };
            let _ = writeln!(
                &mut svg,
                r#"<rect x="1" y="{y}" width="{}" height="{LINE_HEIGHT}" fill="{bg}" />"#,
                total_width - 2
            );

            let mut x = CELL_PADDING;
            for (i, cell) in row.as_ref().iter().enumerate() {
                if i >= col_widths.len() {
                    break;
                }
                let width = col_widths[i];
                let text_x = compute_text_x(
                    x,
                    width,
                    self.alignments.get(i).copied().unwrap_or_default(),
                );
                let anchor = alignment_anchor(self.alignments.get(i).copied().unwrap_or_default());
                let _ = writeln!(
                    &mut svg,
                    r#"<text x="{text_x}" y="{}" fill="{ROW_TEXT}" text-anchor="{anchor}">{}</text>"#,
                    y + LINE_HEIGHT / 2 + FONT_SIZE / 2 - 2,
                    escape_xml(cell)
                );
                x += width + CELL_PADDING;
            }
        }

        svg.push_str("</svg>");
        svg
    }
}

fn estimate_text_width(text: &str) -> u32 {
    let width = text
        .chars()
        .map(|c| {
            if c.is_ascii() {
                CHAR_WIDTH
            } else {
                CHAR_WIDTH * 2.0
            }
        })
        .sum::<f32>();
    (width.ceil() as u32).max(20) + CELL_PADDING * 2
}

fn compute_text_x(cell_x: u32, cell_width: u32, alignment: Alignment) -> u32 {
    match alignment {
        Alignment::Left => cell_x,
        Alignment::Center => cell_x + cell_width / 2,
        Alignment::Right => cell_x + cell_width,
    }
}

fn alignment_anchor(alignment: Alignment) -> &'static str {
    match alignment {
        Alignment::Left => "start",
        Alignment::Center => "middle",
        Alignment::Right => "end",
    }
}

fn escape_xml(s: &str) -> Cow<'_, str> {
    if !s.contains(['&', '<', '>', '"', '\'']) {
        return Cow::Borrowed(s);
    }

    let mut result = String::with_capacity(s.len() + 10);
    for c in s.chars() {
        match c {
            '&' => result.push_str("&amp;"),
            '<' => result.push_str("&lt;"),
            '>' => result.push_str("&gt;"),
            '"' => result.push_str("&quot;"),
            '\'' => result.push_str("&#39;"),
            _ => result.push(c),
        }
    }
    Cow::Owned(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_table() {
        let svg = SvgTableBuilder::new()
            .alignments(&[Alignment::Left, Alignment::Right])
            .headers(&[Cow::Borrowed("Name"), Cow::Borrowed("Balance")])
            .row([Cow::Borrowed("Alice"), Cow::Borrowed("+100")])
            .row([Cow::Borrowed("Bob"), Cow::Borrowed("-100")])
            .build();

        assert!(svg.contains("<svg"));
        assert!(svg.contains("</svg>"));
        assert!(svg.contains("Alice"));
        assert!(svg.contains("Bob"));
        assert!(svg.contains("+100"));
        assert!(svg.contains("-100"));
    }

    #[test]
    fn test_escape_xml() {
        let result = escape_xml("<test & 'value'>");
        assert_eq!(result, "&lt;test &amp; &#39;value&#39;&gt;");
    }
}
