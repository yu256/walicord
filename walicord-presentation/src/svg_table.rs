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
const FONT_FAMILY: &str = "Noto Sans CJK JP";
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

pub fn combine_svgs_vertically(svgs: &[&str]) -> Option<String> {
    if svgs.is_empty() {
        return None;
    }

    const SPACING: u32 = 20;
    const SVG_WRAPPER_OVERHEAD: usize = 512;
    const GROUP_TAG_OVERHEAD: usize = 64;

    let mut total_height = 0u32;
    let mut max_width = 0u32;
    let mut svg_data = Vec::new();

    for svg in svgs {
        let width = extract_svg_dimension(svg, "width")?;
        let height = extract_svg_dimension(svg, "height")?;
        let content = extract_svg_content(svg)?;

        max_width = max_width.max(width);
        svg_data.push((width, height, content));
    }

    for (idx, (_, height, _)) in svg_data.iter().enumerate() {
        total_height += height;
        if idx > 0 {
            total_height += SPACING;
        }
    }

    let base_capacity = svgs.iter().map(|s| s.len()).sum::<usize>();
    let group_overhead = svg_data.len() * GROUP_TAG_OVERHEAD;
    let mut combined = String::with_capacity(base_capacity + SVG_WRAPPER_OVERHEAD + group_overhead);

    let _ = writeln!(
        &mut combined,
        r#"<svg xmlns="http://www.w3.org/2000/svg" width="{max_width}" height="{total_height}" viewBox="0 0 {max_width} {total_height}">"#
    );
    let _ = writeln!(
        &mut combined,
        r#"<style>text {{ font-family: {FONT_FAMILY}; font-size: {FONT_SIZE}px; }}</style>"#
    );

    let mut y_offset = 0u32;
    for (width, height, content) in svg_data {
        let x_offset = (max_width - width) / 2;
        let _ = writeln!(
            &mut combined,
            r#"<g transform="translate({x_offset}, {y_offset})">"#
        );
        combined.push_str(&content);
        combined.push_str("</g>\n");
        y_offset += height + SPACING;
    }

    combined.push_str("</svg>");
    Some(combined)
}

fn extract_svg_dimension(svg: &str, attr: &str) -> Option<u32> {
    let pattern = format!("{attr}=\"");
    let start = svg.find(&pattern)? + pattern.len();
    let end = svg[start..].find('"')? + start;
    svg[start..end].parse().ok()
}

fn extract_svg_content(svg: &str) -> Option<Cow<'_, str>> {
    const STYLE_TAG_OPEN: &str = "<style>";
    const STYLE_TAG_OPEN_LEN: usize = STYLE_TAG_OPEN.len();
    const STYLE_TAG_CLOSE: &str = "</style>";
    const STYLE_TAG_CLOSE_LEN: usize = STYLE_TAG_CLOSE.len();

    let start = svg.find('>')? + 1;
    let end = svg.rfind("</svg>")?;
    let content = &svg[start..end];

    let content = if let Some(style_start) = content.find(STYLE_TAG_OPEN)
        && let Some(style_end) = content[style_start + STYLE_TAG_OPEN_LEN..]
            .find(STYLE_TAG_CLOSE)
            .map(|idx| idx + style_start + STYLE_TAG_OPEN_LEN)
    {
        let before_style = &content[..style_start];
        let after_style = &content[style_end + STYLE_TAG_CLOSE_LEN..];
        Cow::Owned(format!("{before_style}{after_style}"))
    } else {
        Cow::Borrowed(content)
    };

    Some(content)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    const SVG_SIMPLE_A: &str = r#"<svg xmlns="http://www.w3.org/2000/svg" width="100" height="50"><text>First</text></svg>"#;
    const SVG_SIMPLE_B: &str = r#"<svg xmlns="http://www.w3.org/2000/svg" width="120" height="60"><text>Second</text></svg>"#;
    const SVG_STYLE_INNER: &str = r#"<svg xmlns="http://www.w3.org/2000/svg" width="100" height="50"><style>.foo { fill: red; }</style><text>First</text></svg>"#;

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

    #[rstest]
    #[case::escapes_all("<test & 'value'>", "&lt;test &amp; &#39;value&#39;&gt;")]
    #[case::keeps_plain("plain", "plain")]
    fn test_escape_xml(#[case] input: &str, #[case] expected: &str) {
        let result = escape_xml(input);
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::empty(vec![], None, &[])]
    #[case::simple(
        vec![SVG_SIMPLE_A, SVG_SIMPLE_B],
        Some(&[
            "<svg",
            "</svg>",
            "First",
            "Second",
            "width=\"120\"",
            "height=\"130\"",
        ][..]),
        &[],
    )]
    #[case::centering(
        vec![SVG_SIMPLE_A, SVG_SIMPLE_B],
        Some(&[
            "transform=\"translate(10, 0)\"",
            "transform=\"translate(0, 70)\"",
        ][..]),
        &[],
    )]
    #[case::strips_style(
        vec![SVG_STYLE_INNER, SVG_SIMPLE_B],
        Some(&["<style>"][..]),
        &["fill: red"],
    )]
    fn test_combine_svgs_vertically_cases(
        #[case] svgs: Vec<&str>,
        #[case] expect_contains: Option<&[&str]>,
        #[case] expect_not: &[&str],
    ) {
        let combined = combine_svgs_vertically(&svgs);
        match expect_contains {
            None => assert!(combined.is_none()),
            Some(expect_contains) => {
                let combined = combined.expect("combined svg");
                for expected in expect_contains {
                    assert!(combined.contains(expected));
                }
                for unexpected in expect_not {
                    assert!(!combined.contains(unexpected));
                }
            }
        }
    }
}
