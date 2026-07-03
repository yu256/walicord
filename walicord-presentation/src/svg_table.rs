use std::{borrow::Cow, fmt::Write, num::NonZeroU32};

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
const COMBINE_SPACING: u32 = 20;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderedSvg {
    body: String,
    width: NonZeroU32,
    height: NonZeroU32,
}

impl RenderedSvg {
    pub fn body(&self) -> &str {
        &self.body
    }

    pub fn width(&self) -> NonZeroU32 {
        self.width
    }

    pub fn height(&self) -> NonZeroU32 {
        self.height
    }

    pub fn to_svg_string(&self) -> String {
        let w = self.width.get();
        let h = self.height.get();
        let mut svg = String::with_capacity(self.body.len() + 256);
        let _ = writeln!(
            &mut svg,
            r#"<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="0 0 {w} {h}">"#
        );
        let _ = writeln!(
            &mut svg,
            r#"<style>text {{ font-family: {FONT_FAMILY}; font-size: {FONT_SIZE}px; }}</style>"#
        );
        svg.push_str(&self.body);
        svg.push_str("</svg>");
        svg
    }
}

pub struct SvgTableBuilder<'a> {
    alignments: Cow<'a, [Alignment]>,
}

pub struct SvgTableReady<'a, const N: usize> {
    headers: [Cow<'a, str>; N],
    rows: Vec<[Cow<'a, str>; N]>,
    alignments: Cow<'a, [Alignment]>,
}

#[derive(Clone, Copy, Default)]
pub enum Alignment {
    #[default]
    Left,
    Center,
    Right,
}

impl<'a> SvgTableBuilder<'a> {
    pub fn new() -> Self {
        Self {
            alignments: Cow::Borrowed(&[]),
        }
    }

    pub fn alignments(mut self, alignments: &'a [Alignment]) -> Self {
        self.alignments = Cow::Borrowed(alignments);
        self
    }

    pub fn headers<const N: usize>(self, headers: [Cow<'a, str>; N]) -> SvgTableReady<'a, N> {
        let alignments = if self.alignments.is_empty() {
            Cow::Owned(vec![Alignment::default(); N])
        } else {
            self.alignments
        };
        SvgTableReady {
            headers,
            rows: Vec::new(),
            alignments,
        }
    }
}

impl<'a> Default for SvgTableBuilder<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, const N: usize> SvgTableReady<'a, N> {
    pub fn row(mut self, row: [Cow<'a, str>; N]) -> Self {
        self.rows.push(row);
        self
    }

    pub fn build(self) -> RenderedSvg {
        let mut col_widths: Vec<u32> = self
            .headers
            .iter()
            .map(|h| estimate_text_width(h))
            .collect();

        for row in &self.rows {
            for (i, cell) in row.iter().enumerate() {
                if i < col_widths.len() {
                    col_widths[i] = col_widths[i].max(estimate_text_width(cell));
                }
            }
        }

        let dimensions = TableDimensions::from_columns_and_rows(&col_widths, self.rows.len());
        let total_width = dimensions.width.get();
        let total_height = dimensions.height.get();

        let mut body = String::with_capacity(4096);

        let _ = writeln!(
            &mut body,
            r#"<rect width="{total_width}" height="{total_height}" fill="{BORDER_COLOR}" rx="4" />"#
        );

        let _ = writeln!(
            &mut body,
            r#"<rect x="1" y="1" width="{}" height="{LINE_HEIGHT}" fill="{HEADER_BG}" rx="3" />"#,
            total_width - 2
        );

        let mut x = CELL_PADDING;
        for (i, header) in self.headers.iter().enumerate() {
            let width = col_widths[i];
            let alignment = alignment_at(&self.alignments, i);
            let text_x = compute_text_x(x, width, alignment);
            let anchor = alignment_anchor(alignment);
            let _ = writeln!(
                &mut body,
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
                &mut body,
                r#"<rect x="1" y="{y}" width="{}" height="{LINE_HEIGHT}" fill="{bg}" />"#,
                total_width - 2
            );

            let mut x = CELL_PADDING;
            for (i, cell) in row.iter().enumerate() {
                if i >= col_widths.len() {
                    break;
                }
                let width = col_widths[i];
                let alignment = alignment_at(&self.alignments, i);
                let text_x = compute_text_x(x, width, alignment);
                let anchor = alignment_anchor(alignment);
                let _ = writeln!(
                    &mut body,
                    r#"<text x="{text_x}" y="{}" fill="{ROW_TEXT}" text-anchor="{anchor}">{}</text>"#,
                    y + LINE_HEIGHT / 2 + FONT_SIZE / 2 - 2,
                    escape_xml(cell)
                );
                x += width + CELL_PADDING;
            }
        }

        RenderedSvg {
            body,
            width: dimensions.width,
            height: dimensions.height,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TableDimensions {
    width: NonZeroU32,
    height: NonZeroU32,
}

impl TableDimensions {
    fn from_columns_and_rows(col_widths: &[u32], row_count: usize) -> Self {
        let width = col_widths.iter().copied().fold(
            NonZeroU32::MIN.saturating_add(CELL_PADDING - 1),
            |width, col_width| width.saturating_add(col_width).saturating_add(CELL_PADDING),
        );
        let height = (0..=row_count).fold(NonZeroU32::MIN.saturating_add(1), |height, _| {
            height.saturating_add(LINE_HEIGHT)
        });
        Self { width, height }
    }
}

pub fn combine_svgs_vertically(svgs: &walicord_domain::NonEmptyVec<RenderedSvg>) -> RenderedSvg {
    let dimensions = SvgStackDimensions::from_svgs(svgs);
    let max_width = dimensions.width.get();

    let base_capacity: usize = svgs.iter().map(|s| s.body().len()).sum();
    let mut body = String::with_capacity(base_capacity + svgs.len() * 64);

    let mut y_offset = 0u32;
    for svg in svgs {
        let x_offset = (max_width - svg.width().get()) / 2;
        let _ = writeln!(
            &mut body,
            r#"<g transform="translate({x_offset}, {y_offset})">"#
        );
        body.push_str(svg.body());
        body.push_str("</g>\n");
        y_offset += svg.height().get() + COMBINE_SPACING;
    }

    RenderedSvg {
        body,
        width: dimensions.width,
        height: dimensions.height,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SvgStackDimensions {
    width: NonZeroU32,
    height: NonZeroU32,
}

impl SvgStackDimensions {
    fn from_svgs(svgs: &walicord_domain::NonEmptyVec<RenderedSvg>) -> Self {
        let first = svgs.first();
        let mut dimensions = Self {
            width: first.width(),
            height: first.height(),
        };

        for svg in svgs.iter().skip(1) {
            dimensions.include_below(svg);
        }

        dimensions
    }

    fn include_below(&mut self, svg: &RenderedSvg) {
        self.width = self.width.max(svg.width());
        self.height = self
            .height
            .saturating_add(COMBINE_SPACING)
            .saturating_add(svg.height().get());
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

fn alignment_at(alignments: &[Alignment], column_index: usize) -> Alignment {
    match alignments.get(column_index) {
        Some(alignment) => *alignment,
        None => Alignment::default(),
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
    use rstest::rstest;

    #[test]
    fn simple_table_produces_valid_rendered_svg() {
        let svg = SvgTableBuilder::new()
            .alignments(&[Alignment::Left, Alignment::Right])
            .headers([Cow::Borrowed("Name"), Cow::Borrowed("Balance")])
            .row([Cow::Borrowed("Alice"), Cow::Borrowed("+100")])
            .row([Cow::Borrowed("Bob"), Cow::Borrowed("-100")])
            .build();

        let xml = svg.to_svg_string();
        assert!(xml.contains("<svg"));
        assert!(xml.contains("</svg>"));
        assert!(xml.contains("Alice"));
        assert!(xml.contains("Bob"));
        assert!(xml.contains("+100"));
        assert!(xml.contains("-100"));
        assert!(svg.width().get() > 0);
        assert!(svg.height().get() > 0);
    }

    #[test]
    fn body_excludes_svg_wrapper_and_style() {
        let svg = SvgTableBuilder::new()
            .headers([Cow::Borrowed("Col")])
            .build();

        assert!(!svg.body().contains("<svg"));
        assert!(!svg.body().contains("</svg>"));
        assert!(!svg.body().contains("<style>"));
        assert!(svg.body().contains("<rect"));
    }

    #[test]
    fn to_svg_string_wraps_body_with_svg_element() {
        let svg = SvgTableBuilder::new()
            .headers([Cow::Borrowed("Col")])
            .build();

        let xml = svg.to_svg_string();
        assert!(xml.starts_with("<svg"));
        assert!(xml.ends_with("</svg>"));
        assert!(xml.contains("<style>"));
        assert!(xml.contains(svg.body()));
    }

    #[test]
    fn malicious_cell_content_is_xml_escaped() {
        let svg = SvgTableBuilder::new()
            .headers([Cow::Borrowed("X")])
            .row([Cow::Borrowed("<script>alert('xss')</script>")])
            .build();

        let xml = svg.to_svg_string();
        assert!(!xml.contains("<script>"));
        assert!(xml.contains("&lt;script&gt;"));
    }

    #[rstest]
    #[case::escapes_all("<test & 'value'>", "&lt;test &amp; &#39;value&#39;&gt;")]
    #[case::keeps_plain("plain", "plain")]
    fn test_escape_xml(#[case] input: &str, #[case] expected: &str) {
        let result = escape_xml(input);
        assert_eq!(result, expected);
    }

    #[test]
    fn combine_single_svg_preserves_content() {
        let svg = SvgTableBuilder::new()
            .headers([Cow::Borrowed("X")])
            .row([Cow::Borrowed("data")])
            .build();

        let svgs = walicord_domain::NonEmptyVec::new(vec![svg.clone()]).expect("non-empty");
        let combined = combine_svgs_vertically(&svgs);
        assert!(combined.to_svg_string().contains("data"));
        assert_eq!(combined.width(), svg.width());
        assert_eq!(combined.height(), svg.height());
    }

    #[test]
    fn combine_two_svgs_uses_max_width_and_summed_height() {
        let a = SvgTableBuilder::new()
            .headers([Cow::Borrowed("Short")])
            .build();
        let b = SvgTableBuilder::new()
            .headers([Cow::Borrowed("A much longer header text")])
            .build();

        let svgs =
            walicord_domain::NonEmptyVec::new(vec![a.clone(), b.clone()]).expect("non-empty");
        let combined = combine_svgs_vertically(&svgs);
        assert_eq!(combined.width(), a.width().max(b.width()));
        let expected_height = a.height().get() + b.height().get() + COMBINE_SPACING;
        assert_eq!(combined.height().get(), expected_height);
    }

    #[test]
    fn combine_centers_narrower_svg() {
        let narrow = SvgTableBuilder::new().headers([Cow::Borrowed("X")]).build();
        let wide = SvgTableBuilder::new()
            .headers([Cow::Borrowed("Very wide column header")])
            .build();

        let svgs = walicord_domain::NonEmptyVec::new(vec![narrow.clone(), wide.clone()])
            .expect("non-empty");
        let combined = combine_svgs_vertically(&svgs);
        let x_offset = (wide.width().get() - narrow.width().get()) / 2;
        assert!(
            combined
                .body()
                .contains(&format!("translate({x_offset}, 0)"))
        );
    }

    #[test]
    fn header_only_table_has_positive_dimensions() {
        let svg = SvgTableBuilder::new()
            .headers([Cow::Borrowed("A"), Cow::Borrowed("B")])
            .build();

        assert!(svg.width().get() > 0);
        assert!(svg.height().get() > 0);
        assert!(svg.to_svg_string().contains("A"));
        assert!(svg.to_svg_string().contains("B"));
    }
}
