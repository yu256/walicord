use resvg::usvg::{Options, Tree};
use std::sync::LazyLock;
use tiny_skia::Pixmap;
use walicord_presentation::RenderedSvg;

static OPTIONS: LazyLock<Options> = LazyLock::new(|| {
    let mut fontdb = resvg::usvg::fontdb::Database::new();
    fontdb.load_system_fonts();

    Options {
        fontdb: std::sync::Arc::new(fontdb),
        ..Options::default()
    }
});

#[derive(Debug)]
pub enum SvgRasterizeError {
    Parse,
    PixmapAllocation,
    PngEncode,
}

pub fn svg_to_png(svg: &RenderedSvg) -> Result<Vec<u8>, SvgRasterizeError> {
    let svg_string = svg.to_svg_string();
    let tree = Tree::from_str(&svg_string, &OPTIONS).map_err(|_| SvgRasterizeError::Parse)?;
    let size = tree.size();
    let width = size.width().ceil() as u32;
    let height = size.height().ceil() as u32;

    let mut pixmap = Pixmap::new(width, height).ok_or(SvgRasterizeError::PixmapAllocation)?;
    resvg::render(&tree, tiny_skia::Transform::default(), &mut pixmap.as_mut());
    pixmap
        .encode_png()
        .map_err(|_| SvgRasterizeError::PngEncode)
}
