use resvg::usvg::{Options, Tree};
use std::sync::LazyLock;
use tiny_skia::Pixmap;

static OPTIONS: LazyLock<Options> = LazyLock::new(|| {
    let mut fontdb = resvg::usvg::fontdb::Database::new();
    fontdb.load_system_fonts();

    Options {
        fontdb: std::sync::Arc::new(fontdb),
        ..Options::default()
    }
});

/// Convert SVG string to PNG bytes
pub fn svg_to_png(svg: &str) -> Option<Vec<u8>> {
    let tree = Tree::from_str(svg, &OPTIONS).ok()?;
    let size = tree.size();
    let width = size.width().ceil() as u32;
    let height = size.height().ceil() as u32;

    let mut pixmap = Pixmap::new(width, height)?;
    resvg::render(&tree, tiny_skia::Transform::default(), &mut pixmap.as_mut());
    pixmap.encode_png().ok()
}
