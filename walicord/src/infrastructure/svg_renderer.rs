use resvg::usvg::{Options, Tree};
use std::sync::LazyLock;
use tiny_skia::Pixmap;

static OPTIONS: LazyLock<Options> = LazyLock::new(|| {
    let mut fontdb = resvg::usvg::fontdb::Database::new();
    fontdb.load_system_fonts();

    let mut sans_serif: Option<String> = None;
    let mut serif: Option<String> = None;
    let mut monospace: Option<String> = None;
    let mut fallback: Option<String> = None;

    for face in fontdb.faces() {
        for family in face.families.iter() {
            let name = family.0.as_str();
            // Remember any font as ultimate fallback
            if fallback.is_none() {
                fallback = Some(family.0.clone());
            }
            match name {
                // Windows fonts
                "Arial" | "Helvetica" | "Segoe UI"
                // Linux fonts (Ubuntu Server, Debian, etc.)
                | "Liberation Sans" | "DejaVu Sans" | "Noto Sans" | "Ubuntu"
                | "FreeSans" | "Nimbus Sans" | "Droid Sans"
                    if sans_serif.is_none() =>
                {
                    sans_serif = Some(family.0.clone());
                }
                // Windows fonts
                "Times New Roman" | "Georgia"
                // Linux fonts
                | "Liberation Serif" | "DejaVu Serif" | "Noto Serif"
                | "FreeSerif" | "Nimbus Roman"
                    if serif.is_none() =>
                {
                    serif = Some(family.0.clone());
                }
                // Windows fonts
                "Consolas" | "Courier New"
                // Linux fonts
                | "Liberation Mono" | "DejaVu Sans Mono" | "Noto Sans Mono"
                | "Ubuntu Mono" | "FreeMono" | "Nimbus Mono"
                    if monospace.is_none() =>
                {
                    monospace = Some(family.0.clone());
                }
                _ => {}
            }
        }
    }

    // Set generic font family fallbacks, use any available font as fallback
    if let Some(name) = sans_serif.as_ref().or(fallback.as_ref()) {
        fontdb.set_sans_serif_family(name);
    }
    if let Some(name) = serif.as_ref().or(fallback.as_ref()) {
        fontdb.set_serif_family(name);
    }
    if let Some(name) = monospace.as_ref().or(fallback.as_ref()) {
        fontdb.set_monospace_family(name);
    }

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
