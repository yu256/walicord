#![warn(clippy::uninlined_format_args)]

pub mod ocr;
pub mod optimizer;
pub mod parser;

pub use ocr::OcrsReceiptOcr;
pub use optimizer::WalicordSettlementOptimizer;
pub use parser::WalicordProgramParser;
