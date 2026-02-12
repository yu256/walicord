#![warn(clippy::uninlined_format_args)]

pub mod optimizer;
pub mod parser;

pub use optimizer::WalicordSettlementOptimizer;
pub use parser::WalicordProgramParser;
