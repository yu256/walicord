use crate::{
    Script,
    error::{ProgramParseError, ReceiptOcrError, SettlementOptimizationError},
    model::PersonBalance,
    receipt::{OcrText, ReceiptImage},
};
use walicord_domain::Transfer;

pub trait ProgramParser: Send + Sync {
    fn parse<'a>(
        &self,
        members: &'a [&'a str],
        content: &'a str,
    ) -> Result<Script<'a>, ProgramParseError<'a>>;
}

pub trait SettlementOptimizer: Send + Sync {
    fn optimize<'a>(
        &self,
        balances: &[PersonBalance<'a>],
    ) -> Result<Vec<Transfer<'a>>, SettlementOptimizationError>;
}

pub trait ReceiptOcr: Send + Sync {
    fn extract_text(&self, image: &ReceiptImage<'_>) -> Result<OcrText, ReceiptOcrError>;
}
