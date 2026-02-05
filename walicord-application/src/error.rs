#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProgramParseError<'a> {
    MissingMembersDeclaration,
    UndefinedMember { name: &'a str, line: usize },
    FailedToEvaluateGroup { name: &'a str },
    SyntaxError(String),
}

impl<'a> From<ProgramBuildError<'a>> for ProgramParseError<'a> {
    fn from(err: ProgramBuildError<'a>) -> Self {
        match err {
            ProgramBuildError::MissingMembersDeclaration => {
                ProgramParseError::MissingMembersDeclaration
            }
            ProgramBuildError::UndefinedMember { name, line } => {
                ProgramParseError::UndefinedMember { name, line }
            }
            ProgramBuildError::FailedToEvaluateGroup { name, .. } => {
                ProgramParseError::FailedToEvaluateGroup { name }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum SettlementOptimizationError {
    #[error("imbalanced total: {0}")]
    ImbalancedTotal(i64),
    #[error("no solution")]
    NoSolution,
}

use std::{error::Error as StdError, path::PathBuf};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ReceiptOcrError {
    #[error("model load failed ({path}): {source}")]
    ModelLoad {
        path: PathBuf,
        #[source]
        source: Box<dyn StdError + Send + Sync>,
    },
    #[error("image decode failed: {source}")]
    ImageDecode {
        #[source]
        source: Box<dyn StdError + Send + Sync>,
    },
    #[error("ocr engine init failed: {source}")]
    EngineInit {
        #[source]
        source: Box<dyn StdError + Send + Sync>,
    },
    #[error("ocr run failed: {source}")]
    OcrRun {
        #[source]
        source: Box<dyn StdError + Send + Sync>,
    },
}

#[derive(Debug, Error)]
pub enum ReceiptResolveError {
    #[error("ocr unavailable on line {line}")]
    OcrUnavailable { line: usize },
    #[error("missing attachment on line {line} (index {index})")]
    MissingAttachment { line: usize, index: usize },
    #[error("ocr failed: {source}")]
    OcrFailed { source: ReceiptOcrError },
    #[error("total not found on line {line}")]
    TotalNotFound { line: usize },
    #[error("total ambiguous on line {line}")]
    TotalAmbiguous { line: usize },
}

impl PartialEq for ReceiptResolveError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::OcrUnavailable { line: a }, Self::OcrUnavailable { line: b }) => a == b,
            (
                Self::MissingAttachment { line: a, index: ai },
                Self::MissingAttachment { line: b, index: bi },
            ) => a == b && ai == bi,
            (Self::OcrFailed { .. }, Self::OcrFailed { .. }) => true,
            (Self::TotalNotFound { line: a }, Self::TotalNotFound { line: b }) => a == b,
            (Self::TotalAmbiguous { line: a }, Self::TotalAmbiguous { line: b }) => a == b,
            _ => false,
        }
    }
}
impl Eq for ReceiptResolveError {}

#[derive(Debug, Error)]
pub enum SettlementBuildError {
    #[error("settlement optimization failed: {0}")]
    Optimization(#[from] SettlementOptimizationError),
    #[error("receipt resolve failed: {0}")]
    Receipt(#[from] ReceiptResolveError),
}
use walicord_domain::ProgramBuildError;
