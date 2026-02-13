use std::borrow::Cow;
use walicord_domain::{ProgramBuildError, SettlementRoundingError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProgramParseError<'a> {
    FailedToEvaluateGroup { name: Cow<'a, str>, line: usize },
    UndefinedGroup { name: Cow<'a, str>, line: usize },
    UndefinedMember { id: u64, line: usize },
    SyntaxError { line: usize, detail: String },
    MissingContextForImplicitPayment { line: usize },
    InvalidAmountExpression { line: usize, detail: String },
}

impl<'a> From<ProgramBuildError<'a>> for ProgramParseError<'a> {
    fn from(err: ProgramBuildError<'a>) -> Self {
        match err {
            ProgramBuildError::UndefinedGroup { name, line } => ProgramParseError::UndefinedGroup {
                name: Cow::Borrowed(name),
                line,
            },
            ProgramBuildError::FailedToEvaluateGroup { name, line } => {
                ProgramParseError::FailedToEvaluateGroup {
                    name: Cow::Borrowed(name),
                    line,
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SettlementOptimizationError {
    ImbalancedTotal(i64),
    NoSolution,
    RoundingMismatch,
    QuantizationImbalancedTotal { total: walicord_domain::Money },
    QuantizationInvalidAdjustmentCount,
    QuantizationInsufficientCandidates,
    QuantizationZeroSumInvariantViolation,
    QuantizationNonIntegral,
}

impl From<SettlementRoundingError> for SettlementOptimizationError {
    fn from(err: SettlementRoundingError) -> Self {
        match err {
            SettlementRoundingError::ImbalancedTotal(total) => {
                SettlementOptimizationError::QuantizationImbalancedTotal { total }
            }
            SettlementRoundingError::InvalidAdjustmentCount => {
                SettlementOptimizationError::QuantizationInvalidAdjustmentCount
            }
            SettlementRoundingError::InsufficientCandidates => {
                SettlementOptimizationError::QuantizationInsufficientCandidates
            }
            SettlementRoundingError::ZeroSumInvariantViolation => {
                SettlementOptimizationError::QuantizationZeroSumInvariantViolation
            }
            SettlementRoundingError::NonIntegral => {
                SettlementOptimizationError::QuantizationNonIntegral
            }
        }
    }
}
