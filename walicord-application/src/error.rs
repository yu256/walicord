use std::borrow::Cow;
use walicord_domain::{ProgramBuildError, SettlementRoundingError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FailureKind {
    UserInput,
    Misconfiguration,
    InternalBug,
}

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
    QuantizationUnsupportedScale { scale: u32, max_supported: u32 },
}

impl SettlementOptimizationError {
    pub fn kind(&self) -> FailureKind {
        match self {
            SettlementOptimizationError::ImbalancedTotal(_)
            | SettlementOptimizationError::NoSolution
            | SettlementOptimizationError::RoundingMismatch
            | SettlementOptimizationError::QuantizationImbalancedTotal { .. }
            | SettlementOptimizationError::QuantizationInvalidAdjustmentCount
            | SettlementOptimizationError::QuantizationInsufficientCandidates
            | SettlementOptimizationError::QuantizationZeroSumInvariantViolation
            | SettlementOptimizationError::QuantizationNonIntegral => FailureKind::InternalBug,
            SettlementOptimizationError::QuantizationUnsupportedScale { .. } => {
                FailureKind::Misconfiguration
            }
        }
    }
}

impl ProgramParseError<'_> {
    pub fn kind(&self) -> FailureKind {
        match self {
            ProgramParseError::MissingContextForImplicitPayment { .. } => {
                FailureKind::Misconfiguration
            }
            ProgramParseError::FailedToEvaluateGroup { .. }
            | ProgramParseError::UndefinedGroup { .. }
            | ProgramParseError::UndefinedMember { .. }
            | ProgramParseError::SyntaxError { .. }
            | ProgramParseError::InvalidAmountExpression { .. } => FailureKind::UserInput,
        }
    }
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
            SettlementRoundingError::UnsupportedScale {
                scale,
                max_supported,
            } => SettlementOptimizationError::QuantizationUnsupportedScale {
                scale,
                max_supported,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn missing_implicit_context_is_misconfiguration() {
        let err = ProgramParseError::MissingContextForImplicitPayment { line: 1 };
        assert_eq!(err.kind(), FailureKind::Misconfiguration);
    }

    #[test]
    fn quantization_error_is_internal_bug() {
        let err = SettlementOptimizationError::QuantizationNonIntegral;
        assert_eq!(err.kind(), FailureKind::InternalBug);
    }

    #[test]
    fn unsupported_scale_is_misconfiguration() {
        let err = SettlementOptimizationError::QuantizationUnsupportedScale {
            scale: 30,
            max_supported: 22,
        };
        assert_eq!(err.kind(), FailureKind::Misconfiguration);
    }
}
