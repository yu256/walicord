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
    MissingContextForImplicitAuthor { line: usize },
    InvalidAmountExpression { line: usize, detail: String },
    AllZeroWeights { line: usize },
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
    InvalidGrid { g1: i64, g2: i64 },
    ModelTooLarge { edge_count: usize, max_edges: usize },
    NoSolution,
    RoundingMismatch,
    QuantizationImbalancedTotal { total: walicord_domain::Money },
    QuantizationInvalidAdjustmentCount,
    QuantizationInsufficientCandidates,
    QuantizationZeroSumInvariantViolation,
    QuantizationNonIntegral,
    QuantizationOutOfRange,
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
            | SettlementOptimizationError::QuantizationNonIntegral
            | SettlementOptimizationError::QuantizationOutOfRange => FailureKind::InternalBug,
            SettlementOptimizationError::InvalidGrid { .. }
            | SettlementOptimizationError::ModelTooLarge { .. }
            | SettlementOptimizationError::QuantizationUnsupportedScale { .. } => {
                FailureKind::Misconfiguration
            }
        }
    }
}

impl ProgramParseError<'_> {
    pub fn kind(&self) -> FailureKind {
        match self {
            ProgramParseError::MissingContextForImplicitAuthor { .. } => {
                FailureKind::Misconfiguration
            }
            ProgramParseError::FailedToEvaluateGroup { .. }
            | ProgramParseError::UndefinedGroup { .. }
            | ProgramParseError::UndefinedMember { .. }
            | ProgramParseError::SyntaxError { .. }
            | ProgramParseError::InvalidAmountExpression { .. }
            | ProgramParseError::AllZeroWeights { .. } => FailureKind::UserInput,
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
            SettlementRoundingError::TransferConstructionNoSolution => {
                SettlementOptimizationError::NoSolution
            }
            SettlementRoundingError::TransferConstructionInvalidGrid { g1, g2 } => {
                SettlementOptimizationError::InvalidGrid { g1, g2 }
            }
            SettlementRoundingError::TransferConstructionModelTooLarge {
                edge_count,
                max_edges,
            } => SettlementOptimizationError::ModelTooLarge {
                edge_count,
                max_edges,
            },
            SettlementRoundingError::TransferConstructionRoundingMismatch => {
                SettlementOptimizationError::RoundingMismatch
            }
            SettlementRoundingError::TransferConstructionImbalancedTotal(total) => {
                SettlementOptimizationError::ImbalancedTotal(total)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::missing_context(
        ProgramParseError::MissingContextForImplicitAuthor { line: 1 },
        FailureKind::Misconfiguration
    )]
    #[case::syntax_error(
        ProgramParseError::SyntaxError {
            line: 1,
            detail: "unexpected token".to_string(),
        },
        FailureKind::UserInput
    )]
    fn program_parse_error_kind_matches_intent(
        #[case] err: ProgramParseError<'static>,
        #[case] expected: FailureKind,
    ) {
        assert_eq!(err.kind(), expected);
    }

    #[rstest]
    #[case::quantization_non_integral(
        SettlementOptimizationError::QuantizationNonIntegral,
        FailureKind::InternalBug
    )]
    #[case::quantization_out_of_range(
        SettlementOptimizationError::QuantizationOutOfRange,
        FailureKind::InternalBug
    )]
    #[case::unsupported_scale(
        SettlementOptimizationError::QuantizationUnsupportedScale {
            scale: 30,
            max_supported: 22,
        },
        FailureKind::Misconfiguration
    )]
    #[case::invalid_grid(
        SettlementOptimizationError::InvalidGrid { g1: 1000, g2: 300 },
        FailureKind::Misconfiguration
    )]
    #[case::model_too_large(
        SettlementOptimizationError::ModelTooLarge {
            edge_count: 121,
            max_edges: 120,
        },
        FailureKind::Misconfiguration
    )]
    fn settlement_optimization_error_kind_matches_intent(
        #[case] err: SettlementOptimizationError,
        #[case] expected: FailureKind,
    ) {
        assert_eq!(err.kind(), expected);
    }

    #[rstest]
    #[case::rounding_non_integral(
        SettlementRoundingError::NonIntegral,
        SettlementOptimizationError::QuantizationNonIntegral
    )]
    #[case::transfer_no_solution(
        SettlementRoundingError::TransferConstructionNoSolution,
        SettlementOptimizationError::NoSolution
    )]
    #[case::transfer_invalid_grid(
        SettlementRoundingError::TransferConstructionInvalidGrid { g1: 1000, g2: 300 },
        SettlementOptimizationError::InvalidGrid { g1: 1000, g2: 300 }
    )]
    #[case::transfer_model_too_large(
        SettlementRoundingError::TransferConstructionModelTooLarge {
            edge_count: 121,
            max_edges: 120,
        },
        SettlementOptimizationError::ModelTooLarge {
            edge_count: 121,
            max_edges: 120,
        }
    )]
    #[case::transfer_rounding_mismatch(
        SettlementRoundingError::TransferConstructionRoundingMismatch,
        SettlementOptimizationError::RoundingMismatch
    )]
    #[case::transfer_imbalanced_total(
        SettlementRoundingError::TransferConstructionImbalancedTotal(7),
        SettlementOptimizationError::ImbalancedTotal(7)
    )]
    #[case::rounding_unsupported_scale(
        SettlementRoundingError::UnsupportedScale {
            scale: 30,
            max_supported: 22,
        },
        SettlementOptimizationError::QuantizationUnsupportedScale {
            scale: 30,
            max_supported: 22,
        }
    )]
    fn settlement_rounding_error_maps_to_application_error(
        #[case] input: SettlementRoundingError,
        #[case] expected: SettlementOptimizationError,
    ) {
        assert_eq!(SettlementOptimizationError::from(input), expected);
    }
}
