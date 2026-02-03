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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SettlementOptimizationError {
    ImbalancedTotal(i64),
    NoSolution,
}
use walicord_domain::ProgramBuildError;
