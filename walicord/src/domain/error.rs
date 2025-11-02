#[derive(Debug, Clone)]
pub enum ProgramParseError {
    MissingMembersDeclaration,
    UndefinedMember { name: String, line: usize },
    SyntaxError(String),
}
