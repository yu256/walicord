pub enum ProgramParseError<'a> {
    MissingMembersDeclaration,
    UndefinedMember { name: &'a str, line: usize },
    SyntaxError(String),
}
