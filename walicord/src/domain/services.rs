use super::{Program, ProgramParseError};

pub trait ProgramParser: Send + Sync {
    fn parse<'a>(
        &self,
        members: &'a [&'a str],
        content: &'a str,
    ) -> Result<Program<'a>, ProgramParseError<'a>>;
}
