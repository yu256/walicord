use walicord_parser::SetExpr;

pub struct Declaration<'a> {
    pub name: &'a str,
    pub members: Vec<&'a str>,
}

pub struct Payment<'a> {
    pub amount: u64,
    pub payer: SetExpr<'a>,
    pub payee: SetExpr<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    Variables,
    Evaluate,
}

pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Payment(Payment<'a>),
    Command(Command),
}

pub struct Program<'a> {
    pub members: &'a [&'a str],
    pub statements: Vec<Statement<'a>>,
}
