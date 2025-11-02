pub struct Declaration<'a> {
    pub name: &'a str,
    pub members: Vec<&'a str>,
}

pub struct Payment<'a> {
    pub amount: u64,
    pub payer: &'a str,
    pub payee: &'a str,
}

pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Payment(Payment<'a>),
}

pub struct Program<'a> {
    pub members: &'a [&'a str],
    pub statements: Vec<Statement<'a>>,
}
