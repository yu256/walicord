#[derive(Debug, Clone)]
pub struct Declaration<'a> {
    pub name: &'a str,
    pub members: Vec<&'a str>,
}

#[derive(Debug, Clone)]
pub struct Payment<'a> {
    pub amount: u64,
    pub payer: &'a str,
    pub payee: &'a str,
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Declaration(Declaration<'a>),
    Payment(Payment<'a>),
}

#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub members: &'a [&'a str],
    pub statements: Vec<Statement<'a>>,
}
