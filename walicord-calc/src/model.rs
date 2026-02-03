/// Balance of each person (positive: received amount, negative: paid amount)
/// The unit is an integer (e.g., yen)
#[derive(Debug, Clone, Copy)]
pub struct PersonBalance<'a> {
    pub name: &'a str,
    pub balance: i64,
}

pub struct Payment<'a> {
    pub from: &'a str,
    pub to: &'a str,
    pub amount: i64,
}
