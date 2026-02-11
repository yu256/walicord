/// Balance of each person (positive: received amount, negative: paid amount)
/// The unit is an integer (e.g., yen)
#[derive(Debug, Clone, Copy)]
pub struct PersonBalance {
    pub id: u64,
    pub balance: i64,
}

pub struct Payment {
    pub from: u64,
    pub to: u64,
    pub amount: i64,
}
