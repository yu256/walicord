/// Balance of each person (positive: pays, negative: receives)
/// The unit is an integer (e.g., yen)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PersonBalance {
    pub id: u64,
    pub balance: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Payment {
    pub from: u64,
    pub to: u64,
    pub amount: i64,
}
