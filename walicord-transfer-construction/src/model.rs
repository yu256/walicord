/// Balance of each person (positive: pays, negative: receives)
/// The unit is an integer (e.g., yen)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PersonBalance<MemberId = u64> {
    pub id: MemberId,
    pub balance: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Payment<MemberId = u64> {
    pub from: MemberId,
    pub to: MemberId,
    pub amount: i64,
}
