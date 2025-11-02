use serde::{Deserialize, Serialize};

/// Balance of each person (positive: received amount, negative: paid amount)
/// The unit is an integer (e.g., yen)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PersonBalance<'a> {
    pub name: &'a str,
    pub balance: i64,
}

/// Optimization result: who pays how much to whom
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SettlementResult<'a> {
    pub from: &'a str,
    pub to: &'a str,
    pub amount: i64,
}
