#![warn(clippy::uninlined_format_args)]

pub mod model;
pub mod services;

pub use model::{
    AmountError, AmountExpr, AmountOp, BalanceAccumulator, Declaration, MemberBalances, MemberSet,
    MemberSetExpr, MemberSetOp, Money, Payment, Program, ProgramBuildError, Ratios,
    RemainderPolicy, Settlement, SplitError, Statement, StatementWithLine, Transfer,
    distribute_balances,
};
pub use services::{
    AtomicUnitConversionError, FairnessPolicy, MemberSetResolver, RoundingMode, SettleUpPolicy,
    SettlementCalculator, SettlementContext, SettlementRoundingError, quantize_balances,
};
