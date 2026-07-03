#![warn(clippy::uninlined_format_args)]

pub mod model;
pub mod non_empty;
pub mod services;

pub use model::{
    AllocationStrategy, AmountError, AmountExpr, AmountOp, BalanceAccumulator,
    BalanceDeltaDirection, BalanceError, Declaration, MemberBalances, MemberSet, MemberSetExpr,
    MemberSetOp, Money, Payment, Program, ProgramBuildError, Ratios, RemainderPolicy,
    ResolvedAllocationStrategy, RoleId, RoleMembers, Settlement, SplitError, Statement,
    StatementWithLine, Transfer, Weight, WeightOverride, WeightOverrideTarget, WeightOverrides,
    distribute_balances,
};
pub use non_empty::{EmptyVecError, NonEmptyVec};
pub use services::{
    AtomicUnitConversionError, FairnessPolicy, MemberSetResolutionError, MemberSetResolver,
    RoundingMode, SettlementContext, SettlementRoundingError, quantize_balances,
    settlement_epsilon,
};
