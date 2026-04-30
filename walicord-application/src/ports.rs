use crate::{Script, error::ProgramParseError};
use std::collections::HashMap;
use walicord_domain::{
    MemberBalances, Settlement, SettlementContext, SettlementRoundingError,
    model::{MemberId, RoleMembers},
};

pub trait ProgramParser: Send + Sync {
    fn parse<'a>(
        &self,
        member_ids: &'a [MemberId],
        role_members: &'a RoleMembers,
        content: &'a str,
        author_id: Option<MemberId>,
    ) -> Result<Script<'a>, ProgramParseError<'a>>;
}

/// Plans concrete settlement transfers for a balance set. Used for both preview and commit
/// flows; preview/commit drift is prevented by recording the **same plan instance** that
/// the user previewed, not by relying on the planner to be deterministic — see contract
/// note below.
///
/// Returns both the resolved transfers and the post-transfer balances as a raw
/// `Settlement`. The application immediately validates and canonicalizes that output into
/// its own `ValidatedSettlementPlan`; planners do **not** get to decide ledger
/// representability or no-op-vs-recordable branching on their own. The `MemberBalances`
/// boundary keeps Money + quantization concerns in the application layer; implementations
/// are responsible for the actual constraint solving and must be free of side effects so
/// preview callers can invoke them safely.
///
/// `SettleUpPolicy` consumes a `SettlementPlanner` rather than calling a concrete solver
/// directly so that the application layer stays independent of the HiGHS-backed
/// `walicord-transfer-construction` crate. Alternative planners (greedy fallback, exact
/// solver, deterministic test planner, hosted-service adapter) can be substituted without
/// touching application code.
///
/// **Contract:**
/// * **Side-effect-free with respect to application state.** Implementations must not
///   mutate external state observable to the application; preview callers invoke `plan`
///   to display candidate transfers and may discard the result.
/// * **Should prefer deterministic output when practical, but callers must not rely on
///   repeated calls returning identical plans.** Real-world solvers (HiGHS with
///   `time_limit_seconds` + `accept_feasible_on_limit`) may return alternate optima for
///   the same input, and the contract permits this. Implementations that *can* be
///   deterministic should be — fewer surprises in tests, smaller cognitive load — but
///   the application layer never relies on it.
/// * **Interactive confirm flows must persist the `PreviewedSettlement` instead of
///   re-planning.** Preview/commit equivalence is enforced at the application boundary
///   by passing the same previewed value through
///   [`crate::settle_up::SettleUpPolicy::record_previewed_plan`], never by re-running
///   `plan` at commit time and hoping for the same answer.
/// * **Output must be re-validatable.** Application-level validation
///   ([`crate::settle_up::SettleUpPolicy::preview`]) is the trust boundary; implementations
///   cannot rely on callers blindly trusting their output.
pub trait SettlementPlanner: Send + Sync {
    fn plan(
        &self,
        balances: MemberBalances,
        settle_members: &[MemberId],
        cash_members: &[MemberId],
        context: SettlementContext,
    ) -> Result<Settlement, SettlementRoundingError>;
}

pub trait MemberDirectory: Send + Sync {
    fn display_name(&self, member_id: MemberId) -> Option<&str>;
}

impl MemberDirectory for HashMap<MemberId, smol_str::SmolStr> {
    fn display_name(&self, member_id: MemberId) -> Option<&str> {
        self.get(&member_id).map(smol_str::SmolStr::as_str)
    }
}
