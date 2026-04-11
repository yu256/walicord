use crate::{
    BalanceCalculationError, SettlementOptimizationError,
    error::ProgramParseError,
    model::{
        Command, PersonBalance, Script, ScriptStatement, ScriptStatementWithLine, SettleUpContext,
    },
    ports::{ProgramParser, SettlementOptimizer},
};
use std::{borrow::Cow, collections::HashSet};
use walicord_domain::{
    BalanceAccumulator, MemberBalances, MemberSet, Money, SettleUpPolicy, SettlementContext,
    Transfer,
    model::{MemberId, MemberSetExpr, RoleMembers},
    quantize_balances,
};

pub struct SettlementResult {
    // Post-command balances. For settle-up results, confirmed transfers are already applied.
    pub balances: Vec<PersonBalance>,
    pub optimized_transfers: Vec<Transfer>,
    pub settle_up: Option<SettleUpContext>,
    pub quantization_scale: u32,
    pub effective_cash_members: Vec<MemberId>,
    // Captured before quantize_balances so the flow presenter can verify C-Q1 (|sum| <= epsilon).
    pub pre_quantization_sum: Money,
}

#[derive(Clone, Copy)]
pub struct MessageProcessor<'a> {
    parser: &'a dyn ProgramParser,
    optimizer: &'a dyn SettlementOptimizer,
}

pub enum ProcessingOutcome<'a> {
    Success(Script<'a>),
    FailedToEvaluateGroup {
        name: Cow<'a, str>,
        line: usize,
    },
    UndefinedGroup {
        name: Cow<'a, str>,
        line: usize,
    },
    UndefinedRole {
        id: u64,
        line: usize,
    },
    UndefinedMember {
        id: u64,
        line: usize,
    },
    SyntaxError {
        line: usize,
        kind: crate::error::SyntaxErrorKind,
    },
    MissingContextForImplicitAuthor {
        line: usize,
    },
    InvalidAmountExpression {
        line: usize,
        detail: String,
    },
    AllZeroWeights {
        line: usize,
    },
    WeightedReferenceOutsidePayee {
        line: usize,
    },
}

impl<'a> ProcessingOutcome<'a> {
    pub fn into_result(self) -> Result<Script<'a>, crate::error::ProgramParseError<'a>> {
        use crate::error::ProgramParseError;
        match self {
            ProcessingOutcome::Success(script) => Ok(script),
            ProcessingOutcome::FailedToEvaluateGroup { name, line } => {
                Err(ProgramParseError::FailedToEvaluateGroup { name, line })
            }
            ProcessingOutcome::UndefinedGroup { name, line } => {
                Err(ProgramParseError::UndefinedGroup { name, line })
            }
            ProcessingOutcome::UndefinedRole { id, line } => {
                Err(ProgramParseError::UndefinedRole { id, line })
            }
            ProcessingOutcome::UndefinedMember { id, line } => {
                Err(ProgramParseError::UndefinedMember { id, line })
            }
            ProcessingOutcome::SyntaxError { line, kind } => {
                Err(ProgramParseError::SyntaxError { line, kind })
            }
            ProcessingOutcome::MissingContextForImplicitAuthor { line } => {
                Err(ProgramParseError::MissingContextForImplicitAuthor { line })
            }
            ProcessingOutcome::InvalidAmountExpression { line, detail } => {
                Err(ProgramParseError::InvalidAmountExpression { line, detail })
            }
            ProcessingOutcome::AllZeroWeights { line } => {
                Err(ProgramParseError::AllZeroWeights { line })
            }
            ProcessingOutcome::WeightedReferenceOutsidePayee { line } => {
                Err(ProgramParseError::WeightedReferenceOutsidePayee { line })
            }
        }
    }
}

fn line_count_increment(content: &str, prior_ended_with_newline: bool) -> usize {
    content.lines().count() + if prior_ended_with_newline { 1 } else { 0 }
}

struct ApplyResult {
    balances: MemberBalances,
    settle_up: Option<SettleUpContext>,
    settle_up_cash_members: Vec<MemberId>,
    review_cash_members: Vec<MemberId>,
    quantization_scale: u32,
    pre_quantization_sum: Money,
}

#[derive(Default)]
struct CashPreferenceState {
    script_local_cash_members: HashSet<MemberId>,
}

impl CashPreferenceState {
    fn extend(&mut self, members: &MemberSet) {
        self.script_local_cash_members.extend(members.iter());
    }

    fn effective_with(&self, command_cash_members: Option<&MemberSet>) -> Vec<MemberId> {
        let mut effective_cash_members: Vec<MemberId> =
            self.script_local_cash_members.iter().copied().collect();
        effective_cash_members.extend(command_cash_members.iter().flat_map(|set| set.iter()));
        effective_cash_members.sort_unstable();
        effective_cash_members.dedup();
        effective_cash_members
    }

    fn into_sorted_members(self) -> Vec<MemberId> {
        let mut members: Vec<MemberId> = self.script_local_cash_members.into_iter().collect();
        members.sort_unstable();
        members
    }
}

#[derive(Default)]
struct LastSettleState {
    settle_members: Vec<MemberId>,
    cash_members: Vec<MemberId>,
    transfers: Vec<Transfer>,
}

impl LastSettleState {
    fn clear(&mut self) {
        self.settle_members.clear();
        self.cash_members.clear();
        self.transfers.clear();
    }

    fn record(
        &mut self,
        settle_members: &MemberSet,
        effective_cash_members: Vec<MemberId>,
        transfers: Vec<Transfer>,
    ) {
        self.settle_members.extend(settle_members.iter());
        self.cash_members.extend(effective_cash_members);
        self.transfers.extend(transfers);
    }
}

impl<'a> MessageProcessor<'a> {
    // Keep settlement policy selection at the application boundary so domain/infrastructure
    // can stay policy-agnostic and receive context explicitly.
    fn settlement_context() -> SettlementContext {
        SettlementContext::jpy_default()
    }

    pub fn new(parser: &'a dyn ProgramParser, optimizer: &'a dyn SettlementOptimizer) -> Self {
        Self { parser, optimizer }
    }

    pub fn parse_program<'b>(
        &self,
        member_ids: &'b [MemberId],
        role_members: &'b RoleMembers,
        content: &'b str,
        author_id: Option<MemberId>,
    ) -> ProcessingOutcome<'b>
    where
        'a: 'b,
    {
        match self
            .parser
            .parse(member_ids, role_members, content, author_id)
        {
            Ok(program) => ProcessingOutcome::Success(program),
            Err(err) => Self::map_parse_error(err, 0),
        }
    }

    pub fn parse_program_sequence<'b, I>(
        &self,
        member_ids: &'b [MemberId],
        role_members: &'b RoleMembers,
        messages: I,
    ) -> ProcessingOutcome<'b>
    where
        I: IntoIterator<Item = (&'b str, Option<MemberId>)>,
        'a: 'b,
    {
        let mut statements = Vec::new();
        let mut line_count = 0usize;
        let mut ends_with_newline = false;
        let mut has_any = false;

        for (content, author_id) in messages {
            let offset = if has_any {
                line_count + if ends_with_newline { 1 } else { 0 }
            } else {
                0
            };

            match self
                .parser
                .parse(member_ids, role_members, content, author_id)
            {
                Ok(program) => {
                    let mut program_statements = program.into_statements();
                    for statement in &mut program_statements {
                        statement.line += offset;
                    }
                    statements.extend(program_statements);
                }
                Err(err) => return Self::map_parse_error(err, offset),
            }

            if has_any {
                // Offsets reflect concatenating messages with a single newline separator.
                // Update line_count as if messages were concatenated with a single newline between
                // them: add the number of lines in the current content, plus one extra line if the
                // previous message ended with a newline, to account for the implicit separator.
                line_count += line_count_increment(content, ends_with_newline);
            } else {
                line_count = content.lines().count();
            }
            ends_with_newline = content.is_empty() || content.ends_with('\n');
            has_any = true;
        }

        ProcessingOutcome::Success(Script::new(member_ids, role_members, statements))
    }

    pub async fn calculate_balances(
        &self,
        program: &Script<'_>,
    ) -> Result<MemberBalances, BalanceCalculationError> {
        self.apply_statements_without_settlement(program, None)
    }

    pub async fn calculate_balances_for_prefix(
        &self,
        program: &Script<'_>,
        prefix_len: usize,
    ) -> Result<MemberBalances, BalanceCalculationError> {
        self.apply_statements_without_settlement(program, Some(prefix_len))
    }

    pub async fn build_settlement_result(
        &self,
        program: &Script<'_>,
    ) -> Result<SettlementResult, SettlementOptimizationError> {
        let apply_result = self.apply_statements(program, None, true).await?;
        self.build_settlement_result_from_apply(apply_result).await
    }

    pub async fn build_settlement_result_for_prefix(
        &self,
        program: &Script<'_>,
        prefix_len: usize,
    ) -> Result<SettlementResult, SettlementOptimizationError> {
        let apply_result = self
            .apply_statements(program, Some(prefix_len), true)
            .await?;
        self.build_settlement_result_from_apply(apply_result).await
    }

    async fn build_settlement_result_from_apply(
        &self,
        apply_result: ApplyResult,
    ) -> Result<SettlementResult, SettlementOptimizationError> {
        let ApplyResult {
            balances,
            settle_up,
            settle_up_cash_members,
            review_cash_members,
            quantization_scale,
            pre_quantization_sum,
        } = apply_result;

        let mut person_balances: Vec<PersonBalance> = balances
            .into_iter()
            .map(|(id, balance)| PersonBalance { id, balance })
            .collect();
        person_balances.sort_by_key(|p| p.id);

        let context = SettlementContext {
            scale: quantization_scale,
            ..Self::settlement_context()
        };

        let optimizer = self.optimizer;

        let optimized_transfers = match settle_up.as_ref() {
            Some(settle_up) => tokio::task::block_in_place(|| {
                optimizer.optimize(
                    &person_balances,
                    &settle_up.settle_members,
                    &settle_up_cash_members,
                    context,
                )
            })?,
            None => {
                let all_members: Vec<MemberId> =
                    person_balances.iter().map(|balance| balance.id).collect();
                tokio::task::block_in_place(|| {
                    optimizer.optimize(
                        &person_balances,
                        &all_members,
                        &review_cash_members,
                        context,
                    )
                })?
            }
        };

        let effective_cash_members = if settle_up.is_some() {
            settle_up_cash_members
        } else {
            review_cash_members
        };

        Ok(SettlementResult {
            balances: person_balances,
            optimized_transfers,
            settle_up,
            quantization_scale,
            effective_cash_members,
            pre_quantization_sum,
        })
    }

    async fn apply_statements(
        &self,
        program: &Script<'_>,
        prefix_len: Option<usize>,
        apply_settle: bool,
    ) -> Result<ApplyResult, SettlementOptimizationError> {
        let statements = program.statements();
        let end = match prefix_len {
            Some(prefix_len) => self.prefix_end(statements, prefix_len),
            None => statements.len(),
        };

        let mut accumulator =
            BalanceAccumulator::new_with_context(program.members(), program.role_members());
        let mut last_settle_state = LastSettleState::default();
        let last_is_settle = apply_settle
            && end > 0
            && matches!(
                statements[end - 1].statement,
                ScriptStatement::Command(Command::SettleUp { .. })
            );
        // Script-local cash preference state while scanning commands in order.
        let mut cash_preference_state = CashPreferenceState::default();

        for stmt in &statements[..end] {
            match &stmt.statement {
                ScriptStatement::Domain(statement) => {
                    accumulator.apply(statement)?;
                }
                ScriptStatement::Command(command) => {
                    if !apply_settle {
                        continue;
                    }
                    match command {
                        Command::Variables | Command::Review => {}
                        Command::MemberAddCash { members } => {
                            let Some(target_members) =
                                Self::evaluate_command_members_leniently(&accumulator, members)
                            else {
                                continue;
                            };
                            cash_preference_state.extend(&target_members);
                        }
                        Command::SettleUp {
                            members,
                            cash_members,
                        } => {
                            self.apply_settle_command(
                                &mut accumulator,
                                members,
                                cash_members.as_ref(),
                                &cash_preference_state,
                                &mut last_settle_state,
                            )?;
                        }
                    }
                }
            }
        }

        let LastSettleState {
            settle_members: last_settle_members,
            cash_members: last_settle_cash_members,
            transfers: last_settle_transfers,
        } = last_settle_state;

        let settle_up = if last_is_settle {
            Some(SettleUpContext {
                settle_members: last_settle_members,
                immediate_transfers: last_settle_transfers,
            })
        } else {
            None
        };

        let context = Self::settlement_context();
        let balances = accumulator.into_balances();
        let pre_quantization_sum: Money = balances.values().sum();
        let balances = if apply_settle {
            quantize_balances(&balances, context).map_err(SettlementOptimizationError::from)?
        } else {
            balances
        };

        let review_cash_members = cash_preference_state.into_sorted_members();

        Ok(ApplyResult {
            balances,
            settle_up,
            settle_up_cash_members: last_settle_cash_members,
            review_cash_members,
            quantization_scale: context.scale,
            pre_quantization_sum,
        })
    }

    fn apply_settle_command<'b>(
        &self,
        accumulator: &mut BalanceAccumulator<'b>,
        members: &MemberSetExpr<'b>,
        cash_members: Option<&MemberSetExpr<'b>>,
        cash_preference_state: &CashPreferenceState,
        last_settle_state: &mut LastSettleState,
    ) -> Result<(), SettlementOptimizationError> {
        last_settle_state.clear();

        let Some(settle_members) = Self::evaluate_command_members_leniently(accumulator, members)
        else {
            return Ok(());
        };
        if settle_members.is_empty() {
            return Ok(());
        }

        let command_cash_members = if let Some(command_cash_members) = cash_members {
            let Some(command_set) =
                Self::evaluate_command_members_leniently(accumulator, command_cash_members)
            else {
                return Ok(());
            };
            Some(command_set)
        } else {
            None
        };

        let effective_cash_members =
            cash_preference_state.effective_with(command_cash_members.as_ref());

        let balances = accumulator.balances();
        let context = Self::settlement_context();
        let result = tokio::task::block_in_place(|| {
            SettleUpPolicy::settle(
                balances,
                settle_members.members(),
                effective_cash_members.iter().copied(),
                context,
            )
        })
        .map_err(SettlementOptimizationError::from)?;
        accumulator.set_balances(result.new_balances);
        last_settle_state.record(&settle_members, effective_cash_members, result.transfers);

        Ok(())
    }

    fn evaluate_command_members_leniently<'b>(
        accumulator: &BalanceAccumulator<'b>,
        expr: &MemberSetExpr<'b>,
    ) -> Option<MemberSet> {
        // Preserve existing command behavior: invalid member-set expressions in commands
        // are ignored so prior script-local state remains effective.
        accumulator.try_evaluate_members(expr).ok()
    }

    fn apply_statements_without_settlement(
        &self,
        program: &Script<'_>,
        prefix_len: Option<usize>,
    ) -> Result<MemberBalances, BalanceCalculationError> {
        let statements = program.statements();
        let end = match prefix_len {
            Some(prefix_len) => self.prefix_end(statements, prefix_len),
            None => statements.len(),
        };

        let mut accumulator =
            BalanceAccumulator::new_with_context(program.members(), program.role_members());
        for stmt in &statements[..end] {
            if let ScriptStatement::Domain(statement) = &stmt.statement {
                accumulator
                    .apply(statement)
                    .map_err(BalanceCalculationError::from)?;
            }
        }

        Ok(accumulator.into_balances())
    }

    fn prefix_end<'b>(
        &self,
        statements: &[ScriptStatementWithLine<'b>],
        prefix_len: usize,
    ) -> usize {
        let prefix_len = prefix_len.min(statements.len());
        if prefix_len < statements.len()
            && let ScriptStatement::Command(_) = statements[prefix_len].statement
        {
            return prefix_len + 1;
        }
        prefix_len
    }

    fn map_parse_error<'b>(err: ProgramParseError<'b>, offset: usize) -> ProcessingOutcome<'b> {
        match err {
            ProgramParseError::FailedToEvaluateGroup { name, line } => {
                ProcessingOutcome::FailedToEvaluateGroup {
                    name,
                    line: line + offset,
                }
            }
            ProgramParseError::UndefinedGroup { name, line } => ProcessingOutcome::UndefinedGroup {
                name,
                line: line + offset,
            },
            ProgramParseError::UndefinedRole { id, line } => ProcessingOutcome::UndefinedRole {
                id,
                line: line + offset,
            },
            ProgramParseError::UndefinedMember { id, line } => ProcessingOutcome::UndefinedMember {
                id,
                line: line + offset,
            },
            ProgramParseError::SyntaxError { line, kind } => ProcessingOutcome::SyntaxError {
                line: line + offset,
                kind,
            },
            ProgramParseError::MissingContextForImplicitAuthor { line } => {
                ProcessingOutcome::MissingContextForImplicitAuthor {
                    line: line + offset,
                }
            }
            ProgramParseError::InvalidAmountExpression { line, detail } => {
                ProcessingOutcome::InvalidAmountExpression {
                    line: line + offset,
                    detail,
                }
            }
            ProgramParseError::AllZeroWeights { line } => ProcessingOutcome::AllZeroWeights {
                line: line + offset,
            },
            ProgramParseError::WeightedReferenceOutsidePayee { line } => {
                ProcessingOutcome::WeightedReferenceOutsidePayee {
                    line: line + offset,
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        error::{BalanceCalculationError, SettlementOptimizationError},
        ports::{ProgramParser, SettlementOptimizer},
    };
    use fxhash::FxHashSet;
    use proptest::prelude::*;
    use rstest::{fixture, rstest};
    use std::collections::BTreeMap;
    use walicord_domain::{
        Payment, Statement,
        model::{MemberId, MemberSetExpr, MemberSetOp, Money, RoleId, RoleMembers, Weight},
    };

    fn any_string() -> impl Strategy<Value = String> {
        proptest::collection::vec(any::<char>(), 0..40)
            .prop_map(|chars| chars.into_iter().collect())
    }

    fn empty_roles() -> &'static RoleMembers {
        use std::sync::OnceLock;
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(RoleMembers::default)
    }

    fn roles_with_large_cash_role() -> &'static RoleMembers {
        use std::sync::OnceLock;
        static ROLES: OnceLock<RoleMembers> = OnceLock::new();
        ROLES.get_or_init(|| {
            RoleMembers::from_iter([(RoleId(999), FxHashSet::from_iter((1..=129).map(MemberId)))])
        })
    }

    struct StubParser;

    impl ProgramParser for StubParser {
        fn parse<'a>(
            &self,
            _member_ids: &'a [MemberId],
            _role_members: &'a RoleMembers,
            _content: &'a str,
            _author_id: Option<MemberId>,
        ) -> Result<Script<'a>, ProgramParseError<'a>> {
            let statements = vec![
                ScriptStatementWithLine {
                    line: 1,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment::even(
                        Money::try_from(60).expect("amount should fit in i64"),
                        MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                        MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
                    ))),
                },
                ScriptStatementWithLine {
                    line: 2,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment::even(
                        Money::try_from(40).expect("amount should fit in i64"),
                        MemberSetExpr::new([MemberSetOp::Push(MemberId(2))]),
                        MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
                    ))),
                },
                ScriptStatementWithLine {
                    line: 3,
                    statement: ScriptStatement::Command(Command::SettleUp {
                        members: MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                        cash_members: None,
                    }),
                },
            ];

            Ok(Script::new(&[], empty_roles(), statements))
        }
    }

    struct NoopOptimizer;

    impl SettlementOptimizer for NoopOptimizer {
        fn optimize(
            &self,
            _balances: &[PersonBalance],
            _settle_members: &[MemberId],
            _cash_members: &[MemberId],
            _context: SettlementContext,
        ) -> Result<Vec<Transfer>, SettlementOptimizationError> {
            Ok(Vec::new())
        }
    }

    struct AssertSettleMembersOptimizer {
        expected: Vec<MemberId>,
    }

    impl SettlementOptimizer for AssertSettleMembersOptimizer {
        fn optimize(
            &self,
            _balances: &[PersonBalance],
            settle_members: &[MemberId],
            _cash_members: &[MemberId],
            _context: SettlementContext,
        ) -> Result<Vec<Transfer>, SettlementOptimizationError> {
            assert_eq!(settle_members, self.expected.as_slice());
            Ok(Vec::new())
        }
    }

    struct AssertSettleAndCashMembersOptimizer {
        expected_settle: Vec<MemberId>,
        expected_cash: Vec<MemberId>,
    }

    impl SettlementOptimizer for AssertSettleAndCashMembersOptimizer {
        fn optimize(
            &self,
            _balances: &[PersonBalance],
            settle_members: &[MemberId],
            cash_members: &[MemberId],
            _context: SettlementContext,
        ) -> Result<Vec<Transfer>, SettlementOptimizationError> {
            assert_eq!(settle_members, self.expected_settle.as_slice());
            assert_eq!(cash_members, self.expected_cash.as_slice());
            Ok(Vec::new())
        }
    }

    #[fixture]
    fn processor() -> MessageProcessor<'static> {
        MessageProcessor::new(&StubParser, &NoopOptimizer)
    }

    struct SequenceParser;

    impl ProgramParser for SequenceParser {
        fn parse<'a>(
            &self,
            _member_ids: &'a [MemberId],
            _role_members: &'a RoleMembers,
            content: &'a str,
            _author_id: Option<MemberId>,
        ) -> Result<Script<'a>, ProgramParseError<'a>> {
            if content.contains("SYNTAX") {
                return Err(ProgramParseError::SyntaxError {
                    line: 1,
                    kind: crate::SyntaxErrorKind::ParseFailure {
                        attempted_form: None,
                        expected: crate::ExpectedElement::Unknown,
                        near: "stub".to_string(),
                    },
                });
            }
            if content.contains("IMPLICIT") {
                return Err(ProgramParseError::MissingContextForImplicitAuthor { line: 1 });
            }

            Ok(Script::new(
                &[],
                empty_roles(),
                vec![ScriptStatementWithLine {
                    line: 1,
                    statement: ScriptStatement::Command(Command::Variables),
                }],
            ))
        }
    }

    #[rstest]
    #[tokio::test(flavor = "multi_thread")]
    async fn settle_up_response_groups_transfers(processor: MessageProcessor<'_>) {
        let members: [MemberId; 0] = [];
        let program = processor
            .parse_program(&members, empty_roles(), "unused", None)
            .into_result()
            .expect("program should parse");

        let last_index = program.statements().len().saturating_sub(1);
        let result = processor
            .build_settlement_result_for_prefix(&program, last_index)
            .await
            .expect("result generation failed");

        let settle_up = result.settle_up.expect("expected settle up context");
        assert_eq!(
            settle_up.immediate_transfers,
            vec![Transfer {
                from: MemberId(3), // debtor pays
                to: MemberId(1),   // creditor receives
                amount: Money::from_i64(60),
            }]
        );
    }

    #[rstest]
    #[tokio::test(flavor = "multi_thread")]
    async fn settle_up_result_balances_include_confirmed_transfers(
        processor: MessageProcessor<'_>,
    ) {
        let members: [MemberId; 0] = [];
        let program = processor
            .parse_program(&members, empty_roles(), "unused", None)
            .into_result()
            .expect("program should parse");

        let last_index = program.statements().len().saturating_sub(1);
        let result = processor
            .build_settlement_result_for_prefix(&program, last_index)
            .await
            .expect("result generation failed");

        assert_eq!(
            result.balances,
            vec![
                PersonBalance {
                    id: MemberId(1),
                    balance: Money::ZERO,
                },
                PersonBalance {
                    id: MemberId(2),
                    balance: Money::from_i64(40),
                },
                PersonBalance {
                    id: MemberId(3),
                    balance: Money::from_i64(-40),
                },
            ]
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn build_settlement_result_passes_settle_members_to_optimizer() {
        let parser = StubParser;
        let optimizer = AssertSettleMembersOptimizer {
            expected: vec![MemberId(1)],
        };
        let processor = MessageProcessor::new(&parser, &optimizer);

        let members: [MemberId; 0] = [];
        let program = processor
            .parse_program(&members, empty_roles(), "unused", None)
            .into_result()
            .expect("program should parse");

        let _ = processor
            .build_settlement_result(&program)
            .await
            .expect("result generation failed");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn build_settlement_result_without_settleup_passes_all_members_to_optimizer() {
        let optimizer = AssertSettleMembersOptimizer {
            expected: vec![MemberId(1), MemberId(2), MemberId(3)],
        };
        let processor = MessageProcessor::new(&StubParser, &optimizer);

        let script = Script::new(
            &[],
            empty_roles(),
            vec![
                ScriptStatementWithLine {
                    line: 1,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment::even(
                        Money::from_i64(60),
                        MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                        MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
                    ))),
                },
                ScriptStatementWithLine {
                    line: 2,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment::even(
                        Money::from_i64(40),
                        MemberSetExpr::new([MemberSetOp::Push(MemberId(2))]),
                        MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
                    ))),
                },
            ],
        );

        let _ = processor
            .build_settlement_result(&script)
            .await
            .expect("result generation failed");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn build_settlement_result_passes_effective_cash_members_to_optimizer() {
        let optimizer = AssertSettleAndCashMembersOptimizer {
            expected_settle: vec![MemberId(1), MemberId(2), MemberId(3), MemberId(4)],
            expected_cash: vec![MemberId(1), MemberId(2)],
        };
        let processor = MessageProcessor::new(&StubParser, &optimizer);

        let script = Script::new(
            &[],
            empty_roles(),
            vec![
                payment_stmt(1, 1, 3, 700),
                payment_stmt(2, 1, 4, 600),
                payment_stmt(3, 2, 3, 50),
                payment_stmt(4, 2, 4, 50),
                ScriptStatementWithLine {
                    line: 5,
                    statement: ScriptStatement::Command(Command::MemberAddCash {
                        members: union_members(&[1]),
                    }),
                },
                ScriptStatementWithLine {
                    line: 6,
                    statement: ScriptStatement::Command(Command::SettleUp {
                        members: union_members(&[1, 2, 3, 4]),
                        cash_members: Some(union_members(&[2])),
                    }),
                },
            ],
        );

        let _ = processor
            .build_settlement_result(&script)
            .await
            .expect("result generation failed");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn build_settlement_result_for_review_passes_cash_members_to_optimizer() {
        let optimizer = AssertSettleAndCashMembersOptimizer {
            expected_settle: vec![MemberId(1), MemberId(2), MemberId(3), MemberId(4)],
            expected_cash: vec![MemberId(1)],
        };
        let processor = MessageProcessor::new(&StubParser, &optimizer);

        let mut statements = base_cash_sensitivity_payments();
        statements.extend([
            ScriptStatementWithLine {
                line: 5,
                statement: ScriptStatement::Command(Command::MemberAddCash {
                    members: union_members(&[1]),
                }),
            },
            ScriptStatementWithLine {
                line: 6,
                statement: ScriptStatement::Command(Command::Review),
            },
        ]);
        let script = Script::new(&[], empty_roles(), statements);

        let _ = processor
            .build_settlement_result_for_prefix(&script, 5)
            .await
            .expect("review generation failed");
    }

    fn payment_stmt(
        line: usize,
        payer: u64,
        payee: u64,
        amount: i64,
    ) -> ScriptStatementWithLine<'static> {
        ScriptStatementWithLine {
            line,
            statement: ScriptStatement::Domain(Statement::Payment(Payment::even(
                Money::from_i64(amount),
                MemberSetExpr::new([MemberSetOp::Push(MemberId(payer))]),
                MemberSetExpr::new([MemberSetOp::Push(MemberId(payee))]),
            ))),
        }
    }

    fn union_members(ids: &[u64]) -> MemberSetExpr<'static> {
        let mut ops = Vec::with_capacity(ids.len().saturating_mul(2).saturating_sub(1));
        for (idx, id) in ids.iter().copied().enumerate() {
            ops.push(MemberSetOp::Push(MemberId(id)));
            if idx > 0 {
                ops.push(MemberSetOp::Union);
            }
        }
        MemberSetExpr::new(ops)
    }

    fn base_cash_sensitivity_payments() -> Vec<ScriptStatementWithLine<'static>> {
        vec![
            payment_stmt(1, 1, 3, 700),
            payment_stmt(2, 1, 4, 600),
            payment_stmt(3, 2, 3, 50),
            payment_stmt(4, 2, 4, 50),
        ]
    }

    fn script_with_zero_total_weight_payment() -> Script<'static> {
        Script::new(
            &[],
            empty_roles(),
            vec![ScriptStatementWithLine {
                line: 1,
                statement: ScriptStatement::Domain(Statement::Payment(Payment::weighted(
                    Money::from_i64(100),
                    MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                    MemberSetExpr::new([
                        MemberSetOp::Push(MemberId(1)),
                        MemberSetOp::Push(MemberId(2)),
                        MemberSetOp::Union,
                    ]),
                    BTreeMap::from([(MemberId(1), Weight::ZERO), (MemberId(2), Weight::ZERO)]),
                ))),
            }],
        )
    }

    fn script_with_weight_overflow_payment() -> Script<'static> {
        Script::new(
            &[],
            empty_roles(),
            vec![ScriptStatementWithLine {
                line: 1,
                statement: ScriptStatement::Domain(Statement::Payment(Payment::weighted(
                    Money::from_i64(100),
                    MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                    MemberSetExpr::new([
                        MemberSetOp::Push(MemberId(1)),
                        MemberSetOp::Push(MemberId(2)),
                        MemberSetOp::Union,
                    ]),
                    BTreeMap::from([(MemberId(1), Weight::MAX), (MemberId(2), Weight(1))]),
                ))),
            }],
        )
    }

    fn script_with_persisted_cash() -> Script<'static> {
        let mut persisted_statements = base_cash_sensitivity_payments();
        persisted_statements.extend([
            ScriptStatementWithLine {
                line: 5,
                statement: ScriptStatement::Command(Command::MemberAddCash {
                    members: union_members(&[1]),
                }),
            },
            ScriptStatementWithLine {
                line: 6,
                statement: ScriptStatement::Command(Command::SettleUp {
                    members: union_members(&[1, 2, 3, 4]),
                    cash_members: None,
                }),
            },
        ]);

        Script::new(&[], empty_roles(), persisted_statements)
    }

    fn script_with_command_cash() -> Script<'static> {
        script_with_command_cash_with_roles(union_members(&[1]), empty_roles())
    }

    fn script_with_command_cash_with_roles(
        cash_members: MemberSetExpr<'static>,
        roles: &'static RoleMembers,
    ) -> Script<'static> {
        let members = union_members(&[1, 2, 3, 4]);
        let mut command_statements = base_cash_sensitivity_payments();
        command_statements.push(ScriptStatementWithLine {
            line: 5,
            statement: ScriptStatement::Command(Command::SettleUp {
                members,
                cash_members: Some(cash_members),
            }),
        });

        Script::new(&[], roles, command_statements)
    }

    fn script_with_member_cash_on_with_roles(roles: &'static RoleMembers) -> Script<'static> {
        let members = union_members(&[1, 2, 3, 4]);
        let mut baseline = base_cash_sensitivity_payments();
        baseline.extend([
            ScriptStatementWithLine {
                line: 5,
                statement: ScriptStatement::Command(Command::MemberAddCash {
                    members: union_members(&[1]),
                }),
            },
            ScriptStatementWithLine {
                line: 6,
                statement: ScriptStatement::Command(Command::SettleUp {
                    members,
                    cash_members: None,
                }),
            },
        ]);

        Script::new(&[], roles, baseline)
    }

    fn script_with_invalid_member_cash_expr_with_roles(
        invalid_members: MemberSetExpr<'static>,
        roles: &'static RoleMembers,
    ) -> Script<'static> {
        let members = union_members(&[1, 2, 3, 4]);
        let mut with_invalid_cash = base_cash_sensitivity_payments();
        with_invalid_cash.extend([
            ScriptStatementWithLine {
                line: 5,
                statement: ScriptStatement::Command(Command::MemberAddCash {
                    members: union_members(&[1]),
                }),
            },
            ScriptStatementWithLine {
                line: 6,
                statement: ScriptStatement::Command(Command::MemberAddCash {
                    members: invalid_members,
                }),
            },
            ScriptStatementWithLine {
                line: 7,
                statement: ScriptStatement::Command(Command::SettleUp {
                    members,
                    cash_members: None,
                }),
            },
        ]);

        Script::new(&[], roles, with_invalid_cash)
    }

    fn script_with_invalid_member_cash_unknown_group_expr() -> Script<'static> {
        script_with_invalid_member_cash_expr_with_roles(
            MemberSetExpr::new([MemberSetOp::PushGroup("unknown")]),
            empty_roles(),
        )
    }

    fn script_with_invalid_member_cash_unknown_role_expr() -> Script<'static> {
        script_with_invalid_member_cash_expr_with_roles(
            MemberSetExpr::new([MemberSetOp::PushRole(RoleId(42))]),
            empty_roles(),
        )
    }

    fn script_with_invalid_member_cash_invalid_expression() -> Script<'static> {
        script_with_invalid_member_cash_expr_with_roles(
            MemberSetExpr::new([MemberSetOp::Union]),
            empty_roles(),
        )
    }

    fn script_with_invalid_member_cash_oversized_role_expr() -> Script<'static> {
        script_with_invalid_member_cash_expr_with_roles(
            MemberSetExpr::new([MemberSetOp::PushRole(RoleId(999))]),
            roles_with_large_cash_role(),
        )
    }

    fn script_with_member_cash_on() -> Script<'static> {
        script_with_member_cash_on_with_roles(empty_roles())
    }

    fn script_with_command_cash_large_role() -> Script<'static> {
        script_with_command_cash_with_roles(
            MemberSetExpr::new([MemberSetOp::PushRole(RoleId(999))]),
            roles_with_large_cash_role(),
        )
    }

    async fn assert_settle_transfers_equal(
        processor: &MessageProcessor<'_>,
        left: &Script<'_>,
        right: &Script<'_>,
    ) {
        let left_result = processor
            .build_settlement_result(left)
            .await
            .expect("left settle should succeed");
        let right_result = processor
            .build_settlement_result(right)
            .await
            .expect("right settle should succeed");

        let left_transfers = left_result
            .settle_up
            .expect("left settle context")
            .immediate_transfers;
        let right_transfers = right_result
            .settle_up
            .expect("right settle context")
            .immediate_transfers;
        assert_eq!(left_transfers, right_transfers);
    }

    #[rstest]
    #[case::persisted_and_command_cash(script_with_persisted_cash(), script_with_command_cash())]
    #[case::invalid_group_cash_expr_keeps_previous_state(
        script_with_invalid_member_cash_unknown_group_expr(),
        script_with_member_cash_on()
    )]
    #[case::invalid_role_cash_expr_keeps_previous_state(
        script_with_invalid_member_cash_unknown_role_expr(),
        script_with_member_cash_on()
    )]
    #[case::invalid_stack_cash_expr_keeps_previous_state(
        script_with_invalid_member_cash_invalid_expression(),
        script_with_member_cash_on()
    )]
    #[case::oversized_single_role_cash_expr_is_applied(
        script_with_invalid_member_cash_oversized_role_expr(),
        script_with_command_cash_large_role()
    )]
    #[tokio::test(flavor = "multi_thread")]
    async fn cash_configuration_equivalence(
        #[case] left: Script<'static>,
        #[case] right: Script<'static>,
    ) {
        let processor = MessageProcessor::new(&StubParser, &NoopOptimizer);
        assert_settle_transfers_equal(&processor, &left, &right).await;
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn script_local_cash_persists_across_multiple_settleup_commands() {
        let processor = MessageProcessor::new(&StubParser, &NoopOptimizer);

        let members = union_members(&[1, 2, 3, 4]);

        let mut statements = base_cash_sensitivity_payments();
        statements.extend([
            ScriptStatementWithLine {
                line: 5,
                statement: ScriptStatement::Command(Command::MemberAddCash {
                    members: union_members(&[1]),
                }),
            },
            ScriptStatementWithLine {
                line: 6,
                statement: ScriptStatement::Command(Command::SettleUp {
                    members: members.clone(),
                    cash_members: None,
                }),
            },
            payment_stmt(7, 1, 3, 700),
            payment_stmt(8, 1, 4, 600),
            payment_stmt(9, 2, 3, 50),
            payment_stmt(10, 2, 4, 50),
            ScriptStatementWithLine {
                line: 11,
                statement: ScriptStatement::Command(Command::SettleUp {
                    members,
                    cash_members: None,
                }),
            },
        ]);

        let script = Script::new(&[], empty_roles(), statements);

        let first = processor
            .build_settlement_result_for_prefix(&script, 5)
            .await
            .expect("first settle should succeed");
        let second = processor
            .build_settlement_result_for_prefix(&script, 10)
            .await
            .expect("second settle should succeed");

        let first_transfers = first
            .settle_up
            .expect("first settle context")
            .immediate_transfers;
        let second_transfers = second
            .settle_up
            .expect("second settle context")
            .immediate_transfers;
        assert_eq!(first_transfers, second_transfers);
    }

    #[rstest]
    #[case::fractional_thirds_rounding(100, 67, -34, -33)]
    #[case::exact_thirds_no_rounding(99, 66, -33, -33)]
    #[tokio::test(flavor = "multi_thread")]
    async fn apply_statements_quantizes_balances_when_settlement_enabled(
        #[case] amount: i64,
        #[case] expected_member_1: i64,
        #[case] expected_member_2: i64,
        #[case] expected_member_3: i64,
    ) {
        let processor = MessageProcessor::new(&StubParser, &NoopOptimizer);
        let script = Script::new(
            &[],
            empty_roles(),
            vec![ScriptStatementWithLine {
                line: 1,
                statement: ScriptStatement::Domain(Statement::Payment(Payment::even(
                    Money::from_i64(amount),
                    MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                    MemberSetExpr::new([
                        MemberSetOp::Push(MemberId(1)),
                        MemberSetOp::Push(MemberId(2)),
                        MemberSetOp::Union,
                        MemberSetOp::Push(MemberId(3)),
                        MemberSetOp::Union,
                    ]),
                ))),
            }],
        );

        let apply = processor
            .apply_statements(&script, None, true)
            .await
            .expect("apply should succeed");

        assert_eq!(
            apply.balances.get(&MemberId(1)),
            Some(&Money::from_i64(expected_member_1))
        );
        assert_eq!(
            apply.balances.get(&MemberId(2)),
            Some(&Money::from_i64(expected_member_2))
        );
        assert_eq!(
            apply.balances.get(&MemberId(3)),
            Some(&Money::from_i64(expected_member_3))
        );
    }

    #[derive(Clone, Copy, Debug)]
    enum BalanceCalculationRequest {
        Full,
        Prefix(usize),
    }

    async fn calculate_balances_for_request(
        processor: &MessageProcessor<'_>,
        script: &Script<'_>,
        request: BalanceCalculationRequest,
    ) -> Result<walicord_domain::MemberBalances, BalanceCalculationError> {
        match request {
            BalanceCalculationRequest::Full => processor.calculate_balances(script).await,
            BalanceCalculationRequest::Prefix(prefix_len) => {
                processor
                    .calculate_balances_for_prefix(script, prefix_len)
                    .await
            }
        }
    }

    #[rstest]
    #[case::full_zero_total_weight(
        script_with_zero_total_weight_payment(),
        BalanceCalculationRequest::Full,
        Err(BalanceCalculationError::ZeroTotalWeight)
    )]
    #[case::prefix_zero_total_weight(
        script_with_zero_total_weight_payment(),
        BalanceCalculationRequest::Prefix(1),
        Err(BalanceCalculationError::ZeroTotalWeight)
    )]
    #[case::full_weight_overflow(
        script_with_weight_overflow_payment(),
        BalanceCalculationRequest::Full,
        Err(BalanceCalculationError::WeightOverflow)
    )]
    #[case::prefix_weight_overflow(
        script_with_weight_overflow_payment(),
        BalanceCalculationRequest::Prefix(1),
        Err(BalanceCalculationError::WeightOverflow)
    )]
    #[tokio::test(flavor = "multi_thread")]
    async fn calculate_balances_error_cases(
        #[case] script: Script<'static>,
        #[case] request: BalanceCalculationRequest,
        #[case] expected: Result<(), BalanceCalculationError>,
    ) {
        let processor = MessageProcessor::new(&StubParser, &NoopOptimizer);

        let result = calculate_balances_for_request(&processor, &script, request).await;

        assert_eq!(result.map(|_| ()), expected);
    }

    #[rstest]
    #[case::zero_sum_invariant_violation(
        walicord_domain::SettlementRoundingError::ZeroSumInvariantViolation,
        SettlementOptimizationError::QuantizationZeroSumInvariantViolation
    )]
    #[case::non_integral(
        walicord_domain::SettlementRoundingError::NonIntegral,
        SettlementOptimizationError::QuantizationNonIntegral
    )]
    #[case::unsupported_scale(
        walicord_domain::SettlementRoundingError::UnsupportedScale {
            scale: 30,
            max_supported: 22,
        },
        SettlementOptimizationError::QuantizationUnsupportedScale {
            scale: 30,
            max_supported: 22,
        }
    )]
    #[case::transfer_invalid_grid(
        walicord_domain::SettlementRoundingError::TransferConstructionInvalidGrid {
            g1: 1000,
            g2: 300,
        },
        SettlementOptimizationError::InvalidGrid { g1: 1000, g2: 300 }
    )]
    #[case::transfer_model_too_large(
        walicord_domain::SettlementRoundingError::TransferConstructionModelTooLarge {
            edge_count: 121,
            max_edges: 120,
        },
        SettlementOptimizationError::ModelTooLarge {
            edge_count: 121,
            max_edges: 120,
        }
    )]
    fn settlement_optimization_error_from_rounding_error(
        #[case] input: walicord_domain::SettlementRoundingError,
        #[case] expected: SettlementOptimizationError,
    ) {
        assert_eq!(SettlementOptimizationError::from(input), expected);
    }

    #[rstest]
    #[case("a\nb", "c", 3)]
    #[case("a\nb\n", "c", 4)]
    fn parse_program_sequence_offsets_lines(
        #[case] first: &str,
        #[case] second: &str,
        #[case] expected_second_line: usize,
    ) {
        let processor = MessageProcessor::new(&SequenceParser, &NoopOptimizer);
        let members: [MemberId; 0] = [];
        let outcome = processor.parse_program_sequence(
            &members,
            empty_roles(),
            vec![(first, Some(MemberId(1))), (second, Some(MemberId(2)))],
        );

        let program = outcome.into_result().expect("sequence should parse");

        let lines: Vec<_> = program.statements().iter().map(|stmt| stmt.line).collect();
        assert_eq!(lines, vec![1, expected_second_line]);
    }

    #[rstest]
    #[case::syntax_without_trailing_newline(
        "a\nb",
        "SYNTAX",
        Err(ProgramParseError::SyntaxError {
            line: 3,
            kind: crate::SyntaxErrorKind::ParseFailure { attempted_form: None, expected: crate::ExpectedElement::Unknown, near: "stub".to_string() },
        })
    )]
    #[case::syntax_with_trailing_newline(
        "a\nb\n",
        "SYNTAX",
        Err(ProgramParseError::SyntaxError {
            line: 4,
            kind: crate::SyntaxErrorKind::ParseFailure { attempted_form: None, expected: crate::ExpectedElement::Unknown, near: "stub".to_string() },
        })
    )]
    #[case::implicit_without_trailing_newline(
        "a\nb",
        "IMPLICIT",
        Err(ProgramParseError::MissingContextForImplicitAuthor { line: 3 })
    )]
    #[case::implicit_with_trailing_newline(
        "a\nb\n",
        "IMPLICIT",
        Err(ProgramParseError::MissingContextForImplicitAuthor { line: 4 })
    )]
    fn parse_program_sequence_offsets_error_cases(
        #[case] first: &str,
        #[case] second: &str,
        #[case] expected: Result<(), ProgramParseError>,
    ) {
        let processor = MessageProcessor::new(&SequenceParser, &NoopOptimizer);
        let members: [MemberId; 0] = [];
        let outcome = processor.parse_program_sequence(
            &members,
            empty_roles(),
            vec![(first, Some(MemberId(1))), (second, Some(MemberId(2)))],
        );

        assert_eq!(outcome.into_result().map(|_| ()), expected);
    }

    proptest! {
        #[test]
        fn line_count_increment_matches_concat(prev in any_string(), current in any_string()) {
            let prev_line_count = prev.lines().count();
            let prev_ended_with_newline = prev.is_empty() || prev.ends_with('\n');
            let increment = line_count_increment(&current, prev_ended_with_newline);
            let concatenated = format!("{prev}\n{current}");

            prop_assert_eq!(prev_line_count + increment, concatenated.lines().count());
        }
    }
}
