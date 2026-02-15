use crate::{
    SettlementOptimizationError,
    error::ProgramParseError,
    model::{
        Command, PersonBalance, Script, ScriptStatement, ScriptStatementWithLine, SettleUpContext,
    },
    ports::{ProgramParser, SettlementOptimizer},
};
use std::{borrow::Cow, collections::HashSet};
use walicord_domain::{
    BalanceAccumulator, MemberBalances, SettleUpPolicy, SettlementContext, Transfer,
    model::MemberId, quantize_balances,
};

pub struct SettlementResult {
    pub balances: Vec<PersonBalance>,
    pub optimized_transfers: Vec<Transfer>,
    pub settle_up: Option<SettleUpContext>,
    pub quantization_scale: u32,
}

#[derive(Clone, Copy)]
pub struct MessageProcessor<'a> {
    parser: &'a dyn ProgramParser,
    optimizer: &'a dyn SettlementOptimizer,
}

pub enum ProcessingOutcome<'a> {
    Success(Script<'a>),
    FailedToEvaluateGroup { name: Cow<'a, str>, line: usize },
    UndefinedGroup { name: Cow<'a, str>, line: usize },
    UndefinedMember { id: u64, line: usize },
    SyntaxError { line: usize, detail: String },
    MissingContextForImplicitAuthor { line: usize },
    InvalidAmountExpression { line: usize, detail: String },
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
        content: &'b str,
        author_id: Option<MemberId>,
    ) -> ProcessingOutcome<'b>
    where
        'a: 'b,
    {
        match self.parser.parse(member_ids, content, author_id) {
            Ok(program) => ProcessingOutcome::Success(program),
            Err(err) => Self::map_parse_error(err, 0),
        }
    }

    pub fn parse_program_sequence<'b, I>(
        &self,
        member_ids: &'b [MemberId],
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

            match self.parser.parse(member_ids, content, author_id) {
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

        ProcessingOutcome::Success(Script::new(member_ids, statements))
    }

    pub async fn calculate_balances(&self, program: &Script<'_>) -> MemberBalances {
        // apply_settle=false skips settle-up quantization/optimization paths, so rounding-related
        // failures are not expected in this code path.
        self.apply_statements(program, None, false)
            .await
            .expect("apply_statements should not fail without settlement")
            .balances
    }

    pub async fn calculate_balances_for_prefix(
        &self,
        program: &Script<'_>,
        prefix_len: usize,
    ) -> MemberBalances {
        // apply_settle=false skips settle-up quantization/optimization paths, so rounding-related
        // failures are not expected in this code path.
        self.apply_statements(program, Some(prefix_len), false)
            .await
            .expect("apply_statements should not fail without settlement")
            .balances
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
        let mut person_balances: Vec<PersonBalance> = apply_result
            .balances
            .iter()
            .map(|(id, balance)| PersonBalance {
                id: *id,
                balance: *balance,
            })
            .collect();
        person_balances.sort_by_key(|p| p.id);

        let context = SettlementContext {
            scale: apply_result.quantization_scale,
            ..Self::settlement_context()
        };

        let optimizer = self.optimizer;
        let person_balances_clone = person_balances.clone();

        let optimized_transfers = match apply_result.settle_up.as_ref() {
            Some(settle_up) => {
                let settle_members = settle_up.settle_members.clone();
                let cash_members = apply_result.settle_up_cash_members.clone();
                tokio::task::block_in_place(move || {
                    optimizer.optimize(
                        &person_balances_clone,
                        &settle_members,
                        &cash_members,
                        context,
                    )
                })?
            }
            None => {
                let all_members: Vec<MemberId> = person_balances_clone
                    .iter()
                    .map(|balance| balance.id)
                    .collect();
                let cash_members = apply_result.review_cash_members.clone();
                tokio::task::block_in_place(move || {
                    optimizer.optimize(&person_balances_clone, &all_members, &cash_members, context)
                })?
            }
        };

        Ok(SettlementResult {
            balances: person_balances,
            optimized_transfers,
            settle_up: apply_result.settle_up,
            quantization_scale: apply_result.quantization_scale,
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

        let mut accumulator = BalanceAccumulator::new_with_members(program.members());
        let mut last_settle_members: Vec<MemberId> = Vec::new();
        let mut last_settle_cash_members: Vec<MemberId> = Vec::new();
        let mut last_settle_transfers: Vec<Transfer> = Vec::new();
        let last_is_settle = apply_settle
            && end > 0
            && matches!(
                statements[end - 1].statement,
                ScriptStatement::Command(Command::SettleUp { .. })
            );
        // Script-local cash preference state while scanning commands in order.
        let mut script_local_cash_members: HashSet<MemberId> = HashSet::new();

        for stmt in &statements[..end] {
            match &stmt.statement {
                ScriptStatement::Domain(statement) => {
                    accumulator.apply(statement);
                }
                ScriptStatement::Command(command) => {
                    if !apply_settle {
                        continue;
                    }
                    match command {
                        Command::Variables | Command::Review => {}
                        Command::MemberAddCash { members } => {
                            let Some(target_members) = accumulator.evaluate_members(members) else {
                                continue;
                            };
                            script_local_cash_members.extend(target_members.iter());
                        }
                        Command::SettleUp {
                            members,
                            cash_members,
                        } => {
                            last_settle_members.clear();
                            last_settle_cash_members.clear();
                            last_settle_transfers.clear();

                            let Some(settle_members) = accumulator.evaluate_members(members) else {
                                continue;
                            };
                            if settle_members.is_empty() {
                                continue;
                            }

                            last_settle_members.extend(settle_members.iter());

                            let command_cash_members =
                                if let Some(command_cash_members) = cash_members {
                                    let Some(command_set) =
                                        accumulator.evaluate_members(command_cash_members)
                                    else {
                                        continue;
                                    };
                                    Some(command_set)
                                } else {
                                    None
                                };

                            let mut effective_cash_members: Vec<MemberId> =
                                script_local_cash_members.iter().copied().collect();
                            effective_cash_members
                                .extend(command_cash_members.iter().flat_map(|set| set.iter()));
                            effective_cash_members.sort_unstable();
                            effective_cash_members.dedup();

                            let balances = accumulator.balances().clone();
                            let settle_members_vec: Vec<_> = settle_members.members().to_vec();
                            let effective_cash_vec = effective_cash_members.clone();
                            let context = Self::settlement_context();
                            let result = tokio::task::block_in_place(move || {
                                SettleUpPolicy::settle(
                                    &balances,
                                    &settle_members_vec,
                                    effective_cash_vec.iter().copied(),
                                    context,
                                )
                            })
                            .map_err(SettlementOptimizationError::from)?;
                            accumulator.set_balances(result.new_balances);
                            last_settle_cash_members.extend(effective_cash_members);
                            last_settle_transfers.extend(result.transfers);
                        }
                    }
                }
            }
        }

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
        let balances = if apply_settle {
            quantize_balances(&balances, context).map_err(SettlementOptimizationError::from)?
        } else {
            balances
        };

        let mut review_cash_members: Vec<MemberId> =
            script_local_cash_members.iter().copied().collect();
        review_cash_members.sort_unstable();

        Ok(ApplyResult {
            balances,
            settle_up,
            settle_up_cash_members: last_settle_cash_members,
            review_cash_members,
            quantization_scale: context.scale,
        })
    }

    fn prefix_end<'b>(
        &self,
        statements: &[ScriptStatementWithLine<'b>],
        prefix_len: usize,
    ) -> usize {
        let prefix_len = prefix_len.min(statements.len());
        if prefix_len < statements.len()
            && matches!(
                statements[prefix_len].statement,
                ScriptStatement::Command(_)
            )
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
            ProgramParseError::UndefinedMember { id, line } => ProcessingOutcome::UndefinedMember {
                id,
                line: line + offset,
            },
            ProgramParseError::SyntaxError { line, detail } => ProcessingOutcome::SyntaxError {
                line: line + offset,
                detail,
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        error::SettlementOptimizationError,
        ports::{ProgramParser, SettlementOptimizer},
    };
    use proptest::prelude::*;
    use rstest::{fixture, rstest};
    use walicord_domain::{
        Payment, Statement,
        model::{MemberId, MemberSetExpr, MemberSetOp, Money},
    };

    fn any_string() -> impl Strategy<Value = String> {
        proptest::collection::vec(any::<char>(), 0..40)
            .prop_map(|chars| chars.into_iter().collect())
    }

    struct StubParser;

    impl ProgramParser for StubParser {
        fn parse<'a>(
            &self,
            _member_ids: &'a [MemberId],
            _content: &'a str,
            _author_id: Option<MemberId>,
        ) -> Result<Script<'a>, ProgramParseError<'a>> {
            let statements = vec![
                ScriptStatementWithLine {
                    line: 1,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment {
                        amount: Money::try_from(60).expect("amount should fit in i64"),
                        payer: MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                        payee: MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
                    })),
                },
                ScriptStatementWithLine {
                    line: 2,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment {
                        amount: Money::try_from(40).expect("amount should fit in i64"),
                        payer: MemberSetExpr::new([MemberSetOp::Push(MemberId(2))]),
                        payee: MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
                    })),
                },
                ScriptStatementWithLine {
                    line: 3,
                    statement: ScriptStatement::Command(Command::SettleUp {
                        members: MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                        cash_members: None,
                    }),
                },
            ];

            Ok(Script::new(&[], statements))
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
            content: &'a str,
            _author_id: Option<MemberId>,
        ) -> Result<Script<'a>, ProgramParseError<'a>> {
            if content.contains("SYNTAX") {
                return Err(ProgramParseError::SyntaxError {
                    line: 1,
                    detail: "Syntax error - stub".to_string(),
                });
            }
            if content.contains("IMPLICIT") {
                return Err(ProgramParseError::MissingContextForImplicitAuthor { line: 1 });
            }

            Ok(Script::new(
                &[],
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
        let program = match processor.parse_program(&members, "unused", None) {
            ProcessingOutcome::Success(program) => program,
            _ => panic!("unexpected parse outcome"),
        };

        let last_index = program.statements().len().saturating_sub(1);
        let result = processor
            .build_settlement_result_for_prefix(&program, last_index)
            .await
            .expect("result generation failed");

        let settle_up = result.settle_up.expect("expected settle up context");
        assert!(!settle_up.immediate_transfers.is_empty());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn build_settlement_result_passes_settle_members_to_optimizer() {
        let parser = StubParser;
        let optimizer = AssertSettleMembersOptimizer {
            expected: vec![MemberId(1)],
        };
        let processor = MessageProcessor::new(&parser, &optimizer);

        let members: [MemberId; 0] = [];
        let program = match processor.parse_program(&members, "unused", None) {
            ProcessingOutcome::Success(program) => program,
            _ => panic!("unexpected parse outcome"),
        };

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
            vec![
                ScriptStatementWithLine {
                    line: 1,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment {
                        amount: Money::from_i64(60),
                        payer: MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                        payee: MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
                    })),
                },
                ScriptStatementWithLine {
                    line: 2,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment {
                        amount: Money::from_i64(40),
                        payer: MemberSetExpr::new([MemberSetOp::Push(MemberId(2))]),
                        payee: MemberSetExpr::new([MemberSetOp::Push(MemberId(3))]),
                    })),
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
        let script = Script::new(&[], statements);

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
            statement: ScriptStatement::Domain(Statement::Payment(Payment {
                amount: Money::from_i64(amount),
                payer: MemberSetExpr::new([MemberSetOp::Push(MemberId(payer))]),
                payee: MemberSetExpr::new([MemberSetOp::Push(MemberId(payee))]),
            })),
        }
    }

    fn union_members(ids: &[u64]) -> MemberSetExpr<'static> {
        let mut ops = Vec::new();
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

    fn script_with_persisted_cash() -> Script<'static> {
        let members = union_members(&[1, 2, 3, 4]);
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
                    members: members.clone(),
                    cash_members: None,
                }),
            },
        ]);

        Script::new(&[], persisted_statements)
    }

    fn script_with_command_cash() -> Script<'static> {
        let members = union_members(&[1, 2, 3, 4]);
        let mut command_statements = base_cash_sensitivity_payments();
        command_statements.push(ScriptStatementWithLine {
            line: 5,
            statement: ScriptStatement::Command(Command::SettleUp {
                members,
                cash_members: Some(union_members(&[1])),
            }),
        });

        Script::new(&[], command_statements)
    }

    fn script_with_invalid_member_cash_expr() -> Script<'static> {
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
                    members: MemberSetExpr::new([MemberSetOp::PushGroup("unknown")]),
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

        Script::new(&[], with_invalid_cash)
    }

    fn script_with_member_cash_on() -> Script<'static> {
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

        Script::new(&[], baseline)
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
    #[case::invalid_cash_expr_keeps_previous_state(
        script_with_invalid_member_cash_expr(),
        script_with_member_cash_on()
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

        let script = Script::new(&[], statements);

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
    #[case::jpy_thirds(100, 67, -34, -33)]
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
            vec![ScriptStatementWithLine {
                line: 1,
                statement: ScriptStatement::Domain(Statement::Payment(Payment {
                    amount: Money::from_i64(amount),
                    payer: MemberSetExpr::new([MemberSetOp::Push(MemberId(1))]),
                    payee: MemberSetExpr::new([
                        MemberSetOp::Push(MemberId(1)),
                        MemberSetOp::Push(MemberId(2)),
                        MemberSetOp::Union,
                        MemberSetOp::Push(MemberId(3)),
                        MemberSetOp::Union,
                    ]),
                })),
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
            vec![(first, Some(MemberId(1))), (second, Some(MemberId(2)))],
        );

        let program = match outcome {
            ProcessingOutcome::Success(program) => program,
            _ => panic!("unexpected parse outcome"),
        };

        let lines: Vec<_> = program.statements().iter().map(|stmt| stmt.line).collect();
        assert_eq!(lines, vec![1, expected_second_line]);
    }

    #[rstest]
    #[case("a\nb", 3)]
    #[case("a\nb\n", 4)]
    fn parse_program_sequence_offsets_syntax_error(
        #[case] first: &str,
        #[case] expected_line: usize,
    ) {
        let processor = MessageProcessor::new(&SequenceParser, &NoopOptimizer);
        let members: [MemberId; 0] = [];
        let outcome = processor.parse_program_sequence(
            &members,
            vec![(first, Some(MemberId(1))), ("SYNTAX", Some(MemberId(2)))],
        );

        let ProcessingOutcome::SyntaxError { line, detail } = outcome else {
            panic!("unexpected parse outcome");
        };

        assert_eq!(line, expected_line);
        assert_eq!(detail, "Syntax error - stub");
    }

    #[rstest]
    #[case("a\nb", 3)]
    #[case("a\nb\n", 4)]
    fn parse_program_sequence_offsets_implicit_payer_error(
        #[case] first: &str,
        #[case] expected_line: usize,
    ) {
        let processor = MessageProcessor::new(&SequenceParser, &NoopOptimizer);
        let members: [MemberId; 0] = [];
        let outcome = processor.parse_program_sequence(
            &members,
            vec![(first, Some(MemberId(1))), ("IMPLICIT", Some(MemberId(2)))],
        );

        let ProcessingOutcome::MissingContextForImplicitAuthor { line } = outcome else {
            panic!("unexpected parse outcome");
        };

        assert_eq!(line, expected_line);
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
