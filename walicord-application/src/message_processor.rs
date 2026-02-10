use crate::{
    error::ProgramParseError,
    model::{
        Command, PersonBalance, Script, ScriptStatement, ScriptStatementWithLine, SettleUpContext,
    },
    ports::{ProgramParser, SettlementOptimizer},
    SettlementOptimizationError,
};
use fxhash::FxHashMap;
use std::borrow::Cow;
use walicord_domain::{model::MemberId, BalanceAccumulator, Money, SettleUpPolicy, Transfer};

pub struct SettlementResult {
    pub balances: Vec<PersonBalance>,
    pub optimized_transfers: Vec<Transfer>,
    pub settle_up: Option<SettleUpContext>,
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
    SyntaxError { message: String },
}

struct ApplyResult {
    balances: FxHashMap<MemberId, Money>,
    settle_up: Option<SettleUpContext>,
}

impl<'a> MessageProcessor<'a> {
    pub fn new(parser: &'a dyn ProgramParser, optimizer: &'a dyn SettlementOptimizer) -> Self {
        Self { parser, optimizer }
    }

    pub fn parse_program<'b>(
        &self,
        members: &'b [&'b str],
        content: &'b str,
    ) -> ProcessingOutcome<'b>
    where
        'a: 'b,
    {
        match self.parser.parse(members, content) {
            Ok(program) => ProcessingOutcome::Success(program),
            Err(ProgramParseError::FailedToEvaluateGroup { name, line }) => {
                ProcessingOutcome::FailedToEvaluateGroup { name, line }
            }
            Err(ProgramParseError::UndefinedGroup { name, line }) => {
                ProcessingOutcome::UndefinedGroup { name, line }
            }
            Err(ProgramParseError::UndefinedMember { id, line }) => {
                ProcessingOutcome::UndefinedMember { id, line }
            }
            Err(ProgramParseError::SyntaxError(message)) => {
                ProcessingOutcome::SyntaxError { message }
            }
        }
    }

    pub fn calculate_balances<'b>(&self, program: &'b Script<'b>) -> FxHashMap<MemberId, Money>
    where
        'a: 'b,
    {
        self.apply_statements(program, None, false).balances
    }

    pub fn calculate_balances_for_prefix<'b>(
        &self,
        program: &'b Script<'b>,
        prefix_len: usize,
    ) -> FxHashMap<MemberId, Money>
    where
        'a: 'b,
    {
        self.apply_statements(program, Some(prefix_len), false)
            .balances
    }

    pub fn build_settlement_result<'b>(
        &self,
        program: &'b Script<'b>,
    ) -> Result<SettlementResult, SettlementOptimizationError>
    where
        'a: 'b,
    {
        self.build_settlement_result_from_apply(self.apply_statements(program, None, true))
    }

    pub fn build_settlement_result_for_prefix<'b>(
        &self,
        program: &'b Script<'b>,
        prefix_len: usize,
    ) -> Result<SettlementResult, SettlementOptimizationError>
    where
        'a: 'b,
    {
        self.build_settlement_result_from_apply(self.apply_statements(
            program,
            Some(prefix_len),
            true,
        ))
    }

    fn build_settlement_result_from_apply(
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

        let optimized_transfers = self.optimizer.optimize(&person_balances)?;

        Ok(SettlementResult {
            balances: person_balances,
            optimized_transfers,
            settle_up: apply_result.settle_up,
        })
    }

    fn apply_statements<'b>(
        &self,
        program: &'b Script<'b>,
        prefix_len: Option<usize>,
        apply_settle: bool,
    ) -> ApplyResult
    where
        'a: 'b,
    {
        let statements = program.statements();
        let end = match prefix_len {
            Some(prefix_len) => self.prefix_end(statements, prefix_len),
            None => statements.len(),
        };

        let mut accumulator = BalanceAccumulator::new();
        let mut last_settle_members: Vec<MemberId> = Vec::new();
        let mut last_settle_transfers: Vec<Transfer> = Vec::new();
        let last_is_settle = apply_settle
            && end > 0
            && matches!(
                statements[end - 1].statement,
                ScriptStatement::Command(Command::SettleUp(_))
            );

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
                        Command::Variables | Command::Evaluate => {}
                        Command::SettleUp(expr) => {
                            last_settle_members.clear();
                            last_settle_transfers.clear();

                            let Some(settle_members) = accumulator.evaluate_members(expr) else {
                                continue;
                            };
                            if settle_members.is_empty() {
                                continue;
                            }

                            last_settle_members.extend(settle_members.iter());

                            accumulator.ensure_members(settle_members.iter());

                            let result = SettleUpPolicy::settle(
                                accumulator.balances().clone(),
                                settle_members.members(),
                            );
                            accumulator.set_balances(result.new_balances);
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

        ApplyResult {
            balances: accumulator.into_balances(),
            settle_up,
        }
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        error::SettlementOptimizationError,
        ports::{ProgramParser, SettlementOptimizer},
    };
    use rstest::{fixture, rstest};
    use walicord_domain::{
        model::{MemberId, MemberSetExpr, MemberSetOp, Money},
        Payment, Statement,
    };

    struct StubParser;

    impl ProgramParser for StubParser {
        fn parse<'a>(
            &self,
            _members: &'a [&'a str],
            _content: &'a str,
        ) -> Result<Script<'a>, ProgramParseError<'a>> {
            let statements = vec![
                ScriptStatementWithLine {
                    line: 1,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment {
                        amount: Money::from_u64(60),
                        payer: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(1))]),
                        payee: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(3))]),
                    })),
                },
                ScriptStatementWithLine {
                    line: 2,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment {
                        amount: Money::from_u64(40),
                        payer: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(2))]),
                        payee: MemberSetExpr::new(vec![MemberSetOp::Push(MemberId(3))]),
                    })),
                },
                ScriptStatementWithLine {
                    line: 3,
                    statement: ScriptStatement::Command(Command::SettleUp(MemberSetExpr::new(
                        vec![MemberSetOp::Push(MemberId(1))],
                    ))),
                },
            ];

            Ok(Script::new(&["A", "B", "C"], statements))
        }
    }

    struct NoopOptimizer;

    impl SettlementOptimizer for NoopOptimizer {
        fn optimize(
            &self,
            _balances: &[PersonBalance],
        ) -> Result<Vec<Transfer>, SettlementOptimizationError> {
            Ok(Vec::new())
        }
    }

    #[fixture]
    fn processor() -> MessageProcessor<'static> {
        MessageProcessor::new(&StubParser, &NoopOptimizer)
    }

    #[rstest]
    fn settle_up_response_groups_transfers(processor: MessageProcessor<'_>) {
        let members = ["A", "B", "C"];
        let program = match processor.parse_program(&members, "unused") {
            ProcessingOutcome::Success(program) => program,
            _ => panic!("unexpected parse outcome"),
        };

        let last_index = program.statements().len().saturating_sub(1);
        let result = processor
            .build_settlement_result_for_prefix(&program, last_index)
            .expect("result generation failed");

        let settle_up = result.settle_up.expect("expected settle up context");
        assert!(!settle_up.immediate_transfers.is_empty());
    }
}
