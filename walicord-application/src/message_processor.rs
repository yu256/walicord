use crate::{
    error::ProgramParseError,
    model::{
        Command, PersonBalance, Script, ScriptStatement, ScriptStatementWithLine, SettleUpContext,
    },
    ports::{ProgramParser, SettlementOptimizer},
    SettlementOptimizationError,
};
use std::collections::HashMap;
use walicord_domain::{BalanceAccumulator, Money, SettleUpPolicy, Transfer};

pub struct SettlementResult<'a> {
    pub balances: Vec<PersonBalance<'a>>,
    pub optimized_transfers: Vec<Transfer<'a>>,
    pub settle_up: Option<SettleUpContext<'a>>,
}

#[derive(Clone, Copy)]
pub struct MessageProcessor<'a> {
    parser: &'a dyn ProgramParser,
    optimizer: &'a dyn SettlementOptimizer,
}

pub enum ProcessingOutcome<'a> {
    Success(Script<'a>),
    MissingMembersDeclaration,
    UndefinedMember { name: &'a str, line: usize },
    FailedToEvaluateGroup { name: &'a str },
    SyntaxError { message: String },
}

struct ApplyResult<'a> {
    balances: HashMap<&'a str, Money>,
    settle_up: Option<SettleUpContext<'a>>,
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
            Err(ProgramParseError::MissingMembersDeclaration) => {
                ProcessingOutcome::MissingMembersDeclaration
            }
            Err(ProgramParseError::UndefinedMember { name, line }) => {
                ProcessingOutcome::UndefinedMember { name, line }
            }
            Err(ProgramParseError::FailedToEvaluateGroup { name }) => {
                ProcessingOutcome::FailedToEvaluateGroup { name }
            }
            Err(ProgramParseError::SyntaxError(message)) => {
                ProcessingOutcome::SyntaxError { message }
            }
        }
    }

    pub fn calculate_balances<'b>(&self, program: &'b Script<'b>) -> HashMap<&'b str, Money>
    where
        'a: 'b,
    {
        self.apply_statements(program, None, false).balances
    }

    pub fn calculate_balances_for_prefix<'b>(
        &self,
        program: &'b Script<'b>,
        prefix_len: usize,
    ) -> HashMap<&'b str, Money>
    where
        'a: 'b,
    {
        self.apply_statements(program, Some(prefix_len), false)
            .balances
    }

    pub fn build_settlement_result<'b>(
        &self,
        program: &'b Script<'b>,
    ) -> Result<SettlementResult<'b>, SettlementOptimizationError>
    where
        'a: 'b,
    {
        self.build_settlement_result_from_apply(self.apply_statements(program, None, true))
    }

    pub fn build_settlement_result_for_prefix<'b>(
        &self,
        program: &'b Script<'b>,
        prefix_len: usize,
    ) -> Result<SettlementResult<'b>, SettlementOptimizationError>
    where
        'a: 'b,
    {
        self.build_settlement_result_from_apply(self.apply_statements(
            program,
            Some(prefix_len),
            true,
        ))
    }

    fn build_settlement_result_from_apply<'b>(
        &self,
        apply_result: ApplyResult<'b>,
    ) -> Result<SettlementResult<'b>, SettlementOptimizationError> {
        let mut person_balances: Vec<PersonBalance<'b>> = apply_result
            .balances
            .iter()
            .map(|(name, balance)| PersonBalance {
                name,
                balance: *balance,
            })
            .collect();
        person_balances.sort_by_key(|p| p.name);

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
    ) -> ApplyResult<'b>
    where
        'a: 'b,
    {
        let statements = program.statements();
        let end = match prefix_len {
            Some(prefix_len) => self.prefix_end(statements, prefix_len),
            None => statements.len(),
        };

        let mut accumulator = BalanceAccumulator::new(program.members());
        let mut last_settle_members: Vec<&'b str> = Vec::new();
        let mut last_settle_transfers: Vec<Transfer<'b>> = Vec::new();
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

                            let result = SettleUpPolicy::settle(
                                accumulator.balances().clone(),
                                program.members(),
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
        model::{MemberSetExpr, MemberSetOp, Money},
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
                        payer: MemberSetExpr::new(vec![MemberSetOp::Push("A")]),
                        payee: MemberSetExpr::new(vec![MemberSetOp::Push("C")]),
                    })),
                },
                ScriptStatementWithLine {
                    line: 2,
                    statement: ScriptStatement::Domain(Statement::Payment(Payment {
                        amount: Money::from_u64(40),
                        payer: MemberSetExpr::new(vec![MemberSetOp::Push("B")]),
                        payee: MemberSetExpr::new(vec![MemberSetOp::Push("C")]),
                    })),
                },
                ScriptStatementWithLine {
                    line: 3,
                    statement: ScriptStatement::Command(Command::SettleUp(MemberSetExpr::new(
                        vec![MemberSetOp::Push("A")],
                    ))),
                },
            ];

            Ok(Script::new(&["A", "B", "C"], statements))
        }
    }

    struct NoopOptimizer;

    impl SettlementOptimizer for NoopOptimizer {
        fn optimize<'a>(
            &self,
            _balances: &[PersonBalance<'a>],
        ) -> Result<Vec<Transfer<'a>>, SettlementOptimizationError> {
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
