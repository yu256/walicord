use crate::{
    error::ProgramParseError,
    model::{
        Command, PersonBalance, Script, ScriptStatement, ScriptStatementWithLine, SettleUpContext,
    },
    ports::{ProgramParser, SettlementOptimizer},
    SettlementOptimizationError,
};
use std::borrow::Cow;
use walicord_domain::{
    model::MemberId, BalanceAccumulator, MemberBalances, SettleUpPolicy, Transfer,
};

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
    SyntaxError { line: usize, detail: String },
    ImplicitPayerWithoutAuthor { line: usize },
}

struct ApplyResult {
    balances: MemberBalances,
    settle_up: Option<SettleUpContext>,
}

impl<'a> MessageProcessor<'a> {
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
                line_count += content.lines().count() + if ends_with_newline { 1 } else { 0 };
            } else {
                line_count = content.lines().count();
            }
            ends_with_newline = content.is_empty() || content.ends_with('\n');
            has_any = true;
        }

        ProcessingOutcome::Success(Script::new(member_ids, statements))
    }

    pub fn calculate_balances<'b>(&self, program: &'b Script<'b>) -> MemberBalances
    where
        'a: 'b,
    {
        self.apply_statements(program, None, false).balances
    }

    pub fn calculate_balances_for_prefix<'b>(
        &self,
        program: &'b Script<'b>,
        prefix_len: usize,
    ) -> MemberBalances
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

        let mut accumulator = BalanceAccumulator::new_with_members(program.members());
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

                            let result = SettleUpPolicy::settle(
                                accumulator.balances().clone(),
                                accumulator
                                    .balances()
                                    .keys()
                                    .copied()
                                    .chain(settle_members.iter()),
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
            ProgramParseError::ImplicitPayerWithoutAuthor { line } => {
                ProcessingOutcome::ImplicitPayerWithoutAuthor {
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
            _member_ids: &'a [MemberId],
            _content: &'a str,
            _author_id: Option<MemberId>,
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

            Ok(Script::new(&[], statements))
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
                return Err(ProgramParseError::ImplicitPayerWithoutAuthor { line: 1 });
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
    fn settle_up_response_groups_transfers(processor: MessageProcessor<'_>) {
        let members: [MemberId; 0] = [];
        let program = match processor.parse_program(&members, "unused", None) {
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

        let ProcessingOutcome::ImplicitPayerWithoutAuthor { line } = outcome else {
            panic!("unexpected parse outcome");
        };

        assert_eq!(line, expected_line);
    }
}
