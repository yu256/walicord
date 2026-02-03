use crate::{
    application::svg_table::{Alignment, SvgTableBuilder},
    domain::{
        model::{Command, Statement},
        Program, ProgramParseError, ProgramParser,
    },
    i18n,
};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::Write as _,
};
use walicord_calc::{minimize_transactions, PersonBalance};
use walicord_parser::SetExpr;

pub struct SettlementView {
    pub balance_table_svg: String,
    pub transfer_table_svg: Option<String>,
}

pub struct Settlement<'a> {
    pub new_balances: HashMap<&'a str, i64>,
    pub transfers: Vec<Transfer<'a>>,
}

#[derive(Clone, Copy)]
pub struct MessageProcessor<'a> {
    parser: &'a dyn ProgramParser,
}

pub enum ProcessingOutcome<'a> {
    Success(Program<'a>),
    MissingMembersDeclaration,
    UndefinedMember { name: &'a str, line: usize },
    SyntaxError { message: String },
}

struct SetEnvironment<'a> {
    order: &'a [&'a str],
    order_lookup: HashSet<&'a str>,
    sets: HashMap<&'a str, HashSet<&'a str>>,
}

impl<'a> SetEnvironment<'a> {
    fn new(order: &'a [&'a str]) -> Self {
        let order_lookup: HashSet<&str> = order.iter().copied().collect();

        let mut sets: HashMap<&str, HashSet<&str>> = order
            .iter()
            .copied()
            .map(|member| (member, HashSet::from([member])))
            .collect();
        sets.insert("MEMBERS", order_lookup.clone());

        Self {
            order,
            order_lookup,
            sets,
        }
    }

    fn insert_group<I>(&mut self, name: &'a str, members: I)
    where
        I: IntoIterator<Item = &'a str>,
    {
        self.sets.insert(name, members.into_iter().collect());
    }

    fn resolve_members(&self, expr: &SetExpr<'a>) -> Option<Vec<&'a str>> {
        let result = expr.evaluate(&|name| self.sets.get(name))?;
        Some(self.order_members(result.as_ref()))
    }

    fn order_members(&self, set: &HashSet<&'a str>) -> Vec<&'a str> {
        let mut ordered: Vec<&str> = self
            .order
            .iter()
            .copied()
            .filter(|member| set.contains(member))
            .collect();

        if ordered.len() == set.len() {
            return ordered;
        }

        let mut extras: Vec<&str> = set
            .iter()
            .copied()
            .filter(|member| !self.order_lookup.contains(member))
            .collect();
        extras.sort_unstable();
        ordered.extend(extras);
        ordered
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Transfer<'a> {
    pub from: &'a str,
    pub to: &'a str,
    pub amount: i64,
}

fn distribute<'a>(
    balances: &mut HashMap<&'a str, i64>,
    members: &[&'a str],
    amount: u64,
    direction: i64,
) {
    if members.is_empty() {
        return;
    }

    let len = members.len() as u64;
    let base = amount / len;
    let remainder = (amount % len) as usize;

    for (idx, &member) in members.iter().enumerate() {
        let mut share = base;
        if idx < remainder {
            share += 1;
        }
        *balances.entry(member).or_insert(0) += direction * share as i64;
    }
}

impl<'a> MessageProcessor<'a> {
    pub fn new(parser: &'a dyn ProgramParser) -> Self {
        Self { parser }
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
            Err(ProgramParseError::SyntaxError(message)) => {
                ProcessingOutcome::SyntaxError { message }
            }
        }
    }

    pub fn format_variables_response(&self, program: &Program) -> String {
        self.format_variables_response_from_slice(program.members, &program.statements)
    }

    pub fn format_variables_response_for_prefix(
        &self,
        program: &Program,
        prefix_len: usize,
    ) -> String {
        let prefix_len = prefix_len.min(program.statements.len());
        let statements_slice = if prefix_len < program.statements.len()
            && matches!(program.statements[prefix_len], Statement::Command(_))
        {
            &program.statements[..=prefix_len]
        } else {
            &program.statements[..prefix_len]
        };

        self.format_variables_response_from_slice(program.members, statements_slice)
    }

    fn format_variables_response_from_slice(
        &self,
        members: &[&str],
        statements: &[Statement<'_>],
    ) -> String {
        let mut reply = String::with_capacity(512);
        let _ = writeln!(&mut reply, "`MEMBERS` := {}", members.join(", "));
        reply.push('\n');

        for decl in statements.iter().filter_map(|s| match s {
            Statement::Declaration(d) => Some(d),
            _ => None,
        }) {
            let tmp;
            let listing = if decl.members.is_empty() {
                "[empty]"
            } else {
                tmp = decl.members.join(", ");
                &tmp
            };
            let _ = writeln!(&mut reply, "- `{}` := {listing}", decl.name);
        }

        reply
    }

    pub fn calculate_balances<'b>(&self, program: &'b Program<'b>) -> HashMap<&'b str, i64>
    where
        'a: 'b,
    {
        self.calculate_balances_from_slice(program.members, &program.statements)
    }

    pub fn calculate_balances_for_prefix<'b>(
        &self,
        program: &'b Program<'b>,
        prefix_len: usize,
    ) -> HashMap<&'b str, i64>
    where
        'a: 'b,
    {
        let prefix_len = prefix_len.min(program.statements.len());
        self.calculate_balances_from_slice(program.members, &program.statements[..prefix_len])
    }

    fn calculate_balances_from_slice<'b>(
        &self,
        members: &'b [&'b str],
        statements: &[Statement<'b>],
    ) -> HashMap<&'b str, i64>
    where
        'a: 'b,
    {
        self.calculate_balances_from_slice_internal(members, statements, None, None)
    }

    fn calculate_balances_from_slice_internal<'b>(
        &self,
        members: &'b [&'b str],
        statements: &[Statement<'b>],
        mut last_settle_members: Option<&mut Vec<&'b str>>,
        mut recorded_transfers: Option<&mut Vec<Transfer<'b>>>,
    ) -> HashMap<&'b str, i64>
    where
        'a: 'b,
    {
        let mut balances: HashMap<&'b str, i64> =
            members.iter().copied().map(|member| (member, 0)).collect();

        let mut set_env = SetEnvironment::new(members);

        for stmt in statements {
            match stmt {
                Statement::Declaration(decl) => {
                    for &member in &decl.members {
                        balances.entry(member).or_insert(0);
                    }
                    set_env.insert_group(decl.name, decl.members.iter().copied());
                }
                Statement::Payment(payment) => {
                    let Some(payer_members) = set_env.resolve_members(&payment.payer) else {
                        continue;
                    };
                    let Some(payee_members) = set_env.resolve_members(&payment.payee) else {
                        continue;
                    };

                    distribute(&mut balances, &payer_members, payment.amount, 1);
                    distribute(&mut balances, &payee_members, payment.amount, -1);
                }
                Statement::Command(command) => match command {
                    Command::Variables | Command::Evaluate => {}
                    Command::SettleUp(expr) => {
                        if let Some(out) = last_settle_members.as_mut() {
                            let out = &mut **out;
                            out.clear();
                        }
                        if let Some(record) = recorded_transfers.as_mut() {
                            let record = &mut **record;
                            record.clear();
                        }

                        let Some(settle_members) = set_env.resolve_members(expr) else {
                            continue;
                        };
                        if settle_members.is_empty() {
                            continue;
                        }

                        if let Some(out) = last_settle_members.as_mut() {
                            let out = &mut **out;
                            out.extend(settle_members.iter().copied());
                        }

                        let participants = build_participant_order(members, &balances);
                        let (new_balances, transfers) =
                            settle_selected_members(balances, &participants, &settle_members);
                        balances = new_balances;

                        if let Some(record) = recorded_transfers.as_mut() {
                            let record = &mut **record;
                            record.extend(transfers);
                        }
                    }
                },
            }
        }

        balances
    }

    pub fn format_settlement_response(&self, program: &Program) -> Result<SettlementView, String> {
        self.format_settlement_response_from_slice(program.members, &program.statements)
    }

    pub fn format_settlement_response_for_prefix(
        &self,
        program: &Program,
        prefix_len: usize,
    ) -> Result<SettlementView, String> {
        let prefix_len = prefix_len.min(program.statements.len());
        let statements_slice = if prefix_len < program.statements.len()
            && matches!(program.statements[prefix_len], Statement::Command(_))
        {
            &program.statements[..=prefix_len]
        } else {
            &program.statements[..prefix_len]
        };

        self.format_settlement_response_from_slice(program.members, statements_slice)
    }

    fn format_settlement_response_from_slice(
        &self,
        members: &[&str],
        statements: &[Statement<'_>],
    ) -> Result<SettlementView, String> {
        let is_settle_up = matches!(
            statements.last(),
            Some(Statement::Command(Command::SettleUp(_)))
        );

        let mut settle_members_buffer: Vec<&str> = Vec::new();
        let mut settle_transfer_buffer: Vec<Transfer> = Vec::new();

        let balances = if is_settle_up {
            self.calculate_balances_from_slice_internal(
                members,
                statements,
                Some(&mut settle_members_buffer),
                Some(&mut settle_transfer_buffer),
            )
        } else {
            self.calculate_balances_from_slice_internal(members, statements, None, None)
        };

        let mut person_balances: Vec<PersonBalance> = balances
            .iter()
            .map(|(name, balance)| PersonBalance {
                name,
                balance: *balance,
            })
            .collect();
        person_balances.sort_by_key(|p| p.name);

        let settlements = minimize_transactions(&person_balances, 1.0, 0.001)
            .map_err(|e| format!("{}: {e}", i18n::SETTLEMENT_CALCULATION_FAILED))?;

        // Build balance table SVG
        let balance_table_svg = build_balance_table_svg(&person_balances);

        if is_settle_up {
            let has_any_transfers = !settle_transfer_buffer.is_empty() || !settlements.is_empty();

            if !has_any_transfers {
                return Ok(SettlementView {
                    balance_table_svg: balance_table_svg,
                    transfer_table_svg: None,
                });
            }

            let settle_member_lookup: HashSet<&str> =
                settle_members_buffer.iter().copied().collect();

            let mut pay_from_settle: Vec<Transfer> = Vec::new();
            let mut receive_for_settle: Vec<Transfer> = Vec::new();
            let mut other_settlements: Vec<Transfer> = Vec::new();

            for transfer in settle_transfer_buffer.iter().copied() {
                if settle_member_lookup.contains(transfer.from) {
                    pay_from_settle.push(transfer);
                } else if settle_member_lookup.contains(transfer.to) {
                    receive_for_settle.push(transfer);
                } else {
                    other_settlements.push(transfer);
                }
            }

            for settlement in &settlements {
                let transfer = Transfer {
                    from: settlement.from,
                    to: settlement.to,
                    amount: settlement.amount,
                };
                if settle_member_lookup.contains(transfer.from) {
                    pay_from_settle.push(transfer);
                } else if settle_member_lookup.contains(transfer.to) {
                    receive_for_settle.push(transfer);
                } else {
                    other_settlements.push(transfer);
                }
            }

            sort_transfers(&mut pay_from_settle);
            sort_transfers(&mut receive_for_settle);
            sort_transfers(&mut other_settlements);

            // Build combined transfer table SVG
            let transfer_table_svg = build_settle_up_transfer_svg(
                &pay_from_settle,
                &receive_for_settle,
                &other_settlements,
            );

            return Ok(SettlementView {
                balance_table_svg: balance_table_svg,
                transfer_table_svg: Some(transfer_table_svg),
            });
        }

        if settlements.is_empty() {
            Ok(SettlementView {
                balance_table_svg: balance_table_svg,
                transfer_table_svg: None,
            })
        } else {
            let settlement_transfers: Vec<Transfer> = settlements
                .iter()
                .map(|s| Transfer {
                    from: s.from,
                    to: s.to,
                    amount: s.amount,
                })
                .collect();
            let transfer_table_svg = build_transfer_table_svg(&settlement_transfers);
            Ok(SettlementView {
                balance_table_svg: balance_table_svg,
                transfer_table_svg: Some(transfer_table_svg),
            })
        }
    }
}

fn build_participant_order<'a>(
    base_members: &'a [&'a str],
    balances: &HashMap<&'a str, i64>,
) -> Vec<&'a str> {
    let mut order = Vec::with_capacity(base_members.len() + balances.len());
    let mut seen: HashSet<&'a str> = HashSet::new();

    for &member in base_members {
        if balances.contains_key(member) && seen.insert(member) {
            order.push(member);
        }
    }

    let mut extras: Vec<&'a str> = balances
        .keys()
        .copied()
        .filter(|member| seen.insert(*member))
        .collect();
    extras.sort_unstable();
    order.extend(extras);
    order
}

/// Perform settlement calculation
fn calculate_settlement<'a>(
    balances: HashMap<&'a str, i64>,
    participants: &[&'a str],
    settle_members: &[&'a str],
) -> Settlement<'a> {
    let mut working_balances = balances;
    let mut transfers = Vec::new();
    let settle_lookup: HashSet<&str> = settle_members.iter().copied().collect();

    for &member in settle_members {
        let balance = match working_balances.get(member).copied() {
            Some(b) if b != 0 => b,
            _ => continue,
        };

        let (member_receives, target_sign) = if balance > 0 {
            (true, -1) // Receive: look for negative balances
        } else {
            (false, 1) // Pay: look for positive balances
        };

        let mut remaining = balance.abs();
        let mut transferred = 0;

        // Try to settle with other settle members first
        for &other in settle_members {
            if other == member {
                continue;
            }
            if let Some(other_balance) = working_balances.get_mut(other) {
                let can_transfer = if target_sign < 0 {
                    *other_balance < 0
                } else {
                    *other_balance > 0
                };
                if !can_transfer {
                    continue;
                }

                let available = other_balance.abs();
                let amount = remaining.min(available);
                if amount > 0 {
                    *other_balance -= target_sign * amount;
                    remaining -= amount;
                    transferred += amount;
                    let (from, to) = if member_receives {
                        (other, member)
                    } else {
                        (member, other)
                    };
                    transfers.push(Transfer { from, to, amount });
                }
                if remaining == 0 {
                    break;
                }
            }
        }

        // Then try with other participants
        if remaining > 0 {
            for &other in participants {
                if other == member || settle_lookup.contains(other) {
                    continue;
                }
                if let Some(other_balance) = working_balances.get_mut(other) {
                    let can_transfer = if target_sign < 0 {
                        *other_balance < 0
                    } else {
                        *other_balance > 0
                    };
                    if !can_transfer {
                        continue;
                    }

                    let available = other_balance.abs();
                    let amount = remaining.min(available);
                    if amount > 0 {
                        *other_balance -= target_sign * amount;
                        remaining -= amount;
                        transferred += amount;
                        let (from, to) = if member_receives {
                            (other, member)
                        } else {
                            (member, other)
                        };
                        transfers.push(Transfer { from, to, amount });
                    }
                    if remaining == 0 {
                        break;
                    }
                }
            }
        }

        // Update member's balance
        if let Some(member_balance) = working_balances.get_mut(member) {
            *member_balance = if remaining == 0 {
                0
            } else {
                balance.signum() * (balance.abs() - transferred)
            };
        }
        debug_assert_eq!(remaining, 0);
    }

    Settlement {
        new_balances: working_balances,
        transfers,
    }
}

/// Wrapper for backward compatibility
fn settle_selected_members<'a>(
    balances: HashMap<&'a str, i64>,
    participants: &[&'a str],
    settle_members: &[&'a str],
) -> (HashMap<&'a str, i64>, Vec<Transfer<'a>>) {
    let result = calculate_settlement(balances, participants, settle_members);
    (result.new_balances, result.transfers)
}

fn sort_transfers<'a>(transfers: &mut Vec<Transfer<'a>>) {
    transfers.sort_unstable_by(|lhs, rhs| {
        let from_cmp = lhs.from.cmp(rhs.from);
        if from_cmp != std::cmp::Ordering::Equal {
            return from_cmp;
        }
        let to_cmp = lhs.to.cmp(rhs.to);
        if to_cmp != std::cmp::Ordering::Equal {
            return to_cmp;
        }
        lhs.amount.cmp(&rhs.amount)
    });
}

fn build_balance_table_svg(person_balances: &[PersonBalance]) -> String {
    let mut builder = SvgTableBuilder::new()
        .alignments(&[Alignment::Left, Alignment::Right])
        .headers(&[Cow::Borrowed(i18n::MEMBER), Cow::Borrowed(i18n::BALANCE)]);

    for person in person_balances {
        let sign = if person.balance >= 0 { "+" } else { "" };
        builder = builder.row([
            Cow::Borrowed(person.name),
            Cow::Owned(format!("{sign}{}", person.balance)),
        ]);
    }

    builder.build()
}

fn build_transfer_table_svg(transfers: &[Transfer]) -> String {
    let mut builder = SvgTableBuilder::new()
        .alignments(&[Alignment::Left, Alignment::Left, Alignment::Right])
        .headers(&[
            Cow::Borrowed(i18n::FROM),
            Cow::Borrowed(i18n::TO),
            Cow::Borrowed(i18n::AMOUNT),
        ]);

    for transfer in transfers {
        builder = builder.row([
            Cow::Borrowed(transfer.from),
            Cow::Borrowed(transfer.to),
            Cow::Owned(transfer.amount.to_string()),
        ]);
    }

    builder.build()
}

fn build_settle_up_transfer_svg(
    pay_from_settle: &[Transfer],
    receive_for_settle: &[Transfer],
    other_settlements: &[Transfer],
) -> String {
    let mut builder = SvgTableBuilder::new()
        .alignments(&[
            Alignment::Left,
            Alignment::Left,
            Alignment::Left,
            Alignment::Right,
        ])
        .headers(&[
            Cow::Borrowed(i18n::CATEGORY),
            Cow::Borrowed(i18n::FROM),
            Cow::Borrowed(i18n::TO),
            Cow::Borrowed(i18n::AMOUNT),
        ]);

    for transfer in pay_from_settle {
        builder = builder.row([
            Cow::Borrowed(i18n::SETTLEMENT_PAYMENT),
            Cow::Borrowed(transfer.from),
            Cow::Borrowed(transfer.to),
            Cow::Owned(transfer.amount.to_string()),
        ]);
    }

    for transfer in receive_for_settle {
        builder = builder.row([
            Cow::Borrowed(i18n::PAYMENT_TO_SETTLOR),
            Cow::Borrowed(transfer.from),
            Cow::Borrowed(transfer.to),
            Cow::Owned(transfer.amount.to_string()),
        ]);
    }

    for transfer in other_settlements {
        builder = builder.row([
            Cow::Borrowed(i18n::PENDING),
            Cow::Borrowed(transfer.from),
            Cow::Borrowed(transfer.to),
            Cow::Owned(transfer.amount.to_string()),
        ]);
    }

    builder.build()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::infrastructure::parser::WalicordProgramParser;
    use rstest::{fixture, rstest};

    #[fixture]
    fn processor() -> MessageProcessor<'static> {
        MessageProcessor::new(&WalicordProgramParser)
    }

    fn parse_program<'a>(
        processor: &MessageProcessor<'a>,
        members: &'a [&'a str],
        content: &'a str,
    ) -> Program<'a> {
        match processor.parse_program(members, content) {
            ProcessingOutcome::Success(program) => program,
            ProcessingOutcome::MissingMembersDeclaration => {
                panic!("missing members declaration")
            }
            ProcessingOutcome::UndefinedMember { name, line } => {
                panic!("undefined member {name} at line {line}")
            }
            ProcessingOutcome::SyntaxError { message } => {
                panic!("syntax error: {message}")
            }
        }
    }

    #[rstest]
    fn settle_up_resets_balances_for_selected_members(processor: MessageProcessor<'_>) {
        let members = ["A", "B"];
        let program = parse_program(&processor, &members, "A lent 100 to B\n!settleup A");

        let pre_balances = processor.calculate_balances_for_prefix(&program, 1);
        assert_eq!(pre_balances.get("A"), Some(&100));
        assert_eq!(pre_balances.get("B"), Some(&-100));

        let post_balances = processor.calculate_balances(&program);
        assert_eq!(post_balances.get("A"), Some(&0));
        assert_eq!(post_balances.get("B"), Some(&0));
    }

    #[rstest]
    fn settle_up_keeps_other_members_balances(processor: MessageProcessor<'_>) {
        let members = ["A", "B", "C"];
        let program = parse_program(
            &processor,
            &members,
            "A lent 60 to C\nB lent 100 to C\n!settleup A",
        );

        let pre_balances = processor.calculate_balances_for_prefix(&program, 2);
        assert_eq!(pre_balances.get("A"), Some(&60));
        assert_eq!(pre_balances.get("B"), Some(&100));
        assert_eq!(pre_balances.get("C"), Some(&-160));

        let post_balances = processor.calculate_balances(&program);
        assert_eq!(post_balances.get("A"), Some(&0));
        assert_eq!(post_balances.get("B"), Some(&100));
        assert_eq!(post_balances.get("C"), Some(&-100));
    }

    #[rstest]
    fn settle_up_response_groups_transfers(processor: MessageProcessor<'_>) {
        let members = ["A", "B", "C"];
        let program = parse_program(
            &processor,
            &members,
            "A lent 60 to C\nB lent 40 to C\n!settleup A",
        );

        let last_index = program.statements.len().saturating_sub(1);
        let response = processor
            .format_settlement_response_for_prefix(&program, last_index)
            .expect("response generation failed");

        assert!(response.transfer_table_svg.is_some());
    }

    #[rstest]
    fn settle_up_with_negative_balance(processor: MessageProcessor<'_>) {
        let members = ["A", "B", "C"];
        let program = parse_program(
            &processor,
            &members,
            "B lent 100 to A\nC lent 50 to A\n!settleup A",
        );

        let post_balances = processor.calculate_balances(&program);
        assert_eq!(post_balances.get("A"), Some(&0));
    }

    #[rstest]
    fn settle_up_multiple_members(processor: MessageProcessor<'_>) {
        let members = ["A", "B", "C", "D"];
        let program = parse_program(
            &processor,
            &members,
            "A lent 100 to C\nB lent 100 to C\nD lent 50 to A\n!settleup A, B",
        );

        let post_balances = processor.calculate_balances(&program);
        assert_eq!(post_balances.get("A"), Some(&0));
        assert_eq!(post_balances.get("B"), Some(&0));
    }

    #[rstest]
    fn settle_up_cross_group_transfer(processor: MessageProcessor<'_>) {
        let members = ["A", "B", "C", "D"];
        let program = parse_program(
            &processor,
            &members,
            "A lent 100 to C\nB lent 100 to D\n!settleup A, B",
        );

        let post_balances = processor.calculate_balances(&program);
        assert_eq!(post_balances.get("A"), Some(&0));
        assert_eq!(post_balances.get("B"), Some(&0));
    }

    #[rstest]
    fn settle_up_partial_within_group(processor: MessageProcessor<'_>) {
        let members = ["A", "B", "C"];
        let program = parse_program(
            &processor,
            &members,
            "A lent 100 to B\nC lent 50 to A\n!settleup A, B",
        );

        let post_balances = processor.calculate_balances(&program);
        assert_eq!(post_balances.get("A"), Some(&0));
        assert_eq!(post_balances.get("B"), Some(&0));
    }

    #[rstest]
    fn settle_up_with_exact_match(processor: MessageProcessor<'_>) {
        let members = ["A", "B", "C"];
        let program = parse_program(
            &processor,
            &members,
            "A lent 100 to B\nB lent 100 to C\n!settleup A, B, C",
        );

        let post_balances = processor.calculate_balances(&program);
        assert_eq!(post_balances.get("A"), Some(&0));
        assert_eq!(post_balances.get("B"), Some(&0));
        assert_eq!(post_balances.get("C"), Some(&0));
    }
}
