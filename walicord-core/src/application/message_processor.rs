use crate::domain::{
    Program, ProgramParseError, ProgramParser,
    model::{Command, Statement},
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Write as _,
};
use walicord_calc::{PersonBalance, minimize_transactions};
use walicord_parser::SetExpr;

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
        self.format_variables_response_from_slice(
            program.members,
            &program.statements[..prefix_len],
        )
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
                        if let Some(settle_members) = set_env.resolve_members(expr)
                            && !settle_members.is_empty()
                        {
                            let participants = build_participant_order(members, &balances);
                            settle_selected_members(&mut balances, &participants, &settle_members);
                        }
                    }
                },
            }
        }

        balances
    }

    pub fn format_settlement_response(&self, program: &Program) -> Result<String, String> {
        self.format_settlement_response_from_slice(program.members, &program.statements)
    }

    pub fn format_settlement_response_for_prefix(
        &self,
        program: &Program,
        prefix_len: usize,
    ) -> Result<String, String> {
        let prefix_len = prefix_len.min(program.statements.len());
        self.format_settlement_response_from_slice(
            program.members,
            &program.statements[..prefix_len],
        )
    }

    fn format_settlement_response_from_slice(
        &self,
        members: &[&str],
        statements: &[Statement<'_>],
    ) -> Result<String, String> {
        let balances = self.calculate_balances_from_slice(members, statements);

        let mut person_balances: Vec<PersonBalance> = balances
            .iter()
            .map(|(name, balance)| PersonBalance {
                name,
                balance: *balance,
            })
            .collect();
        person_balances.sort_by_key(|p| p.name);

        let settlements = minimize_transactions(&person_balances, 1.0, 0.001)
            .map_err(|e| format!("清算の計算に失敗しました: {e}"))?;

        let mut reply = String::with_capacity(1024);

        reply.push_str("## 💰 割り勘計算結果\n\n");

        reply.push_str("### 各メンバーの収支\n");
        reply.push_str("```\n");
        let _ = writeln!(&mut reply, "{:<15} | {:>10}", "メンバー", "収支");
        let _ = writeln!(&mut reply, "{:-<15}-+-{:-<10}", "", "");
        for person in &person_balances {
            let sign = if person.balance >= 0 { "+" } else { "" };
            let _ = writeln!(
                &mut reply,
                "{:<15} | {sign:>9}{}",
                person.name, person.balance
            );
        }
        reply.push_str("```\n\n");

        if settlements.is_empty() {
            reply.push_str("### ✅ 精算済み\n全員の収支がゼロです。\n");
        } else {
            reply.push_str("### 精算方法\n");
            reply.push_str("```\n");
            let _ = writeln!(
                &mut reply,
                "{:<15} -> {:<15} | {:>10}",
                "支払人", "受取人", "金額"
            );
            let _ = writeln!(&mut reply, "{:-<15}----{:-<15}-+-{:-<10}", "", "", "");
            for settlement in &settlements {
                let _ = writeln!(
                    &mut reply,
                    "{:<15} -> {:<15} | {:>10}",
                    settlement.from, settlement.to, settlement.amount
                );
            }
            reply.push_str("```\n");
        }

        Ok(reply)
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

fn settle_selected_members<'a>(
    balances: &mut HashMap<&'a str, i64>,
    participants: &[&'a str],
    settle_members: &[&'a str],
) {
    for &member in settle_members {
        let balance = match balances.get(member).copied() {
            Some(b) => b,
            None => continue,
        };

        if balance == 0 {
            continue;
        }

        if balance > 0 {
            let mut remaining = balance;
            let mut transferred = 0;
            for &other in participants {
                if other == member {
                    continue;
                }
                if let Some(other_balance) = balances.get_mut(other)
                    && *other_balance < 0
                {
                    let transfer = remaining.min(-*other_balance);
                    *other_balance += transfer;
                    remaining -= transfer;
                    transferred += transfer;
                    if remaining == 0 {
                        break;
                    }
                }
            }
            if let Some(member_balance) = balances.get_mut(member) {
                if remaining == 0 {
                    *member_balance = 0;
                } else {
                    *member_balance -= transferred;
                }
            }
            debug_assert_eq!(remaining, 0);
        } else {
            let mut remaining = -balance;
            let mut transferred = 0;
            for &other in participants {
                if other == member {
                    continue;
                }
                if let Some(other_balance) = balances.get_mut(other)
                    && *other_balance > 0
                {
                    let transfer = remaining.min(*other_balance);
                    *other_balance -= transfer;
                    remaining -= transfer;
                    transferred += transfer;
                    if remaining == 0 {
                        break;
                    }
                }
            }
            if let Some(member_balance) = balances.get_mut(member) {
                if remaining == 0 {
                    *member_balance = 0;
                } else {
                    *member_balance += transferred;
                }
            }
            debug_assert_eq!(remaining, 0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::infrastructure::parser::WalicordProgramParser;

    #[test]
    fn settle_up_resets_balances_for_selected_members() {
        let parser = WalicordProgramParser;
        let processor = MessageProcessor::new(&parser);
        let members = ["A", "B"];
        let content = "A lent 100 to B\n!確定 A";

        let program = match processor.parse_program(&members, content) {
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
        };

        let pre_balances = processor.calculate_balances_for_prefix(&program, 1);
        assert_eq!(pre_balances.get("A"), Some(&100));
        assert_eq!(pre_balances.get("B"), Some(&-100));

        let post_balances = processor.calculate_balances(&program);
        assert_eq!(post_balances.get("A"), Some(&0));
        assert_eq!(post_balances.get("B"), Some(&0));
    }

    #[test]
    fn settle_up_keeps_other_members_balances() {
        let parser = WalicordProgramParser;
        let processor = MessageProcessor::new(&parser);
        let members = ["A", "B", "C"];
        let content = "A lent 60 to C\nB lent 100 to C\n!確定 A";

        let program = match processor.parse_program(&members, content) {
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
        };

        let pre_balances = processor.calculate_balances_for_prefix(&program, 2);
        assert_eq!(pre_balances.get("A"), Some(&60));
        assert_eq!(pre_balances.get("B"), Some(&100));
        assert_eq!(pre_balances.get("C"), Some(&-160));

        let post_balances = processor.calculate_balances(&program);
        assert_eq!(post_balances.get("A"), Some(&0));
        assert_eq!(post_balances.get("B"), Some(&100));
        assert_eq!(post_balances.get("C"), Some(&-100));
    }
}
