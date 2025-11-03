use crate::domain::{Program, ProgramParseError, ProgramParser, model::Statement};
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

    pub fn format_variables_response(&self, program: Program) -> String {
        let mut reply = String::with_capacity(512);
        let _ = writeln!(&mut reply, "`MEMBERS` := {}", program.members.join(", "));
        reply.push('\n');

        let declarations = program.statements.iter().filter_map(|s| match s {
            Statement::Declaration(d) => Some(d),
            _ => None,
        });

        for decl in declarations {
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

    pub fn calculate_balances<'b>(&self, program: &'b Program) -> HashMap<&'b str, i64> {
        let mut balances: HashMap<&str, i64> = program
            .members
            .iter()
            .copied()
            .map(|member| (member, 0))
            .collect();

        let mut set_env = SetEnvironment::new(program.members);

        for stmt in &program.statements {
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
            }
        }

        balances
    }

    pub fn format_settlement_response(&self, program: Program) -> Result<String, String> {
        let balances = self.calculate_balances(&program);

        // Convert to PersonBalance format
        let mut person_balances: Vec<PersonBalance> = balances
            .iter()
            .map(|(name, balance)| PersonBalance {
                name,
                balance: *balance,
            })
            .collect();
        person_balances.sort_by_key(|p| p.name);

        // Calculate settlement
        let settlements = minimize_transactions(&person_balances, 1.0, 0.001)
            .map_err(|e| format!("清算の計算に失敗しました: {e}"))?;

        // Format response
        let mut reply = String::with_capacity(1024);

        reply.push_str("## 💰 割り勘計算結果\n\n");

        // Balance table
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

        // Settlement table
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
