use crate::domain::{Program, ProgramParseError, ProgramParser, model::Statement};
use either::Either;
use std::{collections::HashMap, fmt::Write as _, iter};
use walicord_calc::{PersonBalance, minimize_transactions};

#[derive(Clone, Copy)]
pub struct MessageProcessor<'a> {
    parser: &'a dyn ProgramParser,
}

pub enum ProcessingOutcome<'a> {
    Success(Program<'a>),
    MissingMembersDeclaration,
    UndefinedMember { name: String, line: usize },
    SyntaxError { message: String },
}

impl<'a> MessageProcessor<'a> {
    pub fn new(parser: &'a dyn ProgramParser) -> Self {
        Self { parser }
    }

    pub fn parse_program<'b>(
        &self,
        members: &'b [&'b str],
        content: &'b str,
    ) -> ProcessingOutcome<'b> {
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
        reply.push_str("**MEMBERS**\n");
        reply.push_str("```\n");
        reply.push_str(&program.members.join(", "));
        reply.push_str("\n```\n");

        let declarations: Vec<_> = program
            .statements
            .iter()
            .filter_map(|s| match s {
                Statement::Declaration(d) => Some(d),
                _ => None,
            })
            .collect();

        if !declarations.is_empty() {
            reply.push_str("\n**GROUPS**\n");
            for decl in declarations {
                let listing = if decl.members.is_empty() {
                    "[empty]".to_string()
                } else {
                    decl.members.join(", ")
                };
                let _ = writeln!(&mut reply, "- `{}` := {listing}\n", decl.name);
            }
        }

        reply
    }

    pub fn calculate_balances<'b>(&self, program: &'b Program) -> HashMap<&'b str, i64> {
        let mut balances: HashMap<&str, i64> = HashMap::new();

        // Initialize all members with balance 0
        for member in program.members {
            balances.insert(member, 0);
        }

        // Build group definitions
        let mut groups: HashMap<&str, &[&str]> = HashMap::new();
        for stmt in &program.statements {
            if let Statement::Declaration(decl) = stmt {
                groups.insert(decl.name, &decl.members);
                // Initialize group members with balance 0 if not already present
                for member in &decl.members {
                    balances.entry(member).or_insert(0);
                }
            }
        }

        // Process payments
        for stmt in &program.statements {
            if let Statement::Payment(payment) = stmt {
                fn expand_members<'a>(
                    member: &'a str,
                    groups: &'a HashMap<&str, &[&str]>,
                    all_members: &'a [&'a str],
                ) -> impl ExactSizeIterator<Item = &'a str> {
                    if member == "MEMBERS" {
                        Either::Right(all_members.iter().copied())
                    } else {
                        groups.get(member).map_or_else(
                            || Either::Left(iter::once(member)),
                            |v| Either::Right(v.iter().copied()),
                        )
                    }
                }

                let payer_members = expand_members(payment.payer, &groups, program.members);
                let payee_members = expand_members(payment.payee, &groups, program.members);

                let total_payers = payer_members.len() as u64;
                let total_payees = payee_members.len() as u64;

                if total_payers > 0 {
                    let amount_per_payer = payment.amount / total_payers;
                    let remainder_payer = payment.amount % total_payers;

                    for (i, payer_member) in payer_members.enumerate() {
                        let mut paid_amount = amount_per_payer;
                        if (i as u64) < remainder_payer {
                            paid_amount += 1;
                        }
                        *balances.get_mut(payer_member).unwrap() += paid_amount as i64;
                    }
                }

                if total_payees > 0 {
                    let amount_per_payee = payment.amount / total_payees;
                    let remainder_payee = payment.amount % total_payees;

                    for (i, payee_member) in payee_members.enumerate() {
                        let mut received_amount = amount_per_payee;
                        if (i as u64) < remainder_payee {
                            received_amount += 1;
                        }
                        *balances.get_mut(payee_member).unwrap() -= received_amount as i64;
                    }
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
        let settlements = minimize_transactions(&person_balances, 1.0, 0.0)
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
