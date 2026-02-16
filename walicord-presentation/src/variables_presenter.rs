use std::fmt::Write as _;

use walicord_application::{Script, ScriptStatement, ScriptStatementWithLine};
use walicord_domain::{MemberSetResolver, Statement, model::MemberId};

pub struct VariablesPresenter;

impl VariablesPresenter {
    pub fn render(program: &Script<'_>) -> String {
        Self::render_with_members(program, &[])
    }

    pub fn render_with_members(program: &Script<'_>, member_ids: &[MemberId]) -> String {
        Self::render_from_slice(program.statements(), member_ids)
    }

    pub fn render_for_prefix(program: &Script<'_>, prefix_len: usize) -> String {
        Self::render_for_prefix_with_members(program, prefix_len, &[])
    }

    pub fn render_for_prefix_with_members(
        program: &Script<'_>,
        prefix_len: usize,
        member_ids: &[MemberId],
    ) -> String {
        let statements = program.statements();
        let prefix_len = prefix_len.min(statements.len());
        let statements_slice = if prefix_len < statements.len()
            && let ScriptStatement::Command(_) = statements[prefix_len].statement
        {
            &statements[..=prefix_len]
        } else {
            &statements[..prefix_len]
        };

        Self::render_from_slice(statements_slice, member_ids)
    }

    fn render_from_slice(
        statements: &[ScriptStatementWithLine<'_>],
        member_ids: &[MemberId],
    ) -> String {
        let mut reply = String::with_capacity(512);

        let mut resolver = MemberSetResolver::new_with_members(member_ids.iter().copied());

        for stmt in statements {
            let ScriptStatement::Domain(Statement::Declaration(decl)) = &stmt.statement else {
                continue;
            };

            match resolver.evaluate_members(&decl.expression) {
                Some(members_vec) => {
                    let tmp;
                    let listing = if members_vec.is_empty() {
                        "[empty]"
                    } else {
                        tmp = members_vec
                            .members()
                            .iter()
                            .map(|m| format!("<@{}>", m.0))
                            .collect::<Vec<_>>()
                            .join(", ");
                        &tmp
                    };
                    let _ = writeln!(&mut reply, "- `{}` := {listing}", decl.name);
                    resolver.register_group_members(decl.name, members_vec.iter());
                }
                None => {
                    let _ = writeln!(&mut reply, "- `{}` := [invalid]", decl.name);
                }
            }
        }

        reply
    }
}
