use std::fmt::Write as _;

use walicord_application::{Script, ScriptStatement, ScriptStatementWithLine};
use walicord_domain::{MemberSetResolver, Statement};

pub struct VariablesPresenter;

impl VariablesPresenter {
    pub fn render(program: &Script<'_>) -> String {
        Self::render_from_slice(program.members(), program.statements())
    }

    pub fn render_for_prefix(program: &Script<'_>, prefix_len: usize) -> String {
        let statements = program.statements();
        let prefix_len = prefix_len.min(statements.len());
        let statements_slice = if prefix_len < statements.len()
            && matches!(
                statements[prefix_len].statement,
                ScriptStatement::Command(_)
            ) {
            &statements[..=prefix_len]
        } else {
            &statements[..prefix_len]
        };

        Self::render_from_slice(program.members(), statements_slice)
    }

    fn render_from_slice(members: &[&str], statements: &[ScriptStatementWithLine<'_>]) -> String {
        let mut reply = String::with_capacity(512);
        let _ = writeln!(&mut reply, "`MEMBERS` := {}", members.join(", "));
        reply.push('\n');

        let mut resolver = MemberSetResolver::new(members);

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
                        tmp = members_vec.members().join(", ");
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
