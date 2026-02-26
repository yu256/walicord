use crate::{
    Command as AppCommand, ProgramParseError, Script, ScriptStatement, ScriptStatementWithLine,
};
use std::collections::{BTreeMap, BTreeSet};
use walicord_domain::{
    Statement as DomainStatement, WeightOverrideTarget,
    model::{MemberSetExpr, RoleId},
};

pub type RoleVisibilityDiagnostics = BTreeMap<RoleId, RoleVisibilityDiagnostic>;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct RoleVisibilityDiagnostic {
    pub total_members: usize,
    pub visible_members: usize,
    pub excluded_members: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RoleVisibilityWarning {
    pub role_id: RoleId,
    pub visible_members: usize,
    pub excluded_members: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FilteredEmptyRoleParseError {
    pub role_id: RoleId,
    pub line: usize,
    pub excluded_members: usize,
}

pub fn warnings_for_program_prefix(
    script: &Script<'_>,
    stmt_index_inclusive: usize,
    diagnostics: &RoleVisibilityDiagnostics,
) -> Vec<RoleVisibilityWarning> {
    let mut referenced_role_ids = BTreeSet::new();

    for stmt in script
        .statements()
        .iter()
        .take(stmt_index_inclusive.saturating_add(1))
    {
        collect_referenced_role_ids(stmt, &mut referenced_role_ids);
    }

    referenced_role_ids
        .into_iter()
        .filter_map(|role_id| {
            let diagnostic = diagnostics.get(&role_id)?;
            (diagnostic.excluded_members > 0 && diagnostic.visible_members > 0).then_some(
                RoleVisibilityWarning {
                    role_id,
                    visible_members: diagnostic.visible_members,
                    excluded_members: diagnostic.excluded_members,
                },
            )
        })
        .collect()
}

pub fn filtered_empty_role_parse_error(
    error: &ProgramParseError<'_>,
    diagnostics: &RoleVisibilityDiagnostics,
) -> Option<FilteredEmptyRoleParseError> {
    let ProgramParseError::UndefinedRole { id, line } = error else {
        return None;
    };

    let diagnostic = diagnostics.get(&RoleId(*id))?;
    if diagnostic.excluded_members == 0 || diagnostic.visible_members != 0 {
        return None;
    }

    Some(FilteredEmptyRoleParseError {
        role_id: RoleId(*id),
        line: *line,
        excluded_members: diagnostic.excluded_members,
    })
}

fn collect_referenced_role_ids(stmt: &ScriptStatementWithLine<'_>, out: &mut BTreeSet<RoleId>) {
    match &stmt.statement {
        ScriptStatement::Domain(domain_stmt) => collect_domain_statement_roles(domain_stmt, out),
        ScriptStatement::Command(command) => collect_command_roles(command, out),
    }
}

fn collect_command_roles(command: &AppCommand<'_>, out: &mut BTreeSet<RoleId>) {
    match command {
        AppCommand::Variables | AppCommand::Review => {}
        AppCommand::MemberAddCash { members } => collect_expr_roles(members, out),
        AppCommand::SettleUp {
            members,
            cash_members,
        } => {
            collect_expr_roles(members, out);
            if let Some(cash_members) = cash_members {
                collect_expr_roles(cash_members, out);
            }
        }
    }
}

fn collect_domain_statement_roles(stmt: &DomainStatement<'_>, out: &mut BTreeSet<RoleId>) {
    match stmt {
        DomainStatement::Declaration(decl) => collect_expr_roles(&decl.expression, out),
        DomainStatement::Payment(payment) => {
            collect_expr_roles(&payment.payer, out);
            collect_expr_roles(&payment.payee, out);
            if let Some(overrides) = payment.allocation.weight_overrides() {
                for entry in overrides.entries() {
                    if let WeightOverrideTarget::Role(role_id) = &entry.target {
                        out.insert(*role_id);
                    }
                }
            }
        }
    }
}

fn collect_expr_roles(expr: &MemberSetExpr<'_>, out: &mut BTreeSet<RoleId>) {
    out.extend(expr.referenced_role_ids());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Command, ScriptStatementWithLine};
    use walicord_domain::{
        Declaration, Payment, Statement,
        model::{MemberId, MemberSetExpr, MemberSetOp, RoleMembers},
    };

    fn role_expr(role_id: u64) -> MemberSetExpr<'static> {
        MemberSetExpr::new([MemberSetOp::PushRole(RoleId(role_id))])
    }

    fn make_script(statements: Vec<ScriptStatementWithLine<'static>>) -> Script<'static> {
        let members: &'static [MemberId] = &[];
        let roles = Box::leak(Box::new(RoleMembers::default()));
        Script::new(members, roles, statements)
    }

    #[test]
    fn warnings_for_program_prefix_deduplicates_roles_and_skips_filtered_empty_roles() {
        let script = make_script(vec![
            ScriptStatementWithLine {
                line: 1,
                statement: ScriptStatement::Domain(Statement::Declaration(Declaration {
                    name: "team",
                    expression: role_expr(10),
                })),
            },
            ScriptStatementWithLine {
                line: 2,
                statement: ScriptStatement::Domain(Statement::Payment(Payment::even(
                    walicord_domain::Money::from_i64(100),
                    role_expr(10),
                    role_expr(20),
                ))),
            },
            ScriptStatementWithLine {
                line: 3,
                statement: ScriptStatement::Command(Command::Review),
            },
        ]);
        let diagnostics = RoleVisibilityDiagnostics::from([
            (
                RoleId(10),
                RoleVisibilityDiagnostic {
                    total_members: 3,
                    visible_members: 2,
                    excluded_members: 1,
                },
            ),
            (
                RoleId(20),
                RoleVisibilityDiagnostic {
                    total_members: 2,
                    visible_members: 0,
                    excluded_members: 2,
                },
            ),
        ]);

        let warnings = warnings_for_program_prefix(&script, 2, &diagnostics);

        assert_eq!(
            warnings,
            vec![RoleVisibilityWarning {
                role_id: RoleId(10),
                visible_members: 2,
                excluded_members: 1,
            }]
        );
    }

    #[test]
    fn filtered_empty_role_parse_error_detects_visibility_filtered_role() {
        let diagnostics = RoleVisibilityDiagnostics::from([(
            RoleId(10),
            RoleVisibilityDiagnostic {
                total_members: 1,
                visible_members: 0,
                excluded_members: 1,
            },
        )]);
        let error = ProgramParseError::UndefinedRole { id: 10, line: 4 };

        let actual = filtered_empty_role_parse_error(&error, &diagnostics);

        assert_eq!(
            actual,
            Some(FilteredEmptyRoleParseError {
                role_id: RoleId(10),
                line: 4,
                excluded_members: 1,
            })
        );
    }
}
