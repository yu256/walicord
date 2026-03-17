use insta::assert_snapshot;
use walicord_presentation::{SettlementPresenter, test_fixtures::*};

fn render_snapshot_body(
    result: &walicord_application::SettlementResult,
    member_directory: &dyn walicord_application::MemberDirectory,
) -> String {
    let view = SettlementPresenter::render_with_members(result, member_directory);
    format!("### combined_svg\n{}\n", view.combined_svg)
}

#[test]
fn snapshot_integration_review_view_complex() {
    assert_snapshot!(
        "integration_review_view_complex",
        render_snapshot_body(&complex_review_result(), &member_directory())
    );
}

#[test]
fn snapshot_integration_settleup_view_complex() {
    assert_snapshot!(
        "integration_settleup_view_complex",
        render_snapshot_body(&complex_settleup_result(), &member_directory())
    );
}
