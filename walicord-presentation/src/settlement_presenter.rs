use crate::svg_table::{Alignment, SvgTableBuilder};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};
use walicord_application::{MemberDirectory, PersonBalance, SettlementResult};
use walicord_domain::{Money, Transfer, model::MemberId};
use walicord_i18n as i18n;

pub struct SettlementPresenter;

pub struct SettlementView {
    pub combined_svg: String,
}

impl SettlementPresenter {
    pub fn render_with_members(
        result: &SettlementResult,
        member_directory: &dyn MemberDirectory,
    ) -> SettlementView {
        use crate::svg_table::combine_svgs_vertically;

        let combined_svg = if let Some(settle_up) = &result.settle_up {
            let balance_table_svg = Self::build_settle_up_balance_table_svg(
                &result.balances,
                &settle_up.settle_members,
                member_directory,
                result.quantization_scale,
            );
            let has_any_transfers =
                !settle_up.immediate_transfers.is_empty() || !result.optimized_transfers.is_empty();

            if !has_any_transfers {
                balance_table_svg
            } else {
                let transfer_table_svg = Self::build_settle_up_transfer_svg(
                    &settle_up.immediate_transfers,
                    &result.optimized_transfers,
                    &result.balances,
                    member_directory,
                    result.quantization_scale,
                );

                combine_svgs_vertically(&[&balance_table_svg, &transfer_table_svg])
                    .unwrap_or(balance_table_svg)
            }
        } else {
            let balance_table_svg = Self::build_balance_table_svg(
                &result.balances,
                member_directory,
                result.quantization_scale,
            );
            if result.optimized_transfers.is_empty() {
                balance_table_svg
            } else {
                let transfer_table_svg = Self::build_transfer_table_svg(
                    &result.optimized_transfers,
                    &result.balances,
                    member_directory,
                    result.quantization_scale,
                );
                combine_svgs_vertically(&[&balance_table_svg, &transfer_table_svg])
                    .unwrap_or(balance_table_svg)
            }
        };

        SettlementView { combined_svg }
    }

    pub fn build_balance_table_svg(
        person_balances: &[PersonBalance],
        member_directory: &dyn MemberDirectory,
        quantization_scale: u32,
    ) -> String {
        let mut builder = SvgTableBuilder::new()
            .alignments(&[Alignment::Left, Alignment::Right])
            .headers(&[Cow::Borrowed(i18n::MEMBER), Cow::Borrowed(i18n::BALANCE)]);

        for person in person_balances {
            let sign = if person.balance.signum() >= 0 {
                "+"
            } else {
                ""
            };
            builder = builder.row([
                format_member_label(person.id, member_directory),
                Cow::Owned(format!(
                    "{sign}{}",
                    format_money_for_display(person.balance, quantization_scale)
                )),
            ]);
        }

        builder.build()
    }

    pub fn build_transfer_table_svg(
        transfers: &[Transfer],
        balances: &[PersonBalance],
        member_directory: &dyn MemberDirectory,
        quantization_scale: u32,
    ) -> String {
        let mut running = balances_map(balances);

        let mut builder = SvgTableBuilder::new()
            .alignments(&[
                Alignment::Left,
                Alignment::Left,
                Alignment::Right,
                Alignment::Right,
            ])
            .headers(&[
                Cow::Borrowed(i18n::FROM),
                Cow::Borrowed(i18n::TO),
                Cow::Borrowed(i18n::AMOUNT),
                Cow::Borrowed(i18n::RECEIVER_BALANCE),
            ]);

        for transfer in transfers {
            apply_transfer(&mut running, transfer);
            let to_balance = running.get(&transfer.to).copied().unwrap_or(Money::ZERO);
            builder = builder.row([
                format_member_label(transfer.from, member_directory),
                format_member_label(transfer.to, member_directory),
                Cow::Owned(
                    format_money_for_display(transfer.amount, quantization_scale).to_string(),
                ),
                Cow::Owned(format_balance_after(to_balance, quantization_scale)),
            ]);
        }

        builder.build()
    }

    pub fn build_settle_up_transfer_svg(
        confirmed_transfers: &[Transfer],
        planned_transfers: &[Transfer],
        balances: &[PersonBalance],
        member_directory: &dyn MemberDirectory,
        quantization_scale: u32,
    ) -> String {
        let running = balances_map(balances);

        let mut builder = SvgTableBuilder::new()
            .alignments(&[
                Alignment::Left,
                Alignment::Left,
                Alignment::Left,
                Alignment::Right,
                Alignment::Right,
            ])
            .headers(&[
                Cow::Borrowed(i18n::STATUS),
                Cow::Borrowed(i18n::FROM),
                Cow::Borrowed(i18n::TO),
                Cow::Borrowed(i18n::AMOUNT),
                Cow::Borrowed(i18n::RECEIVER_BALANCE),
            ]);

        let mut confirmed = confirmed_transfers.to_vec();
        let mut planned = planned_transfers.to_vec();
        sort_transfers(&mut confirmed);
        sort_transfers(&mut planned);

        // `SettlementResult::balances` already reflects confirmed settle-up transfers.
        for transfer in &confirmed {
            let to_balance = running.get(&transfer.to).copied().unwrap_or(Money::ZERO);
            builder = builder.row([
                Cow::Borrowed(i18n::SETTLED_TRANSFER),
                format_member_label(transfer.from, member_directory),
                format_member_label(transfer.to, member_directory),
                Cow::Owned(
                    format_money_for_display(transfer.amount, quantization_scale).to_string(),
                ),
                Cow::Owned(format_balance_after(to_balance, quantization_scale)),
            ]);
        }

        let mut running = running;
        for transfer in &planned {
            apply_transfer(&mut running, transfer);
            let to_balance = running.get(&transfer.to).copied().unwrap_or(Money::ZERO);
            builder = builder.row([
                Cow::Borrowed(i18n::PLANNED_TRANSFER),
                format_member_label(transfer.from, member_directory),
                format_member_label(transfer.to, member_directory),
                Cow::Owned(
                    format_money_for_display(transfer.amount, quantization_scale).to_string(),
                ),
                Cow::Owned(format_balance_after(to_balance, quantization_scale)),
            ]);
        }

        builder.build()
    }

    pub fn build_settle_up_balance_table_svg(
        person_balances: &[PersonBalance],
        settle_members: &[MemberId],
        member_directory: &dyn MemberDirectory,
        quantization_scale: u32,
    ) -> String {
        let settle_member_lookup: HashSet<_> = settle_members.iter().copied().collect();
        let mut builder = SvgTableBuilder::new()
            .alignments(&[Alignment::Left, Alignment::Right, Alignment::Left])
            .headers(&[
                Cow::Borrowed(i18n::MEMBER),
                Cow::Borrowed(i18n::BALANCE),
                Cow::Borrowed(i18n::STATUS),
            ]);

        for person in person_balances {
            let sign = if person.balance.signum() >= 0 {
                "+"
            } else {
                ""
            };
            let status = if settle_member_lookup.contains(&person.id) {
                i18n::SETTLED_MEMBER
            } else {
                i18n::UNSETTLED_MEMBER
            };
            builder = builder.row([
                format_member_label(person.id, member_directory),
                Cow::Owned(format!(
                    "{sign}{}",
                    format_money_for_display(person.balance, quantization_scale)
                )),
                Cow::Borrowed(status),
            ]);
        }

        builder.build()
    }
}

fn format_member_label<'a>(
    member_id: MemberId,
    member_directory: &'a dyn MemberDirectory,
) -> Cow<'a, str> {
    match member_directory.display_name(member_id) {
        Some(name) => Cow::Borrowed(name),
        None => Cow::Owned(format!("<@{}>", member_id.0)),
    }
}

fn sort_transfers(transfers: &mut [Transfer]) {
    transfers.sort_unstable_by(|lhs, rhs| {
        let from_cmp = lhs.from.cmp(&rhs.from);
        if from_cmp != std::cmp::Ordering::Equal {
            return from_cmp;
        }
        let to_cmp = lhs.to.cmp(&rhs.to);
        if to_cmp != std::cmp::Ordering::Equal {
            return to_cmp;
        }
        lhs.amount.cmp(&rhs.amount)
    })
}

fn format_money_for_display(amount: Money, quantization_scale: u32) -> impl std::fmt::Display {
    let value = amount.as_decimal();
    if quantization_scale == 0 {
        return if value.fract().is_zero() {
            value.trunc()
        } else {
            value.round_dp(0)
        };
    }

    if value.fract().is_zero() {
        value.trunc()
    } else {
        value.round_dp(quantization_scale).normalize()
    }
}

fn balances_map(balances: &[PersonBalance]) -> HashMap<MemberId, Money> {
    balances.iter().map(|b| (b.id, b.balance)).collect()
}

fn apply_transfer(balances: &mut HashMap<MemberId, Money>, transfer: &Transfer) {
    *balances.entry(transfer.from).or_insert(Money::ZERO) -= transfer.amount;
    *balances.entry(transfer.to).or_insert(Money::ZERO) += transfer.amount;
}

fn format_balance_after(balance: Money, quantization_scale: u32) -> String {
    let sign = if balance.signum() >= 0 { "+" } else { "" };
    let check = if balance.is_zero() { " ✓" } else { "" };
    format!(
        "{sign}{}{check}",
        format_money_for_display(balance, quantization_scale)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use std::collections::HashMap;
    use walicord_application::{MemberDirectory, PersonBalance, SettleUpContext};
    use walicord_domain::{Money, Transfer, model::MemberId};

    struct EmptyMemberDirectory;

    impl MemberDirectory for EmptyMemberDirectory {
        fn display_name(&self, _member_id: MemberId) -> Option<&str> {
            None
        }
    }

    fn sample_result() -> SettlementResult {
        SettlementResult {
            balances: vec![
                PersonBalance {
                    id: MemberId(1),
                    balance: Money::from_i64(120),
                },
                PersonBalance {
                    id: MemberId(2),
                    balance: Money::from_i64(-120),
                },
            ],
            optimized_transfers: vec![Transfer {
                from: MemberId(1),
                to: MemberId(2),
                amount: Money::from_i64(120),
            }],
            settle_up: None,
            quantization_scale: 0,
            effective_cash_members: vec![],
            pre_quantization_sum: Money::ZERO,
        }
    }

    #[rstest]
    #[case::with_display_name(Some("Alice"), "Alice", Some("&lt;@1&gt;"), "Alice")]
    #[case::without_display_name(None, "&lt;@1&gt;", None, "&lt;@2&gt;")]
    fn render_cases(
        #[case] display_name: Option<&str>,
        #[case] expected_balance_contains: &str,
        #[case] forbidden_balance: Option<&str>,
        #[case] expected_transfer_contains: &str,
    ) {
        let mut directory = HashMap::new();
        if let Some(name) = display_name {
            directory.insert(MemberId(1), name.into());
        }

        let view = SettlementPresenter::render_with_members(&sample_result(), &directory);

        assert!(view.combined_svg.contains(expected_balance_contains));
        if let Some(forbidden) = forbidden_balance {
            assert!(!view.combined_svg.contains(forbidden));
        }
        assert!(view.combined_svg.contains(expected_transfer_contains));
    }

    #[test]
    fn transfer_table_shows_receiver_balance_column() {
        let directory = HashMap::from([(MemberId(1), "Alice".into()), (MemberId(2), "Bob".into())]);
        let view = SettlementPresenter::render_with_members(&sample_result(), &directory);
        assert!(view.combined_svg.contains(i18n::RECEIVER_BALANCE));
        // After transfer of 120 from Alice(+120) to Bob(-120), Bob's balance becomes 0
        assert!(view.combined_svg.contains("+0 ✓"));
    }

    #[test]
    fn formats_decimal_balance_with_quantization_scale() {
        let result = SettlementResult {
            balances: vec![PersonBalance {
                id: MemberId(1),
                balance: Money::from_decimal("1.123456789".parse().expect("decimal")),
            }],
            optimized_transfers: vec![],
            settle_up: None,
            quantization_scale: 6,
            effective_cash_members: vec![],
            pre_quantization_sum: Money::ZERO,
        };

        let view = SettlementPresenter::render_with_members(&result, &EmptyMemberDirectory);
        assert!(view.combined_svg.contains("+1.123457"));
    }

    #[test]
    fn scale_zero_uses_rounding_for_non_integral_fallback() {
        let result = SettlementResult {
            balances: vec![PersonBalance {
                id: MemberId(1),
                balance: Money::from_decimal("1.6".parse().expect("decimal")),
            }],
            optimized_transfers: vec![],
            settle_up: None,
            quantization_scale: 0,
            effective_cash_members: vec![],
            pre_quantization_sum: Money::ZERO,
        };

        let view = SettlementPresenter::render_with_members(&result, &EmptyMemberDirectory);
        assert!(view.combined_svg.contains("+2"));
    }

    #[test]
    fn settle_up_view_shows_status_and_transfer_labels() {
        let result = SettlementResult {
            balances: vec![
                PersonBalance {
                    id: MemberId(1),
                    balance: Money::ZERO,
                },
                PersonBalance {
                    id: MemberId(2),
                    balance: Money::ZERO,
                },
            ],
            optimized_transfers: vec![Transfer {
                from: MemberId(2),
                to: MemberId(1),
                amount: Money::from_i64(50),
            }],
            settle_up: Some(SettleUpContext {
                settle_members: vec![MemberId(1)],
                immediate_transfers: vec![Transfer {
                    from: MemberId(1),
                    to: MemberId(2),
                    amount: Money::from_i64(100),
                }],
            }),
            quantization_scale: 0,
            effective_cash_members: vec![],
            pre_quantization_sum: Money::ZERO,
        };

        let view = SettlementPresenter::render_with_members(&result, &EmptyMemberDirectory);
        assert!(view.combined_svg.contains(i18n::STATUS));
        assert!(view.combined_svg.contains(i18n::SETTLED_MEMBER));
        assert!(view.combined_svg.contains(i18n::UNSETTLED_MEMBER));
        assert!(view.combined_svg.contains(i18n::SETTLED_TRANSFER));
        assert!(view.combined_svg.contains(i18n::PLANNED_TRANSFER));
        assert!(view.combined_svg.contains(i18n::RECEIVER_BALANCE));
    }

    #[test]
    fn settle_up_transfer_table_uses_post_settlement_balance_for_confirmed_rows() {
        let directory = HashMap::from([(MemberId(1), "Alice".into()), (MemberId(2), "Bob".into())]);

        let svg = SettlementPresenter::build_settle_up_transfer_svg(
            &[Transfer {
                from: MemberId(1),
                to: MemberId(2),
                amount: Money::from_i64(100),
            }],
            &[],
            &[
                PersonBalance {
                    id: MemberId(1),
                    balance: Money::ZERO,
                },
                PersonBalance {
                    id: MemberId(2),
                    balance: Money::ZERO,
                },
            ],
            &directory,
            0,
        );

        assert!(svg.contains("Bob"));
        assert!(svg.contains("+0 ✓"));
        assert!(!svg.contains("+100"));
    }
}
