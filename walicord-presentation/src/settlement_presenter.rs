use crate::svg_table::{Alignment, SvgTableBuilder};
use std::{borrow::Cow, collections::HashSet};
use walicord_application::{MemberDirectory, PersonBalance, SettlementResult};
use walicord_domain::{Transfer, model::MemberId};
use walicord_i18n as i18n;

pub struct SettlementPresenter;

pub struct SettlementView {
    pub balance_table_svg: String,
    pub transfer_table_svg: Option<String>,
}

impl SettlementPresenter {
    pub fn render(result: &SettlementResult) -> SettlementView {
        let empty_directory = EmptyMemberDirectory;
        Self::render_with_members(result, &empty_directory)
    }

    pub fn render_with_members(
        result: &SettlementResult,
        member_directory: &dyn MemberDirectory,
    ) -> SettlementView {
        let balance_table_svg = Self::build_balance_table_svg(&result.balances, member_directory);

        if let Some(settle_up) = &result.settle_up {
            let has_any_transfers =
                !settle_up.immediate_transfers.is_empty() || !result.optimized_transfers.is_empty();

            if !has_any_transfers {
                return SettlementView {
                    balance_table_svg,
                    transfer_table_svg: None,
                };
            }

            let settle_member_lookup: HashSet<_> =
                settle_up.settle_members.iter().copied().collect();

            let mut pay_from_settle: Vec<Transfer> = Vec::new();
            let mut receive_for_settle: Vec<Transfer> = Vec::new();
            let mut other_settlements: Vec<Transfer> = Vec::new();

            for transfer in settle_up
                .immediate_transfers
                .iter()
                .copied()
                .chain(result.optimized_transfers.iter().copied())
            {
                if settle_member_lookup.contains(&transfer.from) {
                    pay_from_settle.push(transfer);
                } else if settle_member_lookup.contains(&transfer.to) {
                    receive_for_settle.push(transfer);
                } else {
                    other_settlements.push(transfer);
                }
            }

            sort_transfers(&mut pay_from_settle);
            sort_transfers(&mut receive_for_settle);
            sort_transfers(&mut other_settlements);

            let transfer_table_svg = Self::build_settle_up_transfer_svg(
                &pay_from_settle,
                &receive_for_settle,
                &other_settlements,
                member_directory,
            );

            return SettlementView {
                balance_table_svg,
                transfer_table_svg: Some(transfer_table_svg),
            };
        }

        if result.optimized_transfers.is_empty() {
            SettlementView {
                balance_table_svg,
                transfer_table_svg: None,
            }
        } else {
            let transfer_table_svg =
                Self::build_transfer_table_svg(&result.optimized_transfers, member_directory);
            SettlementView {
                balance_table_svg,
                transfer_table_svg: Some(transfer_table_svg),
            }
        }
    }

    pub fn build_balance_table_svg(
        person_balances: &[PersonBalance],
        member_directory: &dyn MemberDirectory,
    ) -> String {
        let mut builder = SvgTableBuilder::new()
            .alignments(&[Alignment::Left, Alignment::Right])
            .headers(&[Cow::Borrowed(i18n::MEMBER), Cow::Borrowed(i18n::BALANCE)]);

        for person in person_balances {
            let sign = if person.balance.amount() >= 0 {
                "+"
            } else {
                ""
            };
            builder = builder.row([
                format_member_label(person.id, member_directory),
                Cow::Owned(format!("{sign}{}", person.balance.amount())),
            ]);
        }

        builder.build()
    }

    pub fn build_transfer_table_svg(
        transfers: &[Transfer],
        member_directory: &dyn MemberDirectory,
    ) -> String {
        let mut builder = SvgTableBuilder::new()
            .alignments(&[Alignment::Left, Alignment::Left, Alignment::Right])
            .headers(&[
                Cow::Borrowed(i18n::FROM),
                Cow::Borrowed(i18n::TO),
                Cow::Borrowed(i18n::AMOUNT),
            ]);

        for transfer in transfers {
            builder = builder.row([
                format_member_label(transfer.from, member_directory),
                format_member_label(transfer.to, member_directory),
                Cow::Owned(transfer.amount.to_string()),
            ]);
        }

        builder.build()
    }

    pub fn build_settle_up_transfer_svg(
        pay_from_settle: &[Transfer],
        receive_for_settle: &[Transfer],
        other_settlements: &[Transfer],
        member_directory: &dyn MemberDirectory,
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
                format_member_label(transfer.from, member_directory),
                format_member_label(transfer.to, member_directory),
                Cow::Owned(transfer.amount.to_string()),
            ]);
        }

        for transfer in receive_for_settle {
            builder = builder.row([
                Cow::Borrowed(i18n::PAYMENT_TO_SETTLOR),
                format_member_label(transfer.from, member_directory),
                format_member_label(transfer.to, member_directory),
                Cow::Owned(transfer.amount.to_string()),
            ]);
        }

        for transfer in other_settlements {
            builder = builder.row([
                Cow::Borrowed(i18n::PENDING),
                format_member_label(transfer.from, member_directory),
                format_member_label(transfer.to, member_directory),
                Cow::Owned(transfer.amount.to_string()),
            ]);
        }

        builder.build()
    }
}

struct EmptyMemberDirectory;

impl MemberDirectory for EmptyMemberDirectory {
    fn display_name(&self, _member_id: MemberId) -> Option<&str> {
        None
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use walicord_application::PersonBalance;
    use walicord_domain::{Money, Transfer, model::MemberId};

    fn sample_result() -> SettlementResult {
        SettlementResult {
            balances: vec![PersonBalance {
                id: MemberId(1),
                balance: Money::from_i64(120),
            }],
            optimized_transfers: vec![Transfer {
                from: MemberId(1),
                to: MemberId(2),
                amount: Money::from_i64(50),
            }],
            settle_up: None,
        }
    }

    #[test]
    fn render_uses_display_name_when_available() {
        let mut directory = HashMap::new();
        directory.insert(MemberId(1), "Alice".to_string());

        let view = SettlementPresenter::render_with_members(&sample_result(), &directory);

        assert!(view.balance_table_svg.contains("Alice"));
        assert!(!view.balance_table_svg.contains("<@1>"));
        assert!(
            view.transfer_table_svg
                .as_ref()
                .expect("transfer table")
                .contains("Alice")
        );
    }

    #[test]
    fn render_falls_back_to_mentions_when_missing() {
        let directory: HashMap<MemberId, String> = HashMap::new();

        let view = SettlementPresenter::render_with_members(&sample_result(), &directory);

        assert!(view.balance_table_svg.contains("&lt;@1&gt;"));
        assert!(
            view.transfer_table_svg
                .as_ref()
                .expect("transfer table")
                .contains("&lt;@2&gt;")
        );
    }
}
