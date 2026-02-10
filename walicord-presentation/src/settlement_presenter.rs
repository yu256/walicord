use crate::svg_table::{Alignment, SvgTableBuilder};
use std::{borrow::Cow, collections::HashSet};
use walicord_application::{PersonBalance, SettlementResult};
use walicord_domain::Transfer;
use walicord_i18n as i18n;

pub struct SettlementPresenter;

pub struct SettlementView {
    pub balance_table_svg: String,
    pub transfer_table_svg: Option<String>,
}

impl SettlementPresenter {
    pub fn render(result: &SettlementResult) -> SettlementView {
        let balance_table_svg = Self::build_balance_table_svg(&result.balances);

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
            let transfer_table_svg = Self::build_transfer_table_svg(&result.optimized_transfers);
            SettlementView {
                balance_table_svg,
                transfer_table_svg: Some(transfer_table_svg),
            }
        }
    }

    pub fn build_balance_table_svg(person_balances: &[PersonBalance]) -> String {
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
                Cow::Owned(format!("<@{}>", person.id.0)),
                Cow::Owned(format!("{sign}{}", person.balance.amount())),
            ]);
        }

        builder.build()
    }

    pub fn build_transfer_table_svg(transfers: &[Transfer]) -> String {
        let mut builder = SvgTableBuilder::new()
            .alignments(&[Alignment::Left, Alignment::Left, Alignment::Right])
            .headers(&[
                Cow::Borrowed(i18n::FROM),
                Cow::Borrowed(i18n::TO),
                Cow::Borrowed(i18n::AMOUNT),
            ]);

        for transfer in transfers {
            builder = builder.row([
                Cow::Owned(format!("<@{}>", transfer.from.0)),
                Cow::Owned(format!("<@{}>", transfer.to.0)),
                Cow::Owned(transfer.amount.to_string()),
            ]);
        }

        builder.build()
    }

    pub fn build_settle_up_transfer_svg(
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
                Cow::Owned(format!("<@{}>", transfer.from.0)),
                Cow::Owned(format!("<@{}>", transfer.to.0)),
                Cow::Owned(transfer.amount.to_string()),
            ]);
        }

        for transfer in receive_for_settle {
            builder = builder.row([
                Cow::Borrowed(i18n::PAYMENT_TO_SETTLOR),
                Cow::Owned(format!("<@{}>", transfer.from.0)),
                Cow::Owned(format!("<@{}>", transfer.to.0)),
                Cow::Owned(transfer.amount.to_string()),
            ]);
        }

        for transfer in other_settlements {
            builder = builder.row([
                Cow::Borrowed(i18n::PENDING),
                Cow::Owned(format!("<@{}>", transfer.from.0)),
                Cow::Owned(format!("<@{}>", transfer.to.0)),
                Cow::Owned(transfer.amount.to_string()),
            ]);
        }

        builder.build()
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
