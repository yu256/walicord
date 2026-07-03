use std::borrow::Cow;

use crate::svg_table::{Alignment, RenderedSvg, SvgTableBuilder};
use walicord_i18n as i18n;

use super::surfaces::{
    BalanceDirection, BalanceRow, PageSection, RecentEntryRow, RequiredPageSection, TransferRow,
};

impl PageSection<BalanceRow> {
    pub fn to_svg(&self) -> Option<RenderedSvg> {
        match self {
            PageSection::Hidden => None,
            PageSection::Empty => Some(
                SvgTableBuilder::new()
                    .alignments(&[Alignment::Left, Alignment::Right])
                    .headers([Cow::Borrowed(i18n::MEMBER), Cow::Borrowed(i18n::BALANCE)])
                    .build(),
            ),
            PageSection::Rows(rows) => {
                let mut builder = SvgTableBuilder::new()
                    .alignments(&[Alignment::Left, Alignment::Right])
                    .headers([Cow::Borrowed(i18n::MEMBER), Cow::Borrowed(i18n::BALANCE)]);
                for row in rows.iter() {
                    let sign = match row.direction {
                        BalanceDirection::Receive => "+",
                        BalanceDirection::Pay => "-",
                    };
                    builder = builder.row([
                        Cow::Owned(row.display_name.as_str().to_owned()),
                        Cow::Owned(format!("{sign}{}", row.amount)),
                    ]);
                }
                Some(builder.build())
            }
        }
    }
}

impl RequiredPageSection<TransferRow> {
    pub fn to_svg(&self) -> Option<RenderedSvg> {
        match self {
            RequiredPageSection::Hidden => None,
            RequiredPageSection::Rows(rows) => {
                let mut builder = SvgTableBuilder::new()
                    .alignments(&[Alignment::Left, Alignment::Left, Alignment::Right])
                    .headers([
                        Cow::Borrowed(i18n::FROM),
                        Cow::Borrowed(i18n::TO),
                        Cow::Borrowed(i18n::AMOUNT),
                    ]);
                for row in rows.iter() {
                    builder = builder.row([
                        Cow::Owned(row.from_display_name.as_str().to_owned()),
                        Cow::Owned(row.to_display_name.as_str().to_owned()),
                        Cow::Owned(row.amount.clone()),
                    ]);
                }
                Some(builder.build())
            }
        }
    }
}

impl PageSection<RecentEntryRow> {
    pub fn to_svg(&self) -> Option<RenderedSvg> {
        match self {
            PageSection::Hidden => None,
            PageSection::Empty => Some(
                SvgTableBuilder::new()
                    .headers([
                        Cow::Borrowed("#"),
                        Cow::Borrowed(i18n::DATE_COLUMN),
                        Cow::Borrowed(i18n::SUMMARY_COLUMN),
                    ])
                    .build(),
            ),
            PageSection::Rows(rows) => {
                let headers = [
                    Cow::Borrowed("#"),
                    Cow::Borrowed(i18n::DATE_COLUMN),
                    Cow::Borrowed(i18n::SUMMARY_COLUMN),
                ];
                let mut builder = SvgTableBuilder::new().headers(headers);
                for row in rows.iter() {
                    builder = builder.row([
                        Cow::Owned(format!("#{}", row.entry_id)),
                        Cow::Owned(row.summary.date().to_string()),
                        Cow::Owned(row.summary.render_recent_entry_summary()),
                    ]);
                }
                Some(builder.build())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discord_ledger::{ExpenseOrSettlementSummary, SafeLiteralText};
    use walicord_application::ledger::{LedgerEffectiveDate, LedgerEntryId};
    use walicord_domain::NonEmptyVec;

    fn name(s: &str) -> SafeLiteralText {
        SafeLiteralText::from_roster_label(s).expect("label")
    }

    fn note(s: &str) -> SafeLiteralText {
        SafeLiteralText::from_note(s).expect("note")
    }

    #[test]
    fn balances_hidden_returns_none() {
        assert!(PageSection::<BalanceRow>::Hidden.to_svg().is_none());
    }

    #[test]
    fn balances_empty_returns_header_only() {
        let svg = PageSection::<BalanceRow>::Empty
            .to_svg()
            .expect("should produce SVG");
        let xml = svg.to_svg_string();
        assert!(xml.contains(i18n::MEMBER));
        assert!(xml.contains(i18n::BALANCE));
    }

    #[test]
    fn balances_rows_contains_member_and_amount() {
        let rows = NonEmptyVec::new(vec![
            BalanceRow {
                display_name: name("Alice"),
                amount: "500".to_owned(),
                direction: BalanceDirection::Receive,
            },
            BalanceRow {
                display_name: name("Bob"),
                amount: "500".to_owned(),
                direction: BalanceDirection::Pay,
            },
        ])
        .expect("non-empty");
        let svg = PageSection::Rows(rows)
            .to_svg()
            .expect("should produce SVG");
        let xml = svg.to_svg_string();
        assert!(xml.contains("Alice"));
        assert!(xml.contains("+500"));
        assert!(xml.contains("Bob"));
        assert!(xml.contains("-500"));
    }

    #[test]
    fn transfers_hidden_returns_none() {
        assert!(
            RequiredPageSection::<TransferRow>::Hidden
                .to_svg()
                .is_none()
        );
    }

    #[test]
    fn transfers_rows_contains_from_to_amount() {
        let rows = NonEmptyVec::new(vec![TransferRow {
            from_display_name: name("Bob"),
            to_display_name: name("Alice"),
            amount: "300".to_owned(),
        }])
        .expect("non-empty");
        let svg = RequiredPageSection::Rows(rows)
            .to_svg()
            .expect("should produce SVG");
        let xml = svg.to_svg_string();
        assert!(xml.contains("Bob"));
        assert!(xml.contains("Alice"));
        assert!(xml.contains("300"));
    }

    #[test]
    fn recent_entries_hidden_returns_none() {
        assert!(PageSection::<RecentEntryRow>::Hidden.to_svg().is_none());
    }

    #[test]
    fn recent_entries_expense_and_settlement() {
        let long_note = format!("{}tail", "x".repeat(40));
        let rows = NonEmptyVec::new(vec![
            RecentEntryRow {
                entry_id: LedgerEntryId(5),
                summary: ExpenseOrSettlementSummary::Expense {
                    date: LedgerEffectiveDate::new("2026-05-26").expect("date"),
                    payer_display_name: name("Alice"),
                    amount: "1500".to_owned(),
                    note: Some(note(&long_note)),
                },
                recovery_reference: super::super::surfaces::RecoveryReference {
                    ledger_id_short: "abcd1234".to_owned(),
                    entry_id: LedgerEntryId(5),
                    message_link: None,
                },
            },
            RecentEntryRow {
                entry_id: LedgerEntryId(3),
                summary: ExpenseOrSettlementSummary::Settlement {
                    date: LedgerEffectiveDate::new("2026-05-25").expect("date"),
                    from_display_name: name("Bob"),
                    to_display_name: name("Alice"),
                    amount: "300".to_owned(),
                    additional_transfers: 0,
                },
                recovery_reference: super::super::surfaces::RecoveryReference {
                    ledger_id_short: "abcd1234".to_owned(),
                    entry_id: LedgerEntryId(3),
                    message_link: None,
                },
            },
        ])
        .expect("non-empty");
        let svg = PageSection::Rows(rows)
            .to_svg()
            .expect("should produce SVG");
        let xml = svg.to_svg_string();
        assert!(xml.contains("#5"));
        assert!(xml.contains("Alice"));
        assert!(xml.contains("1500"));
        assert!(xml.contains(&format!("{}...", "x".repeat(30))));
        assert!(!xml.contains("tail"));
        assert!(xml.contains("#3"));
        assert!(xml.contains("Bob"));
        assert!(xml.contains("300"));
    }
}
