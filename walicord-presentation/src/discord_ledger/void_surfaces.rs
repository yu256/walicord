use super::surfaces::{
    ExpenseOrSettlementSummary, RecoveryCta, RecoveryReference, SurfaceActionRow,
};
use walicord_i18n as i18n;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum VoidSurfacePhase {
    #[default]
    Select,
    Confirm,
    Success,
    OperatorHandoff,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VoidRetargetReason {
    VoidedByAnotherUser,
    EnteredSealedRange,
    ExcludedFromCandidates,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VoidCandidateRow {
    pub summary: ExpenseOrSettlementSummary,
    pub recovery_reference: RecoveryReference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VoidConfirmationRecap {
    pub summary: ExpenseOrSettlementSummary,
    pub total_amount: String,
    pub recovery_reference: RecoveryReference,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct VoidSurfaceModel {
    pub title: String,
    pub phase: VoidSurfacePhase,
    pub stale_page: bool,
    pub page_indicator: Option<String>,
    pub snapshot_notice: Option<String>,
    pub phase_copy: Vec<String>,
    pub warning_lines: Vec<String>,
    pub recovery_cta: RecoveryCta,
    pub recovery_url: Option<String>,
    pub missing_thread_note: bool,
    pub candidates: Vec<VoidCandidateRow>,
    pub confirmation: Option<VoidConfirmationRecap>,
    pub primary_action_label: Option<String>,
    pub secondary_action_label: Option<String>,
    pub in_flight_copy: Option<String>,
    pub wrong_stage_copy: Option<String>,
    pub action_rows: Vec<SurfaceActionRow>,
    pub ephemeral: bool,
}

impl VoidSurfaceModel {
    pub fn selection(
        title: impl Into<String>,
        candidates: Vec<VoidCandidateRow>,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        Self {
            title: title.into(),
            phase: VoidSurfacePhase::Select,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            phase_copy: Vec::new(),
            warning_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            candidates,
            confirmation: None,
            primary_action_label: Some(i18n::VOID_NEXT_LABEL.to_owned()),
            secondary_action_label: None,
            in_flight_copy: None,
            wrong_stage_copy: None,
            action_rows,
            ephemeral,
        }
    }

    pub fn missing_selection(
        title: impl Into<String>,
        candidates: Vec<VoidCandidateRow>,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        Self {
            title: title.into(),
            phase: VoidSurfacePhase::Select,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            phase_copy: vec![i18n::VOID_MISSING_SELECTION_ERROR.to_owned()],
            warning_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            candidates,
            confirmation: None,
            primary_action_label: Some(i18n::VOID_NEXT_LABEL.to_owned()),
            secondary_action_label: None,
            in_flight_copy: None,
            wrong_stage_copy: None,
            action_rows,
            ephemeral,
        }
    }

    pub fn stale_target(
        title: impl Into<String>,
        reason: VoidRetargetReason,
        candidates: Vec<VoidCandidateRow>,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        let reason_text = match reason {
            VoidRetargetReason::VoidedByAnotherUser => i18n::VOID_STALE_TARGET_REASON_VOIDED,
            VoidRetargetReason::EnteredSealedRange => i18n::VOID_STALE_TARGET_REASON_SEALED,
            VoidRetargetReason::ExcludedFromCandidates => i18n::VOID_STALE_TARGET_REASON_EXCLUDED,
        };

        Self {
            title: title.into(),
            phase: VoidSurfacePhase::Select,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            phase_copy: vec![i18n::void_stale_target_error(reason_text).to_string()],
            warning_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            candidates,
            confirmation: None,
            primary_action_label: Some(i18n::VOID_NEXT_LABEL.to_owned()),
            secondary_action_label: None,
            in_flight_copy: None,
            wrong_stage_copy: None,
            action_rows,
            ephemeral,
        }
    }

    pub fn confirmation(
        title: impl Into<String>,
        recap: VoidConfirmationRecap,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        Self {
            title: title.into(),
            phase: VoidSurfacePhase::Confirm,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            phase_copy: vec![i18n::VOID_CONFIRM_APPEND_ONLY_LINE.to_owned()],
            warning_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            candidates: Vec::new(),
            confirmation: Some(recap),
            primary_action_label: Some(i18n::VOID_CONFIRM_LABEL.to_owned()),
            secondary_action_label: Some(i18n::VOID_RESELECT_LABEL.to_owned()),
            in_flight_copy: Some(i18n::VOID_IN_FLIGHT_COPY.to_owned()),
            wrong_stage_copy: Some(i18n::VOID_WRONG_STAGE_COPY.to_owned()),
            action_rows,
            ephemeral,
        }
    }

    pub fn success(
        title: impl Into<String>,
        canonical_thread_mention: impl std::fmt::Display,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        Self::success_with_recovery(
            title,
            Some(canonical_thread_mention.to_string()),
            RecoveryCta::None,
            None,
            action_rows,
            ephemeral,
        )
    }

    pub fn success_with_recovery(
        title: impl Into<String>,
        canonical_thread_mention: Option<String>,
        recovery_cta: RecoveryCta,
        recovery_url: Option<String>,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        Self {
            title: title.into(),
            phase: VoidSurfacePhase::Success,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            phase_copy: vec![
                i18n::VOID_SUCCESS_ACK.to_owned(),
                i18n::VOID_SUCCESS_EFFECT.to_owned(),
                canonical_thread_mention
                    .map(|thread_mention| {
                        i18n::void_success_thread_line(thread_mention).to_string()
                    })
                    .unwrap_or_else(|| i18n::VOID_SUCCESS_THREAD_LINE_WITHOUT_MENTION.to_owned()),
            ],
            warning_lines: Vec::new(),
            recovery_cta,
            recovery_url,
            missing_thread_note: false,
            candidates: Vec::new(),
            confirmation: None,
            primary_action_label: None,
            secondary_action_label: None,
            in_flight_copy: None,
            wrong_stage_copy: None,
            action_rows,
            ephemeral,
        }
    }

    pub fn empty(
        title: impl Into<String>,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        Self {
            title: title.into(),
            phase: VoidSurfacePhase::OperatorHandoff,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            phase_copy: vec![i18n::VOID_EMPTY_STATE.to_owned()],
            warning_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            candidates: Vec::new(),
            confirmation: None,
            primary_action_label: None,
            secondary_action_label: None,
            in_flight_copy: None,
            wrong_stage_copy: None,
            action_rows,
            ephemeral,
        }
    }

    pub fn no_candidates(
        title: impl Into<String>,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        Self {
            title: title.into(),
            phase: VoidSurfacePhase::OperatorHandoff,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            phase_copy: vec![i18n::VOID_NO_CANDIDATE_STATE.to_owned()],
            warning_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            candidates: Vec::new(),
            confirmation: None,
            primary_action_label: None,
            secondary_action_label: None,
            in_flight_copy: None,
            wrong_stage_copy: None,
            action_rows,
            ephemeral,
        }
    }

    pub fn older_than_window(
        title: impl Into<String>,
        reference_hint: impl std::fmt::Display,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        Self {
            title: title.into(),
            phase: VoidSurfacePhase::OperatorHandoff,
            stale_page: false,
            page_indicator: None,
            snapshot_notice: None,
            phase_copy: vec![
                i18n::VOID_WINDOW_EMPTY_STATE.to_owned(),
                i18n::void_window_handoff_line(reference_hint).to_string(),
            ],
            warning_lines: Vec::new(),
            recovery_cta: RecoveryCta::None,
            recovery_url: None,
            missing_thread_note: false,
            candidates: Vec::new(),
            confirmation: None,
            primary_action_label: None,
            secondary_action_label: None,
            in_flight_copy: None,
            wrong_stage_copy: None,
            action_rows,
            ephemeral,
        }
    }

    pub fn stale_page(
        title: impl Into<String>,
        recovery_cta: RecoveryCta,
        recovery_url: Option<String>,
        missing_thread_note: bool,
        action_rows: Vec<SurfaceActionRow>,
        ephemeral: bool,
    ) -> Self {
        Self {
            title: title.into(),
            phase: VoidSurfacePhase::OperatorHandoff,
            stale_page: true,
            page_indicator: None,
            snapshot_notice: None,
            phase_copy: Vec::new(),
            warning_lines: Vec::new(),
            recovery_cta,
            recovery_url,
            missing_thread_note,
            candidates: Vec::new(),
            confirmation: None,
            primary_action_label: None,
            secondary_action_label: None,
            in_flight_copy: None,
            wrong_stage_copy: None,
            action_rows,
            ephemeral,
        }
    }
}
