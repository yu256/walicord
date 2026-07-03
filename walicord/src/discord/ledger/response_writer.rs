use serenity::{
    all::{
        ButtonStyle, CreateActionRow, CreateAttachment, CreateButton,
        CreateInteractionResponseMessage, CreateSelectMenu, CreateSelectMenuKind,
        CreateSelectMenuOption, EditInteractionResponse,
    },
    builder::{CreateAllowedMentions, CreateMessage},
};
use walicord_presentation::{
    RenderedSvg,
    discord_ledger::{
        MessageContent, MessageContentBuilder, PresentationSurfaceBody, RenderBudgetError,
        RenderedSurface, SurfaceActionRow, SurfaceButton, SurfaceInteractiveButtonStyle,
        TextRenderedSurface,
    },
};

pub(crate) fn suppressed_allowed_mentions() -> CreateAllowedMentions {
    CreateAllowedMentions::new()
        .all_users(false)
        .all_roles(false)
        .everyone(false)
        .replied_user(false)
        .empty_users()
        .empty_roles()
}

pub(crate) fn safe_interaction_response_message() -> CreateInteractionResponseMessage {
    CreateInteractionResponseMessage::new().allowed_mentions(suppressed_allowed_mentions())
}

pub(crate) fn safe_ephemeral_interaction_response_message() -> CreateInteractionResponseMessage {
    safe_interaction_response_message().ephemeral(true)
}

pub(crate) fn deferred_ephemeral_interaction_response_message() -> CreateInteractionResponseMessage
{
    CreateInteractionResponseMessage::new().ephemeral(true)
}

pub(crate) fn safe_edit_interaction_response() -> EditInteractionResponse {
    EditInteractionResponse::new().allowed_mentions(suppressed_allowed_mentions())
}

pub(crate) fn safe_create_message() -> CreateMessage {
    CreateMessage::new().allowed_mentions(suppressed_allowed_mentions())
}

pub(crate) enum DiscordDraft {
    TextOnly {
        content: MessageContent,
        components: Vec<CreateActionRow>,
        ephemeral: bool,
    },
    WithAttachment {
        content: MessageContent,
        components: Vec<CreateActionRow>,
        attachment: CreateAttachment,
        ephemeral: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct DiscordTextMessage {
    content: MessageContent,
    components: Vec<CreateActionRow>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct DiscordTextMessageBuilder {
    content: MessageContentBuilder,
    components: Vec<CreateActionRow>,
}

impl DiscordTextMessage {
    pub(crate) fn parse(
        content: impl Into<String>,
        components: Vec<CreateActionRow>,
    ) -> Result<Self, RenderBudgetError> {
        Ok(Self {
            content: MessageContent::parse(content)?,
            components,
        })
    }

    pub(crate) fn fit_to_discord_limit(
        content: impl Into<String>,
        components: Vec<CreateActionRow>,
    ) -> Self {
        Self {
            content: MessageContent::fit_to_discord_limit(content),
            components,
        }
    }

    pub(crate) fn into_builder(self) -> DiscordTextMessageBuilder {
        DiscordTextMessageBuilder {
            content: self.content.into_builder(),
            components: self.components,
        }
    }

    pub(crate) fn into_parts(self) -> (String, Vec<CreateActionRow>) {
        (self.content.into_string(), self.components)
    }
}

impl DiscordTextMessageBuilder {
    pub(crate) fn push_content_line(&mut self, line: &str) {
        self.content.push_line(line);
    }

    pub(crate) fn build(self) -> Result<DiscordTextMessage, RenderBudgetError> {
        Ok(DiscordTextMessage {
            content: self.content.build()?,
            components: self.components,
        })
    }
}

impl DiscordDraft {
    pub fn resolve(
        rendered: RenderedSurface,
        rasterize: impl FnOnce(
            &RenderedSvg,
        )
            -> Result<Vec<u8>, crate::discord::svg_renderer::SvgRasterizeError>,
    ) -> Self {
        let parts = rendered.into_parts();
        let components = surface_action_rows_to_components(&parts.action_rows);
        match parts.body {
            PresentationSurfaceBody::Text(content) => Self::TextOnly {
                content,
                components,
                ephemeral: parts.ephemeral,
            },
            PresentationSurfaceBody::ImageBacked {
                compact_text,
                fallback_text,
                image,
            } => match rasterize(&image) {
                Ok(png) => Self::WithAttachment {
                    content: compact_text,
                    components,
                    attachment: CreateAttachment::bytes(png, "ledger.png"),
                    ephemeral: parts.ephemeral,
                },
                Err(e) => {
                    tracing::warn!("SVG rasterize failed: {e:?}");
                    Self::TextOnly {
                        content: fallback_text,
                        components,
                        ephemeral: parts.ephemeral,
                    }
                }
            },
        }
    }

    pub fn into_edit_response(self) -> EditInteractionResponse {
        let mut response = safe_edit_interaction_response();
        match self {
            Self::TextOnly {
                content,
                components,
                ..
            } => {
                response = response
                    .content(content.into_string())
                    .components(components);
                response = response.clear_attachments();
            }
            Self::WithAttachment {
                content,
                components,
                attachment,
                ..
            } => {
                response = response
                    .content(content.into_string())
                    .components(components)
                    .clear_attachments()
                    .new_attachment(attachment);
            }
        }
        response
    }

    pub fn into_update_message(self) -> CreateInteractionResponseMessage {
        match self {
            Self::TextOnly {
                content,
                components,
                ephemeral,
            } => {
                let mut msg = safe_interaction_response_message()
                    .content(content.into_string())
                    .components(components)
                    .files(std::iter::empty::<CreateAttachment>());
                if ephemeral {
                    msg = msg.ephemeral(true);
                }
                msg
            }
            Self::WithAttachment {
                content,
                components,
                attachment,
                ephemeral,
            } => {
                let mut msg = safe_interaction_response_message()
                    .content(content.into_string())
                    .components(components)
                    .add_file(attachment);
                if ephemeral {
                    msg = msg.ephemeral(true);
                }
                msg
            }
        }
    }

    pub fn push_component(&mut self, row: CreateActionRow) {
        match self {
            Self::TextOnly { components, .. } | Self::WithAttachment { components, .. } => {
                components.push(row);
            }
        }
    }
}

impl From<TextRenderedSurface> for DiscordTextMessage {
    fn from(rendered: TextRenderedSurface) -> Self {
        let parts = rendered.into_text_message_parts();
        Self {
            content: parts.body,
            components: surface_action_rows_to_components(&parts.action_rows),
        }
    }
}

fn surface_action_rows_to_components(action_rows: &[SurfaceActionRow]) -> Vec<CreateActionRow> {
    action_rows
        .iter()
        .map(|action_row| match action_row {
            SurfaceActionRow::Buttons(buttons) => CreateActionRow::Buttons(
                buttons
                    .iter()
                    .map(surface_button_to_create_button)
                    .collect(),
            ),
            SurfaceActionRow::Select(menu) => {
                let options = menu
                    .options
                    .iter()
                    .map(|option| {
                        let select_option = CreateSelectMenuOption::new(
                            option.label.as_str(),
                            option.value.clone(),
                        )
                        .default_selection(option.selected);
                        if let Some(description) = &option.description {
                            select_option.description(description.clone())
                        } else {
                            select_option
                        }
                    })
                    .collect();
                let select_menu = CreateSelectMenu::new(
                    menu.custom_id.clone(),
                    CreateSelectMenuKind::String { options },
                )
                .min_values(menu.min_values)
                .max_values(menu.max_values)
                .disabled(menu.disabled);
                let select_menu = if let Some(placeholder) = &menu.placeholder {
                    select_menu.placeholder(placeholder.clone())
                } else {
                    select_menu
                };
                CreateActionRow::SelectMenu(select_menu)
            }
        })
        .collect()
}

fn surface_button_to_create_button(button: &SurfaceButton) -> CreateButton {
    match button {
        SurfaceButton::Link {
            label,
            url,
            disabled,
        } => CreateButton::new_link(url.as_str())
            .label(label.clone())
            .disabled(*disabled),
        SurfaceButton::Interactive {
            label,
            custom_id,
            style,
            disabled,
        } => CreateButton::new(custom_id.clone())
            .label(label.clone())
            .style(match style {
                SurfaceInteractiveButtonStyle::Primary => ButtonStyle::Primary,
                SurfaceInteractiveButtonStyle::Secondary => ButtonStyle::Secondary,
                SurfaceInteractiveButtonStyle::Danger => ButtonStyle::Danger,
            })
            .disabled(*disabled),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn suppressed_allowed_mentions_json() -> serde_json::Value {
        serde_json::json!({
            "parse": [],
            "users": [],
            "roles": [],
            "replied_user": false
        })
    }

    #[test]
    fn canonical_message_builder_suppresses_every_allowed_mention_kind() {
        let actual = serde_json::to_value(safe_create_message().content("<@1> <@&2> @everyone"))
            .expect("message builder should serialize");

        assert_eq!(
            actual["allowed_mentions"],
            suppressed_allowed_mentions_json()
        );
    }

    #[test]
    fn interaction_response_builder_suppresses_every_allowed_mention_kind() {
        let actual = serde_json::to_value(
            safe_ephemeral_interaction_response_message().content("<@1> <@&2> @everyone"),
        )
        .expect("interaction response builder should serialize");

        assert_eq!(
            actual["allowed_mentions"],
            suppressed_allowed_mentions_json()
        );
    }

    #[test]
    fn deferred_response_builder_omits_allowed_mentions() {
        let actual = serde_json::to_value(deferred_ephemeral_interaction_response_message())
            .expect("deferred response builder should serialize");

        assert_eq!(actual.get("allowed_mentions"), None);
    }

    #[test]
    fn edit_response_builder_suppresses_every_allowed_mention_kind() {
        let actual =
            serde_json::to_value(safe_edit_interaction_response().content("<@1> <@&2> @everyone"))
                .expect("edit response builder should serialize");

        assert_eq!(
            actual["allowed_mentions"],
            suppressed_allowed_mentions_json()
        );
    }

    use walicord_presentation::svg_table::SvgTableBuilder;

    fn text_surface(body: &str) -> RenderedSurface {
        TextRenderedSurface::new(body, Vec::new(), true)
            .expect("text surface")
            .into()
    }

    fn image_backed_surface(compact: &str, fallback: &str) -> RenderedSurface {
        let svg = SvgTableBuilder::new()
            .headers([std::borrow::Cow::Borrowed("X")])
            .build();
        RenderedSurface::new_image_backed(compact, fallback, svg, Vec::new(), true)
            .expect("image-backed surface")
    }

    fn rasterize_ok(
        _: &RenderedSvg,
    ) -> Result<Vec<u8>, crate::discord::svg_renderer::SvgRasterizeError> {
        Ok(vec![1, 2, 3])
    }

    fn rasterize_fail(
        _: &RenderedSvg,
    ) -> Result<Vec<u8>, crate::discord::svg_renderer::SvgRasterizeError> {
        Err(crate::discord::svg_renderer::SvgRasterizeError::Parse)
    }

    #[test]
    fn resolve_draft_text_surface_becomes_text_only() {
        let draft = DiscordDraft::resolve(text_surface("hello"), rasterize_ok);
        let DiscordDraft::TextOnly { content, .. } = draft else {
            panic!("expected TextOnly");
        };
        assert_eq!(content.as_str(), "hello");
    }

    #[test]
    fn resolve_draft_image_backed_with_successful_rasterize_uses_compact_text() {
        let draft =
            DiscordDraft::resolve(image_backed_surface("compact", "fallback"), rasterize_ok);
        let DiscordDraft::WithAttachment { content, .. } = draft else {
            panic!("expected WithAttachment");
        };
        assert_eq!(content.as_str(), "compact");
    }

    #[test]
    fn resolve_draft_image_backed_with_failed_rasterize_falls_back_to_full_text() {
        let draft =
            DiscordDraft::resolve(image_backed_surface("compact", "fallback"), rasterize_fail);
        let DiscordDraft::TextOnly { content, .. } = draft else {
            panic!("expected TextOnly fallback");
        };
        assert_eq!(content.as_str(), "fallback");
    }

    #[test]
    fn text_message_builder_rejects_overlong_content_on_build() {
        let message = DiscordTextMessage::parse("hello", Vec::new()).expect("message");
        let mut builder = message.into_builder();
        builder.push_content_line(&"x".repeat(2_000));
        let actual = builder.build();

        assert!(matches!(
            actual,
            Err(RenderBudgetError::MessageContentTooLong { .. })
        ));
    }

    #[test]
    fn text_message_builder_appends_content_lines_in_order() {
        let message = DiscordTextMessage::parse("hello", Vec::new()).expect("message");
        let mut builder = message.into_builder();
        builder.push_content_line("world");
        builder.push_content_line("again");
        let actual = builder.build().expect("content should fit");

        assert_eq!(actual.into_parts().0, "hello\nworld\nagain");
    }

    #[test]
    fn text_only_edit_response_clears_attachments() {
        let draft = DiscordDraft::resolve(text_surface("hello"), rasterize_ok);
        let actual = serde_json::to_value(draft.into_edit_response())
            .expect("edit response should serialize");
        assert_eq!(actual["attachments"], serde_json::json!([]));
    }

    #[test]
    fn with_attachment_edit_response_carries_one_attachment() {
        let draft =
            DiscordDraft::resolve(image_backed_surface("compact", "fallback"), rasterize_ok);
        let actual = serde_json::to_value(draft.into_edit_response())
            .expect("edit response should serialize");
        assert_eq!(
            actual["attachments"]
                .as_array()
                .expect("attachments array")
                .len(),
            1
        );
    }

    #[test]
    fn text_only_update_message_clears_attachments() {
        let draft = DiscordDraft::resolve(text_surface("hello"), rasterize_ok);
        let actual = serde_json::to_value(draft.into_update_message())
            .expect("update message should serialize");
        assert_eq!(actual["attachments"], serde_json::json!([]));
    }

    #[test]
    fn with_attachment_update_message_carries_one_attachment() {
        let draft =
            DiscordDraft::resolve(image_backed_surface("compact", "fallback"), rasterize_ok);
        let actual = serde_json::to_value(draft.into_update_message())
            .expect("update message should serialize");
        assert_eq!(
            actual["attachments"]
                .as_array()
                .expect("attachments array")
                .len(),
            1
        );
    }
}
