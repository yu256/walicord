use serenity::{
    all::{
        ButtonStyle, CreateActionRow, CreateButton, CreateInteractionResponseMessage,
        CreateSelectMenu, CreateSelectMenuKind, CreateSelectMenuOption, EditInteractionResponse,
    },
    builder::{CreateAllowedMentions, CreateMessage, EditMessage},
};
use walicord_presentation::discord_ledger::{
    RenderedSurface, SurfaceActionRow, SurfaceButton, SurfaceInteractiveButtonStyle,
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

pub(crate) fn safe_edit_interaction_response() -> EditInteractionResponse {
    EditInteractionResponse::new().allowed_mentions(suppressed_allowed_mentions())
}

pub(crate) fn safe_create_message() -> CreateMessage {
    CreateMessage::new().allowed_mentions(suppressed_allowed_mentions())
}

pub(crate) fn safe_edit_message() -> EditMessage {
    EditMessage::new().allowed_mentions(suppressed_allowed_mentions())
}

pub(crate) fn rendered_surface_to_message(
    rendered: RenderedSurface,
) -> (String, Vec<CreateActionRow>) {
    (
        rendered.body,
        surface_action_rows_to_components(&rendered.action_rows),
    )
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
                        let select_option =
                            CreateSelectMenuOption::new(option.label.clone(), option.value.clone())
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
        } => CreateButton::new_link(url.clone())
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
