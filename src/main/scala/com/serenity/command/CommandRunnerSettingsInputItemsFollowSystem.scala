package com.serenity.command

import com.serenity.config.{AppearanceSlot, ThemeFollowConfig}

/** The theme each OS appearance maps to while `theme.follow_system` is on. Typed names, like the config keys they set:
  * a name no theme answers to leaves the theme where it was when that appearance comes round.
  */
private[command] object CommandRunnerSettingsInputItemsFollowSystem:

  private[command] def items(follow: ThemeFollowConfig): List[CommandSurfaceItem.InputItem] = List(
    row("follow-system-light", "Light Theme", "Theme for a light OS", AppearanceSlot.Light, follow),
    row("follow-system-dark", "Dark Theme", "Theme for a dark OS", AppearanceSlot.Dark, follow),
    row(
      "follow-system-high-contrast",
      "High-Contrast Theme",
      "Theme for an OS asking for high contrast",
      AppearanceSlot.HighContrast,
      follow
    )
  )

  private def row(
    id: String,
    label: String,
    hint: String,
    slot: AppearanceSlot,
    follow: ThemeFollowConfig
  ): CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = id,
      label = label,
      hint = hint,
      currentValue = follow.themeIn(slot),
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        CommandRunnerSettingsTextParsing
          .nonEmptyText(text)
          .map(name => CommandIntent.Theme(ThemeIntent.SetFollowSystemTheme(slot, name))),
      category = CommandCategory.Settings,
      defaultValue = Some(ThemeFollowConfig().themeIn(slot))
    )
