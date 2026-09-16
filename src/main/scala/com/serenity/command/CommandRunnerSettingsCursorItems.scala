package com.serenity.command

import com.serenity.config.CursorMode

/** Cursor style settings items. */
private[command] object CommandRunnerSettingsCursorItems:

  private[command] def cursorModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-mode",
      label = "Cursor Style",
      options = List(
        CommandOption(
          "Blink",
          CommandIntent.Settings(SettingsIntent.Cursor(CursorIntent.SetCursorMode(CursorMode.Blink)))
        ),
        CommandOption(
          "Breathe",
          CommandIntent.Settings(SettingsIntent.Cursor(CursorIntent.SetCursorMode(CursorMode.Breathe)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("cursor-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Blink or breathe")
    )
