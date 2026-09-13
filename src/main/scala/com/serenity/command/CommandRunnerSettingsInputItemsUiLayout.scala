package com.serenity.command

import com.serenity.config.*

/** UI spacing and command-runner layout input items. Split out of `CommandRunnerSettingsInputItems.build` to keep both
  * under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsInputItemsUiLayout:

  private[command] def uiSpacingItems(
    elementGapValue: String,
    cornerRadiusValue: String,
    outlineThicknessValue: String
  ): List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "ui-element-gap",
      label = "UI Element Gap",
      hint = s"Cells, decimals supported (${AppConfig.MinUiElementGap}-${AppConfig.MaxUiElementGap})",
      currentValue = elementGapValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toDoubleOption
          .filter(value => value >= AppConfig.MinUiElementGap && value <= AppConfig.MaxUiElementGap)
          .map(commandIntentArg =>
            CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiElementGap(commandIntentArg)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(CommandRunnerSettingsInputItems.formatDecimal(AppConfig.default.interfaceConfig.elementGap))
    ),
    CommandSurfaceItem.InputItem(
      id = "ui-corner-radius",
      label = "UI Corner Radius",
      hint = s"Pixels (${AppConfig.MinUiCornerRadiusPx}-${AppConfig.MaxUiCornerRadiusPx})",
      currentValue = cornerRadiusValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = false),
      parse = text =>
        text.toIntOption
          .filter(value => value >= AppConfig.MinUiCornerRadiusPx && value <= AppConfig.MaxUiCornerRadiusPx)
          .map(commandIntentArg =>
            CommandIntent
              .Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiCornerRadiusPx(commandIntentArg)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(AppConfig.default.interfaceConfig.cornerRadiusPx.toString)
    ),
    CommandSurfaceItem.InputItem(
      id = "ui-outline-thickness",
      label = "UI Outline Thickness",
      hint = s"Pixels (${AppConfig.MinUiOutlineThicknessPx}-${AppConfig.MaxUiOutlineThicknessPx})",
      currentValue = outlineThicknessValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = false),
      parse = text =>
        text.toIntOption
          .filter(value => value >= AppConfig.MinUiOutlineThicknessPx && value <= AppConfig.MaxUiOutlineThicknessPx)
          .map(commandIntentArg =>
            CommandIntent
              .Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiOutlineThicknessPx(commandIntentArg)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(AppConfig.default.interfaceConfig.outlineThicknessPx.toString)
    )
  )

  private[command] def commandRunnerLayoutItems(
    commandRowsValue: String,
    commandItemGapRowsValue: String,
    commandCursorGapRowsValue: String
  ): List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "command-runner-visible-rows",
      label = "Visible Commands",
      hint =
        s"Command rows (${AppConfig.MinCommandRunnerVisibleRows}-${AppConfig.MaxCommandRunnerVisibleRows}) or auto",
      currentValue = commandRowsValue,
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        val normalized = text.trim.toLowerCase
        if normalized == "auto" then
          Some(CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerVisibleRows(None))))
        else
          normalized.toIntOption
            .filter(value =>
              value >= AppConfig.MinCommandRunnerVisibleRows &&
                value <= AppConfig.MaxCommandRunnerVisibleRows
            )
            .map(value =>
              CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerVisibleRows(Some(value))))
            )
      ,
      category = CommandCategory.Settings
    ),
    CommandSurfaceItem.InputItem(
      id = "command-runner-item-gap-rows",
      label = "Command Item Spacing",
      hint =
        s"Rows, decimals supported (${AppConfig.MinCommandRunnerItemGapRows}-${AppConfig.MaxCommandRunnerItemGapRows})",
      currentValue = commandItemGapRowsValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.trim.toDoubleOption
          .filter(value =>
            value >= AppConfig.MinCommandRunnerItemGapRows &&
              value <= AppConfig.MaxCommandRunnerItemGapRows
          )
          .map(commandIntentArg =>
            CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerItemGapRows(commandIntentArg)))
          ),
      category = CommandCategory.Settings
    ),
    CommandSurfaceItem.InputItem(
      id = "command-runner-cursor-gap-rows",
      label = "Command Cursor Spacing",
      hint =
        s"Rows, decimals supported (${AppConfig.MinCommandRunnerCursorGapRows}-${AppConfig.MaxCommandRunnerCursorGapRows}) or auto",
      currentValue = commandCursorGapRowsValue,
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        val normalized = text.trim.toLowerCase
        if normalized == "auto" then
          Some(CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerCursorGapRows(None))))
        else
          normalized.toDoubleOption
            .filter(value =>
              value >= AppConfig.MinCommandRunnerCursorGapRows &&
                value <= AppConfig.MaxCommandRunnerCursorGapRows
            )
            .map(value =>
              CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerCursorGapRows(Some(value))))
            )
      ,
      category = CommandCategory.Settings
    )
  )
