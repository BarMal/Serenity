package com.serenity.command

/** UI-preset management input items that inherently need a typed name (a new preset name, or a "Source -> New" pair) --
  * save-as-new, duplicate, rename. Apply/Overwrite/Delete/Reset act on one *existing* preset and are built as pickers
  * instead (`CommandRunnerSettingsItems.presetActionOptionItem`, issue #1060). Split out of
  * `CommandRunnerSettingsInputItems.build` to keep both under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsInputItemsPresets:

  private[command] def presetCreateItems: List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "ui-preset-save-as-new",
      label = "Save As New Preset",
      hint = "New preset name",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        CommandRunnerSettingsTextParsing
          .nonEmptyText(text)
          .map(commandIntentArg => CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew(commandIntentArg))),
      category = CommandCategory.Settings
    ),
    CommandSurfaceItem.InputItem(
      id = "ui-preset-duplicate",
      label = "Duplicate Preset",
      hint = "Source -> Copy",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        CommandRunnerSettingsTextParsing.namedPair(text).map {
          case (sourceName, targetName) =>
            CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset(sourceName, targetName))
        },
      category = CommandCategory.Settings
    )
  )

  private[command] def presetManageItems: List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "ui-preset-rename",
      label = "Rename Preset",
      hint = "Current -> New",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        CommandRunnerSettingsTextParsing.namedPair(text).map {
          case (sourceName, targetName) =>
            CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset(sourceName, targetName))
        },
      category = CommandCategory.Settings
    )
  )
