package com.serenity.command

/** UI-preset management input items (save, apply, overwrite, duplicate, rename, delete, reset). Split out of
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
      id = "ui-preset-apply",
      label = "Apply Preset",
      hint = "Preset name",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        CommandRunnerSettingsTextParsing
          .nonEmptyText(text)
          .map(commandIntentArg => CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(commandIntentArg))),
      category = CommandCategory.Settings
    ),
    CommandSurfaceItem.InputItem(
      id = "ui-preset-overwrite",
      label = "Overwrite Preset",
      hint = "Existing custom preset name",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        CommandRunnerSettingsTextParsing
          .nonEmptyText(text)
          .map(commandIntentArg => CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset(commandIntentArg))),
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
    ),
    CommandSurfaceItem.InputItem(
      id = "ui-preset-delete",
      label = "Delete Preset",
      hint = "Preset name",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        CommandRunnerSettingsTextParsing
          .nonEmptyText(text)
          .map(commandIntentArg => CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset(commandIntentArg))),
      category = CommandCategory.Settings
    ),
    CommandSurfaceItem.InputItem(
      id = "ui-preset-reset",
      label = "Reset Preset",
      hint = "Built-in preset name",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        CommandRunnerSettingsTextParsing
          .nonEmptyText(text)
          .map(commandIntentArg => CommandIntent.UiPresets(UiPresetsIntent.ResetUiPreset(commandIntentArg))),
      category = CommandCategory.Settings
    )
  )
