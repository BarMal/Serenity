package com.serenity.command

import com.serenity.ui.presets.UiPreset

/** The UI Presets subtree of the settings tree: select, create, and edit a preset. Split out of
  * `CommandRunnerSettingsGroups` so each stays under the architecture size targets.
  */
private[command] object CommandRunnerSettingsPresetGroups:

  def build(
    optionSelections: Map[String, Int],
    inputItems: List[CommandSurfaceItem.InputItem],
    uiPresetPreviews: List[UiPreset.Preview],
    editingPresetName: Option[String],
    presetScopedGroups: List[CommandSurfaceItem.GroupItem]
  ): CommandSurfaceItem.GroupItem =
    def group(id: String, label: String, hint: String, children: List[CommandSurfaceItem]) =
      CommandSurfaceItem.GroupItem(id, label, children, CommandCategory.Settings, Some(hint))
    val editingPreset = presetEditContextName(optionSelections, uiPresetPreviews, editingPresetName)
    val presetInputItems =
      inputItems.filter(_.id.startsWith("ui-preset-")).map(withPresetInputContext(_, editingPreset))
    // issue #1060: Apply/Overwrite/Delete/Reset act on one *existing* preset, so they pick from the same built-in-
    // plus-saved catalog `ui-preset-select` already carousels through, instead of requiring a typed exact name.
    def action(id: String, label: String, hint: String, intent: String => UiPresetsIntent) =
      CommandRunnerSettingsItems.presetActionOptionItem(id, label, hint, uiPresetPreviews, editingPreset, intent)
    val presetActionItems = List(
      action("ui-preset-apply", "Apply Preset", "Reapply this preset's settings", UiPresetsIntent.ApplyUiPreset(_)),
      action(
        "ui-preset-overwrite",
        "Overwrite Preset",
        "Save the current workspace into this preset",
        UiPresetsIntent.OverwriteUiPreset(_)
      )
    ) ++ presetInputItems.filter(_.id == "ui-preset-duplicate") ++ List(
      action("ui-preset-delete", "Delete Preset", "Remove this preset", UiPresetsIntent.DeleteUiPreset(_)),
      action("ui-preset-reset", "Reset Preset", "Discard this preset's overrides", UiPresetsIntent.ResetUiPreset(_))
    )
    val selectPresetGroup = group(
      "settings-preset-select",
      "Select Preset",
      "Browse available presets",
      List(CommandRunnerSettingsItems.uiPresetSelectOptionItem(uiPresetPreviews, optionSelections))
    )
    val createPresetGroup = group(
      "settings-preset-create",
      "Create New Preset",
      "Start from current workspace settings",
      group(
        "settings-preset-create-name",
        "Name",
        "Save the current workspace as a new preset",
        presetInputItems.filter(_.id == "ui-preset-save-as-new")
      ) :: presetScopedGroups
    )
    val editPresetGroup = group(
      "settings-preset-edit",
      editingPreset.fold("Edit Preset")(name => s"Edit Preset: $name"),
      editingPreset.fold("Document, layout, typography, motion")(name => s"Editing $name"),
      List(
        group(
          "settings-preset-name",
          "Name",
          "Rename this preset",
          presetInputItems.filter(_.id == "ui-preset-rename")
        ),
        group(
          "settings-preset-actions",
          "Preset Actions",
          "Apply, overwrite, duplicate, delete, or reset",
          presetActionItems
        )
      ) ++ presetScopedGroups
    )
    group(
      "settings-ui-presets",
      "UI Presets",
      "Save or apply named layouts",
      List(selectPresetGroup, createPresetGroup, editPresetGroup)
    )

  private[command] def presetEditContextName(
    optionSelections: Map[String, Int],
    uiPresetPreviews: List[UiPreset.Preview],
    editingPresetName: Option[String]
  ): Option[String] =
    editingPresetName
      .map(_.trim)
      .filter(_.nonEmpty)
      .orElse(optionSelections.get("ui-preset-custom").flatMap(uiPresetPreviews.lift).map(_.name))
      .orElse(optionSelections.get("ui-preset-built-in").flatMap(UiPreset.builtIns.lift).map(_.name))
      .orElse(UiPreset.builtIns.headOption.map(_.name))

  // issue #1060: only Duplicate/Rename still take typed input (both need a new name, which can't be picked from an
  // existing-preset list) -- Apply/Overwrite/Delete/Reset are pickers now (`presetActionOptionItem`), so they no
  // longer take a prefill via `currentValue`.
  private def withPresetInputContext(
    item: CommandSurfaceItem.InputItem,
    presetName: Option[String]
  ): CommandSurfaceItem.InputItem =
    (presetName, item.id) match
      case (Some(name), "ui-preset-duplicate" | "ui-preset-rename") => item.copy(currentValue = s"$name -> ")
      case _                                                        => item
