package com.serenity.command

import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.state.models.Shell
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.presets.UiPreset

/** The UI Presets subtree of the settings tree: select, save as new, and edit a preset. Split out of
  * `CommandRunnerSettingsGroups` so each stays under the architecture size targets.
  */
private[command] object CommandRunnerSettingsPresetGroups:

  def build(
    optionSelections: Map[String, Int],
    inputItems: List[CommandSurfaceItem.InputItem],
    uiPresetPreviews: List[UiPreset.Preview],
    editingPresetName: Option[String],
    capabilities: FrontendCapabilities,
    fontFamilies: FontLoader.FontFamilyCatalog,
    showAllSettings: Boolean
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
      action("ui-preset-apply", "Apply Preset", "Reapply this preset's settings", UiPresetsIntent.ReviewUiPreset(_)),
      action(
        "ui-preset-overwrite",
        "Overwrite Preset",
        "Save the current workspace into this preset",
        UiPresetsIntent.OverwriteUiPreset(_)
      )
    ) ++ presetInputItems.filter(_.id == "ui-preset-duplicate") ++ presetInputItems.filter(
      _.id == "ui-preset-set-theme"
    ) ++ List(
      action(
        "ui-preset-use-current-theme",
        "Use Current Theme",
        "Make this preset apply the theme in use now",
        UiPresetsIntent.UseCurrentThemeForUiPreset(_)
      ),
      action(
        "ui-preset-clear-theme",
        "Clear Preset Theme",
        "Make this preset leave the theme alone",
        UiPresetsIntent.ClearUiPresetTheme(_)
      ),
      action("ui-preset-delete", "Delete Preset", "Remove this preset", UiPresetsIntent.DeleteUiPreset(_)),
      action("ui-preset-reset", "Reset Preset", "Discard this preset's overrides", UiPresetsIntent.ResetUiPreset(_))
    )
    val selectPresetGroup = group(
      "settings-preset-select",
      "Select Preset",
      "Browse available presets",
      List(CommandRunnerSettingsItems.uiPresetSelectOptionItem(uiPresetPreviews, optionSelections))
    )
    val saveAsNewGroup = group(
      "settings-preset-create",
      "Save As New Preset",
      "Save the current workspace as a new preset",
      presetInputItems.filter(_.id == "ui-preset-save-as-new")
    )
    val editPresetGroup = group(
      "settings-preset-edit",
      editingPreset.fold("Edit Preset")(name => s"Edit Preset: $name"),
      editingPreset.fold("Fonts, cursor, documents, spelling")(name =>
        s"Editing $name. Panel layout: use Overwrite Preset"
      ),
      presetInputItems.filter(_.id == "ui-preset-rename") ++ presetActionItems ++ editingPreset.toList.flatMap(
        editableRows(_, optionSelections, inputItems, uiPresetPreviews, capabilities, fontFamilies, showAllSettings)
      )
    )
    group(
      "settings-ui-presets",
      "UI Presets",
      "Save or apply named layouts",
      List(selectPresetGroup, saveAsNewGroup, editPresetGroup)
    )

  // The rows show the preset's own values; a preview built from a name alone carries none, so those rows show the
  // live ones.
  private def editableRows(
    presetName: String,
    optionSelections: Map[String, Int],
    inputItems: List[CommandSurfaceItem.InputItem],
    uiPresetPreviews: List[UiPreset.Preview],
    capabilities: FrontendCapabilities,
    fontFamilies: FontLoader.FontFamilyCatalog,
    showAllSettings: Boolean
  ): List[CommandSurfaceItem] =
    val (shownSelections, shownInputs) =
      presetConfig(presetName, uiPresetPreviews).fold(optionSelections -> inputItems) { config =>
        val inFlight = CommandRunnerSettingsPresetScope.selectionsFor(presetName, optionSelections)
        (CommandRunnerOptionSelections.default(config) ++ inFlight) ->
          CommandRunnerSettingsInputItems.build(config, capabilities)
      }
    val editable   = CommandRunnerSettingsEditableGroups.withFamilyCarousels(shownSelections, shownInputs, fontFamilies)
    val onFrontend = SettingsFrontendFilter(Shell.of(capabilities), showAllSettings)
    def rows(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
      CommandRunnerSettingsPresetScope.rowsOf(presetName, group)
    def guiOnly(item: CommandSurfaceItem): Option[CommandSurfaceItem] =
      item match
        case option: CommandSurfaceItem.OptionItem => onFrontend.row(FrontendSupport.GuiOnly, option)
        case input: CommandSurfaceItem.InputItem   => onFrontend.row(FrontendSupport.GuiOnly, input)
        case other                                 => Some(other)
    rows(editable.cursor) ++
      List(editable.proseFont, editable.codeFont, editable.uiFont).flatMap(rows).flatMap(guiOnly(_)) ++
      rows(editable.documentDefaults) ++ rows(editable.spellCheck)

  private def presetConfig(presetName: String, uiPresetPreviews: List[UiPreset.Preview]): Option[AppConfig] =
    uiPresetPreviews
      .find(preview => UiPreset.nameKey(preview.name) == UiPreset.nameKey(presetName))
      .flatMap(_.config)
      .orElse(UiPreset.builtIn(presetName).map(_.config))

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

  // issue #1060: only Duplicate/Rename (a new name) and Set Theme (a theme name, typed like the follow-system
  // themes) still take typed input -- Apply/Overwrite/Delete/Reset are pickers now (`presetActionOptionItem`), so they no
  // longer take a prefill via `currentValue`.
  private def withPresetInputContext(
    item: CommandSurfaceItem.InputItem,
    presetName: Option[String]
  ): CommandSurfaceItem.InputItem =
    (presetName, item.id) match
      case (Some(name), "ui-preset-duplicate" | "ui-preset-rename" | "ui-preset-set-theme") =>
        item.copy(currentValue = s"$name -> ")
      case _ => item
