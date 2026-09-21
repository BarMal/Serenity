package com.serenity.ui.presets

import com.serenity.config.*

/** One setting a [[UiPreset]] would change, named and rendered for a UI to list as a toggle.
  *
  * `key` is a [[ConfigField.key]] for a scalar setting, or one of this module's own coarser group keys (`"theme"`,
  * `"motion"`, `"hotkey"`, ...) for a setting [[ConfigRegistry]] does not cover on its own. Either way it is stable
  * across calls, so a UI can use it to remember which changes a person toggled off.
  */
final case class PresetChange(key: String, label: String, currentValue: String, newValue: String)

/** Computes what applying a [[UiPreset]] would actually change, as a flat list a UI can render as toggles.
  *
  * A built-in workflow preset (`UiPreset.applyBuiltInWorkflowToState`) only touches the settings its workflow cares
  * about, resolved through [[UiPreset.mergeBuiltInWorkflowConfig]] -- the same resolution this reuses rather than
  * reimplementing. Any other preset (`UiPreset.applyToState`) replaces the whole config, so every differing setting is
  * in scope.
  */
object UiPresetDiff:

  def changes(
    currentConfig: AppConfig,
    currentThemeName: String,
    currentHasDockedPanels: Boolean,
    currentHasWorkspaceTree: Boolean,
    preset: UiPreset
  ): List[PresetChange] =
    val resolvedConfig =
      if isBuiltInWorkflow(preset) then UiPreset.mergeBuiltInWorkflowConfig(currentConfig, preset)
      else preset.config

    themeChange(currentThemeName, preset).toList :::
      scalarChanges(currentConfig, resolvedConfig) :::
      groupChanges(currentConfig, resolvedConfig) :::
      dockedPanelsChange(currentHasDockedPanels, preset).toList :::
      workspaceTreeChange(currentHasWorkspaceTree, preset).toList

  private def isBuiltInWorkflow(preset: UiPreset): Boolean =
    UiPreset.builtInNames.exists(name => UiPreset.nameKey(name) == UiPreset.nameKey(preset.name))

  private def themeChange(currentThemeName: String, preset: UiPreset): Option[PresetChange] =
    Option.when(currentThemeName != preset.themeName)(
      PresetChange("theme", "Theme", currentThemeName, preset.themeName)
    )

  private def scalarChanges(current: AppConfig, resolved: AppConfig): List[PresetChange] =
    ConfigRegistry.fields.flatMap(scalarChange(_, current, resolved))

  private def scalarChange(field: ConfigField[?], current: AppConfig, resolved: AppConfig): Option[PresetChange] =
    val (key, currentValue) = field.setting(current)
    val (_, resolvedValue)  = field.setting(resolved)
    Option.when(currentValue.rendered != resolvedValue.rendered)(
      PresetChange(key, labelFor(key), currentValue.rendered, resolvedValue.rendered)
    )

  /** The composite settings [[ConfigRegistry]] leaves out because they are not one key to one value (see
    * `ConfigRegistry.scala`'s comment on why). Each is reported as a single coarse entry rather than one per
    * underlying key -- a preset that changes a motion family's speed scale does not need forty toggles, one that says
    * "motion" changes does.
    */
  private def groupChanges(current: AppConfig, resolved: AppConfig): List[PresetChange] =
    List(
      groupChange("motion", "Motion & animation", ConfigGroups.motion(current), ConfigGroups.motion(resolved)),
      groupChange(
        "motion.character",
        "Character animation",
        ConfigGroups.characterAnimation(current),
        ConfigGroups.characterAnimation(resolved)
      ),
      groupChange("hotkey", "Keyboard shortcuts", ConfigGroups.hotkeys(current), ConfigGroups.hotkeys(resolved)),
      groupChange("keymap", "Focused keymap", ConfigGroups.keymaps(current), ConfigGroups.keymaps(resolved)),
      groupChange(
        "lsp",
        "Language server overrides",
        ConfigGroups.lsp(current.languageToolsConfig.lspUserConfig),
        ConfigGroups.lsp(resolved.languageToolsConfig.lspUserConfig)
      )
    ).flatten

  private def groupChange(
    key: String,
    label: String,
    currentEntries: List[(String, HoconValue)],
    resolvedEntries: List[(String, HoconValue)]
  ): Option[PresetChange] =
    val currentByKey  = currentEntries.toMap
    val resolvedByKey = resolvedEntries.toMap
    val changedKeys = (currentByKey.keySet ++ resolvedByKey.keySet).count { entryKey =>
      currentByKey.get(entryKey).map(_.rendered) != resolvedByKey.get(entryKey).map(_.rendered)
    }
    Option.when(changedKeys > 0)(
      PresetChange(
        key,
        label,
        "current settings",
        s"$changedKeys setting${if changedKeys == 1 then "" else "s"} updated"
      )
    )

  private def dockedPanelsChange(currentHasDockedPanels: Boolean, preset: UiPreset): Option[PresetChange] =
    val presetHasDockedPanels = preset.dockedPanels.nonEmpty
    Option.when(currentHasDockedPanels != presetHasDockedPanels)(
      PresetChange(
        "dockedPanels",
        "Docked panels",
        presenceLabel(currentHasDockedPanels),
        presenceLabel(presetHasDockedPanels)
      )
    )

  private def workspaceTreeChange(currentHasWorkspaceTree: Boolean, preset: UiPreset): Option[PresetChange] =
    val presetHasWorkspaceTree = preset.workspaceTree.isDefined
    Option.when(currentHasWorkspaceTree != presetHasWorkspaceTree)(
      PresetChange(
        "workspaceTree",
        "Workspace layout",
        presenceLabel(currentHasWorkspaceTree),
        presenceLabel(presetHasWorkspaceTree)
      )
    )

  private def presenceLabel(present: Boolean): String = if present then "present" else "none"

  private def labelFor(key: String): String =
    key.split("[._]").filter(_.nonEmpty).map(_.capitalize).mkString(" ")
