package com.serenity.ui.presets

import com.serenity.config.*
import com.serenity.state.models.AppState
import com.serenity.ui.theme.Theme

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

  /** Applies only the changes named by `selectedKeys` (`PresetChange.key`s from [[changes]]) rather than the whole
    * preset -- the toggle-off path of the preset diff-toggle UI. `theme` is the already-loaded `Theme` for
    * `preset.themeName`, exactly as `UiPreset.applyToState`/`applyBuiltInWorkflowToState` expect it; it is only
    * actually used when `"theme"` is selected.
    *
    * Docked panels and workspace layout (`"dockedPanels"`/`"workspaceTree"`) are the one pair applied together,
    * all-or-nothing: `UiPreset.applyToState`'s panel/tree restore (pruning, redocking, the editor-pane target) is one
    * intricate, mutually-dependent operation with no smaller unit to select within it, so the existing full apply
    * runs when either is selected and is skipped entirely -- leaving the current layout untouched -- when neither is.
    * Every other selected key is layered on top of that result's config afterward, so a layout-only selection still
    * gets exactly the config values it asked for, not the preset's full resolved config.
    */
  def applySelected(state: AppState, theme: Theme, preset: UiPreset, selectedKeys: Set[String]): AppState =
    val current  = state.persisted.config
    val builtIn  = isBuiltInWorkflow(preset)
    val resolved = if builtIn then UiPreset.mergeBuiltInWorkflowConfig(current, preset) else preset.config

    val selectedConfig = mergeSelected(current, resolved, selectedKeys)
    val selectedTheme  = if selectedKeys.contains("theme") then theme else state.persisted.theme
    val appliesLayout  = selectedKeys.contains("dockedPanels") || selectedKeys.contains("workspaceTree")

    val base =
      if !appliesLayout then state
      else if builtIn then UiPreset.applyBuiltInWorkflowToState(preset, state, selectedTheme)
      else UiPreset.applyToState(preset, state, selectedTheme)

    base.copy(persisted = base.persisted.copy(config = selectedConfig, theme = selectedTheme))

  private def mergeSelected(current: AppConfig, resolved: AppConfig, selectedKeys: Set[String]): AppConfig =
    val withScalars = ConfigRegistry.fields.foldLeft(current) { (acc, field) =>
      if selectedKeys.contains(field.key) then field.restoreDefault(acc, resolved) else acc
    }
    val withMotion = if selectedKeys.contains("motion") then applyMotionGroup(withScalars, resolved) else withScalars
    val withCharacterAnimation =
      if selectedKeys.contains("motion.character") then applyCharacterAnimationGroup(withMotion, resolved)
      else withMotion
    val withHotkeys =
      if selectedKeys.contains("hotkey") then applyHotkeyGroup(withCharacterAnimation, resolved)
      else withCharacterAnimation
    val withKeymaps = if selectedKeys.contains("keymap") then applyKeymapGroup(withHotkeys, resolved) else withHotkeys
    if selectedKeys.contains("lsp") then applyLspGroup(withKeymaps, resolved) else withKeymaps

  private def applyMotionGroup(base: AppConfig, resolved: AppConfig): AppConfig =
    base.withSurfaceConfig(
      base.surfaceConfig.copy(
        motionPreset = resolved.surfaceConfig.motionPreset,
        elementTransitionSpeedScale = resolved.surfaceConfig.elementTransitionSpeedScale,
        editorTextTransitionSpeedScale = resolved.surfaceConfig.editorTextTransitionSpeedScale,
        commandRunnerTransitionSpeedScale = resolved.surfaceConfig.commandRunnerTransitionSpeedScale,
        uiTransitionSpeedScale = resolved.surfaceConfig.uiTransitionSpeedScale,
        cursorTransitionSpeedScale = resolved.surfaceConfig.cursorTransitionSpeedScale,
        commandRunnerAnimation = resolved.surfaceConfig.commandRunnerAnimation,
        uiAnimation = resolved.surfaceConfig.uiAnimation,
        editorInsertionTransitionKind = resolved.surfaceConfig.editorInsertionTransitionKind,
        commandRunnerTransitionKind = resolved.surfaceConfig.commandRunnerTransitionKind,
        panelOpenTransitionKind = resolved.surfaceConfig.panelOpenTransitionKind,
        panelCloseTransitionKind = resolved.surfaceConfig.panelCloseTransitionKind,
        motionConfiguration = resolved.surfaceConfig.motionConfiguration
      )
    )

  private def applyCharacterAnimationGroup(base: AppConfig, resolved: AppConfig): AppConfig =
    base.withEditorConfig(base.editorConfig.copy(characterAnimation = resolved.editorConfig.characterAnimation))

  private def applyHotkeyGroup(base: AppConfig, resolved: AppConfig): AppConfig =
    base.withHotkeyConfig(resolved.inputConfig.hotkeyConfig)

  private def applyKeymapGroup(base: AppConfig, resolved: AppConfig): AppConfig =
    base.withFocusedKeymapConfig(resolved.inputConfig.focusedKeymapConfig)

  private def applyLspGroup(base: AppConfig, resolved: AppConfig): AppConfig =
    base.withLspUserConfig(resolved.languageToolsConfig.lspUserConfig)
