package com.serenity.command

import com.serenity.config.*
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.ui.presets.UiPreset

/** `CommandRunner` methods for the overlay's lifecycle -- activating and deactivating it, rebuilding its config-
  * derived state, and the palette's visible-window computation. Split out of `CommandRunner` to keep both under the
  * architecture size targets -- see that class's doc.
  */
private[command] trait CommandRunnerLifecycle:
  self: CommandRunner =>

  /** Activate the command runner with given registry and config.
    *
    * `isTuiMode` is not carried by `config` (see `AppState.Runtime.isTuiMode`'s doc) -- callers pass it separately from
    * `state.runtime.isTuiMode` so settings rendering can hide/annotate controls that are inert in cell space.
    * `keyboardFidelityTier` is likewise passed separately from `state.runtime.keyboardFidelityTier` (issue #1194).
    */
  def activate(
    registry: CommandRegistry,
    config: AppConfig,
    isTuiMode: Boolean = false,
    keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full
  ): CommandRunner =
    copy(
      isActive = true,
      surface = CommandRunnerSurface.Palette(CommandPaletteState(filteredCommands = registry.getAllCommands)),
      optionSelections = CommandRunner.defaultOptionSelections(config),
      inputItems = CommandRunner.buildInputItems(config),
      commandBindings = CommandRunner.commandBindings(config),
      isTuiMode = isTuiMode,
      keyboardFidelityTier = keyboardFidelityTier,
      cursorInfoBarSegments = config.cursorInfoBarSegments
    ).syncEditMode

  /** Rebuild input items from a new config (called after a setting is applied) */
  def updateInputItems(config: AppConfig): CommandRunner =
    copy(
      inputItems = CommandRunner.buildInputItems(config),
      optionSelections = CommandRunner.defaultOptionSelections(config),
      commandBindings = CommandRunner.commandBindings(config),
      cursorInfoBarSegments = config.cursorInfoBarSegments
    ).syncEditMode.normalizeSubmenuEditMode

  def withUiPresetNames(names: List[String]): CommandRunner =
    withUiPresetPreviews(CommandRunnerSettingsItems.normalizedUiPresetNames(names).map(UiPreset.Preview.fromName))

  def withUiPresetPreviews(previews: List[UiPreset.Preview]): CommandRunner =
    copy(uiPresetPreviews =
      CommandRunnerSettingsItems.normalizedUiPresetPreviews(previews)
    ).syncEditMode.normalizeSubmenuEditMode

  /** Deactivate the command runner */
  def deactivate: CommandRunner =
    copy(
      isActive = false,
      surface = CommandRunnerSurface.Palette(),
      optionSelections = Map.empty,
      inputItems = List.empty,
      editingItemId = None,
      editingText = "",
      recordingItemId = None,
      submenuSelections = Map.empty,
      uiPresetPreviews = Nil,
      editingPresetName = None,
      cursorInfoBarSegments = Nil
    )

  /** Enter edit mode on the currently selected InputItem, or clear edit state otherwise */
  def syncEditMode: CommandRunner =
    selectedItem match
      case Some(item: CommandSurfaceItem.InputItem) if editingItemId.contains(item.id) =>
        this
      case Some(item: CommandSurfaceItem.InputItem) =>
        copy(editingItemId = Some(item.id), editingText = item.currentValue)
      case _ =>
        copy(editingItemId = None, editingText = "")

  def normalizeSubmenuEditMode: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        val items   = filteredPageItems(surface.current, submenuItems(groupId))
        val stillEditingAnExistingItem = surface.current match
          case editing: SettingsPage.Editing =>
            items.exists {
              case item: CommandSurfaceItem.InputItem => item.id == editing.itemId
              case _                                  => false
            }
          case _: SettingsPage.Group => false
        if stillEditingAnExistingItem then this
        else
          withDrilledSettingsSurface(
            surface.copy(current =
              SettingsPage.Group(groupId, pageSelectedIndex(surface.current), surface.current.searchTerm)
            )
          )
      case None =>
        this

  def updateSubmenuSearch(term: String): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        // Searching always exits edit mode and resets the index, and is always scoped to a Group page.
        withDrilledSettingsSurface(surface.copy(current = SettingsPage.Group(surface.current.groupId, 0, term)))
      case None =>
        this

  /** Get commands to display based on selected index and viewport */
  def visibleCommands: List[Command] =
    val visibleCount = 5
    val items        = visibleItems
    if items.length <= visibleCount then items.collect { case CommandSurfaceItem.CommandItem(command) => command }
    else
      val halfVisible  = visibleCount / 2
      val targetOffset = selectedIndex - halfVisible
      val offset       = math.max(0, math.min(targetOffset, items.length - visibleCount))
      items.slice(offset, offset + visibleCount).collect { case CommandSurfaceItem.CommandItem(command) => command }

  /** Check if there are more commands beyond visible ones */
  def hasMoreCommands: Boolean = visibleItems.length > 5
