package com.serenity.command

import com.serenity.config.*
import com.serenity.frontend.FrontendCapabilities
import com.serenity.ui.presets.UiPreset

/** `CommandRunner` methods for the overlay's lifecycle -- activating and deactivating it, rebuilding its config-
  * derived state, and the palette's visible-window computation. Split out of `CommandRunner` to keep both under the
  * architecture size targets -- see that class's doc.
  */
private[command] trait CommandRunnerLifecycle:
  self: CommandRunner =>

  /** Activate the command runner with given registry and config.
    *
    * `capabilities` is not carried by `config` (see `AppState.Runtime.capabilities`'s doc) -- callers pass it
    * separately from `state.runtime.capabilities` so settings rendering can hide/annotate controls that are inert in
    * cell space, and `CommandRunnerReducer.assignRecordedBinding` can warn on the negotiated keyboard tier (#1194).
    */
  def activate(
    registry: CommandRegistry,
    config: AppConfig,
    capabilities: FrontendCapabilities = FrontendCapabilities.gui,
    context: CommandRunnerContext = CommandRunnerContext.empty
  ): CommandRunner =
    copy(
      isActive = true,
      surface = CommandRunnerSurface.Palette(CommandPaletteState(filteredCommands = registry.getAllCommands)),
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config, capabilities),
      commandBindings = CommandRunner.commandBindings(config),
      capabilities = capabilities,
      statusSegments = config.statusLine.segments,
      context = context
    ).withSearchCacheRefreshed.syncEditMode

  /** Rebuild input items from a new config (called after a setting is applied) */
  def updateInputItems(config: AppConfig): CommandRunner =
    copy(
      inputItems = CommandRunnerSettingsInputItems.build(config, capabilities),
      optionSelections = CommandRunnerOptionSelections.default(config),
      commandBindings = CommandRunner.commandBindings(config),
      statusSegments = config.statusLine.segments
    ).withSearchCacheRefreshed.syncEditMode.normalizeSubmenuEditMode

  def withUiPresetNames(names: List[String]): CommandRunner =
    withUiPresetPreviews(CommandRunnerSettingsItems.normalizedUiPresetNames(names).map(UiPreset.Preview.fromName))

  def withUiPresetPreviews(previews: List[UiPreset.Preview]): CommandRunner =
    copy(uiPresetPreviews =
      CommandRunnerSettingsItems.normalizedUiPresetPreviews(previews)
    ).withSearchCacheRefreshed.syncEditMode.normalizeSubmenuEditMode

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
      statusSegments = Nil,
      context = CommandRunnerContext.empty
    )

  /** Enter edit mode on the currently selected InputItem, or clear edit state otherwise */
  def syncEditMode: CommandRunner =
    selectedItem match
      case Some(item: CommandSurfaceItem.InputItem) if editingItemId.contains(item.id) =>
        this
      case Some(item: CommandSurfaceItem.InputItem) =>
        copy(editingItemId = Some(item.id), editingText = item.currentValue)
      case _ if editingItemId.isEmpty && editingText.isEmpty =>
        this
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
    if items.length <= visibleCount then items.collect { case CommandSurfaceItem.CommandItem(command, _) => command }
    else
      val halfVisible  = visibleCount / 2
      val targetOffset = selectedIndex - halfVisible
      val offset       = math.max(0, math.min(targetOffset, items.length - visibleCount))
      items.slice(offset, offset + visibleCount).collect { case CommandSurfaceItem.CommandItem(command, _) => command }

  /** Check if there are more commands beyond visible ones */
  def hasMoreCommands: Boolean = visibleItems.length > 5
