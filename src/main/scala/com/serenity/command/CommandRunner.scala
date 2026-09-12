package com.serenity.command

import com.serenity.config.*
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.presets.UiPreset

/** State for the command runner overlay */
final case class CommandRunner(
    isActive: Boolean,
    // The former mode-flag grab-bag (`mode`/`isSettingsSurface`, `searchTerm`, `selectedIndex`, `filteredCommands`,
    // `activeCategory`, `activeSettingsSurface`) lives entirely inside `surface` now (issue #931, Stage 2) -- see
    // `CommandRunnerSurface`'s own doc. `searchTerm`/`selectedIndex`/`filteredCommands`/`activeSettingsSurface`/
    // `isSettingsSurface` survive below as read-only derived accessors so external readers are unaffected; there is
    // no `activeCategory` accessor -- category browsing is retired outright, not migrated anywhere.
    surface: CommandRunnerSurface = CommandRunnerSurface.Palette(),
    optionSelections: Map[String, Int] = Map.empty,
    inputItems: List[CommandSurfaceItem.InputItem] = List.empty,
    editingItemId: Option[String] = None,
    editingText: String = "",
    recordingItemId: Option[String] = None,
    submenuSelections: Map[String, Int] = Map.empty,
    statusMessage: Option[String] = None,
    uiPresetPreviews: List[UiPreset.Preview] = Nil,
    editingPresetName: Option[String] = None,
    commandBindings: Map[String, String] = Map.empty,
    isTuiMode: Boolean = false,
    // Never carried by `config` (see `AppState.Runtime.keyboardFidelityTier`'s doc) -- callers pass it separately from
    // `state.runtime.keyboardFidelityTier`, mirroring `isTuiMode` above, so `CommandRunnerReducer.assignRecordedBinding`
    // can warn when a just-recorded binding can't actually fire at the currently negotiated tier (issue #1194).
    keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full,
    // Defaults to real installed fonts (`FontLoader.FontFamilyCatalog.system`); tests that search the settings tree
    // override this with a deterministic catalog so results don't depend on what's installed on the machine running
    // them -- see `FontLoader.FontFamilyCatalog`'s doc.
    fontFamilies: FontLoader.FontFamilyCatalog = FontLoader.FontFamilyCatalog.system,
    // The cursor info bar segments' actual current order, refreshed alongside `optionSelections` in `activate`/
    // `updateInputItems` -- threaded into `settingsGroups` so its reorder commands reflect it (issue #1298).
    cursorInfoBarSegments: List[CursorInfoBarSegment] = Nil,
    // issue #1048: MRU (most-recently-used) tracking for palette commands -- keyed by `Command.name`, valued by an
    // incrementing "recency generation" (higher = used more recently), bumped by `recordCommandUsage` whenever a
    // command executes from the palette. A generation counter rather than wall-clock time: recency-*ordering* is all
    // ranking needs, and it keeps this pure and IO-free. In-session only (not persisted across restarts) -- issue
    // #1049's empty-query "recents" view is expected to read this same map, not a separate one.
    commandUsage: Map[String, Int] = Map.empty
) extends CommandRunnerSubmenuEditing
    with CommandRunnerLifecycle
    with CommandRunnerSettingsSearch:

  def isSettingsSurface: Boolean = surface match
    case _: CommandRunnerSurface.Settings => true
    case _: CommandRunnerSurface.Palette  => false

  def activeSettingsSurface: Option[SettingsSurfaceState] = surface match
    case CommandRunnerSurface.Settings(_, drilled) => drilled
    case CommandRunnerSurface.Palette(_)           => None

  def searchTerm: String              = rootState.searchTerm
  def selectedIndex: Int              = rootState.selectedIndex
  def filteredCommands: List[Command] = rootState.filteredCommands

  /** The active root's search/select state -- the palette's if `surface` is `Palette`, the settings surface's top-level
    * one otherwise (regardless of whether a group is drilled into on top of it; see `CommandRunnerSurface`).
    */
  private[command] def rootState: CommandPaletteState = surface match
    case CommandRunnerSurface.Palette(state)    => state
    case CommandRunnerSurface.Settings(root, _) => root

  private[command] def withRootSelectedIndex(index: Int): CommandRunner =
    val updatedSurface = surface match
      case CommandRunnerSurface.Palette(state) => CommandRunnerSurface.Palette(state.copy(selectedIndex = index))
      case CommandRunnerSurface.Settings(root, drilled) =>
        CommandRunnerSurface.Settings(root.copy(selectedIndex = index), drilled)
    copy(surface = updatedSurface)

  /** Replaces the drilled-in page, preserving the current root (whichever it is) and `drilled`'s own history -- the one
    * place nearly every submenu-mutating method below bottoms out. Public: a couple of `StateManagerEffectHandlers`
    * call sites (conflict messaging, focusing a just-created preset's editing group) need to set a drilled page
    * directly, the same way, from outside this class.
    */
  def withDrilledSettingsSurface(updated: SettingsSurfaceState): CommandRunner =
    copy(surface = CommandRunnerSurface.Settings(root = rootState, drilled = Some(updated)))

  def bindingFor(command: Command): Option[String] =
    commandBindings.get(command.name)

  lazy val visibleItems: List[CommandSurfaceItem] =
    surface match
      case _: CommandRunnerSurface.Settings => settingsSurfaceItems
      case CommandRunnerSurface.Palette(state) =>
        val commandItems = state.filteredCommands.map(CommandSurfaceItem.CommandItem(_))
        // Category tabs are retired (issue #931): an empty query is just every command, no category to default to.
        // Settings are still reachable here -- via search, below -- exactly as issue #931's "fold into text search"
        // intends; there is just no longer a separate navigation mode for it.
        //
        // issue #1049: opening to a raw registry-order list showed an arbitrary alphabetical-ish top (whatever
        // happens to be first in `defaultCommands`) rather than anything personalized. A stable sort by MRU
        // recency (issue #1048's `commandUsage`) puts recently/frequently-used commands first while leaving every
        // never-used command in its original relative order -- so a fresh session (empty `commandUsage`) still
        // shows the exact same "sensible default set" it always has.
        if state.searchTerm.isEmpty then
          commandItems.sortBy(item => -commandUsage.getOrElse(item.command.name, 0))
        else
          val (strongCommandMatches, remainingCommandMatches) =
            commandItems.partition(item => CommandRunnerSearch.isStrongCommandMatch(item.command, state.searchTerm))
          val (exactCommandMatches, remainingStrongCommandMatches) =
            strongCommandMatches.partition(item =>
              CommandRunnerSearch.isExactCommandMatch(item.command, state.searchTerm)
            )
          val settingsMatches = matchingSettingsResults(state.searchTerm)
          val (exactSettingsMatches, remainingSettingsMatches) =
            settingsMatches.partition(item =>
              CommandRunnerSearch.isExactSettingsTarget(
                item,
                CommandRunnerSearch.normalizedSearchTerm(state.searchTerm)
              )
            )
          exactCommandMatches ++ exactSettingsMatches ++ remainingStrongCommandMatches ++ remainingSettingsMatches ++
            remainingCommandMatches

  def selectedItem: Option[CommandSurfaceItem] =
    visibleItems.lift(selectedIndex)

  /** Update search term and filter commands. No longer scoped by category (issue #931: category tabs are retired) -- an
    * empty term is every registered command. Works for either root (the palette's or the settings surface's) --
    * whichever `surface` currently is -- and always clears any drilled-in page, since typing at the root always means
    * "search the root", never "keep editing a nested page" (that goes through `updateSubmenuSearch` instead).
    */
  def updateSearchTerm(term: String)(using registry: CommandRegistry): CommandRunner =
    val filtered =
      if term.isEmpty then registry.getAllCommands
      // issue #1048: `searchCommands` already ranks by fuzzy relevance; re-sorting (stably) by recency on top of
      // that lets a recently-used command float above an equally (or less) relevant one without ever displacing a
      // clearly stronger match, since a `sortBy` is stable across ties in `-commandUsage`.
      else registry.searchCommands(term, maxResults = 50).sortBy(command => -commandUsage.getOrElse(command.name, 0))
    val updatedState = CommandPaletteState(term, 0, filtered)
    val updatedSurface = surface match
      case CommandRunnerSurface.Palette(_)     => CommandRunnerSurface.Palette(updatedState)
      case CommandRunnerSurface.Settings(_, _) => CommandRunnerSurface.Settings(updatedState, None)
    copy(surface = updatedSurface, recordingItemId = None, statusMessage = None)

  /** Move selection up or down, with wrapping */
  def moveSelection(delta: Int): CommandRunner =
    val itemCount = visibleItems.size
    if itemCount == 0 then this
    else
      val newIndex     = (selectedIndex + delta) % itemCount
      val wrappedIndex = if newIndex < 0 then itemCount + newIndex else newIndex
      withRootSelectedIndex(wrappedIndex).syncEditMode

  def selectedCommand: Option[Command] =
    selectedItem.collect { case CommandSurfaceItem.CommandItem(command) => command }

  /** issue #1048: record a command's execution for MRU ranking -- the new generation is always one past every
    * generation recorded so far, so the command just run is always the most recent regardless of how many others
    * have run before it.
    */
  def recordCommandUsage(name: String): CommandRunner =
    val nextGeneration = commandUsage.values.maxOption.getOrElse(0) + 1
    copy(commandUsage = commandUsage + (name -> nextGeneration))

  lazy val settingsGroups: List[CommandSurfaceItem.GroupItem] =
    CommandRunnerSettingsGroups.build(
      optionSelections = optionSelections,
      inputItems = inputItems,
      uiPresetPreviews = uiPresetPreviews,
      editingPresetName = editingPresetName,
      isTuiMode = isTuiMode,
      fontFamilies = fontFamilies,
      cursorInfoBarSegments = cursorInfoBarSegments
    )

  def openSettings: CommandRunner =
    copy(surface = CommandRunnerSurface.Settings(), statusMessage = None)

  /** Both the Settings-tab-in-palette and dedicated Settings entry points render a settings group through these three
    * methods on the one `CommandPalette` surface (issue #1059) -- there is no second surface to desync from.
    */
  def settingsSurfaceItems: List[CommandSurfaceItem] =
    surface match
      case CommandRunnerSurface.Settings(_, Some(drilled)) =>
        filteredPageItems(drilled.current, submenuItems(drilled.current.groupId))
      case CommandRunnerSurface.Settings(root, None) if root.searchTerm.nonEmpty =>
        matchingSettingsResults(root.searchTerm)
      case CommandRunnerSurface.Settings(_, None) => settingsGroups
      case CommandRunnerSurface.Palette(_)        => Nil

  def settingsSurfaceSelectedIndex: Int =
    surface match
      case CommandRunnerSurface.Settings(_, Some(drilled)) => pageSelectedIndex(drilled.current)
      case CommandRunnerSurface.Settings(root, None)       => root.selectedIndex
      case CommandRunnerSurface.Palette(state)             => state.selectedIndex

  def settingsSurfaceBreadcrumbLabels: List[String] =
    activeSettingsSurface match
      case Some(surface) => "Settings" :: submenuBreadcrumbLabels(surface.current.groupId)
      case None          => List("Settings")

  def updateSettingsSearch(term: String)(using registry: CommandRegistry): CommandRunner =
    updateSearchTerm(term)

  def enterSelectedGroup: CommandRunner =
    selectedItem match
      case Some(setting: CommandSurfaceItem.SettingSearchItem) =>
        val items         = submenuItems(setting.targetGroupId)
        val selectedIndex = items.indexWhere(_.id == setting.targetItemId).max(0)
        val ancestorIds   = preferredAncestorGroupIds(setting.targetGroupId)
        copy(surface =
          CommandRunnerSurface.Settings(
            root = rootState,
            drilled = Some(
              SettingsSurfaceState(
                SettingsPage.Group(setting.targetGroupId, selectedIndex),
                ancestorPagesFor(ancestorIds)
              )
            )
          )
        )
      case Some(group: CommandSurfaceItem.GroupItem) =>
        val carriedSearchTerm = submenuSearchTermFor(group)
        val rememberedIndex   = if carriedSearchTerm.nonEmpty then 0 else submenuSelections.getOrElse(group.id, 0)
        val ancestorIds       = preferredAncestorGroupIds(group.id)
        val editContext =
          group.id match
            case "settings-preset-edit"   => presetEditContextName
            case "settings-preset-create" => None
            case _                        => editingPresetName
        copy(
          surface = CommandRunnerSurface.Settings(
            root = rootState,
            drilled = Some(
              SettingsSurfaceState(
                SettingsPage.Group(group.id, rememberedIndex, carriedSearchTerm),
                ancestorPagesFor(ancestorIds)
              )
            )
          ),
          editingPresetName = editContext
        )
      case _ => this

  /** `exitSubmenuToPreview`'s job is not a plain stack pop: it re-points the revealed parent page at the child we just
    * left (so the capped group preview shows under the right row), overriding whatever `selectedIndex` that ancestor
    * page carried from when it was pushed. Reading the parent id straight off `surface.ancestors.headOption` (rather
    * than a separately tracked `parentGroupId` field) makes the once-real self-referential-parent bug (see the
    * migration report) structurally impossible: there is no second copy of "what's my parent" left to drift.
    *
    * Popping the *last* level always lands on the settings-root view (`Settings(root, drilled = None)`), even if the
    * group was reached via a `Palette` search rather than `CommandRunner.openSettings` -- see `CommandRunnerSurface`'s
    * doc for why that's a deliberate simplification, not a bug.
    */
  def exitSubmenuToPreview: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        surface.ancestors match
          case parent :: _ =>
            val parentId    = parent.groupId
            val parentItems = submenuItems(parentId)
            val parentIndex =
              parentItems.indexWhere(_.id == groupId) match
                case -1    => submenuSelections.getOrElse(parentId, 0)
                case index => index
            copy(
              submenuSelections =
                submenuSelections + (groupId -> pageSelectedIndex(surface.current)) + (parentId -> parentIndex),
              surface = CommandRunnerSurface.Settings(
                root = rootState,
                drilled = surface.pop.map(popped => popped.copy(current = SettingsPage.Group(parentId, parentIndex)))
              )
            )
          case Nil =>
            copy(
              submenuSelections = submenuSelections + (groupId -> pageSelectedIndex(surface.current)),
              surface = CommandRunnerSurface.Settings(root = rootState, drilled = None)
            )
      case None =>
        this

  def submenuItems(groupId: String): List[CommandSurfaceItem] =
    submenuGroup(groupId).map(_.children).getOrElse(Nil)

  def submenuGroup(groupId: String): Option[CommandSurfaceItem.GroupItem] =
    findGroup(groupId, settingsGroups)

  private def findGroup(
    groupId: String,
    groups: List[CommandSurfaceItem.GroupItem]
  ): Option[CommandSurfaceItem.GroupItem] =
    groups
      .collectFirst { case group if group.id == groupId => group }
      .orElse(
        groups
          .flatMap(_.children.collect { case group: CommandSurfaceItem.GroupItem => group })
          .view
          .flatMap(group => findGroup(groupId, List(group)))
          .headOption
      )

  private def preferredAncestorGroupIds(groupId: String): List[String] =
    groupPaths(groupId, settingsGroups)
      .sortBy(path =>
        if path.contains("settings-preset-edit") then 0
        else if path.contains("settings-preset-create") then 1
        else 2
      )
      .headOption
      .map(_.dropRight(1))
      .getOrElse(Nil)

  /** `ancestorGroupIds`-shaped (root-first) group ids as `SettingsSurfaceState` ancestor pages (nearest-first), each
    * restored at its remembered `submenuSelections` index.
    */
  private def ancestorPagesFor(ancestorIds: List[String]): List[SettingsPage] =
    ancestorIds.reverse.map(id => SettingsPage.Group(id, submenuSelections.getOrElse(id, 0)))

  /** A page's item list, filtered by its `searchTerm` extension so it filters identically whether the page is `Group`
    * or `Editing`.
    */
  private[command] def filteredPageItems(
    page: SettingsPage,
    items: List[CommandSurfaceItem]
  ): List[CommandSurfaceItem] =
    val lowerTerm = page.searchTerm.trim.toLowerCase
    if lowerTerm.isEmpty then items
    else items.filter(_.searchText.toLowerCase.contains(lowerTerm))

  /** The list index a page corresponds to. `Group` carries one directly; `Editing` doesn't (it names its item by id,
    * not position), so it's recovered by looking the item up in the same filtered list `beginSubmenuEditMode` read it
    * from -- mirroring how `enterSelectedGroup` recovers a search-jump's index via `indexWhere(_.id == ...).max(0)`.
    */
  private[command] def pageSelectedIndex(page: SettingsPage): Int =
    page match
      case group: SettingsPage.Group => group.selectedIndex
      case editing: SettingsPage.Editing =>
        filteredPageItems(editing, submenuItems(editing.groupId)).indexWhere(_.id == editing.itemId).max(0)

  private def groupPaths(
    groupId: String,
    groups: List[CommandSurfaceItem.GroupItem]
  ): List[List[String]] =
    groups.flatMap { group =>
      val current = Option.when(group.id == groupId)(List(group.id)).toList
      val childGroups = group.children.collect {
        case child: CommandSurfaceItem.GroupItem =>
          child
      }
      current ++ groupPaths(groupId, childGroups).map(group.id :: _)
    }

  def focusedSubmenuItems: List[CommandSurfaceItem] =
    activeSettingsSurface.toList.flatMap(surface =>
      filteredPageItems(surface.current, submenuItems(surface.current.groupId))
    )

  def submenuBreadcrumbLabels(groupId: String): List[String] =
    activeSettingsSurface match
      case Some(surface) if surface.current.groupId == groupId && surface.ancestors.nonEmpty =>
        // `ancestors` is nearest-first; breadcrumbs read root-first, so reverse it back.
        (surface.ancestors.reverse.map(_.groupId) :+ groupId).flatMap(id => submenuGroup(id).map(_.label))
      case _ =>
        submenuGroup(groupId).map(_.label).toList

  def settingsGroupBreadcrumbLabels(groupId: String): List[String] =
    val ancestorIds = preferredAncestorGroupIds(groupId)
    val groupIds    = if ancestorIds.isEmpty then List(groupId) else ancestorIds :+ groupId
    groupIds.flatMap(id => submenuGroup(id).map(_.label))

object CommandRunner:

  private[command] def commandBindings(config: AppConfig): Map[String, String] =
    Map(
      "save"                  -> HotkeyAction.Save,
      "save-as"               -> HotkeyAction.SaveAs,
      "open"                  -> HotkeyAction.OpenFile,
      "file-search"           -> HotkeyAction.FileSearch,
      "quit"                  -> HotkeyAction.Quit,
      "new"                   -> HotkeyAction.NewTab,
      "next-tab"              -> HotkeyAction.NextTab,
      "previous-tab"          -> HotkeyAction.PreviousTab,
      "close"                 -> HotkeyAction.CloseTab,
      "split-pane-horizontal" -> HotkeyAction.SplitPaneHorizontal,
      "split-pane-vertical"   -> HotkeyAction.SplitPaneVertical,
      "close-pane"            -> HotkeyAction.ClosePane,
      "find"                  -> HotkeyAction.Find,
      "replace"               -> HotkeyAction.Replace,
      "copy"                  -> HotkeyAction.Copy,
      "cut"                   -> HotkeyAction.Cut,
      "paste"                 -> HotkeyAction.Paste,
      "select-all"            -> HotkeyAction.SelectAll,
      "undo"                  -> HotkeyAction.Undo,
      "redo"                  -> HotkeyAction.Redo,
      "goto-line"             -> HotkeyAction.GoToLine
    ).flatMap {
      case (commandName, action) =>
        config.inputConfig.hotkeyConfig.bindingsFor(action).headOption.map(trigger => commandName -> trigger.render)
    }

  /** Empty/inactive command runner */
  def empty: CommandRunner = CommandRunner(isActive = false)

  /** Create command runner with specific commands for testing */
  def withCommands(commands: List[Command]): CommandRunner =
    CommandRunner(
      isActive = false,
      surface = CommandRunnerSurface.Palette(CommandPaletteState(filteredCommands = commands))
    )
