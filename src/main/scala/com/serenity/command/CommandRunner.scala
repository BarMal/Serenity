package com.serenity.command

import java.util.Locale

import com.serenity.config.*
import com.serenity.keystroke.{KeyStrokeInfo, KeyboardFidelityTier}
import com.serenity.ui.presets.UiPreset

final case class CommandRunnerSubmenuState(
    groupId: String,
    selectedIndex: Int = 0,
    editingItemId: Option[String] = None,
    editingText: String = "",
    recordingItemId: Option[String] = None,
    pendingRecordedBinding: Option[(KeyStrokeInfo, Long)] = None,
    pendingGlobalHotkeyConflict: Option[(HotkeyAction, String)] = None,
    pendingFocusedKeymapConflict: Option[(String, String)] = None,
    searchTerm: String = "",
    parentGroupId: Option[String] = None,
    ancestorGroupIds: List[String] = Nil
):
  def selectedItem(items: List[CommandSurfaceItem]): Option[CommandSurfaceItem] =
    items.lift(selectedIndex)

  def filteredItems(items: List[CommandSurfaceItem]): List[CommandSurfaceItem] =
    val lowerTerm = searchTerm.trim.toLowerCase
    if lowerTerm.isEmpty then items
    else items.filter(_.searchText.toLowerCase.contains(lowerTerm))

  def selectedItemFromAll(items: List[CommandSurfaceItem]): Option[CommandSurfaceItem] =
    selectedItem(filteredItems(items))

/** In-flight keybinding recording for a settings item, and any conflict it surfaced.
  *
  * Consolidates the four `recordingItemId`/`pending*` fields carried per-item on `CommandRunnerSubmenuState` today into
  * one value scoped to the page that owns the recording (see `SettingsPage.Editing.recording`), for issue #1059.
  */
final case class RecordingState(
    itemId: String,
    pendingRecordedBinding: Option[(KeyStrokeInfo, Long)] = None,
    pendingGlobalHotkeyConflict: Option[(HotkeyAction, String)] = None,
    pendingFocusedKeymapConflict: Option[(String, String)] = None
)

/** One page of the unified settings navigation stack (issue #1059).
  *
  * `Group` is a browsable list of a group's children (with its own local search); `Editing` is a single input item with
  * an in-progress text edit or keybinding recording. Both carry `groupId` so a page always knows which group it belongs
  * to without a separate ancestor-lookup field.
  */
enum SettingsPage:
  case Group(groupId: String, selectedIndex: Int = 0, searchTerm: String = "")
  case Editing(groupId: String, itemId: String, draftText: String, recording: Option[RecordingState] = None)

object SettingsPage:

  /** The group a page belongs to, regardless of which case it is. A plain `extension` rather than a member of the enum
    * itself, so each case stays a normal case class -- notably keeping its compiler-synthesized `copy` (an inherited
    * abstract `groupId` overridden by a same-named case parameter suppresses that synthesis under Scala 3, so this
    * sidesteps it).
    */
  extension (page: SettingsPage)

    def groupId: String = page match
      case Group(id, _, _)      => id
      case Editing(id, _, _, _) => id

/** The unified settings surface's navigation state: an explicit page stack (issue #1059).
  *
  * `current` is the page on screen; `ancestors` are the pages to return to, nearest first. Splitting the two (rather
  * than one `List[SettingsPage]` checked non-empty at runtime) makes "the stack always has a current page" a structural
  * guarantee instead of an invariant that has to be defended with a partial `head`/`tail` -- closing the settings
  * surface entirely is represented by the absence of a `SettingsSurfaceState`, not by an empty one.
  */
final case class SettingsSurfaceState(current: SettingsPage, ancestors: List[SettingsPage] = Nil):

  /** `current` followed by `ancestors`, nearest first -- for callers that want the whole stack as a list. */
  def stack: List[SettingsPage] = current :: ancestors

  def depth: Int = ancestors.size + 1

  /** Enter a child page, pushing it above the current one. */
  def push(page: SettingsPage): SettingsSurfaceState = SettingsSurfaceState(page, current :: ancestors)

  /** Leave the current page, revealing its parent. `None` at the root, where there is nothing left to pop -- callers
    * (Escape at depth 1) treat that as "close the whole surface" instead.
    */
  def pop: Option[SettingsSurfaceState] = ancestors match
    case parent :: rest => Some(SettingsSurfaceState(parent, rest))
    case Nil            => None

object SettingsSurfaceState:

  /** Reconstruct a state from a full stack (nearest-first, `head` = current). `Nil` is rejected since a
    * `SettingsSurfaceState` always has a current page -- see the class doc.
    */
  def apply(stack: List[SettingsPage]): SettingsSurfaceState =
    require(stack.nonEmpty, "SettingsSurfaceState requires at least one page")
    (stack: @unchecked) match
      case head :: rest => SettingsSurfaceState(head, rest)

  /** Escape always means "up one level, or close if there is no level left" -- never a mix of that and text-clearing,
    * unlike the Backspace overload it replaces (issue #1059).
    */
  enum EscapeOutcome:
    case Popped(state: SettingsSurfaceState)
    case CloseSurface

  def escape(state: SettingsSurfaceState): EscapeOutcome =
    state.pop match
      case Some(popped) => EscapeOutcome.Popped(popped)
      case None         => EscapeOutcome.CloseSurface

  /** Backspace always means "delete one character of the current page's text" -- it never pops or otherwise navigates
    * the stack, and is a no-op when the current page has no text to delete (issue #1059's "Backspace is overloaded
    * across search-term / edit-text / go-up-a-level").
    */
  def deleteBackward(state: SettingsSurfaceState): SettingsSurfaceState =
    state.current match
      case page: SettingsPage.Group if page.searchTerm.nonEmpty =>
        state.copy(current = page.copy(searchTerm = page.searchTerm.dropRight(1)))
      case page: SettingsPage.Editing if page.draftText.nonEmpty =>
        state.copy(current = page.copy(draftText = page.draftText.dropRight(1)))
      case _ =>
        state

  private val MaxPreviewRows = 4

  /** Ghost-preview rows for a group nested under the selected item, capped to `MaxPreviewRows` with an overflow count
    * -- a pure function of the selected item, carrying no state of its own (issue #1059's ghost-preview submenu).
    */
  final case class PreviewRows(rows: List[String], overflowCount: Int):
    def isEmpty: Boolean = rows.isEmpty

  def previewRows(items: List[CommandSurfaceItem], selectedIndex: Int): PreviewRows =
    items.lift(selectedIndex) match
      case Some(group: CommandSurfaceItem.GroupItem) =>
        val labels = group.children.map(previewLabel)
        PreviewRows(labels.take(MaxPreviewRows), math.max(0, labels.size - MaxPreviewRows))
      case _ =>
        PreviewRows(Nil, 0)

  private def previewLabel(item: CommandSurfaceItem): String =
    item match
      case CommandSurfaceItem.CommandItem(command)    => command.label
      case item: CommandSurfaceItem.OptionItem        => item.label
      case item: CommandSurfaceItem.InputItem         => item.label
      case item: CommandSurfaceItem.SettingSearchItem => item.label
      case item: CommandSurfaceItem.GroupItem         => item.label

/** State for the command runner overlay */
final case class CommandRunner(
    isActive: Boolean,
    searchTerm: String,
    selectedIndex: Int,
    filteredCommands: List[Command],
    activeCategory: CommandCategory = CommandCategory.All,
    optionSelections: Map[String, Int] = Map.empty,
    inputItems: List[CommandSurfaceItem.InputItem] = List.empty,
    editingItemId: Option[String] = None,
    editingText: String = "",
    recordingItemId: Option[String] = None,
    submenuSelections: Map[String, Int] = Map.empty,
    previewedGroupId: Option[String] = None,
    activeSubmenu: Option[CommandRunnerSubmenuState] = None,
    statusMessage: Option[String] = None,
    uiPresetPreviews: List[UiPreset.Preview] = Nil,
    editingPresetName: Option[String] = None,
    commandBindings: Map[String, String] = Map.empty,
    mode: CommandRunnerMode = CommandRunnerMode.Palette,
    isTuiMode: Boolean = false,
    // Never carried by `config` (see `AppState.Runtime.keyboardFidelityTier`'s doc) -- callers pass it separately from
    // `state.runtime.keyboardFidelityTier`, mirroring `isTuiMode` above, so `CommandRunnerReducer.assignRecordedBinding`
    // can warn when a just-recorded binding can't actually fire at the currently negotiated tier (issue #1194).
    keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full
):

  def isSettingsSurface: Boolean = mode == CommandRunnerMode.Settings

  def bindingFor(command: Command): Option[String] =
    commandBindings.get(command.name)

  lazy val visibleItems: List[CommandSurfaceItem] =
    if isSettingsSurface then settingsSurfaceItems
    else
      val commandItems = filteredCommands.map(CommandSurfaceItem.CommandItem(_))
      if searchTerm.isEmpty then
        activeCategory match
          case CommandCategory.Settings => settingsGroups ++ commandItems
          case _                        => commandItems
      else
        val (strongCommandMatches, remainingCommandMatches) =
          commandItems.partition(item => CommandRunner.isStrongCommandMatch(item.command, searchTerm))
        val (exactCommandMatches, remainingStrongCommandMatches) =
          strongCommandMatches.partition(item => CommandRunner.isExactCommandMatch(item.command, searchTerm))
        val settingsMatches = matchingSettingsResults(searchTerm)
        val (exactSettingsMatches, remainingSettingsMatches) =
          settingsMatches.partition(item =>
            CommandRunner.isExactSettingsTarget(item, CommandRunner.normalizedSearchTerm(searchTerm))
          )
        exactCommandMatches ++ exactSettingsMatches ++ remainingStrongCommandMatches ++ remainingSettingsMatches ++
          remainingCommandMatches

  def selectedItem: Option[CommandSurfaceItem] =
    visibleItems.lift(selectedIndex)

  /** Update search term and filter commands */
  def updateSearchTerm(term: String)(using registry: CommandRegistry): CommandRunner =
    val filtered =
      if term.isEmpty then registry.commandsForCategory(activeCategory)
      else registry.searchCommands(term, maxResults = 50)
    copy(
      searchTerm = term,
      selectedIndex = 0,
      filteredCommands = filtered,
      previewedGroupId = None,
      activeSubmenu = None,
      recordingItemId = None,
      statusMessage = None
    )

  /** Move selection up or down, with wrapping */
  def moveSelection(delta: Int): CommandRunner =
    val itemCount = visibleItems.size
    if itemCount == 0 then this
    else
      val newIndex     = (selectedIndex + delta) % itemCount
      val wrappedIndex = if newIndex < 0 then itemCount + newIndex else newIndex
      copy(selectedIndex = wrappedIndex).syncEditMode

  /** Get currently selected command */
  def selectedCommand: Option[Command] =
    selectedItem.collect { case CommandSurfaceItem.CommandItem(command) => command }

  lazy val settingsGroups: List[CommandSurfaceItem.GroupItem] =
    CommandRunnerSettingsGroups.build(
      optionSelections = optionSelections,
      inputItems = inputItems,
      uiPresetPreviews = uiPresetPreviews,
      editingPresetName = editingPresetName,
      isTuiMode = isTuiMode
    )

  def openSettings: CommandRunner =
    copy(
      mode = CommandRunnerMode.Settings,
      activeCategory = CommandCategory.Settings,
      searchTerm = "",
      selectedIndex = 0,
      previewedGroupId = None,
      activeSubmenu = None,
      statusMessage = None
    )

  def settingsSurfaceItems: List[CommandSurfaceItem] =
    activeSubmenu match
      case Some(submenu)               => submenu.filteredItems(submenuItems(submenu.groupId))
      case None if searchTerm.nonEmpty => matchingSettingsResults(searchTerm)
      case None                        => settingsGroups

  def settingsSurfaceSelectedIndex: Int =
    activeSubmenu.map(_.selectedIndex).getOrElse(selectedIndex)

  def settingsSurfaceBreadcrumbLabels: List[String] =
    activeSubmenu match
      case Some(submenu) => "Settings" :: submenuBreadcrumbLabels(submenu.groupId)
      case None          => List("Settings")

  def updateSettingsSearch(term: String)(using registry: CommandRegistry): CommandRunner =
    updateSearchTerm(term)

  def previewGroup(groupId: String): CommandRunner =
    copy(previewedGroupId = Some(groupId))

  def clearGroupPreview: CommandRunner =
    copy(previewedGroupId = None, activeSubmenu = None)

  def enterSelectedGroup: CommandRunner =
    selectedItem match
      case Some(setting: CommandSurfaceItem.SettingSearchItem) =>
        val items         = submenuItems(setting.targetGroupId)
        val selectedIndex = items.indexWhere(_.id == setting.targetItemId).max(0)
        val ancestorIds   = preferredAncestorGroupIds(setting.targetGroupId)
        copy(
          previewedGroupId = Some(setting.targetGroupId),
          activeSubmenu = Some(
            CommandRunnerSubmenuState(
              setting.targetGroupId,
              selectedIndex = selectedIndex,
              parentGroupId = ancestorIds.lastOption,
              ancestorGroupIds = ancestorIds
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
          previewedGroupId = Some(group.id),
          activeSubmenu = Some(
            CommandRunnerSubmenuState(
              group.id,
              selectedIndex = rememberedIndex,
              searchTerm = carriedSearchTerm,
              parentGroupId = ancestorIds.lastOption,
              ancestorGroupIds = ancestorIds
            )
          ),
          editingPresetName = editContext
        )
      case _ => this

  def exitSubmenuToPreview: CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        submenu.parentGroupId match
          case Some(parentId) =>
            val parentItems = submenuItems(parentId)
            val parentIndex =
              parentItems.indexWhere(_.id == submenu.groupId) match
                case -1    => submenuSelections.getOrElse(parentId, 0)
                case index => index
            copy(
              submenuSelections =
                submenuSelections + (submenu.groupId -> submenu.selectedIndex) + (parentId -> parentIndex),
              activeSubmenu = Some(
                CommandRunnerSubmenuState(
                  parentId,
                  selectedIndex = parentIndex,
                  parentGroupId = submenu.ancestorGroupIds.lastOption,
                  ancestorGroupIds = submenu.ancestorGroupIds.dropRight(1)
                )
              )
            )
          case None =>
            copy(
              submenuSelections = submenuSelections + (submenu.groupId -> submenu.selectedIndex),
              activeSubmenu = None
            )
      case None =>
        copy(activeSubmenu = None)

  def previewOrFocusedGroupId: Option[String] =
    activeSubmenu.map(_.groupId).orElse(previewedGroupId)

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
    activeSubmenu.toList.flatMap(submenu => submenu.filteredItems(submenuItems(submenu.groupId)))

  def submenuBreadcrumbLabels(groupId: String): List[String] =
    activeSubmenu match
      case Some(submenu) if submenu.groupId == groupId && submenu.ancestorGroupIds.nonEmpty =>
        (submenu.ancestorGroupIds :+ groupId).flatMap(id => submenuGroup(id).map(_.label))
      case _ =>
        submenuGroup(groupId).map(_.label).toList

  def settingsGroupBreadcrumbLabels(groupId: String): List[String] =
    val ancestorIds = preferredAncestorGroupIds(groupId)
    val groupIds    = if ancestorIds.isEmpty then List(groupId) else ancestorIds :+ groupId
    groupIds.flatMap(id => submenuGroup(id).map(_.label))

  def moveSubmenuSelection(delta: Int): CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        val items = submenu.filteredItems(submenuItems(submenu.groupId))
        if items.isEmpty then this
        else
          val itemCount    = items.size
          val newIndex     = (submenu.selectedIndex + delta) % itemCount
          val wrappedIndex = if newIndex < 0 then itemCount + newIndex else newIndex
          copy(
            submenuSelections = submenuSelections + (submenu.groupId -> wrappedIndex),
            activeSubmenu = Some(
              submenu.copy(
                selectedIndex = wrappedIndex,
                editingItemId = None,
                editingText = "",
                recordingItemId = None,
                pendingRecordedBinding = None,
                pendingGlobalHotkeyConflict = None,
                pendingFocusedKeymapConflict = None
              )
            )
          )
      case None => this

  def beginSubmenuEditMode: CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        submenu.selectedItemFromAll(submenuItems(submenu.groupId)) match
          case Some(item: CommandSurfaceItem.InputItem) =>
            copy(
              activeSubmenu = Some(
                submenu.copy(
                  editingItemId = Some(item.id),
                  editingText = item.currentValue,
                  recordingItemId = None,
                  pendingRecordedBinding = None,
                  pendingGlobalHotkeyConflict = None,
                  pendingFocusedKeymapConflict = None
                )
              )
            )
          case _ =>
            this
      case None =>
        this

  def enterSelectedSubmenuGroup: CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        submenu.selectedItemFromAll(submenuItems(submenu.groupId)) match
          case Some(group: CommandSurfaceItem.GroupItem) =>
            val rememberedIndex = submenuSelections.getOrElse(group.id, 0)
            copy(
              submenuSelections = submenuSelections + (submenu.groupId -> submenu.selectedIndex),
              activeSubmenu = Some(
                CommandRunnerSubmenuState(
                  group.id,
                  selectedIndex = rememberedIndex,
                  parentGroupId = Some(submenu.groupId),
                  ancestorGroupIds = submenu.ancestorGroupIds :+ submenu.groupId
                )
              )
            )
          case _ =>
            this
      case None =>
        this

  def adjustSelectedSubmenuOption(delta: Int): CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        submenu.selectedItemFromAll(submenuItems(submenu.groupId)) match
          case Some(option: CommandSurfaceItem.OptionItem) =>
            val updatedOption = option.moveSelection(delta)
            copy(optionSelections = optionSelections + (option.id -> updatedOption.selectedIndex))
          case _ =>
            this
      case None =>
        this

  def withActiveCategory(category: CommandCategory)(using registry: CommandRegistry): CommandRunner =
    copy(
      activeCategory = category,
      selectedIndex = 0,
      previewedGroupId = None,
      activeSubmenu = None
    ).updateSearchTerm("").syncEditMode

  def switchCategory(delta: Int)(using registry: CommandRegistry): CommandRunner =
    val categories = List(
      CommandCategory.All,
      CommandCategory.File,
      CommandCategory.View,
      CommandCategory.Edit,
      CommandCategory.Project,
      CommandCategory.Settings
    )
    val currentIndex = categories.indexOf(activeCategory)
    val newIndex     = (currentIndex + delta + categories.length) % categories.length
    withActiveCategory(categories(newIndex))

  def adjustSelectedOption(delta: Int): CommandRunner =
    selectedItem match
      case Some(option: CommandSurfaceItem.OptionItem) =>
        val updatedOption = option.moveSelection(delta)
        copy(optionSelections = optionSelections + (option.id -> updatedOption.selectedIndex))
      case _ =>
        this

  def withSelectedItem(itemId: String): CommandRunner =
    visibleItems.zipWithIndex.find(_._1.id == itemId) match
      case Some((_, index)) => copy(selectedIndex = index).syncEditMode
      case None             => this

  def withSelectedVisibleIndex(index: Int): CommandRunner =
    if visibleItems.indices.contains(index) then copy(selectedIndex = index).syncEditMode
    else this

  def withSelectedFocusedSubmenuIndex(index: Int): CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        val items = submenu.filteredItems(submenuItems(submenu.groupId))
        if items.indices.contains(index) then
          copy(
            submenuSelections = submenuSelections + (submenu.groupId -> index),
            activeSubmenu = Some(
              submenu.copy(
                selectedIndex = index,
                editingItemId = None,
                editingText = "",
                recordingItemId = None,
                pendingRecordedBinding = None,
                pendingGlobalHotkeyConflict = None,
                pendingFocusedKeymapConflict = None
              )
            )
          )
        else this
      case None =>
        this

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
      searchTerm = "",
      selectedIndex = 0,
      filteredCommands = registry.commandsForCategory(activeCategory),
      optionSelections = CommandRunner.defaultOptionSelections(config),
      inputItems = CommandRunner.buildInputItems(config),
      commandBindings = CommandRunner.commandBindings(config),
      isTuiMode = isTuiMode,
      keyboardFidelityTier = keyboardFidelityTier
    ).syncEditMode

  /** Rebuild input items from a new config (called after a setting is applied) */
  def updateInputItems(config: AppConfig): CommandRunner =
    copy(
      inputItems = CommandRunner.buildInputItems(config),
      optionSelections = CommandRunner.defaultOptionSelections(config),
      commandBindings = CommandRunner.commandBindings(config)
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
      searchTerm = "",
      selectedIndex = 0,
      filteredCommands = List.empty,
      activeCategory = CommandCategory.All,
      optionSelections = Map.empty,
      inputItems = List.empty,
      editingItemId = None,
      editingText = "",
      recordingItemId = None,
      submenuSelections = Map.empty,
      previewedGroupId = None,
      activeSubmenu = None,
      uiPresetPreviews = Nil,
      editingPresetName = None
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
    activeSubmenu match
      case Some(submenu) =>
        submenu.selectedItemFromAll(submenuItems(submenu.groupId)) match
          case Some(item: CommandSurfaceItem.InputItem) if submenu.editingItemId.contains(item.id) =>
            this
          case _ =>
            copy(
              activeSubmenu = Some(
                submenu.copy(
                  editingItemId = None,
                  editingText = "",
                  recordingItemId = None,
                  pendingRecordedBinding = None,
                  pendingGlobalHotkeyConflict = None,
                  pendingFocusedKeymapConflict = None
                )
              )
            )
      case None =>
        this

  def updateSubmenuSearch(term: String): CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        val updated = submenu.copy(
          searchTerm = term,
          selectedIndex = 0,
          editingItemId = None,
          editingText = "",
          recordingItemId = None,
          pendingRecordedBinding = None,
          pendingGlobalHotkeyConflict = None,
          pendingFocusedKeymapConflict = None
        )
        copy(activeSubmenu = Some(updated))
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

  private def matchingSettingsResults(term: String): List[CommandSurfaceItem] =
    val lowerTerm = CommandRunner.normalizedSearchTerm(term)
    if lowerTerm.length < 3 then Nil
    else
      val leafResults = matchingSettingLeaves(lowerTerm)
      exactSettingsGroup(lowerTerm) match
        case Some(exactGroup) => List(exactGroup)
        case None if leafResults.exists(CommandRunner.isExactSettingsTarget(_, lowerTerm)) =>
          leafResults.filter(CommandRunner.isExactSettingsTarget(_, lowerTerm))
        case None if CommandRunner.isSpecificSettingQuery(lowerTerm) && leafResults.nonEmpty => leafResults
        case None => matchingSettingsGroups(lowerTerm)

  private def exactSettingsGroup(term: String): Option[CommandSurfaceItem.GroupItem] =
    allSettingsGroups.find { group =>
      val label = CommandRunner.normalizedSearchTerm(group.label)
      val id    = CommandRunner.normalizedSearchTerm(group.id)
      label == term || id == term
    }

  private def matchingSettingsGroups(lowerTerm: String): List[CommandSurfaceItem.GroupItem] =
    if lowerTerm.length < 3 then Nil
    else
      val matchingGroups = allSettingsGroups.zipWithIndex
        .flatMap {
          case (group, index) =>
            val groupMatch = CommandRunner.directGroupSearchText(group).contains(lowerTerm)
            val childMatch = group.children.exists {
              case _: CommandSurfaceItem.GroupItem => false
              case child                           => CommandRunner.directItemSearchText(child).contains(lowerTerm)
            }
            Option.when(groupMatch || childMatch)(group -> (settingsSearchRank(group, lowerTerm, groupMatch), index))
        }
        .sortBy { case (group, (rank, index)) => (rank, index, group.id) }
      val directGlobalGroups = matchingGroups.collect {
        case (group, _) if !group.id.startsWith("settings-preset-") && group.children.exists {
              case _: CommandSurfaceItem.GroupItem => false
              case _                               => true
            } =>
          group
      }
      if directGlobalGroups.size == 1 then directGlobalGroups
      else directGlobalGroups ++ matchingGroups.map(_._1).filterNot(group => directGlobalGroups.contains(group))

  private def matchingSettingLeaves(term: String): List[CommandSurfaceItem.SettingSearchItem] =
    val leaves           = settingLeaves
    val globalTargetIds  = leaves.filterNot(_.isPresetScoped).map(_.item.id).toSet
    val directSearchable = leaves.filter(leaf => !leaf.isPresetScoped || !globalTargetIds.contains(leaf.item.id))
    directSearchable
      .flatMap {
        case leaf =>
          val (group, item, breadcrumb) = (leaf.group, leaf.item, leaf.breadcrumb)
          CommandRunner.settingSearchRank(item, breadcrumb, term).map { rank =>
            (
              CommandSurfaceItem.SettingSearchItem(
                id = s"settings-search:${item.id}",
                targetGroupId = group.id,
                targetItemId = item.id,
                label = CommandRunner.itemLabel(item),
                breadcrumb = breadcrumb,
                effectiveValue = CommandRunner.itemEffectiveValue(item),
                sourceScope = if leaf.isPresetScoped then "Preset" else "Global",
                category = CommandCategory.Settings,
                hint = CommandRunner.itemHint(item)
              ),
              rank
            )
          }
      }
      .sortBy { case (item, rank) => (rank, item.breadcrumb, item.targetItemId) }
      .map(_._1)
      .distinctBy(_.targetItemId)
      .take(CommandRunner.MaximumSettingSearchResults)

  final private case class SettingLeaf(
      group: CommandSurfaceItem.GroupItem,
      item: CommandSurfaceItem,
      breadcrumb: String,
      isPresetScoped: Boolean
  )

  private def settingLeaves: List[SettingLeaf] =
    def loop(
      group: CommandSurfaceItem.GroupItem,
      ancestorIds: List[String],
      ancestorLabels: List[String]
    ): List[SettingLeaf] =
      group.children.flatMap {
        case child: CommandSurfaceItem.GroupItem =>
          loop(child, ancestorIds :+ group.id, ancestorLabels :+ group.label)
        case child =>
          List(
            SettingLeaf(
              group = group,
              item = child,
              breadcrumb = (("Settings" :: ancestorLabels) :+ group.label).mkString(" > "),
              isPresetScoped = ancestorIds.contains("settings-ui-presets") || group.id == "settings-ui-presets"
            )
          )
      }

    settingsGroups.flatMap(group => loop(group, Nil, Nil))

  private def settingsSearchRank(
    group: CommandSurfaceItem.GroupItem,
    term: String,
    groupMatch: Boolean
  ): Int =
    val label = group.label.toLowerCase
    if groupMatch && label == term then 0
    else if groupMatch && label.startsWith(term) then 1
    else if groupMatch then 2
    else 3

  private def submenuSearchTermFor(group: CommandSurfaceItem.GroupItem): String =
    val lowerTerm = CommandRunner.normalizedSearchTerm(searchTerm)
    if lowerTerm.length < 3 then ""
    else if CommandRunner.directGroupSearchText(group).contains(lowerTerm) then ""
    else if group.children.exists(child => CommandRunner.directItemSearchText(child).contains(lowerTerm)) then
      searchTerm
    else ""

  private def allSettingsGroups: List[CommandSurfaceItem.GroupItem] =
    def loop(groups: List[CommandSurfaceItem.GroupItem]): List[CommandSurfaceItem.GroupItem] =
      groups ++ groups.flatMap { group =>
        loop(group.children.collect { case child: CommandSurfaceItem.GroupItem => child })
      }

    loop(settingsGroups).distinctBy(_.id)

  private def presetEditContextName: Option[String] =
    CommandRunnerSettingsGroups.presetEditContextName(
      optionSelections = optionSelections,
      uiPresetPreviews = uiPresetPreviews,
      editingPresetName = editingPresetName
    )

  /** Store the focus that should be restored when runner closes */

object CommandRunner:

  private val MaximumSettingSearchResults = 10

  private def normalizedSearchTerm(term: String): String =
    term.trim
      .stripPrefix("\"")
      .stripSuffix("\"")
      .toLowerCase(Locale.ROOT)
      .replaceAll("[^\\p{L}\\p{N}]+", " ")
      .trim

  private def isSpecificSettingQuery(term: String): Boolean =
    term.split(" ").count(_.nonEmpty) > 1

  private def isExactSettingsTarget(item: CommandSurfaceItem, term: String): Boolean =
    item match
      case item: CommandSurfaceItem.SettingSearchItem =>
        val label = normalizedSearchTerm(item.label)
        val id    = normalizedSearchTerm(item.targetItemId)
        label == term || id == term
      case item: CommandSurfaceItem.GroupItem =>
        val label = normalizedSearchTerm(item.label)
        val id    = normalizedSearchTerm(item.id)
        label == term || id == term
      case _ => false

  private def settingSearchRank(item: CommandSurfaceItem, breadcrumb: String, term: String): Option[Int] =
    val label         = normalizedSearchTerm(itemLabel(item))
    val id            = normalizedSearchTerm(item.id)
    val scope         = normalizedSearchTerm(breadcrumb)
    val terms         = term.split(" ").filter(_.nonEmpty).toList
    val allTermsMatch = terms.nonEmpty && terms.forall(token => s"$label $id $scope".contains(token))
    if label == term || id == term then Some(0)
    else if label.startsWith(term) || id.startsWith(term) then Some(1)
    else if allTermsMatch then Some(2)
    else None

  private def itemLabel(item: CommandSurfaceItem): String =
    item match
      case CommandSurfaceItem.CommandItem(command)    => command.label
      case item: CommandSurfaceItem.OptionItem        => item.label
      case item: CommandSurfaceItem.InputItem         => item.label
      case item: CommandSurfaceItem.SettingSearchItem => item.label
      case item: CommandSurfaceItem.GroupItem         => item.label

  private def itemHint(item: CommandSurfaceItem): Option[String] =
    item match
      case item: CommandSurfaceItem.OptionItem        => item.hint
      case item: CommandSurfaceItem.InputItem         => Some(item.hint)
      case item: CommandSurfaceItem.SettingSearchItem => item.hint
      case item: CommandSurfaceItem.GroupItem         => item.hint
      case _: CommandSurfaceItem.CommandItem          => None

  private def itemEffectiveValue(item: CommandSurfaceItem): Option[String] =
    item match
      case item: CommandSurfaceItem.OptionItem => Some(item.selectedOption)
      case item: CommandSurfaceItem.InputItem  => Some(item.currentValue)
      case _                                   => None

  private def isStrongCommandMatch(command: Command, term: String): Boolean =
    val lowerTerm        = term.toLowerCase
    val nameLower        = command.name.toLowerCase
    val labelLower       = command.label.toLowerCase
    val descriptionLower = command.description.toLowerCase
    nameLower.startsWith(lowerTerm) ||
    labelLower.startsWith(lowerTerm) ||
    descriptionLower == lowerTerm ||
    descriptionLower.startsWith(lowerTerm)

  private def isExactCommandMatch(command: Command, term: String): Boolean =
    val normalizedTerm = normalizedSearchTerm(term)
    normalizedSearchTerm(command.name) == normalizedTerm ||
    normalizedSearchTerm(command.label) == normalizedTerm

  private[command] def directGroupSearchText(group: CommandSurfaceItem.GroupItem): String =
    normalizedSearchTerm(s"${group.id} ${group.label} ${group.hint.getOrElse("")}")

  private[command] def directItemSearchText(item: CommandSurfaceItem): String =
    item match
      case group: CommandSurfaceItem.GroupItem =>
        CommandRunner.directGroupSearchText(group)
      case other =>
        normalizedSearchTerm(s"${other.id} ${other.searchText}")

  private[command] def defaultOptionSelections(config: AppConfig): Map[String, Int] =
    CommandRunnerOptionSelections.default(config)

  private[command] def buildInputItems(config: AppConfig): List[CommandSurfaceItem.InputItem] =
    CommandRunnerSettingsInputItems.build(config)

  private def commandBindings(config: AppConfig): Map[String, String] =
    Map(
      "save"         -> HotkeyAction.Save,
      "save-as"      -> HotkeyAction.SaveAs,
      "open"         -> HotkeyAction.OpenFile,
      "file-search"  -> HotkeyAction.FileSearch,
      "quit"         -> HotkeyAction.Quit,
      "new"          -> HotkeyAction.NewTab,
      "next-tab"     -> HotkeyAction.NextTab,
      "previous-tab" -> HotkeyAction.PreviousTab,
      "close"        -> HotkeyAction.CloseTab,
      "find"         -> HotkeyAction.Find,
      "replace"      -> HotkeyAction.Replace,
      "copy"         -> HotkeyAction.Copy,
      "cut"          -> HotkeyAction.Cut,
      "paste"        -> HotkeyAction.Paste,
      "select-all"   -> HotkeyAction.SelectAll,
      "undo"         -> HotkeyAction.Undo,
      "redo"         -> HotkeyAction.Redo,
      "goto-line"    -> HotkeyAction.GoToLine
    ).flatMap {
      case (commandName, action) =>
        config.inputConfig.hotkeyConfig.bindingsFor(action).headOption.map(trigger => commandName -> trigger.render)
    }

  /** Empty/inactive command runner */
  def empty: CommandRunner = CommandRunner(
    isActive = false,
    searchTerm = "",
    selectedIndex = 0,
    filteredCommands = List.empty
  )

  /** Create command runner with specific commands for testing */
  def withCommands(commands: List[Command]): CommandRunner = CommandRunner(
    isActive = false,
    searchTerm = "",
    selectedIndex = 0,
    filteredCommands = commands
  )
