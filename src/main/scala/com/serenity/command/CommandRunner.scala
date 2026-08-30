package com.serenity.command

import java.util.Locale

import com.serenity.config.*
import com.serenity.keystroke.{KeyStrokeInfo, KeyboardFidelityTier}
import com.serenity.ui.presets.UiPreset

/** In-flight keybinding recording for a settings item, and any conflict it surfaced.
  *
  * Scoped to whichever page owns the recording (see `SettingsPage.Editing.recording`) rather than carried as per-item
  * fields on a flat submenu state, since the page stack (issue #1059) already knows which item, if any, is being
  * edited.
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

  case Editing(
      groupId: String,
      itemId: String,
      draftText: String,
      searchTerm: String = "",
      recording: Option[RecordingState] = None
  )

object SettingsPage:

  /** The group a page belongs to, regardless of which case it is, and the local search filtering its item list. Plain
    * `extension`s rather than members of the enum itself, so each case stays a normal case class -- notably keeping its
    * compiler-synthesized `copy` (an inherited abstract `def` overridden by a same-named case parameter suppresses that
    * synthesis under Scala 3, so this sidesteps it). `Editing` carries its own `searchTerm` (rather than reusing the
    * `Group` page's) so drilling into one input's edit doesn't lose the list filter it was reached through.
    */
  extension (page: SettingsPage)

    def groupId: String = page match
      case Group(id, _, _)         => id
      case Editing(id, _, _, _, _) => id

    def searchTerm: String = page match
      case Group(_, _, term)         => term
      case Editing(_, _, _, term, _) => term

    /** The item currently mid-edit, if `page` is an `Editing` page -- `None` for a plain `Group` page. */
    def editingItemId: Option[String] = page match
      case _: Group         => None
      case editing: Editing => Some(editing.itemId)

    /** The in-progress draft text for `page`'s edit -- empty for a plain `Group` page, since nothing is being edited
      * there.
      */
    def draftText: String = page match
      case _: Group         => ""
      case editing: Editing => editing.draftText

    /** The in-flight keybinding recording for `page`'s edit, if any -- `None` for a plain `Group` page. */
    def recording: Option[RecordingState] = page match
      case _: Group         => None
      case editing: Editing => editing.recording

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

  /** Capped, expand-in-place group preview rows for a group nested under the selected item -- capped to
    * `MaxPreviewRows` with an overflow count -- a pure function of the selected item, carrying no state of its own
    * (issue #1059).
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
    keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full
):

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
  private def rootState: CommandPaletteState = surface match
    case CommandRunnerSurface.Palette(state)    => state
    case CommandRunnerSurface.Settings(root, _) => root

  private def withRootSelectedIndex(index: Int): CommandRunner =
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
        if state.searchTerm.isEmpty then commandItems
        else
          val (strongCommandMatches, remainingCommandMatches) =
            commandItems.partition(item => CommandRunner.isStrongCommandMatch(item.command, state.searchTerm))
          val (exactCommandMatches, remainingStrongCommandMatches) =
            strongCommandMatches.partition(item => CommandRunner.isExactCommandMatch(item.command, state.searchTerm))
          val settingsMatches = matchingSettingsResults(state.searchTerm)
          val (exactSettingsMatches, remainingSettingsMatches) =
            settingsMatches.partition(item =>
              CommandRunner.isExactSettingsTarget(item, CommandRunner.normalizedSearchTerm(state.searchTerm))
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
      else registry.searchCommands(term, maxResults = 50)
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
  private def filteredPageItems(page: SettingsPage, items: List[CommandSurfaceItem]): List[CommandSurfaceItem] =
    val lowerTerm = page.searchTerm.trim.toLowerCase
    if lowerTerm.isEmpty then items
    else items.filter(_.searchText.toLowerCase.contains(lowerTerm))

  /** The list index a page corresponds to. `Group` carries one directly; `Editing` doesn't (it names its item by id,
    * not position), so it's recovered by looking the item up in the same filtered list `beginSubmenuEditMode` read it
    * from -- mirroring how `enterSelectedGroup` recovers a search-jump's index via `indexWhere(_.id == ...).max(0)`.
    */
  private def pageSelectedIndex(page: SettingsPage): Int =
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

  def moveSubmenuSelection(delta: Int): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        val items   = filteredPageItems(surface.current, submenuItems(groupId))
        if items.isEmpty then this
        else
          val itemCount    = items.size
          val newIndex     = (pageSelectedIndex(surface.current) + delta) % itemCount
          val wrappedIndex = if newIndex < 0 then itemCount + newIndex else newIndex
          copy(
            submenuSelections = submenuSelections + (groupId -> wrappedIndex),
            // Moving selection always exits edit mode, so the new current page is always rebuilt as a Group,
            // dropping any Editing page that was there.
            surface = CommandRunnerSurface.Settings(
              root = rootState,
              drilled =
                Some(surface.copy(current = SettingsPage.Group(groupId, wrappedIndex, surface.current.searchTerm)))
            )
          )
      case None => this

  def beginSubmenuEditMode: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        val items   = filteredPageItems(surface.current, submenuItems(groupId))
        items.lift(pageSelectedIndex(surface.current)) match
          case Some(item: CommandSurfaceItem.InputItem) =>
            withDrilledSettingsSurface(
              surface.copy(current =
                SettingsPage.Editing(
                  groupId = groupId,
                  itemId = item.id,
                  draftText = item.currentValue,
                  searchTerm = surface.current.searchTerm
                )
              )
            )
          case _ =>
            this
      case None =>
        this

  /** Sets the currently-edited item's draft text, beginning a fresh edit of `itemId` if nothing (or a different item)
    * was being edited. Used for both starting an edit from a single keystroke and continuing one (`RunnerInsertChar`),
    * so it always writes `itemId` rather than assuming the previous one still applies.
    */
  def withSubmenuEditingItem(itemId: String, text: String): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        withDrilledSettingsSurface(
          surface.copy(current =
            SettingsPage.Editing(
              groupId = surface.current.groupId,
              itemId = itemId,
              draftText = text,
              searchTerm = surface.current.searchTerm
            )
          )
        )
      case None =>
        this

  /** Replaces the currently-edited item's draft text in place (word-delete, not character Backspace -- see
    * `deleteSubmenuTextBackward` for that). A no-op when nothing is being edited.
    */
  def withSubmenuEditingText(text: String): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        surface.current match
          case editing: SettingsPage.Editing =>
            withDrilledSettingsSurface(surface.copy(current = editing.copy(draftText = text)))
          case _ =>
            this
      case None =>
        this

  /** Cancels an in-progress edit without touching any pending recording/conflict state or navigating -- Escape's
    * "cancel this edit, stay on this page" behavior. Contrast `clearSubmenuEditingAndRecording`, which also clears
    * recording state (used once a value has actually been submitted or a recording finished).
    */
  def cancelSubmenuEditingText: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        withDrilledSettingsSurface(
          surface.copy(current =
            SettingsPage.Group(surface.current.groupId, pageSelectedIndex(surface.current), surface.current.searchTerm)
          )
        )
      case None =>
        this

  /** Clears all in-progress editing/recording sub-state for the current submenu item -- used once a value has been
    * submitted, a conflict resolved, or a recording finished. Always rebuilds the current page as a Group; there is no
    * separate recording sub-state left to clear once the page is rebuilt this way, since `Editing.recording` only
    * exists on an `Editing` page.
    */
  def clearSubmenuEditingAndRecording: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        withDrilledSettingsSurface(
          surface.copy(current =
            SettingsPage.Group(surface.current.groupId, pageSelectedIndex(surface.current), surface.current.searchTerm)
          )
        )
      case None =>
        this

  /** Begins recording a keybinding for `itemId`: an edit with empty draft text, tagged with a fresh `RecordingState`.
    */
  def beginSubmenuRecording(itemId: String): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        withDrilledSettingsSurface(
          surface.copy(current =
            SettingsPage.Editing(
              groupId = surface.current.groupId,
              itemId = itemId,
              draftText = "",
              searchTerm = surface.current.searchTerm,
              recording = Some(RecordingState(itemId))
            )
          )
        )
      case None =>
        this

  /** Stashes a just-recorded keystroke as pending, awaiting a possible double-tap within the recorder's time window. */
  def withPendingRecordedBinding(info: KeyStrokeInfo, recordedAtMillis: Long): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        surface.current match
          case editing: SettingsPage.Editing =>
            val recording = editing.recording.getOrElse(RecordingState(editing.itemId))
            withDrilledSettingsSurface(
              surface.copy(current =
                editing.copy(recording = Some(recording.copy(pendingRecordedBinding = Some(info -> recordedAtMillis))))
              )
            )
          case _ =>
            this
      case None =>
        this

  /** Deletes one character from the current settings page's text -- an in-progress edit's draft, or (when not editing)
    * a group's local search -- via `SettingsSurfaceState.deleteBackward`. A no-op when there is no text to delete;
    * never navigates the stack. This replaces Backspace's old fallback to `exitSubmenuToPreview` once text was already
    * empty (issue #1059).
    */
  def deleteSubmenuTextBackward: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val updated = SettingsSurfaceState.deleteBackward(surface)
        if updated == surface then this
        else withDrilledSettingsSurface(updated).copy(statusMessage = None)
      case None =>
        this

  def enterSelectedSubmenuGroup: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        val items   = filteredPageItems(surface.current, submenuItems(groupId))
        items.lift(pageSelectedIndex(surface.current)) match
          case Some(group: CommandSurfaceItem.GroupItem) =>
            val rememberedIndex = submenuSelections.getOrElse(group.id, 0)
            copy(
              submenuSelections = submenuSelections + (groupId -> pageSelectedIndex(surface.current)),
              // A true push: the group we're leaving becomes the nearest ancestor of the group we're entering.
              surface = CommandRunnerSurface.Settings(
                root = rootState,
                drilled = Some(surface.push(SettingsPage.Group(group.id, rememberedIndex)))
              )
            )
          case _ =>
            this
      case None =>
        this

  /** Adjusting an option's value writes to `optionSelections`, not to the page-stack itself, so `activeSettingsSurface`
    * is left exactly as it was.
    */
  def adjustSelectedSubmenuOption(delta: Int): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val items = filteredPageItems(surface.current, submenuItems(surface.current.groupId))
        items.lift(pageSelectedIndex(surface.current)) match
          case Some(option: CommandSurfaceItem.OptionItem) =>
            val updatedOption = option.moveSelection(delta)
            copy(optionSelections = optionSelections + (option.id -> updatedOption.selectedIndex))
          case _ =>
            this
      case None =>
        this

  def adjustSelectedOption(delta: Int): CommandRunner =
    selectedItem match
      case Some(option: CommandSurfaceItem.OptionItem) =>
        val updatedOption = option.moveSelection(delta)
        copy(optionSelections = optionSelections + (option.id -> updatedOption.selectedIndex))
      case _ =>
        this

  def withSelectedItem(itemId: String): CommandRunner =
    visibleItems.zipWithIndex.find(_._1.id == itemId) match
      case Some((_, index)) => withRootSelectedIndex(index).syncEditMode
      case None             => this

  def withSelectedVisibleIndex(index: Int): CommandRunner =
    if visibleItems.indices.contains(index) then withRootSelectedIndex(index).syncEditMode
    else this

  def withSelectedFocusedSubmenuIndex(index: Int): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        val items   = filteredPageItems(surface.current, submenuItems(groupId))
        if items.indices.contains(index) then
          copy(
            submenuSelections = submenuSelections + (groupId -> index),
            // As with moveSubmenuSelection: setting an index directly always exits edit mode, so this is always
            // rebuilt as a Group.
            surface = CommandRunnerSurface.Settings(
              root = rootState,
              drilled = Some(surface.copy(current = SettingsPage.Group(groupId, index, surface.current.searchTerm)))
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
      surface = CommandRunnerSurface.Palette(CommandPaletteState(filteredCommands = registry.getAllCommands)),
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
      surface = CommandRunnerSurface.Palette(),
      optionSelections = Map.empty,
      inputItems = List.empty,
      editingItemId = None,
      editingText = "",
      recordingItemId = None,
      submenuSelections = Map.empty,
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
  def empty: CommandRunner = CommandRunner(isActive = false)

  /** Create command runner with specific commands for testing */
  def withCommands(commands: List[Command]): CommandRunner =
    CommandRunner(
      isActive = false,
      surface = CommandRunnerSurface.Palette(CommandPaletteState(filteredCommands = commands))
    )
