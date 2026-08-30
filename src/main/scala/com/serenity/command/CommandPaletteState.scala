package com.serenity.command

/** The command palette's own navigation state: search text, the ranked results it produces, and which one is
  * selected. Introduced as Stage 2 of the command-runner interaction-model rework (issue #931) to split the palette
  * concern out of `CommandRunner`'s shared mode-flag grab-bag -- mirrors how Stage 1 (issue #1059) gave the settings
  * surface its own `SettingsSurfaceState` rather than a further pile of fields on the same class.
  *
  * Deliberately carries no `activeCategory` -- the category-switcher UI it once drove is retired in favor of the
  * registry's fuzzy search (`CommandRegistry.searchCommands`); each result still carries its own `Command.category`
  * for rendering as a quiet inline tag, but there is no separate navigation mode for it any more.
  *
  * Not yet wired into `CommandRunner`/`UiSurface` -- this is the additive first step (mirroring Stage 1's own first
  * turn): the type exists and is exercised by its own spec, but every existing call site still reads and writes the
  * `CommandRunner` fields it will eventually replace.
  */
final case class CommandPaletteState(
    searchTerm: String = "",
    selectedIndex: Int = 0,
    filteredCommands: List[Command] = Nil
):

  def visibleCommands: List[Command] = filteredCommands

  def selectedCommand: Option[Command] = filteredCommands.lift(selectedIndex)

  /** Update the search term and re-derive `filteredCommands` from the registry -- every command family (including
    * what used to be reached only via the Settings category tab) is searchable through the one fuzzy index, per
    * issue #931's "retire category tabs, fold into text search".
    */
  def updateSearchTerm(term: String)(using registry: CommandRegistry): CommandPaletteState =
    val filtered =
      if term.isEmpty then registry.getAllCommands
      else registry.searchCommands(term, maxResults = CommandPaletteState.MaxSearchResults)
    copy(searchTerm = term, selectedIndex = 0, filteredCommands = filtered)

  /** Move selection up or down, wrapping at either end. A no-op on an empty result list. */
  def moveSelection(delta: Int): CommandPaletteState =
    val itemCount = filteredCommands.size
    if itemCount == 0 then this
    else
      val newIndex     = (selectedIndex + delta) % itemCount
      val wrappedIndex = if newIndex < 0 then itemCount + newIndex else newIndex
      copy(selectedIndex = wrappedIndex)

  /** Jump directly to a visible index (mouse hover/click) -- a no-op outside the current result list's bounds. */
  def withSelectedIndex(index: Int): CommandPaletteState =
    if filteredCommands.indices.contains(index) then copy(selectedIndex = index) else this

object CommandPaletteState:

  private val MaxSearchResults = 50

  def empty: CommandPaletteState = CommandPaletteState()

  /** Populate the palette on open: every registered command, unfiltered -- there is no category to default to any
    * more. (Recency/frequency-weighted defaults and an explicit empty-results state are issues #1049/#1048, tracked
    * separately -- not part of this stage.)
    */
  def activate(registry: CommandRegistry): CommandPaletteState =
    CommandPaletteState(filteredCommands = registry.getAllCommands)

/** The command-runner overlay's top-level shape: either the palette or the settings surface, never both -- the
  * dispatch `CommandRunnerReducer`, `SurfaceContentResolver`, and `CommandRunnerMouseHitTesting` fork on in place of
  * `CommandRunner.mode`/`isSettingsSurface`/`activeSettingsSurface.isDefined` (issue #931, Stage 2).
  *
  * `Settings` carries `Option[SettingsSurfaceState]` rather than a bare `SettingsSurfaceState`: the settings surface
  * has a real state with no page at all yet -- browsing the top-level settings groups before drilling into any of
  * them (reached via `CommandRunner.isSettingsSurface` alone, `activeSettingsSurface = None`) -- and `SettingsPage`
  * has no case for "no page", by design (see `SettingsSurfaceState`'s own doc: it always has a current page once it
  * exists at all). `None` here is that settings-root state, distinct from the case where there is no settings surface
  * showing at all (`Palette`).
  *
  * `CommandRunner.surface` is the only producer of this type today; the payloads it carries (`CommandPaletteState`,
  * `activeSettingsSurface`) are still read off `CommandRunner`'s own fields underneath, not separate storage -- this
  * stage migrates the *dispatch decision*, not the state representation itself (a larger change, out of scope here;
  * see the PR notes).
  */
enum CommandRunnerSurface:
  case Palette(state: CommandPaletteState)
  case Settings(surface: Option[SettingsSurfaceState])
