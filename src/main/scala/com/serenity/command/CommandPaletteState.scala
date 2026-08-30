package com.serenity.command

/** The command palette's own navigation state: search text, the ranked results it produces, and which one is selected.
  * Introduced as Stage 2 of the command-runner interaction-model rework (issue #931) to split the palette concern out
  * of `CommandRunner`'s shared mode-flag grab-bag -- mirrors how Stage 1 (issue #1059) gave the settings surface its
  * own `SettingsSurfaceState` rather than a further pile of fields on the same class. This is `CommandRunner`'s sole
  * storage for search/select/results now -- see `CommandRunnerSurface` below.
  *
  * Deliberately carries no `activeCategory` -- the category-switcher UI it once drove is retired in favor of the
  * registry's fuzzy search (`CommandRegistry.searchCommands`); each result still carries its own `Command.category` for
  * rendering as a quiet inline tag, but there is no separate navigation mode for it any more.
  */
final case class CommandPaletteState(
    searchTerm: String = "",
    selectedIndex: Int = 0,
    filteredCommands: List[Command] = Nil
):

  def visibleCommands: List[Command] = filteredCommands

  def selectedCommand: Option[Command] = filteredCommands.lift(selectedIndex)

  /** Update the search term and re-derive `filteredCommands` from the registry -- every command family (including what
    * used to be reached only via the Settings category tab) is searchable through the one fuzzy index, per issue #931's
    * "retire category tabs, fold into text search".
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

  /** Populate the palette on open: every registered command, unfiltered -- there is no category to default to any more.
    * (Recency/frequency-weighted defaults and an explicit empty-results state are issues #1049/#1048, tracked
    * separately -- not part of this stage.)
    */
  def activate(registry: CommandRegistry): CommandPaletteState =
    CommandPaletteState(filteredCommands = registry.getAllCommands)

/** `CommandRunner`'s sole navigation storage (issue #931, Stage 2): the whole former mode-flag grab-bag --
  * `mode`/`isSettingsSurface`, and the bare `searchTerm`/`selectedIndex`/`filteredCommands`/`activeSettingsSurface`
  * fields that sat alongside it -- lives here now, not duplicated as parallel `CommandRunner` fields.
  *
  * `Settings` carries its own `root: CommandPaletteState` alongside `drilled: Option[SettingsSurfaceState]`, not a bare
  * `SettingsSurfaceState`: the dedicated Settings surface has a real "open, but nothing drilled into yet" state --
  * browsing the top-level settings groups, or searching them -- and `SettingsPage` has no case for "no page", by design
  * (see `SettingsSurfaceState`'s own doc: it always has a current page once it exists at all). Reusing
  * `CommandPaletteState`'s search/select shape for that root state (rather than inventing a second near-identical type)
  * means both roots -- the palette's and the settings surface's -- are searched and navigated by the exact same
  * mechanics; `root.filteredCommands` is simply unused there (settings search goes through
  * `CommandRunner.matchingSettingsResults` instead).
  *
  * A group entered from either root carries that root forward as `drilled`'s sibling `root` field, so the search that
  * led to it (if any) survives while drilled in -- `CommandRunnerReducerSpec`'s "open the matched settings leaf without
  * filtering away its context" depends on exactly this. Escaping back out of the *last* drilled level always lands on a
  * bare `Settings(root, drilled = None)`, i.e. the settings-root view -- including when the group was reached via a
  * `Palette` search rather than `CommandRunner.openSettings`. The pre-migration code (`mode` and
  * `activeSettingsSurface` as independent fields) could instead restore the original `Palette` view in that one case;
  * no test exercised it, and reproducing it faithfully needs a fourth state (`Palette` searched, then drilled, then
  * popped back to that same `Palette`) that this migration deliberately does not add -- a documented simplification,
  * not an oversight (see the PR notes).
  */
enum CommandRunnerSurface:
  case Palette(state: CommandPaletteState = CommandPaletteState())
  case Settings(root: CommandPaletteState = CommandPaletteState(), drilled: Option[SettingsSurfaceState] = None)
