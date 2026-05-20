package com.serenity.command

/** State for the command runner overlay */
case class CommandRunner(
  isActive: Boolean,
  searchTerm: String,
  selectedIndex: Int,
  filteredCommands: List[Command],
  previousFocus: Option[com.serenity.state.models.Focus] = None
):
  /** Update search term and filter commands */
  def updateSearchTerm(term: String)(using registry: CommandRegistry): CommandRunner =
    val filtered = registry.searchCommands(term, maxResults = 10)
    copy(
      searchTerm = term,
      selectedIndex = 0,
      filteredCommands = filtered
    )

  /** Move selection up or down, with wrapping */
  def moveSelection(delta: Int): CommandRunner =
    if filteredCommands.isEmpty then this
    else
      val newIndex = (selectedIndex + delta) % filteredCommands.length
      val wrappedIndex = if newIndex < 0 then filteredCommands.length + newIndex else newIndex
      copy(selectedIndex = wrappedIndex)

  /** Get currently selected command */
  def selectedCommand: Option[Command] =
    filteredCommands.lift(selectedIndex)

  /** Activate the command runner with given registry */
  def activate(registry: CommandRegistry): CommandRunner =
    copy(
      isActive = true,
      searchTerm = "",
      selectedIndex = 0,
      filteredCommands = registry.searchCommands("", maxResults = 10)
    )

  /** Deactivate the command runner */
  def deactivate: CommandRunner =
    copy(
      isActive = false,
      searchTerm = "",
      selectedIndex = 0,
      filteredCommands = List.empty,
      previousFocus = None
    )

  /** Get commands to display (top 5) */
  def visibleCommands: List[Command] = filteredCommands.take(5)

  /** Check if there are more commands beyond visible ones */
  def hasMoreCommands: Boolean = filteredCommands.length > 5

  /** Store the focus that should be restored when runner closes */
  def withPreviousFocus(focus: com.serenity.state.models.Focus): CommandRunner =
    copy(previousFocus = Some(focus))

object CommandRunner:
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