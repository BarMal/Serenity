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
    val filtered =
      if term.isEmpty then registry.getAllCommands
      else registry.searchCommands(term, maxResults = 50) // Allow more results for search
    copy(
      searchTerm = term,
      selectedIndex = 0,
      filteredCommands = filtered
    )

  /** Move selection up or down, with wrapping */
  def moveSelection(delta: Int): CommandRunner =
    if filteredCommands.isEmpty then this
    else
      val newIndex     = (selectedIndex + delta) % filteredCommands.length
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
      filteredCommands = registry.getAllCommands
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

  /** Get commands to display based on selected index and viewport */
  def visibleCommands: List[Command] =
    val visibleCount = 5
    if filteredCommands.length <= visibleCount then filteredCommands
    else
      val halfVisible  = visibleCount / 2
      val targetOffset = selectedIndex - halfVisible
      val offset       = math.max(0, math.min(targetOffset, filteredCommands.length - visibleCount))
      filteredCommands.slice(offset, offset + visibleCount)

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
