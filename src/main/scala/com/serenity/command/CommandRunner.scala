package com.serenity.command

/** State for the command runner overlay */
case class CommandRunner(
    isActive: Boolean,
    searchTerm: String,
    selectedIndex: Int,
    filteredCommands: List[Command],
    activeCategory: CommandCategory = CommandCategory.All,
    optionSelections: Map[String, Int] = Map.empty,
    previousFocus: Option[com.serenity.state.models.Focus] = None
):

  def visibleItems: List[CommandSurfaceItem] =
    val commandItems = filteredCommands.map(CommandSurfaceItem.CommandItem(_))
    val optionItems =
      activeCategory match
        case CommandCategory.Settings =>
          val animationItem = CommandRunner.animationOptionItem(optionSelections)
          if searchTerm.isEmpty || animationItem.searchText.toLowerCase.contains(searchTerm.toLowerCase) then List(animationItem)
          else Nil
        case _ => Nil
    optionItems ++ commandItems

  def selectedItem: Option[CommandSurfaceItem] =
    visibleItems.lift(selectedIndex)

  /** Update search term and filter commands */
  def updateSearchTerm(term: String)(using registry: CommandRegistry): CommandRunner =
    val filtered =
      if term.isEmpty then registry.commandsForCategory(activeCategory)
      else registry.searchCommands(term, maxResults = 50) // Allow more results for search
    copy(
      searchTerm = term,
      selectedIndex = 0,
      filteredCommands = filtered
    )

  /** Move selection up or down, with wrapping */
  def moveSelection(delta: Int): CommandRunner =
    val itemCount = visibleItems.size
    if itemCount == 0 then this
    else
      val newIndex     = (selectedIndex + delta) % itemCount
      val wrappedIndex = if newIndex < 0 then itemCount + newIndex else newIndex
      copy(selectedIndex = wrappedIndex)

  /** Get currently selected command */
  def selectedCommand: Option[Command] =
    selectedItem.collect { case CommandSurfaceItem.CommandItem(command) => command }

  def withActiveCategory(category: CommandCategory)(using registry: CommandRegistry): CommandRunner =
    copy(
      activeCategory = category,
      selectedIndex = 0
    ).updateSearchTerm("")

  def switchCategory(delta: Int)(using registry: CommandRegistry): CommandRunner =
    val categories = List(
      CommandCategory.All,
      CommandCategory.File,
      CommandCategory.View,
      CommandCategory.Edit,
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
      case Some((_, index)) => copy(selectedIndex = index)
      case None             => this

  /** Activate the command runner with given registry */
  def activate(registry: CommandRegistry): CommandRunner =
    copy(
      isActive = true,
      searchTerm = "",
      selectedIndex = 0,
      filteredCommands = registry.commandsForCategory(activeCategory)
    )

  /** Deactivate the command runner */
  def deactivate: CommandRunner =
    copy(
      isActive = false,
      searchTerm = "",
      selectedIndex = 0,
      filteredCommands = List.empty,
      activeCategory = CommandCategory.All,
      optionSelections = Map.empty,
      previousFocus = None
    )

  /** Get commands to display based on selected index and viewport */
  def visibleCommands: List[Command] =
    val visibleCount = 5
    val items = visibleItems
    if items.length <= visibleCount then
      items.collect { case CommandSurfaceItem.CommandItem(command) => command }
    else
      val halfVisible  = visibleCount / 2
      val targetOffset = selectedIndex - halfVisible
      val offset       = math.max(0, math.min(targetOffset, items.length - visibleCount))
      items.slice(offset, offset + visibleCount).collect { case CommandSurfaceItem.CommandItem(command) => command }

  /** Check if there are more commands beyond visible ones */
  def hasMoreCommands: Boolean = visibleItems.length > 5

  /** Store the focus that should be restored when runner closes */
  def withPreviousFocus(focus: com.serenity.state.models.Focus): CommandRunner =
    copy(previousFocus = Some(focus))

object CommandRunner:

  private[command] def animationOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "animation-mode",
      label = "Animation",
      options = List(
        CommandOption("None", CommandIntent.SetAnimationMode(AnimationMode.None)),
        CommandOption("Subtle", CommandIntent.SetAnimationMode(AnimationMode.Subtle)),
        CommandOption("Full", CommandIntent.SetAnimationMode(AnimationMode.Smooth))
      ),
      selectedIndex = optionSelections.getOrElse("animation-mode", 2),
      category = CommandCategory.Settings,
      hint = Some("Mode")
    )

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
