package com.serenity.command

import com.serenity.config.{AppConfig, CursorMode}

/** State for the command runner overlay */
case class CommandRunner(
    isActive: Boolean,
    searchTerm: String,
    selectedIndex: Int,
    filteredCommands: List[Command],
    activeCategory: CommandCategory = CommandCategory.All,
    optionSelections: Map[String, Int] = Map.empty,
    previousFocus: Option[com.serenity.state.models.Focus] = None,
    inputItems: List[CommandSurfaceItem.InputItem] = List.empty,
    editingItemId: Option[String] = None,
    editingText: String = ""
):

  def visibleItems: List[CommandSurfaceItem] =
    val commandItems = filteredCommands.map(CommandSurfaceItem.CommandItem(_))
    val settingsItems =
      activeCategory match
        case CommandCategory.Settings =>
          val animationItem  = CommandRunner.animationOptionItem(optionSelections)
          val cursorModeItem = CommandRunner.cursorModeOptionItem(optionSelections)
          val allSettings: List[CommandSurfaceItem] = List(animationItem, cursorModeItem) ++ inputItems
          if searchTerm.isEmpty then allSettings
          else allSettings.filter(_.searchText.toLowerCase.contains(searchTerm.toLowerCase))
        case _ => Nil
    settingsItems ++ commandItems

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
      filteredCommands = filtered
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

  def withActiveCategory(category: CommandCategory)(using registry: CommandRegistry): CommandRunner =
    copy(
      activeCategory = category,
      selectedIndex = 0
    ).updateSearchTerm("").syncEditMode

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
      case Some((_, index)) => copy(selectedIndex = index).syncEditMode
      case None             => this

  /** Activate the command runner with given registry and config */
  def activate(registry: CommandRegistry, config: AppConfig = AppConfig.default): CommandRunner =
    copy(
      isActive = true,
      searchTerm = "",
      selectedIndex = 0,
      filteredCommands = registry.commandsForCategory(activeCategory),
      inputItems = CommandRunner.buildInputItems(config)
    ).syncEditMode

  /** Rebuild input items from a new config (called after a setting is applied) */
  def updateInputItems(config: AppConfig): CommandRunner =
    copy(inputItems = CommandRunner.buildInputItems(config)).syncEditMode

  /** Deactivate the command runner */
  def deactivate: CommandRunner =
    copy(
      isActive = false,
      searchTerm = "",
      selectedIndex = 0,
      filteredCommands = List.empty,
      activeCategory = CommandCategory.All,
      optionSelections = Map.empty,
      previousFocus = None,
      inputItems = List.empty,
      editingItemId = None,
      editingText = ""
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

  private[command] def cursorModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-mode",
      label = "Cursor Mode",
      options = List(
        CommandOption("Blink",   CommandIntent.SetCursorMode(CursorMode.Blink)),
        CommandOption("Breathe", CommandIntent.SetCursorMode(CursorMode.Breathe))
      ),
      selectedIndex = optionSelections.getOrElse("cursor-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Style")
    )

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

  private[command] def buildInputItems(config: AppConfig): List[CommandSurfaceItem.InputItem] =
    val durationValue = config.characterAnimation.map(_.durationMs.toString).getOrElse("0")
    val stepsValue    = config.characterAnimation.map(_.steps.toString).getOrElse("0")
    val blurValue     = config.blurRadius.toString

    List(
      CommandSurfaceItem.InputItem(
        id = "animation-duration",
        label = "Anim Duration",
        hint = "ms (0–10000)",
        currentValue = durationValue,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(v => v >= 0 && v <= 10000)
            .map(CommandIntent.SetAnimationDuration(_)),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "animation-steps",
        label = "Anim Steps",
        hint = "(0–100)",
        currentValue = stepsValue,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(v => v >= 0 && v <= 100)
            .map(CommandIntent.SetAnimationSteps(_)),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "blur-radius",
        label = "Blur Radius",
        hint = "(0.0–1.0)",
        currentValue = blurValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 0.0f && v <= 1.0f)
            .map(CommandIntent.SetBlurRadius(_)),
        category = CommandCategory.Settings
      )
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
