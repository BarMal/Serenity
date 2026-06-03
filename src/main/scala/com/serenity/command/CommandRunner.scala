package com.serenity.command

import com.serenity.animation.AnimationConfig
import com.serenity.config.{AppConfig, BackgroundStyle, CursorMode}
import com.serenity.ui.fonts.FontLoader

case class CommandRunnerSubmenuState(
    groupId: String,
    selectedIndex: Int = 0,
    editingItemId: Option[String] = None,
    editingText: String = ""
):
  def selectedItem(items: List[CommandSurfaceItem]): Option[CommandSurfaceItem] =
    items.lift(selectedIndex)

/** State for the command runner overlay */
case class CommandRunner(
    isActive: Boolean,
    searchTerm: String,
    selectedIndex: Int,
    filteredCommands: List[Command],
    activeCategory: CommandCategory = CommandCategory.All,
    optionSelections: Map[String, Int] = Map.empty,
    inputItems: List[CommandSurfaceItem.InputItem] = List.empty,
    editingItemId: Option[String] = None,
    editingText: String = "",
    submenuSelections: Map[String, Int] = Map.empty,
    previewedGroupId: Option[String] = None,
    activeSubmenu: Option[CommandRunnerSubmenuState] = None
):

  def visibleItems: List[CommandSurfaceItem] =
    val commandItems = filteredCommands.map(CommandSurfaceItem.CommandItem(_))
    val settingsItems =
      activeCategory match
        case CommandCategory.Settings =>
          val allSettings: List[CommandSurfaceItem] = settingsGroups
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
      filteredCommands = filtered,
      previewedGroupId = None,
      activeSubmenu = None
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

  def settingsGroups: List[CommandSurfaceItem.GroupItem] =
    val animationItem       = CommandRunner.animationOptionItem(optionSelections)
    val cursorModeItem      = CommandRunner.cursorModeOptionItem(optionSelections)
    val backgroundStyleItem = CommandRunner.backgroundStyleOptionItem(optionSelections)
    val codeFontItem        = CommandRunner.codeFontOptionItem(optionSelections)
    val textFontItem        = CommandRunner.textFontOptionItem(optionSelections)
    val ligaturesItem       = CommandRunner.ligaturesOptionItem(optionSelections)
    List(
      CommandSurfaceItem.GroupItem(
        id = "settings-animation",
        label = "Animation",
        children = List(animationItem) ++ inputItems.filter(item =>
          item.id == "animation-duration" || item.id == "animation-steps"
        ),
        category = CommandCategory.Settings,
        hint = Some("Mode, timing, steps")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-appearance",
        label = "Appearance",
        children = List(cursorModeItem, backgroundStyleItem) ++ inputItems.filter(_.id == "blur-radius"),
        category = CommandCategory.Settings,
        hint = Some("Cursor, background, blur")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-typography",
        label = "Typography",
        children = List(codeFontItem, textFontItem, ligaturesItem) ++ inputItems.filter(_.id == "font-size"),
        category = CommandCategory.Settings,
        hint = Some("Code, text, ligatures, size")
      )
    )

  def previewGroup(groupId: String): CommandRunner =
    copy(previewedGroupId = Some(groupId))

  def clearGroupPreview: CommandRunner =
    copy(previewedGroupId = None, activeSubmenu = None)

  def enterSelectedGroup: CommandRunner =
    selectedItem match
      case Some(group: CommandSurfaceItem.GroupItem) =>
        val rememberedIndex = submenuSelections.getOrElse(group.id, 0)
        copy(
          previewedGroupId = Some(group.id),
          activeSubmenu = Some(CommandRunnerSubmenuState(group.id, selectedIndex = rememberedIndex))
        )
      case _ => this

  def exitSubmenuToPreview: CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        copy(
          submenuSelections = submenuSelections + (submenu.groupId -> submenu.selectedIndex),
          activeSubmenu = None
        )
      case None =>
        copy(activeSubmenu = None)

  def previewOrFocusedGroupId: Option[String] =
    activeSubmenu.map(_.groupId).orElse(previewedGroupId)

  def submenuItems(groupId: String): List[CommandSurfaceItem] =
    settingsGroups.find(_.id == groupId).map(_.children).getOrElse(Nil)

  def focusedSubmenuItems: List[CommandSurfaceItem] =
    activeSubmenu.toList.flatMap(submenu => submenuItems(submenu.groupId))

  def moveSubmenuSelection(delta: Int): CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        val items = submenuItems(submenu.groupId)
        if items.isEmpty then this
        else
          val itemCount     = items.size
          val newIndex      = (submenu.selectedIndex + delta) % itemCount
          val wrappedIndex  = if newIndex < 0 then itemCount + newIndex else newIndex
          copy(
            submenuSelections = submenuSelections + (submenu.groupId -> wrappedIndex),
            activeSubmenu = Some(submenu.copy(selectedIndex = wrappedIndex, editingItemId = None, editingText = ""))
          )
      case None => this

  def beginSubmenuEditMode: CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        submenu.selectedItem(submenuItems(submenu.groupId)) match
          case Some(item: CommandSurfaceItem.InputItem) =>
            copy(activeSubmenu = Some(submenu.copy(editingItemId = Some(item.id), editingText = item.currentValue)))
          case _ =>
            this
      case None =>
        this

  def adjustSelectedSubmenuOption(delta: Int): CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        submenu.selectedItem(submenuItems(submenu.groupId)) match
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
  def activate(registry: CommandRegistry, config: AppConfig): CommandRunner =
    copy(
      isActive = true,
      searchTerm = "",
      selectedIndex = 0,
      filteredCommands = registry.commandsForCategory(activeCategory),
      optionSelections = CommandRunner.defaultOptionSelections(config),
      inputItems = CommandRunner.buildInputItems(config)
    ).syncEditMode

  /** Rebuild input items from a new config (called after a setting is applied) */
  def updateInputItems(config: AppConfig): CommandRunner =
    copy(
      inputItems = CommandRunner.buildInputItems(config),
      optionSelections = CommandRunner.defaultOptionSelections(config)
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
      submenuSelections = Map.empty,
      previewedGroupId = None,
      activeSubmenu = None
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
        submenu.selectedItem(submenuItems(submenu.groupId)) match
          case Some(item: CommandSurfaceItem.InputItem) if submenu.editingItemId.contains(item.id) =>
            this
          case _ =>
            copy(activeSubmenu = Some(submenu.copy(editingItemId = None, editingText = "")))
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

  /** Store the focus that should be restored when runner closes */

object CommandRunner:

  private[command] def defaultOptionSelections(config: AppConfig): Map[String, Int] =
    Map(
      "animation-mode" -> animationModeIndex(config),
      "cursor-mode" -> cursorModeIndex(config.cursorMode),
      "background-style" -> backgroundStyleIndex(config.backgroundStyle),
      "code-font" -> codeFontIndex(config.fontConfig.codeFontFamily),
      "text-font" -> textFontIndex(config.fontConfig.textFontFamily),
      "ligatures" -> ligaturesIndex(config.fontConfig.enableLigatures)
    )

  private[command] def cursorModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-mode",
      label = "Cursor Mode",
      options = List(
        CommandOption("Blink", CommandIntent.SetCursorMode(CursorMode.Blink)),
        CommandOption("Breathe", CommandIntent.SetCursorMode(CursorMode.Breathe))
      ),
      selectedIndex = optionSelections.getOrElse("cursor-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Style")
    )

  private[command] def backgroundStyleOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "background-style",
      label = "Background",
      options = List(
        CommandOption("Solid", CommandIntent.SetBackgroundStyle(BackgroundStyle.Solid)),
        CommandOption("Transparent", CommandIntent.SetBackgroundStyle(BackgroundStyle.Transparent)),
        CommandOption("Frosted", CommandIntent.SetBackgroundStyle(BackgroundStyle.Frosted)),
        CommandOption("Glass", CommandIntent.SetBackgroundStyle(BackgroundStyle.GlassLike))
      ),
      selectedIndex = optionSelections.getOrElse("background-style", 2),
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

  private[command] def codeFontOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "code-font",
      label = "Code Font",
      options = FontLoader.availableMonospaceFamilies.map { family =>
        CommandOption(family, CommandIntent.SetCodeFontFamily(family))
      },
      selectedIndex = optionSelections.getOrElse("code-font", 0),
      category = CommandCategory.Settings,
      hint = Some("Monospace")
    )

  private[command] def textFontOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "text-font",
      label = "Text Font",
      options = FontLoader.availableTextFamilies.map { family =>
        CommandOption(family, CommandIntent.SetTextFontFamily(family))
      },
      selectedIndex = optionSelections.getOrElse("text-font", 0),
      category = CommandCategory.Settings,
      hint = Some("Proportional")
    )

  private[command] def ligaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "ligatures",
      label = "Ligatures",
      options = List(
        CommandOption("On", CommandIntent.SetLigatures(true)),
        CommandOption("Off", CommandIntent.SetLigatures(false))
      ),
      selectedIndex = optionSelections.getOrElse("ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Text shaping")
    )

  private[command] def buildInputItems(config: AppConfig): List[CommandSurfaceItem.InputItem] =
    val durationValue = config.characterAnimation.map(_.durationMs.toString).getOrElse("0")
    val stepsValue    = config.characterAnimation.map(_.steps.toString).getOrElse("0")
    val blurValue     = config.blurRadius.toString
    val fontSizeValue = config.fontConfig.fontSize.toString

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
      ),
      CommandSurfaceItem.InputItem(
        id = "font-size",
        label = "Font Size",
        hint = "(8.0-48.0)",
        currentValue = fontSizeValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 8.0f && v <= 48.0f)
            .map(CommandIntent.SetFontSize(_)),
        category = CommandCategory.Settings
      )
    )

  private def animationModeIndex(config: AppConfig): Int =
    config.characterAnimation match
      case None                                                   => 0
      case Some(animation) if AnimationConfig.subtle.contains(animation) => 1
      case _                                                      => 2

  private def cursorModeIndex(mode: CursorMode): Int =
    mode match
      case CursorMode.Blink   => 0
      case CursorMode.Breathe => 1

  private def backgroundStyleIndex(style: BackgroundStyle): Int =
    style match
      case BackgroundStyle.Solid       => 0
      case BackgroundStyle.Transparent => 1
      case BackgroundStyle.Frosted     => 2
      case BackgroundStyle.GlassLike   => 3

  private def codeFontIndex(family: String): Int =
    FontLoader.availableMonospaceFamilies.indexOf(family) match
      case -1    => 0
      case index => index

  private def textFontIndex(family: String): Int =
    FontLoader.availableTextFamilies.indexOf(family) match
      case -1    => 0
      case index => index

  private def ligaturesIndex(enabled: Boolean): Int =
    if enabled then 0 else 1

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
