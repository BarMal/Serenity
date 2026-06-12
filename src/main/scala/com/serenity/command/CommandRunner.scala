package com.serenity.command

import com.serenity.animation.AnimationConfig
import com.serenity.config.*
import com.serenity.lsp.config.LanguageId
import com.serenity.ui.fonts.FontLoader

case class CommandRunnerSubmenuState(
    groupId: String,
    selectedIndex: Int = 0,
    editingItemId: Option[String] = None,
    editingText: String = "",
    searchTerm: String = "",
    parentGroupId: Option[String] = None
):
  def selectedItem(items: List[CommandSurfaceItem]): Option[CommandSurfaceItem] =
    items.lift(selectedIndex)

  def filteredItems(items: List[CommandSurfaceItem]): List[CommandSurfaceItem] =
    val lowerTerm = searchTerm.trim.toLowerCase
    if lowerTerm.isEmpty then items
    else items.filter(_.searchText.toLowerCase.contains(lowerTerm))

  def selectedItemFromAll(items: List[CommandSurfaceItem]): Option[CommandSurfaceItem] =
    selectedItem(filteredItems(items))

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
    if searchTerm.isEmpty then
      activeCategory match
        case CommandCategory.Settings => settingsGroups ++ commandItems
        case _                        => commandItems
    else
      val (strongCommandMatches, remainingCommandMatches) =
        commandItems.partition(item => CommandRunner.isStrongCommandMatch(item.command, searchTerm))
      strongCommandMatches ++ matchingSettingsGroups(searchTerm) ++ remainingCommandMatches

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
    val animationItem        = CommandRunner.animationOptionItem(optionSelections)
    val cursorModeItem       = CommandRunner.cursorModeOptionItem(optionSelections)
    val backgroundStyleItem  = CommandRunner.backgroundStyleOptionItem(optionSelections)
    val interfaceDensityItem = CommandRunner.interfaceDensityOptionItem(optionSelections)
    val markdownViewItem     = CommandRunner.markdownViewOptionItem(optionSelections)
    val keymapItems          = inputItems.filter(_.id.startsWith("keymap-"))
    List(
      CommandSurfaceItem.GroupItem(
        id = "settings-animation",
        label = "Animation",
        children = List(animationItem) ++ inputItems.filter(item =>
          item.id == "animation-duration" || item.id == "animation-steps"
        ),
        category = CommandCategory.Settings,
        hint = Some("Style, duration, steps")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-appearance",
        label = "Appearance",
        children = List(cursorModeItem, backgroundStyleItem, interfaceDensityItem) ++ inputItems.filter(
          _.id == "blur-radius"
        ),
        category = CommandCategory.Settings,
        hint = Some("Cursor, background, density, blur")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-ui-presets",
        label = "UI Presets",
        children = inputItems.filter(item => item.id == "ui-preset-save" || item.id == "ui-preset-apply"),
        category = CommandCategory.Settings,
        hint = Some("Save or apply named layouts")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-code-font",
        label = "Code Font",
        children = List(
          CommandRunner.codeFontGroupItem(optionSelections),
          CommandRunner.codeLigaturesOptionItem(optionSelections)
        ) ++ inputItems.filter(_.id == "code-font-size"),
        category = CommandCategory.Settings,
        hint = Some("Family, size, ligatures")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-prose-font",
        label = "Prose Font",
        children = List(
          CommandRunner.textFontGroupItem(optionSelections),
          CommandRunner.textLigaturesOptionItem(optionSelections)
        ) ++ inputItems.filter(_.id == "text-font-size"),
        category = CommandCategory.Settings,
        hint = Some("Family, size, ligatures")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-ui-font",
        label = "UI Font",
        children = List(
          CommandRunner.uiFontGroupItem(optionSelections),
          CommandRunner.uiLigaturesOptionItem(optionSelections)
        ) ++ inputItems.filter(_.id == "ui-font-size"),
        category = CommandCategory.Settings,
        hint = Some("Family, size, ligatures")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-markdown",
        label = "Markdown",
        children = List(markdownViewItem),
        category = CommandCategory.Settings,
        hint = Some("Source, split preview, or inline lens")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-language",
        label = "Language",
        children = CommandRunner.languageItems,
        category = CommandCategory.Settings,
        hint = Some("Set the current buffer language mode")
      ),
      CommandSurfaceItem.GroupItem(
        id = "settings-keymap",
        label = "Keymap",
        children = keymapItems,
        category = CommandCategory.Settings,
        hint = Some("Inspect and edit bindings")
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
      case Some(submenu) if submenu.parentGroupId.nonEmpty =>
        val parentId    = submenu.parentGroupId.get
        val parentItems = submenuItems(parentId)
        val parentIndex =
          parentItems.indexWhere(_.id == submenu.groupId) match
            case -1    => submenuSelections.getOrElse(parentId, 0)
            case index => index
        copy(
          submenuSelections =
            submenuSelections + (submenu.groupId -> submenu.selectedIndex) + (parentId -> parentIndex),
          activeSubmenu = Some(CommandRunnerSubmenuState(parentId, selectedIndex = parentIndex))
        )
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

  def focusedSubmenuItems: List[CommandSurfaceItem] =
    activeSubmenu.toList.flatMap(submenu => submenu.filteredItems(submenuItems(submenu.groupId)))

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
            activeSubmenu = Some(submenu.copy(selectedIndex = wrappedIndex, editingItemId = None, editingText = ""))
          )
      case None => this

  def beginSubmenuEditMode: CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        submenu.selectedItemFromAll(submenuItems(submenu.groupId)) match
          case Some(item: CommandSurfaceItem.InputItem) =>
            copy(activeSubmenu = Some(submenu.copy(editingItemId = Some(item.id), editingText = item.currentValue)))
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
                  parentGroupId = Some(submenu.groupId)
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
        submenu.selectedItemFromAll(submenuItems(submenu.groupId)) match
          case Some(item: CommandSurfaceItem.InputItem) if submenu.editingItemId.contains(item.id) =>
            this
          case _ =>
            copy(activeSubmenu = Some(submenu.copy(editingItemId = None, editingText = "")))
      case None =>
        this

  def updateSubmenuSearch(term: String): CommandRunner =
    activeSubmenu match
      case Some(submenu) =>
        val updated = submenu.copy(searchTerm = term, selectedIndex = 0, editingItemId = None, editingText = "")
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

  private def matchingSettingsGroups(term: String): List[CommandSurfaceItem.GroupItem] =
    val lowerTerm = term.toLowerCase
    if lowerTerm.length < 3 then Nil
    else settingsGroups.filter(_.searchText.toLowerCase.contains(lowerTerm))

  /** Store the focus that should be restored when runner closes */

object CommandRunner:

  private def isStrongCommandMatch(command: Command, term: String): Boolean =
    val lowerTerm  = term.toLowerCase
    val nameLower  = command.name.toLowerCase
    val labelLower = command.label.toLowerCase
    nameLower.startsWith(lowerTerm) || labelLower.startsWith(lowerTerm)

  private[command] def defaultOptionSelections(config: AppConfig): Map[String, Int] =
    Map(
      "animation-mode"    -> animationModeIndex(config),
      "cursor-mode"       -> cursorModeIndex(config.cursorMode),
      "background-style"  -> backgroundStyleIndex(config.backgroundStyle),
      "interface-density" -> interfaceDensityIndex(config.interfaceDensity),
      "markdown-view"     -> markdownViewModeIndex(config.markdownViewMode),
      "code-font"         -> codeFontIndex(config.fontConfig.codeFontFamily),
      "text-font"         -> textFontIndex(config.fontConfig.textFontFamily),
      "ui-font"           -> uiFontIndex(config.fontConfig.uiFontFamily),
      "code-ligatures"    -> ligaturesIndex(config.fontConfig.codeLigatures),
      "text-ligatures"    -> ligaturesIndex(config.fontConfig.textLigatures),
      "ui-ligatures"      -> ligaturesIndex(config.fontConfig.uiLigatures)
    )

  private[command] def cursorModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-mode",
      label = "Cursor Style",
      options = List(
        CommandOption("Blink", CommandIntent.SetCursorMode(CursorMode.Blink)),
        CommandOption("Breathe", CommandIntent.SetCursorMode(CursorMode.Breathe))
      ),
      selectedIndex = optionSelections.getOrElse("cursor-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Blink or breathe")
    )

  private[command] def backgroundStyleOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "background-style",
      label = "Background Style",
      options = List(
        CommandOption("Solid", CommandIntent.SetBackgroundStyle(BackgroundStyle.Solid)),
        CommandOption("Transparent", CommandIntent.SetBackgroundStyle(BackgroundStyle.Transparent)),
        CommandOption("Frosted", CommandIntent.SetBackgroundStyle(BackgroundStyle.Frosted)),
        CommandOption("Glass", CommandIntent.SetBackgroundStyle(BackgroundStyle.GlassLike))
      ),
      selectedIndex = optionSelections.getOrElse("background-style", 2),
      category = CommandCategory.Settings,
      hint = Some("Solid, transparent, frosted, or glass")
    )

  private[command] def interfaceDensityOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "interface-density",
      label = "Interface Density",
      options = List(
        CommandOption("Compact", CommandIntent.SetInterfaceDensity(InterfaceDensity.Compact)),
        CommandOption("Comfortable", CommandIntent.SetInterfaceDensity(InterfaceDensity.Comfortable)),
        CommandOption("Spacious", CommandIntent.SetInterfaceDensity(InterfaceDensity.Spacious))
      ),
      selectedIndex = optionSelections.getOrElse("interface-density", 1),
      category = CommandCategory.Settings,
      hint = Some("Compact, comfortable, or spacious")
    )

  private[command] def markdownViewOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "markdown-view",
      label = "Markdown View",
      options = List(
        CommandOption("Source", CommandIntent.SetMarkdownViewMode(MarkdownViewMode.Source)),
        CommandOption("Split Preview", CommandIntent.SetMarkdownViewMode(MarkdownViewMode.SplitPreview)),
        CommandOption("Inline Lens", CommandIntent.SetMarkdownViewMode(MarkdownViewMode.InlineLens))
      ),
      selectedIndex = optionSelections.getOrElse("markdown-view", 0),
      category = CommandCategory.Settings,
      hint = Some("Source, side preview, or inline editing lens")
    )

  private[command] def animationOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "animation-mode",
      label = "Animation Style",
      options = List(
        CommandOption("None", CommandIntent.SetAnimationMode(AnimationMode.None)),
        CommandOption("Subtle", CommandIntent.SetAnimationMode(AnimationMode.Subtle)),
        CommandOption("Full", CommandIntent.SetAnimationMode(AnimationMode.Smooth))
      ),
      selectedIndex = optionSelections.getOrElse("animation-mode", 2),
      category = CommandCategory.Settings,
      hint = Some("None, subtle, or full")
    )

  private[command] def codeFontGroupItem(optionSelections: Map[String, Int]): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "code-font",
      label = "Code Font",
      selectedIndex = optionSelections.getOrElse("code-font", 0),
      families = FontLoader.availableMonospaceFamilies,
      intent = CommandIntent.SetCodeFontFamily(_),
      hint = "Used in code buffers"
    )

  private[command] def textFontGroupItem(optionSelections: Map[String, Int]): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "text-font",
      label = "Text Font",
      selectedIndex = optionSelections.getOrElse("text-font", 0),
      families = FontLoader.availableTextFamilies,
      intent = CommandIntent.SetTextFontFamily(_),
      hint = "Used in prose buffers"
    )

  private[command] def uiFontGroupItem(optionSelections: Map[String, Int]): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "ui-font",
      label = "UI Font",
      selectedIndex = optionSelections.getOrElse("ui-font", 0),
      families = FontLoader.availableUiFamilies,
      intent = CommandIntent.SetUiFontFamily(_),
      hint = "Used in the app interface"
    )

  private def fontFamilyGroupItem(
    id: String,
    label: String,
    selectedIndex: Int,
    families: List[String],
    intent: String => CommandIntent,
    hint: String
  ): CommandSurfaceItem.GroupItem =
    val selectedFamily = families.lift(selectedIndex).orElse(families.headOption).getOrElse("")
    val children = families.zipWithIndex.map {
      case (family, index) =>
        CommandSurfaceItem.CommandItem(
          Command.typed(
            s"$id-$index-${family.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")}",
            hint,
            intent(family),
            CommandCategory.Settings,
            label = family
          )
        )
    }
    CommandSurfaceItem.GroupItem(
      id = id,
      label = label,
      children = children,
      category = CommandCategory.Settings,
      hint = Some(selectedFamily)
    )

  private[command] def codeLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "code-ligatures",
      label = "Ligature Shaping",
      options = List(
        CommandOption("On", CommandIntent.SetCodeLigatures(true)),
        CommandOption("Off", CommandIntent.SetCodeLigatures(false))
      ),
      selectedIndex = optionSelections.getOrElse("code-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] def textLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "text-ligatures",
      label = "Ligature Shaping",
      options = List(
        CommandOption("On", CommandIntent.SetTextLigatures(true)),
        CommandOption("Off", CommandIntent.SetTextLigatures(false))
      ),
      selectedIndex = optionSelections.getOrElse("text-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] def uiLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "ui-ligatures",
      label = "Ligature Shaping",
      options = List(
        CommandOption("On", CommandIntent.SetUiLigatures(true)),
        CommandOption("Off", CommandIntent.SetUiLigatures(false))
      ),
      selectedIndex = optionSelections.getOrElse("ui-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] def buildInputItems(config: AppConfig): List[CommandSurfaceItem.InputItem] =
    val durationValue     = config.characterAnimation.map(_.durationMs.toString).getOrElse("0")
    val stepsValue        = config.characterAnimation.map(_.steps.toString).getOrElse("0")
    val blurValue         = config.blurRadius.toString
    val codeFontSizeValue = config.fontConfig.codeFontSize.toString
    val textFontSizeValue = config.fontConfig.textFontSize.toString
    val uiFontSizeValue   = config.fontConfig.uiFontSize.toString

    val presetItems = List(
      CommandSurfaceItem.InputItem(
        id = "ui-preset-save",
        label = "Save Current Preset",
        hint = "Preset name",
        currentValue = "",
        isDecimal = false,
        parse = text => nonEmptyText(text).map(CommandIntent.SaveUiPreset(_)),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-apply",
        label = "Apply Preset",
        hint = "Preset name",
        currentValue = "",
        isDecimal = false,
        parse = text => nonEmptyText(text).map(CommandIntent.ApplyUiPreset(_)),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      )
    )

    val numericItems = List(
      CommandSurfaceItem.InputItem(
        id = "animation-duration",
        label = "Animation Duration",
        hint = "Milliseconds (0-10000)",
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
        label = "Animation Steps",
        hint = "Steps (0-100)",
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
        hint = "Strength (0.0-1.0)",
        currentValue = blurValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 0.0f && v <= 1.0f)
            .map(CommandIntent.SetBlurRadius(_)),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "code-font-size",
        label = "Code Font Size",
        hint = "Points (8.0-48.0)",
        currentValue = codeFontSizeValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 8.0f && v <= 48.0f)
            .map(CommandIntent.SetCodeFontSize(_)),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "text-font-size",
        label = "Prose Font Size",
        hint = "Points (8.0-48.0)",
        currentValue = textFontSizeValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 8.0f && v <= 48.0f)
            .map(CommandIntent.SetTextFontSize(_)),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-font-size",
        label = "UI Font Size",
        hint = "Points (8.0-48.0)",
        currentValue = uiFontSizeValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 8.0f && v <= 48.0f)
            .map(CommandIntent.SetUiFontSize(_)),
        category = CommandCategory.Settings
      )
    )

    presetItems ++ numericItems ++ buildKeymapInputItems(config)

  private def nonEmptyText(text: String): Option[String] =
    Option(text.trim).filter(_.nonEmpty)

  private def buildKeymapInputItems(config: AppConfig): List[CommandSurfaceItem.InputItem] =
    List(
      bindingInputItem(
        id = "keymap-global-command_palette",
        label = "Command Palette",
        currentValue = config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).headOption.map(_.render),
        parse = binding => CommandIntent.SetGlobalHotkey(HotkeyAction.ToggleCommandRunner, binding)
      ),
      bindingInputItem(
        id = "keymap-global-file_search",
        label = "File Search",
        currentValue = config.hotkeyConfig.bindingsFor(HotkeyAction.FileSearch).headOption.map(_.render),
        parse = binding => CommandIntent.SetGlobalHotkey(HotkeyAction.FileSearch, binding)
      ),
      bindingInputItem(
        id = "keymap-editor-page_down",
        label = "Editor Page Down",
        currentValue = config.focusedKeymapConfig.editor.bindingsFor(EditorKeyAction.PageDown).headOption.map(_.render),
        parse = binding => CommandIntent.SetEditorKeyBinding(EditorKeyAction.PageDown, binding)
      ),
      bindingInputItem(
        id = "keymap-command-runner-submit",
        label = "Command Submit",
        currentValue =
          config.focusedKeymapConfig.commandRunner.bindingsFor(CommandRunnerKeyAction.Submit).headOption.map(_.render),
        parse = binding => CommandIntent.SetCommandRunnerKeyBinding(CommandRunnerKeyAction.Submit, binding)
      ),
      bindingInputItem(
        id = "keymap-modal-dismiss",
        label = "Modal Dismiss",
        currentValue = config.focusedKeymapConfig.modal.bindingsFor(ModalKeyAction.Dismiss).headOption.map(_.render),
        parse = binding => CommandIntent.SetModalKeyBinding(ModalKeyAction.Dismiss, binding)
      ),
      bindingInputItem(
        id = "keymap-panel-activate",
        label = "Panel Activate",
        currentValue = config.focusedKeymapConfig.panel.bindingsFor(PanelKeyAction.Activate).headOption.map(_.render),
        parse = binding => CommandIntent.SetPanelKeyBinding(PanelKeyAction.Activate, binding)
      ),
      bindingInputItem(
        id = "keymap-peek-accept",
        label = "Peek Accept",
        currentValue = config.focusedKeymapConfig.peek.bindingsFor(PeekKeyAction.Accept).headOption.map(_.render),
        parse = binding => CommandIntent.SetPeekKeyBinding(PeekKeyAction.Accept, binding)
      )
    )

  private def bindingInputItem(
    id: String,
    label: String,
    currentValue: Option[String],
    parse: String => CommandIntent
  ): CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = id,
      label = label,
      hint = "Binding",
      currentValue = currentValue.getOrElse(""),
      isDecimal = false,
      parse = text => HotkeyTrigger.parse(text).map(_ => parse(text)),
      category = CommandCategory.Settings,
      acceptsBindingText = true
    )

  private def animationModeIndex(config: AppConfig): Int =
    config.characterAnimation match
      case None                                                          => 0
      case Some(animation) if AnimationConfig.subtle.contains(animation) => 1
      case _                                                             => 2

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

  private def interfaceDensityIndex(density: InterfaceDensity): Int =
    density match
      case InterfaceDensity.Compact     => 0
      case InterfaceDensity.Comfortable => 1
      case InterfaceDensity.Spacious    => 2

  private def markdownViewModeIndex(mode: MarkdownViewMode): Int =
    mode match
      case MarkdownViewMode.Source       => 0
      case MarkdownViewMode.SplitPreview => 1
      case MarkdownViewMode.InlineLens   => 2

  private def codeFontIndex(family: String): Int =
    FontLoader.availableMonospaceFamilies.indexOf(family) match
      case -1    => 0
      case index => index

  private def textFontIndex(family: String): Int =
    FontLoader.availableTextFamilies.indexOf(family) match
      case -1    => 0
      case index => index

  private def uiFontIndex(family: String): Int =
    FontLoader.availableUiFamilies.indexOf(family) match
      case -1    => 0
      case index => index

  private def ligaturesIndex(enabled: Boolean): Int =
    if enabled then 0 else 1

  private[command] val languageItems: List[CommandSurfaceItem] =
    val plainText = CommandSurfaceItem.CommandItem(
      Command.typed(
        "lang-plain-text",
        "Use plain text mode for the current buffer.",
        CommandIntent.SetBufferLanguage(None),
        CommandCategory.Settings,
        label = "Plain Text"
      )
    )
    val langItems = LanguageId.values.toList.sortBy(_.displayName).map { lang =>
      CommandSurfaceItem.CommandItem(
        Command.typed(
          s"lang-${lang.id}",
          s"Use ${lang.displayName} mode for the current buffer.",
          CommandIntent.SetBufferLanguage(Some(lang)),
          CommandCategory.Settings,
          label = lang.displayName
        )
      )
    }
    plainText :: langItems

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
