package com.serenity.command

import com.serenity.command.CommandSurfaceItem.CommandItem
import com.serenity.ui.presets.UiPreset

class CommandRegistry(private val commands: List[Command]):

  private val searcher = new CommandSearcher(commands)

  private lazy val commandsByCategory: Map[CommandCategory, List[Command]] =
    CommandCategory.values.map { category =>
      val categoryCommands =
        category match
          case CommandCategory.All => commands
          case _                   => commands.filter(_.category == category)
      category -> categoryCommands
    }.toMap

  def getAllCommands: List[Command] = commands

  def searchCommands(term: String, maxResults: Int = 5): List[Command] =
    searcher.search(term, maxResults)

  def commandsForCategory(category: CommandCategory): List[Command] =
    commandsByCategory(category)

  def surfaceItemsForCategory(
    category: CommandCategory,
    optionSelections: Map[String, Int] = Map.empty
  ): List[CommandSurfaceItem] =
    val commandItems =
      commandsForCategory(category)
        .map(CommandItem(_))

    val optionItems =
      if category == CommandCategory.Settings then
        List(
          CommandRunnerSettingsCursorItems.cursorModeOptionItem(optionSelections),
          CommandRunnerSettingsAppearanceItems.backgroundStyleOptionItem(optionSelections),
          CommandRunnerSettingsAppearanceItems.postProcessingOptionItem(optionSelections),
          CommandRunnerSettingsAppearanceItems.uiShadowsOptionItem(optionSelections)
        )
      else Nil

    optionItems ++ commandItems

  def searchSurfaceItems(
    term: String,
    optionSelections: Map[String, Int] = Map.empty,
    maxResults: Int = 50
  ): List[CommandSurfaceItem] =
    val commandItems = searchCommands(term, maxResults).map(CommandItem(_))
    val optionItems = List(
      CommandRunnerSettingsCursorItems.cursorModeOptionItem(optionSelections),
      CommandRunnerSettingsAppearanceItems.backgroundStyleOptionItem(optionSelections),
      CommandRunnerSettingsAppearanceItems.postProcessingOptionItem(optionSelections),
      CommandRunnerSettingsAppearanceItems.uiShadowsOptionItem(optionSelections)
    ).filter { item =>
      val lowerTerm = term.toLowerCase
      lowerTerm.isEmpty || item.searchText.toLowerCase.contains(lowerTerm)
    }

    (optionItems ++ commandItems).take(maxResults)

  /** Find a command by exact name */
  def findCommand(name: String): Option[Command] =
    commands.find(_.name == name)

object CommandRegistry:

  def apply(commands: List[Command]): CommandRegistry = new CommandRegistry(commands)

  /** Registry with default commands. The command list is static, so this is built once and reused. */
  lazy val default: CommandRegistry = new CommandRegistry(defaultCommands)

  /** Registry with default commands plus UI toggle commands. The command list is static, so this is built once and
    * reused.
    */
  lazy val withToggleUI: CommandRegistry = new CommandRegistry(defaultCommands ++ toggleUICommands)

  /** Pure typed UI toggle commands. */
  private def toggleUICommands: List[Command] = List(
    Command.typed(
      "toggle-line-numbers",
      "Show or hide line numbers.",
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleLineNumbers)),
      CommandCategory.View,
      label = "Toggle Line Numbers"
    ),
    Command.typed(
      "toggle-status-line",
      "Show or hide the status line: the pinned gutter row at the bottom, or the floating row at the caret.",
      CommandIntent.Settings(SettingsIntent.StatusLine(StatusLineIntent.ToggleVisibility)),
      CommandCategory.View,
      label = "Toggle Status Line"
    ),
    Command.typed(
      "toggle-pane-headers",
      "Show or hide pane header bars.",
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.TogglePaneHeaders)),
      CommandCategory.View,
      label = "Toggle Pane Headers"
    ),
    Command.typed(
      "toggle-visual-line-navigation",
      "Move Up/Down by visual row instead of jumping straight to the previous/next logical line under word wrap.",
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleVisualLineCursorNavigation)),
      CommandCategory.View,
      label = "Toggle Visual Line Navigation"
    ),
    Command.typed(
      "toggle-typewriter-scrolling",
      "Keep the cursor's line vertically centred as you type, padding past the document's end.",
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleTypewriterScrolling)),
      CommandCategory.View,
      label = "Toggle Typewriter Scrolling"
    ),
    Command.typed(
      "toggle-line-wrap",
      "Soft-wrap long logical lines to the editor width (word wrap).",
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleWordWrap)),
      CommandCategory.View,
      label = "Toggle Line Wrap"
    ),
    Command.typed(
      "toggle-text-body-focus",
      "Dim text outside the current paragraph or code block.",
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleFocusedTextBody)),
      CommandCategory.View,
      label = "Toggle Text Body Focus"
    ),
    Command.typed(
      "toggle-contextual-toolbar",
      "Show or hide the floating rich-text formatting toolbar near the cursor.",
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleContextualToolbar)),
      CommandCategory.View,
      label = "Toggle Contextual Toolbar"
    )
  )

  /** Default set of editor commands.
    *
    * Split by domain across `CommandRegistry*Commands` objects in this package to keep both this method and each split
    * file under the architecture size targets -- the concatenation order below is the exact order the commands appeared
    * in before the split, so behavior (search ranking, palette default ordering) is unaffected.
    */
  private def defaultCommands: List[Command] =
    CommandRegistryFileCommands.fileCommands ++
      CommandRegistryFileCommands.sessionAndTabCommands ++
      CommandRegistryEditCommands.editCommands ++
      CommandRegistryEditCommands.richTextCommands ++
      CommandRegistryNavigationCommands.commentAndBookmarkCommands ++
      CommandRegistryNavigationCommands.navigationAndLspCommands ++
      CommandRegistryViewSettingsCommands.themeAndViewCommands ++
      CommandRegistryViewSettingsCommands.markdownAndModeCommands ++
      CommandRegistryPanelProjectCommands.panelFocusCommands ++
      CommandRegistryPanelProjectCommands.paneCommands ++
      CommandRegistryPanelProjectCommands.projectCommands ++
      builtInPresetCommands

  private def builtInPresetCommands: List[Command] =
    UiPreset.builtIns.map { preset =>
      val preview = UiPreset.Preview.fromPreset(preset)
      Command.typed(
        name = s"apply-${preset.name.toLowerCase.replace(' ', '-')}-preset",
        description = s"Apply the ${preset.name} workspace preset: ${preview.hint}.",
        intent = CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(preset.name)),
        category = CommandCategory.Settings,
        label = s"Apply ${preset.name} Preset"
      )
    }
