package com.serenity.command

import com.serenity.command.CommandSurfaceItem.CommandItem
import com.serenity.state.manager.StateManager

/** Registry of all available commands */
class CommandRegistry(private val commands: List[Command]):

  private val searcher = new CommandSearcher(commands)

  /** Get all registered commands */
  def getAllCommands: List[Command] = commands

  /** Search commands by term */
  def searchCommands(term: String, maxResults: Int = 5): List[Command] =
    searcher.search(term, maxResults)

  def commandsForCategory(category: CommandCategory): List[Command] =
    category match
      case CommandCategory.All => commands
      case _                   => commands.filter(_.category == category)

  def surfaceItemsForCategory(
    category: CommandCategory,
    optionSelections: Map[String, Int] = Map.empty
  ): List[CommandSurfaceItem] =
    val commandItems =
      commandsForCategory(category)
        .map(CommandItem(_))

    val optionItems =
      if category == CommandCategory.Settings then List(CommandRunner.animationOptionItem(optionSelections))
      else Nil

    optionItems ++ commandItems

  def searchSurfaceItems(
    term: String,
    optionSelections: Map[String, Int] = Map.empty,
    maxResults: Int = 50
  ): List[CommandSurfaceItem] =
    val commandItems = searchCommands(term, maxResults).map(CommandItem(_))
    val optionItems = List(CommandRunner.animationOptionItem(optionSelections)).filter { item =>
      val lowerTerm = term.toLowerCase
      lowerTerm.isEmpty || item.searchText.toLowerCase.contains(lowerTerm)
    }

    (optionItems ++ commandItems).take(maxResults)

  /** Find a command by exact name */
  def findCommand(name: String): Option[Command] =
    commands.find(_.name == name)

object CommandRegistry:

  /** Create registry with custom commands */
  def apply(commands: List[Command]): CommandRegistry = new CommandRegistry(commands)

  /** Create registry with default commands */
  def default: CommandRegistry = new CommandRegistry(defaultCommands)

  /** Create registry with default commands plus UI toggle commands */
  def withToggleUI: CommandRegistry = new CommandRegistry(defaultCommands ++ toggleUICommands)

  /** Compatibility alias during the reducer migration. */
  def withToggleUIStateful(stateManager: StateManager): CommandRegistry =
    withToggleUI

  /** Pure typed UI toggle commands. */
  private def toggleUICommands: List[Command] = List(
    Command.typed(
      "toggle-line-numbers",
      "Toggle line numbers display on/off",
      CommandIntent.ToggleLineNumbers,
      CommandCategory.View
    ),
    Command.typed(
      "toggle-gutter",
      "Toggle status gutter display on/off",
      CommandIntent.ToggleGutter,
      CommandCategory.View
    )
  )

  /** Default set of editor commands */
  private def defaultCommands: List[Command] = List(
    Command.typed(
      "save",
      "Save current file",
      CommandIntent.SaveCurrentFile,
      CommandCategory.File
    ),
    Command.typed(
      "save-as",
      "Save file with new name",
      CommandIntent.SaveCurrentFileAs,
      CommandCategory.File
    ),
    Command.typed(
      "open",
      "Open file",
      CommandIntent.OpenFile,
      CommandCategory.File
    ),
    Command.typed(
      "quit",
      "Quit application",
      CommandIntent.QuitApp,
      CommandCategory.File
    ),
    Command.typed(
      "new",
      "Create new file",
      CommandIntent.NewFile,
      CommandCategory.File
    ),
    Command.typed(
      "close",
      "Close current file",
      CommandIntent.CloseCurrentFile,
      CommandCategory.File
    ),
    Command.typed(
      "close-all",
      "Close all files",
      CommandIntent.CloseAll,
      CommandCategory.File
    ),
    Command.typed(
      "close-others",
      "Close all files except the current one",
      CommandIntent.CloseOthers,
      CommandCategory.File
    ),
    Command.typed(
      "find",
      "Find text in file",
      CommandIntent.FindInCurrentFile,
      CommandCategory.Edit
    ),
    Command.typed(
      "replace",
      "Find and replace text",
      CommandIntent.ReplaceInCurrentFile,
      CommandCategory.Edit
    ),
    Command.typed(
      "goto-line",
      "Go to specific line number",
      CommandIntent.OpenGotoLine,
      CommandCategory.Edit
    ),
    Command.typed(
      "toggle-theme",
      "Switch between light and dark theme",
      CommandIntent.ToggleTheme,
      CommandCategory.Settings
    ),
    Command.typed(
      "reload-theme",
      "Reload theme configuration",
      CommandIntent.ReloadTheme,
      CommandCategory.Settings
    ),
    Command.typed(
      "theme-chooser",
      "Choose theme with live preview",
      CommandIntent.OpenThemeChooser,
      CommandCategory.Settings
    ),
    Command.typed(
      "reload-themes",
      "Reload available themes from disk",
      CommandIntent.ReloadThemes,
      CommandCategory.Settings
    ),
    Command.typed(
      "format",
      "Format current file",
      CommandIntent.FormatCurrentFile,
      CommandCategory.Edit
    )
  )
