package com.serenity.command

import com.serenity.state.manager.StateManager

/** Registry of all available commands */
class CommandRegistry(private val commands: List[Command]):

  private val searcher = new CommandSearcher(commands)

  /** Get all registered commands */
  def getAllCommands: List[Command] = commands

  /** Search commands by term */
  def searchCommands(term: String, maxResults: Int = 5): List[Command] =
    searcher.search(term, maxResults)

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
      CommandIntent.ToggleLineNumbers
    ),
    Command.typed(
      "toggle-gutter",
      "Toggle status gutter display on/off",
      CommandIntent.ToggleGutter
    )
  )

  /** Default set of editor commands */
  private def defaultCommands: List[Command] = List(
    Command.typed(
      "save",
      "Save current file",
      CommandIntent.SaveCurrentFile
    ),
    Command.typed(
      "save-as",
      "Save file with new name",
      CommandIntent.SaveCurrentFileAs
    ),
    Command.typed(
      "open",
      "Open file",
      CommandIntent.OpenFile
    ),
    Command.typed(
      "quit",
      "Quit application",
      CommandIntent.QuitApp
    ),
    Command.typed(
      "new",
      "Create new file",
      CommandIntent.NewFile
    ),
    Command.typed(
      "close",
      "Close current file",
      CommandIntent.CloseCurrentFile
    ),
    Command.typed(
      "find",
      "Find text in file",
      CommandIntent.FindInCurrentFile
    ),
    Command.typed(
      "replace",
      "Find and replace text",
      CommandIntent.ReplaceInCurrentFile
    ),
    Command.typed(
      "goto-line",
      "Go to specific line number",
      CommandIntent.OpenGotoLine
    ),
    Command.typed(
      "toggle-theme",
      "Switch between light and dark theme",
      CommandIntent.ToggleTheme
    ),
    Command.typed(
      "reload-theme",
      "Reload theme configuration",
      CommandIntent.ReloadTheme
    ),
    Command.typed(
      "format",
      "Format current file",
      CommandIntent.FormatCurrentFile
    ),
    Command.typed(
      "animation-none",
      "Disable character animations",
      CommandIntent.SetAnimationMode(AnimationMode.None)
    ),
    Command.typed(
      "animation-quick",
      "Enable quick character animations",
      CommandIntent.SetAnimationMode(AnimationMode.Quick)
    ),
    Command.typed(
      "animation-smooth",
      "Enable smooth character animations",
      CommandIntent.SetAnimationMode(AnimationMode.Smooth)
    ),
    Command.typed(
      "animation-subtle",
      "Enable subtle character animations",
      CommandIntent.SetAnimationMode(AnimationMode.Subtle)
    )
  )
