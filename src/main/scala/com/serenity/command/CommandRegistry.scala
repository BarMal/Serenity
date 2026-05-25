package com.serenity.command

import cats.effect.IO
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

  /** Create registry with StateManager reference for commands that need state updates */
  def withToggleUIStateful(stateManager: StateManager): CommandRegistry =
    new CommandRegistry(defaultCommands ++ createToggleUICommands(stateManager))

  /** UI toggle commands - these need access to StateManager for actual functionality */
  private def toggleUICommands: List[Command] = List(
    Command(
      "toggle-line-numbers",
      "Toggle line numbers display on/off",
      state => IO.println("[CMD] Toggle line numbers executed - state update needed")
    ),
    Command(
      "toggle-gutter",
      "Toggle status gutter display on/off",
      state => IO.println("[CMD] Toggle gutter executed - state update needed")
    )
  )

  /** Create toggle UI commands with actual StateManager integration */
  private def createToggleUICommands(stateManager: StateManager): List[Command] = List(
    Command(
      "toggle-line-numbers",
      "Toggle line numbers display on/off",
      state =>
        stateManager.updateState(s =>
          s.copy(config =
            s.config.copy(
              showLineNumbers = !s.config.showLineNumbers
            )
          )
        )
    ),
    Command(
      "toggle-gutter",
      "Toggle status gutter display on/off",
      state =>
        stateManager.updateState(s =>
          s.copy(config =
            s.config.copy(
              showGutter = !s.config.showGutter
            )
          )
        )
    )
  )

  /** Default set of editor commands */
  private def defaultCommands: List[Command] = List(
    Command(
      "save",
      "Save current file",
      state => IO.println("[CMD] Save executed")
    ),
    Command(
      "save-as",
      "Save file with new name",
      state => IO.println("[CMD] Save As executed")
    ),
    Command(
      "open",
      "Open file",
      state => IO.println("[CMD] Open executed")
    ),
    Command(
      "quit",
      "Quit application",
      state => IO.println("[CMD] Quit executed")
    ),
    Command(
      "new",
      "Create new file",
      state => IO.println("[CMD] New file executed")
    ),
    Command(
      "close",
      "Close current file",
      state => IO.println("[CMD] Close file executed")
    ),
    Command(
      "find",
      "Find text in file",
      state => IO.println("[CMD] Find executed")
    ),
    Command(
      "replace",
      "Find and replace text",
      state => IO.println("[CMD] Replace executed")
    ),
    Command(
      "goto-line",
      "Go to specific line number",
      state => IO.println("[CMD] Go to line executed")
    ),
    Command(
      "toggle-theme",
      "Switch between light and dark theme",
      state => IO.println("[CMD] Toggle theme executed")
    ),
    Command(
      "reload-theme",
      "Reload theme configuration",
      state => IO.println("[CMD] Reload theme executed")
    ),
    Command(
      "format",
      "Format current file",
      state => IO.println("[CMD] Format executed")
    ),
    Command(
      "animation-none",
      "Disable character animations",
      state => IO.println("[CMD] Animation disabled")
    ),
    Command(
      "animation-quick",
      "Enable quick character animations",
      state => IO.println("[CMD] Quick animations enabled")
    ),
    Command(
      "animation-smooth",
      "Enable smooth character animations",
      state => IO.println("[CMD] Smooth animations enabled")
    ),
    Command(
      "animation-subtle",
      "Enable subtle character animations",
      state => IO.println("[CMD] Subtle animations enabled")
    )
  )
