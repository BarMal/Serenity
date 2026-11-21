package com.serenity.command

import com.serenity.config.AppMode

object AppModeCommands:

  def switchTo(mode: AppMode): Command =
    val name = mode.configKey.capitalize
    Command.typed(
      s"switch-to-${mode.configKey}-mode",
      s"Change the app mode to $name.",
      CommandIntent.View(ViewIntent.SetAppMode(mode)),
      CommandCategory.View,
      label = s"Switch to $name Mode"
    )

  /** What someone looks for when they want the other mode: a toggle, which no mode-specific command can be named for.
    */
  val toggle: Command =
    Command.typed(
      "toggle-app-mode",
      "Switch between Prose and Code mode.",
      CommandIntent.View(ViewIntent.ToggleAppMode),
      CommandCategory.View,
      label = "Toggle Prose/Code Mode"
    )

  val switchCommands: List[Command] = toggle :: List(AppMode.Prose, AppMode.Code).map(switchTo)

  /** The command the project-task prompt runs when the writer agrees to leave code mode. */
  def switchStoppingProjectTask(mode: AppMode): Command =
    Command.typed(
      s"app-mode-${mode.configKey}-stopping-project-task",
      s"Stop the running project task and switch to ${mode.configKey} mode",
      CommandIntent.View(ViewIntent.SetAppModeStoppingProjectTask(mode)),
      CommandCategory.Settings
    )
