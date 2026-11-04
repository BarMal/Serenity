package com.serenity.command

import com.serenity.config.AppMode

object AppModeCommands:

  def switchTo(mode: AppMode): Command =
    val name = mode.configKey.capitalize
    Command.typed(
      s"switch-to-${mode.configKey}-mode",
      s"Switch the workspace to $name mode, which sets its typography, panels and tooling.",
      CommandIntent.View(ViewIntent.SetAppMode(mode)),
      CommandCategory.View,
      label = s"Switch to $name Mode"
    )

  val switchCommands: List[Command] = List(AppMode.Prose, AppMode.Code).map(switchTo)

  /** The command the project-task prompt runs when the writer agrees to leave code mode. */
  def switchStoppingProjectTask(mode: AppMode): Command =
    Command.typed(
      s"app-mode-${mode.configKey}-stopping-project-task",
      s"Stop the running project task and switch to ${mode.configKey} mode",
      CommandIntent.View(ViewIntent.SetAppModeStoppingProjectTask(mode)),
      CommandCategory.Settings
    )
