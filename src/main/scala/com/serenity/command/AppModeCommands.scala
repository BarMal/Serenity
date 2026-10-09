package com.serenity.command

import com.serenity.config.AppMode

/** The command the project-task prompt runs when the writer agrees to leave code mode. */
object AppModeCommands:

  def switchStoppingProjectTask(mode: AppMode): Command =
    Command.typed(
      s"app-mode-${mode.configKey}-stopping-project-task",
      s"Stop the running project task and switch to ${mode.configKey} mode",
      CommandIntent.View(ViewIntent.SetAppModeStoppingProjectTask(mode)),
      CommandCategory.Settings
    )
