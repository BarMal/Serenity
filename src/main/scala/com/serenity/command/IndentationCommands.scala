package com.serenity.command

import com.serenity.state.models.{BufferId, IndentationSettings}

/** Choosing how one document is indented (#1996): tabs or spaces and their width, or handing the choice back to what
  * the document itself shows.
  */
object IndentationCommands:

  val chooseIndentation: Command =
    Command.typed(
      "change-indentation",
      "Choose whether the current file is indented with spaces or tabs, and how wide.",
      CommandIntent.File(FileIntent.ChooseIndentation),
      CommandCategory.File,
      label = "Change Indentation"
    )

  def set(bufferId: BufferId, settings: IndentationSettings): Command =
    Command.typed(
      s"set-indentation-${IndentationSettings.configKey(settings).replace(':', '-')}",
      s"Indent the file with ${description(settings)}.",
      CommandIntent.File(FileIntent.SetIndentation(bufferId, Some(settings))),
      CommandCategory.File,
      label = s"Indent Using ${label(settings)}"
    )

  def useAutomatic(bufferId: BufferId): Command =
    Command.typed(
      "set-indentation-automatic",
      "Indent the file as its own text shows, or by the settings when it shows nothing.",
      CommandIntent.File(FileIntent.SetIndentation(bufferId, None)),
      CommandCategory.File,
      label = "Use Automatic Indentation"
    )

  private def label(settings: IndentationSettings): String =
    s"${if settings.insertSpaces then "Spaces" else "Tabs"}: ${settings.tabSize}"

  private def description(settings: IndentationSettings): String =
    if settings.insertSpaces then s"${settings.tabSize} spaces per level" else s"tabs ${settings.tabSize} columns wide"
