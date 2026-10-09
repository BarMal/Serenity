package com.serenity.command

import com.serenity.state.models.BufferId
import com.serenity.text.LineEnding

/** Choosing the line ending a buffer saves with (#1964), for a file read as one ending that should be written as
  * another, or a mixed file whose uniform ending the user wants to pick.
  */
object LineEndingCommands:

  val chooseLineEnding: Command =
    Command.typed(
      "change-line-ending",
      "Choose the line ending (LF, CRLF or CR) the current file is saved with.",
      CommandIntent.File(FileIntent.ChooseLineEnding),
      CommandCategory.File,
      label = "Change Line Ending"
    )

  def set(bufferId: BufferId, ending: LineEnding): Command =
    Command.typed(
      s"set-line-ending-${ending.configKey}",
      s"Save the file with ${ending.label} line endings.",
      CommandIntent.File(FileIntent.SetLineEnding(bufferId, ending)),
      CommandCategory.File,
      label = s"Line Ending: ${ending.label}"
    )
