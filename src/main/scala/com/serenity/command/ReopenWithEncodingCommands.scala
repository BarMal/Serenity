package com.serenity.command

import com.serenity.state.models.BufferId
import com.serenity.text.TextEncoding

/** Reading a file again in an encoding the user picks (#1627), for when the one detected on open was wrong. */
object ReopenWithEncodingCommands:

  val chooseEncoding: Command =
    Command.typed(
      "reopen-with-encoding",
      "Read the current file again from disk in an encoding you choose.",
      CommandIntent.File(FileIntent.ChooseReopenEncoding),
      CommandCategory.File,
      label = "Reopen with Encoding"
    )

  /** `discardEdits` answers the unsaved-changes prompt: without it, a buffer with unsaved edits asks first. */
  def reopen(bufferId: BufferId, encoding: TextEncoding, discardEdits: Boolean = false): Command =
    Command.typed(
      s"reopen-as-${encoding.configKey.toLowerCase}",
      s"Read the file again from disk as ${encoding.configKey}.",
      CommandIntent.File(FileIntent.ReopenWithEncoding(bufferId, encoding, discardEdits)),
      CommandCategory.File,
      label = s"Reopen as ${encoding.configKey}"
    )
