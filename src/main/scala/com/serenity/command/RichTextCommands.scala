package com.serenity.command

import com.serenity.state.models.BufferId

/** The commands a plain-text file's formatting prompts can run: "convert to rich text?" and "save without formatting?".
  */
object RichTextCommands:

  def convertToRichText(andThen: Option[RichTextIntent]): Command =
    Command.typed(
      "convert-to-rich-text",
      "Let the current file carry formatting. A plain-text file still saves without it; use Save As to keep it.",
      CommandIntent.RichText(RichTextIntent.ConvertToRichText(andThen)),
      CommandCategory.Edit,
      label = "Convert to Rich Text"
    )

  val saveAsRichDocument: Command = CommandRegistryFileCommands.saveAs

  def saveWithoutFormatting(bufferId: BufferId): Command =
    Command.typed(
      "save-without-formatting",
      "Drop the file's formatting and save it as plain text",
      CommandIntent.File(FileIntent.SaveWithoutFormatting(bufferId)),
      CommandCategory.File
    )
