package com.serenity.command

/** The commands a plain-text file's "convert to rich text?" prompt can run. */
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
