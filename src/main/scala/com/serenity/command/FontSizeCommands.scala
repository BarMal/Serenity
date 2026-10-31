package com.serenity.command

/** What the answers to the Font Sizes group run. Not in the palette: each exists to be run by a settings row or by the
  * Set All Font Sizes prompt that holds the size it asked about.
  */
object FontSizeCommands:

  def setAll(size: Float): Command =
    Command.typed(
      "font-sizes.set-all",
      s"Set the prose, code and UI font sizes to $size.",
      intent(FontIntent.SetAllFontSizes(size)),
      CommandCategory.Settings,
      label = "Set All Sizes"
    )

  val undo: Command =
    Command.typed(
      "undo-font-size-change",
      "Restore the prose, code and UI font sizes from before the last size change.",
      intent(FontIntent.UndoFontSizeChange),
      CommandCategory.Settings,
      label = "Undo Last Size Change",
      keepMenuOpenOnSubmit = true
    )

  private def intent(font: FontIntent): CommandIntent = CommandIntent.Settings(SettingsIntent.Font(font))
