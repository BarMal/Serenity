package com.serenity.command

import com.serenity.state.models.ClipboardEntry

/** Pasting one of the recently copied or cut texts instead of the current clipboard (#1962). */
object ClipboardHistoryCommands:

  val choose: Command =
    Command.typed(
      "paste-from-history",
      "Pick one of the recently copied or cut texts to paste at the cursor.",
      CommandIntent.Edit(EditIntent.ChoosePasteFromHistory),
      CommandCategory.Edit,
      label = "Paste from History"
    )

  def paste(entry: ClipboardEntry): Command =
    Command.typed(
      "paste-history-entry",
      "Paste this recently copied text at the cursor.",
      CommandIntent.Edit(EditIntent.PasteFromHistory(entry)),
      CommandCategory.Edit,
      label = "Paste"
    )
