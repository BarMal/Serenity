package com.serenity.command

import com.serenity.state.models.BufferId

/** What picking a line in "Search in Open Files" runs. */
object NavigationCommands:

  def goToBufferLine(bufferId: BufferId, line: Int): Command =
    Command.typed(
      "go-to-buffer-line",
      "Go to a line of an open file.",
      CommandIntent.Navigation(NavigationIntent.GoToBufferLine(bufferId, line)),
      CommandCategory.Edit
    )
