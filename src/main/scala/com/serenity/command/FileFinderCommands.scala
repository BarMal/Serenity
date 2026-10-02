package com.serenity.command

import java.nio.file.Path

/** "Go to File", and what picking a file in its finder runs. */
object FileFinderCommands:

  val goToFile: Command =
    Command.typed(
      "go-to-file",
      "Open a file under the project root by typing part of its name.",
      CommandIntent.File(FileIntent.GoToFile),
      CommandCategory.File,
      label = "Go to File"
    )

  def openFile(path: Path): Command =
    Command.typed(
      "open-found-file",
      s"Open $path.",
      CommandIntent.File(FileIntent.OpenRecentFile(path)),
      CommandCategory.File
    )
