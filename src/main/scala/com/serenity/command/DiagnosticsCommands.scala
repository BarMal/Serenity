package com.serenity.command

/** The palette's way to see which build is running and to find its logs, and what the prompts offering the same run. */
object DiagnosticsCommands:

  val about: Command =
    Command.typed(
      "about-serenity",
      "Show the version, commit, operating system, JVM and toolkit this Serenity is running on.",
      CommandIntent.Diagnostics(DiagnosticsIntent.ShowAbout),
      CommandCategory.File,
      label = "About Serenity"
    )

  val openLogsFolder: Command =
    Command.typed(
      "open-logs-folder",
      "Open the folder holding Serenity's logs and crash reports in the system file manager.",
      CommandIntent.Diagnostics(DiagnosticsIntent.OpenLogsFolder),
      CommandCategory.File,
      label = "Open Logs Folder"
    )

  /** Not in the palette: it only exists to be run by a prompt that holds the text. */
  def copyToClipboard(text: String): Command =
    Command.typed(
      "diagnostics.copy-to-clipboard",
      "Copy the details shown to the clipboard.",
      CommandIntent.Diagnostics(DiagnosticsIntent.CopyToClipboard(text))
    )
