package com.serenity.command

/** File and session management commands (open, save, tabs, close). Split out of `CommandRegistry.defaultCommands` to
  * keep both under the architecture size targets -- see that method's doc.
  */
private[command] object CommandRegistryFileCommands:

  private[command] val saveAs: Command =
    Command.typed(
      "save-as",
      "Save the current file under a new name.",
      CommandIntent.File(FileIntent.SaveCurrentFileAs),
      CommandCategory.File,
      label = "Save As"
    )

  private[command] def fileCommands: List[Command] = ManuscriptExportCommands.all ++ List(
    Command.typed(
      "open-settings",
      "Browse, search, inspect, and change application settings.",
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.OpenSettings)),
      CommandCategory.Settings,
      label = "Open Settings"
    ),
    Command.typed(
      "save",
      "Save the current file.",
      CommandIntent.File(FileIntent.SaveCurrentFile),
      CommandCategory.File,
      label = "Save"
    ),
    saveAs,
    ReopenWithEncodingCommands.chooseEncoding,
    Command.typed(
      "save-config",
      "Write the current settings using the latest config format.",
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SaveConfig)),
      CommandCategory.Settings,
      label = "Save Config"
    ),
    Command.typed(
      "reset-settings",
      "Back up config.conf, then restore every setting to its default.",
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.ResetSettings)),
      CommandCategory.Settings,
      label = "Reset Settings"
    ),
    Command.typed(
      "save-session",
      "Save the current editor session.",
      CommandIntent.Session(SessionIntent.SaveSession),
      CommandCategory.File,
      label = "Save Session"
    ),
    Command.typed(
      "restore-session",
      "Restore the last saved editor session.",
      CommandIntent.Session(SessionIntent.RestoreSession),
      CommandCategory.File,
      label = "Restore Session"
    ),
    Command.typed(
      "clear-session",
      "Clear the saved editor session.",
      CommandIntent.Session(SessionIntent.ClearSession),
      CommandCategory.File,
      label = "Clear Session"
    ),
    Command.typed(
      "return-to-start-page",
      "Snapshot the session and return to the start page (Tab resumes it).",
      CommandIntent.Session(SessionIntent.ReturnToStartPage),
      CommandCategory.File,
      label = "Return to Start Page"
    ),
    Command.typed(
      "save-session-as",
      "Save the current editor session under a new name, keeping it alongside your other named sessions.",
      CommandIntent.Session(SessionIntent.OpenSaveSessionAsPrompt),
      CommandCategory.File,
      label = "Save Session As..."
    ),
    Command.typed(
      "open-session",
      "Open one of your saved named sessions.",
      CommandIntent.Session(SessionIntent.OpenSessionPicker),
      CommandCategory.File,
      label = "Open Session..."
    ),
    Command.typed(
      "rename-session",
      "Rename one of your saved named sessions.",
      CommandIntent.Session(SessionIntent.OpenRenameSessionPicker),
      CommandCategory.File,
      label = "Rename Session..."
    ),
    Command.typed(
      "open",
      "Open a file.",
      CommandIntent.File(FileIntent.OpenFile),
      CommandCategory.File,
      label = "Open File"
    ),
    FileFinderCommands.goToFile
  )

  private[command] def sessionAndTabCommands: List[Command] = List(
    Command.typed(
      "file-search",
      "Search the text of every open file.",
      CommandIntent.File(FileIntent.OpenFileSearch),
      CommandCategory.File,
      label = "Search in Open Files"
    ),
    Command.typed(
      "quit",
      "Quit the application.",
      CommandIntent.Lifecycle(LifecycleIntent.QuitApp),
      CommandCategory.File,
      label = "Quit"
    ),
    SafeModeCommands.restart,
    SafeModeCommands.restartNormally,
    Command.typed(
      "new",
      "Create a new file.",
      CommandIntent.File(FileIntent.NewFile),
      CommandCategory.File,
      label = "New File"
    ),
    Command.typed(
      "show-licence-and-notices",
      "Open the Serenity licence (GPL-3.0-or-later) and the third-party notices, read-only.",
      CommandIntent.File(FileIntent.ShowLicenceAndNotices),
      CommandCategory.File,
      label = "Show Licence and Notices"
    ),
    Command.typed(
      "about",
      "Open the About Serenity document: version, file locations, privacy statement, licence and notices.",
      CommandIntent.File(FileIntent.ShowAbout),
      CommandCategory.File,
      label = "About Serenity"
    ),
    Command.typed(
      "open-releases-page",
      "Open the Serenity releases page in the default browser.",
      CommandIntent.File(FileIntent.OpenReleasesPage),
      CommandCategory.File,
      label = "Open Releases Page"
    ),
    Command.typed(
      "next-tab",
      "Switch to the next open file.",
      CommandIntent.View(ViewIntent.NextTab),
      CommandCategory.File,
      label = "Next Tab"
    ),
    Command.typed(
      "previous-tab",
      "Switch to the previous open file.",
      CommandIntent.View(ViewIntent.PreviousTab),
      CommandCategory.File,
      label = "Previous Tab"
    ),
    Command.typed(
      "close",
      "Close the current file.",
      CommandIntent.File(FileIntent.CloseCurrentFile),
      CommandCategory.File,
      label = "Close File"
    ),
    Command.typed(
      "close-all",
      "Close all files.",
      CommandIntent.File(FileIntent.CloseAll),
      CommandCategory.File,
      label = "Close All Files"
    ),
    Command.typed(
      "close-others",
      "Close every file except the current one.",
      CommandIntent.File(FileIntent.CloseOthers),
      CommandCategory.File,
      label = "Close Other Files"
    )
  )
