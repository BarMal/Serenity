package com.serenity.command

/** File and session management commands (open, save, tabs, close). Split out of `CommandRegistry.defaultCommands`
  * to keep both under the architecture size targets -- see that method's doc.
  */
private[command] object CommandRegistryFileCommands:

  private[command] def fileCommands: List[Command] = List(
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
Command.typed(
  "save-as",
  "Save the current file under a new name.",
  CommandIntent.File(FileIntent.SaveCurrentFileAs),
  CommandCategory.File,
  label = "Save As"
),
Command.typed(
  "save-config",
  "Write the current settings using the latest config format.",
  CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SaveConfig)),
  CommandCategory.Settings,
  label = "Save Config"
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
  "open",
  "Open a file.",
  CommandIntent.File(FileIntent.OpenFile),
  CommandCategory.File,
  label = "Open File"
),
  )

  private[command] def sessionAndTabCommands: List[Command] = List(
Command.typed(
  "file-search",
  "Search for a file to open.",
  CommandIntent.File(FileIntent.OpenFileSearch),
  CommandCategory.File,
  label = "File Search"
),
Command.typed(
  "quit",
  "Quit the application.",
  CommandIntent.Lifecycle(LifecycleIntent.QuitApp),
  CommandCategory.File,
  label = "Quit"
),
Command.typed(
  "new",
  "Create a new file.",
  CommandIntent.File(FileIntent.NewFile),
  CommandCategory.File,
  label = "New File"
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
),
  )

