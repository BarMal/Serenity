package com.serenity.command

/** Which build is running and where its logs are (#2020); see DiagnosticsCommands. */
enum DiagnosticsIntent:
  case ShowAbout
  case OpenLogsFolder
  case CopyToClipboard(text: String)
