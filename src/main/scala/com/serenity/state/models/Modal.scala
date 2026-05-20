package com.serenity.state.models

import java.nio.file.Path

enum Modal:

  case CommandRunner(
      input: String,
      suggestions: List[Command],
      selectedIndex: Int
  )

  case FileSearch(
      query: String,
      results: List[Path],
      selectedIndex: Int
  )

  case GotoLine(
      input: String
  )

case class Command(
    name: String,
    description: String,
    action: CommandAction
)

enum CommandAction:
  case OpenFile(path: Path)
  case SaveBuffer
  case CloseBuffer
  case SplitPane
  case ToggleSidebar
  case Custom(handler: String)
