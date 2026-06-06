package com.serenity.keystroke.events

import java.nio.file.Path

import com.serenity.ui.layout.{DirEntry, PanelPosition}

enum ExplorerEvent extends SystemEvent:

  case RootDirectoryLoaded(
      position: PanelPosition,
      rootPath: Path,
      size: Int,
      entries: List[DirEntry],
      selectedPath: Option[Path]
  )

  case DirectoryLoaded(
      position: PanelPosition,
      path: Path,
      entries: List[DirEntry]
  )
