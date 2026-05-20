package com.serenity.ui.layout

import java.nio.file.Path

import com.serenity.state.models.CursorPosition

case class PeekOverlay(
    content: PeekContent,
    position: CursorPosition,
    dismissOnMove: Boolean = true
)

enum PeekContent:
  case QuickInfo(text: String)
  case FilePreview(path: Path, content: String)
  case SymbolDefinition(symbol: String, location: Location)
  case DirectoryListing(path: Path, entries: List[DirEntry])
