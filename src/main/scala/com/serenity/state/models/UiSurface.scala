package com.serenity.state.models

import java.nio.file.Path

import com.serenity.command.CommandRunner
import com.serenity.ui.layout.*

case class SurfaceId(value: String)

case class StartupPage(
    title: String,
    options: List[String],
    statusMessage: Option[String] = None
):
  def renderLines: List[String] =
    val baseLines = List(title, "") ++ options
    statusMessage match
      case Some(message) => baseLines ++ List("", message)
      case None          => baseLines

enum SurfacePlacement:
  case AboveCursor
  case BelowCursor

enum SurfacePresentation:
  case Floating(anchor: Option[CursorPosition], placement: SurfacePlacement)
  case Pinned(position: PanelPosition, size: Int)

enum SurfaceContent:
  case StartPage(page: StartupPage)
  case QuickInfo(text: String)
  case FilePreview(path: Path, content: String)
  case SymbolDefinition(symbol: String, location: Location)
  case DirectoryListing(path: Path, entries: List[DirEntry], selectedPath: Option[Path] = None)
  case CommandPalette(runner: CommandRunner)
  case ModalWorkflow(modal: Modal)
  case Terminal(buffer: String, cursor: Int)
  case Outline(symbols: List[Symbol])
  case Diagnostics(issues: List[Diagnostic])

case class UiSurface(
    id: SurfaceId,
    content: SurfaceContent,
    presentation: SurfacePresentation,
    dismissOnMove: Boolean = false
)

object UiSurface:

  def fromPanelContent(
    id: SurfaceId,
    content: PanelContent,
    position: PanelPosition,
    size: Int
  ): UiSurface =
    val surfaceContent = content match
      case PanelContent.DirectoryTree(tree, selectedPath) =>
        SurfaceContent.DirectoryListing(
          tree.rootPath,
          tree.entries.getOrElse(tree.rootPath, List.empty),
          selectedPath
        )
      case PanelContent.Terminal(buffer, cursor) =>
        SurfaceContent.Terminal(buffer, cursor)
      case PanelContent.Outline(symbols) =>
        SurfaceContent.Outline(symbols)
      case PanelContent.Diagnostics(issues) =>
        SurfaceContent.Diagnostics(issues)

    UiSurface(
      id = id,
      content = surfaceContent,
      presentation = SurfacePresentation.Pinned(position, size)
    )
