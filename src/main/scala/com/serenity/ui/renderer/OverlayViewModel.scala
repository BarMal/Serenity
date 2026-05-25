package com.serenity.ui.renderer

import com.serenity.state.models.AppState
import com.serenity.ui.layout.{CalculatedLayout, PeekContent, LayoutRect}

case class TextOverlayView(
    rect: LayoutRect,
    lines: List[String]
)

case class OverlayViews(
    aboveCursor: Option[TextOverlayView] = None,
    belowCursor: Option[TextOverlayView] = None
)

object OverlayViewModel:

  def fromState(state: AppState, layout: CalculatedLayout): OverlayViews =
    val aboveCursor =
      for
        overlay <- state.peekOverlay
        rect    <- layout.aboveCursorOverlayRect
      yield TextOverlayView(rect, peekContentLines(overlay.content))

    OverlayViews(
      aboveCursor = aboveCursor,
      belowCursor = None
    )

  private def peekContentLines(content: PeekContent): List[String] =
    content match
      case PeekContent.QuickInfo(text) =>
        text.linesIterator.toList match
          case Nil   => List("")
          case lines => lines
      case PeekContent.FilePreview(path, content) =>
        (s"Preview: ${path.getFileName}" :: content.linesIterator.take(4).toList).take(5)
      case PeekContent.SymbolDefinition(symbol, location) =>
        List(
          s"Symbol: $symbol",
          s"Line ${location.line + 1}, Col ${location.column + 1}"
        )
      case PeekContent.DirectoryListing(path, entries) =>
        val header = s"Directory: ${path.getFileName}"
        header :: entries.take(4).map(_.name)
