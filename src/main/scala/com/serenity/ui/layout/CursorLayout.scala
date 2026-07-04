package com.serenity.ui.layout

import com.serenity.rope.Rope
import com.serenity.state.models.{CursorPosition, Viewport}

case class ScreenPosition(x: Int, y: Int)

object CursorLayout:

  def contentRectForPane(paneRect: LayoutRect): LayoutRect =
    LayoutEngine.contentRectForPane(paneRect)

  def calculateVisualPosition(
    cursor: CursorPosition,
    rope: Rope,
    panelWidth: Int,
    viewport: Viewport,
    wordWrapEnabled: Boolean = true
  ): Option[(Int, Int)] =

    def findCursorPosition(bufferLine: Int, currentVisualLine: Int): Option[(Int, Int)] =
      if bufferLine >= rope.lineCount then None
      else if bufferLine == cursor.line then
        val visualLineInBuffer = cursor.column / panelWidth
        val visualColumnInLine = cursor.column % panelWidth
        val totalVisualLine    = currentVisualLine + visualLineInBuffer
        Some((totalVisualLine, visualColumnInLine))
      else
        val lineContent             = rope.getLine(bufferLine).getOrElse("")
        val visualLinesInThisBuffer = math.max(1, (lineContent.length + panelWidth - 1) / panelWidth)
        findCursorPosition(bufferLine + 1, currentVisualLine + visualLinesInThisBuffer)

    if panelWidth <= 0 || cursor.line < viewport.topLine then None
    else if wordWrapEnabled then findCursorPosition(viewport.topLine, 0)
    else if cursor.line >= rope.lineCount then None
    else Some((cursor.line - viewport.topLine, cursor.column - viewport.leftColumn))

  def calculateScreenPosition(
    cursor: CursorPosition,
    rope: Rope,
    paneRect: LayoutRect,
    viewport: Viewport,
    wordWrapEnabled: Boolean = true
  ): Option[ScreenPosition] =
    calculateScreenPositionInContent(cursor, rope, contentRectForPane(paneRect), viewport, wordWrapEnabled)

  def calculateScreenPositionInContent(
    cursor: CursorPosition,
    rope: Rope,
    contentRect: LayoutRect,
    viewport: Viewport,
    wordWrapEnabled: Boolean = true
  ): Option[ScreenPosition] =
    calculateVisualPosition(cursor, rope, contentRect.width, viewport, wordWrapEnabled).map {
      case (visualLine, visualColumn) =>
        val viewportTopVisualLine = if wordWrapEnabled then viewport.topVisualLine else 0
        ScreenPosition(
          x = contentRect.x + visualColumn,
          y = contentRect.y + (visualLine - viewportTopVisualLine)
        )
    }
