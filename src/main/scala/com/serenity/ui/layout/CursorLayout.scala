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
    viewport: Viewport
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

    if panelWidth <= 0 then None
    else findCursorPosition(0, 0)

  def calculateScreenPosition(
    cursor: CursorPosition,
    rope: Rope,
    paneRect: LayoutRect,
    viewport: Viewport
  ): Option[ScreenPosition] =
    calculateScreenPositionInContent(cursor, rope, contentRectForPane(paneRect), viewport)

  def calculateScreenPositionInContent(
    cursor: CursorPosition,
    rope: Rope,
    contentRect: LayoutRect,
    viewport: Viewport
  ): Option[ScreenPosition] =
    calculateVisualPosition(cursor, rope, contentRect.width, viewport).map {
      case (visualLine, visualColumn) =>
        ScreenPosition(
          x = contentRect.x + visualColumn,
          y = contentRect.y + (visualLine - viewport.topLine)
        )
    }
