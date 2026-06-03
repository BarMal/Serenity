package com.serenity.ui.layout

import com.serenity.rope.Rope
import com.serenity.state.models.{CursorPosition, Viewport}

case class ScreenPosition(x: Int, y: Int)

object CursorLayout:

  def contentRectForPane(paneRect: LayoutRect): LayoutRect =
    LayoutRect(
      paneRect.x,
      paneRect.y + 1,
      paneRect.width,
      math.max(1, paneRect.height - 1)
    )

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
    val contentRect = contentRectForPane(paneRect)

    calculateVisualPosition(cursor, rope, contentRect.width, viewport).map {
      case (visualLine, visualColumn) =>
        ScreenPosition(
          x = contentRect.x + visualColumn,
          y = contentRect.y + (visualLine - viewport.topLine)
        )
    }
