package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString

import com.serenity.state.models.Buffer

case class TextCaretStop(column: Int, xPx: Float)

case class TextVisualLine(
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    text: String,
    widthPx: Float,
    caretStops: Vector[TextCaretStop]
):
  def xForColumn(column: Int): Option[Float] =
    caretStops.find(_.column == column).map(_.xPx)

  def nearestColumnForXPx(xPx: Float): Int =
    caretStops.minBy(stop => math.abs(stop.xPx - xPx)).column

case class TextLayoutSnapshot(
    visualLines: Vector[TextVisualLine],
    panelWidthPx: Int,
    lineHeightPx: Int
):
  def xPxForCursor(cursor: com.serenity.state.models.CursorPosition): Option[Float] =
    visualLines.collectFirst {
      case line if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
        line.xForColumn(cursor.column).getOrElse(line.widthPx)
    }

  def cursorForVisualRowAndXPx(row: Int, xPx: Float): Option[com.serenity.state.models.CursorPosition] =
    visualLines.lift(row).map { line =>
      com.serenity.state.models.CursorPosition(line.bufferLine, line.nearestColumnForXPx(xPx))
    }

  def moveVertical(
    cursor: com.serenity.state.models.CursorPosition,
    direction: Int,
    preferredXPx: Float
  ): Option[com.serenity.state.models.CursorPosition] =
    visualLines.zipWithIndex.collectFirst {
      case (line, index)
          if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
        index + direction
    }.flatMap(targetRow => cursorForVisualRowAndXPx(targetRow, preferredXPx))

object TextLayoutSnapshot:

  def fromBuffer(
    buffer: Buffer,
    panelWidthPx: Int,
    font: Font
  ): TextLayoutSnapshot =
    val frc = new FontRenderContext(null, true, true)
    val lineHeightPx =
      math.max(1, math.ceil(font.getLineMetrics("Mg", frc).getHeight.toDouble).toInt)
    val visibleLogicalLines =
      buffer.viewport.topLine until math.min(
        buffer.content.lineCount,
        buffer.viewport.topLine + buffer.viewport.visibleLines
      )

    val visualLines = visibleLogicalLines.toVector.flatMap { lineIndex =>
      val line = buffer.content.getLine(lineIndex).getOrElse("")
      wrapLogicalLine(line, lineIndex, math.max(1, panelWidthPx), font, frc)
    }

    TextLayoutSnapshot(
      visualLines = visualLines,
      panelWidthPx = math.max(1, panelWidthPx),
      lineHeightPx = lineHeightPx
    )

  private def wrapLogicalLine(
    line: String,
    bufferLine: Int,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext
  ): Vector[TextVisualLine] =
    if line.isEmpty then Vector(shapeSegment("", bufferLine, 0, 0, font, frc))
    else
      def loop(startColumn: Int, acc: Vector[TextVisualLine]): Vector[TextVisualLine] =
        if startColumn >= line.length then acc
        else
          val remaining = line.substring(startColumn)
          val segmentLength = fittingSegmentLength(remaining, panelWidthPx, font, frc)
          val endColumn     = startColumn + segmentLength
          val segment       = line.substring(startColumn, endColumn)
          val visualLine    = shapeSegment(segment, bufferLine, startColumn, endColumn, font, frc)
          loop(endColumn, acc :+ visualLine)

      loop(0, Vector.empty)

  private def fittingSegmentLength(
    text: String,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext
  ): Int =
    val carets = caretXs(text, font, frc)
    val maxFitting =
      carets.zipWithIndex.takeWhile { case (x, _) => x <= panelWidthPx.toFloat }.map(_._2).lastOption.getOrElse(0)
    math.max(1, maxFitting)

  private def shapeSegment(
    text: String,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    font: Font,
    frc: FontRenderContext
  ): TextVisualLine =
    val xs = caretXs(text, font, frc)
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = startColumn,
      endColumn = endColumn,
      text = text,
      widthPx = xs.lastOption.getOrElse(0.0f),
      caretStops = xs.zipWithIndex.map { case (x, index) =>
        TextCaretStop(startColumn + index, x)
      }.toVector
    )

  private def caretXs(text: String, font: Font, frc: FontRenderContext): Vector[Float] =
    if text.isEmpty then Vector(0.0f)
    else
      val attributed = AttributedString(text)
      attributed.addAttribute(TextAttribute.FONT, font)
      val layout = TextLayout(attributed.getIterator, frc)
      val leadingCarets =
        (0 until text.length).toVector.map { index =>
          layout.getCaretInfo(TextHitInfo.leading(index))(0)
        }
      leadingCarets :+ layout.getAdvance
