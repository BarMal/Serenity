package com.serenity.ui.layout

import java.awt.font.*
import java.awt.image.BufferedImage
import java.awt.{Font, RenderingHints}
import java.text.AttributedString

import com.serenity.state.models.Buffer
import com.serenity.ui.fonts.FontLoader

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
    lineHeightPx: Int,
    ascentPx: Int,
    isProportional: Boolean = false,
    usesMeasuredLayout: Boolean = false
):

  def xPxForCursor(cursor: com.serenity.state.models.CursorPosition): Option[Float] =
    visualLines.collectFirst {
      case line
          if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
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
    visualLines.zipWithIndex
      .collectFirst {
        case (line, index)
            if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
          index + direction
      }
      .flatMap(targetRow => cursorForVisualRowAndXPx(targetRow, preferredXPx))

object TextLayoutSnapshot:

  def caretXsForText(
    text: String,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext()
  ): Vector[Float] =
    val measuredLayout = shouldUseMeasuredLayout(font, fontRenderContext)
    caretXs(text, font, fontRenderContext, measuredLayout)

  def leftColumnForCursorVisibility(
    lineText: String,
    cursorColumn: Int,
    visibleWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext()
  ): Int =
    if lineText.isEmpty || visibleWidthPx <= 0 then 0
    else
      val measuredLayout = shouldUseMeasuredLayout(font, fontRenderContext)
      val xs             = caretXs(lineText, font, fontRenderContext, measuredLayout)
      val safeColumn     = cursorColumn.max(0).min(lineText.length)
      val cursorXPx      = xs.lift(safeColumn).getOrElse(xs.lastOption.getOrElse(0.0f))
      val targetLeftXPx  = math.max(0.0f, cursorXPx - visibleWidthPx.toFloat + 1.0f)
      xs.zipWithIndex.takeWhile { case (x, _) => x <= targetLeftXPx }.map(_._2).lastOption.getOrElse(0)

  def fromBuffer(
    buffer: Buffer,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext()
  ): TextLayoutSnapshot =
    val measuredLayout =
      shouldUseMeasuredLayout(font, fontRenderContext)
    val lineHeightPx =
      math.max(1, math.ceil(font.getLineMetrics("Mg", fontRenderContext).getHeight.toDouble).toInt)
    val ascentPx =
      math.max(1, math.ceil(font.getLineMetrics("Mg", fontRenderContext).getAscent.toDouble).toInt)
    val visibleLogicalLines =
      buffer.viewport.topLine until math.min(
        buffer.content.lineCount,
        buffer.viewport.topLine + buffer.viewport.visibleLines
      )

    val visualLines = visibleLogicalLines.toVector.flatMap { lineIndex =>
      val rawLine      = buffer.content.getLine(lineIndex).getOrElse("")
      val startColumn  = math.min(buffer.viewport.leftColumn, rawLine.length)
      val visibleSlice = rawLine.drop(startColumn)
      wrapLogicalLine(
        visibleSlice,
        lineIndex,
        math.max(1, panelWidthPx),
        font,
        fontRenderContext,
        measuredLayout,
        startColumn
      )
    }

    TextLayoutSnapshot(
      visualLines = visualLines,
      panelWidthPx = math.max(1, panelWidthPx),
      lineHeightPx = lineHeightPx,
      ascentPx = ascentPx,
      isProportional = !FontLoader.isMonospacedFont(font),
      usesMeasuredLayout = measuredLayout
    )

  private def wrapLogicalLine(
    line: String,
    bufferLine: Int,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    baseColumn: Int = 0
  ): Vector[TextVisualLine] =
    if line.isEmpty then Vector(shapeSegment("", bufferLine, baseColumn, baseColumn, font, frc, measuredLayout))
    else
      def loop(startColumn: Int, acc: Vector[TextVisualLine]): Vector[TextVisualLine] =
        if startColumn >= line.length then acc
        else
          val remaining        = line.substring(startColumn)
          val segmentLength    = fittingSegmentLength(remaining, panelWidthPx, font, frc, measuredLayout)
          val endColumnInSlice = startColumn + segmentLength
          val segment          = line.substring(startColumn, endColumnInSlice)
          val segmentStart     = baseColumn + startColumn
          val segmentEnd       = baseColumn + endColumnInSlice
          val visualLine       = shapeSegment(segment, bufferLine, segmentStart, segmentEnd, font, frc, measuredLayout)
          loop(endColumnInSlice, acc :+ visualLine)

      loop(0, Vector.empty)

  private def fittingSegmentLength(
    text: String,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean
  ): Int =
    val carets = caretXs(text, font, frc, measuredLayout)
    val maxFitting =
      carets.zipWithIndex.takeWhile { case (x, _) => x <= panelWidthPx.toFloat }.map(_._2).lastOption.getOrElse(0)
    math.max(1, maxFitting)

  private def shapeSegment(
    text: String,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean
  ): TextVisualLine =
    val xs = caretXs(text, font, frc, measuredLayout)
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = startColumn,
      endColumn = endColumn,
      text = text,
      widthPx = xs.lastOption.getOrElse(0.0f),
      caretStops = xs.zipWithIndex.map {
        case (x, index) =>
          TextCaretStop(startColumn + index, x)
      }.toVector
    )

  private def caretXs(text: String, font: Font, frc: FontRenderContext, measuredLayout: Boolean): Vector[Float] =
    if text.isEmpty then Vector(0.0f)
    else if !measuredLayout then
      val charWidth = CellMetrics.fromFont(font).charWidth.toFloat
      Vector.tabulate(text.length + 1)(index => index * charWidth)
    else
      val attributed = AttributedString(text)
      attributed.addAttribute(TextAttribute.FONT, font)
      val layout = TextLayout(attributed.getIterator, frc)
      val leadingCarets =
        (0 until text.length).toVector.map(index => layout.getCaretInfo(TextHitInfo.leading(index))(0))
      normalizeCollapsedCarets(leadingCarets :+ layout.getAdvance)

  private def shouldUseMeasuredLayout(font: Font, frc: FontRenderContext): Boolean =
    !FontLoader.isMonospacedFont(font) ||
      FontLoader.ligaturesEnabled(font) ||
      hasFractionalAdvanceDrift(font, frc)

  private def hasFractionalAdvanceDrift(font: Font, frc: FontRenderContext): Boolean =
    val sampleText = "iiiiiiiiiiii"
    if sampleText.isEmpty then false
    else
      val measuredXs      = caretXs(sampleText, font, frc, measuredLayout = true)
      val measuredAdvance = measuredXs.lastOption.getOrElse(0.0f)
      val cellAdvance     = CellMetrics.fromFont(font).charWidth.toFloat * sampleText.length
      math.abs(measuredAdvance - cellAdvance) > 0.5f

  private def normalizeCollapsedCarets(rawXs: Vector[Float]): Vector[Float] =
    if rawXs.length < 3 then rawXs
    else
      val epsilon = 0.01f

      def plateauEndFrom(xs: Vector[Float], plateauValue: Float, index: Int): Int =
        if index + 1 < xs.length && math.abs(xs(index + 1) - plateauValue) <= epsilon then
          plateauEndFrom(xs, plateauValue, index + 1)
        else index

      def normalizePlateau(xs: Vector[Float], plateauStart: Int, plateauEnd: Int): Vector[Float] =
        val startX       = xs(plateauStart)
        val endX         = xs(plateauEnd)
        val segmentCount = plateauEnd - plateauStart

        if endX > startX && segmentCount > 0 then
          val step = (endX - startX) / segmentCount.toFloat
          (plateauStart + 1 to plateauEnd).foldLeft(xs) { (acc, pointIndex) =>
            acc.updated(pointIndex, startX + step * (pointIndex - plateauStart))
          }
        else xs

      def loop(xs: Vector[Float], index: Int): Vector[Float] =
        if index >= xs.length - 1 then xs
        else
          val plateauValue = xs(index)
          if math.abs(xs(index + 1) - plateauValue) <= epsilon then
            val plateauEnd = plateauEndFrom(xs, plateauValue, index + 1)
            loop(normalizePlateau(xs, index - 1, plateauEnd), plateauEnd + 1)
          else loop(xs, index + 1)

      loop(rawXs, 1)

  def defaultFontRenderContext(): FontRenderContext =
    val image = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
      g.getFontRenderContext
    finally g.dispose()
