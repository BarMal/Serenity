package com.serenity.ui.layout

import java.awt.font.*
import java.awt.image.BufferedImage
import java.awt.{Font, RenderingHints}
import java.text.AttributedString

import com.serenity.richtext.{ParagraphAlignment, RichTextDocument}
import com.serenity.state.models.Buffer
import com.serenity.ui.fonts.FontLoader

case class TextCaretStop(column: Int, xPx: Float)

case class TextVisualLine(
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    text: String,
    widthPx: Float,
    caretStops: Vector[TextCaretStop],
    xOffsetPx: Float = 0.0f
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

  def visualLineForText(
    text: String,
    bufferLine: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    startColumn: Int = 0
  ): TextVisualLine =
    val measuredLayout = shouldUseMeasuredLayout(font, fontRenderContext)
    shapeSegment(text, bufferLine, startColumn, startColumn + text.length, font, fontRenderContext, measuredLayout)

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

  def visualLineIndexForCursor(
    lineText: String,
    cursorColumn: Int,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext()
  ): Int =
    val measuredLayout = shouldUseMeasuredLayout(font, fontRenderContext)
    wrapLogicalLine(lineText, 0, math.max(1, panelWidthPx), font, fontRenderContext, measuredLayout).zipWithIndex
      .collectFirst {
        case (line, index) if cursorColumn >= line.startColumn && cursorColumn <= line.endColumn => index
      }
      .getOrElse(0)

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
    val richDocument =
      buffer.richTextDocument.filter(_.matchesPlainText(buffer.content.collect()))
    val visualLineLimit = buffer.viewport.topVisualLine + buffer.viewport.visibleLines
    val visualLines =
      collectVisualLines(
        buffer,
        math.max(1, panelWidthPx),
        font,
        fontRenderContext,
        measuredLayout,
        visualLineLimit,
        richDocument
      ).drop(buffer.viewport.topVisualLine).take(buffer.viewport.visibleLines)

    TextLayoutSnapshot(
      visualLines = visualLines,
      panelWidthPx = math.max(1, panelWidthPx),
      lineHeightPx = lineHeightPx,
      ascentPx = ascentPx,
      isProportional = !FontLoader.isMonospacedFont(font),
      usesMeasuredLayout = measuredLayout
    )

  private def collectVisualLines(
    buffer: Buffer,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    visualLineLimit: Int,
    richDocument: Option[RichTextDocument]
  ): Vector[TextVisualLine] =
    @annotation.tailrec
    def loop(lineIndex: Int, acc: Vector[TextVisualLine]): Vector[TextVisualLine] =
      if lineIndex >= buffer.content.lineCount || acc.length >= visualLineLimit then acc
      else
        val rawLine = buffer.content.getLine(lineIndex).getOrElse("")
        val startColumn =
          if buffer.viewport.topVisualLine > 0 then 0
          else math.min(buffer.viewport.leftColumn, rawLine.length)
        val visibleSlice = rawLine.drop(startColumn)
        val wrapped =
          wrapLogicalLine(
            visibleSlice,
            lineIndex,
            panelWidthPx,
            font,
            frc,
            measuredLayout,
            startColumn
          )
        val aligned = applyParagraphAlignment(wrapped, lineIndex, panelWidthPx, richDocument)
        loop(lineIndex + 1, acc ++ aligned)

    loop(buffer.viewport.topLine, Vector.empty)

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
          val fittingLength    = fittingSegmentLength(remaining, panelWidthPx, font, frc, measuredLayout)
          val segmentLength    = wordBoundarySegmentLength(remaining, fittingLength)
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

  private def wordBoundarySegmentLength(text: String, fittingLength: Int): Int =
    if fittingLength >= text.length then text.length
    else
      val lastWhitespace = text.lastIndexWhere(_.isWhitespace, fittingLength - 1)
      if lastWhitespace > 0 then lastWhitespace + 1 else fittingLength

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

  private def applyParagraphAlignment(
    lines: Vector[TextVisualLine],
    lineIndex: Int,
    panelWidthPx: Int,
    richDocument: Option[RichTextDocument]
  ): Vector[TextVisualLine] =
    val alignment = richDocument
      .flatMap(_.paragraphs.lift(lineIndex))
      .map(_.alignment)
      .getOrElse(ParagraphAlignment.Left)

    lines.map(line => applyAlignment(line, alignment, panelWidthPx))

  private def applyAlignment(
    line: TextVisualLine,
    alignment: ParagraphAlignment,
    panelWidthPx: Int
  ): TextVisualLine =
    val availablePx = math.max(0.0f, panelWidthPx.toFloat - line.widthPx)
    val offsetPx =
      alignment match
        case ParagraphAlignment.Left | ParagraphAlignment.Justify => 0.0f
        case ParagraphAlignment.Center                            => availablePx / 2.0f
        case ParagraphAlignment.Right                             => availablePx

    if offsetPx <= 0.0f then line
    else
      line.copy(
        caretStops = line.caretStops.map(stop => stop.copy(xPx = stop.xPx + offsetPx)),
        xOffsetPx = offsetPx
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
      val normalized = rawXs.toArray
      val epsilon    = 0.01f

      @annotation.tailrec
      def plateauEndFrom(index: Int, plateauValue: Float): Int =
        if index + 1 < normalized.length && math.abs(normalized(index + 1) - plateauValue) <= epsilon then
          plateauEndFrom(index + 1, plateauValue)
        else index

      @annotation.tailrec
      def normalizeFrom(index: Int): Unit =
        if index < normalized.length - 1 then
          val plateauValue = normalized(index)
          if math.abs(normalized(index + 1) - plateauValue) <= epsilon then
            val plateauEnd = plateauEndFrom(index + 1, plateauValue)

            val plateauStart = index - 1
            val startX       = normalized(plateauStart)
            val endX         = normalized(plateauEnd)
            val segmentCount = plateauEnd - plateauStart

            if endX > startX && segmentCount > 0 then
              val step = (endX - startX) / segmentCount.toFloat
              (plateauStart + 1 to plateauEnd).foreach { pointIndex =>
                normalized(pointIndex) = startX + step * (pointIndex - plateauStart)
              }

            normalizeFrom(plateauEnd + 1)
          else normalizeFrom(index + 1)

      normalizeFrom(1)

      normalized.toVector

  def defaultFontRenderContext(): FontRenderContext =
    val image = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
      g.getFontRenderContext
    finally g.dispose()
