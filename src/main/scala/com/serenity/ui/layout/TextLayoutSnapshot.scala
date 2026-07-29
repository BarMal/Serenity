package com.serenity.ui.layout

import java.awt.font.*
import java.awt.image.BufferedImage
import java.awt.{Font, RenderingHints}
import java.text.AttributedString

import com.serenity.richtext.{ParagraphAlignment, RichTextDocument}
import com.serenity.state.models.Buffer
import com.serenity.text.TextEditing
import com.serenity.ui.fonts.FontLoader

case class TextCaretStop(column: Int, xPx: Float)

case class TextVisualLine(
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    text: String,
    widthPx: Float,
    caretStops: Vector[TextCaretStop],
    xOffsetPx: Float = 0.0f,
    xSortedCaretStops: Vector[TextCaretStop] = Vector.empty
):

  def xForColumn(column: Int): Option[Float] =
    @annotation.tailrec
    def search(low: Int, high: Int, best: Option[TextCaretStop]): Option[Float] =
      if low > high then best.orElse(caretStops.lastOption).map(_.xPx)
      else
        val middle = (low + high) >>> 1
        val stop   = caretStops(middle)
        if stop.column >= column then search(low, middle - 1, Some(stop))
        else search(middle + 1, high, best)

    search(0, caretStops.length - 1, None)

  def nearestColumnForXPx(xPx: Float): Int =
    val stops = if xSortedCaretStops.nonEmpty then xSortedCaretStops else caretStops.sortBy(_.xPx)

    @annotation.tailrec
    def search(low: Int, high: Int): TextCaretStop =
      if low > high then
        (stops.lift(high), stops.lift(low)) match
          case (Some(left), Some(right)) => closer(left, right, xPx)
          case (Some(left), None)        => left
          case (None, Some(right))       => right
          case (None, None)              => stops.head
      else
        val middle = (low + high) >>> 1
        if stops(middle).xPx < xPx then search(middle + 1, high)
        else search(low, middle - 1)

    if stops.isEmpty then 0 else search(0, stops.length - 1).column

  private def closer(first: TextCaretStop, second: TextCaretStop, xPx: Float): TextCaretStop =
    if math.abs(second.xPx - xPx) < math.abs(first.xPx - xPx) then second else first

case class TextLayoutSnapshot(
    visualLines: Vector[TextVisualLine],
    panelWidthPx: Int,
    lineHeightPx: Int,
    ascentPx: Int,
    isProportional: Boolean = false,
    usesMeasuredLayout: Boolean = false,
    richTextDocument: Option[RichTextDocument] = None
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
  private val UnwrappedOverscanColumns = 2
  private case class MeasuredLayoutKey(font: Font, fontRenderContext: FontRenderContext)
  private val measuredLayoutCache = java.util.concurrent.ConcurrentHashMap[MeasuredLayoutKey, java.lang.Boolean]()

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
      val safeColumn     = cursorColumn.max(0).min(lineText.length)
      if !measuredLayout then
        val charWidth      = math.max(1, CellMetrics.fromFont(font).charWidth)
        val visibleColumns = math.max(1, visibleWidthPx / charWidth)
        math.max(0, safeColumn - visibleColumns + 1)
      else
        val xs            = caretXs(lineText, font, fontRenderContext, measuredLayout)
        val cursorXPx     = xs.lift(safeColumn).getOrElse(xs.lastOption.getOrElse(0.0f))
        val targetLeftXPx = math.max(0.0f, cursorXPx - visibleWidthPx.toFloat + 1.0f)
        xs.zipWithIndex.takeWhile { case (x, _) => x <= targetLeftXPx }.map(_._2).lastOption.getOrElse(0)

  def visualLineIndexForCursor(
    lineText: String,
    cursorColumn: Int,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    wordWrapEnabled: Boolean = true
  ): Int =
    if !wordWrapEnabled then 0
    else
      val measuredLayout = shouldUseMeasuredLayout(font, fontRenderContext)
      wrapLogicalLine(lineText, 0, math.max(1, panelWidthPx), font, fontRenderContext, measuredLayout).zipWithIndex
        .collectFirst {
          case (line, index) if cursorColumn >= line.startColumn && cursorColumn <= line.endColumn => index
        }
        .getOrElse(0)

  private[serenity] def boundedVisualLinesForText(
    text: String,
    bufferLine: Int,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    baseColumn: Int = 0,
    maxVisualLines: Int = Int.MaxValue
  ): Vector[TextVisualLine] =
    val measuredLayout = shouldUseMeasuredLayout(font, fontRenderContext)
    wrapLogicalLine(
      text,
      bufferLine,
      math.max(1, panelWidthPx),
      font,
      fontRenderContext,
      measuredLayout,
      baseColumn,
      maxVisualLines
    )

  def fromBuffer(
    buffer: Buffer,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    wordWrapEnabled: Boolean = true
  ): TextLayoutSnapshot =
    val measuredLayout =
      shouldUseMeasuredLayout(font, fontRenderContext)
    val lineHeightPx =
      math.max(1, math.ceil(font.getLineMetrics("Mg", fontRenderContext).getHeight.toDouble).toInt)
    val ascentPx =
      math.max(1, math.ceil(font.getLineMetrics("Mg", fontRenderContext).getAscent.toDouble).toInt)
    val totalLines = buffer.content.lineCount
    val richDocument =
      buffer.richTextDocument.filter(_.matchesPlainTextShape(totalLines, buffer.content.weight))
    val viewportTopVisualLine = if wordWrapEnabled then buffer.viewport.topVisualLine else 0
    val visualLineLimit       = viewportTopVisualLine + buffer.viewport.visibleLines
    val visualLines =
      collectVisualLines(
        buffer,
        totalLines,
        math.max(1, panelWidthPx),
        font,
        fontRenderContext,
        measuredLayout,
        visualLineLimit,
        richDocument,
        wordWrapEnabled
      ).drop(viewportTopVisualLine).take(buffer.viewport.visibleLines)

    TextLayoutSnapshot(
      visualLines = visualLines,
      panelWidthPx = math.max(1, panelWidthPx),
      lineHeightPx = lineHeightPx,
      ascentPx = ascentPx,
      isProportional = !FontLoader.isMonospacedFont(font),
      usesMeasuredLayout = measuredLayout,
      richTextDocument = richDocument
    )

  private def collectVisualLines(
    buffer: Buffer,
    totalLines: Int,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    visualLineLimit: Int,
    richDocument: Option[RichTextDocument],
    wordWrapEnabled: Boolean
  ): Vector[TextVisualLine] =
    @annotation.tailrec
    def loop(lines: Vector[(Int, String)], acc: Vector[TextVisualLine]): Vector[TextVisualLine] =
      if lines.isEmpty || acc.length >= visualLineLimit then acc
      else
        val (lineIndex, rawLine) = lines.head
        val startColumn =
          if wordWrapEnabled then 0
          else math.min(buffer.viewport.leftColumn, rawLine.length)
        val visibleSlice =
          if wordWrapEnabled then rawLine.drop(startColumn)
          else unwrappedVisibleSlice(rawLine, startColumn, buffer.viewport.visibleColumns)
        val remainingVisualLines = math.max(0, visualLineLimit - acc.length)
        val wrapped =
          if remainingVisualLines <= 0 then Vector.empty
          else if wordWrapEnabled then
            wrapLogicalLine(
              visibleSlice,
              lineIndex,
              panelWidthPx,
              font,
              frc,
              measuredLayout,
              startColumn,
              remainingVisualLines
            )
          else
            Vector(
              shapeSegment(
                visibleSlice,
                lineIndex,
                startColumn,
                startColumn + visibleSlice.length,
                font,
                frc,
                measuredLayout
              )
            )
        val aligned = applyParagraphAlignment(wrapped, lineIndex, panelWidthPx, richDocument)
        loop(lines.tail, acc ++ aligned)

    val maxLogicalLines = math.min(math.max(0, totalLines - buffer.viewport.topLine), math.max(1, visualLineLimit))
    loop(buffer.content.linesIteratorFrom(buffer.viewport.topLine).take(maxLogicalLines).toVector, Vector.empty)

  private def unwrappedVisibleSlice(rawLine: String, startColumn: Int, visibleColumns: Int): String =
    val visibleEndColumn = startColumn + math.max(1, visibleColumns) + UnwrappedOverscanColumns
    rawLine.slice(startColumn, math.min(rawLine.length, visibleEndColumn))

  private def wrapLogicalLine(
    line: String,
    bufferLine: Int,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    baseColumn: Int = 0,
    maxVisualLines: Int = Int.MaxValue
  ): Vector[TextVisualLine] =
    if maxVisualLines <= 0 then Vector.empty
    else if line.isEmpty then Vector(shapeSegment("", bufferLine, baseColumn, baseColumn, font, frc, measuredLayout))
    else
      def loop(startColumn: Int, acc: Vector[TextVisualLine]): Vector[TextVisualLine] =
        if startColumn >= line.length || acc.length >= maxVisualLines then acc
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
    if !measuredLayout then
      val charWidth = math.max(1, CellMetrics.fromFont(font).charWidth)
      math.max(1, math.min(text.length, panelWidthPx / charWidth))
    else fittingMeasuredSegmentLength(text, panelWidthPx, font, frc, measuredLayout)

  private def fittingMeasuredSegmentLength(
    text: String,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean
  ): Int =
    val cellWidth    = math.max(1, CellMetrics.fromFont(font).charWidth)
    val initialLimit = math.min(text.length, math.max(16, panelWidthPx / cellWidth + 32))

    @annotation.tailrec
    def loop(limit: Int): Int =
      val candidate = text.take(limit)
      val carets    = caretXs(candidate, font, frc, measuredLayout)
      val maxFitting =
        carets.zipWithIndex.takeWhile { case (x, _) => x <= panelWidthPx.toFloat }.map(_._2).lastOption.getOrElse(0)
      val candidateExhausted = limit >= text.length
      val panelFilled        = carets.lastOption.exists(_ > panelWidthPx.toFloat)
      if candidateExhausted || panelFilled || maxFitting < candidate.length then math.max(1, maxFitting)
      else loop(math.min(text.length, math.max(limit + 1, limit * 2)))

    loop(initialLimit)

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
    val xs              = caretXs(text, font, frc, measuredLayout)
    val boundaryOffsets = graphemeBoundaryOffsets(text)
    val caretStops = boundaryOffsets.map { offset =>
      TextCaretStop(startColumn + offset, xs.lift(offset).getOrElse(xs.lastOption.getOrElse(0.0f)))
    }.toVector
    val xSortedCaretStops =
      if caretStops.sliding(2).forall {
            case Vector(first, second) => first.xPx <= second.xPx
            case _                     => true
          }
      then caretStops
      else caretStops.sortBy(_.xPx)
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = startColumn,
      endColumn = endColumn,
      text = text,
      widthPx = xs.lastOption.getOrElse(0.0f),
      caretStops = caretStops,
      xSortedCaretStops = xSortedCaretStops
    )

  private def applyParagraphAlignment(
    lines: Vector[TextVisualLine],
    lineIndex: Int,
    panelWidthPx: Int,
    richDocument: Option[RichTextDocument]
  ): Vector[TextVisualLine] =
    val alignment = richDocument
      .flatMap(_.paragraphAt(lineIndex))
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
        xSortedCaretStops = line.xSortedCaretStops.map(stop => stop.copy(xPx = stop.xPx + offsetPx)),
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

  private def graphemeBoundaryOffsets(text: String): Vector[Int] =
    @annotation.tailrec
    def loop(offset: Int, acc: Vector[Int]): Vector[Int] =
      if offset >= text.length then if acc.lastOption.contains(text.length) then acc else acc :+ text.length
      else
        val next = TextEditing.nextGraphemeBoundary(text, offset)
        loop(next, acc :+ next)

    loop(0, Vector(0))

  private def shouldUseMeasuredLayout(font: Font, frc: FontRenderContext): Boolean =
    measuredLayoutCache.computeIfAbsent(
      MeasuredLayoutKey(font, frc),
      key =>
        (!FontLoader.isMonospacedFont(key.font) ||
          FontLoader.ligaturesEnabled(key.font) ||
          hasFractionalAdvanceDrift(key.font, key.fontRenderContext)): java.lang.Boolean
    )

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
