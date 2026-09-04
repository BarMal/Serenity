package com.serenity.ui.layout

import java.awt.font.*
import java.awt.image.BufferedImage
import java.awt.{Font, RenderingHints}
import java.text.AttributedString

import com.serenity.richtext.{ParagraphAlignment, RichTextDocument}
import com.serenity.state.models.{
  Buffer,
  CursorPosition,
  NavigationGeometry,
  TextCaretStop,
  TextVisualLine,
  TypographyRole
}
import com.serenity.text.TextEditing
import com.serenity.ui.fonts.FontLoader

final case class TextLayoutSnapshot(
    visualLines: Vector[TextVisualLine],
    panelWidthPx: Int,
    lineHeightPx: Int,
    ascentPx: Int,
    isProportional: Boolean = false,
    usesMeasuredLayout: Boolean = false,
    richTextDocument: Option[RichTextDocument] = None
):

  def navigationGeometry: NavigationGeometry = NavigationGeometry(visualLines)

  def xPxForCursor(cursor: CursorPosition): Option[Float] =
    navigationGeometry.xPxForCursor(cursor)

  def cursorForVisualRowAndXPx(row: Int, xPx: Float): Option[CursorPosition] =
    navigationGeometry.cursorForVisualRowAndXPx(row, xPx)

  def moveVertical(cursor: CursorPosition, direction: Int, preferredXPx: Float): Option[CursorPosition] =
    navigationGeometry.moveVertical(cursor, direction, preferredXPx)

object TextLayoutSnapshot:
  private val UnwrappedOverscanColumns = 2
  final private case class MeasuredLayoutKey(font: Font, fontRenderContext: FontRenderContext)
  private val measuredLayoutCache = java.util.concurrent.ConcurrentHashMap[MeasuredLayoutKey, java.lang.Boolean]()

  /** The pixel width text layout wraps at. The screen grid is the code font's cells whatever font a buffer draws with,
    * so scroll and navigation math must wrap at the grid width the renderer uses -- not the buffer font's own
    * `M`-width, which over-estimates the width for a proportional prose font and pushes wrapped rows off screen.
    */
  def gridWrapWidthPx(gridColumns: Int, fontConfig: FontLoader.FontConfig): Int =
    gridColumns * CellMetrics.fromFont(FontLoader.previewFontForRole(fontConfig, TypographyRole.Code)).charWidth

  /** `cellMetrics` is the unit every non-measured (cell-based) layout call below expresses its "pixel" positions in --
    * defaulting to the font's own natural cell size (`CellMetrics.fromFont(font)`, GUI's long-standing behaviour) so
    * every caller that does not pass one keeps today's exact numbers. A caller with its own notion of what a "pixel"
    * means here (TUI's `TerminalRenderSurface`, where 1 pixel is defined to be exactly 1 terminal cell) passes that
    * explicitly instead of leaving this module to silently re-derive the real AWT font's metrics, which is what let
    * TUI's `CellMetricsOne` go unhonoured end-to-end (#1215).
    */
  def caretXsForText(
    text: String,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false
  ): Vector[Float] =
    val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout = !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext)
    caretXs(text, font, fontRenderContext, measuredLayout, cellMetrics)

  def visualLineForText(
    text: String,
    bufferLine: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    startColumn: Int = 0,
    cellMetricsOverride: Option[CellMetrics] = None
  ): TextVisualLine =
    val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout = shouldUseMeasuredLayout(font, fontRenderContext)
    shapeSegment(
      text,
      bufferLine,
      startColumn,
      startColumn + text.length,
      font,
      fontRenderContext,
      measuredLayout,
      cellMetrics
    )

  def leftColumnForCursorVisibility(
    lineText: String,
    cursorColumn: Int,
    visibleWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false
  ): Int =
    if lineText.isEmpty || visibleWidthPx <= 0 then 0
    else
      val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
      val measuredLayout = !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext)
      val safeColumn     = cursorColumn.max(0).min(lineText.length)
      // A display-width-aware grid takes the caret-stop path too: on it a column is not a cell count, so the uniform
      // `column - visibleColumns + 1` arithmetic below would scroll to the wrong place through any wide glyph.
      if !measuredLayout && !cellMetrics.displayWidthAware then
        val charWidth      = math.max(1, cellMetrics.charWidth)
        val visibleColumns = math.max(1, visibleWidthPx / charWidth)
        math.max(0, safeColumn - visibleColumns + 1)
      else
        val xs            = caretXs(lineText, font, fontRenderContext, measuredLayout, cellMetrics)
        val cursorXPx     = xs.lift(safeColumn).getOrElse(xs.lastOption.getOrElse(0.0f))
        val targetLeftXPx = math.max(0.0f, cursorXPx - visibleWidthPx.toFloat + 1.0f)
        xs.zipWithIndex.takeWhile { case (x, _) => x <= targetLeftXPx }.map(_._2).lastOption.getOrElse(0)

  def visualLineIndexForCursor(
    lineText: String,
    cursorColumn: Int,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    wordWrapEnabled: Boolean = true,
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false
  ): Int =
    if !wordWrapEnabled then 0
    else
      val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
      val measuredLayout = !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext)
      wrapLogicalLine(
        lineText,
        0,
        math.max(1, panelWidthPx),
        font,
        fontRenderContext,
        measuredLayout,
        cellMetrics
      ).zipWithIndex
        // `.lastOption`, not `collectFirst`: at a wrap boundary (one row's endColumn == the next row's startColumn) the
        // cursor belongs to the *later* row, matching NavigationGeometry.visualRowIndexFor and the renderer's caret, so
        // viewport centring measures the cursor's visual row the same way the caret is actually drawn.
        .filter { case (line, _) => cursorColumn >= line.startColumn && cursorColumn <= line.endColumn }
        .lastOption
        .map(_._2)
        .getOrElse(0)

  private[serenity] def boundedVisualLinesForText(
    text: String,
    bufferLine: Int,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    baseColumn: Int = 0,
    maxVisualLines: Int = Int.MaxValue,
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false
  ): Vector[TextVisualLine] =
    val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout = !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext)
    wrapLogicalLine(
      text,
      bufferLine,
      math.max(1, panelWidthPx),
      font,
      fontRenderContext,
      measuredLayout,
      cellMetrics,
      baseColumn,
      maxVisualLines
    )

  /** `forceCellLayout` bypasses the font-driven measured-vs-cell auto-detection (`shouldUseMeasuredLayout`) entirely,
    * always taking the cell path. A caller with no real font rendering to measure against at all -- a terminal surface
    * reporting no `FontRenderContext` (#1105) -- needs this: `shouldUseMeasuredLayout`'s fractional-advance-drift probe
    * measures the font with a manufactured default `FontRenderContext` regardless of whether the eventual surface can
    * draw a measured run, so a "monospaced" logical font whose headless/host font-substitution isn't pixel-perfect can
    * still trip the drift check and route TUI onto the measured path -- silently discarding the caller's `cellMetrics`
    * override, since the measured path never consults it (#1215).
    */
  def fromBuffer(
    buffer: Buffer,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    wordWrapEnabled: Boolean = true,
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false
  ): TextLayoutSnapshot =
    val cellMetrics = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout =
      !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext)
    // The non-measured (cell) path draws every row on `cellMetrics`' own grid (see `TextRowMetrics`'s non-measured
    // `lineTopPx`), so its line height/ascent must come from that same caller-supplied unit rather than the real
    // font's metrics -- otherwise a caller whose "pixel" is coarser or finer than the font's actual line height (TUI's
    // CellMetricsOne, 1 pixel == 1 terminal row) gets a snapshot describing rows in a scale nothing downstream uses.
    val lineHeightPx =
      if measuredLayout then
        math.max(1, math.ceil(font.getLineMetrics("Mg", fontRenderContext).getHeight.toDouble).toInt)
      else math.max(1, cellMetrics.lineHeight)
    val ascentPx =
      if measuredLayout then
        math.max(1, math.ceil(font.getLineMetrics("Mg", fontRenderContext).getAscent.toDouble).toInt)
      else math.max(0, cellMetrics.ascent)
    val totalLines = buffer.document.content.lineCount
    val richDocument =
      buffer.richText.richTextDocument.filter(_.matchesPlainTextShape(totalLines, buffer.document.content.weight))
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
        cellMetrics,
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
    cellMetrics: CellMetrics,
    visualLineLimit: Int,
    richDocument: Option[RichTextDocument],
    wordWrapEnabled: Boolean
  ): Vector[TextVisualLine] =
    @annotation.tailrec
    def loop(lines: Vector[(Int, String)], acc: Vector[TextVisualLine]): Vector[TextVisualLine] =
      if acc.length >= visualLineLimit then acc
      else
        lines match
          case (lineIndex, rawLine) +: rest =>
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
                  cellMetrics,
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
                    measuredLayout,
                    cellMetrics
                  )
                )
            val aligned = applyParagraphAlignment(wrapped, lineIndex, panelWidthPx, richDocument)
            loop(rest, acc ++ aligned)
          case _ => acc

    val maxLogicalLines = math.min(math.max(0, totalLines - buffer.viewport.topLine), math.max(1, visualLineLimit))
    loop(
      buffer.document.content.linesIteratorFrom(buffer.viewport.topLine).take(maxLogicalLines).toVector,
      Vector.empty
    )

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
    cellMetrics: CellMetrics,
    baseColumn: Int = 0,
    maxVisualLines: Int = Int.MaxValue
  ): Vector[TextVisualLine] =
    if maxVisualLines <= 0 then Vector.empty
    else if line.isEmpty then
      Vector(shapeSegment("", bufferLine, baseColumn, baseColumn, font, frc, measuredLayout, cellMetrics))
    else
      def loop(startColumn: Int, acc: Vector[TextVisualLine]): Vector[TextVisualLine] =
        if startColumn >= line.length || acc.length >= maxVisualLines then acc
        else
          val remaining        = line.substring(startColumn)
          val fittingLength    = fittingSegmentLength(remaining, panelWidthPx, font, frc, measuredLayout, cellMetrics)
          val segmentLength    = wordBoundarySegmentLength(remaining, fittingLength)
          val endColumnInSlice = startColumn + segmentLength
          val segment          = line.substring(startColumn, endColumnInSlice)
          val segmentStart     = baseColumn + startColumn
          val segmentEnd       = baseColumn + endColumnInSlice
          val visualLine =
            shapeSegment(segment, bufferLine, segmentStart, segmentEnd, font, frc, measuredLayout, cellMetrics)
          loop(endColumnInSlice, acc :+ visualLine)

      loop(0, Vector.empty)

  private def fittingSegmentLength(
    text: String,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics
  ): Int =
    if !measuredLayout then
      val charWidth = math.max(1, cellMetrics.charWidth)
      if cellMetrics.displayWidthAware then fittingDisplayWidthSegmentLength(text, panelWidthPx, charWidth)
      else math.max(1, math.min(text.length, panelWidthPx / charWidth))
    else fittingMeasuredSegmentLength(text, panelWidthPx, font, frc, measuredLayout, cellMetrics)

  private def fittingMeasuredSegmentLength(
    text: String,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics
  ): Int =
    val cellWidth    = math.max(1, cellMetrics.charWidth)
    val initialLimit = math.min(text.length, math.max(16, panelWidthPx / cellWidth + 32))

    @annotation.tailrec
    def loop(limit: Int): Int =
      val candidate = text.take(limit)
      val carets    = caretXs(candidate, font, frc, measuredLayout, cellMetrics)
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
    measuredLayout: Boolean,
    cellMetrics: CellMetrics
  ): TextVisualLine =
    val xs              = caretXs(text, font, frc, measuredLayout, cellMetrics)
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

  private def caretXs(
    text: String,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics
  ): Vector[Float] =
    if text.isEmpty then Vector(0.0f)
    else if !measuredLayout then
      val charWidth = cellMetrics.charWidth.toFloat
      if cellMetrics.displayWidthAware then displayWidthCaretXs(text, charWidth)
      else Vector.tabulate(text.length + 1)(index => index * charWidth)
    else
      val attributed = AttributedString(text)
      attributed.addAttribute(TextAttribute.FONT, font)
      val layout = TextLayout(attributed.getIterator, frc)
      val leadingCarets =
        (0 until text.length).toVector.map(index => layout.getCaretInfo(TextHitInfo.leading(index))(0))
      normalizeCollapsedCarets(leadingCarets :+ layout.getAdvance)

  /** Caret stops for a display-width-aware cell grid: each codepoint advances by its own cell count ([[CharWidth]])
    * rather than one cell per character, so the stops agree with the cells `TerminalScreenBuffer` actually paints. A
    * surrogate pair contributes one advance across its two char indices; its low half is never a grapheme boundary (see
    * [[graphemeBoundaryOffsets]]) and so never a caret stop, and taking the glyph's trailing edge there keeps the
    * sequence non-decreasing for the callers that index it by raw column.
    */
  private def displayWidthCaretXs(text: String, charWidth: Float): Vector[Float] =
    @annotation.tailrec
    def loop(index: Int, xPx: Float, acc: Vector[Float]): Vector[Float] =
      if index >= text.length then acc :+ xPx
      else
        val codePoint = text.codePointAt(index)
        val advanced  = xPx + CharWidth.of(codePoint) * charWidth
        val charCount = Character.charCount(codePoint)
        val stops     = if charCount == 2 then acc :+ xPx :+ advanced else acc :+ xPx
        loop(index + charCount, advanced, stops)

    loop(0, 0.0f, Vector.empty)

  /** How many characters of `text` fit in `panelWidthPx` when each glyph costs its own cells. Never splits a wide glyph
    * across the wrap boundary, and never splits a surrogate pair; like the uniform-advance branch it always consumes at
    * least one glyph, so wrapping makes progress even in a panel narrower than a single cell.
    */
  private def fittingDisplayWidthSegmentLength(text: String, panelWidthPx: Int, charWidth: Int): Int =
    @annotation.tailrec
    def loop(index: Int, usedPx: Int): Int =
      if index >= text.length then index
      else
        val codePoint = text.codePointAt(index)
        val advance   = CharWidth.of(codePoint) * charWidth
        if usedPx + advance > panelWidthPx then index
        else loop(index + Character.charCount(codePoint), usedPx + advance)

    val fitted = loop(0, 0)
    if fitted > 0 then fitted else math.min(text.length, Character.charCount(text.codePointAt(0)))

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
      // Whether this font itself has fractional-advance drift is a property of the font, independent of whatever
      // "pixel" unit a caller's cellMetrics defines -- so this always measures against the font's own natural cell
      // size, not a caller override (which would otherwise make every font look like it drifts under TUI's
      // CellMetricsOne).
      val fontCellMetrics = CellMetrics.fromFont(font)
      val measuredXs      = caretXs(sampleText, font, frc, measuredLayout = true, fontCellMetrics)
      val measuredAdvance = measuredXs.lastOption.getOrElse(0.0f)
      val cellAdvance     = fontCellMetrics.charWidth.toFloat * sampleText.length
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
