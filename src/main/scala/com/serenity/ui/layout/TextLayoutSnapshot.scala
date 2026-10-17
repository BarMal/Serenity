package com.serenity.ui.layout

import java.awt.font.*
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.{Font, RenderingHints}
import java.text.StringCharacterIterator
import java.util.Locale

import com.ibm.icu.text.BreakIterator
import com.serenity.config.MarkdownViewMode
import com.serenity.markdown.MarkdownInlineView
import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.state.models.{
  Buffer,
  CursorPosition,
  IndentationSettings,
  MeasuredRow,
  NavigationGeometry,
  RowAffinity,
  TextVisualLine,
  TypographyRole
}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.TextCaretMeasurement.*
import com.serenity.ui.renderer.FontFirstUse
import com.serenity.ui.theme.HeadingSettings

final case class TextLayoutSnapshot(
    visualLines: Vector[TextVisualLine],
    panelWidthPx: Int,
    lineHeightPx: Int,
    ascentPx: Int,
    isProportional: Boolean = false,
    usesMeasuredLayout: Boolean = false,
    richTextDocument: Option[RichTextDocument] = None,
    // Prose zoom applied to rich-text runs (1x = authored). The draw path reads it so its per-run font sizes match the
    // sizes this snapshot measured caret advances and per-line heights with.
    proseScale: Float = 1.0f,
    // How the draw path restyles inline Markdown, so the glyphs it paints are the ones this snapshot measured.
    markdownInline: MarkdownInlineView = MarkdownInlineView.Off,
    // The heading look the rows were measured with, so the draw path paints the sizes this snapshot laid out.
    headings: HeadingSettings = HeadingSettings.default
):

  def navigationGeometry: NavigationGeometry = NavigationGeometry(visualLines)

  /** What each visual row occupies when rows are stacked: its measured height, or the snapshot's uniform one, plus the
    * height its paragraph reserves below it. Painting, row bands and click targeting all stack rows by this.
    */
  def rowHeightsPx: Vector[Int] =
    visualLines.map(line => (if line.heightPx > 0 then line.heightPx else lineHeightPx) + line.reservedBelowPx)

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
    val xs             = caretXs(text, 0, singleFontResolver(font), fontRenderContext, measuredLayout, cellMetrics)
    Vector.tabulate(xs.length)(xs(_))

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
    TextVisualLine(
      bufferLine,
      measureSegment(
        text,
        startColumn,
        startColumn + text.length,
        singleFontResolver(font),
        fontRenderContext,
        measuredLayout,
        cellMetrics
      ),
      xOffsetPx = 0.0f,
      spaceBeforePx = 0,
      spaceAfterPx = 0
    )

  def leftColumnForCursorVisibility(
    lineText: String,
    cursorColumn: Int,
    visibleWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    tabSize: Int = IndentationSettings.DefaultTabSize
  ): Int =
    if lineText.isEmpty || visibleWidthPx <= 0 then 0
    else
      val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
      val measuredLayout = !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext)
      val safeColumn     = cursorColumn.max(0).min(lineText.length)
      // A display-width-aware grid, or a line with a tab, takes the caret-stop path too: there a column is not a cell
      // count, so the uniform `column - visibleColumns + 1` arithmetic below would scroll wrong through a wide glyph.
      if !measuredLayout && !cellMetrics.displayWidthAware && lineText.indexOf('\t') < 0 then
        val visibleColumns = math.max(1, visibleWidthPx / math.max(1, cellMetrics.charWidth))
        math.max(0, safeColumn - visibleColumns + 1)
      else
        val resolver      = singleFontResolver(font, tabSize)
        val xs            = caretXs(lineText, 0, resolver, fontRenderContext, measuredLayout, cellMetrics)
        val cursorXPx     = xs(math.min(safeColumn, xs.length - 1))
        val targetLeftXPx = math.max(0.0f, cursorXPx - visibleWidthPx.toFloat + 1.0f)
        val firstPast     = xs.indexWhere(_ > targetLeftXPx)
        if firstPast < 0 then xs.length - 1 else math.max(0, firstPast - 1)

  def visualLineIndexForCursor(
    lineText: String,
    cursorColumn: Int,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    wordWrapEnabled: Boolean = true,
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    rowAffinity: RowAffinity = RowAffinity.Downstream,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    bufferLine: Int = 0,
    richText: RichTextContext = RichTextContext.plain
  ): Int =
    if !wordWrapEnabled then 0
    else
      val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
      val measuredLayout = LineLayout.measuredLayoutFor(font, fontRenderContext, forceCellLayout, richText)
      // At a wrap boundary (one row's endColumn == the next row's startColumn) both rows match the column, and the
      // cursor's own affinity settles it exactly as `NavigationGeometry.visualRowIndexFor` does -- so viewport centring
      // measures the cursor's visual row as the row the caret is actually drawn on.
      val matching = LineLayout
        .wrappedLine(
          lineText,
          bufferLine,
          math.max(1, panelWidthPx),
          font,
          fontRenderContext,
          measuredLayout,
          cellMetrics,
          richText,
          lineLength = lineText.length,
          wrapCache = wrapCache
        )
        .zipWithIndex
        .filter { case (line, _) => cursorColumn >= line.startColumn && cursorColumn <= line.endColumn }
      val resolved = rowAffinity match
        case RowAffinity.Upstream   => matching.headOption
        case RowAffinity.Downstream => matching.lastOption
      resolved.map(_._2).getOrElse(0)

  private[serenity] def boundedVisualLinesForText(
    text: String,
    bufferLine: Int,
    panelWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    baseColumn: Int = 0,
    maxVisualLines: Int = Int.MaxValue,
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    richText: RichTextContext = RichTextContext.plain
  ): Vector[TextVisualLine] =
    val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout = LineLayout.measuredLayoutFor(font, fontRenderContext, forceCellLayout, richText)
    LineLayout.wrappedLine(
      text,
      bufferLine,
      math.max(1, panelWidthPx),
      font,
      fontRenderContext,
      measuredLayout,
      cellMetrics,
      richText,
      lineLength = text.length,
      baseColumn,
      maxVisualLines,
      wrapCache
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
    forceCellLayout: Boolean = false,
    // Prose zoom (1x = authored). Multiplies each rich-text run's font size for measurement so caret advances, wrap
    // points, and per-line heights track the scaled glyphs the draw path paints. Only affects rich-text buffers.
    proseScale: Float = 1.0f,
    // `document.drop_caps_enabled` config toggle (`RichTextStyling.effectiveRole`'s gate). Defaults to the config's
    // own default so callers that predate drop caps keep measuring exactly as before.
    dropCapsEnabled: Boolean = true,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    tabSize: Int = IndentationSettings.DefaultTabSize,
    headings: HeadingSettings = HeadingSettings.default
  ): TextLayoutSnapshot =
    val cellMetrics = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val totalLines  = buffer.document.content.lineCount
    val richDocument =
      buffer.richText.richTextDocument.filter(_.matchesPlainTextShape(totalLines, buffer.document.content.weight))
    val markdownInline =
      RichTextContext.markdownViewFor(buffer, markdownViewMode, forceCellLayout, wrapCache, font, fontRenderContext)
    val richText       = RichTextContext(richDocument, proseScale, dropCapsEnabled, markdownInline, tabSize, headings)
    val measuredLayout = LineLayout.measuredLayoutFor(font, fontRenderContext, forceCellLayout, richText)
    // The non-measured (cell) path draws every row on `cellMetrics`' own grid (see `TextRowMetrics`'s non-measured
    // `lineTopPx`), so its line height/ascent must come from that same caller-supplied unit rather than the real
    // font's metrics -- otherwise a caller whose "pixel" is coarser or finer than the font's actual line height (TUI's
    // CellMetricsOne, 1 pixel == 1 terminal row) gets a snapshot describing rows in a scale nothing downstream uses.
    val lineHeightPx =
      if measuredLayout then math.max(1, FontLineExtents.of(font, fontRenderContext).heightPx)
      else math.max(1, cellMetrics.lineHeight)
    val ascentPx =
      if measuredLayout then math.max(1, FontLineExtents.of(font, fontRenderContext).ascentPx)
      else math.max(0, cellMetrics.ascent)
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
        richText,
        wordWrapEnabled,
        wrapCache
      ).drop(viewportTopVisualLine).take(buffer.viewport.visibleLines)
    val windowed = HeadingSpacingLayout.droppingLeadingSpace(visualLines)

    TextLayoutSnapshot(
      visualLines = windowed,
      panelWidthPx = math.max(1, panelWidthPx),
      lineHeightPx = lineHeightPx,
      ascentPx = ascentPx,
      isProportional = !FontLoader.isMonospacedFont(font),
      usesMeasuredLayout = measuredLayout,
      richTextDocument = richDocument,
      proseScale = proseScale,
      markdownInline = markdownInline,
      headings = headings
    )

  // Column-based document layout (issue #1338, Phase 1): `columnChunksForBuffer`/`fromBufferColumn` live in
  // `TextLayoutSnapshotColumnMode` (600-line architecture ratchet split) and are exported here so existing
  // `TextLayoutSnapshot.foo` call sites keep working unchanged.
  export TextLayoutSnapshotColumnMode.{columnChunksForBuffer, fromBufferColumn, fromBufferColumns}

  private[layout] def collectVisualLines(
    buffer: Buffer,
    totalLines: Int,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    visualLineLimit: Int,
    richText: RichTextContext,
    wordWrapEnabled: Boolean,
    wrapCache: WrappedLineCache
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
            val inputs =
              LineLayout.lineLayoutInputs(font, frc, measuredLayout, richText, lineIndex, rawLine.length, rawLine)
            val resolver      = inputs.resolver
            val paragraphRole = inputs.paragraphRole
            val glyphWidthPx  = inputs.glyphWidthPx
            val wrapped =
              if remainingVisualLines <= 0 then Vector.empty
              else if wordWrapEnabled then
                wrapLogicalLine(
                  visibleSlice,
                  lineIndex,
                  panelWidthPx,
                  resolver,
                  frc,
                  measuredLayout,
                  cellMetrics,
                  startColumn,
                  remainingVisualLines,
                  paragraphRole,
                  glyphWidthPx,
                  wrapCache
                )
              else
                val row = measureSegment(
                  visibleSlice,
                  startColumn,
                  startColumn + visibleSlice.length,
                  resolver,
                  frc,
                  measuredLayout,
                  cellMetrics
                )
                DropCapLayout.reservingGlyphHeight(
                  Vector(placed(row, lineIndex, paragraphRole, lineWithinParagraph = 0, glyphWidthPx)),
                  paragraphRole
                )
            val aligned  = ParagraphAlignmentLayout.apply(wrapped, lineIndex, panelWidthPx, richText)
            val complete = !wordWrapEnabled || wrapped.length < remainingVisualLines
            val spaced = HeadingSpacingLayout
              .apply(aligned, inputs.headingLevel, complete, font, measuredLayout, cellMetrics, richText)
            loop(rest, acc ++ spaced)
          case _ => acc

    val maxLogicalLines = math.min(math.max(0, totalLines - buffer.viewport.topLine), math.max(1, visualLineLimit))
    loop(
      buffer.document.content.linesIteratorFrom(buffer.viewport.topLine).take(maxLogicalLines).toVector,
      Vector.empty
    )

  private def unwrappedVisibleSlice(rawLine: String, startColumn: Int, visibleColumns: Int): String =
    val visibleEndColumn = startColumn + math.max(1, visibleColumns) + UnwrappedOverscanColumns
    rawLine.slice(startColumn, math.min(rawLine.length, visibleEndColumn))

  private[layout] def wrapLogicalLine(
    line: String,
    bufferLine: Int,
    panelWidthPx: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    baseColumn: Int,
    maxVisualLines: Int,
    // A drop cap paragraph's first `paragraphRole.lines` visual lines (0-based `lineWithinParagraph`, tracked below via
    // `acc.length`) reserve `dropCapGlyphWidthPx` of left margin so wrapping leaves room for the glyph beside them --
    // `ParagraphRole.Body` (every non-drop-cap paragraph) makes both branches below exactly today's behaviour.
    paragraphRole: ParagraphRole,
    dropCapGlyphWidthPx: Float,
    wrapCache: WrappedLineCache
  ): Vector[TextVisualLine] =
    if maxVisualLines <= 0 then Vector.empty
    else
      val spec = WrappedLineKey(
        line,
        panelWidthPx,
        resolver,
        frc,
        measuredLayout,
        cellMetrics,
        baseColumn,
        paragraphRole,
        dropCapGlyphWidthPx
      )
      // Measuring keeps the caller's context so the shaped-text cache is shared with whoever draws with it; only the
      // cache identity ignores the device transform.
      val rows = wrapCache.wrapped(spec.copy(frc = logicalFontRenderContext(frc)), maxVisualLines)(
        limit => wrapRows(spec, limit),
        predecessors => IncrementalWrap.rewrap(spec, predecessors)
      )
      val placedRows =
        Vector.tabulate(rows.length)(index =>
          placed(rows(index), bufferLine, paragraphRole, index, dropCapGlyphWidthPx)
        )
      if rows.length < maxVisualLines then DropCapLayout.reservingGlyphHeight(placedRows, paragraphRole) else placedRows

  /** A cached row read onto `bufferLine`; the drop cap inset is the only offset the row carries until alignment. */
  private def placed(
    row: MeasuredRow,
    bufferLine: Int,
    role: ParagraphRole,
    lineWithinParagraph: Int,
    glyphWidthPx: Float
  ): TextVisualLine =
    TextVisualLine(
      bufferLine,
      row,
      DropCapLayout.leftInsetPx(role, lineWithinParagraph, glyphWidthPx),
      spaceBeforePx = 0,
      spaceAfterPx = 0
    )

  private[layout] def wrapRows(spec: WrappedLineKey, maxVisualLines: Int): RowWrap =
    import spec.{baseColumn, cellMetrics, frc, measuredLayout, resolver, text}
    if text.isEmpty then
      val empty = measureSegment("", baseColumn, baseColumn, resolver, frc, measuredLayout, cellMetrics)
      RowWrap(Vector(empty), 1, 0, None)
    else
      val initial   = ParagraphMeasurement(spec, 0, maxVisualLines)
      val traceable = maxVisualLines == Int.MaxValue && IncrementalWrap.traceable(spec)
      @annotation.tailrec
      def loop(
        startColumn: Int,
        measured: ParagraphMeasurement,
        acc: Vector[MeasuredRow],
        reaches: Vector[Int]
      ): (Vector[MeasuredRow], Vector[Int]) =
        if startColumn >= text.length || acc.length >= maxVisualLines then (acc, reaches)
        else
          val paragraph = measured.coveringRowAt(startColumn, maxVisualLines - acc.length)
          val row       = wrapRow(spec, paragraph, startColumn, acc.length)
          val reach =
            if traceable then reaches :+ IncrementalWrap.reachOf(spec, startColumn, row.fitLength) else reaches
          loop(row.endColumn, paragraph, acc :+ row.row, reach)

      val (rows, reaches) = loop(0, initial, Vector.empty, Vector.empty)
      val trace           = Option.when(traceable)(WrapTrace(IArray.from(reaches), initial.advances))
      RowWrap(rows, rows.length, initial.measuredChars, trace)

  /** One visual row starting at `startColumn`, the shared step of the cold wrap and [[IncrementalWrap]]. `paragraph`
    * must already cover the row.
    */
  private[layout] def wrapRow(
    spec: WrappedLineKey,
    paragraph: ParagraphMeasurement,
    startColumn: Int,
    lineWithinParagraph: Int
  ): WrappedRow =
    import spec.{baseColumn, cellMetrics, dropCapGlyphWidthPx, frc, measuredLayout, paragraphRole, resolver, text}
    val insetPx          = DropCapLayout.leftInsetPx(paragraphRole, lineWithinParagraph, dropCapGlyphWidthPx)
    val wrapWidthPx      = math.max(1, spec.panelWidthPx - math.round(insetPx))
    val segmentStart     = baseColumn + startColumn
    val fit              = fittingSegment(paragraph, text, startColumn, wrapWidthPx, segmentStart, spec)
    val segmentLength    = wordBoundarySegmentLength(text, startColumn, fit.length)
    val endColumnInSlice = startColumn + segmentLength
    val row = measureSegment(
      text.substring(startColumn, endColumnInSlice),
      segmentStart,
      baseColumn + endColumnInSlice,
      resolver,
      frc,
      measuredLayout,
      cellMetrics,
      fit.caretXsForPrefix(segmentLength, resolver, segmentStart),
      Some(paragraph.graphemeOffsets(startColumn, endColumnInSlice))
    )
    WrappedRow(row, endColumnInSlice, fit.length)

  /** Where to break `text` once `fittingLength` characters have used up the available pixel width: the last legal
    * UAX#14 line-break boundary at or before `fittingLength`, per `BreakIterator.getLineInstance` -- covering no-break
    * spaces (never a break point, unlike a bare whitespace scan would risk) and CJK text (breakable between most
    * adjacent ideographs with no whitespace at all, unlike the old ad hoc implementation this replaces, which only ever
    * looked for a preceding whitespace character). Falls back to a forced break at `fittingLength` when no such
    * boundary exists ahead of it (a single word/run too long to fit at all), same as the old implementation's fallback.
    */
  /** One `BreakIterator.getLineInstance` per thread, reused across calls via `setText` rather than constructed fresh
    * each time -- constructing a line-break instance is expensive enough (it clones a larger, locale-specific rule set
    * than the character-break instance) that doing it once per wrapped visual line measurably slowed down documents
    * with many lines (#1277). `BreakIterator` is stateful and not thread-safe, hence `ThreadLocal` rather than one
    * shared instance.
    */
  private val threadLocalLineBreakIterator: ThreadLocal[BreakIterator] =
    ThreadLocal.withInitial(() => BreakIterator.getLineInstance(Locale.ROOT))

  /** Reads `line` from `from` on through a character iterator rather than a per-row substring of the paragraph's
    * remainder, which made wrapping one long paragraph quadratic in its length.
    */
  private[layout] def wordBoundarySegmentLength(line: String, from: Int, fittingLength: Int): Int =
    if fittingLength >= line.length - from then line.length - from
    else
      val boundary = threadLocalLineBreakIterator.get()
      boundary.setText(StringCharacterIterator(line, from, line.length, from))
      val candidate = boundary.preceding(from + fittingLength + 1) - from
      if candidate > 0 then candidate else fittingLength

  private[layout] def measureSegment(
    text: String,
    startColumn: Int,
    endColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    measuredXs: Option[IArray[Float]] = None,
    graphemeOffsets: Option[IArray[Int]] = None
  ): MeasuredRow =
    val xs      = measuredXs.getOrElse(caretXs(text, startColumn, resolver, frc, measuredLayout, cellMetrics))
    val offsets = graphemeOffsets.getOrElse(ParagraphMeasurement.graphemeBoundaryOffsets(text))
    val stopXs  = IArray.tabulate(offsets.length)(index => xs(math.min(offsets(index), xs.length - 1)))
    val (landingXs, landingOffsets) =
      if resolver.hasHidden then landableStops(stopXs, offsets, startColumn, resolver)
      else MeasuredRow.orderedByX(stopXs, offsets)
    val (heightPx, ascentPx) =
      if measuredLayout then resolver.lineMetrics(frc, startColumn, endColumn) else (0, 0)
    MeasuredRow(
      startColumn = startColumn,
      endColumn = endColumn,
      text = text,
      widthPx = xs(xs.length - 1),
      caretXs = stopXs,
      caretOffsets = offsets,
      landingXs = landingXs,
      landingOffsets = landingOffsets,
      heightPx = heightPx,
      ascentPx = ascentPx
    )

  private[layout] def shouldUseMeasuredLayout(font: Font, frc: FontRenderContext): Boolean =
    FontFirstUse.settle(font, frc)
    measuredLayoutCache.computeIfAbsent(
      MeasuredLayoutKey(font, logicalFontRenderContext(frc)),
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
      val measuredXs = caretXs(sampleText, 0, singleFontResolver(font), frc, measuredLayout = true, fontCellMetrics)
      val measuredAdvance = measuredXs(measuredXs.length - 1)
      val cellAdvance     = fontCellMetrics.charWidth.toFloat * sampleText.length
      math.abs(measuredAdvance - cellAdvance) > 0.5f

  /** With fractional metrics the advances do not depend on the device transform, so a surface's scaled context and the
    * identity one measure alike and must share cache entries; without them hinting does depend on it, so it stays.
    */
  private[layout] def logicalFontRenderContext(frc: FontRenderContext): FontRenderContext =
    if frc.isTransformed && frc.usesFractionalMetrics then
      FontRenderContext(AffineTransform(), frc.isAntiAliased, frc.usesFractionalMetrics)
    else frc

  def defaultFontRenderContext(): FontRenderContext = sharedDefaultFontRenderContext

  // `FontRenderContext` is immutable, so one instance serves every caller instead of a scratch image per call.
  private lazy val sharedDefaultFontRenderContext: FontRenderContext =
    val image = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
      g.getFontRenderContext
    finally g.dispose()
