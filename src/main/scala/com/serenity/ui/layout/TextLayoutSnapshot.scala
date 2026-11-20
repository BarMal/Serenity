package com.serenity.ui.layout

import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.awt.{Font, RenderingHints}
import java.text.StringCharacterIterator

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
import com.serenity.ui.fonts.{FontFallback, FontLoader}
import com.serenity.ui.layout.SegmentMeasurement.measureSegment
import com.serenity.ui.layout.TextCaretMeasurement.*
import com.serenity.ui.openings.{OpeningSettings, ParagraphRoles}
import com.serenity.ui.renderer.FontSpec
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
    headings: HeadingSettings = HeadingSettings.default,
    // How the rows' paragraphs were resolved to roles, so paint and dirty tracking read the roles layout used.
    roles: ParagraphRoles = ParagraphRoles.enabled,
    // For the rows of a multi-column page laid end to end: the row each column after the first starts at. Columns hold
    // as many rows as fit their height, so moving a break moves rows between columns without changing any of them.
    columnBreaks: Vector[Int] = Vector.empty
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

  /** The pixel width text layout wraps at. The screen grid is the code font's cells whatever font a buffer draws with,
    * so scroll and navigation math must wrap at the grid width the renderer uses -- not the buffer font's own
    * `M`-width, which over-estimates the width for a proportional prose font and pushes wrapped rows off screen.
    */
  def gridWrapWidthPx(gridColumns: Int, fontConfig: FontLoader.FontConfig, memos: LayoutMemos): Int =
    gridColumns * cellMetricsOf(FontLoader.previewFontForRole(fontConfig, TypographyRole.Code), memos).charWidth

  /** `cellMetrics` is the unit every non-measured (cell-based) layout call below expresses its "pixel" positions in --
    * defaulting to the font's own cell as the owner's measurer gives it ([[cellMetricsOf]]) so every caller that does
    * not pass one lays the grid out in the font it draws. A caller with its own notion of what a "pixel" means here
    * (TUI's `TerminalRenderSurface`, where 1 pixel is defined to be exactly 1 terminal cell) passes that explicitly
    * instead of leaving this module to derive a real font's metrics, which is what let TUI's `CellMetricsOne` go
    * unhonoured end-to-end (#1215).
    *
    * Every builder measures through the measurer of the memos or wrap cache it is handed, and lays out on the cell grid
    * when they have none: a terminal or a headless owner has no glyphs to measure.
    */
  def caretXsForText(
    text: String,
    font: Font,
    memos: LayoutMemos,
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false
  ): Vector[Float] =
    val spec        = FontSpec.fromAwt(font)
    val cellMetrics = cellMetricsOverride.getOrElse(cellMetricsOf(font, memos))
    val measuring   = memos.measuring(!forceCellLayout && shouldUseMeasuredLayout(spec, memos))
    val xs          = caretXs(text, 0, singleFontResolver(spec), measuring, cellMetrics)
    Vector.tabulate(xs.length)(xs(_))

  def visualLineForText(
    text: String,
    bufferLine: Int,
    font: Font,
    memos: LayoutMemos,
    startColumn: Int = 0,
    cellMetricsOverride: Option[CellMetrics] = None,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): TextVisualLine =
    val cellMetrics    = cellMetricsOverride.getOrElse(cellMetricsOf(font, memos))
    val measuredLayout = shouldUseMeasuredLayout(FontSpec.fromAwt(font), memos)
    TextVisualLine(
      bufferLine,
      measureSegment(
        text,
        startColumn,
        startColumn + text.length,
        LineLayout.plainResolver(text, startColumn, font, measuredLayout, wrapCache = wrapCache),
        measuredLayout,
        cellMetrics,
        memos
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
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    tabSize: Int = IndentationSettings.DefaultTabSize,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    fallback: FontFallback = FontFallback.Default
  ): Int =
    if lineText.isEmpty || visibleWidthPx <= 0 then 0
    else
      val memos          = wrapCache.layoutMemos
      val cellMetrics    = cellMetricsOverride.getOrElse(cellMetricsOf(font, memos))
      val measuredLayout = !forceCellLayout && shouldUseMeasuredLayout(FontSpec.fromAwt(font), memos)
      val safeColumn     = cursorColumn.max(0).min(lineText.length)
      // A display-width-aware grid, or a line with a tab, takes the caret-stop path too: there a column is not a cell
      // count, so the uniform `column - visibleColumns + 1` arithmetic below would scroll wrong through a wide glyph.
      if !measuredLayout && !cellMetrics.displayWidthAware && lineText.indexOf('\t') < 0 then
        val visibleColumns = math.max(1, visibleWidthPx / math.max(1, cellMetrics.charWidth))
        math.max(0, safeColumn - visibleColumns + 1)
      else
        val resolver      = LineLayout.plainResolver(lineText, 0, font, measuredLayout, tabSize, wrapCache, fallback)
        val xs            = caretXs(lineText, 0, resolver, memos.measuring(measuredLayout), cellMetrics)
        val cursorXPx     = xs(math.min(safeColumn, xs.length - 1))
        val targetLeftXPx = math.max(0.0f, cursorXPx - visibleWidthPx.toFloat + 1.0f)
        val firstPast     = xs.indexWhere(_ > targetLeftXPx)
        if firstPast < 0 then xs.length - 1 else math.max(0, firstPast - 1)

  def visualLineIndexForCursor(
    lineText: String,
    cursorColumn: Int,
    panelWidthPx: Int,
    font: Font,
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
      val cellMetrics    = cellMetricsOverride.getOrElse(cellMetricsOf(font, wrapCache.layoutMemos))
      val measuredLayout = LineLayout.measuredLayoutFor(font, forceCellLayout, richText, wrapCache.layoutMemos)
      // At a wrap boundary (one row's endColumn == the next row's startColumn) both rows match the column, and the
      // cursor's own affinity settles it exactly as `NavigationGeometry.visualRowIndexFor` does -- so viewport centring
      // measures the cursor's visual row as the row the caret is actually drawn on.
      val matching = LineLayout
        .wrappedLine(
          lineText,
          bufferLine,
          math.max(1, panelWidthPx),
          font,
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
    baseColumn: Int = 0,
    maxVisualLines: Int = Int.MaxValue,
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    richText: RichTextContext = RichTextContext.plain
  ): Vector[TextVisualLine] =
    val cellMetrics    = cellMetricsOverride.getOrElse(cellMetricsOf(font, wrapCache.layoutMemos))
    val measuredLayout = LineLayout.measuredLayoutFor(font, forceCellLayout, richText, wrapCache.layoutMemos)
    LineLayout.wrappedLine(
      text,
      bufferLine,
      math.max(1, panelWidthPx),
      font,
      measuredLayout,
      cellMetrics,
      richText,
      lineLength = text.length,
      baseColumn,
      maxVisualLines,
      wrapCache
    )

  /** `forceCellLayout` bypasses the font-driven measured-vs-cell auto-detection (`shouldUseMeasuredLayout`) entirely,
    * always taking the cell path, as a wrap cache whose owner has no measurer always does. A caller whose surface
    * cannot draw a measured run at all -- a terminal surface reporting no `FontRenderContext` (#1105) -- needs this
    * even when its owner can measure, or the measured path would discard the caller's `cellMetrics` override, which it
    * never consults (#1215).
    */
  def fromBuffer(
    buffer: Buffer,
    panelWidthPx: Int,
    font: Font,
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
    headings: HeadingSettings = HeadingSettings.default,
    openings: OpeningSettings = OpeningSettings.default,
    // `document.drop_cap_gap`: the room beside a drop cap's glyph, in em of the body font.
    dropCapGapEm: Float = DropCapLayout.DefaultGapEm,
    fallback: FontFallback = FontFallback.Default
  ): TextLayoutSnapshot =
    val memos       = wrapCache.layoutMemos
    val cellOnly    = forceCellLayout || memos.measurer.isEmpty
    val cellMetrics = cellMetricsOverride.getOrElse(cellMetricsOf(font, memos))
    val totalLines  = buffer.document.content.lineCount
    val richDocument =
      buffer.richText.richTextDocument.filter(_.matchesPlainTextShape(totalLines, buffer.document.content.weight))
    val markdownInline =
      RichTextContext.markdownViewFor(buffer, markdownViewMode, cellOnly, wrapCache, font)
    val caps = richDocument.flatMap(wrapCache.virtualDropCaps(_, openings))
    val richText =
      RichTextContext(
        richDocument,
        proseScale,
        dropCapsEnabled,
        markdownInline,
        tabSize,
        headings,
        caps,
        dropCapGapEm,
        fallback = fallback
      )
    val measuredLayout = LineLayout.measuredLayoutFor(font, cellOnly, richText, memos)
    // The non-measured (cell) path draws every row on `cellMetrics`' own grid (see `TextRowMetrics`'s non-measured
    // `lineTopPx`), so its line height/ascent must come from that same caller-supplied unit rather than the real
    // font's metrics -- otherwise a caller whose "pixel" is coarser or finer than the font's actual line height (TUI's
    // CellMetricsOne, 1 pixel == 1 terminal row) gets a snapshot describing rows in a scale nothing downstream uses.
    val (lineHeightPx, ascentPx) = snapshotLineMetrics(font, measuredLayout, cellMetrics, memos)
    val viewportTopVisualLine    = if wordWrapEnabled then buffer.viewport.topVisualLine else 0
    val visualLineLimit          = viewportTopVisualLine + buffer.viewport.visibleLines
    val visualLines =
      collectVisualLines(
        buffer,
        totalLines,
        math.max(1, panelWidthPx),
        font,
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
      headings = headings,
      roles = richText.roles
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
            val remainingVisualLines = math.max(0, visualLineLimit - acc.length)
            val spaced =
              if wordWrapEnabled then
                LineLayout.placedWrappedLine(
                  lineIndex,
                  rawLine,
                  panelWidthPx,
                  font,
                  measuredLayout,
                  cellMetrics,
                  remainingVisualLines,
                  richText,
                  wrapCache
                )
              else
                LineLayout.unwrappedLine(
                  lineIndex,
                  rawLine,
                  buffer.viewport,
                  panelWidthPx,
                  font,
                  measuredLayout,
                  cellMetrics,
                  richText,
                  wrapCache
                )
            loop(rest, acc ++ spaced)
          case _ => acc

    val maxLogicalLines = math.min(math.max(0, totalLines - buffer.viewport.topLine), math.max(1, visualLineLimit))
    loop(
      buffer.document.content.linesIteratorFrom(buffer.viewport.topLine).take(maxLogicalLines).toVector,
      Vector.empty
    )

  private[layout] def wrapLogicalLine(
    line: String,
    bufferLine: Int,
    panelWidthPx: Int,
    resolver: LineFontResolver,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    baseColumn: Int,
    maxVisualLines: Int,
    // A drop cap paragraph's first `paragraphRole.lines` visual lines (0-based `lineWithinParagraph`, tracked below via
    // `acc.length`) reserve `dropCap.indentPx` of left margin so wrapping leaves room for the glyph beside them --
    // `ParagraphRole.Body` (every non-drop-cap paragraph) makes both branches below exactly today's behaviour.
    paragraphRole: ParagraphRole,
    dropCap: DropCapLayout.Reserve,
    wrapCache: WrappedLineCache
  ): Vector[TextVisualLine] =
    if maxVisualLines <= 0 then Vector.empty
    else
      val spec =
        WrappedLineKey(
          line,
          panelWidthPx,
          resolver,
          measuredLayout,
          cellMetrics,
          baseColumn,
          paragraphRole,
          dropCap.indentPx
        )
      val memos = wrapCache.layoutMemos
      val rows = wrapCache.wrapped(spec, maxVisualLines)(
        limit => wrapRows(spec, limit, memos),
        predecessors => IncrementalWrap.rewrap(spec, predecessors, memos)
      )
      val placedRows =
        Vector.tabulate(rows.length)(index =>
          DropCapLayout.placed(rows(index), bufferLine, paragraphRole, index, dropCap)
        )
      if rows.length < maxVisualLines then DropCapLayout.reservingGlyphHeight(placedRows, paragraphRole) else placedRows

  private[layout] def wrapRows(spec: WrappedLineKey, maxVisualLines: Int, memos: LayoutMemos): RowWrap =
    import spec.{baseColumn, cellMetrics, measuredLayout, resolver, text}
    if text.isEmpty then
      val empty = measureSegment("", baseColumn, baseColumn, resolver, measuredLayout, cellMetrics, memos)
      RowWrap(Vector(empty), 1, 0, None)
    else
      val measuring = memos.measuring(measuredLayout)
      val initial   = ParagraphMeasurement(spec, 0, maxVisualLines, measuring, memos.boundaries)
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
          val row       = wrapRow(spec, paragraph, startColumn, acc.length, memos)
          val reach =
            if traceable then reaches :+ IncrementalWrap.reachOf(spec, startColumn, row.fitLength, measuring)
            else reaches
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
    lineWithinParagraph: Int,
    memos: LayoutMemos
  ): WrappedRow =
    import spec.{baseColumn, cellMetrics, dropCapIndentPx, measuredLayout, paragraphRole, resolver, text}
    val spanInsetPx = DropCapLayout.leftInsetPx(paragraphRole, lineWithinParagraph, dropCapIndentPx)
    val givenBackPx =
      paragraph.measuring
        .filter(_ => DropCapLayout.isHomeRow(paragraphRole, lineWithinParagraph, baseColumn + startColumn))
        .fold(0.0f)(DropCapLayout.replacedCharacterWidthPx(text, baseColumn, resolver, _).min(spanInsetPx))
    val wrapWidthPx      = math.max(1, spec.panelWidthPx - math.round(spanInsetPx))
    val segmentStart     = baseColumn + startColumn
    val fit              = fittingSegment(paragraph, text, startColumn, wrapWidthPx, segmentStart, spec, givenBackPx)
    val segmentLength    = wordBoundarySegmentLength(text, startColumn, fit.length, memos.boundaries)
    val endColumnInSlice = startColumn + segmentLength
    val row = measureSegment(
      text.substring(startColumn, endColumnInSlice),
      segmentStart,
      baseColumn + endColumnInSlice,
      resolver,
      measuredLayout,
      cellMetrics,
      memos,
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
  /** Reads `line` from `from` on through a character iterator rather than a per-row substring of the paragraph's
    * remainder, which made wrapping one long paragraph quadratic in its length.
    */
  private[layout] def wordBoundarySegmentLength(
    line: String,
    from: Int,
    fittingLength: Int,
    breaks: BoundaryScratch
  ): Int =
    if fittingLength >= line.length - from then line.length - from
    else
      val boundary = breaks.lineBreaks
      boundary.setText(StringCharacterIterator(line, from, line.length, from))
      val candidate = boundary.preceding(from + fittingLength + 1) - from
      if candidate > 0 then candidate else fittingLength

  /** Whether `font` is laid out from measured glyphs rather than on its cell grid, as the measurer `memos`' owner lays
    * text out with decides: never without one.
    */
  private[layout] def shouldUseMeasuredLayout(font: FontSpec, memos: LayoutMemos): Boolean =
    memos.measurer.exists(_.measuresProportionally(font))

  /** The cell grid `font` is laid out on when no caller says otherwise: the measurer's cell for it, or, for an owner
    * with no measurer, the terminal's own cell.
    */
  private[serenity] def cellMetricsOf(font: Font, memos: LayoutMemos): CellMetrics =
    memos.measurer.fold(CellMetrics.cellUnit)(_.cellMetrics(FontSpec.fromAwt(font)))

  /** The line height and ascent of a snapshot of rows in `font`: the measured line box of the font, or the cell's. */
  private[layout] def snapshotLineMetrics(
    font: Font,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    memos: LayoutMemos
  ): (Int, Int) =
    if measuredLayout then
      val extent = memos.lineExtents.of(FontSpec.fromAwt(font))
      (math.max(1, extent.heightPx), math.max(1, extent.ascentPx))
    else (math.max(1, cellMetrics.lineHeight), math.max(0, cellMetrics.ascent))

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
