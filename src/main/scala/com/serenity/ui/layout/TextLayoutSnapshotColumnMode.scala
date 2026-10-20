package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.richtext.RichTextDocument
import com.serenity.state.models.{Buffer, IndentationSettings, TextVisualLine}
import com.serenity.ui.fonts.{FontFallback, FontLoader}
import com.serenity.ui.layout.ColumnPaging.{ColumnStart, Frame}
import com.serenity.ui.openings.OpeningSettings
import com.serenity.ui.theme.HeadingSettings

/** Column-based document layout (issue #1338, Phase 1): the two entry points column-mode rendering needs on top of
  * [[TextLayoutSnapshot.fromBuffer]]'s ordinary single-window slice. Split out of `TextLayoutSnapshot` itself once
  * adding these pushed that file past the architecture ratchet's file-length target (following `LayoutEngine`'s own
  * `export`-from-a-sibling-object precedent); [[TextLayoutSnapshot]] exports both so `TextLayoutSnapshot.foo` call
  * sites are unaffected by where the code actually lives.
  */
object TextLayoutSnapshotColumnMode:

  import TextLayoutSnapshot.{defaultFontRenderContext, shouldUseMeasuredLayout}

  private def columnContext(
    document: Option[RichTextDocument],
    proseScale: Float,
    dropCapsEnabled: Boolean,
    tabSize: Int,
    headings: HeadingSettings,
    openings: OpeningSettings,
    dropCapGapEm: Float,
    wrapCache: WrappedLineCache,
    fallback: FontFallback
  ): RichTextContext =
    RichTextContext(
      document,
      proseScale,
      dropCapsEnabled,
      tabSize = tabSize,
      headings = headings,
      virtualDropCaps = document.flatMap(wrapCache.virtualDropCaps(_, openings)),
      dropCapGapEm = dropCapGapEm,
      fallback = fallback
    )

  /** The pager for `buffer` at one layout: the one place the painter and the viewport anchoring read column breaks
    * from. A column is `visibleLines` base lines tall; `proseScale`, the font and the cell metrics decide how tall each
    * row is, so callers must pass what they paint with.
    */
  def pagerFor(
    buffer: Buffer,
    columnWidthPx: Int,
    font: Font,
    visibleLines: Int,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    proseScale: Float = 1.0f,
    dropCapsEnabled: Boolean = true,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    tabSize: Int = IndentationSettings.DefaultTabSize,
    headings: HeadingSettings = HeadingSettings.default,
    openings: OpeningSettings = OpeningSettings.default,
    dropCapGapEm: Float = DropCapLayout.DefaultGapEm,
    fallback: FontFallback = FontFallback.Default
  ): ColumnPager =
    val cellMetrics = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout =
      !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext, wrapCache.shapedText.probes)
    val lineHeightPx =
      if measuredLayout then math.max(1, wrapCache.layoutMemos.lineExtents.of(font, fontRenderContext).heightPx)
      else math.max(1, cellMetrics.lineHeight)
    val width   = math.max(1, columnWidthPx)
    val content = buffer.document.content
    val richDocument =
      buffer.richText.richTextDocument.filter(_.matchesPlainTextShape(content.lineCount, content.weight))
    val richText =
      columnContext(
        richDocument,
        proseScale,
        dropCapsEnabled,
        tabSize,
        headings,
        openings,
        dropCapGapEm,
        wrapCache,
        fallback
      )
    val key = BandHeightKey(
      VisualRowKey(
        buffer.id,
        width,
        font,
        fontRenderContext,
        cellMetricsOverride,
        forceCellLayout,
        dropCapsEnabled,
        fallback
      ),
      proseScale
    )
    val revision = ContentRevision(buffer.document.contentVersion, buffer.document.changesSince)
    ColumnPager(
      richText,
      content.lineCount,
      Frame(math.max(1, visibleLines) * math.max(lineHeightPx, cellMetrics.lineHeight), lineHeightPx, measuredLayout),
      measure => wrapCache.bandHeights(key, content, measure, VisualLineIndexStore.stampOf(richText), Some(revision)),
      (line, maxRows) =>
        LineLayout.placedWrappedLine(
          line,
          content.getLine(line).getOrElse(""),
          width,
          font,
          fontRenderContext,
          measuredLayout,
          cellMetrics,
          maxRows,
          richText,
          wrapCache
        )
    )

  /** The rows of each of `columnCount` columns of the page anchored at the viewport's own `topLine`/`topVisualLine`
    * (already snapped to a column boundary by `CursorViewport.adjustForCursorColumnMode`): columns are filled by pixel
    * height, so one holds fewer rows when its rows are tall. A document that runs out of lines before filling every
    * column simply yields a shorter final chunk.
    */
  def columnChunksForBuffer(
    buffer: Buffer,
    columnWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    proseScale: Float = 1.0f,
    columnCount: Int = 1,
    // `document.drop_caps_enabled` config toggle -- see `TextLayoutSnapshot.fromBuffer`'s own parameter.
    dropCapsEnabled: Boolean = true,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    tabSize: Int = IndentationSettings.DefaultTabSize,
    headings: HeadingSettings = HeadingSettings.default,
    openings: OpeningSettings = OpeningSettings.default,
    dropCapGapEm: Float = DropCapLayout.DefaultGapEm,
    fallback: FontFallback = FontFallback.Default
  ): Vector[Vector[TextVisualLine]] =
    pagerFor(
      buffer,
      columnWidthPx,
      font,
      buffer.viewport.visibleLines,
      fontRenderContext,
      cellMetricsOverride,
      forceCellLayout,
      proseScale,
      dropCapsEnabled,
      wrapCache,
      tabSize,
      headings,
      openings,
      dropCapGapEm,
      fallback
    ).fill(ColumnStart(buffer.viewport.topLine, buffer.viewport.topVisualLine), columnCount)

  /** The snapshot for the ONE column currently showing -- `columnChunksForBuffer`'s first chunk (the viewport's own
    * `topLine`/`topVisualLine` is always already snapped to a page boundary by
    * `CursorViewport.adjustForCursorColumnMode`, so that first chunk is always the page's first column), wrapped as an
    * ordinary [[TextLayoutSnapshot]] so every downstream consumer (painting, cursor placement) needs no column-mode
    * branch of its own. `panelWidthPx` is the column's own (narrower) width, not the pane's full width. Delegates to
    * [[fromBufferColumns]] and takes the first column; kept as a named entry point for the single-column consumers
    * (`MouseTargetCache`, `RendererPaneSetup`) that only ever want one column.
    */
  def fromBufferColumn(
    buffer: Buffer,
    columnWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    proseScale: Float = 1.0f,
    dropCapsEnabled: Boolean = true,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    tabSize: Int = IndentationSettings.DefaultTabSize,
    headings: HeadingSettings = HeadingSettings.default,
    openings: OpeningSettings = OpeningSettings.default,
    dropCapGapEm: Float = DropCapLayout.DefaultGapEm,
    fallback: FontFallback = FontFallback.Default
  ): TextLayoutSnapshot =
    fromBufferColumns(
      buffer,
      columnWidthPx,
      font,
      fontRenderContext,
      cellMetricsOverride,
      forceCellLayout,
      proseScale,
      columnCount = 1,
      dropCapsEnabled,
      wrapCache,
      tabSize,
      headings,
      openings,
      dropCapGapEm,
      fallback
    ).headOption.getOrElse(
      emptyColumnSnapshot(
        columnWidthPx,
        font,
        fontRenderContext,
        cellMetricsOverride,
        forceCellLayout,
        proseScale,
        headings,
        wrapCache
      )
    )

  /** All `columnCount` per-column snapshots for the page anchored at the viewport's own `topLine`/`topVisualLine` (the
    * page-first-column boundary set by `CursorViewport.adjustForCursorColumnMode`): text fills column 1 top-to-bottom
    * (`visibleLines` rows) then flows into column 2, etc. Each chunk from [[columnChunksForBuffer]] is wrapped into its
    * own ordinary [[TextLayoutSnapshot]] at the shared (narrower) `columnWidthPx`, so the renderer composes the page by
    * painting each at its own x-origin (`contentRect.x + columnIndex * (columnWidthCells + columnGap)`) -- generalizing
    * the single-column machinery rather than introducing a new data model. A document that runs out of lines before
    * filling every column simply yields a shorter final snapshot (and no snapshots past it).
    */
  def fromBufferColumns(
    buffer: Buffer,
    columnWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    proseScale: Float = 1.0f,
    columnCount: Int = 1,
    dropCapsEnabled: Boolean = true,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    tabSize: Int = IndentationSettings.DefaultTabSize,
    headings: HeadingSettings = HeadingSettings.default,
    openings: OpeningSettings = OpeningSettings.default,
    dropCapGapEm: Float = DropCapLayout.DefaultGapEm,
    fallback: FontFallback = FontFallback.Default
  ): Vector[TextLayoutSnapshot] =
    val cellMetrics = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout =
      !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext, wrapCache.shapedText.probes)
    val lineHeightPx =
      if measuredLayout then math.max(1, wrapCache.layoutMemos.lineExtents.of(font, fontRenderContext).heightPx)
      else math.max(1, cellMetrics.lineHeight)
    val ascentPx =
      if measuredLayout then math.max(1, wrapCache.layoutMemos.lineExtents.of(font, fontRenderContext).ascentPx)
      else math.max(0, cellMetrics.ascent)
    val totalLines = buffer.document.content.lineCount
    val richDocument =
      buffer.richText.richTextDocument.filter(_.matchesPlainTextShape(totalLines, buffer.document.content.weight))
    val laidOutWith =
      columnContext(
        richDocument,
        proseScale,
        dropCapsEnabled,
        tabSize,
        headings,
        openings,
        dropCapGapEm,
        wrapCache,
        fallback
      )

    columnChunksForBuffer(
      buffer,
      columnWidthPx,
      font,
      fontRenderContext,
      cellMetricsOverride,
      forceCellLayout,
      proseScale,
      columnCount,
      dropCapsEnabled,
      wrapCache,
      tabSize,
      headings,
      openings,
      dropCapGapEm,
      fallback
    ).map { chunk =>
      TextLayoutSnapshot(
        visualLines = chunk,
        panelWidthPx = math.max(1, columnWidthPx),
        lineHeightPx = lineHeightPx,
        ascentPx = ascentPx,
        isProportional = !FontLoader.isMonospacedFont(font),
        usesMeasuredLayout = measuredLayout,
        richTextDocument = richDocument,
        proseScale = proseScale,
        headings = headings,
        roles = laidOutWith.roles
      )
    }

  /** The empty-content fallback `fromBufferColumn` returns when the page has no rows at all (`columnChunksForBuffer`
    * yields nothing) -- an empty single-column snapshot with the same per-column width and metrics the real path would
    * have produced, so callers never see a width of zero.
    */
  private def emptyColumnSnapshot(
    columnWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext,
    cellMetricsOverride: Option[CellMetrics],
    forceCellLayout: Boolean,
    proseScale: Float,
    headings: HeadingSettings,
    wrapCache: WrappedLineCache
  ): TextLayoutSnapshot =
    val cellMetrics = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout =
      !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext, wrapCache.shapedText.probes)
    val lineHeightPx =
      if measuredLayout then math.max(1, wrapCache.layoutMemos.lineExtents.of(font, fontRenderContext).heightPx)
      else math.max(1, cellMetrics.lineHeight)
    val ascentPx =
      if measuredLayout then math.max(1, wrapCache.layoutMemos.lineExtents.of(font, fontRenderContext).ascentPx)
      else math.max(0, cellMetrics.ascent)
    TextLayoutSnapshot(
      visualLines = Vector.empty,
      panelWidthPx = math.max(1, columnWidthPx),
      lineHeightPx = lineHeightPx,
      ascentPx = ascentPx,
      isProportional = !FontLoader.isMonospacedFont(font),
      usesMeasuredLayout = measuredLayout,
      proseScale = proseScale,
      headings = headings
    )
