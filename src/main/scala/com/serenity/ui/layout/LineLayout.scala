package com.serenity.ui.layout

import java.awt.Font

import com.serenity.markdown.MarkdownInlineSpans
import com.serenity.richtext.ParagraphRole
import com.serenity.state.models.{IndentationSettings, TextVisualLine, Viewport}
import com.serenity.ui.fonts.{FontFallback, FontLoader}
import com.serenity.ui.layout.TextCaretMeasurement.{
  LineFontResolver,
  markdownResolver,
  resolverForLine,
  singleFontResolver
}
import com.serenity.ui.renderer.FontSpec

/** The line-level inputs of a wrap -- per-run fonts, drop cap role and the room its glyph reserves -- shared by the
  * painted layout and every row count so the two cannot disagree.
  */
private[layout] object LineLayout:

  /** The rows one logical line wraps into, exactly as the painted layout wraps it: per-run fonts, the paragraph's drop
    * cap role and the room its glyph reserves. Everything that counts or locates rows -- centring, scrolling, the
    * cursor's own row -- goes through this, so what is counted is what is painted.
    */
  def wrappedLine(
    text: String,
    bufferLine: Int,
    panelWidthPx: Int,
    font: Font,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    richText: RichTextContext,
    lineLength: Int,
    baseColumn: Int = 0,
    maxVisualLines: Int = Int.MaxValue,
    wrapCache: WrappedLineCache
  ): Vector[TextVisualLine] =
    // A slice of a line cannot be scanned for the markup that opened before it.
    val lineText = if baseColumn == 0 then text else ""
    val inputs =
      lineLayoutInputs(
        font,
        measuredLayout,
        richText,
        bufferLine,
        lineLength,
        lineText,
        fallbackText = text,
        fallbackFirstColumn = baseColumn,
        wrapCache = wrapCache
      )
    TextLayoutSnapshot.wrapLogicalLine(
      text,
      bufferLine,
      panelWidthPx,
      inputs.resolver,
      measuredLayout,
      cellMetrics,
      baseColumn,
      maxVisualLines,
      inputs.paragraphRole,
      inputs.dropCap,
      wrapCache
    )

  /** The resolver a line of `text` with no rich-text styling measures with, whose first character is at `startColumn`:
    * the one the wrapped layout derives for it, so measuring a row's text alone gives the row the wrap produced.
    */
  def plainResolver(
    text: String,
    startColumn: Int,
    font: Font,
    measuredLayout: Boolean,
    tabSize: Int = IndentationSettings.DefaultTabSize,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    fallback: FontFallback = FontFallback.Default
  ): LineFontResolver =
    lineLayoutInputs(
      font,
      measuredLayout,
      RichTextContext.plain.copy(tabSize = tabSize, fallback = fallback),
      0,
      text.length,
      "",
      fallbackText = text,
      fallbackFirstColumn = startColumn,
      wrapCache = wrapCache
    ).resolver

  /** The visible slice of an unwrapped line as one row, placed as the painted layout places it. The slice runs a couple
    * of columns past the viewport so a caret at its edge still has a stop to land on.
    */
  def unwrappedLine(
    lineIndex: Int,
    rawLine: String,
    viewport: Viewport,
    panelWidthPx: Int,
    font: Font,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    richText: RichTextContext,
    wrapCache: WrappedLineCache
  ): Vector[TextVisualLine] =
    val startColumn = math.min(viewport.leftColumn, rawLine.length)
    val endColumn   = math.min(rawLine.length, startColumn + math.max(1, viewport.visibleColumns) + OverscanColumns)
    val slice       = rawLine.slice(startColumn, endColumn)
    val inputs =
      lineLayoutInputs(font, measuredLayout, richText, lineIndex, rawLine.length, rawLine, wrapCache = wrapCache)
    val row =
      SegmentMeasurement.measureSegment(
        slice,
        startColumn,
        startColumn + slice.length,
        inputs.resolver,
        measuredLayout,
        cellMetrics,
        wrapCache.layoutMemos
      )
    val wrapped = DropCapLayout.reservingGlyphHeight(
      Vector(DropCapLayout.placed(row, lineIndex, inputs.paragraphRole, lineWithinParagraph = 0, inputs.dropCap)),
      inputs.paragraphRole
    )
    val aligned = ParagraphAlignmentLayout.apply(wrapped, lineIndex, panelWidthPx, richText)
    HeadingSpacingLayout.apply(aligned, inputs.headingLevel, true, font, measuredLayout, cellMetrics, richText)

  private val OverscanColumns = 2

  /** The rows of one whole-line wrap (at most `maxVisualLines` of them), placed as the painted layout places them:
    * alignment, drop cap inset and heading space applied. A heading's space below its last row is left off when the
    * limit cut the paragraph short, since that row is not the paragraph's last.
    */
  def placedWrappedLine(
    lineIndex: Int,
    rawLine: String,
    panelWidthPx: Int,
    font: Font,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    maxVisualLines: Int,
    richText: RichTextContext,
    wrapCache: WrappedLineCache
  ): Vector[TextVisualLine] =
    if maxVisualLines <= 0 then Vector.empty
    else
      val inputs =
        LineLayout.lineLayoutInputs(
          font,
          measuredLayout,
          richText,
          lineIndex,
          rawLine.length,
          rawLine,
          wrapCache = wrapCache
        )
      val wrapped = TextLayoutSnapshot.wrapLogicalLine(
        rawLine,
        lineIndex,
        panelWidthPx,
        inputs.resolver,
        measuredLayout,
        cellMetrics,
        0,
        maxVisualLines,
        inputs.paragraphRole,
        inputs.dropCap,
        wrapCache
      )
      val aligned  = ParagraphAlignmentLayout.apply(wrapped, lineIndex, panelWidthPx, richText)
      val complete = wrapped.length < maxVisualLines
      HeadingSpacingLayout.apply(aligned, inputs.headingLevel, complete, font, measuredLayout, cellMetrics, richText)

  /** A cell grid is forced when the caller has no glyph metrics at all, or the owner of `memos` no measurer; otherwise
    * a font is measured glyph by glyph when it needs to be, or when hiding Markdown markers needs it to be.
    */
  def measuredLayoutFor(font: Font, forceCellLayout: Boolean, richText: RichTextContext, memos: LayoutMemos): Boolean =
    !forceCellLayout && memos.measurer.isDefined &&
      (richText.requiresMeasuredLayout || TextLayoutSnapshot.shouldUseMeasuredLayout(FontSpec.fromAwt(font), memos))

  /** `headingLevel` is the level of the heading the line is, whether its paragraph says so or its Markdown does. */
  final case class LineLayoutInputs(
      resolver: LineFontResolver,
      paragraphRole: ParagraphRole,
      dropCap: DropCapLayout.Reserve,
      headingLevel: Option[Int]
  )

  /** Cell layout (TUI) never consults per-run fonts -- one glyph per cell, one row per line -- nor spans a drop cap
    * glyph across rows (`DropCapRenderer.renderGlyphCell`), so only a measured layout derives them. `lineText` is the
    * whole logical line, which the inline Markdown view scans for the stretches it restyles.
    */
  def lineLayoutInputs(
    font: Font,
    measuredLayout: Boolean,
    richText: RichTextContext,
    lineIndex: Int,
    lineLength: Int,
    lineText: String,
    fallbackText: String = "",
    fallbackFirstColumn: Int = 0,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): LineLayoutInputs =
    val memos     = wrapCache.layoutMemos
    val measuring = memos.measuring(measuredLayout)
    val inputs = measuring.map { measured =>
      val markdownRuns = Option.when(richText.markdown.isActive)(richText.markdown.runsOn(lineIndex, lineText))
      val styled =
        markdownRuns.fold(
          resolverForLine(
            font,
            richText.document,
            lineIndex,
            lineLength,
            richText.proseScale,
            richText.headings,
            richText.roles
          )
        )(markdownAwareResolver(font, richText, lineIndex, lineLength, lineText, measured, _))
      val paragraphRole = richText.layoutRole(lineIndex)
      LineLayoutInputs(
        withFallback(styled, font, richText, lineText, fallbackText, fallbackFirstColumn, wrapCache, measured),
        paragraphRole,
        DropCapLayout.reserve(
          font,
          richText.document,
          lineIndex,
          paragraphRole,
          richText.proseScale,
          richText.dropCapGapEm,
          memos,
          richText.letterShape(lineIndex)
        ),
        headingLevelOf(paragraphRole, markdownRuns)
      )
    }
    inputs.getOrElse {
      val paragraphRole = richText.layoutRole(lineIndex)
      LineLayoutInputs(
        singleFontResolver(FontSpec.fromAwt(font), richText.tabSize),
        paragraphRole,
        DropCapLayout.Reserve.none,
        headingLevelOf(paragraphRole, None)
      )
    }

  private def headingLevelOf(role: ParagraphRole, markdownRuns: Option[Vector[MarkdownInlineSpans.Run]]): Option[Int] =
    role match
      case ParagraphRole.Heading(level) => Some(level)
      case _ =>
        markdownRuns.flatMap(_.collectFirst { case run if run.style.headingLevel > 0 => run.style.headingLevel })

  /** `styled` with the runs the line needs in fallback fonts. The whole logical line is itemised when it is at hand, a
    * wrapped slice (which cannot see what precedes it) otherwise. Code rows keep their pitch whatever glyphs the
    * fallback fonts add, and set each wide character on two cells of the advance of `0`.
    */
  private def withFallback(
    styled: LineFontResolver,
    font: Font,
    richText: RichTextContext,
    lineText: String,
    sliceText: String,
    sliceFirstColumn: Int,
    wrapCache: WrappedLineCache,
    measuring: MeasuredBy
  ): LineFontResolver =
    val (text, firstColumn) = if lineText.nonEmpty then (lineText, 0) else (sliceText, sliceFirstColumn)
    if text.isEmpty || wrapCache.isPlainAscii(text) then styled
    else
      val monospaced = FontLoader.isMonospacedFont(font)
      val runs       = wrapCache.fallbackRuns(font, richText.fallback, text, stickyNeutrals = !monospaced)
      val itemised   = FallbackResolution(styled, runs, firstColumn, richText.fallback)
      if !monospaced then itemised
      else
        val coded = if itemised eq styled then styled else itemised.copy(fixedLinePitch = true)
        if WideCells.hasWide(text) || (itemised ne styled) then
          coded.copy(wideCellPx = measuring.measurer.glyphWidthPx(FontSpec.fromAwt(font), "0"))
        else coded

  private def markdownAwareResolver(
    font: Font,
    richText: RichTextContext,
    lineIndex: Int,
    lineLength: Int,
    lineText: String,
    measuring: MeasuredBy,
    runs: Vector[MarkdownInlineSpans.Run]
  ): LineFontResolver =
    if runs.isEmpty then
      resolverForLine(
        font,
        richText.document,
        lineIndex,
        lineLength,
        richText.proseScale,
        richText.headings,
        richText.roles
      )
    else
      val resolver = markdownResolver(
        font,
        lineLength,
        runs,
        richText.markdown.hidesMarkersOn(lineIndex),
        richText.markdown.baseIsMonospaced,
        richText.headings
      )
      richText.markdown
        .columnsOn(lineIndex)
        .fold(resolver)(MarkdownTableMetrics.padded(lineText, resolver, _, measuring))
