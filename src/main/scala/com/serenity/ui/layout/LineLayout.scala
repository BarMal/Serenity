package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.markdown.MarkdownInlineSpans
import com.serenity.richtext.ParagraphRole
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.layout.TextCaretMeasurement.{
  LineFontResolver,
  markdownResolver,
  resolverForLine,
  singleFontResolver
}

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
    frc: FontRenderContext,
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
    val inputs   = lineLayoutInputs(font, frc, measuredLayout, richText, bufferLine, lineLength, lineText)
    TextLayoutSnapshot.wrapLogicalLine(
      text,
      bufferLine,
      panelWidthPx,
      inputs.resolver,
      frc,
      measuredLayout,
      cellMetrics,
      baseColumn,
      maxVisualLines,
      inputs.paragraphRole,
      inputs.dropCap,
      wrapCache
    )

  /** The rows of one whole-line wrap (at most `maxVisualLines` of them), placed as the painted layout places them:
    * alignment, drop cap inset and heading space applied. A heading's space below its last row is left off when the
    * limit cut the paragraph short, since that row is not the paragraph's last.
    */
  def placedWrappedLine(
    lineIndex: Int,
    rawLine: String,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    maxVisualLines: Int,
    richText: RichTextContext,
    wrapCache: WrappedLineCache
  ): Vector[TextVisualLine] =
    if maxVisualLines <= 0 then Vector.empty
    else
      val inputs =
        LineLayout.lineLayoutInputs(font, frc, measuredLayout, richText, lineIndex, rawLine.length, rawLine)
      val wrapped = TextLayoutSnapshot.wrapLogicalLine(
        rawLine,
        lineIndex,
        panelWidthPx,
        inputs.resolver,
        frc,
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

  /** A cell grid is forced only when the caller has no glyph metrics at all; otherwise a font is measured glyph by
    * glyph when it needs to be, or when hiding Markdown markers needs it to be.
    */
  def measuredLayoutFor(
    font: Font,
    frc: FontRenderContext,
    forceCellLayout: Boolean,
    richText: RichTextContext
  ): Boolean =
    !forceCellLayout && (richText.requiresMeasuredLayout || TextLayoutSnapshot.shouldUseMeasuredLayout(font, frc))

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
    frc: FontRenderContext,
    measuredLayout: Boolean,
    richText: RichTextContext,
    lineIndex: Int,
    lineLength: Int,
    lineText: String
  ): LineLayoutInputs =
    val markdownRuns =
      if measuredLayout && richText.markdown.isActive then Some(richText.markdown.runsOn(lineIndex, lineText))
      else None
    val resolver =
      if !measuredLayout then singleFontResolver(font, richText.tabSize)
      else
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
        )(markdownAwareResolver(font, frc, richText, lineIndex, lineLength, lineText, _))
    val paragraphRole = richText.layoutRole(lineIndex)
    val dropCap =
      if measuredLayout then
        DropCapLayout.reserve(
          font,
          frc,
          richText.document,
          lineIndex,
          paragraphRole,
          richText.proseScale,
          richText.dropCapGapEm
        )
      else DropCapLayout.Reserve.none
    LineLayoutInputs(resolver, paragraphRole, dropCap, headingLevelOf(paragraphRole, markdownRuns))

  private def headingLevelOf(role: ParagraphRole, markdownRuns: Option[Vector[MarkdownInlineSpans.Run]]): Option[Int] =
    role match
      case ParagraphRole.Heading(level) => Some(level)
      case _ =>
        markdownRuns.flatMap(_.collectFirst { case run if run.style.headingLevel > 0 => run.style.headingLevel })

  private def markdownAwareResolver(
    font: Font,
    frc: FontRenderContext,
    richText: RichTextContext,
    lineIndex: Int,
    lineLength: Int,
    lineText: String,
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
      richText.markdown.columnsOn(lineIndex).fold(resolver)(MarkdownTableMetrics.padded(lineText, resolver, _, frc))
