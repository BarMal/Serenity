package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.richtext.ParagraphRole
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.layout.TextCaretMeasurement.{
  LineFontResolver,
  markdownResolver,
  resolverForLine,
  singleFontResolver
}
import com.serenity.ui.theme.RichTextStyling

/** The line-level inputs of a wrap -- per-run fonts, drop cap role and glyph width -- shared by the painted layout and
  * every row count so the two cannot disagree.
  */
private[layout] object LineLayout:

  /** The rows one logical line wraps into, exactly as the painted layout wraps it: per-run fonts, the paragraph's drop
    * cap role and the glyph's reserved width. Everything that counts or locates rows -- centring, scrolling, the
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
      inputs.glyphWidthPx,
      wrapCache
    )

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

  final case class LineLayoutInputs(
      resolver: LineFontResolver,
      paragraphRole: ParagraphRole,
      glyphWidthPx: Float
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
    val resolver =
      if !measuredLayout then singleFontResolver(font)
      else if richText.markdown.isActive then markdownAwareResolver(font, richText, lineIndex, lineLength, lineText)
      else resolverForLine(font, richText.document, lineIndex, lineLength, richText.proseScale)
    val paragraphRole = RichTextStyling.effectiveRole(
      richText.document.flatMap(_.paragraphAt(lineIndex)).map(_.role).getOrElse(ParagraphRole.Body),
      richText.dropCapsEnabled
    )
    val glyphWidthPx =
      if measuredLayout then
        DropCapLayout.measuredGlyphWidthPx(font, frc, richText.document, lineIndex, paragraphRole, richText.proseScale)
      else 0.0f
    LineLayoutInputs(resolver, paragraphRole, glyphWidthPx)

  private def markdownAwareResolver(
    font: Font,
    richText: RichTextContext,
    lineIndex: Int,
    lineLength: Int,
    lineText: String
  ): LineFontResolver =
    val runs = richText.markdown.runsOn(lineIndex, lineText)
    if runs.isEmpty then resolverForLine(font, richText.document, lineIndex, lineLength, richText.proseScale)
    else
      markdownResolver(
        font,
        lineLength,
        runs,
        richText.markdown.hidesMarkersOn(lineIndex),
        richText.markdown.baseIsMonospaced
      )
