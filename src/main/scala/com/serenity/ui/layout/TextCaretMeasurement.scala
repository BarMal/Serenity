package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString

import com.serenity.richtext.RichTextDocument
import com.serenity.text.TextEditing
import com.serenity.ui.theme.{RichTextStyling, TextStyle}

/** Caret-stop measurement for one logical line: which AWT font each column measures with, and the x position of every
  * caret boundary under either the fixed cell grid or the measured (proportional/ligature/rich) path. Pure geometry
  * over a line of text -- `TextLayoutSnapshot` composes these into visual lines and wraps them.
  */
private[layout] object TextCaretMeasurement:

  /** The AWT font a single buffer-line column is measured/drawn with, over the buffer's base font. */
  final private[layout] case class ColumnFontRun(startColumn: Int, endColumn: Int, font: Font)

  /** Per-logical-line font lookup for the measured path: maps an absolute buffer column to its run's scaled font, and
    * reports a visual line's height/ascent from the tallest run covering it. Non-rich lines carry no runs and fall back
    * to the base font everywhere, reproducing the old single-font behaviour exactly.
    */
  final private[layout] class LineFontResolver(baseFont: Font, runs: Vector[ColumnFontRun]):
    def fontAt(column: Int): Font =
      runs.find(run => column >= run.startColumn && column < run.endColumn).map(_.font).getOrElse(baseFont)

    def lineMetrics(frc: FontRenderContext, startColumn: Int, endColumn: Int): (Int, Int) =
      val covering = runs.collect { case run if run.endColumn > startColumn && run.startColumn < endColumn => run.font }
      val fonts    = if covering.nonEmpty then covering else Vector(baseFont)
      val (height, ascent) =
        fonts.foldLeft((1, 1)) {
          case ((maxHeight, maxAscent), font) =>
            val line = font.getLineMetrics("Mg", frc)
            (
              math.max(maxHeight, math.ceil(line.getHeight.toDouble).toInt),
              math.max(maxAscent, math.ceil(line.getAscent.toDouble).toInt)
            )
        }
      (height, ascent)

  /** A resolver with no per-run fonts: every column measures with the one base font, reproducing the pre-rich
    * single-font behaviour. Used by the plain-text measurement helpers that carry no rich document.
    */
  private[layout] def singleFontResolver(font: Font): LineFontResolver =
    LineFontResolver(font, Vector.empty)

  private[layout] def resolverForLine(
    baseFont: Font,
    richDocument: Option[RichTextDocument],
    bufferLine: Int,
    lineLength: Int,
    proseScale: Float
  ): LineFontResolver =
    val runs = richDocument match
      case Some(document) =>
        RichTextStyling
          .styledFontSpans(document, bufferLine, 0, lineLength, proseScale)
          .foldLeft((0, Vector.empty[ColumnFontRun])) {
            case ((offset, acc), span) =>
              val end = offset + span.text.length
              (end, acc :+ ColumnFontRun(offset, end, TextStyle.styledFont(baseFont, span.style)))
          }
          ._2
          .toVector
      case None => Vector.empty
    LineFontResolver(baseFont, runs)

  private[layout] def caretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
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
      // One FONT attribute per contiguous run of equal per-column fonts, so a mixed-size rich line's caret advances
      // (and thus wrap points and widths) match the per-run glyphs the draw path paints.
      @annotation.tailrec
      def applyRuns(index: Int): Unit =
        if index < text.length then
          val runFont = resolver.fontAt(absoluteStartColumn + index)
          @annotation.tailrec
          def runEnd(cursor: Int): Int =
            if cursor < text.length && resolver.fontAt(absoluteStartColumn + cursor) == runFont then runEnd(cursor + 1)
            else cursor
          val end = runEnd(index + 1)
          attributed.addAttribute(TextAttribute.FONT, runFont, index, end)
          applyRuns(end)
      applyRuns(0)
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
  private[layout] def displayWidthCaretXs(text: String, charWidth: Float): Vector[Float] =
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
  private[layout] def fittingDisplayWidthSegmentLength(text: String, panelWidthPx: Int, charWidth: Int): Int =
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

  private[layout] def graphemeBoundaryOffsets(text: String): Vector[Int] =
    @annotation.tailrec
    def loop(offset: Int, acc: Vector[Int]): Vector[Int] =
      if offset >= text.length then if acc.lastOption.contains(text.length) then acc else acc :+ text.length
      else
        val next = TextEditing.nextGraphemeBoundary(text, offset)
        loop(next, acc :+ next)

    loop(0, Vector(0))

  private[layout] def normalizeCollapsedCarets(rawXs: Vector[Float]): Vector[Float] =
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
