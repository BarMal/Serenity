package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString

import com.serenity.richtext.RichTextDocument
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
  final private[layout] case class LineFontResolver(baseFont: Font, runs: Vector[ColumnFontRun]):
    def fontAt(column: Int): Font =
      runs.find(run => column >= run.startColumn && column < run.endColumn).map(_.font).getOrElse(baseFont)

    /** `[startColumn, endColumn)` cut into maximal ranges of one font each. */
    def fontRuns(startColumn: Int, endColumn: Int): Vector[(Int, Int, Font)] =
      val cuts = (runs
        .flatMap(run => Vector(run.startColumn, run.endColumn))
        .filter(column => column > startColumn && column < endColumn) :+ startColumn :+ endColumn).distinct.sorted
      cuts.zip(cuts.drop(1)).foldLeft(Vector.empty[(Int, Int, Font)]) {
        case (merged :+ ((runStart, _, font)), (_, cutEnd)) if fontAt(cutEnd - 1) == font =>
          merged :+ ((runStart, cutEnd, font))
        case (merged, (cutStart, cutEnd)) => merged :+ ((cutStart, cutEnd, fontAt(cutStart)))
      }

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
  ): IArray[Float] =
    if text.isEmpty then IArray(0.0f)
    else if !measuredLayout then
      val charWidth = cellMetrics.charWidth.toFloat
      if cellMetrics.displayWidthAware then displayWidthCaretXs(text, charWidth)
      else IArray.tabulate(text.length + 1)(index => index * charWidth)
    else normalizeCollapsedCarets(rawMeasuredCaretXs(text, absoluteStartColumn, resolver, frc))

  /** Each character's leading caret plus the run's full advance, before [[normalizeCollapsedCarets]]: summed glyph
    * advances when every character is context-free (see [[GlyphAdvances]]), a `TextLayout` otherwise.
    */
  private[layout] def rawMeasuredCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): IArray[Float] =
    if text.isEmpty then IArray(0.0f)
    else
      val advances =
        if GlyphAdvances.hasContextFreeCharacters(text) then
          Some(GlyphAdvances.measure(text, 0, text.length, absoluteStartColumn, resolver, frc))
        else None
      advances.filter(_.isContextFree(0, text.length)) match
        case Some(measured) => measured.caretsFrom(0, text.length)
        case None           => layoutCaretXs(text, absoluteStartColumn, resolver, frc)

  /** Carets read back from one `TextLayout` of `text`: exact for any text, but a caret query per character. */
  private def layoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): IArray[Float] =
    val attributed = AttributedString(text)
    // One FONT attribute per contiguous run of equal per-column fonts, so a mixed-size rich line's caret advances
    // (and thus wrap points and widths) match the per-run glyphs the draw path paints.
    resolver.fontRuns(absoluteStartColumn, absoluteStartColumn + text.length).foreach { (runStart, runEnd, font) =>
      attributed.addAttribute(TextAttribute.FONT, font, runStart - absoluteStartColumn, runEnd - absoluteStartColumn)
    }
    val layout = TextLayout(attributed.getIterator, frc)
    val carets = new Array[Float](text.length + 1)
    (0 until text.length).foreach(index => carets(index) = layout.getCaretInfo(TextHitInfo.leading(index))(0))
    carets(text.length) = layout.getAdvance
    IArray.unsafeFromArray(carets)

  /** How many characters of a row fit, plus -- when they came from summed advances -- the candidate's raw carets, so
    * the chosen row reuses them instead of being measured a second time.
    */
  final private[layout] case class SegmentFit(length: Int, reusableRawCarets: Option[IArray[Float]] = None):
    def caretXsForPrefix(prefixLength: Int): Option[IArray[Float]] =
      reusableRawCarets.filter(_.length > prefixLength).map(raw => normalizeCollapsedCarets(raw.take(prefixLength + 1)))

  private[layout] def fittingSegment(
    paragraph: ParagraphMeasurement,
    line: String,
    from: Int,
    panelWidthPx: Int,
    absoluteStartColumn: Int,
    spec: WrappedLineKey
  ): SegmentFit =
    val cellMetrics = spec.cellMetrics
    if !spec.measuredLayout then
      val charWidth = math.max(1, cellMetrics.charWidth)
      if cellMetrics.displayWidthAware then
        SegmentFit(fittingDisplayWidthSegmentLength(line, from, panelWidthPx, charWidth))
      else SegmentFit(math.max(1, math.min(line.length - from, panelWidthPx / charWidth)))
    else
      paragraph
        .measuredFit(from, panelWidthPx)
        .getOrElse(
          fittingMeasuredSegment(line, from, panelWidthPx, absoluteStartColumn, spec.resolver, spec.frc, cellMetrics)
        )

  /** The exact search: lays out ever-longer candidates until one overflows the row. */
  private def fittingMeasuredSegment(
    line: String,
    from: Int,
    panelWidthPx: Int,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    cellMetrics: CellMetrics
  ): SegmentFit =
    val remainingLength = line.length - from
    val width           = panelWidthPx.toFloat

    @annotation.tailrec
    def loop(limit: Int): SegmentFit =
      val carets = normalizeCollapsedCarets(
        layoutCaretXs(line.substring(from, from + limit), absoluteStartColumn, resolver, frc)
      )
      val firstOver  = carets.indexWhere(_ > width)
      val maxFitting = if firstOver < 0 then limit else math.max(0, firstOver - 1)
      if limit >= remainingLength || carets(limit) > width || maxFitting < limit then
        SegmentFit(math.max(1, maxFitting))
      else loop(math.min(remainingLength, math.max(limit + 1, limit * 2)))

    loop(math.min(remainingLength, ParagraphMeasurement.initialCandidateLength(panelWidthPx, cellMetrics)))

  /** Caret stops for a display-width-aware cell grid: each codepoint advances by its own cell count ([[CharWidth]])
    * rather than one cell per character, so the stops agree with the cells `TerminalScreenBuffer` actually paints. A
    * surrogate pair contributes one advance across its two char indices; its low half is never a grapheme boundary (see
    * [[ParagraphMeasurement.graphemeBoundaryOffsets]]) and so never a caret stop, and taking the glyph's trailing edge
    * there keeps the sequence non-decreasing for the callers that index it by raw column.
    */
  private[layout] def displayWidthCaretXs(text: String, charWidth: Float): IArray[Float] =
    val stops = new Array[Float](text.length + 1)
    @annotation.tailrec
    def loop(index: Int, xPx: Float): Unit =
      if index >= text.length then stops(index) = xPx
      else
        val codePoint = text.codePointAt(index)
        val advanced  = xPx + CharWidth.of(codePoint) * charWidth
        val charCount = Character.charCount(codePoint)
        stops(index) = xPx
        if charCount == 2 then stops(index + 1) = advanced
        loop(index + charCount, advanced)

    loop(0, 0.0f)
    IArray.unsafeFromArray(stops)

  /** How many characters of `text`, starting at `from`, fit in `panelWidthPx` when each glyph costs its own cells.
    * Never splits a wide glyph across the wrap boundary, and never splits a surrogate pair; like the uniform-advance
    * branch it always consumes at least one glyph, so wrapping makes progress even in a panel narrower than one cell.
    */
  private[layout] def fittingDisplayWidthSegmentLength(
    text: String,
    from: Int,
    panelWidthPx: Int,
    charWidth: Int
  ): Int =
    @annotation.tailrec
    def loop(index: Int, usedPx: Int): Int =
      if index >= text.length then index
      else
        val codePoint = text.codePointAt(index)
        val advance   = CharWidth.of(codePoint) * charWidth
        if usedPx + advance > panelWidthPx then index
        else loop(index + Character.charCount(codePoint), usedPx + advance)

    val fitted = loop(from, 0) - from
    if fitted > 0 then fitted else math.min(text.length - from, Character.charCount(text.codePointAt(from)))

  private[layout] def normalizeCollapsedCarets(rawXs: IArray[Float]): IArray[Float] =
    if rawXs.length < 3 then rawXs
    else
      val normalized = Array.tabulate(rawXs.length)(rawXs(_))
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

      IArray.unsafeFromArray(normalized)
