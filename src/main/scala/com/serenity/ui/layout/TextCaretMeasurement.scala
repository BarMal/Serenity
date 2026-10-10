package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString

import com.serenity.markdown.MarkdownInlineSpans
import com.serenity.richtext.RichTextDocument
import com.serenity.state.models.TextCaretStop
import com.serenity.ui.theme.{MarkdownInlineStyling, RichTextStyling, TextStyle}

/** Caret-stop measurement for one logical line: which AWT font each column measures with, and the x position of every
  * caret boundary under either the fixed cell grid or the measured (proportional/ligature/rich) path. Pure geometry
  * over a line of text -- `TextLayoutSnapshot` composes these into visual lines and wraps them.
  */
private[layout] object TextCaretMeasurement:

  /** The AWT font a single buffer-line column is measured/drawn with, over the buffer's base font. A `hidden` run stays
    * in the line's text but has no width, so it neither advances the line nor takes a caret stop of its own. The
    * resolver's `extraAdvances` are the dual: a column that is wider than its glyph by that many pixels, which a table
    * uses to pad a cell out to its column.
    */
  final private[layout] case class ColumnFontRun(startColumn: Int, endColumn: Int, font: Font, hidden: Boolean = false)

  /** `[startColumn, endColumn)` measured in one `font`, or not at all when `hidden`. */
  final private[layout] case class FontSpan(startColumn: Int, endColumn: Int, font: Font, hidden: Boolean)

  /** Per-logical-line font lookup for the measured path: maps an absolute buffer column to its run's scaled font, and
    * reports a visual line's height/ascent from the tallest run covering it. Non-rich lines carry no runs and fall back
    * to the base font everywhere, reproducing the old single-font behaviour exactly.
    */
  final private[layout] case class LineFontResolver(
      baseFont: Font,
      runs: Vector[ColumnFontRun],
      extraAdvances: Map[Int, Float] = Map.empty
  ):
    val hasHidden: Boolean = runs.exists(_.hidden)

    private def runAt(column: Int): Option[ColumnFontRun] =
      runs.find(run => column >= run.startColumn && column < run.endColumn)

    def fontAt(column: Int): Font =
      runAt(column).map(_.font).getOrElse(baseFont)

    /** `[startColumn, endColumn)` cut into maximal ranges of one font each, hidden ranges kept apart. */
    def fontRuns(startColumn: Int, endColumn: Int): Vector[FontSpan] =
      val cuts = (runs
        .flatMap(run => Vector(run.startColumn, run.endColumn))
        .filter(column => column > startColumn && column < endColumn) :+ startColumn :+ endColumn).distinct.sorted
      cuts.zip(cuts.drop(1)).foldLeft(Vector.empty[FontSpan]) {
        case (merged :+ previous, (_, cutEnd))
            if fontAt(cutEnd - 1) == previous.font && runAt(cutEnd - 1).exists(_.hidden) == previous.hidden =>
          merged :+ previous.copy(endColumn = cutEnd)
        case (merged, (cutStart, cutEnd)) =>
          merged :+ FontSpan(cutStart, cutEnd, fontAt(cutStart), runAt(cutStart).exists(_.hidden))
      }

    /** Which of the `count` columns from `startColumn` are hidden. */
    def hiddenMask(startColumn: Int, count: Int): Array[Boolean] =
      val mask = new Array[Boolean](count)
      runs.filter(_.hidden).foreach { run =>
        (math.max(run.startColumn, startColumn) until math.min(run.endColumn, startColumn + count))
          .foreach(column => mask(column - startColumn) = true)
      }
      mask

    def isHidden(column: Int): Boolean = hasHidden && runAt(column).exists(_.hidden)

    def lineMetrics(frc: FontRenderContext, startColumn: Int, endColumn: Int): (Int, Int) =
      val covering = runs.collect {
        case run if !run.hidden && run.endColumn > startColumn && run.startColumn < endColumn => run.font
      }
      val fonts = if covering.nonEmpty then covering else Vector(baseFont)
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

  /** The stops a click or a vertical move can land on, by x: a stop before a hidden column shares its x with the stop
    * after it, so it would only ever compete with a column the writer can see. A row's last stop stays, so a row of
    * nothing but hidden columns still has somewhere to land.
    */
  private[layout] def landableStops(stops: Vector[TextCaretStop], resolver: LineFontResolver): Vector[TextCaretStop] =
    val landable = stops.dropRight(1).filterNot(stop => resolver.isHidden(stop.column)) ++ stops.lastOption
    landable.sortBy(_.xPx)

  /** The resolver for a Markdown line whose emphasis and code `runs` are restyled: the decorated stretches measure in
    * their own fonts, every other column in `baseFont`, and the delimiter characters are hidden when `hideMarkers`.
    */
  private[layout] def markdownResolver(
    baseFont: Font,
    lineLength: Int,
    runs: Vector[MarkdownInlineSpans.Run],
    hideMarkers: Boolean,
    baseIsMonospaced: Boolean
  ): LineFontResolver =
    def runFor(run: MarkdownInlineSpans.Run): ColumnFontRun =
      if run.isMarker then ColumnFontRun(run.startColumn, run.endColumn, baseFont, hidden = hideMarkers)
      else
        ColumnFontRun(
          run.startColumn,
          run.endColumn,
          TextStyle.styledFont(
            baseFont,
            MarkdownInlineStyling.textStyle(run.style, baseIsMonospaced, baseFont.getSize2D)
          )
        )
    val (covered, columnRuns) = runs.foldLeft((0, Vector.empty[ColumnFontRun])) {
      case ((from, acc), run) =>
        val gap = Option.when(run.startColumn > from)(ColumnFontRun(from, run.startColumn, baseFont))
        (run.endColumn, acc ++ gap :+ runFor(run))
    }
    LineFontResolver(
      baseFont,
      columnRuns ++ Option.when(covered < lineLength)(ColumnFontRun(covered, lineLength, baseFont))
    )

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
    else
      normalizeCollapsedCarets(
        rawMeasuredCaretXs(text, absoluteStartColumn, resolver, frc),
        resolver,
        absoluteStartColumn
      )

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
    withExtraAdvances(
      if resolver.hasHidden then visibleLayoutCaretXs(text, absoluteStartColumn, resolver, frc)
      else plainLayoutCaretXs(text, absoluteStartColumn, resolver, frc),
      absoluteStartColumn,
      resolver
    )

  /** `carets` with each column's extra advance added to every caret after it. */
  private def withExtraAdvances(
    carets: IArray[Float],
    absoluteStartColumn: Int,
    resolver: LineFontResolver
  ): IArray[Float] =
    if resolver.extraAdvances.isEmpty then carets
    else
      val shifts = (0 until carets.length).scanLeft(0.0f) { (sum, index) =>
        sum + resolver.extraAdvances.getOrElse(absoluteStartColumn + index, 0.0f)
      }
      IArray.tabulate(carets.length)(index => carets(index) + shifts(index))

  private def plainLayoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): IArray[Float] =
    val attributed = AttributedString(text)
    // One FONT attribute per contiguous run of equal per-column fonts, so a mixed-size rich line's caret advances
    // (and thus wrap points and widths) match the per-run glyphs the draw path paints.
    resolver.fontRuns(absoluteStartColumn, absoluteStartColumn + text.length).foreach { span =>
      attributed.addAttribute(
        TextAttribute.FONT,
        span.font,
        span.startColumn - absoluteStartColumn,
        span.endColumn - absoluteStartColumn
      )
    }
    val layout = TextLayout(attributed.getIterator, frc)
    val carets = new Array[Float](text.length + 1)
    (0 until text.length).foreach(index => carets(index) = layout.getCaretInfo(TextHitInfo.leading(index))(0))
    carets(text.length) = layout.getAdvance
    IArray.unsafeFromArray(carets)

  /** [[layoutCaretXs]] for a line with hidden columns: the visible characters are laid out as if the hidden ones were
    * not in the text, and each hidden column takes the caret of the next visible one.
    */
  private def visibleLayoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): IArray[Float] =
    val hidden        = resolver.hiddenMask(absoluteStartColumn, text.length)
    val visibleBefore = hidden.scanLeft(0)((count, isHidden) => if isHidden then count else count + 1)
    val visibleText   = text.indices.filterNot(hidden(_)).map(text.charAt(_)).mkString
    val carets        = new Array[Float](text.length + 1)
    if visibleText.nonEmpty then
      val attributed = AttributedString(visibleText)
      resolver.fontRuns(absoluteStartColumn, absoluteStartColumn + text.length).filterNot(_.hidden).foreach { span =>
        attributed.addAttribute(
          TextAttribute.FONT,
          span.font,
          visibleBefore(span.startColumn - absoluteStartColumn),
          visibleBefore(span.endColumn - absoluteStartColumn)
        )
      }
      val layout = TextLayout(attributed.getIterator, frc)
      carets(text.length) = layout.getAdvance
      (text.length - 1 to 0 by -1).foreach { index =>
        carets(index) =
          if hidden(index) then carets(index + 1)
          else layout.getCaretInfo(TextHitInfo.leading(visibleBefore(index)))(0)
      }
    IArray.unsafeFromArray(carets)

  /** How many characters of a row fit, plus -- when they came from summed advances -- the candidate's raw carets, so
    * the chosen row reuses them instead of being measured a second time.
    */
  final private[layout] case class SegmentFit(length: Int, reusableRawCarets: Option[IArray[Float]] = None):

    def caretXsForPrefix(
      prefixLength: Int,
      resolver: LineFontResolver,
      absoluteStartColumn: Int
    ): Option[IArray[Float]] =
      reusableRawCarets
        .filter(_.length > prefixLength)
        .map(raw => normalizeCollapsedCarets(raw.take(prefixLength + 1), resolver, absoluteStartColumn))

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
        layoutCaretXs(line.substring(from, from + limit), absoluteStartColumn, resolver, frc),
        resolver,
        absoluteStartColumn
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

  /** [[normalizeCollapsedCarets]] of carets whose first character is at `absoluteStartColumn`. A hidden column's caret
    * is the next visible one's, which is no collapse to spread apart, so only the visible stops are normalised.
    */
  private[layout] def normalizeCollapsedCarets(
    rawXs: IArray[Float],
    resolver: LineFontResolver,
    absoluteStartColumn: Int
  ): IArray[Float] =
    if !resolver.hasHidden then normalizeCollapsedCarets(rawXs)
    else
      val characters = rawXs.length - 1
      val hidden     = resolver.hiddenMask(absoluteStartColumn, characters)
      val visible    = (0 until characters).filterNot(hidden(_))
      val normalised = normalizeCollapsedCarets(IArray.from(visible.map(rawXs(_)) :+ rawXs(characters)))
      val carets     = new Array[Float](rawXs.length)
      carets(characters) = normalised(visible.length)

      @annotation.tailrec
      def fill(index: Int, visibleRank: Int): Unit =
        if index >= 0 then
          if hidden(index) then
            carets(index) = carets(index + 1)
            fill(index - 1, visibleRank)
          else
            carets(index) = normalised(visibleRank - 1)
            fill(index - 1, visibleRank - 1)

      fill(characters - 1, visible.length)
      IArray.unsafeFromArray(carets)

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
