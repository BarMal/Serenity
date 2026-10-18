package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString
import java.util.concurrent.atomic.AtomicLong

import com.serenity.markdown.MarkdownInlineSpans
import com.serenity.richtext.RichTextDocument
import com.serenity.state.models.{IndentationSettings, MeasuredRow}
import com.serenity.ui.openings.ParagraphRoles
import com.serenity.ui.renderer.FontFirstUse
import com.serenity.ui.theme.{HeadingSettings, MarkdownInlineStyling, RichTextStyling, TextStyle}

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
    * reports a visual line's height/ascent from the highest ascent and deepest descent of the runs covering it.
    * Non-rich lines carry no runs and fall back to the base font everywhere, reproducing the old single-font behaviour
    * exactly.
    */
  final private[layout] case class LineFontResolver(
      baseFont: Font,
      runs: Vector[ColumnFontRun],
      extraAdvances: Map[Int, Float] = Map.empty,
      tabSize: Int = IndentationSettings.DefaultTabSize
  ):
    val hasHidden: Boolean = runs.exists(_.hidden)

    /** Whether the runs are sorted and disjoint, which is what lets [[runAt]] bisect. Overlapping or unordered runs
      * keep the first-listed-wins scan, so the answer never depends on how the runs happened to be supplied.
      */
    private val bisectable: Boolean =
      runs.forall(run => run.startColumn <= run.endColumn) &&
        runs.zip(runs.drop(1)).forall((earlier, later) => earlier.endColumn <= later.startColumn)

    /** Every column a run starts or ends at, ascending: the places a line's font can change. */
    private val boundaries: Vector[Int] = runs.flatMap(run => Vector(run.startColumn, run.endColumn)).distinct.sorted

    private def runAt(column: Int): Option[ColumnFontRun] =
      if bisectable then
        val latestStart = lastIndexWhere(runs.length)(runs(_).startColumn <= column)
        runs.lift(latestStart).filter(run => column < run.endColumn)
      else runs.find(run => column >= run.startColumn && column < run.endColumn)

    /** The greatest index below `size` satisfying `holds`, or `-1`, for a predicate that holds on a prefix. */
    private def lastIndexWhere(size: Int)(holds: Int => Boolean): Int =
      @annotation.tailrec
      def narrow(low: Int, high: Int): Int =
        if low >= high then low - 1
        else
          val middle = (low + high) >>> 1
          if holds(middle) then narrow(middle + 1, high) else narrow(low, middle)
      narrow(0, size)

    def fontAt(column: Int): Font =
      runAt(column).map(_.font).getOrElse(baseFont)

    /** `[startColumn, endColumn)` cut into maximal ranges of one font each, hidden ranges kept apart. */
    def fontRuns(startColumn: Int, endColumn: Int): Vector[FontSpan] =
      val firstInside = lastIndexWhere(boundaries.length)(boundaries(_) <= startColumn) + 1
      val inside      = boundaries.drop(firstInside).takeWhile(_ < endColumn)
      val cuts        = (inside :+ startColumn :+ endColumn).distinct.sorted
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
      val (maxAscent, maxDescent) = fonts.foldLeft((1, 0)) {
        case ((ascent, descent), font) =>
          val extent = FontLineExtents.of(font, frc)
          (math.max(ascent, extent.ascentPx), math.max(descent, extent.descentPx))
      }
      (maxAscent + maxDescent, maxAscent)

  /** A resolver with no per-run fonts: every column measures with the one base font, reproducing the pre-rich
    * single-font behaviour. Used by the plain-text measurement helpers that carry no rich document.
    */
  private[layout] def singleFontResolver(
    font: Font,
    tabSize: Int = IndentationSettings.DefaultTabSize
  ): LineFontResolver =
    LineFontResolver(font, Vector.empty, tabSize = tabSize)

  private[layout] def resolverForLine(
    baseFont: Font,
    richDocument: Option[RichTextDocument],
    bufferLine: Int,
    lineLength: Int,
    proseScale: Float,
    headings: HeadingSettings = HeadingSettings.default,
    roles: ParagraphRoles = ParagraphRoles.enabled
  ): LineFontResolver =
    val runs = richDocument match
      case Some(document) =>
        RichTextStyling
          .styledFontSpans(document, bufferLine, 0, lineLength, proseScale, headings, roles)
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
  private[layout] def landableStops(
    xs: IArray[Float],
    offsets: IArray[Int],
    startColumn: Int,
    resolver: LineFontResolver
  ): (IArray[Float], IArray[Int]) =
    val last = xs.length - 1
    val landable =
      IArray.from(
        (0 until xs.length).filter(index => index == last || !resolver.isHidden(startColumn + offsets(index)))
      )
    MeasuredRow.orderedByX(landable.map(xs(_)), landable.map(offsets(_)))

  /** The resolver for a Markdown line whose emphasis, code and heading `runs` are restyled: the decorated stretches
    * measure in their own fonts, every other column in `baseFont`, and the delimiter characters are hidden when
    * `hideMarkers`.
    */
  private[layout] def markdownResolver(
    baseFont: Font,
    lineLength: Int,
    runs: Vector[MarkdownInlineSpans.Run],
    hideMarkers: Boolean,
    baseIsMonospaced: Boolean,
    headings: HeadingSettings
  ): LineFontResolver =
    def runFor(run: MarkdownInlineSpans.Run): ColumnFontRun =
      if run.isMarker then ColumnFontRun(run.startColumn, run.endColumn, baseFont, hidden = hideMarkers)
      else
        ColumnFontRun(
          run.startColumn,
          run.endColumn,
          TextStyle.styledFont(
            baseFont,
            MarkdownInlineStyling.textStyle(run.style, baseIsMonospaced, baseFont.getSize2D, headings)
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
      if cellMetrics.displayWidthAware then displayWidthCaretXs(text, charWidth, resolver.tabSize)
      else uniformCaretXs(text, charWidth, resolver.tabSize)
    else
      normalizeCollapsedCarets(
        rawMeasuredCaretXs(text, absoluteStartColumn, resolver, frc),
        resolver,
        absoluteStartColumn
      )

  @volatile var diagOn: Boolean = false
  val diagLog = new java.util.concurrent.ConcurrentLinkedQueue[String]()
  def diag(msg: String): Unit = { val _ = diagLog.add(msg) }

  private val textLayoutsBuilt: ThreadLocal[AtomicLong] = ThreadLocal.withInitial(() => AtomicLong(0))

  /** How many `TextLayout`s caret and fit measurement has built on the calling thread: the cost wrapping pays when
    * summed glyph advances cannot answer, which the wrap specs bound per paragraph. Counted per thread so specs running
    * side by side do not see each other's layouts.
    */
  private[layout] def textLayoutsConstructed: Long = textLayoutsBuilt.get().get()

  private def settleFonts(resolver: LineFontResolver, frc: FontRenderContext): Unit =
    FontFirstUse.settleAll(resolver.baseFont +: resolver.runs.map(_.font), frc)

  private def textLayoutOf(text: AttributedString, frc: FontRenderContext): TextLayout =
    val _ = textLayoutsBuilt.get().incrementAndGet()
    if TextCaretMeasurement.diagOn then
      val it = text.getIterator
      val sb = new StringBuilder
      var c = it.first()
      while c != java.text.CharacterIterator.DONE do { sb.append(c); c = it.next() }
      TextCaretMeasurement.diag(s"DIAG textLayout len=${sb.length} text='${sb.toString.take(40)}' stack=\n" +
        new Throwable().getStackTrace.slice(1, 14).mkString(" | "))
    TextLayout(text.getIterator, frc)

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
    settleFonts(resolver, frc)
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
    val layout = textLayoutOf(attributed, frc)
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
    settleFonts(resolver, frc)
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
      val layout = textLayoutOf(attributed, frc)
      carets(text.length) = layout.getAdvance
      (text.length - 1 to 0 by -1).foreach { index =>
        carets(index) =
          if hidden(index) then carets(index + 1)
          else layout.getCaretInfo(TextHitInfo.leading(visibleBefore(index)))(0)
      }
    IArray.unsafeFromArray(carets)

  /** How many characters of a row fit, plus -- when they came from summed advances -- the candidate's raw carets, so
    * the chosen row reuses them instead of being measured a second time. A kerned character's leading caret already
    * holds half the kerning of the one before it, which a row ending there does not feel, so `kerningAfter` holds the
    * pull of each candidate character on its successor, for a row cut short of the candidate.
    */
  final private[layout] case class SegmentFit(
      length: Int,
      reusableRawCarets: Option[IArray[Float]] = None,
      kerningAfter: IArray[Float] = IArray.empty[Float]
  ):

    def caretXsForPrefix(
      prefixLength: Int,
      resolver: LineFontResolver,
      absoluteStartColumn: Int
    ): Option[IArray[Float]] =
      reusableRawCarets
        .filter(_.length > prefixLength)
        .map { raw =>
          val prefix = raw.take(prefixLength + 1)
          val kerning =
            if prefixLength > 0 && prefixLength < raw.length - 1 then kerningAfter(prefixLength - 1) else 0.0f
          normalizeCollapsedCarets(
            if kerning == 0.0f then prefix else prefix.updated(prefixLength, prefix(prefixLength) - kerning / 2.0f),
            resolver,
            absoluteStartColumn
          )
        }

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
        SegmentFit(fittingDisplayWidthSegmentLength(line, from, panelWidthPx, charWidth, spec.resolver.tabSize))
      else SegmentFit(fittingUniformSegmentLength(line, from, panelWidthPx, charWidth, spec.resolver.tabSize))
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
  private[layout] def displayWidthCaretXs(
    text: String,
    charWidth: Float,
    tabSize: Int = IndentationSettings.DefaultTabSize
  ): IArray[Float] =
    val stops = new Array[Float](text.length + 1)
    @annotation.tailrec
    def loop(index: Int, cells: Int): Unit =
      if index >= text.length then stops(index) = cells * charWidth
      else
        val codePoint = text.codePointAt(index)
        val advanced  = cells + TabStops.advance(codePoint, cells, tabSize)
        val charCount = Character.charCount(codePoint)
        stops(index) = cells * charWidth
        if charCount == 2 then stops(index + 1) = advanced * charWidth
        loop(index + charCount, advanced)

    loop(0, 0)
    IArray.unsafeFromArray(stops)

  /** Caret stops for a grid where every character takes one cell and a tab takes its stop's worth. */
  private[layout] def uniformCaretXs(text: String, charWidth: Float, tabSize: Int): IArray[Float] =
    if text.indexOf('\t') < 0 then IArray.tabulate(text.length + 1)(index => index * charWidth)
    else
      val stops = new Array[Float](text.length + 1)
      @annotation.tailrec
      def loop(index: Int, cells: Int): Unit =
        stops(index) = cells * charWidth
        if index < text.length then
          loop(index + 1, cells + (if text.charAt(index) == '\t' then TabStops.cellsAt(cells, tabSize) else 1))

      loop(0, 0)
      IArray.unsafeFromArray(stops)

  /** How many characters of `line` from `from` fit `panelWidthPx` on a grid of one-cell characters and tab stops. Only
    * the first `panelWidthPx / charWidth` characters can matter, since every character takes at least a cell.
    */
  private[layout] def fittingUniformSegmentLength(
    line: String,
    from: Int,
    panelWidthPx: Int,
    charWidth: Int,
    tabSize: Int
  ): Int =
    val cellsAvailable = panelWidthPx / charWidth
    val candidates     = math.min(line.length - from, cellsAvailable)

    @annotation.tailrec
    def hasTab(index: Int): Boolean =
      index < from + candidates && (line.charAt(index) == '\t' || hasTab(index + 1))

    @annotation.tailrec
    def fitting(index: Int, usedCells: Int): Int =
      if index >= line.length then index
      else
        val advance = if line.charAt(index) == '\t' then TabStops.cellsAt(usedCells, tabSize) else 1
        if usedCells + advance > cellsAvailable then index else fitting(index + 1, usedCells + advance)

    if hasTab(from) then math.max(1, fitting(from, 0) - from) else math.max(1, candidates)

  /** How many characters of `text`, starting at `from`, fit in `panelWidthPx` when each glyph costs its own cells.
    * Never splits a wide glyph across the wrap boundary, and never splits a surrogate pair; like the uniform-advance
    * branch it always consumes at least one glyph, so wrapping makes progress even in a panel narrower than one cell.
    */
  private[layout] def fittingDisplayWidthSegmentLength(
    text: String,
    from: Int,
    panelWidthPx: Int,
    charWidth: Int,
    tabSize: Int = IndentationSettings.DefaultTabSize
  ): Int =
    @annotation.tailrec
    def loop(index: Int, usedCells: Int): Int =
      if index >= text.length then index
      else
        val codePoint = text.codePointAt(index)
        val advance   = TabStops.advance(codePoint, usedCells, tabSize)
        if (usedCells + advance) * charWidth > panelWidthPx then index
        else loop(index + Character.charCount(codePoint), usedCells + advance)

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

  private val CollapsedCaretEpsilon = 0.01f

  /** Only a pair of neighbouring stops at the same x past the first can be spread, so lines without one are returned as
    * measured instead of being copied.
    */
  private def hasCollapsedStops(rawXs: IArray[Float]): Boolean =
    (1 until rawXs.length - 1).exists(index => math.abs(rawXs(index + 1) - rawXs(index)) <= CollapsedCaretEpsilon)

  private[layout] def normalizeCollapsedCarets(rawXs: IArray[Float]): IArray[Float] =
    if rawXs.length < 3 || !hasCollapsedStops(rawXs) then rawXs
    else
      val normalized = Array.tabulate(rawXs.length)(rawXs(_))

      @annotation.tailrec
      def plateauEndFrom(index: Int, plateauValue: Float): Int =
        if index + 1 < normalized.length && math.abs(normalized(index + 1) - plateauValue) <= CollapsedCaretEpsilon then
          plateauEndFrom(index + 1, plateauValue)
        else index

      @annotation.tailrec
      def normalizeFrom(index: Int): Unit =
        if index < normalized.length - 1 then
          val plateauValue = normalized(index)
          if math.abs(normalized(index + 1) - plateauValue) <= CollapsedCaretEpsilon then
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
