package com.serenity.ui.layout

import java.text.StringCharacterIterator

import com.ibm.icu.text.BreakIterator
import com.serenity.ui.layout.TextCaretMeasurement.{SegmentFit, normalizeCollapsedCarets}

/** What wrapping a logical line needs measured, computed once over a window of it rather than once per row: grapheme
  * boundaries from a single sweep and, for a measured layout, every character's glyph advance. Rows are then fitted and
  * shaped by arithmetic over those arrays; any row the arrays cannot answer exactly falls back to per-row measurement.
  * The window is bounded so a row-limited wrap of a huge line measures only what its rows can reach.
  */
final private[layout] class ParagraphMeasurement private (
    spec: WrappedLineKey,
    start: Int,
    end: Int,
    boundaries: Array[Int],
    val advances: Option[GlyphAdvances]
):
  import ParagraphMeasurement.*

  /** Characters whose glyph advances this window measured. */
  def measuredChars: Int = if advances.isDefined then end - start else 0

  /** This window, or a fresh one from `from` when the next row could run past its end. */
  def coveringRowAt(from: Int, rowsLeft: Int): ParagraphMeasurement =
    val budget = rowBudget(spec)
    if from >= start && (end == spec.text.length || from + budget <= end) then this
    else ParagraphMeasurement(spec, from, rowsLeft)

  /** A row's caret-stop offsets from its start. Agrees with sweeping the row alone whenever the row both starts and
    * ends on this window's boundaries, since grapheme rules never look back past a boundary; a row cut mid-cluster (a
    * forced break inside an emoji sequence) is swept alone.
    */
  def graphemeOffsets(from: Int, until: Int): IArray[Int] =
    val first = java.util.Arrays.binarySearch(boundaries, from)
    if from < start || until > end || first < 0 || java.util.Arrays.binarySearch(boundaries, until) < 0 then
      graphemeBoundaryOffsets(spec.text.substring(from, until))
    else
      @annotation.tailrec
      def lastInside(index: Int): Int =
        if index + 1 < boundaries.length && boundaries(index + 1) < until then lastInside(index + 1) else index
      val count   = lastInside(first) - first + 1
      val offsets = new Array[Int](count + 1)
      (0 until count).foreach(index => offsets(index) = boundaries(first + index) - from)
      offsets(count) = until - from
      IArray.unsafeFromArray(offsets)

  /** The row fit the per-row `TextLayout` search would find, read from the measured advances -- or `None` when the
    * candidate leaves the window, holds a character needing per-row measurement, or lands too near the width for float
    * rounding between the two measurements to be ruled out.
    */
  def measuredFit(from: Int, panelWidthPx: Int): Option[SegmentFit] =
    val remaining = spec.text.length - from
    val width     = panelWidthPx.toFloat
    @annotation.tailrec
    def loop(limit: Int): Option[SegmentFit] =
      advances match
        case Some(measured) if from + limit <= end && measured.isContextFree(from - start, from - start + limit) =>
          val raw  = measured.caretsFrom(from - start, limit)
          val last = raw(limit)
          if math.abs(last - width) < DecisionGuardPx then None
          else if limit >= remaining || last > width then settledFit(raw, limit, width)
          else loop(math.min(remaining, math.max(limit + 1, limit * 2)))
        case _ => None
    loop(math.min(remaining, initialCandidateLength(panelWidthPx, spec.cellMetrics)))

private[layout] object ParagraphMeasurement:

  /** Comfortably above the largest gap seen between summed glyph advances and `TextLayout` carets (under 0.01px). */
  private val DecisionGuardPx = 0.05f

  private[layout] val MaxWindowChars = 16384

  private val threadLocalCharacterBreaks: ThreadLocal[BreakIterator] =
    ThreadLocal.withInitial(() => BreakIterator.getCharacterInstance())

  def apply(spec: WrappedLineKey, from: Int, rowsLeft: Int): ParagraphMeasurement =
    val budget = rowBudget(spec).toLong
    val wanted = math.max(budget, math.min(MaxWindowChars.toLong, rowsLeft.toLong * budget))
    val end    = math.min(spec.text.length.toLong, from + wanted).toInt
    val advances = Option.when(spec.measuredLayout)(
      GlyphAdvances.measure(spec.text, from, end, spec.baseColumn, spec.resolver, spec.frc)
    )
    new ParagraphMeasurement(spec, from, end, boundariesOf(spec.text, from, end), advances)

  /** A window over the whole of `spec.text` whose advances were assembled rather than measured in one sweep. It holds
    * no grapheme boundaries, so every row sweeps its own, which agrees with reading them off a window's boundaries.
    */
  def assembled(spec: WrappedLineKey, advances: Option[GlyphAdvances]): ParagraphMeasurement =
    new ParagraphMeasurement(spec, 0, spec.text.length, Array.empty[Int], advances)

  /** The first candidate the per-row search measures, in characters. */
  def initialCandidateLength(panelWidthPx: Int, cellMetrics: CellMetrics): Int =
    math.max(16, panelWidthPx / math.max(1, cellMetrics.charWidth) + 32)

  private def rowBudget(spec: WrappedLineKey): Int = 2 * initialCandidateLength(spec.panelWidthPx, spec.cellMetrics)

  def graphemeBoundaryOffsets(text: String): IArray[Int] =
    IArray.unsafeFromArray(boundariesOf(text, 0, text.length))

  private def boundariesOf(text: String, from: Int, until: Int): Array[Int] =
    val breaks = threadLocalCharacterBreaks.get()
    breaks.setText(StringCharacterIterator(text, from, until, from))
    val found = new Array[Int](until - from + 1)
    @annotation.tailrec
    def collect(count: Int, boundary: Int): Int =
      if boundary == BreakIterator.DONE then count
      else
        found(count) = boundary
        collect(count + 1, breaks.next())
    java.util.Arrays.copyOf(found, collect(0, breaks.first()))

  /** Mirrors the per-row search's stopping rule over the candidate's normalized carets. */
  private def settledFit(raw: IArray[Float], limit: Int, width: Float): Option[SegmentFit] =
    val carets     = normalizeCollapsedCarets(raw)
    val firstOver  = carets.indexWhere(_ > width)
    val maxFitting = if firstOver < 0 then limit else firstOver - 1
    val nearFit    = maxFitting >= 0 && width - carets(maxFitting) < DecisionGuardPx
    val nearOver   = firstOver >= 0 && carets(firstOver) - width < DecisionGuardPx
    Option.unless(nearFit || nearOver)(SegmentFit(math.max(1, maxFitting), Some(raw)))
