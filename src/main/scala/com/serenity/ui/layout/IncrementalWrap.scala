package com.serenity.ui.layout

import com.serenity.state.models.{TextCaretStop, TextVisualLine}

/** A wrapped line plus what resuming from it after an edit needs: the text offset up to which each row's break decision
  * read ([[IncrementalWrap.reachOf]]), and the glyph advances the wrap used.
  */
final private[layout] case class WrapTrace(reaches: IArray[Int], advances: Option[GlyphAdvances])

/** `computedRows` and `measuredChars` count the work this wrap did rather than reused. */
final private[layout] case class RowWrap(
    rows: Vector[TextVisualLine],
    computedRows: Int,
    measuredChars: Int,
    trace: Option[WrapTrace]
)

final private[layout] case class WrappedRow(line: TextVisualLine, endColumn: Int, fitLength: Int)

/** Re-wraps an edited line by redoing only the rows the edit can have changed, producing exactly what
  * [[TextLayoutSnapshot]]'s cold wrap of the new text produces.
  *
  * Rows are chosen by a left-to-right walk in which the row starting at `from` depends only on `text[from..]` and on
  * the glyph advances of the characters it reads, so an old wrap stays valid where the text it read is unchanged:
  *
  *   - Prefix. Row `i` is kept when `reaches(i) <= edit start`, and one further row is redone as a margin (an added
  *     space in a row's first word can move a word up from the row below, and the row above reads that word).
  *   - Walk. Rows are computed by the cold wrap's own [[TextLayoutSnapshot.wrapRow]] over advances that equal what the
  *     cold wrap measures (below), so every row equals its cold counterpart.
  *   - Resync. Once a new row ends at or past the first text whose advances are untouched, at a position where an old
  *     row started (shifted by the length delta), every later old row is what the cold wrap would produce: its start
  *     and everything it reads are the same text with the same advances. Those rows are shifted and appended.
  *
  * The cold wrap measures one paragraph-wide window of glyph advances. A window re-measured around the edit and spliced
  * into the old advances is identical to it as long as both cuts are [[ShapingBarriers]]; the fresh window runs from
  * the barrier before the edit to the first barrier past its end.
  *
  * Excluded, because the cold wrap cannot be reproduced from rows alone: rich-text runs (per-run fonts), drop caps (the
  * inset depends on the row index), row-limited wraps (the cold wrap's window then depends on the limit), paragraphs
  * over one measurement window, edits over [[MaxEditChars]], and text where a break or an advance may depend on text
  * far away: dictionary-segmented scripts (ICU breaks a whole CJK or Thai run together), and, for a measured layout,
  * surrogates, format and control characters and anything `Font.textRequiresLayout`.
  */
private[layout] object IncrementalWrap:

  val MaxEditChars = 128

  /** More than the UAX#14 rules ever look past the character they decide on: a break depends on the characters up to
    * the one after it, and a few more for numeric and quotation sequences.
    */
  val IcuLookaheadUnits = 16

  /** Reach of a row whose decision read up to the end of the text, which any edit of its length can change. */
  val ToEndOfText = Int.MaxValue

  /** Whether a wrap of `spec` can be resumed from, or serve as the base for resuming. */
  def traceable(spec: WrappedLineKey): Boolean =
    val text = spec.text
    text.nonEmpty && text.length <= ParagraphMeasurement.MaxWindowChars && spec.resolver.runs.isEmpty &&
    !DropCapLayout.spansLine(spec.paragraphRole, 0) && text.forall(breaksFromNearbyText) &&
    (!spec.measuredLayout || GlyphAdvances.hasContextFreeCharacters(text))

  /** Scripts ICU's line instance segments with a dictionary, which sees the whole run of them at once. */
  private def breaksFromNearbyText(char: Char): Boolean =
    !(char >= 0x0e00 && char <= 0x0eff || char >= 0x1000 && char <= 0x109f || char >= 0x1780 && char <= 0x17ff ||
      char >= 0x19e0 && char <= 0x19ff || char >= 0x3040 && char <= 0x30ff || char >= 0x31f0 && char <= 0x31ff ||
      char >= 0x3400 && char <= 0x4dbf || char >= 0x4e00 && char <= 0x9fff || char >= 0xf900 && char <= 0xfaff ||
      char >= 0xff66 && char <= 0xff9f)

  /** The offset up to which the row starting at `from`, with `fitLength` characters fitting, read the text. The fit
    * search lays out candidates of the initial length doubling until one overflows, so it reads at most the larger of
    * that and twice the fit; the break search reads one past the fit and ICU's lookahead; and the advances of the
    * characters read depend on their shaping run, which ends at the next barrier (and the pair across it).
    */
  def reachOf(spec: WrappedLineKey, from: Int, fitLength: Int): Int =
    val text     = spec.text
    val initial  = ParagraphMeasurement.initialCandidateLength(spec.panelWidthPx, spec.cellMetrics)
    val searched = from + math.max(initial, 2 * fitLength + 2)
    val read     = math.max(searched, from + fitLength + 1 + IcuLookaheadUnits)
    if read >= text.length then ToEndOfText
    else if !spec.measuredLayout then read
    else
      val barrier = ShapingBarriers.cutAtOrAfter(text, searched, spec.resolver.baseFont, spec.frc)
      if barrier >= text.length then ToEndOfText else math.max(read, barrier + 1)

  /** The wrap of `spec` resumed from the candidate whose text differs least from it, or `None` when no candidate
    * qualifies.
    */
  def rewrap(spec: WrappedLineKey, bufferLine: Int, predecessors: Seq[Predecessor]): Option[RowWrap] =
    if predecessors.isEmpty || !traceable(spec) then None
    else
      closestEdit(spec.text, predecessors.iterator)
        .flatMap((candidate, edit) => resume(spec, bufferLine, candidate, edit))

  /** Candidates arrive newest first, and an edit of a character or two cannot be beaten, so the scan stops there. */
  private def closestEdit(text: String, candidates: Iterator[Predecessor]): Option[(Predecessor, EditSpan)] =
    @annotation.tailrec
    def scan(best: Option[(Predecessor, EditSpan)]): Option[(Predecessor, EditSpan)] =
      if best.exists(_._2.changedChars <= SmallestEditChars) || !candidates.hasNext then best
      else
        val candidate = candidates.next()
        val closer =
          EditSpan.between(candidate.key.text, text).filter(edit => best.forall(_._2.changedChars > edit.changedChars))
        scan(closer.map(candidate -> _).orElse(best))
    scan(None)

  private val SmallestEditChars = 2

  final private case class EditSpan(prefix: Int, suffix: Int, oldLength: Int, newLength: Int):
    def changedChars: Int = oldLength + newLength - 2 * (prefix + suffix)
    def delta: Int        = newLength - oldLength

    /** Where the text shared with the old one resumes. */
    def changedEnd: Int = newLength - suffix

  private object EditSpan:

    def between(old: String, text: String): Option[EditSpan] =
      Option
        .when(old != text && math.abs(old.length - text.length) <= MaxEditChars) {
          val shared = math.min(old.length, text.length)
          val prefix = commonPrefix(old, text, shared)
          EditSpan(prefix, commonSuffix(old, text, shared - prefix), old.length, text.length)
        }
        .filter(_.changedChars <= MaxEditChars)

    @annotation.tailrec
    private def commonPrefix(old: String, text: String, limit: Int, count: Int = 0): Int =
      if count < limit && old.charAt(count) == text.charAt(count) then commonPrefix(old, text, limit, count + 1)
      else count

    @annotation.tailrec
    private def commonSuffix(old: String, text: String, limit: Int, count: Int = 0): Int =
      if count < limit && old.charAt(old.length - 1 - count) == text.charAt(text.length - 1 - count) then
        commonSuffix(old, text, limit, count + 1)
      else count

  /** The advances for the new text and the first offset from which they are the old ones. */
  final private case class Assembled(advances: Option[GlyphAdvances], untouchedFrom: Int, measuredChars: Int)

  private def assemble(spec: WrappedLineKey, candidate: Predecessor, edit: EditSpan): Option[Assembled] =
    if !spec.measuredLayout then Some(Assembled(None, edit.changedEnd, 0))
    else
      candidate.trace.advances.filter(_.length == edit.oldLength).map { old =>
        val font   = spec.resolver.baseFont
        val from   = ShapingBarriers.cutAtOrBefore(spec.text, edit.prefix - 1, font, spec.frc)
        val until  = ShapingBarriers.cutAtOrAfter(spec.text, edit.changedEnd + 1, font, spec.frc)
        val middle = GlyphAdvances.measure(spec.text, from, until, spec.baseColumn, spec.resolver, spec.frc)
        Assembled(Some(old.spliced(from, middle, until - edit.delta)), until, until - from)
      }

  private def resume(
    spec: WrappedLineKey,
    bufferLine: Int,
    candidate: Predecessor,
    edit: EditSpan
  ): Option[RowWrap] =
    assemble(spec, candidate, edit).map { assembled =>
      val oldRows  = candidate.rows
      val reaches  = candidate.trace.reaches
      val reader   = reaches.indexWhere(_ > edit.prefix)
      val kept     = math.max(0, (if reader < 0 then oldRows.length else reader) - 1)
      val measured = ParagraphMeasurement.assembled(spec, assembled.advances)

      /** The old row that began at `column` of the old text, if any: every row after it is still right. */
      def oldRowStarting(column: Int): Option[Int] =
        @annotation.tailrec
        def search(low: Int, high: Int): Option[Int] =
          if low > high then None
          else
            val middle = (low + high) >>> 1
            val start  = oldRows(middle).startColumn - spec.baseColumn
            if start == column then Some(middle)
            else if start < column then search(middle + 1, high)
            else search(low, middle - 1)
        search(0, oldRows.length - 1)

      def reuseFrom(oldIndex: Int, rows: Vector[TextVisualLine], freshReaches: Vector[Int]): RowWrap =
        val tail = oldRows.drop(oldIndex).map(shifted(_, edit.delta, bufferLine, candidate.bufferLine))
        done(rows ++ tail, freshReaches, oldIndex, computed = rows.length - kept)

      /** The reaches of the kept rows, the fresh ones, and the old ones from `tailFrom` shifted by the length change.
        */
      def done(rows: Vector[TextVisualLine], freshReaches: Vector[Int], tailFrom: Int, computed: Int): RowWrap =
        val tailLength = if tailFrom < 0 then 0 else reaches.length - tailFrom
        val merged     = new Array[Int](kept + freshReaches.length + tailLength)
        (0 until kept).foreach(index => merged(index) = reaches(index))
        freshReaches.indices.foreach(index => merged(kept + index) = freshReaches(index))
        val tailAt = kept + freshReaches.length
        (0 until tailLength).foreach { index =>
          val reach = reaches(tailFrom + index)
          merged(tailAt + index) = if reach == ToEndOfText then reach else reach + edit.delta
        }
        RowWrap(
          rows,
          computed,
          assembled.measuredChars,
          Some(WrapTrace(IArray.unsafeFromArray(merged), assembled.advances))
        )

      @annotation.tailrec
      def walk(startColumn: Int, rows: Vector[TextVisualLine], freshReaches: Vector[Int]): RowWrap =
        val row       = TextLayoutSnapshot.wrapRow(spec, bufferLine, measured, startColumn, rows.length)
        val nextRows  = rows :+ row.line
        val nextReach = freshReaches :+ reachOf(spec, startColumn, row.fitLength)
        if row.endColumn >= spec.text.length then done(nextRows, nextReach, -1, computed = nextRows.length - kept)
        else
          val unchanged = Option.when(row.endColumn >= assembled.untouchedFrom)(row.endColumn - edit.delta)
          unchanged.flatMap(oldRowStarting) match
            case Some(oldIndex) => reuseFrom(oldIndex, nextRows, nextReach)
            case None           => walk(row.endColumn, nextRows, nextReach)

      val keptRows = oldRows.take(kept).map(shifted(_, 0, bufferLine, candidate.bufferLine))
      val resumeAt = if kept == 0 then 0 else oldRows(kept - 1).endColumn - spec.baseColumn
      walk(resumeAt, keptRows, Vector.empty)
    }

  /** `row` moved by `delta` columns onto `bufferLine`, which is what the cold wrap of the shifted text produces. */
  private def shifted(row: TextVisualLine, delta: Int, bufferLine: Int, fromBufferLine: Int): TextVisualLine =
    if delta == 0 && bufferLine == fromBufferLine then row
    else
      def move(stops: Vector[TextCaretStop]): Vector[TextCaretStop] =
        if delta == 0 then stops else stops.map(stop => stop.copy(column = stop.column + delta))
      val stops = move(row.caretStops)
      row.copy(
        bufferLine = bufferLine,
        startColumn = row.startColumn + delta,
        endColumn = row.endColumn + delta,
        caretStops = stops,
        xSortedCaretStops = if row.xSortedCaretStops eq row.caretStops then stops else move(row.xSortedCaretStops)
      )
