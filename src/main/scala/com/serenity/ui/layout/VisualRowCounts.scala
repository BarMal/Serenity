package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import scala.annotation.tailrec

import com.serenity.config.MarkdownViewMode
import com.serenity.rope.ChangeSet
import com.serenity.state.models.{Buffer, BufferId, IndentationSettings}

/** How many visual rows the logical lines of one buffer's content wrap into under one wrap setting -- the counts
  * scrolling, centring, page moves and jumps place the viewport with. Every answer is exact: it measures each line it
  * depends on, and only those, so an index's unmeasured-line estimates never reach a viewport position or a caret.
  */
sealed abstract class VisualRowCounts:

  def lineCount: Int

  def rowsIn(line: Int): Int

  /** Rows in `[from, until)`, negated when `until` is above `from`. */
  def rowsBetween(from: Int, until: Int): Int

  /** The line and row within it `rows` (> 0) visual rows above the top of `line`, or `(0, 0)` when fewer lie above. */
  def rowAbove(line: Int, rows: Int): (Int, Int)

  /** The line and row within it `rows` visual rows below the top of `line`, or `None` once the document has ended. */
  def rowBelow(line: Int, rows: Int): Option[(Int, Int)]

  /** As [[rowsBetween]], but a line nobody has measured counts as one row instead of being measured: constant time on a
    * document of any size, exact over whatever has been measured so far.
    */
  def estimatedRowsBetween(from: Int, until: Int): Int

  /** The rows above `topLine` and in the whole document, in one pass over the counts. The lines from `topLine` to
    * `visibleUntil` are measured; so is every other line when `exact`, and otherwise those nobody has measured count
    * one row each.
    */
  def rowsAboveAndTotal(topLine: Int, visibleUntil: Int, exact: Boolean): (Int, Int)

  /** The line and row within it holding visual row `row` of the whole document, found without measuring: the last row
    * of the document when `row` lies past it. The inverse of [[estimatedRowsBetween]] from the document's start.
    */
  def estimatedLineAtRow(row: Int): (Int, Int)

/** What a line's visual row count depends on besides its text. The buffer's rich-text styling and the way its inline
  * Markdown is restyled are the other inputs: they change without the text changing, so they stamp the index instead
  * (see [[VisualLineIndexStore]]) -- a new document or a caret on another line would otherwise leave a new key behind
  * per change and crowd other buffers' indexes out.
  */
final private[layout] case class VisualRowKey(
    bufferId: BufferId,
    panelWidthPx: Int,
    font: Font,
    frc: FontRenderContext,
    cellMetricsOverride: Option[CellMetrics],
    forceCellLayout: Boolean,
    dropCapsEnabled: Boolean
)

object VisualRowCounts:

  /** Counts `buffer`'s lines as [[TextLayoutSnapshot.boundedVisualLinesForText]] wraps them, through `wrapCache`'s
    * per-buffer index when it keeps one. `known` answers for lines already wrapped the same way, so they are not
    * wrapped again.
    */
  def forBuffer(
    buffer: Buffer,
    panelWidthPx: Int,
    font: Font,
    cellMetricsOverride: Option[CellMetrics],
    forceCellLayout: Boolean,
    wrapCache: WrappedLineCache,
    dropCapsEnabled: Boolean = true,
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    tabSize: Int = IndentationSettings.DefaultTabSize,
    known: Int => Option[Int] = _ => None
  ): VisualRowCounts =
    val content = buffer.document.content
    val frc     = TextLayoutSnapshot.defaultFontRenderContext()
    val richText =
      RichTextContext.forBuffer(
        buffer,
        font,
        dropCapsEnabled,
        markdownViewMode,
        forceCellLayout,
        wrapCache,
        frc,
        tabSize
      )
    def measure(line: Int): Int =
      known(line).getOrElse(wrap(line))
    def wrap(line: Int): Int =
      TextLayoutSnapshot
        .boundedVisualLinesForText(
          content.getLine(line).getOrElse(""),
          line,
          panelWidthPx,
          font,
          frc,
          cellMetricsOverride = cellMetricsOverride,
          forceCellLayout = forceCellLayout,
          wrapCache = wrapCache,
          richText = richText
        )
        .length
        .max(1)
    val key = VisualRowKey(buffer.id, panelWidthPx, font, frc, cellMetricsOverride, forceCellLayout, dropCapsEnabled)
    val revision = ContentRevision(buffer.document.contentVersion, buffer.document.changesSince)
    wrapCache.visualRowCounts(key, content, measure, VisualLineIndexStore.stampOf(richText), Some(revision))

  def oneRowPerLine(lineCount: Int): VisualRowCounts = walking(lineCount, _ => 1)

  /** Measures every line it passes on every call. */
  def walking(lineCount: Int, measure: Int => Int): VisualRowCounts = new Walking(lineCount, measure)

  final private class Walking(val lineCount: Int, measure: Int => Int) extends VisualRowCounts:

    def rowsIn(line: Int): Int = measure(line)

    def rowsBetween(from: Int, until: Int): Int =
      if from <= until then (from until until).map(measure).sum else -(until until from).map(measure).sum

    @tailrec
    def rowAbove(line: Int, rows: Int): (Int, Int) =
      if line <= 0 then (0, 0)
      else
        val previousLineRows = measure(line - 1)
        if previousLineRows >= rows then (line - 1, previousLineRows - rows)
        else rowAbove(line - 1, rows - previousLineRows)

    def rowBelow(line: Int, rows: Int): Option[(Int, Int)] = below(line, 0, rows)

    def estimatedRowsBetween(from: Int, until: Int): Int = rowsBetween(from, until)

    def rowsAboveAndTotal(topLine: Int, visibleUntil: Int, exact: Boolean): (Int, Int) =
      (rowsBetween(0, topLine), rowsBetween(0, lineCount))

    def estimatedLineAtRow(row: Int): (Int, Int) =
      rowBelow(0, row.max(0)).getOrElse(if lineCount <= 0 then (0, 0) else (lineCount - 1, measure(lineCount - 1) - 1))

    @tailrec
    private def below(line: Int, consumedRows: Int, targetRow: Int): Option[(Int, Int)] =
      if line >= lineCount then None
      else
        val lineRows = measure(line)
        if consumedRows + lineRows > targetRow then Some((line, targetRow - consumedRows))
        else below(line + 1, consumedRows + lineRows, targetRow)

  /** Reads and refines `store`'s index for `key`, which every answer leaves measured over the lines it read. */
  private[layout] def indexed[K](
    store: VisualLineIndexStore[K],
    key: K,
    content: com.serenity.rope.Rope,
    measure: Int => Int,
    stamp: AnyRef = VisualLineIndexStore.Unstamped,
    revision: Option[ContentRevision] = None
  ): VisualRowCounts = new Indexed(store, key, content, measure, stamp, revision)

  final private class Indexed[K](
      store: VisualLineIndexStore[K],
      key: K,
      content: com.serenity.rope.Rope,
      measure: Int => Int,
      stamp: AnyRef,
      revision: Option[ContentRevision]
  ) extends VisualRowCounts:

    val lineCount: Int = content.lineCount

    private def withIndex[A](answer: VisualLineIndex => (A, VisualLineIndex)): A =
      val (result, refined) = answer(store.indexFor(key, content, stamp, revision))
      store.update(key, content, refined, stamp, revision)
      result

    private def inDocument(line: Int): Boolean = line >= 0 && line < lineCount

    def rowsIn(line: Int): Int =
      if !inDocument(line) then measure(line)
      else
        withIndex { index =>
          index.measuredRows(line) match
            case Some(rows) => (rows, index)
            case None =>
              val rows = measure(line)
              (rows, index.measured(line, rows))
        }

    def rowsBetween(from: Int, until: Int): Int =
      val low  = math.min(from, until)
      val high = math.max(from, until)
      val outsideRows =
        (low until math.min(high, 0)).map(measure).sum + (math.max(low, lineCount) until high).map(measure).sum
      val first = math.max(0, low)
      val last  = math.min(lineCount, high)
      val inside =
        if first >= last then 0
        else
          withIndex { index =>
            val filled = measuredThrough(index, first, last)
            (filled.rowsBetween(first, last), filled)
          }
      if from <= until then inside + outsideRows else -(inside + outsideRows)

    def rowAbove(line: Int, rows: Int): (Int, Int) =
      if line <= 0 then (0, 0)
      else if rows <= 0 then (line - 1, rowsIn(line - 1) - rows)
      else if line > lineCount then walking(lineCount, measure).rowAbove(line, rows)
      else
        withIndex { index =>
          val filled = measuredAbove(index, line, rows)
          val target = filled.rowOfLine(line) - rows
          (if target < 0 then (0, 0) else filled.lineAtRow(target).getOrElse((0, 0)), filled)
        }

    def rowBelow(line: Int, rows: Int): Option[(Int, Int)] =
      if !inDocument(line) then walking(lineCount, measure).rowBelow(line, rows)
      else if rows < 0 then Some((line, rows))
      else
        withIndex { index =>
          val filled = measuredBelow(index, line, rows)
          (filled.lineAtRow(filled.rowOfLine(line) + rows), filled)
        }

    def estimatedRowsBetween(from: Int, until: Int): Int =
      store.indexFor(key, content, stamp, revision).rowsBetween(from, until)

    def rowsAboveAndTotal(topLine: Int, visibleUntil: Int, exact: Boolean): (Int, Int) =
      val top = math.max(0, math.min(topLine, lineCount))
      withIndex { index =>
        val seen   = measuredThrough(index, top, math.max(top, math.min(visibleUntil, lineCount)))
        val filled = if exact then measuredThrough(seen, 0, lineCount) else seen
        ((filled.rowOfLine(top), filled.rowOfLine(lineCount)), filled)
      }

    def estimatedLineAtRow(row: Int): (Int, Int) =
      val index = store.indexFor(key, content, stamp, revision)
      val total = index.rowOfLine(lineCount)
      if total <= 0 then (0, 0) else index.lineAtRow(row.max(0).min(total - 1)).getOrElse((0, 0))

    @tailrec
    private def measuredThrough(index: VisualLineIndex, from: Int, until: Int): VisualLineIndex =
      index.firstUnmeasuredFrom(from).filter(_ < until) match
        case None       => index
        case Some(line) => measuredThrough(index.measured(line, measure(line)), line + 1, until)

    /** Measures upward from `line` until the measured lines directly above it hold `rows` rows, or the top is reached.
      */
    @tailrec
    private def measuredAbove(index: VisualLineIndex, line: Int, rows: Int): VisualLineIndex =
      index.lastUnmeasuredBefore(line) match
        case Some(unmeasured) if index.rowsBetween(unmeasured + 1, line) < rows =>
          measuredAbove(index.measured(unmeasured, measure(unmeasured)), line, rows)
        case _ => index

    /** Measures downward from `line` until the measured lines from it hold more than `rows` rows, or the end is
      * reached.
      */
    @tailrec
    private def measuredBelow(index: VisualLineIndex, line: Int, rows: Int): VisualLineIndex =
      index.firstUnmeasuredFrom(line) match
        case Some(unmeasured) if index.rowsBetween(line, unmeasured) <= rows =>
          measuredBelow(index.measured(unmeasured, measure(unmeasured)), line, rows)
        case _ => index

/** What a document's text was at `version`, and how to reach it from earlier ones: lets a cached per-line measurement
  * follow an edit from the change that made it instead of comparing the old and new text.
  */
final case class ContentRevision(version: Long, changesSince: Long => Option[ChangeSet])
