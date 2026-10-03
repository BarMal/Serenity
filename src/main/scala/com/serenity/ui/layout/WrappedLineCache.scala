package com.serenity.ui.layout

import java.awt.font.FontRenderContext
import java.util.LinkedHashMap
import java.util.concurrent.atomic.AtomicLong

import com.serenity.richtext.ParagraphRole
import com.serenity.rope.Rope
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver

/** Memoises [[TextLayoutSnapshot]]'s wrap-and-measure of one logical line -- the AWT `TextLayout` work that otherwise
  * dominates every keystroke on word-wrapped prose, and that the state-update path and the render path each repeat for
  * the same lines. One instance lives on [[com.serenity.state.manager.RenderCaches]] so both paths share it. Results
  * are identical with or without a cache.
  *
  * It also owns each buffer's [[VisualLineIndex]] of the row counts those wraps produce, which shares its keys' notion
  * of what a wrap depends on and so belongs with it.
  */
sealed abstract class WrappedLineCache:

  /** `wrap(limit)` wraps the line into at most `limit` rows. */
  private[layout] def wrapped(key: WrappedLineKey, bufferLine: Int, maxVisualLines: Int)(
    wrap: Int => Vector[TextVisualLine]
  ): Vector[TextVisualLine]

  private[layout] def visualRowCounts(key: VisualRowKey, content: Rope, measure: Int => Int): VisualRowCounts

/** Everything a line's wrap and caret measurement depends on except its buffer line number, which only labels the rows
  * -- so a line keeps its entry when lines are inserted or deleted above it. `resolver` carries the base font and the
  * per-column rich-text run fonts (style, size and prose zoom are all baked into those fonts).
  */
final private[layout] case class WrappedLineKey(
    text: String,
    panelWidthPx: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    baseColumn: Int,
    paragraphRole: ParagraphRole,
    dropCapGlyphWidthPx: Float
)

object WrappedLineCache:

  val DefaultMaxLines = 4096

  /** Caps retained text as well as line count: one entry holds a caret stop per character, so a few thousand long
    * paragraphs would otherwise pin tens of megabytes.
    */
  val DefaultMaxChars: Long = 1_000_000L

  /** Measures on every call; the default for callers that own no cache. */
  object Uncached extends WrappedLineCache:

    private[layout] def wrapped(key: WrappedLineKey, bufferLine: Int, maxVisualLines: Int)(
      wrap: Int => Vector[TextVisualLine]
    ): Vector[TextVisualLine] = wrap(maxVisualLines)

    private[layout] def visualRowCounts(key: VisualRowKey, content: Rope, measure: Int => Int): VisualRowCounts =
      VisualRowCounts.walking(content.lineCount, measure)

  def bounded(maxLines: Int = DefaultMaxLines, maxChars: Long = DefaultMaxChars): Bounded =
    new Bounded(math.max(1, maxLines), math.max(1L, maxChars))

  /** `complete` is false when the wrap stopped at a row limit, so the entry only answers requests for that many rows.
    */
  final private case class Entry(rows: Vector[TextVisualLine], complete: Boolean, bufferLine: Int):
    val chars: Long = rows.foldLeft(0L)((sum, row) => sum + row.text.length)

    def covers(maxVisualLines: Int): Boolean = complete || rows.length >= maxVisualLines

    def rowsFor(requestedBufferLine: Int, maxVisualLines: Int): Vector[TextVisualLine] =
      val limited = rows.take(maxVisualLines)
      if requestedBufferLine == bufferLine then limited else limited.map(_.copy(bufferLine = requestedBufferLine))

  /** An access-ordered LRU. Rendering and the state dispatcher may run on different threads, so every map access holds
    * this instance's monitor; wrapping a missed line happens outside it.
    */
  final class Bounded private[WrappedLineCache] (maxLines: Int, maxChars: Long) extends WrappedLineCache:
    private val entries      = new LinkedHashMap[WrappedLineKey, Entry](256, 0.75f, true)
    private val retainedChar = new AtomicLong(0L)
    private val visualRows   = new VisualLineIndexStore[VisualRowKey](VisualLineIndexStore.DefaultMaxEntries)

    def size: Int = synchronized(entries.size())

    def retainedChars: Long = retainedChar.get()

    def visualRowIndexes: Int = visualRows.size

    private[layout] def visualRowCounts(key: VisualRowKey, content: Rope, measure: Int => Int): VisualRowCounts =
      visualRows.counts(key, content, measure)

    private[layout] def wrapped(key: WrappedLineKey, bufferLine: Int, maxVisualLines: Int)(
      wrap: Int => Vector[TextVisualLine]
    ): Vector[TextVisualLine] =
      val entry = lookup(key).filter(_.covers(maxVisualLines)).getOrElse {
        val rows  = wrap(maxVisualLines)
        val fresh = Entry(rows, complete = rows.length < maxVisualLines, bufferLine)
        store(key, fresh)
        fresh
      }
      entry.rowsFor(bufferLine, maxVisualLines)

    private def lookup(key: WrappedLineKey): Option[Entry] = synchronized(Option(entries.get(key)))

    private def store(key: WrappedLineKey, entry: Entry): Unit = synchronized {
      Option(entries.put(key, entry)).foreach(replaced => retainedChar.addAndGet(-replaced.chars))
      retainedChar.addAndGet(entry.chars)
      evictEldest(entries.values().iterator())
    }

    @annotation.tailrec
    private def evictEldest(eldestFirst: java.util.Iterator[Entry]): Unit =
      if (entries.size() > maxLines || retainedChar.get() > maxChars) && entries.size() > 1 && eldestFirst.hasNext
      then
        retainedChar.addAndGet(-eldestFirst.next().chars)
        eldestFirst.remove()
        evictEldest(eldestFirst)
