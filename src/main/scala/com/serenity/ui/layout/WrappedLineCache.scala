package com.serenity.ui.layout

import java.awt.font.FontRenderContext
import java.util.concurrent.atomic.AtomicLong
import java.util.{ArrayDeque, HashMap, LinkedHashMap}

import scala.jdk.CollectionConverters.*

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

  /** `cold(limit)` wraps the line into at most `limit` rows from scratch. `incremental` may instead resume from a
    * recently wrapped edit of the same line, and answers `None` whenever it cannot reproduce `cold` exactly.
    */
  private[layout] def wrapped(key: WrappedLineKey, bufferLine: Int, maxVisualLines: Int)(
    cold: Int => RowWrap,
    incremental: Seq[Predecessor] => Option[RowWrap]
  ): Vector[TextVisualLine]

  private[layout] def visualRowCounts(
    key: VisualRowKey,
    content: Rope,
    measure: Int => Int,
    stamp: AnyRef
  ): VisualRowCounts

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
):
  def shape: WrapShape =
    WrapShape(panelWidthPx, resolver, frc, measuredLayout, cellMetrics, baseColumn, paragraphRole, dropCapGlyphWidthPx)

/** [[WrappedLineKey]] without its text: lines that share a shape wrap alike wherever their text agrees. */
final private[layout] case class WrapShape(
    panelWidthPx: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    baseColumn: Int,
    paragraphRole: ParagraphRole,
    dropCapGlyphWidthPx: Float
)

/** An earlier complete wrap of a line with the same [[WrapShape]], and what [[IncrementalWrap]] resumes from. */
final private[layout] case class Predecessor(
    key: WrappedLineKey,
    bufferLine: Int,
    rows: Vector[TextVisualLine],
    trace: WrapTrace
)

/** What the cache has had to compute: wraps from scratch, wraps resumed from a predecessor, and the visual rows and
  * measured characters they cost.
  */
final case class WrapStats(coldWraps: Long, incrementalWraps: Long, rowsComputed: Long, charsMeasured: Long)

object WrappedLineCache:

  val DefaultMaxLines = 4096

  /** How many recent wraps per [[WrapShape]] a line may resume from. */
  val MaxRecentPerShape = 8

  /** Caps retained text as well as line count: one entry holds a caret stop per character, so a few thousand long
    * paragraphs would otherwise pin tens of megabytes.
    */
  val DefaultMaxChars: Long = 1_000_000L

  /** Measures on every call; the default for callers that own no cache. */
  object Uncached extends WrappedLineCache:

    private[layout] def wrapped(key: WrappedLineKey, bufferLine: Int, maxVisualLines: Int)(
      cold: Int => RowWrap,
      incremental: Seq[Predecessor] => Option[RowWrap]
    ): Vector[TextVisualLine] = cold(maxVisualLines).rows

    private[layout] def visualRowCounts(
      key: VisualRowKey,
      content: Rope,
      measure: Int => Int,
      stamp: AnyRef
    ): VisualRowCounts =
      VisualRowCounts.walking(content.lineCount, measure)

  def bounded(maxLines: Int = DefaultMaxLines, maxChars: Long = DefaultMaxChars): Bounded =
    new Bounded(math.max(1, maxLines), math.max(1L, maxChars))

  /** `complete` is false when the wrap stopped at a row limit, so the entry only answers requests for that many rows.
    */
  final private case class Entry(
      rows: Vector[TextVisualLine],
      complete: Boolean,
      bufferLine: Int,
      trace: Option[WrapTrace]
  ):
    val chars: Long = rows.foldLeft(0L)((sum, row) => sum + row.text.length)

    def covers(maxVisualLines: Int): Boolean = complete || rows.length >= maxVisualLines

    def rowsFor(requestedBufferLine: Int, maxVisualLines: Int): Vector[TextVisualLine] =
      val limited = rows.take(maxVisualLines)
      if requestedBufferLine == bufferLine then limited else limited.map(_.copy(bufferLine = requestedBufferLine))

  /** An access-ordered LRU. Rendering and the state dispatcher may run on different threads, so every map access holds
    * this instance's monitor; wrapping a missed line happens outside it.
    */
  final class Bounded private[WrappedLineCache] (maxLines: Int, maxChars: Long) extends WrappedLineCache:
    private val entries          = new LinkedHashMap[WrappedLineKey, Entry](256, 0.75f, true)
    private val recent           = new HashMap[WrapShape, ArrayDeque[(WrappedLineKey, Entry)]]
    private val retainedChar     = new AtomicLong(0L)
    private val coldCount        = new AtomicLong(0L)
    private val incrementalCount = new AtomicLong(0L)
    private val rowCount         = new AtomicLong(0L)
    private val measuredCount    = new AtomicLong(0L)
    private val visualRows       = new VisualLineIndexStore[VisualRowKey](VisualLineIndexStore.DefaultMaxEntries)

    def size: Int = synchronized(entries.size())

    def retainedChars: Long = retainedChar.get()

    def visualRowIndexes: Int = visualRows.size

    def wrapStats: WrapStats =
      WrapStats(coldCount.get, incrementalCount.get, rowCount.get, measuredCount.get)

    private[layout] def visualRowCounts(
      key: VisualRowKey,
      content: Rope,
      measure: Int => Int,
      stamp: AnyRef
    ): VisualRowCounts =
      visualRows.counts(key, content, measure, stamp)

    private[layout] def wrapped(key: WrappedLineKey, bufferLine: Int, maxVisualLines: Int)(
      cold: Int => RowWrap,
      incremental: Seq[Predecessor] => Option[RowWrap]
    ): Vector[TextVisualLine] =
      val entry = lookup(key).filter(_.covers(maxVisualLines)).getOrElse {
        val resumed = Option.when(maxVisualLines == Int.MaxValue)(predecessors(key)).flatMap(incremental(_))
        val wrap    = resumed.getOrElse(cold(maxVisualLines))
        (if resumed.isDefined then incrementalCount else coldCount).incrementAndGet()
        rowCount.addAndGet(wrap.computedRows.toLong)
        measuredCount.addAndGet(wrap.measuredChars.toLong)
        val fresh = Entry(wrap.rows, complete = wrap.rows.length < maxVisualLines, bufferLine, wrap.trace)
        store(key, fresh)
        fresh
      }
      entry.rowsFor(bufferLine, maxVisualLines)

    private def predecessors(key: WrappedLineKey): Seq[Predecessor] = synchronized {
      Option(recent.get(key.shape)).fold(Seq.empty[Predecessor]) { candidates =>
        candidates.asScala.iterator.flatMap { (candidate, entry) =>
          entry.trace.map(Predecessor(candidate, entry.bufferLine, entry.rows, _))
        }.toVector
      }
    }

    private def lookup(key: WrappedLineKey): Option[Entry] = synchronized(Option(entries.get(key)))

    private def store(key: WrappedLineKey, entry: Entry): Unit = synchronized {
      Option(entries.put(key, entry)).foreach(replaced => retainedChar.addAndGet(-replaced.chars))
      retainedChar.addAndGet(entry.chars)
      if entry.complete && entry.trace.isDefined then remember(key, entry)
      evictEldest(entries.entrySet().iterator())
    }

    /** Newest first, at most [[MaxRecentPerShape]] per shape, and only entries the line cache still holds. */
    private def remember(key: WrappedLineKey, entry: Entry): Unit =
      val candidates = Option(recent.get(key.shape)).getOrElse {
        val fresh = new ArrayDeque[(WrappedLineKey, Entry)]
        recent.put(key.shape, fresh)
        fresh
      }
      candidates.removeIf((candidate, _) => candidate == key)
      candidates.addFirst((key, entry))
      trimOldest(candidates)

    @annotation.tailrec
    private def trimOldest(candidates: ArrayDeque[(WrappedLineKey, Entry)]): Unit =
      if candidates.size() > MaxRecentPerShape then
        candidates.removeLast()
        trimOldest(candidates)

    private def forget(key: WrappedLineKey): Unit =
      Option(recent.get(key.shape)).foreach { candidates =>
        candidates.removeIf((candidate, _) => candidate == key)
        if candidates.isEmpty then
          val _ = recent.remove(key.shape)
      }

    @annotation.tailrec
    private def evictEldest(eldestFirst: java.util.Iterator[java.util.Map.Entry[WrappedLineKey, Entry]]): Unit =
      if (entries.size() > maxLines || retainedChar.get() > maxChars) && entries.size() > 1 && eldestFirst.hasNext
      then
        val eldest = eldestFirst.next()
        retainedChar.addAndGet(-eldest.getValue.chars)
        forget(eldest.getKey)
        eldestFirst.remove()
        evictEldest(eldestFirst)
