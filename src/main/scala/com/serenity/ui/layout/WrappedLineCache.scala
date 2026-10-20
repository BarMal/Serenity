package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.atomic.AtomicLong
import java.util.{ArrayDeque, HashMap, LinkedHashMap}

import scala.jdk.CollectionConverters.*

import com.serenity.markdown.MarkdownBlockLens.BlockIndex
import com.serenity.markdown.{LazyBlockIndex, TableColumns}
import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.rope.{Balance, Rope, RopeDiff}
import com.serenity.state.models.{BufferId, MeasuredRow}
import com.serenity.text.fallback.{FallbackItemiser, FallbackRun}
import com.serenity.ui.fonts.{FallbackCoverages, FallbackItemisations, FontFallback, Java2DGlyphCoverage}
import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.openings.{OpeningLines, OpeningRules, OpeningSettings, VirtualDropCaps}
import com.serenity.ui.renderer.ShapedTextCache

/** Memoises [[TextLayoutSnapshot]]'s wrap-and-measure of one logical line -- the AWT `TextLayout` work that otherwise
  * dominates every keystroke on word-wrapped prose, and that the state-update path and the render path each repeat for
  * the same lines. One instance lives on [[com.serenity.state.manager.RenderCaches]] so both paths share it. Results
  * are identical with or without a cache.
  *
  * It also owns each buffer's [[VisualLineIndex]] of the row counts those wraps produce, which shares its keys' notion
  * of what a wrap depends on and so belongs with it.
  */
sealed abstract class WrappedLineCache:

  /** The cache a wrap measures its glyphs through. A wrap shapes the runs it measures so that painting the row finds
    * them shaped; the painter and this cache must therefore be handed the same instance, which is why it lives here,
    * with the layout state the renderer's owner already threads to every wrap.
    */
  private[serenity] def shapedText: ShapedTextCache

  /** The font-keyed measurements wraps and painting share, kept for as long as this cache is. */
  private[serenity] def layoutMemos: LayoutMemos

  /** `cold(limit)` wraps the line into at most `limit` rows from scratch. `incremental` may instead resume from a
    * recently wrapped edit of the same line, and answers `None` whenever it cannot reproduce `cold` exactly.
    */
  private[layout] def wrapped(key: WrappedLineKey, maxVisualLines: Int)(
    cold: Int => RowWrap,
    incremental: Seq[Predecessor] => Option[RowWrap]
  ): Vector[MeasuredRow]

  private[layout] def visualRowCounts(
    key: VisualRowKey,
    content: Rope,
    measure: Int => Int,
    stamp: AnyRef,
    revision: Option[ContentRevision] = None
  ): VisualRowCounts

  /** How many pixels the rows of each logical line stack to, in the same edit-following index as [[visualRowCounts]]:
    * what column paging fills a column by.
    */
  private[layout] def bandHeights(
    key: BandHeightKey,
    content: Rope,
    measure: Int => Int,
    stamp: AnyRef,
    revision: Option[ContentRevision] = None
  ): VisualRowCounts

  /** The fenced code blocks, tables and setext headings of `content`, which a live Markdown preview restyles or leaves
    * unstyled. It reads the document only as far as it is asked about, and a cache keeps it for as long as the buffer's
    * content is the same rope; for an edited rope it resumes from the line the edit reached, found from `revision`'s
    * recorded change when there is one.
    */
  def fenceIndex(bufferId: BufferId, content: Rope, revision: Option[ContentRevision] = None): BlockIndex

  /** The column geometry of `content`'s pipe tables at `font`, which `compute` measures; a cache keeps it for as long
    * as the content is the same rope and the font the same.
    */
  def tableColumns(bufferId: BufferId, content: Rope, font: Font, frc: FontRenderContext)(
    compute: => TableColumns
  ): TableColumns

  /** The openings of `document` that `settings` set as drop caps. Finding them reads the whole document, so a cache
    * keeps the openings of the documents it was last asked about: a layout asks about one document many times over
    * before an edit makes the next.
    */
  def virtualDropCaps(document: RichTextDocument, settings: OpeningSettings): Option[VirtualDropCaps]

  /** Whether `text` is printable ASCII and tabs, which every font a line is set in has glyphs for: such a line is never
    * itemised, and deciding that from the text alone keeps the common line off the coverage lookups entirely.
    */
  def isPlainAscii(text: String): Boolean = FallbackItemisations.isPlainAscii(text)

  /** The runs `text` is drawn in when `primary` and the chain `fallback` describes are asked in turn, `stickyNeutrals`
    * letting spaces and punctuation follow the cluster before them. A cache keeps the answer, and the coverage memo
    * behind it, for the next layout of the same line; without one every call itemises afresh.
    */
  def fallbackRuns(primary: Font, fallback: FontFallback, text: String, stickyNeutrals: Boolean): Vector[FallbackRun] =
    FallbackItemiser.itemise(text, 0, text.length, Java2DGlyphCoverage.of(primary, fallback), stickyNeutrals)

/** Everything a line's wrap and caret measurement depends on. The rows it yields are position-independent -- no buffer
  * line, alignment or inset -- so a line keeps its entry when lines are inserted or deleted above it. `resolver`
  * carries the base font and the per-column rich-text run fonts (style, size and prose zoom are all baked into those
  * fonts).
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
    dropCapIndentPx: Float
):
  // The text and fonts are hashed in full, and every map operation on a lookup rehashes the key.
  override lazy val hashCode: Int = scala.util.hashing.MurmurHash3.caseClassHash(this)

  def shape: WrapShape =
    WrapShape(panelWidthPx, resolver, frc, measuredLayout, cellMetrics, baseColumn, paragraphRole, dropCapIndentPx)

/** [[WrappedLineKey]] without its text: lines that share a shape wrap alike wherever their text agrees. */
final private[layout] case class WrapShape(
    panelWidthPx: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    baseColumn: Int,
    paragraphRole: ParagraphRole,
    dropCapIndentPx: Float
)

/** An earlier complete wrap of a line with the same [[WrapShape]], and what [[IncrementalWrap]] resumes from. */
final private[layout] case class Predecessor(
    key: WrappedLineKey,
    rows: Vector[MeasuredRow],
    trace: WrapTrace
)

/** A document by identity: a new version of it is a new key whatever it compares equal to. */
final private[layout] class IdentityKey(val document: RichTextDocument):

  override def equals(other: Any): Boolean =
    other match
      case that: IdentityKey => document eq that.document
      case _                 => false

  override def hashCode(): Int = System.identityHashCode(document)

final private[layout] case class ScannedOpenings(document: IdentityKey, rules: OpeningRules)

/** What the cache has had to compute: wraps from scratch, wraps resumed from a predecessor, and the visual rows and
  * measured characters they cost.
  */
final case class WrapStats(coldWraps: Long, incrementalWraps: Long, rowsComputed: Long, charsMeasured: Long)

object WrappedLineCache:

  val DefaultMaxLines = 4096

  /** How many buffers' fenced-block indexes are kept. */
  val MaxFenceIndexes = 16

  /** How many documents' openings are kept. */
  val MaxScannedDocuments = 8

  /** How many recent wraps per [[WrapShape]] a line may resume from. */
  val MaxRecentPerShape = 8

  /** Caps retained text as well as line count: one entry holds a caret stop and an advance per character, about 50
    * bytes in all. Entries are keyed by text, so every keystroke leaves the paragraph's previous version behind until
    * it is evicted; a million characters let those fill about 100MB while editing a 9KB file (#1798). This holds the
    * wraps of several hundred prose paragraphs, far more than the screen needs.
    */
  val DefaultMaxChars: Long = 262_144L

  /** A cache that remembers no wrap: every call measures afresh. */
  sealed abstract class Unremembering extends WrappedLineCache:

    private[layout] def wrapped(key: WrappedLineKey, maxVisualLines: Int)(
      cold: Int => RowWrap,
      incremental: Seq[Predecessor] => Option[RowWrap]
    ): Vector[MeasuredRow] = cold(maxVisualLines).rows

    private[layout] def visualRowCounts(
      key: VisualRowKey,
      content: Rope,
      measure: Int => Int,
      stamp: AnyRef,
      revision: Option[ContentRevision]
    ): VisualRowCounts =
      VisualRowCounts.walking(content.lineCount, measure)

    private[layout] def bandHeights(
      key: BandHeightKey,
      content: Rope,
      measure: Int => Int,
      stamp: AnyRef,
      revision: Option[ContentRevision]
    ): VisualRowCounts =
      VisualRowCounts.walking(content.lineCount, measure)

    def fenceIndex(bufferId: BufferId, content: Rope, revision: Option[ContentRevision]): BlockIndex =
      LazyBlockIndex.of(content)

    def tableColumns(bufferId: BufferId, content: Rope, font: Font, frc: FontRenderContext)(
      compute: => TableColumns
    ): TableColumns = compute

    def virtualDropCaps(document: RichTextDocument, settings: OpeningSettings): Option[VirtualDropCaps] =
      VirtualDropCaps.of(settings, document)

  /** Measures on every call; the default for callers that own no cache. Nothing is remembered, so each wrap shapes into
    * and probes caches of its own and keeps none of it.
    */
  object Uncached extends Unremembering:

    private[serenity] def shapedText: ShapedTextCache = ShapedTextCache()

    private[serenity] def layoutMemos: LayoutMemos = LayoutMemos()

  /** Measures every wrap afresh over a shaper and font memos that outlive it, as when no wrap is remembered but the
    * measurements underneath are.
    */
  final class Measuring private[WrappedLineCache] (
      private[serenity] val shapedText: ShapedTextCache,
      private[serenity] val layoutMemos: LayoutMemos
  ) extends Unremembering

  def measuring(shapedText: ShapedTextCache, layoutMemos: LayoutMemos): Measuring =
    new Measuring(shapedText, layoutMemos)

  def bounded(maxLines: Int = DefaultMaxLines, maxChars: Long = DefaultMaxChars): Bounded =
    new Bounded(math.max(1, maxLines), math.max(1L, maxChars), ShapedTextCache())

  /** A default-sized cache whose wraps measure through `shapedText`, which the painter of the same owner must share. */
  def sharing(shapedText: ShapedTextCache): Bounded = new Bounded(DefaultMaxLines, DefaultMaxChars, shapedText)

  final private case class TableColumnsEntry(
      content: Rope,
      font: Font,
      frc: FontRenderContext,
      columns: TableColumns
  )

  final private case class FenceEntry(content: Rope, version: Option[Long], index: LazyBlockIndex)

  /** The first line at which `content` differs from the text `previous` indexed: by the change `revision` recorded when
    * it reaches back that far, by comparing the two ropes otherwise.
    */
  private def firstChangedLine(previous: FenceEntry, content: Rope, revision: Option[ContentRevision]): Int =
    val recorded = for
      current <- revision
      since   <- previous.version
      change  <- current.changesSince(since)
      if change.oldLength == previous.content.weight && change.newLength == content.weight
      span <- change.lineSpans(previous.content).headOption
    yield span.newFirst
    recorded.getOrElse {
      given Balance = Balance.default
      RopeDiff
        .changedOffsetRange(previous.content, content)
        .fold(content.lineCount)((from, _) => content.offsetToLineColumn(from)._1)
    }

  /** `complete` is false when the wrap stopped at a row limit, so the entry only answers requests for that many rows.
    */
  final private case class Entry(rows: Vector[MeasuredRow], complete: Boolean, trace: Option[WrapTrace]):
    val chars: Long = rows.foldLeft(0L)((sum, row) => sum + row.text.length)

    def covers(maxVisualLines: Int): Boolean = complete || rows.length >= maxVisualLines

    def rowsFor(maxVisualLines: Int): Vector[MeasuredRow] = rows.take(maxVisualLines)

  /** An access-ordered LRU. Rendering and the state dispatcher may run on different threads, so every map access holds
    * this instance's monitor; wrapping a missed line happens outside it.
    */
  final class Bounded private[WrappedLineCache] (maxLines: Int, maxChars: Long, shaper: ShapedTextCache)
      extends WrappedLineCache:
    private[serenity] def shapedText: ShapedTextCache = shaper

    private[serenity] val layoutMemos: LayoutMemos = LayoutMemos()
    private val entries                            = new LinkedHashMap[WrappedLineKey, Entry](256, 0.75f, true)
    private val recent                             = new HashMap[WrapShape, ArrayDeque[(WrappedLineKey, Entry)]]
    private val retainedChar                       = new AtomicLong(0L)
    private val coldCount                          = new AtomicLong(0L)
    private val incrementalCount                   = new AtomicLong(0L)
    private val rowCount                           = new AtomicLong(0L)
    private val measuredCount                      = new AtomicLong(0L)
    private val visualRows = new VisualLineIndexStore[VisualRowKey](VisualLineIndexStore.DefaultMaxEntries)
    private val bandRows   = new VisualLineIndexStore[BandHeightKey](VisualLineIndexStore.DefaultMaxEntries)
    private val fences     = new LinkedHashMap[BufferId, FenceEntry](16, 0.75f, true)
    private val columns    = new LinkedHashMap[BufferId, TableColumnsEntry](16, 0.75f, true)
    private val openings   = new LinkedHashMap[ScannedOpenings, OpeningLines](16, 0.75f, true)
    private val coverages  = FallbackCoverages(FallbackCoverages.DefaultCapacity)

    private val itemisations = FallbackItemisations(coverages, FallbackItemisations.DefaultEntries)

    override def isPlainAscii(text: String): Boolean = itemisations.isPlainAscii(text)

    def plainAsciiScans: Long = itemisations.plainAsciiScans

    override def fallbackRuns(
      primary: Font,
      fallback: FontFallback,
      text: String,
      stickyNeutrals: Boolean
    ): Vector[FallbackRun] =
      itemisations.runs(primary, fallback, text, stickyNeutrals)

    /** How many times a layout has asked this cache for a fallback coverage. */
    def fallbackCoverageLookups: Long = coverages.lookups

    def tableColumns(bufferId: BufferId, content: Rope, font: Font, frc: FontRenderContext)(
      compute: => TableColumns
    ): TableColumns =
      cachedColumns(bufferId, content, font, frc).getOrElse {
        val found = compute
        synchronized {
          val _ = columns.put(bufferId, TableColumnsEntry(content, font, frc, found))
          if columns.size() > MaxFenceIndexes then
            val eldest = columns.keySet().iterator()
            val _      = eldest.next()
            eldest.remove()
        }
        found
      }

    private def cachedColumns(bufferId: BufferId, content: Rope, font: Font, frc: FontRenderContext) = synchronized {
      Option(columns.get(bufferId)).collect {
        case entry if sameRope(entry.content, content) && entry.font == font && entry.frc == frc => entry.columns
      }
    }

    def fenceIndex(bufferId: BufferId, content: Rope, revision: Option[ContentRevision]): BlockIndex =
      val known = synchronized(Option(fences.get(bufferId)))
      known match
        case Some(entry) if sameRope(entry.content, content) => entry.index
        case _ =>
          val index = known.fold(LazyBlockIndex.of(content))(entry =>
            entry.index.after(content, firstChangedLine(entry, content, revision))
          )
          synchronized {
            val _ = fences.put(bufferId, FenceEntry(content, revision.map(_.version), index))
            trimFences()
          }
          index

    def virtualDropCaps(document: RichTextDocument, settings: OpeningSettings): Option[VirtualDropCaps] =
      VirtualDropCaps.of(settings, document, openingsOf)

    private def openingsOf(document: RichTextDocument, rules: OpeningRules): OpeningLines =
      val key = ScannedOpenings(IdentityKey(document), rules)
      synchronized(Option(openings.get(key))).getOrElse {
        val scanned = VirtualDropCaps.scanning(document, rules)
        synchronized {
          val _ = openings.put(key, scanned)
          if openings.size() > WrappedLineCache.MaxScannedDocuments then
            val eldest = openings.keySet().iterator()
            val _      = eldest.next()
            eldest.remove()
        }
        scanned
      }

    private def sameRope(a: Rope, b: Rope): Boolean = (a: AnyRef) eq (b: AnyRef)

    @annotation.tailrec
    private def trimFences(): Unit =
      if fences.size() > MaxFenceIndexes then
        val eldest = fences.keySet().iterator()
        val _      = eldest.next()
        eldest.remove()
        trimFences()

    def size: Int = synchronized(entries.size())

    def retainedChars: Long = retainedChar.get()

    def visualRowIndexes: Int = visualRows.size

    def wrapStats: WrapStats =
      WrapStats(coldCount.get, incrementalCount.get, rowCount.get, measuredCount.get)

    private[layout] def visualRowCounts(
      key: VisualRowKey,
      content: Rope,
      measure: Int => Int,
      stamp: AnyRef,
      revision: Option[ContentRevision]
    ): VisualRowCounts =
      visualRows.counts(key, content, measure, stamp, revision)

    private[layout] def bandHeights(
      key: BandHeightKey,
      content: Rope,
      measure: Int => Int,
      stamp: AnyRef,
      revision: Option[ContentRevision]
    ): VisualRowCounts =
      bandRows.counts(key, content, measure, stamp, revision)

    private[layout] def wrapped(key: WrappedLineKey, maxVisualLines: Int)(
      cold: Int => RowWrap,
      incremental: Seq[Predecessor] => Option[RowWrap]
    ): Vector[MeasuredRow] =
      val held = lookup(key)
      val entry = held.filter(_.covers(maxVisualLines)).getOrElse {
        val resumed = Option.when(maxVisualLines == Int.MaxValue)(predecessors(key)).flatMap(incremental(_))
        val wrap    = resumed.getOrElse(cold(maxVisualLines))
        (if resumed.isDefined then incrementalCount else coldCount).incrementAndGet()
        rowCount.addAndGet(wrap.computedRows.toLong)
        measuredCount.addAndGet(wrap.measuredChars.toLong)
        val fresh = Entry(sharingRows(held, wrap.rows), complete = wrap.rows.length < maxVisualLines, wrap.trace)
        store(key, fresh)
        fresh
      }
      entry.rowsFor(maxVisualLines)

    /** A longer wrap replacing a row-limited one keeps the rows it has in common with it, by reference: frames built
      * before and after the replacement then compare their shared rows in constant time instead of float by float.
      */
    private def sharingRows(replaced: Option[Entry], wrapped: Vector[MeasuredRow]): Vector[MeasuredRow] =
      replaced.fold(wrapped)(old =>
        wrapped.zipWithIndex.map((row, index) => old.rows.lift(index).filter(_ == row).getOrElse(row))
      )

    private def predecessors(key: WrappedLineKey): Seq[Predecessor] = synchronized {
      Option(recent.get(key.shape)).fold(Seq.empty[Predecessor]) { candidates =>
        candidates.asScala.iterator.flatMap { (candidate, entry) =>
          entry.trace.map(Predecessor(candidate, entry.rows, _))
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
