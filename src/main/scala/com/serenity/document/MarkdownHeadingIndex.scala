package com.serenity.document

import scala.jdk.CollectionConverters.*

import com.serenity.rope.{Balance, ChangeSet, Rope, RopeDiff}
import com.serenity.state.models.HeadingIdentity
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}
import org.commonmark.Extension
import org.commonmark.ext.gfm.tables.TablesExtension
import org.commonmark.ext.task.list.items.TaskListItemsExtension
import org.commonmark.node.{Document, FencedCodeBlock, Heading, Node}
import org.commonmark.parser.{IncludeSourceSpans, Parser}
import org.commonmark.renderer.text.TextContentRenderer

/** The headings of one Markdown text, kept up to date from the edit rather than re-parsed (#1848).
  *
  * [[updatedTo]] finds what changed from the edit's [[ChangeSet]], or with [[RopeDiff]] when it has none, and re-parses
  * only the top-level blocks around it: a window that starts and ends where a blank line is followed by an unindented
  * line outside any fenced block, because nothing a block says about headings crosses such a boundary. Headings and
  * fenced blocks before the window are kept, those after it are renumbered by the edit's line delta. When the edit
  * touches a fenced block or a fence marker, or the text holds a construct whose meaning reaches across blank lines
  * (HTML comments and raw `<pre>`/`<script>` blocks, link reference definitions, which change how a heading's title
  * renders), the whole text is parsed instead, so the result is always what a full parse gives.
  *
  * The fenced blocks come from the same parse as the headings, so a `#` line inside code is never a heading.
  *
  * @param parsedLines
  *   how many lines the parse that produced this index covered: the whole text for [[MarkdownHeadingIndex.build]], the
  *   window for an incremental [[updatedTo]]
  * @param parsedFrom
  *   the first line that parse covered
  * @param parsedWholeText
  *   whether the parse covered the whole text, as [[MarkdownHeadingIndex.build]] does and [[updatedTo]] falls back to
  *   when an edit reaches across blocks: lines far from the edit may then differ too
  */
final class MarkdownHeadingIndex private (
    val content: Rope,
    private[document] val entries: Vector[MarkdownHeadingIndex.Entry],
    private[document] val fences: Vector[Range.Inclusive],
    private val spansBlankLines: Boolean,
    val parsedLines: Int,
    val parsedWholeText: Boolean = false,
    val parsedFrom: Int = 0
):

  lazy val headings: Vector[Symbol] = entries.map(_.symbol)

  /** The top-level heading written on `line`, including a setext heading's underline. */
  def topLevelHeadingAt(line: Int): Option[MarkdownHeadingIndex.HeadingLines] =
    val index = MarkdownHeadingIndex.lastStartingAtOrBefore(entries.length, line, entries(_).symbol.location.line)
    index
      .map(entries(_))
      .filter(entry => entry.topLevel && line <= entry.lastLine)
      .map(entry => MarkdownHeadingIndex.HeadingLines(entry.level, entry.symbol.location.line, entry.lastLine))

  def isInFence(line: Int): Boolean =
    MarkdownHeadingIndex
      .lastStartingAtOrBefore(fences.length, line, fences(_).start)
      .exists(index => fences(index).contains(line))

  /** The headings with the identity chapter notes are keyed by, each title normalised once when its block was parsed
    * rather than on every read.
    */
  lazy val identified: List[(HeadingIdentity, Symbol)] =
    HeadingIdentity.forTitled(entries.iterator.map(entry => entry.symbol -> entry.normalizedTitle).toList)

  /** This index advanced to `next`. `change`, when it is the edit that took [[content]] to `next`, names what changed;
    * without it the two ropes are compared.
    */
  def updatedTo(next: Rope, change: Option[ChangeSet] = None): MarkdownHeadingIndex =
    given Balance = Balance.default
    if next eq content then this
    else
      changedRange(next, change) match
        case None => new MarkdownHeadingIndex(next, entries, fences, spansBlankLines, 0)
        case Some((from, to)) =>
          Option
            .unless(spansBlankLines)(
              incrementally(next, next.offsetToLineColumn(from)._1, next.offsetToLineColumn(to)._1)
            )
            .flatten
            .getOrElse(MarkdownHeadingIndex.build(next))

  private def changedRange(next: Rope, change: Option[ChangeSet])(using Balance): Option[(Int, Int)] =
    change.filter(known => known.oldLength == content.weight && known.newLength == next.weight) match
      case Some(known) => known.newEnvelope
      case None        => RopeDiff.changedOffsetRange(content, next)

  private def incrementally(next: Rope, firstChanged: Int, lastChanged: Int): Option[MarkdownHeadingIndex] =
    val delta          = next.lineCount - content.lineCount
    val oldLastChanged = (lastChanged - delta).max(firstChanged)
    val touchesFence   = fences.exists(fence => fence.end >= firstChanged - 1 && fence.start <= oldLastChanged + 1)
    val unsafeNewText = next
      .linesIteratorFrom(firstChanged)
      .take(lastChanged - firstChanged + 1)
      .exists((_, line) => MarkdownHeadingIndex.opensFence(line) || MarkdownHeadingIndex.spansBlankLines(line))
    if touchesFence || unsafeNewText then None
    else
      val windowStart = windowStartFor(next, firstChanged)
      val windowEnd   = windowEndFor(next, lastChanged, delta)
      val window      = MarkdownHeadingIndex.parse(next, windowStart, windowEnd)
      val oldEnd      = windowEnd - delta
      Some(
        new MarkdownHeadingIndex(
          next,
          entries.takeWhile(_.symbol.location.line < windowStart) ++ window.headings ++
            entries.dropWhile(_.symbol.location.line < oldEnd).map(shifted(_, delta)),
          fences.takeWhile(_.end < windowStart) ++ window.fences ++
            fences.dropWhile(_.start < oldEnd).map(fence => (fence.start + delta) to (fence.end + delta)),
          spansBlankLines = false,
          parsedLines = windowEnd - windowStart,
          parsedFrom = windowStart
        )
      )

  /** The last line at or before `line` that begins a top-level block. */
  private def windowStartFor(next: Rope, line: Int): Int =
    Iterator
      .iterate(line)(_ - 1)
      .takeWhile(_ > 0)
      .find(candidate => beginsBlock(next, candidate, insideFence = lineInFence(_, fences)))
      .getOrElse(0)

  /** One past the last line of the window: the first block start whose separating blank line the edit did not touch, or
    * the end of the text. A blank line the edit wrote or kept at its edge is not enough: the block after it may have
    * been a continuation of one that began inside the edit before.
    */
  private def windowEndFor(next: Rope, lastChanged: Int, delta: Int): Int =
    (lastChanged + 2 until next.lineCount)
      .find(candidate => beginsBlock(next, candidate, insideFence = line => lineInFence(line - delta, fences)))
      .getOrElse(next.lineCount)

  private def beginsBlock(next: Rope, line: Int, insideFence: Int => Boolean): Boolean =
    val before = next.getLine(line - 1).getOrElse("")
    val here   = next.getLine(line).getOrElse("")
    MarkdownHeadingIndex.isBlank(before) && !MarkdownHeadingIndex.isBlank(here) && !MarkdownHeadingIndex.isIndented(
      here
    ) &&
    !insideFence(line - 1)

  private def lineInFence(line: Int, ranges: Vector[Range.Inclusive]): Boolean =
    ranges.exists(_.contains(line))

  private def shifted(entry: MarkdownHeadingIndex.Entry, delta: Int): MarkdownHeadingIndex.Entry =
    entry.copy(
      symbol = entry.symbol.copy(location = entry.symbol.location.copy(line = entry.symbol.location.line + delta)),
      lastLine = entry.lastLine + delta
    )

object MarkdownHeadingIndex:

  /** A heading with what readers would otherwise fetch from the text again for every one: the identity-ready title, and
    * the source line it was written on.
    */
  final private[document] case class Entry(
      symbol: Symbol,
      normalizedTitle: String,
      sourceLine: String,
      level: Int,
      lastLine: Int,
      topLevel: Boolean
  )

  /** The lines a heading occupies: one for an ATX heading, the title lines and underline for a setext one. */
  final case class HeadingLines(level: Int, firstLine: Int, lastLine: Int)

  /** The last index below `count` whose start, by `startOf`, is at or before `line`; the starts are ascending. */
  private[document] def lastStartingAtOrBefore(count: Int, line: Int, startOf: Int => Int): Option[Int] =
    @annotation.tailrec
    def search(low: Int, high: Int, found: Option[Int]): Option[Int] =
      if low > high then found
      else
        val middle = (low + high) >>> 1
        if startOf(middle) <= line then search(middle + 1, high, Some(middle)) else search(low, middle - 1, found)
    search(0, count - 1, None)

  final private case class Parsed(headings: Vector[Entry], fences: Vector[Range.Inclusive])

  /** Same extension set as [[com.serenity.markdown.MarkdownDocumentPreview]], so heading extraction agrees with what
    * the Markdown preview itself parses.
    */
  private val extensions: java.util.List[Extension] =
    List[Extension](TablesExtension.create(), TaskListItemsExtension.create()).asJava

  private val markdownParser: Parser =
    Parser.builder().extensions(extensions).includeSourceSpans(IncludeSourceSpans.BLOCKS).build()

  private val headingTextRenderer: TextContentRenderer =
    TextContentRenderer.builder().build()

  /** A full parse of `content`: the reference an incremental [[MarkdownHeadingIndex.updatedTo]] must agree with. */
  def build(content: Rope): MarkdownHeadingIndex =
    val parsed = parse(content, 0, content.lineCount)
    new MarkdownHeadingIndex(
      content,
      parsed.headings,
      parsed.fences,
      content.linesIteratorFrom(0).exists((_, line) => spansBlankLines(line)),
      content.lineCount,
      parsedWholeText = true
    )

  private def parse(content: Rope, from: Int, until: Int): Parsed =
    val lines    = content.linesIteratorFrom(from).take(until - from).map(_._2).toVector
    val document = markdownParser.parse(lines.mkString("\n"))
    val blocks   = descendants(document)
    Parsed(
      blocks.collect { case heading: Heading => heading }.flatMap(headingEntry(_, lines, from)).toVector,
      blocks.collect { case fence: FencedCodeBlock => fence }.flatMap(fenceLines(_, content, from, until)).toVector
    )

  private def isTopLevel(heading: Heading): Boolean =
    heading.getParent match
      case _: Document => true
      case _           => false

  private def headingEntry(heading: Heading, lines: Vector[String], firstLine: Int): Option[Entry] =
    val title = headingTextRenderer.render(heading).trim
    val spans = heading.getSourceSpans.asScala
    spans.headOption
      .filter(_ => title.nonEmpty)
      .map { span =>
        val symbol =
          Symbol(name = title, kind = SymbolKind.Heading, location = Location(firstLine + span.getLineIndex, 0))
        Entry(
          symbol,
          HeadingIdentity.normalizedTitle(title),
          lines.lift(span.getLineIndex).getOrElse(""),
          heading.getLevel,
          firstLine + spans.map(_.getLineIndex).foldLeft(0)(math.max),
          isTopLevel(heading)
        )
      }

  /** The lines of `fence`. A fence never closed runs to the end of the text, and the parse reports it ending at its
    * last content line, so when the parse reaches the end of the text its trailing blank lines are added: an edit there
    * is an edit to the fence.
    */
  private def fenceLines(fence: FencedCodeBlock, content: Rope, from: Int, until: Int): Option[Range.Inclusive] =
    val lines = fence.getSourceSpans.asScala.map(_.getLineIndex)
    lines.headOption.zip(lines.lastOption).map { (first, last) =>
      val start  = from + first
      val end    = from + last
      val closed = end > start && content.getLine(end).exists(opensFence)
      val onlyBlankAfter =
        until == content.lineCount && content.linesIteratorFrom(end + 1).forall((_, line) => isBlank(line))
      if closed || !onlyBlankAfter then start to end else start to (until - 1).max(end)
    }

  private def descendants(node: Node): List[Node] =
    children(node).flatMap(child => child :: descendants(child))

  private def children(node: Node): List[Node] =
    @annotation.tailrec
    def loop(current: Node, acc: List[Node]): List[Node] =
      if current == null then acc.reverse else loop(current.getNext, current :: acc)
    loop(node.getFirstChild, Nil)

  private[document] def isBlank(line: String): Boolean =
    line.forall(char => char == ' ' || char == '\t')

  private[document] def isIndented(line: String): Boolean =
    line.headOption.exists(char => char == ' ' || char == '\t')

  private def opensFence(line: String): Boolean =
    val text = line.stripLeading
    text.startsWith("```") || text.startsWith("~~~")

  /** Whether `line` can start or hold a construct whose meaning reaches past the next blank line. */
  private def spansBlankLines(line: String): Boolean =
    val text = line.stripLeading.toLowerCase(java.util.Locale.ROOT)
    text.startsWith("<!") || text.startsWith("<?") || text.startsWith("<pre") || text.startsWith("<script") ||
    text.startsWith("<style") || text.startsWith("<textarea") || (text.startsWith("[") && text.contains("]:"))

end MarkdownHeadingIndex
