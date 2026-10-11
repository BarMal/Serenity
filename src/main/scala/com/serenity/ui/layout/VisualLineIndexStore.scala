package com.serenity.ui.layout

import java.util.LinkedHashMap

import scala.annotation.tailrec

import com.serenity.markdown.MarkdownBlockLens.FenceRangeIndex
import com.serenity.markdown.{LazyBlockIndex, MarkdownInlineView}
import com.serenity.rope.{ChangeSet, Leaf, Node, Rope}
import com.serenity.state.models.IndentationSettings
import com.serenity.ui.layout.VisualLineIndexStore.StampRelation

/** The [[VisualLineIndex]] last built for each key -- a buffer under one wrap setting -- together with the content it
  * describes. A key's index follows edits to its buffer: only the lines an edit touched lose their measurements, and
  * the lines after them shift. Anything else a row count depends on (wrap width, font and zoom, rendering context, cell
  * metrics) is part of the key, so changing it starts a fresh index. Derived data only: it lives beside the
  * [[WrappedLineCache]] that owns it, never in persisted state.
  *
  * The state dispatcher and a render may run on different threads, so every map access holds this instance's monitor;
  * measuring happens outside it, and the last refinement stored wins.
  */
final class VisualLineIndexStore[K](maxEntries: Int):

  final private case class Entry(content: Rope, version: Option[Long], stamp: AnyRef, index: VisualLineIndex)

  private val entries = new LinkedHashMap[K, Entry](16, 0.75f, true)

  def size: Int = synchronized(entries.size())

  def counts(
    key: K,
    content: Rope,
    measure: Int => Int,
    stamp: AnyRef = VisualLineIndexStore.Unstamped,
    revision: Option[ContentRevision] = None
  ): VisualRowCounts =
    VisualRowCounts.indexed(this, key, content, measure, stamp, revision)

  private[layout] def indexFor(
    key: K,
    content: Rope,
    stamp: AnyRef = VisualLineIndexStore.Unstamped,
    revision: Option[ContentRevision] = None
  ): VisualLineIndex =
    synchronized(Option(entries.get(key))) match
      case None => VisualLineIndex.unmeasured(content.lineCount)
      case Some(entry) =>
        VisualLineIndexStore.stampsRelate(entry.stamp, stamp) match
          case StampRelation.Unrelated => VisualLineIndex.unmeasured(content.lineCount)
          case StampRelation.Same      => followed(entry, content, revision)
          case StampRelation.AgreesBefore(line) =>
            VisualLineIndexStore.unmeasuredFrom(followed(entry, content, revision), line)

  private def followed(entry: Entry, content: Rope, revision: Option[ContentRevision]): VisualLineIndex =
    if isSameContent(entry.content, content) then entry.index
    else
      val recorded = for
        current <- revision
        since   <- entry.version
        if since != current.version
        change <- current.changesSince(since)
        if change.oldLength == entry.content.weight && change.newLength == content.weight
      yield change
      recorded.fold(VisualLineIndexStore.followEdit(entry.index, entry.content, content))(
        VisualLineIndexStore.followChange(entry.index, entry.content, content, _)
      )

  private[layout] def update(
    key: K,
    content: Rope,
    index: VisualLineIndex,
    stamp: AnyRef,
    revision: Option[ContentRevision] = None
  ): Unit = synchronized {
    val _ = entries.put(key, Entry(content, revision.map(_.version), stamp, index))
    evictEldest(entries.values().iterator())
  }

  private def isSameContent(a: Rope, b: Rope): Boolean = (a: AnyRef) eq (b: AnyRef)

  @tailrec
  private def evictEldest(eldestFirst: java.util.Iterator[Entry]): Unit =
    if entries.size() > maxEntries && eldestFirst.hasNext then
      val _ = eldestFirst.next()
      eldestFirst.remove()
      evictEldest(eldestFirst)

object VisualLineIndexStore:

  /** The stamp of content whose row counts depend on nothing but its text. */
  val Unstamped: AnyRef = new Object

  /** Rich-text styling is stamped by the identity of its document; a restyled Markdown view by what decides how it
    * restyles (mode, revealed lines, font kind) and by its block index, which is compared by how far two indexes are
    * known to agree rather than by value (see [[stampsRelate]]). A tab size other than the default stamps too: a tab
    * wraps by its width, so a new size leaves no row count standing.
    */
  final private[layout] case class ViewStamp(
      base: AnyRef,
      markdown: MarkdownInlineView,
      tabSize: Int = IndentationSettings.DefaultTabSize
  )

  /** How a stamp the index was built under relates to the one asked for. */
  enum StampRelation:
    /** Row counts depend on the same things: every measurement stands. */
    case Same

    /** Only lines from `line` on may count differently: the block structure above it is the same. */
    case AgreesBefore(line: Int)

    case Unrelated

  private[layout] def stampsRelate(built: AnyRef, asked: AnyRef): StampRelation =
    (built, asked) match
      case _ if built eq asked => StampRelation.Same
      case (x: ViewStamp, y: ViewStamp)
          if x.base == y.base && x.tabSize == y.tabSize && sameRestyling(x.markdown, y.markdown) =>
        (x.markdown.fences, y.markdown.fences) match
          case (a, b) if a eq b => StampRelation.Same
          case (a: LazyBlockIndex, b: LazyBlockIndex) if b.follows(a) =>
            StampRelation.AgreesBefore(b.agreesBefore)
          case (a: FenceRangeIndex, b: FenceRangeIndex) if a == b => StampRelation.Same
          case _                                                  => StampRelation.Unrelated
      case _ => StampRelation.Unrelated

  private def sameRestyling(a: MarkdownInlineView, b: MarkdownInlineView): Boolean =
    a.mode == b.mode && a.revealed == b.revealed && a.baseIsMonospaced == b.baseIsMonospaced

  private[layout] def unmeasuredFrom(index: VisualLineIndex, line: Int): VisualLineIndex =
    val remaining = index.lineCount - line
    if remaining <= 0 then index else index.replacedLines(line, remaining, remaining)

  private[layout] def stampOf(richText: RichTextContext): AnyRef =
    val base = richText.document.getOrElse(Unstamped)
    if richText.markdown.isActive || richText.tabSize != IndentationSettings.DefaultTabSize then
      ViewStamp(base, richText.markdown, richText.tabSize)
    else base

  val DefaultMaxEntries = 32

  /** `index`, which describes `before`, carried over to `after` by the `change` that made it: the lines the change's
    * parts touched become unmeasured and every other line keeps its measurement, without reading either text.
    */
  def followChange(index: VisualLineIndex, before: Rope, after: Rope, change: ChangeSet): VisualLineIndex =
    if index.lineCount != before.lineCount then VisualLineIndex.unmeasured(after.lineCount)
    else
      val followed = change.lineSpans(before).foldLeft(index) { (current, span) =>
        current.replacedLines(span.newFirst, span.oldLast - span.oldFirst + 1, span.newLast - span.newFirst + 1)
      }
      if followed.lineCount == after.lineCount then followed else VisualLineIndex.unmeasured(after.lineCount)

  /** `index`, which describes `before`, carried over to `after`: the lines an edit may have changed become unmeasured,
    * and every line outside them -- identical text, so an identical row count -- keeps its measurement.
    */
  def followEdit(index: VisualLineIndex, before: Rope, after: Rope): VisualLineIndex =
    if index.lineCount != before.lineCount then VisualLineIndex.unmeasured(after.lineCount)
    else
      val bound  = math.min(before.weight, after.weight)
      val prefix = sharedRun(before, after, bound, fromEnd = false)
      if prefix.length == bound && before.weight == after.weight then index
      else
        val suffix     = sharedRun(before, after, bound - prefix.length, fromEnd = true)
        val firstLine  = prefix.newlines
        val lastBefore = before.newlineCount - suffix.newlines
        val lastAfter  = after.newlineCount - suffix.newlines
        val followed   = index.replacedLines(firstLine, lastBefore - firstLine + 1, lastAfter - firstLine + 1)
        if followed.lineCount == after.lineCount then followed else VisualLineIndex.unmeasured(after.lineCount)

  final private case class Run(length: Int, newlines: Int):
    def plus(more: Run): Run = Run(length + more.length, newlines + more.newlines)

  /** The text `a` and `b` share from their start (or, `fromEnd`, their end), at most `limit` characters, found by
    * walking both trees in step. An edit rebuilds only the nodes on its path and the leaves it touches, so the walk
    * skips every subtree and leaf string the two ropes hold in common by reference, and costs the tree's depth plus the
    * changed region rather than the document. Nodes that never line up are expanded and compared leaf by leaf, then
    * character by character: slower, never wrong. The run's newline count is what lets the caller find the lines either
    * side of the change without descending the ropes again.
    */
  private def sharedRun(a: Rope, b: Rope, limit: Int, fromEnd: Boolean): Run =
    def expand(node: Node, rest: List[Rope]): List[Rope] =
      if fromEnd then node.right :: node.left :: rest else node.left :: node.right :: rest

    def matching(x: String, xUsed: Int, y: String, yUsed: Int, count: Int): Run =
      def at(value: String, used: Int, offset: Int): Char =
        value.charAt(if fromEnd then value.length - 1 - used - offset else used + offset)
      @tailrec
      def scan(offset: Int, newlines: Int): Run =
        if offset >= count then Run(count, newlines)
        else
          val char = at(x, xUsed, offset)
          if char != at(y, yUsed, offset) then Run(offset, newlines)
          else scan(offset + 1, if char == '\n' then newlines + 1 else newlines)
      scan(0, 0)

    def same(x: AnyRef, y: AnyRef): Boolean = x eq y

    @tailrec
    def walk(left: List[Rope], leftUsed: Int, right: List[Rope], rightUsed: Int, run: Run): Run =
      if run.length >= limit then run
      else
        (left, right) match
          case (x :: xs, y :: ys) =>
            if same(x, y) && leftUsed == 0 && rightUsed == 0 && run.length + x.weight <= limit then
              walk(xs, 0, ys, 0, run.plus(Run(x.weight, x.newlineCount)))
            else
              (x, y) match
                case (Leaf(s), _) if leftUsed >= s.length  => walk(xs, 0, right, rightUsed, run)
                case (_, Leaf(t)) if rightUsed >= t.length => walk(left, leftUsed, ys, 0, run)
                case (Leaf(s), Leaf(t)) =>
                  val count = math.min(math.min(s.length - leftUsed, t.length - rightUsed), limit - run.length)
                  val shared =
                    if same(s, t) && leftUsed == 0 && rightUsed == 0 && count == s.length then
                      Run(count, x.newlineCount)
                    else matching(s, leftUsed, t, rightUsed, count)
                  if shared.length < count then run.plus(shared)
                  else walk(left, leftUsed + count, right, rightUsed + count, run.plus(shared))
                case (nodeX: Node, nodeY: Node) =>
                  if nodeX.weight > nodeY.weight then walk(expand(nodeX, xs), leftUsed, right, rightUsed, run)
                  else if nodeY.weight > nodeX.weight then walk(left, leftUsed, expand(nodeY, ys), rightUsed, run)
                  else walk(expand(nodeX, xs), leftUsed, expand(nodeY, ys), rightUsed, run)
                case (nodeX: Node, _) => walk(expand(nodeX, xs), leftUsed, right, rightUsed, run)
                case (_, nodeY: Node) => walk(left, leftUsed, expand(nodeY, ys), rightUsed, run)
          case _ => run

    walk(List(a), 0, List(b), 0, Run(0, 0))
