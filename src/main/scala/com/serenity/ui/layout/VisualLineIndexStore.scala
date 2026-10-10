package com.serenity.ui.layout

import java.util.LinkedHashMap

import scala.annotation.tailrec

import com.serenity.markdown.MarkdownInlineView
import com.serenity.rope.{Leaf, Node, Rope}

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

  final private case class Entry(content: Rope, stamp: AnyRef, index: VisualLineIndex)

  private val entries = new LinkedHashMap[K, Entry](16, 0.75f, true)

  def size: Int = synchronized(entries.size())

  def counts(
    key: K,
    content: Rope,
    measure: Int => Int,
    stamp: AnyRef = VisualLineIndexStore.Unstamped
  ): VisualRowCounts =
    VisualRowCounts.indexed(this, key, content, measure, stamp)

  private[layout] def indexFor(key: K, content: Rope, stamp: AnyRef = VisualLineIndexStore.Unstamped): VisualLineIndex =
    synchronized(Option(entries.get(key))) match
      case Some(entry) if !sameStamp(entry.stamp, stamp)        => VisualLineIndex.unmeasured(content.lineCount)
      case Some(entry) if isSameContent(entry.content, content) => entry.index
      case Some(entry) => VisualLineIndexStore.followEdit(entry.index, entry.content, content)
      case None        => VisualLineIndex.unmeasured(content.lineCount)

  private[layout] def update(key: K, content: Rope, index: VisualLineIndex, stamp: AnyRef): Unit = synchronized {
    val _ = entries.put(key, Entry(content, stamp, index))
    evictEldest(entries.values().iterator())
  }

  private def isSameContent(a: Rope, b: Rope): Boolean = (a: AnyRef) eq (b: AnyRef)

  private def sameStamp(a: AnyRef, b: AnyRef): Boolean =
    (a eq b) || ((a, b) match
      case (x: VisualLineIndexStore.ViewStamp, y: VisualLineIndexStore.ViewStamp) => x == y
      case _                                                                      => false)

  @tailrec
  private def evictEldest(eldestFirst: java.util.Iterator[Entry]): Unit =
    if entries.size() > maxEntries && eldestFirst.hasNext then
      val _ = eldestFirst.next()
      eldestFirst.remove()
      evictEldest(eldestFirst)

object VisualLineIndexStore:

  /** The stamp of content whose row counts depend on nothing but its text. */
  val Unstamped: AnyRef = new Object

  /** Rich-text styling is stamped by the identity of its document; a restyled Markdown view by its value, since each
    * frame builds an equal one.
    */
  final private[layout] case class ViewStamp(base: AnyRef, markdown: MarkdownInlineView)

  private[layout] def stampOf(richText: RichTextContext): AnyRef =
    val base = richText.document.getOrElse(Unstamped)
    if richText.markdown.isActive then ViewStamp(base, richText.markdown) else base

  val DefaultMaxEntries = 32

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
