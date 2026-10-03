package com.serenity.ui.layout

import java.util.LinkedHashMap

import scala.annotation.tailrec

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

  final private case class Entry(content: Rope, index: VisualLineIndex)

  private val entries = new LinkedHashMap[K, Entry](16, 0.75f, true)

  def size: Int = synchronized(entries.size())

  def counts(key: K, content: Rope, measure: Int => Int): VisualRowCounts =
    VisualRowCounts.indexed(this, key, content, measure)

  private[layout] def indexFor(key: K, content: Rope): VisualLineIndex =
    synchronized(Option(entries.get(key))) match
      case Some(entry) if isSameContent(entry.content, content) => entry.index
      case Some(entry) => VisualLineIndexStore.followEdit(entry.index, entry.content, content)
      case None        => VisualLineIndex.unmeasured(content.lineCount)

  private[layout] def update(key: K, content: Rope, index: VisualLineIndex): Unit = synchronized {
    val _ = entries.put(key, Entry(content, index))
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

  val DefaultMaxEntries = 32

  /** `index`, which describes `before`, carried over to `after`: the lines an edit may have changed become unmeasured,
    * and every line outside them -- identical text, so an identical row count -- keeps its measurement.
    */
  def followEdit(index: VisualLineIndex, before: Rope, after: Rope): VisualLineIndex =
    if index.lineCount != before.lineCount then VisualLineIndex.unmeasured(after.lineCount)
    else
      val beforeLeaves = leafStrings(before)
      val afterLeaves  = leafStrings(after)
      val bound        = math.min(before.weight, after.weight)
      val prefix       = commonPrefix(beforeLeaves, afterLeaves, bound)
      if prefix == bound && before.weight == after.weight then index
      else
        val suffix          = commonPrefix(beforeLeaves.reverse, afterLeaves.reverse, bound - prefix, fromEnd = true)
        val (firstLine, _)  = after.offsetToLineColumn(prefix)
        val (lastBefore, _) = before.offsetToLineColumn(before.weight - suffix)
        val (lastAfter, _)  = after.offsetToLineColumn(after.weight - suffix)
        val followed        = index.replacedLines(firstLine, lastBefore - firstLine + 1, lastAfter - firstLine + 1)
        if followed.lineCount == after.lineCount then followed else VisualLineIndex.unmeasured(after.lineCount)

  /** An edit rebuilds only the leaves it touches, so every other leaf keeps the very same string: comparing those by
    * reference makes finding an edit proportional to the leaf count rather than the document's length, which a
    * `RopeDiff` walk splitting both trees is not.
    */
  private def leafStrings(rope: Rope): Vector[String] =
    @tailrec
    def collect(pending: List[Rope], acc: Vector[String]): Vector[String] =
      pending match
        case Node(left, right) :: rest            => collect(left :: right :: rest, acc)
        case Leaf(value) :: rest if value.isEmpty => collect(rest, acc)
        case Leaf(value) :: rest                  => collect(rest, acc :+ value)
        case Nil                                  => acc
    collect(List(rope), Vector.empty)

  /** How many characters `a` and `b` share from their start (or, `fromEnd`, from their end, with both leaf vectors
    * already reversed), up to `bound`.
    */
  private def commonPrefix(a: Vector[String], b: Vector[String], bound: Int, fromEnd: Boolean = false): Int =
    def charAt(leaf: String, offset: Int): Char = leaf.charAt(if fromEnd then leaf.length - 1 - offset else offset)
    @tailrec
    def sharedLeaves(index: Int, shared: Int): (Int, Int) =
      if index < a.length && index < b.length && (a(index) eq b(index)) && shared + a(index).length <= bound then
        sharedLeaves(index + 1, shared + a(index).length)
      else (index, shared)
    @tailrec
    def sharedChars(aLeaf: Int, aOffset: Int, bLeaf: Int, bOffset: Int, shared: Int): Int =
      if shared >= bound || aLeaf >= a.length || bLeaf >= b.length then shared
      else if aOffset >= a(aLeaf).length then sharedChars(aLeaf + 1, 0, bLeaf, bOffset, shared)
      else if bOffset >= b(bLeaf).length then sharedChars(aLeaf, aOffset, bLeaf + 1, 0, shared)
      else if charAt(a(aLeaf), aOffset) != charAt(b(bLeaf), bOffset) then shared
      else sharedChars(aLeaf, aOffset + 1, bLeaf, bOffset + 1, shared + 1)
    val (leaf, shared) = sharedLeaves(0, 0)
    sharedChars(leaf, 0, leaf, 0, shared)
