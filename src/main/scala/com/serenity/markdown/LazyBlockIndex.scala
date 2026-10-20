package com.serenity.markdown

import java.lang.ref.WeakReference
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

import com.serenity.markdown.MarkdownBlockLens.{BlockIndex, SetextHeading}
import com.serenity.rope.Rope

/** The block structure of `content`, found only as far down as lines are asked about. A line's fenced block, table or
  * setext heading depends on the lines above it and on the few below it that close what it is in, so answering for a
  * line reads to the end of whatever the scan has open there and no further: the top of a long document costs the lines
  * on screen, not the document.
  *
  * Every answer equals the one a scan of the whole text gives ([[MarkdownBlockLens.fenceRangeIndex]]), including that a
  * fence never closed is not a block.
  *
  * The scan is remembered every [[LazyBlockIndex.Stride]] lines, so [[after]] an edit starts from the last mark above
  * it and keeps all it knew of the text before. [[agreesBefore]] says how many lines of such an index are known to
  * match its predecessor's, which is what lets layout keep what it measured above the edit.
  */
final class LazyBlockIndex private (
    content: Rope,
    resumed: LazyBlockIndex.Progress,
    predecessor: Option[LazyBlockIndex],
    val agreesBefore: Int
) extends BlockIndex:

  // Weak, so that remembering which index this one came from does not keep every earlier text of the buffer alive.
  private val cameFrom = predecessor.map(new WeakReference(_))

  def follows(earlier: LazyBlockIndex): Boolean = cameFrom.exists(_.get eq earlier)

  private val progress = new AtomicReference(resumed)

  def rangeAt(line: Int): Option[Range.Inclusive] =
    MarkdownBlockLens.containing(settledTo(line).closedFences, line)(identity)

  def tableAt(line: Int): Option[Range.Inclusive] =
    MarkdownBlockLens.containing(settledTo(line).tables, line)(identity)

  def setextHeadingAt(line: Int): Option[SetextHeading] =
    MarkdownBlockLens.containing(settledTo(line).setext, line)(_.lines)

  def tablesIn(lines: Range.Inclusive): Vector[Range.Inclusive] =
    settledTo(lines.end).tables.filter(table => table.end >= lines.start && table.start <= lines.end)

  /** How many lines of the text have been read so far. */
  def linesRead: Int = progress.get.scan.lineCount

  /** An index of `edited`, which differs from this index's text from `firstChangedLine` on. */
  def after(edited: Rope, firstChangedLine: Int): LazyBlockIndex =
    val known = progress.get
    val mark  = known.marks.take(firstChangedLine / LazyBlockIndex.Stride + 1)
    val from  = mark.lastOption.getOrElse(BlockScan.empty)
    new LazyBlockIndex(
      edited,
      LazyBlockIndex.Progress(from, mark),
      Some(this),
      from.settledBefore
    )

  /** The scan, carried far enough that everything it records about `line` is final. */
  private def settledTo(line: Int): BlockScan =
    val current = progress.get
    if current.ended then current.scan.finished
    else if current.scan.settledBefore > line then current.scan
    else
      val further = extended(current, line)
      val _       = progress.compareAndSet(current, further)
      if further.ended then further.scan.finished else further.scan

  private def extended(from: LazyBlockIndex.Progress, line: Int): LazyBlockIndex.Progress =
    @tailrec
    def read(lines: Iterator[String], so: LazyBlockIndex.Progress): LazyBlockIndex.Progress =
      if so.scan.settledBefore > line then so
      else if lines.hasNext then read(lines, so.advanced(lines.next()))
      else so.copy(ended = true)
    read(content.linesIteratorFrom(from.scan.lineCount).map(_._2), from)

object LazyBlockIndex:

  /** How many lines apart the scan is remembered. */
  val Stride = 256

  /** The scan so far, `marks` (the scan as it stood after every [[Stride]] lines, first of all none) and whether the
    * text has run out.
    */
  final private[markdown] case class Progress(scan: BlockScan, marks: Vector[BlockScan], ended: Boolean = false):

    def advanced(line: String): Progress =
      val next = scan.next(line)
      Progress(next, if next.lineCount % Stride == 0 then marks :+ next else marks)

  def of(content: Rope): LazyBlockIndex =
    new LazyBlockIndex(content, Progress(BlockScan.empty, Vector(BlockScan.empty)), None, 0)
