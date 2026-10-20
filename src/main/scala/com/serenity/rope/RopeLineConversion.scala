package com.serenity.rope

import scala.annotation.tailrec

/** Offset to line/column conversion over the rope's per-branch newline counts. A line is found by selecting its
  * bounding newlines -- one descent each, choosing the child by the cumulative counts -- and an offset by counting the
  * newlines before it; inside a leaf, `indexOf` jumps from newline to newline rather than visiting each character.
  */
private[rope] object RopeLineConversion:

  /** `line` must be within `[0, rope.newlineCount]`; a `column` past the end of the line lands on its last position. */
  def lineColumnToOffset(rope: Rope, line: Int, column: Int): Int =
    val start = if line == 0 then 0 else offsetOfNewline(rope, line, 0) + 1
    val end   = if line < rope.newlineCount then offsetOfNewline(rope, line + 1, 0) else rope.weight
    start + math.min(column, end - start)

  /** `offset` must be within `[0, rope.weight]`. */
  def offsetToLineColumn(rope: Rope, offset: Int): (Int, Int) =
    val line  = newlinesBefore(rope, offset, 0)
    val start = if line == 0 then 0 else offsetOfNewline(rope, line, 0) + 1
    (line, offset - start)

  /** `offsets` must be ascending and within the rope. */
  def offsetsToLineColumns(rope: Rope, offsets: IndexedSeq[Int]): Vector[(Int, Int)] =
    val lines   = new Array[Int](offsets.size)
    val columns = new Array[Int](offsets.size)
    fill(rope, offsets, 0, offsets.size, Cursor(0, 0, 0), lines, columns)
    Vector.tabulate(offsets.size)(index => (lines(index), columns(index)))

  /** Where a subtree begins: its absolute offset, line and column. */
  final private case class Cursor(offset: Int, line: Int, column: Int)

  /** The absolute offset of the `nth` newline (counting from 1) at or below `rope`, which begins at `base`. */
  @tailrec
  private def offsetOfNewline(rope: Rope, nth: Int, base: Int): Int =
    rope match
      case Leaf(value) => base + indexOfNewline(value, nth, 0)
      case branch: Branch =>
        val child = branch.childHoldingNewline(nth)
        offsetOfNewline(branch.children(child), nth - branch.newlines(child), base + branch.starts(child))

  @tailrec
  private def indexOfNewline(value: String, nth: Int, from: Int): Int =
    val found = value.indexOf('\n', from)
    if nth <= 1 || found < 0 then found else indexOfNewline(value, nth - 1, found + 1)

  /** How many newlines lie before `offset`, counting those of the subtrees to the left on the way down. */
  @tailrec
  private def newlinesBefore(rope: Rope, offset: Int, counted: Int): Int =
    if offset >= rope.weight then counted + rope.newlineCount
    else
      rope match
        case Leaf(value) => counted + newlinesWithin(value, offset, 0, 0)
        case branch: Branch =>
          val child = branch.childContaining(offset)
          newlinesBefore(branch.children(child), offset - branch.starts(child), counted + branch.newlines(child))

  @tailrec
  private def newlinesWithin(value: String, end: Int, from: Int, counted: Int): Int =
    val newline = value.indexOf('\n', from)
    if newline >= 0 && newline < end then newlinesWithin(value, end, newline + 1, counted + 1) else counted

  private def advancePosition(rope: Rope, baseLine: Int, baseColumn: Int): (Int, Int) =
    if rope.weight == 0 then (baseLine, baseColumn)
    else if rope.newlineCount == 0 then (baseLine, baseColumn + rope.weight)
    else (baseLine + rope.newlineCount, rope.lastLineLength)

  private def fill(
    rope: Rope,
    offsets: IndexedSeq[Int],
    from: Int,
    until: Int,
    start: Cursor,
    lines: Array[Int],
    columns: Array[Int]
  ): Unit =
    if from < until then
      rope match
        case Leaf(value)    => fillLeaf(value, offsets, from, until, start, lines, columns)
        case branch: Branch => fillChildren(branch, offsets, from, until, start, lines, columns, 0)

  /** Hands each child the offsets that fall in it -- one on a boundary belongs to the child that ends there. */
  @tailrec
  private def fillChildren(
    branch: Branch,
    offsets: IndexedSeq[Int],
    from: Int,
    until: Int,
    start: Cursor,
    lines: Array[Int],
    columns: Array[Int],
    child: Int
  ): Unit =
    if from < until && child < branch.children.size then
      val node     = branch.children(child)
      val boundary = start.offset + node.weight
      val split    = if child == branch.children.size - 1 then until else firstAbove(offsets, from, until, boundary)
      fill(node, offsets, from, split, start, lines, columns)
      val (line, column) = advancePosition(node, start.line, start.column)
      fillChildren(branch, offsets, split, until, Cursor(boundary, line, column), lines, columns, child + 1)

  @tailrec
  private def firstAbove(offsets: IndexedSeq[Int], low: Int, high: Int, boundary: Int): Int =
    if low >= high then low
    else
      val middle = (low + high) >>> 1
      if offsets(middle) > boundary then firstAbove(offsets, low, middle, boundary)
      else firstAbove(offsets, middle + 1, high, boundary)

  private def fillLeaf(
    value: String,
    offsets: IndexedSeq[Int],
    from: Int,
    until: Int,
    start: Cursor,
    lines: Array[Int],
    columns: Array[Int]
  ): Unit =
    def nextNewline(position: Int): Int =
      val found = value.indexOf('\n', position)
      if found < 0 then Int.MaxValue else found

    @tailrec
    def loop(index: Int, line: Int, lineStart: Int, newline: Int): Unit =
      if index < until then
        val local = math.min(offsets(index) - start.offset, value.length)
        if newline < local then loop(index, line + 1, newline + 1, nextNewline(newline + 1))
        else
          lines(index) = line
          columns(index) = if line == start.line then start.column + local else local - lineStart
          loop(index + 1, line, lineStart, newline)

    loop(from, start.line, 0, nextNewline(0))
