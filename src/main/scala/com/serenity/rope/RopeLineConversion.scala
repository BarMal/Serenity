package com.serenity.rope

import scala.annotation.tailrec

/** Offset to line/column conversion over the rope's per-node newline metrics: the descent picks the leaf, and the leaf
  * is then scanned with `indexOf` jumps from newline to newline rather than character by character.
  */
private[rope] object RopeLineConversion:

  def lineColumnToOffset(rope: Rope, line: Int, column: Int): Int = lineColumnToOffsetIn(rope, line, column, 0)

  def offsetToLineColumn(rope: Rope, offset: Int): (Int, Int) = offsetToLineColumnIn(rope, offset, 0, 0)

  /** `offsets` must be ascending and within the rope. */
  def offsetsToLineColumns(rope: Rope, offsets: IndexedSeq[Int]): Vector[(Int, Int)] =
    val lines   = new Array[Int](offsets.size)
    val columns = new Array[Int](offsets.size)
    fill(rope, offsets, 0, offsets.size, Cursor(0, 0, 0), lines, columns)
    Vector.tabulate(offsets.size)(index => (lines(index), columns(index)))

  /** Where a subtree begins: its absolute offset, line and column. */
  final private case class Cursor(offset: Int, line: Int, column: Int)

  private def lineColumnToOffsetIn(rope: Rope, line: Int, column: Int, baseOffset: Int): Int =
    rope match
      case Leaf(value) =>
        leafLineColumnToOffset(value, line, column, baseOffset)
      case node: Node =>
        if line < node.left.newlineCount then lineColumnToOffsetIn(node.left, line, column, baseOffset)
        else if line > node.left.newlineCount then
          lineColumnToOffsetIn(node.right, line - node.left.newlineCount, column, baseOffset + node.left.weight)
        else if node.left.endsWithNewline || node.left.weight == 0 then
          lineColumnToOffsetIn(node.right, 0, column, baseOffset + node.left.weight)
        else if column <= node.left.lastLineLength then lineColumnToOffsetIn(node.left, line, column, baseOffset)
        else lineColumnToOffsetIn(node.right, 0, column - node.left.lastLineLength, baseOffset + node.left.weight)

  private def leafLineColumnToOffset(value: String, line: Int, column: Int, baseOffset: Int): Int =
    @tailrec
    def lineStart(from: Int, remaining: Int): Int =
      if remaining == 0 then from
      else
        val newline = value.indexOf('\n', from)
        if newline < 0 then -1 else lineStart(newline + 1, remaining - 1)

    val start = lineStart(0, line)
    if start < 0 then baseOffset + value.length
    else
      val newline = value.indexOf('\n', start)
      val end     = if newline < 0 then value.length else newline
      baseOffset + start + math.min(column, end - start)

  private def offsetToLineColumnIn(rope: Rope, offset: Int, baseLine: Int, baseColumn: Int): (Int, Int) =
    if offset >= rope.weight then advancePosition(rope, baseLine, baseColumn)
    else
      rope match
        case Leaf(value) =>
          leafOffsetToLineColumn(value, offset, baseLine, baseColumn)
        case node: Node =>
          if offset <= node.left.weight then offsetToLineColumnIn(node.left, offset, baseLine, baseColumn)
          else
            val (rightBaseLine, rightBaseColumn) = advancePosition(node.left, baseLine, baseColumn)
            offsetToLineColumnIn(node.right, offset - node.left.weight, rightBaseLine, rightBaseColumn)

  private def advancePosition(rope: Rope, baseLine: Int, baseColumn: Int): (Int, Int) =
    if rope.weight == 0 then (baseLine, baseColumn)
    else if rope.newlineCount == 0 then (baseLine, baseColumn + rope.weight)
    else (baseLine + rope.newlineCount, rope.lastLineLength)

  private def leafOffsetToLineColumn(value: String, offset: Int, baseLine: Int, baseColumn: Int): (Int, Int) =
    val end = math.min(offset, value.length)

    @tailrec
    def loop(from: Int, line: Int): (Int, Int) =
      val newline = value.indexOf('\n', from)
      if newline >= 0 && newline < end then loop(newline + 1, line + 1)
      else (line, if line == baseLine then baseColumn + end else end - from)

    loop(0, baseLine)

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
        case Leaf(value) => fillLeaf(value, offsets, from, until, start, lines, columns)
        case node: Node =>
          val boundary       = start.offset + node.left.weight
          val split          = firstAbove(offsets, from, until, boundary)
          val (line, column) = advancePosition(node.left, start.line, start.column)
          fill(node.left, offsets, from, split, start, lines, columns)
          fill(node.right, offsets, split, until, Cursor(boundary, line, column), lines, columns)

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
