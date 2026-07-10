package com.serenity.rope

import scala.annotation.tailrec

trait Rope(using balance: Balance):
  def weight: Int
  def height: Int
  def newlineCount: Int
  def lastLineLength: Int
  def endsWithNewline: Boolean

  def isWeightBalanced: Boolean
  def isHeightBalanced: Boolean

  def concat(that: Rope): Rope = Node(this, that).rebalance

  def rebuild: Rope = Rope(this.collect())

  def rebalance: Rope

  def index(i: Int): Option[Char]

  def splitAt(index: Int): Option[(Rope, Rope)]

  def insert(index: Int, str: String): Rope =
    splitAt(index) match
      case Some(pre, post) =>
        pre.concat(Leaf(str)).concat(post).rebalance
      case None => this

  def delete(startIndex: Int, endIndex: Int): Rope =
    deleteRight(startIndex, endIndex - startIndex)

  def deleteLeft(start: Int, count: Int): Rope =
    if count == 0 then this
    else if count < 0 then deleteRight(start, Math.abs(count))
    else if start <= 0 then this
    else
      val actualCount = Math.min(count, start)
      deleteRight(start - actualCount, actualCount)

  def deleteRight(start: Int, count: Int): Rope =
    if count == 0 then this
    else if count < 0 then deleteLeft(start, Math.abs(count))
    else if start + count > weight then deleteRight(start, weight - start)
    else
      (for
        startAndRest <- splitAt(Math.max(0, start))
        (start, rest) = startAndRest
        restAndEnd <- rest.splitAt(count)
        (_, end) = restAndEnd
      yield Node(start, end).rebalance).getOrElse(this)

  def dropLeft(n: Int): Rope = deleteRight(0, n)

  def dropRight(n: Int): Rope = deleteLeft(weight, n)

  def replace(index: Int, char: Char): Rope =
    if index < 0 || index >= weight then this
    else
      splitAt(index) match
        case Some((l, r)) =>
          l.concat(Leaf(char.toString).concat(r.dropLeft(1)))
        case None => this

  def replaceAll(term: String, replacement: String): Rope =
    searchAll(term).sorted.reverse.foldLeft(this)((rope, index) =>
      rope.delete(index, index + term.length).insert(index, replacement)
    )

  def collect(): String =
    val sb = new StringBuilder
    @tailrec
    def go(stack: List[Rope]): Unit = stack match
      case Nil => ()
      case head :: rest =>
        head match
          case Node(l, r) => go(l :: r :: rest)
          case Leaf(v)    => sb.append(v); go(rest)
    go(List(this))
    sb.toString

  override def toString: String = collect()

  def slice(startIndex: Int, endIndex: Int): Rope =
    dropRight(weight - endIndex).dropLeft(startIndex)

  def sliceString(startIndex: Int, endIndex: Int): String =
    val start = math.max(0, math.min(startIndex, weight))
    val end   = math.max(start, math.min(endIndex, weight))
    if start == end then ""
    else
      val value = new StringBuilder(end - start)
      appendRange(this, start, end, 0, value)
      value.toString

  def searchAll(term: String): List[Int] =
    if term.isEmpty || term.length > weight then List.empty
    else
      @tailrec
      def findAll(start: Int, acc: List[Int]): List[Int] =
        if start > weight - term.length then acc.reverse
        else if matchesAt(start, term) then findAll(start + term.length, start :: acc)
        else findAll(start + 1, acc)

      findAll(0, List.empty)

  def lineCount: Int =
    newlineCount + 1

  def getLine(lineIndex: Int): Option[String] =
    if lineIndex < 0 || lineIndex > newlineCount then None
    else
      val value = new StringBuilder
      appendLine(this, lineIndex, value)
      Some(value.toString)

  def linesFrom(lineIndex: Int, maxLines: Int): Vector[String] =
    linesIteratorFrom(lineIndex).take(maxLines).map(_._2).toVector

  def linesIteratorFrom(lineIndex: Int): Iterator[(Int, String)] =
    if lineIndex < 0 || lineIndex > newlineCount then Iterator.empty
    else
      Iterator.unfold((lineColumnToOffset(lineIndex, 0), lineIndex, false)) {
        case (_, _, true)                            => None
        case (_, line, false) if line > newlineCount => None
        case (offset, line, false) =>
          val next = lineAt(offset)
          Some((line -> next.value) -> (next.nextOffset, line + 1, next.isFinalLine))
      }

  /** Resolve a logical line and internal UTF-16 column to the corresponding rope offset. Columns are not grapheme
    * counts; they share the same UTF-16 code-unit contract as Java `String` indexes and `CursorPosition.column`.
    */
  def lineColumnToOffset(line: Int, column: Int): Int =
    val targetLine = math.max(0, line)
    val targetCol  = math.max(0, column)

    if targetLine > newlineCount then weight
    else lineColumnToOffsetIn(this, targetLine, targetCol, 0)

  /** Resolve an internal rope offset to a logical line and UTF-16 column pair. The returned column may point inside a
    * multi-code-unit grapheme; user-facing edits should snap through grapheme helpers before mutating text.
    */
  def offsetToLineColumn(offset: Int): (Int, Int) =
    val clamped = math.max(0, math.min(offset, weight))
    offsetToLineColumnIn(this, clamped, 0, 0)

  def search(term: String): Option[Int] =
    if term.isEmpty || term.length > weight then None
    else
      @tailrec
      def loop(offset: Int): Option[Int] =
        if offset > weight - term.length then None
        else if matchesAt(offset, term) then Some(offset)
        else loop(offset + 1)

      loop(0)

  private def matchesAt(offset: Int, term: String): Boolean =
    @tailrec
    def loop(index: Int): Boolean =
      if index >= term.length then true
      else
        this.index(offset + index) match
          case Some(char) if char == term.charAt(index) => loop(index + 1)
          case _                                        => false

    loop(0)

  private def appendRange(
    rope: Rope,
    startIndex: Int,
    endIndex: Int,
    baseOffset: Int,
    value: StringBuilder
  ): Unit =
    rope match
      case Leaf(leafValue) =>
        val leafStart = math.max(0, startIndex - baseOffset)
        val leafEnd   = math.min(leafValue.length, endIndex - baseOffset)
        if leafStart < leafEnd then value.append(leafValue.substring(leafStart, leafEnd))
      case node: Node =>
        val leftEnd = baseOffset + node.left.weight
        if startIndex < leftEnd then appendRange(node.left, startIndex, endIndex, baseOffset, value)
        if endIndex > leftEnd then appendRange(node.right, startIndex, endIndex, leftEnd, value)
      case other =>
        appendIndexedRange(other, startIndex - baseOffset, endIndex - baseOffset, value)

  private def appendIndexedRange(
    rope: Rope,
    startIndex: Int,
    endIndex: Int,
    value: StringBuilder
  ): Unit =
    @tailrec
    def loop(offset: Int): Unit =
      if offset < endIndex then
        rope.index(offset) match
          case Some(char) =>
            value.append(char)
            loop(offset + 1)
          case None => ()

    loop(math.max(0, startIndex))

  private def appendLine(rope: Rope, lineIndex: Int, value: StringBuilder): Unit =
    rope match
      case Leaf(leafValue) =>
        appendLeafLine(leafValue, lineIndex, value)
      case node: Node =>
        if lineIndex < node.left.newlineCount then appendLine(node.left, lineIndex, value)
        else if lineIndex > node.left.newlineCount then
          appendLine(node.right, lineIndex - node.left.newlineCount, value)
        else
          if node.left.weight > 0 && !node.left.endsWithNewline then appendLine(node.left, lineIndex, value)
          appendLine(node.right, 0, value)
      case other =>
        appendIndexedLine(other, lineIndex, value)

  private def appendLeafLine(leafValue: String, lineIndex: Int, value: StringBuilder): Unit =
    @tailrec
    def loop(offset: Int, currentLine: Int): Unit =
      if offset < leafValue.length then
        val char = leafValue.charAt(offset)
        if currentLine == lineIndex then
          if char != '\n' then
            value.append(char)
            loop(offset + 1, currentLine)
        else if char == '\n' then loop(offset + 1, currentLine + 1)
        else loop(offset + 1, currentLine)

    loop(0, 0)

  private def appendIndexedLine(rope: Rope, lineIndex: Int, value: StringBuilder): Unit =
    @tailrec
    def loop(offset: Int, currentLine: Int): Unit =
      if offset < rope.weight then
        rope.index(offset) match
          case Some('\n') if currentLine == lineIndex => ()
          case Some('\n')                             => loop(offset + 1, currentLine + 1)
          case Some(char) if currentLine == lineIndex =>
            value.append(char)
            loop(offset + 1, currentLine)
          case Some(_) => loop(offset + 1, currentLine)
          case None    => ()

    loop(0, 0)

  private case class LineRead(value: String, nextOffset: Int, isFinalLine: Boolean)

  private def lineAt(offset: Int): LineRead =
    val value = new StringBuilder

    @tailrec
    def loop(index: Int): LineRead =
      if index >= weight then LineRead(value.toString, index, isFinalLine = true)
      else
        this.index(index) match
          case Some('\n') => LineRead(value.toString, index + 1, isFinalLine = false)
          case Some(char) =>
            value.append(char)
            loop(index + 1)
          case None => LineRead(value.toString, index, isFinalLine = true)

    loop(math.max(0, offset))

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
      case other =>
        indexedLineColumnToOffset(other, line, column, baseOffset)

  private def leafLineColumnToOffset(value: String, line: Int, column: Int, baseOffset: Int): Int =
    @tailrec
    def loop(offset: Int, currentLine: Int, currentColumn: Int): Int =
      if offset >= value.length then baseOffset + value.length
      else
        val char = value.charAt(offset)
        if currentLine == line && currentColumn >= column then baseOffset + offset
        else if currentLine == line && char == '\n' then baseOffset + offset
        else if char == '\n' then loop(offset + 1, currentLine + 1, 0)
        else if currentLine == line then loop(offset + 1, currentLine, currentColumn + 1)
        else loop(offset + 1, currentLine, currentColumn)

    loop(0, 0, 0)

  private def indexedLineColumnToOffset(rope: Rope, line: Int, column: Int, baseOffset: Int): Int =
    @tailrec
    def loop(offset: Int, currentLine: Int, currentColumn: Int): Int =
      if offset >= rope.weight then baseOffset + rope.weight
      else
        rope.index(offset) match
          case None => baseOffset + rope.weight
          case Some(char) =>
            if currentLine == line && currentColumn >= column then baseOffset + offset
            else if currentLine == line && char == '\n' then baseOffset + offset
            else if char == '\n' then loop(offset + 1, currentLine + 1, 0)
            else if currentLine == line then loop(offset + 1, currentLine, currentColumn + 1)
            else loop(offset + 1, currentLine, currentColumn)

    loop(0, 0, 0)

  private def offsetToLineColumnIn(
    rope: Rope,
    offset: Int,
    baseLine: Int,
    baseColumn: Int
  ): (Int, Int) =
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
        case other =>
          indexedOffsetToLineColumn(other, offset, baseLine, baseColumn)

  private def advancePosition(rope: Rope, baseLine: Int, baseColumn: Int): (Int, Int) =
    if rope.weight == 0 then (baseLine, baseColumn)
    else if rope.newlineCount == 0 then (baseLine, baseColumn + rope.weight)
    else (baseLine + rope.newlineCount, rope.lastLineLength)

  private def leafOffsetToLineColumn(
    value: String,
    offset: Int,
    baseLine: Int,
    baseColumn: Int
  ): (Int, Int) =
    @tailrec
    def loop(index: Int, line: Int, column: Int): (Int, Int) =
      if index >= offset || index >= value.length then (line, column)
      else if value.charAt(index) == '\n' then loop(index + 1, line + 1, 0)
      else loop(index + 1, line, column + 1)

    loop(0, baseLine, baseColumn)

  private def indexedOffsetToLineColumn(
    rope: Rope,
    offset: Int,
    baseLine: Int,
    baseColumn: Int
  ): (Int, Int) =
    @tailrec
    def loop(index: Int, line: Int, column: Int): (Int, Int) =
      if index >= offset || index >= rope.weight then (line, column)
      else
        rope.index(index) match
          case Some('\n') => loop(index + 1, line + 1, 0)
          case Some(_)    => loop(index + 1, line, column + 1)
          case None       => (line, column)

    loop(0, baseLine, baseColumn)

object Rope:

  def empty(using balance: Balance): Rope = Leaf("")

  // Normalizes CRLF and bare CR to LF on entry so all downstream code
  // (WrapEngine, RenderEngine, cursor arithmetic) only ever sees '\n'.
  def apply(in: String)(using balance: Balance): Rope =
    build(in.replace("\r\n", "\n").replace("\r", "\n"))

  private def build(in: String)(using balance: Balance): Rope =
    if in.length <= balance.leafChunkSize then Leaf(in)
    else
      val (left, right) = in.splitAt(Math.floorDiv(in.length, 2))
      Node(build(left), build(right)).rebalance
