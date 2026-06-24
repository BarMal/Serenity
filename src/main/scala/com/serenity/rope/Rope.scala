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

  def searchAll(term: String): List[Int] =
    if term.isEmpty then List.empty
    else
      val content = this.collect()
      @tailrec
      def findAll(start: Int, acc: List[Int]): List[Int] =
        val index = content.indexOf(term, start)
        if index == -1 then acc.reverse
        else findAll(index + term.length, index :: acc)
      findAll(0, List.empty)

  def lineCount: Int =
    newlineCount + 1

  def getLine(lineIndex: Int): Option[String] =
    if lineIndex < 0 || lineIndex > newlineCount then None
    else
      val value = new StringBuilder
      appendLine(this, lineIndex, value)
      Some(value.toString)

  def lineColumnToOffset(line: Int, column: Int): Int =
    val targetLine = math.max(0, line)
    val targetCol  = math.max(0, column)

    if targetLine > newlineCount then weight
    else lineColumnToOffsetIn(this, targetLine, targetCol, 0)

  def search(term: String): Option[Int] =
    if term.isEmpty then None
    else
      val content = this.collect()
      val index   = content.indexOf(term)
      if index == -1 then None else Some(index)

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
