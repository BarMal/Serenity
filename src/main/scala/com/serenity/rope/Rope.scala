package com.serenity.rope

import scala.annotation.tailrec

trait Rope(using balance: Balance):
  def weight: Int
  def height: Int
  def newlineCount: Int

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
    if lineIndex < 0 then None
    else
      case class LineScan(currentLine: Int, value: StringBuilder)

      val scan = foldCharsUntil(LineScan(0, new StringBuilder)) { (state, char) =>
        if char == '\n' then
          if state.currentLine == lineIndex then Right(state)
          else Left(LineScan(state.currentLine + 1, state.value))
        else if state.currentLine == lineIndex then
          state.value.append(char)
          Left(state)
        else Left(state)
      }

      if scan.currentLine == lineIndex then Some(scan.value.toString) else None

  def lineColumnToOffset(line: Int, column: Int): Int =
    case class OffsetScan(currentLine: Int, currentColumn: Int, offset: Int)

    val targetLine = math.max(0, line)
    val targetCol  = math.max(0, column)

    val scan = foldCharsUntil(OffsetScan(0, 0, 0)) { (state, char) =>
      if state.currentLine == targetLine && state.currentColumn >= targetCol then Right(state)
      else if state.currentLine == targetLine && char == '\n' then Right(state)
      else if char == '\n' then Left(OffsetScan(state.currentLine + 1, 0, state.offset + 1))
      else Left(OffsetScan(state.currentLine, state.currentColumn + 1, state.offset + 1))
    }

    scan.offset

  def search(term: String): Option[Int] =
    if term.isEmpty then None
    else
      val content = this.collect()
      val index   = content.indexOf(term)
      if index == -1 then None else Some(index)

  @tailrec
  private def foldCharsUntil[A](
    acc: A
  )(step: (A, Char) => Either[A, A], stack: List[(Rope, Int)] = List((this, 0))): A =
    stack match
      case Nil => acc
      case (head, offset) :: rest =>
        head match
          case node: Node =>
            foldCharsUntil(acc)(step, (node.left, 0) :: (node.right, 0) :: rest)
          case leaf: Leaf =>
            if offset >= leaf.value.length then foldCharsUntil(acc)(step, rest)
            else
              step(acc, leaf.value.charAt(offset)) match
                case Right(done) => done
                case Left(next)  => foldCharsUntil(next)(step, (leaf, offset + 1) :: rest)
          case other =>
            if offset >= other.weight then foldCharsUntil(acc)(step, rest)
            else
              other.index(offset) match
                case None => foldCharsUntil(acc)(step, rest)
                case Some(char) =>
                  step(acc, char) match
                    case Right(done) => done
                    case Left(next)  => foldCharsUntil(next)(step, (other, offset + 1) :: rest)

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
