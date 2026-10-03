package com.serenity.ui.layout

import scala.annotation.tailrec

/** A persistent prefix-sum index of how many visual rows each logical line wraps into, so a line's visual row, the line
  * at a visual row, and the rows between two lines are O(log n) lookups rather than a line-by-line walk.
  *
  * A line counts as one row until it is measured; [[VisualRowCounts]] measures every line an answer depends on before
  * reading it, so those estimates only ever stand in for lines outside the span being placed. Runs of unmeasured lines
  * are held as a single piece, so indexing a document starts in constant space and grows only with what is measured.
  */
final class VisualLineIndex private (private val root: VisualLineIndex.Tree):
  import VisualLineIndex.*

  def lineCount: Int = root.lines

  /** Rows above `line`, with every unmeasured line counted as one. */
  def rowOfLine(line: Int): Int = rowsBefore(root, math.max(0, math.min(line, lineCount)))

  def rowsBetween(from: Int, until: Int): Int = rowOfLine(until) - rowOfLine(from)

  /** The line holding visual row `row` and the row's offset within it; `None` outside the document. */
  def lineAtRow(row: Int): Option[(Int, Int)] =
    Option.when(row >= 0 && row < root.rows)(lineAt(root, row, 0))

  def measuredRows(line: Int): Option[Int] =
    pieceAt(root, line).filter(_.measured).map(_.rows)

  def measured(line: Int, rows: Int): VisualLineIndex =
    if line < 0 || line >= lineCount then this
    else
      val (before, rest) = split(root, line)
      val (_, after)     = split(rest, 1)
      new VisualLineIndex(join(before, Piece.line(math.max(1, rows)), after))

  /** The last unmeasured line above `line`. */
  def lastUnmeasuredBefore(line: Int): Option[Int] = lastUnmeasured(root, line, 0)

  /** The first unmeasured line at or below `line`. */
  def firstUnmeasuredFrom(line: Int): Option[Int] = firstUnmeasured(root, math.max(0, line), 0)

  /** Replaces the `removed` lines starting at `from` with `inserted` unmeasured ones; the lines after them keep their
    * measurements and shift.
    */
  def replacedLines(from: Int, removed: Int, inserted: Int): VisualLineIndex =
    val start          = math.max(0, math.min(from, lineCount))
    val (before, rest) = split(root, start)
    val (_, after)     = split(rest, math.max(0, removed))
    val middle         = if inserted > 0 then node(Leaf, Piece.unmeasured(inserted), Leaf) else Leaf
    new VisualLineIndex(concat(concat(before, middle), after))

  private[layout] def isBalanced: Boolean = balanced(root)

object VisualLineIndex:

  def unmeasured(lineCount: Int): VisualLineIndex =
    new VisualLineIndex(if lineCount > 0 then node(Leaf, Piece.unmeasured(lineCount), Leaf) else Leaf)

  /** `lines` consecutive lines spanning `rows` rows: one measured line, or a run of unmeasured ones at a row each. */
  final private case class Piece(lines: Int, rows: Int, measured: Boolean)

  private object Piece:
    def line(rows: Int): Piece        = Piece(1, rows, measured = true)
    def unmeasured(lines: Int): Piece = Piece(lines, lines, measured = false)

  sealed private trait Tree:
    def height: Int
    def lines: Int
    def rows: Int
    def unmeasuredLines: Int

  private case object Leaf extends Tree:
    val height          = 0
    val lines           = 0
    val rows            = 0
    val unmeasuredLines = 0

  final private case class Node(left: Tree, piece: Piece, right: Tree) extends Tree:
    val height: Int = 1 + math.max(left.height, right.height)
    val lines: Int  = left.lines + piece.lines + right.lines
    val rows: Int   = left.rows + piece.rows + right.rows
    val unmeasuredLines: Int =
      left.unmeasuredLines + (if piece.measured then 0 else piece.lines) + right.unmeasuredLines

  private def node(left: Tree, piece: Piece, right: Tree): Tree = Node(left, piece, right)

  private def balanced(tree: Tree): Boolean =
    tree match
      case Leaf => true
      case Node(left, _, right) =>
        math.abs(left.height - right.height) <= 1 && balanced(left) && balanced(right)

  @tailrec
  private def rowsBefore(tree: Tree, line: Int, acc: Int = 0): Int =
    tree match
      case Leaf => acc
      case Node(left, piece, right) =>
        if line <= left.lines then rowsBefore(left, line, acc)
        else if line < left.lines + piece.lines then acc + left.rows + (if piece.measured then 0 else line - left.lines)
        else rowsBefore(right, line - left.lines - piece.lines, acc + left.rows + piece.rows)

  @tailrec
  private def lineAt(tree: Tree, row: Int, firstLine: Int): (Int, Int) =
    tree match
      case Leaf => (firstLine, row)
      case Node(left, piece, right) =>
        if row < left.rows then lineAt(left, row, firstLine)
        else
          val withinPiece = row - left.rows
          val pieceLine   = firstLine + left.lines
          if withinPiece < piece.rows then
            if piece.measured then (pieceLine, withinPiece) else (pieceLine + withinPiece, 0)
          else lineAt(right, withinPiece - piece.rows, pieceLine + piece.lines)

  @tailrec
  private def pieceAt(tree: Tree, line: Int): Option[Piece] =
    tree match
      case Leaf => None
      case Node(left, piece, right) =>
        if line < 0 then None
        else if line < left.lines then pieceAt(left, line)
        else if line < left.lines + piece.lines then Some(piece)
        else pieceAt(right, line - left.lines - piece.lines)

  private def lastUnmeasured(tree: Tree, until: Int, firstLine: Int): Option[Int] =
    tree match
      case Leaf => None
      case Node(left, piece, right) =>
        if tree.unmeasuredLines == 0 || until <= firstLine then None
        else
          val pieceStart = firstLine + left.lines
          val rightStart = pieceStart + piece.lines
          lastUnmeasured(right, until, rightStart)
            .orElse(Option.when(!piece.measured && until > pieceStart)(math.min(until, rightStart) - 1))
            .orElse(lastUnmeasured(left, until, firstLine))

  private def firstUnmeasured(tree: Tree, from: Int, firstLine: Int): Option[Int] =
    tree match
      case Leaf => None
      case Node(left, piece, right) =>
        val rightStart = firstLine + tree.lines - right.lines
        if tree.unmeasuredLines == 0 || from >= firstLine + tree.lines then None
        else
          val pieceStart = firstLine + left.lines
          firstUnmeasured(left, from, firstLine)
            .orElse(Option.when(!piece.measured && from < rightStart)(math.max(from, pieceStart)))
            .orElse(firstUnmeasured(right, from, rightStart))

  /** The first `line` lines, and the rest. */
  private def split(tree: Tree, line: Int): (Tree, Tree) =
    tree match
      case Leaf => (Leaf, Leaf)
      case Node(left, piece, right) =>
        val pieceStart = left.lines
        val pieceEnd   = pieceStart + piece.lines
        if line <= pieceStart then
          val (leftBefore, leftAfter) = split(left, line)
          (leftBefore, join(leftAfter, piece, right))
        else if line >= pieceEnd then
          val (rightBefore, rightAfter) = split(right, line - pieceEnd)
          (join(left, piece, rightBefore), rightAfter)
        else
          val cut = line - pieceStart
          (join(left, Piece.unmeasured(cut), Leaf), join(Leaf, Piece.unmeasured(piece.lines - cut), right))

  private def concat(left: Tree, right: Tree): Tree =
    right match
      case Leaf => left
      case _ =>
        val (first, rest) = removeFirst(right)
        join(left, first, rest)

  private def removeFirst(tree: Tree): (Piece, Tree) =
    tree match
      case Node(Leaf, piece, right) => (piece, right)
      case Node(left, piece, right) =>
        val (first, rest) = removeFirst(left)
        (first, join(rest, piece, right))
      case Leaf => (Piece.unmeasured(0), Leaf)

  /** AVL join: every line of `left`, then `piece`, then every line of `right`, rebalanced in O(|height difference|). */
  private def join(left: Tree, piece: Piece, right: Tree): Tree =
    if piece.lines <= 0 then concat(left, right)
    else if left.height > right.height + 1 then joinRight(left, piece, right)
    else if right.height > left.height + 1 then joinLeft(left, piece, right)
    else node(left, piece, right)

  private def joinRight(left: Tree, piece: Piece, right: Tree): Tree =
    left match
      case Node(leftLeft, leftPiece, leftRight) =>
        if leftRight.height <= right.height + 1 then
          val joined = node(leftRight, piece, right)
          if joined.height <= leftLeft.height + 1 then node(leftLeft, leftPiece, joined)
          else rotateLeft(node(leftLeft, leftPiece, rotateRight(joined)))
        else
          val joined = joinRight(leftRight, piece, right)
          val parent = node(leftLeft, leftPiece, joined)
          if joined.height <= leftLeft.height + 1 then parent else rotateLeft(parent)
      case Leaf => node(Leaf, piece, right)

  private def joinLeft(left: Tree, piece: Piece, right: Tree): Tree =
    right match
      case Node(rightLeft, rightPiece, rightRight) =>
        if rightLeft.height <= left.height + 1 then
          val joined = node(left, piece, rightLeft)
          if joined.height <= rightRight.height + 1 then node(joined, rightPiece, rightRight)
          else rotateRight(node(rotateLeft(joined), rightPiece, rightRight))
        else
          val joined = joinLeft(left, piece, rightLeft)
          val parent = node(joined, rightPiece, rightRight)
          if joined.height <= rightRight.height + 1 then parent else rotateRight(parent)
      case Leaf => node(left, piece, Leaf)

  private def rotateLeft(tree: Tree): Tree =
    tree match
      case Node(left, piece, Node(rightLeft, rightPiece, rightRight)) =>
        node(node(left, piece, rightLeft), rightPiece, rightRight)
      case other => other

  private def rotateRight(tree: Tree): Tree =
    tree match
      case Node(Node(leftLeft, leftPiece, leftRight), piece, right) =>
        node(leftLeft, leftPiece, node(leftRight, piece, right))
      case other => other
