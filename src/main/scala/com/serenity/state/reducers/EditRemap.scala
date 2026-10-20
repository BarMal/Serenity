package com.serenity.state.reducers

import scala.annotation.tailrec

import com.serenity.rope.Rope
import com.serenity.state.models.CursorPosition
import com.serenity.state.reducers.EditorEditSupport.MultiCursorEdit

/** Carries line/column positions and cursor offsets across one batch of edits without touching the rope per position.
  * The batch is described once (two conversions per edit), after which each position is mapped by a binary search and
  * line/column arithmetic, so a keystroke costs the same however many annotations the buffer holds.
  *
  * Only batches whose edits are separated by an unchanged character are described; edits that touch or overlap nest in
  * ways the legacy offset remap defines, so [[LineColumnEditMap.forEdits]] declines them and callers keep that path.
  */
private[reducers] object EditRemap:

  /** `edits` in ascending order. */
  def areSeparated(edits: List[MultiCursorEdit]): Boolean =
    edits.zip(edits.drop(1)).forall((first, second) => first.end < second.start)

  def isBefore(first: CursorPosition, second: CursorPosition): Boolean =
    first.line < second.line || (first.line == second.line && first.column < second.column)

  /** Cursor offsets after `edits` (ascending, separated): an offset inside or on the edges of an edit lands after the
    * text inserted there. Offsets are mapped in one merge over the sorted offsets, then returned in their given order.
    */
  def mapCursorOffsets(offsets: List[Int], edits: List[MultiCursorEdit]): List[Int] =
    val editArray = edits.toArray
    val ordered   = offsets.zipWithIndex.sortBy(_._1)

    @tailrec
    def loop(pending: List[(Int, Int)], editIndex: Int, shift: Int, done: List[(Int, Int)]): List[(Int, Int)] =
      pending match
        case Nil => done
        case (offset, position) :: rest =>
          if editIndex < editArray.length && editArray(editIndex).end < offset then
            val edit = editArray(editIndex)
            loop(pending, editIndex + 1, shift + edit.insertedText.length - (edit.end - edit.start), done)
          else if editIndex < editArray.length && editArray(editIndex).start <= offset then
            val edit = editArray(editIndex)
            loop(rest, editIndex, shift, (shift + edit.start + edit.insertedText.length, position) :: done)
          else loop(rest, editIndex, shift, (offset + shift, position) :: done)

    loop(ordered, 0, 0, Nil).sortBy(_._2).map(_._1)

  /** The edits of a batch located by line and column in the text they were made to, with where each ends up. */
  final class LineColumnEditMap private (steps: Vector[LineColumnEditMap.Step]):
    import LineColumnEditMap.key

    def map(position: CursorPosition, insertionAtBoundaryMoves: Boolean): CursorPosition =
      val at    = key(position.line, position.column)
      val index = lastStepStartingAtOrBefore(at, 0, steps.size)
      if index < 0 then position
      else
        val step = steps(index)
        if at > step.endKey then
          val column = if position.line == step.endLine then position.column + step.columnShift else position.column
          shifted(position, position.line + step.lineShift, column)
        else if at == step.endKey && step.isInsertion == insertionAtBoundaryMoves then
          CursorPosition(step.newEndLine, step.newEndColumn)
        else CursorPosition(step.newStartLine, step.newStartColumn)

    private def shifted(position: CursorPosition, line: Int, column: Int): CursorPosition =
      if line == position.line && column == position.column then position else CursorPosition(line, column)

    @tailrec
    private def lastStepStartingAtOrBefore(at: Long, low: Int, high: Int): Int =
      if low >= high then low - 1
      else
        val middle = (low + high) >>> 1
        if steps(middle).startKey > at then lastStepStartingAtOrBefore(at, low, middle)
        else lastStepStartingAtOrBefore(at, middle + 1, high)

  object LineColumnEditMap:

    private[EditRemap] def key(line: Int, column: Int): Long = (line.toLong << 32) | column.toLong

    final private[EditRemap] case class Step(
        startLine: Int,
        startColumn: Int,
        endLine: Int,
        endColumn: Int,
        newStartLine: Int,
        newStartColumn: Int,
        newEndLine: Int,
        newEndColumn: Int
    ):
      def startKey: Long       = key(startLine, startColumn)
      def endKey: Long         = key(endLine, endColumn)
      def isInsertion: Boolean = startKey == endKey
      def lineShift: Int       = newEndLine - endLine
      def columnShift: Int     = newEndColumn - endColumn

    /** `before` is the text `edits` (ascending, separated, in its coordinates) were made to. */
    def forEdits(before: Rope, edits: List[MultiCursorEdit]): Option[LineColumnEditMap] =
      Option.when(areSeparated(edits))(new LineColumnEditMap(describe(before, edits)))

    private def describe(before: Rope, edits: List[MultiCursorEdit]): Vector[Step] =
      val boundaries = before.offsetsToLineColumns(edits.flatMap(edit => List(edit.start, edit.end)).toIndexedSeq)

      edits.zipWithIndex
        .foldLeft((Vector.empty[Step], 0, -1, 0)) {
          case ((steps, lineShift, carryLine, carryShift), (edit, index)) =>
            val (startLine, startColumn) = boundaries(2 * index)
            val (endLine, endColumn)     = boundaries(2 * index + 1)
            val newStartLine             = startLine + lineShift
            val newStartColumn           = startColumn + (if startLine == carryLine then carryShift else 0)
            val inserted                 = edit.insertedText
            val insertedLines            = inserted.count(_ == '\n')
            val (newEndLine, newEndColumn) =
              if insertedLines == 0 then (newStartLine, newStartColumn + inserted.length)
              else (newStartLine + insertedLines, inserted.length - inserted.lastIndexOf('\n') - 1)
            val step =
              Step(startLine, startColumn, endLine, endColumn, newStartLine, newStartColumn, newEndLine, newEndColumn)
            (steps :+ step, newEndLine - endLine, endLine, newEndColumn - endColumn)
        }
        ._1
