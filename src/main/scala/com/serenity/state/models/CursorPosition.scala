package com.serenity.state.models

import com.serenity.rope.Rope

/** Which of two wrapped visual rows a cursor belongs to when its column is the boundary they share -- one row's
  * `endColumn` is the next row's `startColumn`, so the column alone cannot say where the caret is drawn or which row
  * vertical movement steps from.
  *
  * [[RowAffinity.Downstream]] is the answer for every way of reaching that column except one: arriving there is
  * arriving at the start of the next row. End is the exception -- it means "the end of the row I am on" -- so it is the
  * only thing that produces [[RowAffinity.Upstream]]. Away from a boundary the two are indistinguishable.
  */
enum RowAffinity:
  case Upstream, Downstream

/** Internal editor cursor position.
  *
  * `column` is a UTF-16 code-unit index within the logical line, matching rope offsets, Java `String` indexing, and
  * measured text layout APIs. User-facing movement/edit commands should snap through grapheme helpers before
  * constructing or mutating a `CursorPosition`.
  *
  * `rowAffinity` is view state, never persisted (`SessionCursorPosition` carries line and column alone) and defaulted
  * on every construction, so a cursor built anywhere in the editor is downstream unless something deliberately says
  * otherwise. `copy` preserves it, which is why the movement helpers below rebuild rather than copy.
  */
final case class CursorPosition(line: Int, column: Int, rowAffinity: RowAffinity = RowAffinity.Downstream):
  def moveRight: CursorPosition = CursorPosition(line, column + 1)
  def moveLeft: CursorPosition  = CursorPosition(line, Math.max(0, column - 1))
  def moveDown: CursorPosition  = CursorPosition(line + 1, column)
  def moveUp: CursorPosition    = CursorPosition(Math.max(0, line - 1), column)

  def downstream: CursorPosition =
    if rowAffinity == RowAffinity.Downstream then this else copy(rowAffinity = RowAffinity.Downstream)

  def upstream: CursorPosition =
    if rowAffinity == RowAffinity.Upstream then this else copy(rowAffinity = RowAffinity.Upstream)

  /** The default affinity is left out, so the many diagnostics that interpolate a position (invariant violations, logs)
    * keep reading as a line and a column, and mention the row a cursor is pinned to only where that is not the default.
    */
  override def toString: String =
    if rowAffinity == RowAffinity.Downstream then s"CursorPosition($line,$column)"
    else s"CursorPosition($line,$column,$rowAffinity)"

/** Line-then-column ordering, the single comparison every hand-rolled `isBefore`/`isAfter`/`isAtOrBefore` and
  * `DirectedRange` start/end/contains check used to reimplement separately (`#1065`, `#1053`).
  */
given Ordering[CursorPosition] = Ordering.by(cursor => (cursor.line, cursor.column))

extension (cursors: List[CursorPosition])

  /** The primary (first) cursor, or the document origin for a cursor-less list. Centralises the
    * `cursors.headOption.getOrElse(CursorPosition(0, 0))` fallback repeated across the editor reducer and viewport
    * capability (`#1066`).
    */
  def primaryCursor: CursorPosition = cursors.headOption.getOrElse(CursorPosition(0, 0))

extension (rope: Rope)

  /** Rope offset -> logical `CursorPosition`, via `Rope.offsetToLineColumn`. The single canonical conversion for every
    * offset->position call site, including ones that used to iterate a materialised `String` by hand.
    */
  def offsetToCursorPosition(offset: Int): CursorPosition =
    val (line, column) = rope.offsetToLineColumn(offset)
    CursorPosition(line, column)
