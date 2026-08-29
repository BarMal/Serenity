package com.serenity.state.models

import com.serenity.rope.Rope

/** Internal editor cursor position.
  *
  * `column` is a UTF-16 code-unit index within the logical line, matching rope offsets, Java `String` indexing, and
  * measured text layout APIs. User-facing movement/edit commands should snap through grapheme helpers before
  * constructing or mutating a `CursorPosition`.
  */
final case class CursorPosition(line: Int, column: Int):
  def moveRight: CursorPosition = copy(column = column + 1)
  def moveLeft: CursorPosition  = copy(column = Math.max(0, column - 1))
  def moveDown: CursorPosition  = copy(line = line + 1)
  def moveUp: CursorPosition    = copy(line = Math.max(0, line - 1))

extension (rope: Rope)

  /** Rope offset -> logical `CursorPosition`, via `Rope.offsetToLineColumn`. The single canonical conversion for every
    * offset->position call site, including ones that used to iterate a materialised `String` by hand.
    */
  def offsetToCursorPosition(offset: Int): CursorPosition =
    val (line, column) = rope.offsetToLineColumn(offset)
    CursorPosition(line, column)
