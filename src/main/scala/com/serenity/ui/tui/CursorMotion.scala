package com.serenity.ui.tui

/** A cell the terminal's cursor sits on. `x == width` means the cursor is in the pending-wrap state after a glyph was
  * written in the last column: its row is known but its column is not reliable.
  */
final case class CursorPos(x: Int, y: Int)

/** The shortest byte sequence that moves the cursor between two cells. Absolute CUP is always a candidate; relative
  * moves (CUU/CUD/CUF/CUB), CHA and a carriage return with line feeds win when the cursor is already close.
  */
object CursorMotion:

  private val Esc = 0x1b.toChar

  def absolute(target: CursorPos): String = s"$Esc[${target.y + 1};${target.x + 1}H"

  def between(from: Option[CursorPos], target: CursorPos, width: Int): String =
    from match
      case Some(held) if held == target => ""
      case Some(held)                   => shortest(absolute(target), relative(held, target, width))
      case None                         => absolute(target)

  private def shortest(absoluteMove: String, alternatives: List[String]): String =
    alternatives.foldLeft(absoluteMove)((best, candidate) => if candidate.length < best.length then candidate else best)

  private def relative(from: CursorPos, target: CursorPos, width: Int): List[String] =
    val rows       = target.y - from.y
    val columnHeld = from.x < width
    List(
      Option.when(rows >= 0)("\r" + "\n" * rows + horizontal(0, target.x)),
      Option.when(columnHeld || rows == 0)(vertical(rows) + column(target.x)),
      Option.when(columnHeld)(vertical(rows) + horizontal(from.x, target.x))
    ).flatten

  private def vertical(rows: Int): String =
    if rows > 0 then parameterised(rows, 'B') else if rows < 0 then parameterised(-rows, 'A') else ""

  private def horizontal(from: Int, to: Int): String =
    if to > from then parameterised(to - from, 'C') else if to < from then parameterised(from - to, 'D') else ""

  private def column(x: Int): String = parameterised(x + 1, 'G')

  private def parameterised(count: Int, code: Char): String =
    if count == 1 then s"$Esc[$code" else s"$Esc[$count$code"
