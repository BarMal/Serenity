package com.serenity.ui.tui

import java.awt.Color

import scala.annotation.tailrec

import com.serenity.ui.theme.TextStyle

/** A minimal terminal simulator, test-only: interprets the CUP/SGR/clear-screen subset of ANSI that
  * [[TerminalAnsiDiff]] emits and applies it to a starting grid, so property tests can check that replaying an emitted
  * diff actually reproduces the frame it was diffed against -- the same way a real terminal would.
  */
object AnsiInterpreter:

  private val Esc: Char = 0x1b.toChar

  /** The interpreted result of SGR 49 -- matches [[TerminalAnsiDiff]]'s alpha-0 "use the terminal's native background"
    * sentinel exactly, so replaying an emitted diff reproduces a transparent background cell too.
    */
  private[tui] val TransparentBackground: Color = new Color(0, 0, 0, 0)

  final private case class CursorState(row: Int, col: Int, fg: Color, bg: Color, style: TextStyle)

  def apply(initial: TerminalFrame, ansi: String): TerminalFrame =
    val grid = Array.tabulate(initial.height, initial.width)((y, x) => initial(x, y))

    @tailrec def loop(i: Int, state: CursorState): Unit =
      if i >= ansi.length then ()
      else if ansi(i) == Esc && i + 1 < ansi.length && ansi(i + 1) == '[' then
        val end     = ansi.indexWhere(_.isLetter, i + 2)
        val body    = ansi.substring(i + 2, end)
        val command = ansi(end)
        val nextState = command match
          case 'H' =>
            if body.isEmpty then state.copy(row = 0, col = 0)
            else
              val parts = body.split(";").map(_.toInt)
              state.copy(row = parts(0) - 1, col = parts(1) - 1)
          case 'J' =>
            for y <- grid.indices; x <- grid(y).indices do grid(y)(x) = TerminalCell.blank(state.fg, state.bg)
            state
          case 'm' =>
            val (style, fg, bg) =
              applySgr(if body.isEmpty then Array("0") else body.split(";"), state.style, state.fg, state.bg)
            state.copy(style = style, fg = fg, bg = bg)
          case _ => state
        loop(end + 1, nextState)
      else
        grid(state.row)(state.col) = TerminalCell(ansi(i).toInt, state.fg, state.bg, state.style, CellSpan.Narrow)
        loop(i + 1, state.copy(col = state.col + 1))

    loop(0, CursorState(0, 0, Color.WHITE, Color.BLACK, TextStyle.normal))
    TerminalFrame(initial.width, initial.height, grid.map(_.toVector).toVector)

  private def applySgr(parts: Array[String], current: TextStyle, fg: Color, bg: Color): (TextStyle, Color, Color) =
    @tailrec def loop(idx: Int, style: TextStyle, fg: Color, bg: Color): (TextStyle, Color, Color) =
      if idx >= parts.length then (style, fg, bg)
      else
        parts(idx) match
          case "0" => loop(idx + 1, TextStyle.normal, fg, bg)
          case "1" => loop(idx + 1, style.copy(isBold = true), fg, bg)
          case "3" => loop(idx + 1, style.copy(isItalic = true), fg, bg)
          case "4" => loop(idx + 1, style.copy(isUnderlined = true), fg, bg)
          case "38" =>
            loop(idx + 5, style, new Color(parts(idx + 2).toInt, parts(idx + 3).toInt, parts(idx + 4).toInt), bg)
          case "48" =>
            loop(idx + 5, style, fg, new Color(parts(idx + 2).toInt, parts(idx + 3).toInt, parts(idx + 4).toInt))
          // SGR 49: reset to the terminal's own default background, mirroring TerminalAnsiDiff's alpha-0 sentinel --
          // interpreted back as the same alpha-0 `Color` so a round trip through emit-then-replay reproduces it.
          case "49" => loop(idx + 1, style, fg, AnsiInterpreter.TransparentBackground)
          case _    => loop(idx + 1, style, fg, bg)
    loop(0, current, fg, bg)
