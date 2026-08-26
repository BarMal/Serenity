package com.serenity.ui.tui

import java.awt.Color

import com.serenity.ui.theme.TextStyle

/** Turns one screen-buffer transition into the minimal ANSI byte sequence needed to reproduce it on a real terminal:
  * absolute cursor positioning (CUP), 24-bit truecolor SGR runs batched while the style stays unchanged, and a full
  * clear-and-repaint when there is no previous frame to diff against (first frame, or a resize).
  */
object TerminalAnsiDiff:

  private val Esc: Char   = 0x1b.toChar
  private val Reset       = s"$Esc[0m"
  private val ClearScreen = s"$Esc[2J"
  private val CursorHome  = s"$Esc[H"

  def emit(previous: Option[TerminalFrame], next: TerminalFrame): String =
    previous match
      case Some(prev) if prev.width == next.width && prev.height == next.height => emitDiff(prev, next)
      case _                                                                    => emitFull(next)

  private def emitFull(frame: TerminalFrame): String =
    val sb         = new StringBuilder
    val _          = sb.append(ClearScreen).append(CursorHome)
    val hadContent = appendRuns(sb, frame, (0 until frame.height).map(y => y -> (0 until frame.width)))
    val _          = if hadContent then sb.append(Reset) else sb
    sb.toString

  private def emitDiff(previous: TerminalFrame, next: TerminalFrame): String =
    val sb = new StringBuilder
    val rowRuns = (0 until next.height).flatMap { y =>
      val changedCols = (0 until next.width).filter(x => next(x, y) != previous(x, y))
      runsOf(changedCols).map(run => y -> run)
    }
    val hadContent = appendRuns(sb, next, rowRuns)
    val _          = if hadContent then sb.append(Reset) else sb
    sb.toString

  /** Group a sorted sequence of column indices into maximal contiguous runs. */
  private def runsOf(cols: Seq[Int]): Seq[Seq[Int]] =
    cols.foldLeft(Vector.empty[Vector[Int]]) { (runs, col) =>
      runs.lastOption match
        case Some(run) if run.lastOption.contains(col - 1) => runs.dropRight(1) :+ (run :+ col)
        case _                                             => runs :+ Vector(col)
    }

  final private case class RunState(fg: Option[Color], bg: Option[Color], style: Option[TextStyle], wrote: Boolean)

  private def appendRuns(sb: StringBuilder, frame: TerminalFrame, rowRuns: Seq[(Int, Seq[Int])]): Boolean =
    rowRuns.foldLeft(false) {
      case (wroteSoFar, (_, run)) if run.isEmpty => wroteSoFar
      case (wroteSoFar, (y, run)) =>
        val _ = sb.append(cup(run(0), y))
        val finalState = run.foldLeft(RunState(None, None, None, false)) { (state, x) =>
          val cell = frame(x, y)
          if cell.span == CellSpan.Continuation then state
          else
            val styleChanged =
              !state.fg.contains(cell.fg) || !state.bg.contains(cell.bg) || !state.style.contains(cell.style)
            val _ = if styleChanged then sb.append(sgr(cell.fg, cell.bg, cell.style)) else sb
            val _ = sb.append(cell.text)
            RunState(Some(cell.fg), Some(cell.bg), Some(cell.style), true)
        }
        wroteSoFar || finalState.wrote
    }

  private def cup(x: Int, y: Int): String = s"$Esc[${y + 1};${x + 1}H"

  private def sgr(fg: Color, bg: Color, style: TextStyle): String =
    val attrs = List(
      Some("0"),
      Option.when(style.isBold)("1"),
      Option.when(style.isItalic)("3"),
      Option.when(style.isUnderlined)("4"),
      Some(s"38;2;${fg.getRed};${fg.getGreen};${fg.getBlue}"),
      Some(s"48;2;${bg.getRed};${bg.getGreen};${bg.getBlue}")
    ).flatten
    s"$Esc[${attrs.mkString(";")}m"
