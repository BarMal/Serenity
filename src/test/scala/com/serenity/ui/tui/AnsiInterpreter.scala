package com.serenity.ui.tui

import java.awt.Color

/** Replays an emitted diff against the frame it was diffed from, the way a real terminal would, so
  * [[TerminalAnsiDiffPropertySpec]] can check that the two always agree. A thin front end over [[TerminalEmulator]] --
  * the same interpreter every TUI behaviour spec asserts through -- narrowed to the frame-in/frame-out shape this
  * property needs.
  */
object AnsiInterpreter:

  /** The interpreted result of SGR 49 -- matches [[TerminalAnsiDiff]]'s alpha-0 "use the terminal's native background"
    * sentinel exactly, so replaying an emitted diff reproduces a transparent background cell too.
    */
  private[tui] val TransparentBackground: Color = TerminalEmulator.TransparentBackground

  def apply(initial: TerminalFrame, ansi: String): TerminalFrame =
    TerminalEmulator.fromFrame(initial).consume(ansi).frame
