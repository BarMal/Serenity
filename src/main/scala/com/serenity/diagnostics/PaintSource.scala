package com.serenity.diagnostics

/** What stamped a paint in [[FrameTimings]]: the Swing canvas's own `paintComponent`, or a draw of the Skia layer.
  * Counted apart so a `[FRAME]` line shows which of them a window's paints, and its keystrokes' paint ends, came from
  * (#2489).
  */
enum PaintSource(val label: String):
  case Canvas extends PaintSource("canvas")
  case Layer  extends PaintSource("layer")

object PaintSource:

  /** `canvas=2 layer=1`, the sources with a count, in declaration order; empty when none has. */
  def describe(counts: Map[PaintSource, Int]): String =
    values.toList
      .flatMap(source => counts.get(source).filter(_ > 0).map(count => s"${source.label}=$count"))
      .mkString(" ")
