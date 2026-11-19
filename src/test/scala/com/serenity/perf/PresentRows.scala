package com.serenity.perf

import com.serenity.perf.TypingWhileIndexingProfile.Summary

/** The clock readings of one presented frame, in nanoseconds: the draw requested, the draw finished on the SDL thread,
  * the present started and finished (copy, carets, `flushAndSubmit`) and the swap returned (#2380).
  */
final private[perf] case class FrameStamps(
    requested: Long,
    drawn: Long,
    presentStart: Long,
    presentEnd: Long,
    swapEnd: Long
):

  def isOrdered: Boolean =
    requested <= drawn && drawn <= presentStart && presentStart <= presentEnd && presentEnd <= swapEnd

  def times: FrameTimes =
    FrameTimes(
      drawMs = FrameTimes.ms(requested, drawn),
      handoffMs = FrameTimes.ms(drawn, presentStart),
      presentMs = FrameTimes.ms(presentStart, presentEnd),
      swapMs = FrameTimes.ms(presentEnd, swapEnd)
    )

/** `handoffMs` is the benchmark thread noticing the draw finished and asking for the present; it is part of
  * `drawToRenderedMs` and no real frame has it.
  */
final private[perf] case class FrameTimes(drawMs: Double, handoffMs: Double, presentMs: Double, swapMs: Double):
  def drawToRenderedMs: Double = drawMs + handoffMs + presentMs + swapMs

private[perf] object FrameTimes:
  def ms(fromNanos: Long, toNanos: Long): Double = (toNanos - fromNanos) / 1_000_000.0

/** p50 and p95 of each row over a run of frames. `interval` is one swap's end to the next's. */
final private[perf] case class PresentRows(
    draw: Summary,
    handoff: Summary,
    present: Summary,
    swap: Summary,
    drawToRendered: Summary,
    interval: Summary
):

  /** One table line: `label`, then each row as p50/p95 in milliseconds. */
  def describe(label: String): String =
    def cell(name: String, summary: Summary): String = f"$name ${summary.p50Ms}%.2f/${summary.p95Ms}%.2f"
    List(
      cell("draw", draw),
      cell("handoff", handoff),
      cell("present", present),
      cell("swap", swap),
      cell("draw_to_rendered", drawToRendered),
      cell("interval", interval)
    ).mkString(f"$label%-44s ", " | ", f" (n=${drawToRendered.count})")

private[perf] object PresentRows:

  def of(frames: Vector[FrameStamps]): PresentRows =
    val times = frames.map(_.times)
    PresentRows(
      draw = Summary.of(times.map(_.drawMs)),
      handoff = Summary.of(times.map(_.handoffMs)),
      present = Summary.of(times.map(_.presentMs)),
      swap = Summary.of(times.map(_.swapMs)),
      drawToRendered = Summary.of(times.map(_.drawToRenderedMs)),
      interval =
        Summary.of(frames.zip(frames.drop(1)).map((earlier, later) => FrameTimes.ms(earlier.swapEnd, later.swapEnd)))
    )
