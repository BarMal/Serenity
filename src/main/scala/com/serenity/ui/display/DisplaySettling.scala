package com.serenity.ui.display

import scala.concurrent.duration.FiniteDuration

import cats.effect.IO

/** Reads a window's size until the window manager has finished answering the window being shown (#2479). */
object DisplaySettling:

  final case class Outcome(snapshot: DisplaySnapshot, settled: Boolean)

  /** Reads `sample` every `interval` until two reads agree on the frame and canvas size, or `maxWait` has passed. */
  def untilSettled(sample: IO[DisplaySnapshot], interval: FiniteDuration, maxWait: FiniteDuration): IO[Outcome] =
    val attempts = math.max(1L, maxWait.toNanos / math.max(1L, interval.toNanos))
    def next(previous: DisplaySnapshot, remaining: Long): IO[Outcome] =
      IO.sleep(interval) >> sample.flatMap { current =>
        if current.sameWindowSizeAs(previous) then IO.pure(Outcome(current, settled = true))
        else if remaining <= 1 then IO.pure(Outcome(current, settled = false))
        else next(current, remaining - 1)
      }
    sample.flatMap(next(_, attempts))
