package com.serenity.diagnostics

import scala.concurrent.duration.*

import cats.effect.IO
import fs2.Stream

/** Switches [[FrameTimings]], [[FrameCounters]] and the [[EdtLagTrace]] with `ui.render.frame_timing` and, while it is
  * on, logs one `[FRAME]` line per interval. A frontend with an event thread also has it probed meanwhile, so the line
  * says how late that thread ran what was posted to it.
  */
object FrameTimingReport:

  val Interval: FiniteDuration = 5.seconds

  /** Drains every tick, enabled or not, so turning timing on starts from a fresh window rather than a backlog. */
  def tick(
    timings: FrameTimings,
    enabled: IO[Boolean],
    log: String => IO[Unit],
    counters: FrameCounters = FrameCounters(),
    edtLag: EdtLagTrace = EdtLagTrace()
  ): IO[Unit] =
    IO(timings.drain().withCounters(counters.drain()).withEdtLag(edtLag.drain())).flatMap(summary =>
      enabled.flatMap(on => if on then log(summary.logLine) else IO.unit)
    )

  /** Runs a report timer only while `enabled` last said on, so with frame timing off nothing wakes at all (#1938). */
  def stream(
    timings: FrameTimings,
    enabled: Stream[IO, Boolean],
    log: String => IO[Unit],
    counters: FrameCounters = FrameCounters(),
    edtLag: EdtLagTrace = EdtLagTrace(),
    eventThread: Option[Runnable => Unit] = None
  ): Stream[IO, Unit] =
    enabled.changes.switchMap(on =>
      Stream.exec(IO { timings.setEnabled(on); counters.setEnabled(on); edtLag.setEnabled(on) }) ++
        (if on then reporting(timings, counters, edtLag, eventThread, log) else Stream.empty)
    )

  /** Discards what gathered while timing was off, so the first line covers only its own window. */
  private def reporting(
    timings: FrameTimings,
    counters: FrameCounters,
    edtLag: EdtLagTrace,
    eventThread: Option[Runnable => Unit],
    log: String => IO[Unit]
  ): Stream[IO, Unit] =
    val probe = eventThread.fold(Stream.empty)(post => EdtLagProbe.stream(edtLag, post))
    Stream.exec(IO(timings.drain()).void >> IO(counters.drain()).void >> IO(edtLag.drain()).void) ++
      Stream
        .awakeEvery[IO](Interval)
        .evalMap(_ => tick(timings, IO.pure(true), log, counters, edtLag))
        .concurrently(probe)
