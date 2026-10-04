package com.serenity.diagnostics

import scala.concurrent.duration.*

import cats.effect.IO
import fs2.Stream

/** Turns [[FrameTimings]] into one `[FRAME]` log line per interval while `ui.render.frame_timing` is on. */
object FrameTimingReport:

  val Interval: FiniteDuration = 5.seconds

  /** Drains every tick, enabled or not, so turning timing on starts from a fresh window rather than a backlog. */
  def tick(timings: FrameTimings, enabled: IO[Boolean], log: String => IO[Unit]): IO[Unit] =
    IO(timings.drain()).flatMap(summary => enabled.flatMap(on => if on then log(summary.logLine) else IO.unit))

  /** Runs a report timer only while `enabled` last said on, so with frame timing off nothing wakes at all (#1938). */
  def stream(timings: FrameTimings, enabled: Stream[IO, Boolean], log: String => IO[Unit]): Stream[IO, Unit] =
    enabled.changes.switchMap(on => if on then reporting(timings, log) else Stream.empty)

  /** Discards what gathered while timing was off, so the first line covers only its own window. */
  private def reporting(timings: FrameTimings, log: String => IO[Unit]): Stream[IO, Unit] =
    Stream.exec(IO(timings.drain()).void) ++
      Stream.awakeEvery[IO](Interval).evalMap(_ => tick(timings, IO.pure(true), log))
