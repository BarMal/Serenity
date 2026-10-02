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

  def stream(timings: FrameTimings, enabled: IO[Boolean], log: String => IO[Unit]): Stream[IO, Unit] =
    Stream.awakeEvery[IO](Interval).evalMap(_ => tick(timings, enabled, log))
