package com.serenity.diagnostics

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.foldable.*
import fs2.Stream

/** Switches [[KeyLatencyTrace]] with `ui.render.latency_trace` and, while it is on, logs each painted keystroke's
  * `[LATENCY]` line followed by the window's summary once per interval.
  */
object KeyLatencyReport:

  val Interval: FiniteDuration = 5.seconds

  def tick(trace: KeyLatencyTrace, log: String => IO[Unit]): IO[Unit] =
    IO(trace.drain()).flatMap(_.logLines.traverse_(log))

  /** With tracing off nothing wakes, and every hook in the pipeline stays a flag read. */
  def stream(trace: KeyLatencyTrace, enabled: Stream[IO, Boolean], log: String => IO[Unit]): Stream[IO, Unit] =
    enabled.changes.switchMap { on =>
      Stream.exec(IO(trace.setEnabled(on))) ++
        (if on then Stream.awakeEvery[IO](Interval).evalMap(_ => tick(trace, log)) else Stream.empty)
    }
