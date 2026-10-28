package com.serenity.app

import java.io.PrintStream

import scala.concurrent.duration.FiniteDuration

import cats.effect.{Deferred, IO}

/** What `--startup-probe` reports (#2420): the editor is interactive once a keystroke typed after the first frame has
  * been applied and painted. The harness times the marker line from process start, as it times
  * [[SmokeTest.readyLine]]. Every outcome releases `quit`, so a probe that cannot finish cannot hang the launch.
  */
object StartupProbe:

  val interactiveLine: String = "SERENITY_PROBE_INTERACTIVE"
  val timedOutLine: String    = "SERENITY_PROBE_TIMED_OUT"
  val failedLine: String      = "SERENITY_PROBE_FAILED"

  def run(
    inject: IO[Unit],
    keyPainted: IO[Unit],
    quit: Deferred[IO, Unit],
    timeout: FiniteDuration,
    out: PrintStream = System.out
  ): IO[Unit] =
    val painted: IO[String] = keyPainted.as(interactiveLine).timeoutTo(timeout, IO.pure(timedOutLine))
    val outcome: IO[String] = (inject >> painted).handleError(_ => failedLine)
    outcome.flatMap(line => IO { out.println(line); out.flush() }) >> quit.complete(()).void
