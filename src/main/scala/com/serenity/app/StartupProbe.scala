package com.serenity.app

import java.io.PrintStream
import java.nio.file.{Files, Path}

import scala.concurrent.duration.FiniteDuration

import cats.effect.{Deferred, IO}

/** What `--startup-probe` reports (#2420): the editor is interactive once a keystroke typed after the first frame has
  * been applied and painted. The harness times the marker line from process start, as it times [[SmokeTest.readyLine]].
  * Every outcome releases `quit`, so a probe that cannot finish cannot hang the launch.
  */
object StartupProbe:

  val interactiveLine: String = "SERENITY_PROBE_INTERACTIVE"
  val timedOutLine: String    = "SERENITY_PROBE_TIMED_OUT"
  val failedLine: String      = "SERENITY_PROBE_FAILED"

  /** A file the harness creates in the home it hands the app (`-Duser.home`), to say that home is its own. */
  val IsolatedHomeMarker: String = ".serenity-startup-probe-home"

  /** The probe types a character, so it must never run against someone's real files or session: only in safe mode,
    * which keeps its session in a scratch folder and changes nothing on disk, or in a home that carries the marker.
    */
  def refusal(options: LaunchOptions, userHome: Path): Option[String] =
    val marked = Files.isRegularFile(userHome.resolve(IsolatedHomeMarker))
    Option.when(options.startupProbe && !options.safeMode && !marked)(
      "--startup-probe types a character into the editor, so it needs --safe-mode or a home the harness has marked " +
        s"as its own (a file named $IsolatedHomeMarker in user.home). Not starting."
    )

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
