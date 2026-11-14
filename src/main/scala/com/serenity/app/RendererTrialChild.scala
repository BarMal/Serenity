package com.serenity.app

import java.io.PrintStream

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO}
import com.serenity.app.RendererChoice.{Candidate, Outcome}
import com.serenity.app.RendererWorkload.{Plan, Window}

/** What a `--renderer-trial=<mode>` child does (#2618): run the workload in its window, print one `RENDERER_TRIAL` line
  * with what it measured, and quit. Whatever goes wrong, it prints a line saying so: a child that printed nothing would
  * give the coordinator a failure with no reason.
  */
object RendererTrialChild:

  /** What the child needs, apart from the window it already has: how to time a step, and the facts the line reports. */
  final case class Setup(
      candidate: Candidate,
      plan: Plan,
      window: Window,
      clockResolutionMs: IO[Double],
      scale: Double,
      backendKey: String,
      renderApi: IO[Option[String]],
      timeout: FiniteDuration
  )

  /** A ceiling above the workload's two minutes or so, so a hung window cannot hang the coordinator. */
  val Timeout: FiniteDuration = 5.minutes

  /** The trial types into the window, so it must never run against someone's real session: only in safe mode, which
    * keeps its session in a scratch folder and changes nothing on disk.
    */
  def refusal(options: LaunchOptions): Option[String] =
    Option.when(options.rendererTrial.isDefined && !options.safeMode)(
      "--renderer-trial types into the editor, so it needs --safe-mode. Not starting."
    )

  private def backendFor(candidate: Candidate): RenderBackend = candidate match
    case Candidate.LayerImage | Candidate.LayerPicture => RenderBackend.Skia
    case Candidate.SkiaRaster                          => RenderBackend.SkiaRaster

  def run(setup: Setup, quit: Deferred[IO, Unit], out: PrintStream): IO[Unit] =
    val measured: IO[Outcome] =
      for
        resolution <- setup.clockResolutionMs
        result     <- RendererWorkload.run(setup.plan, setup.window)
      yield RendererWorkload.outcome(result, setup.scale, resolution)
    val guarded: IO[Outcome] =
      measured
        .timeoutTo(setup.timeout, IO.pure(Outcome.Failed(s"the trial timed out after ${setup.timeout.toSeconds} s")))
        .handleError(error => Outcome.Failed(s"the trial failed: ${error.getMessage}"))
    for
      outcome <- guarded
      api     <- setup.renderApi.handleError(_ => None)
      report = RenderBackend.values.find(_.configKey == setup.backendKey) match
        case Some(backend) => RendererTrial.Report(setup.candidate, backend, api, outcome)
        case None =>
          RendererTrial.Report(
            setup.candidate,
            backendFor(setup.candidate),
            api,
            Outcome.Failed(s"the window reported a backend this build does not know: ${setup.backendKey}")
          )
      _ <- IO { out.println(RendererTrial.line(report)); out.flush() }
      _ <- quit.complete(()).void
    yield ()
