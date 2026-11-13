package com.serenity.app

import java.io.PrintStream
import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.{ExitCode, IO}
import com.serenity.app.RendererChoice.{Candidate, Outcome, Recommendation}
import com.serenity.app.RendererCoordinator.{Environment, Finished, Spawner}
import com.serenity.config.{AppConfig, ConfigManager, RenderBackendSetting, SkiaFrames}

/** `serenity --optimize-renderer` (#2618): says what it is about to do, measures each renderer mode in a window of its
  * own, prints what it found and, only when asked with `--apply`, writes the recommended mode to the config.
  */
object RendererOptimizeCommand:

  /** What the command needs from outside, so it can be run without windows. */
  final case class Deps(
      spawner: Spawner,
      environment: Environment,
      configPath: Path,
      hasDisplay: Boolean,
      countdown: FiniteDuration,
      out: PrintStream
  )

  /** The mode the config puts the window in now: the raster surface, or the layer with its frame form. */
  def currentCandidate(config: AppConfig): Candidate =
    val surface = config.surfaceConfig
    if surface.renderBackend == RenderBackendSetting.SkiaRaster then Candidate.SkiaRaster
    else if surface.renderSkiaFrames == SkiaFrames.Picture then Candidate.LayerPicture
    else Candidate.LayerImage

  private def notice(options: LaunchOptions, deps: Deps): List[String] =
    List(
      s"Measuring the renderers on this machine: about ${RendererRunOrder.minutes(options.thorough)} minutes, " +
        s"${RendererRunOrder.order(options.thorough).size} trials, each in a window of its own.",
      "It types into those windows and changes their size and theme: leave the keyboard and mouse alone.",
      s"Starting in ${deps.countdown.toSeconds} seconds; press Ctrl+C to cancel."
    )

  def run(options: LaunchOptions, deps: Deps): IO[ExitCode] =
    def say(lines: List[String]): IO[Unit] = IO(lines.foreach(deps.out.println))
    if !deps.hasDisplay then
      say(List("--optimize-renderer opens windows, so it needs a display. Run it from a graphical session.")).as(
        ExitCode.Error
      )
    else
      for
        _      <- say(notice(options, deps))
        _      <- IO.sleep(deps.countdown)
        config <- ConfigManager.loadConfigIO(Some(deps.configPath.toString))
        finished <- RendererCoordinator.optimize(
          currentCandidate(config),
          options.thorough,
          deps.spawner,
          deps.environment,
          (number, of, candidate) => say(List(s"Trial $number of $of: ${candidate.label}")),
          (number, candidate, reason) =>
            say(List(s"Trial $number (${candidate.label}) failed: $reason; running it once more."))
        )
        _       <- say(RendererCoordinator.report(finished))
        applied <- if options.apply then apply(finished, deps) else IO.pure(None)
        _       <- applied.fold(IO.unit)(line => say(List(line)))
      yield exitCode(finished, applied)

  private def apply(finished: Finished, deps: Deps): IO[Option[String]] =
    finished.recommendation match
      case Recommendation.Switch(to, _) =>
        RendererApply(to, deps.configPath).map {
          case Right(_) =>
            Some(s"The recommended settings are written to ${deps.configPath}: ${RendererSettings.line(to)}.")
          case Left(message) => Some(s"The recommended settings could not be written to ${deps.configPath}: $message")
        }
      case Recommendation.Keep(_, _) => IO.pure(None)

  private def exitCode(finished: Finished, applied: Option[String]): ExitCode =
    val everyTrialFailed = finished.runs.forall(_.runs.forall {
      case _: Outcome.Failed   => true
      case _: Outcome.Measured => false
    })
    if everyTrialFailed || applied.exists(_.contains("could not be written")) then ExitCode.Error else ExitCode.Success
