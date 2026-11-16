package com.serenity.app

import java.io.PrintStream

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO}
import com.serenity.app.RendererChoice.Candidate
import com.serenity.app.RendererTrialChild.Setup
import com.serenity.app.RendererWorkload.{Plan, Step, Window}
import com.serenity.diagnostics.FrameTimings
import com.serenity.ui.display.DisplayScale
import com.serenity.ui.host.WindowHost
import com.serenity.ui.terminal.SwingWindowHost
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** What `--renderer-trial=<mode>` does at the first painted frame (#2618): after a short warm-up it runs the trial in
  * the background, as `--startup-probe` runs its probe, and `quit` is released when it has printed its line.
  */
object RendererTrialLaunch:

  /** A step that has not shown by now never will: long enough for a slow frame, short enough for a run of them. */
  val StepTimeout: FiniteDuration = 2.seconds

  val WarmUp: FiniteDuration = 2.seconds

  private val ResolutionSamples = 50

  /** The start-up's safe-mode notice closes with a short animation; the trial waits for it. */
  private val NoticeSettle = 500.millis

  def firstFrame(
    options: LaunchOptions,
    host: WindowHost,
    backendKey: String,
    quit: Deferred[IO, Unit],
    out: PrintStream = System.out,
    plan: Plan = Plan.standard,
    stepTimeout: FiniteDuration = StepTimeout,
    warmUp: FiniteDuration = WarmUp,
    frameTimings: FrameTimings = FrameTimings()
  ): IO[Unit] =
    options.rendererTrial.fold(IO.unit)(candidate =>
      (IO.sleep(warmUp) *> setup(candidate, host, backendKey, plan, stepTimeout, frameTimings).flatMap(
        RendererTrialChild.run(_, quit, out)
      )).start.void
    )

  private def setup(
    candidate: Candidate,
    host: WindowHost,
    backendKey: String,
    plan: Plan,
    stepTimeout: FiniteDuration,
    frameTimings: FrameTimings
  ): IO[Setup] =
    host match
      case swing: SwingWindowHost =>
        val driver = SwingTrialDriver(swing)
        Slf4jLogger.create[IO].flatMap(logger => RobotScreen().memoize.map(screen => (logger, screen))).map {
          (logger, screen) =>
            val window: Window =
              step => screen.flatMap(grabbing => RendererTrialWindow(driver, grabbing, stepTimeout).perform(step))
            Setup(
              candidate = candidate,
              plan = plan,
              window = window,
              clockResolutionMs = for
                grabbing <- screen
                region   <- driver.watchedFor(Step.Type('a'))
                quickest <- PixelClock.resolution(grabbing, region, ResolutionSamples)
              yield quickest,
              scale = DisplayScale.forComponent(swing.window.canvas).x,
              backendKey = backendKey,
              renderApi = IO(swing.window.layerPresenter.map(_.renderApi)),
              device = IO(swing.window.layerPresenter.flatMap(_.deviceName)),
              timeout = RendererTrialChild.Timeout,
              prepare = driver.dismissNotices *> IO.sleep(NoticeSettle),
              prepareTimeout = stepTimeout + NoticeSettle,
              progress = line => logger.info(line),
              inApp = Some(
                IO { frameTimings.keyLatency.setEnabled(true); frameTimings.setEnabled(true) }
                  .as(InAppTrialWindow(driver, frameTimings, stepTimeout))
              )
            )
        }
      case _ =>
        val refused = new UnsupportedOperationException("this window host cannot run a trial")
        IO.pure(
          Setup(
            candidate,
            plan,
            _ => IO.raiseError(refused),
            IO.raiseError(refused),
            1.0,
            backendKey,
            IO.pure(None),
            RendererTrialChild.Timeout
          )
        )
