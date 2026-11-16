package com.serenity.app

import java.io.PrintStream

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
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

  /** The start-up's safe-mode notice closes with a short animation, and a window sized up is laid out again: the trial
    * waits until what it will watch has stayed the same for this long, looking at it this often, for at most the cap.
    */
  private val QuietFor  = 300.millis
  private val QuietPoll = 50.millis
  private val QuietCap  = 5.seconds

  /** What sizing a window up to the screen may take, beyond the step timeout, before the trial gives up on it. */
  private val FitAllowance = 5.seconds

  /** Waits for the regions the workload watches first to stop changing, so nothing but the trial's own steps does it.
    */
  private def quietScreen(driver: SwingTrialDriver, screen: IO[PixelClock.Screen]): IO[Unit] =
    for
      grabbing <- screen
      regions  <- List(Step.Type('a'), Step.PageDown).traverse(driver.watchedFor)
      _        <- PixelClock.quiet(grabbing, regions, QuietFor, QuietPoll, QuietCap)
    yield ()

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
              canvasPixels = IO {
                val canvas = swing.window.canvas
                val scale  = DisplayScale.forComponent(canvas)
                (canvas.getWidth * scale.x).round * (canvas.getHeight * scale.y).round
              },
              backendKey = backendKey,
              renderApi = IO(swing.window.layerPresenter.map(_.renderApi)),
              device = IO(swing.window.layerPresenter.flatMap(_.deviceName)),
              timeout = RendererTrialChild.Timeout,
              prepare = driver.dismissNotices *> driver.fitToScreen *> quietScreen(driver, screen),
              prepareTimeout = stepTimeout + QuietCap + FitAllowance,
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
