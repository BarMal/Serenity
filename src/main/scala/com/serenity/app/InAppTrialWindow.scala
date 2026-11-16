package com.serenity.app

import scala.concurrent.duration.*

import cats.effect.IO
import com.serenity.app.RendererWorkload.{Redraw, Step, Window}
import com.serenity.diagnostics.{FramePhase, FrameTimings}

/** The window of a renderer trial timed by the app's own stamps, for a screen the pixel clock cannot read (#2618).
  *
  * A typed character or a page-down is answered by that keystroke's `key_to_screen` from the latency trace, the clock
  * `bench-laptop.sh` reads; a theme toggle by the frame timings' `input-to-paint`. A resize has no input for the app to
  * stamp, so it is left out and answers none without waiting. The stamps stop at the end of the draw cycle (the canvas
  * paint for the raster), and a key the trial submits has no OS-to-event-thread hop, so these are lower bounds.
  */
object InAppTrialWindow:

  private val Poll = 5.millis

  def apply(driver: RendererTrialWindow.Driver, timings: FrameTimings, timeout: FiniteDuration): Window =
    case Step.Type(char) => answered(clearKeys(timings), driver.typeCharacter(char), keyMs(timings), timeout)
    case Step.PageDown   => answered(clearKeys(timings), driver.pressPageDown, keyMs(timings), timeout)
    case Step.FullRedraw(Redraw.ThemeToggle) =>
      answered(clearFrames(timings), driver.toggleTheme, frameMs(timings), timeout)
    case Step.FullRedraw(Redraw.Resize) => IO.pure(None)

  private def clearKeys(timings: FrameTimings): IO[Unit]   = IO(timings.keyLatency.drain()).void
  private def clearFrames(timings: FrameTimings): IO[Unit] = IO(timings.drain()).void

  private def keyMs(timings: FrameTimings): IO[Option[Double]] =
    IO(timings.keyLatency.drain().keys.map(_.keyToScreenNanos).maxOption.map(_ / 1_000_000.0))

  private def frameMs(timings: FrameTimings): IO[Option[Double]] =
    IO(timings.drain().phases.get(FramePhase.InputToPaint).map(_.maxMs))

  /** Clears what an earlier step left, runs the trigger, then reads until there is an answer or `timeout` has passed,
    * the trigger's own time counting against it.
    */
  private def answered(
    clear: IO[Unit],
    trigger: IO[Unit],
    read: IO[Option[Double]],
    timeout: FiniteDuration
  ): IO[Option[Double]] =
    def poll(deadline: FiniteDuration): IO[Option[Double]] =
      read.flatMap {
        case some @ Some(_) => IO.pure(some)
        case None =>
          IO.monotonic.flatMap(now => if now >= deadline then IO.pure(None) else IO.sleep(Poll) *> poll(deadline))
      }
    for
      _         <- clear
      started   <- IO.monotonic
      triggered <- trigger.as(true).timeoutTo(timeout, IO.pure(false))
      answer    <- if triggered then poll(started + timeout) else IO.pure(None)
    yield answer
