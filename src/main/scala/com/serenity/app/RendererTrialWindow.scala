package com.serenity.app

import scala.concurrent.duration.FiniteDuration

import cats.effect.IO
import com.serenity.app.PixelClock.{Region, Screen}
import com.serenity.app.RendererWorkload.{Redraw, Step, Window}

/** The window a renderer trial drives (#2618): each step of the workload is a trigger in the real window and a region
  * of the screen to watch, timed by [[PixelClock]]. What the window does and where things land is the [[Driver]]'s;
  * this only pairs them.
  */
object RendererTrialWindow:

  /** What the trial can do to the window, and where each effect shows. */
  trait Driver:
    def typeCharacter(char: Char): IO[Unit]
    def pressPageDown: IO[Unit]
    def toggleTheme: IO[Unit]

    /** Moves the window's edge, alternating between two sizes. */
    def resize: IO[Unit]

    /** The screen region that must change for `step` to have shown. */
    def watchedFor(step: Step): IO[Region]

  def apply(driver: Driver, screen: Screen, timeout: FiniteDuration): Window =
    step =>
      driver
        .watchedFor(step)
        .flatMap(region => PixelClock.timeChange(screen, region, trigger(driver, step), timeout))

  private def trigger(driver: Driver, step: Step): IO[Unit] = step match
    case Step.Type(char)                     => driver.typeCharacter(char)
    case Step.PageDown                       => driver.pressPageDown
    case Step.FullRedraw(Redraw.Resize)      => driver.resize
    case Step.FullRedraw(Redraw.ThemeToggle) => driver.toggleTheme
