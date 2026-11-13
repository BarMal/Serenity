package com.serenity.app

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*

/** A clock that reads the screen, for timing how long a change takes to show (#2618).
  *
  * It does not depend on the renderer's own hooks: it looks at the pixels. The cost is its own resolution: a poll
  * cannot say a change showed sooner than one grab takes, so [[resolution]] measures that and the result carries it.
  */
object PixelClock:

  /** A rectangle of the screen in screen pixels. */
  final case class Region(x: Int, y: Int, width: Int, height: Int)

  /** Reads the pixels of a region; what it answers is compared for equality, so a colour is one `Int`. */
  trait Screen:
    def grab(region: Region): IO[Vector[Int]]

  private def millis(duration: FiniteDuration): Double = duration.toNanos / 1_000_000.0

  /** Grabs a baseline, runs `trigger`, then grabs until the region differs from the baseline: the milliseconds from the
    * trigger to the end of that grab, or `None` if `timeout` passes first. A screen that cannot be read fails.
    */
  def timeChange(screen: Screen, region: Region, trigger: IO[Unit], timeout: FiniteDuration): IO[Option[Double]] =
    for
      baseline <- screen.grab(region)
      started  <- IO.monotonic
      _        <- trigger
      shown    <- poll(screen, region, baseline, started + timeout)
      answered <- IO.monotonic
    yield Option.when(shown)(millis(answered - started))

  private def poll(screen: Screen, region: Region, baseline: Vector[Int], deadline: FiniteDuration): IO[Boolean] =
    screen.grab(region).flatMap { pixels =>
      if pixels != baseline then IO.pure(true)
      else
        IO.monotonic
          .flatMap(now => if now >= deadline then IO.pure(false) else poll(screen, region, baseline, deadline))
    }

  /** The quickest of `samples` grabs, in milliseconds: the least a change can be late by in this clock. */
  def resolution(screen: Screen, region: Region, samples: Int): IO[Double] =
    List
      .fill(samples)(for
        started  <- IO.monotonic
        _        <- screen.grab(region)
        finished <- IO.monotonic
      yield millis(finished - started))
      .sequence
      .map(_.minOption.getOrElse(0.0))
