package com.serenity.testkit

import scala.concurrent.duration.*

import cats.effect.IO

/** Waits for work that lands after the call that started it returns -- lane jobs and the results they hand back -- by
  * polling a condition rather than sleeping a fixed time.
  */
object AwaitCondition:

  def awaitValue[A](read: IO[A], timeout: FiniteDuration = 20.seconds)(condition: A => Boolean): IO[A] =
    (IO.sleep(20.millis) >> read).iterateUntil(condition).timeout(timeout)
