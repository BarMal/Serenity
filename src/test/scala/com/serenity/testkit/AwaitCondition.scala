package com.serenity.testkit

import scala.concurrent.duration.*

import cats.effect.IO

/** Waits for background work a spec kicked off to land, by condition rather than by elapsed time. */
object AwaitCondition:

  def awaitValue[A](read: IO[A], timeout: FiniteDuration = 10.seconds)(condition: A => Boolean): IO[A] =
    def poll: IO[A] = read.flatMap(value => if condition(value) then IO.pure(value) else IO.sleep(5.millis) >> poll)
    poll.timeoutTo(timeout, IO.raiseError(new AssertionError(s"condition did not hold within $timeout")))
