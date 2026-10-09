package com.serenity.io

import java.util.concurrent.{CompletableFuture, TimeUnit, TimeoutException}

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/** Runs a body on the AppKit main thread from another thread and waits for its result.
  *
  * `schedule` queues a `Runnable` on the main run loop and returns the function that withdraws it. The wait has two
  * parts. Getting started is bounded by `startTimeout`: if the main run loop never picks the work up, the caller gets a
  * failure instead of waiting forever, and the work is withdrawn. Once the main thread has begun, the wait is
  * unbounded, because a modal panel stays up as long as the user keeps it.
  *
  * The two sides race for one claim, a [[CompletableFuture]] completed by whichever gets there first: the main thread
  * runs the body only if it won, and a caller that has timed out has won it, so the body can never run afterwards.
  */
private[io] object MainThreadHandoff:

  final class MainThreadUnavailable(timeout: FiniteDuration)
      extends RuntimeException(s"The AppKit main thread did not start the panel within $timeout")

  def run[A](schedule: Runnable => () => Unit, startTimeout: FiniteDuration)(body: => A): Either[Throwable, A] =
    val started = new CompletableFuture[Boolean]()
    val outcome = new CompletableFuture[Either[Throwable, A]]()
    val task: Runnable = () =>
      if started.complete(true) then
        val _ = outcome.complete(attempt(body))
    attempt(schedule(task)).flatMap { withdraw =>
      try
        if mainThreadStarted(started, startTimeout) then outcome.get()
        else Left(new MainThreadUnavailable(startTimeout))
      finally
        val _ = attempt(withdraw())
    }

  private def mainThreadStarted(started: CompletableFuture[Boolean], timeout: FiniteDuration): Boolean =
    try started.get(timeout.toNanos, TimeUnit.NANOSECONDS)
    catch case _: TimeoutException => !started.complete(false)

  private def attempt[A](body: => A): Either[Throwable, A] =
    try Right(body)
    catch
      case NonFatal(failure)     => Left(failure)
      case failure: LinkageError => Left(failure)
