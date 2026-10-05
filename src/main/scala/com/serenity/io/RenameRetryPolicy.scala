package com.serenity.io

import java.nio.file.{AccessDeniedException, FileSystemException}

import scala.concurrent.duration.*

import cats.effect.IO

/** How long an atomic replace keeps trying when the OS refuses the rename (#2016).
  *
  * On Windows an antivirus scanner or the search indexer can hold the target or the temp file open for a few hundred
  * milliseconds, and the rename then fails with [[AccessDeniedException]] or a sharing-violation
  * [[FileSystemException]]. The hold passes, so retrying with a growing pause lands the save where giving up would lose
  * it. `delays` has one entry per retry, so a policy of `n` delays makes at most `n + 1` attempts.
  */
final case class RenameRetryPolicy(delays: List[FiniteDuration]):

  def attempts: Int = delays.size + 1

  /** A plain [[FileSystemException]] or [[AccessDeniedException]]. The subclasses that name a definite cause (a missing
    * file, a non-empty directory in the way, an unsupported atomic move) will not clear by waiting.
    */
  def retryable(error: Throwable): Boolean =
    error match
      case _: AccessDeniedException   => true
      case other: FileSystemException => other.getClass == classOf[FileSystemException]
      case _                          => false

  /** Runs `attempt`, and again after each delay in turn while it fails with an error `retryable` of its own cause; the
    * last error is returned unchanged once the delays are used up.
    */
  def run[A](attempt: IO[A], causeOf: Throwable => Throwable = identity): IO[A] =
    def loop(remaining: List[FiniteDuration]): IO[A] =
      attempt.handleErrorWith { error =>
        remaining match
          case delay :: rest if retryable(causeOf(error)) => IO.sleep(delay) >> loop(rest)
          case _                                          => IO.raiseError(error)
      }
    loop(delays)

object RenameRetryPolicy:

  /** Each delay twice the one before: `attempts - 1` retries, `initial` apart at first. */
  def exponential(attempts: Int, initial: FiniteDuration): RenameRetryPolicy =
    RenameRetryPolicy(List.iterate(initial, math.max(attempts - 1, 0))(_ * 2))

  /** Five attempts over about half a second: 35 + 70 + 140 + 280 ms of waiting. */
  val default: RenameRetryPolicy = exponential(attempts = 5, initial = 35.millis)

  val none: RenameRetryPolicy = RenameRetryPolicy(Nil)
