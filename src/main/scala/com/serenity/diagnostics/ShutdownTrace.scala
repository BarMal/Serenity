package com.serenity.diagnostics

import scala.concurrent.duration.FiniteDuration

import cats.effect.{IO, Ref, Resource}
import org.typelevel.log4cats.Logger

/** Names the release a hung quit is stuck in (#2507): a `[SHUTDOWN] releasing X` line with no `[SHUTDOWN] released X`
  * after it is the step that never finished, so a hang can be read off the log without a thread dump.
  */
object ShutdownTrace:

  private val Tag = "[SHUTDOWN]"

  /** `resource` behaves exactly as before; its own release is bracketed by the two lines. A resource that was never
    * acquired says nothing, since it has nothing to release.
    */
  def traced[A](name: String)(resource: Resource[IO, A])(using logger: Logger[IO]): Resource[IO, A] =
    for
      began    <- Resource.eval(Ref.of[IO, Option[FiniteDuration]](None))
      _        <- Resource.onFinalize(finished(name, began))
      acquired <- resource
      _        <- Resource.onFinalize(starting(name, began))
    yield acquired

  /** An action that belongs to shutdown but is not a resource's release, such as saving the session on the way out. */
  def step[A](name: String)(action: IO[A])(using logger: Logger[IO]): IO[A] =
    Ref.of[IO, Option[FiniteDuration]](None).flatMap { began =>
      (starting(name, began) *> action).guarantee(finished(name, began))
    }

  private def starting(name: String, began: Ref[IO, Option[FiniteDuration]])(using logger: Logger[IO]): IO[Unit] =
    IO.monotonic.flatMap(now => began.set(Some(now))) *> logger.info(s"$Tag releasing $name")

  private def finished(name: String, began: Ref[IO, Option[FiniteDuration]])(using logger: Logger[IO]): IO[Unit] =
    began.get.flatMap {
      case None => IO.unit
      case Some(start) =>
        IO.monotonic.flatMap(now => logger.info(s"$Tag released $name (${(now - start).toMillis} ms)"))
    }
