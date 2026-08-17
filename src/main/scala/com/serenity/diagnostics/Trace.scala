package com.serenity.diagnostics

import cats.effect.{IO, Outcome}
import org.typelevel.log4cats.Logger

/** Times an `IO` action and logs the elapsed duration at `.debug`, so tracing stays silent in normal runs (per
  * docs/coding-standards.md) and is enabled simply by raising the logger's level -- no dedicated config toggle.
  *
  * Timing and logging happen exactly once per call, regardless of whether the action succeeds, fails, or is canceled,
  * and the original outcome is always preserved untouched.
  */
object Trace:

  def timed[A](label: String)(action: IO[A])(using logger: Logger[IO]): IO[A] =
    IO.realTime.flatMap { start =>
      action.guaranteeCase { outcome =>
        IO.realTime.flatMap { end =>
          val elapsedMs = (end - start).toMillis
          outcome match
            case Outcome.Succeeded(_) =>
              logger.debug(s"[TRACE] $label ${elapsedMs}ms")
            case Outcome.Errored(error) =>
              logger.debug(s"[TRACE] $label ${elapsedMs}ms (failed: ${error.getClass.getSimpleName})")
            case Outcome.Canceled() =>
              logger.debug(s"[TRACE] $label ${elapsedMs}ms (canceled)")
        }
      }
    }
