package com.serenity.diagnostics

import cats.effect.{IO, Outcome}
import org.typelevel.log4cats.Logger

/** Times an `IO` action and logs the elapsed duration at `.debug`, so tracing stays silent in normal runs (per
  * docs/coding-standards.md) and is enabled simply by raising the logger's level -- no dedicated config toggle.
  *
  * Timing and logging happen exactly once per call, regardless of whether the action succeeds, fails, or is canceled,
  * and the original outcome is always preserved untouched.
  *
  * `label` is by-name because this sits on the per-event path. log4cats already takes its message by name, so the
  * interpolation inside this object costs nothing while debug is off -- but a caller writing
  * `Trace.timed(s"event.${event.getClass.getSimpleName}")` would otherwise build that string, reflective class-name
  * lookup included, on every single event regardless of log level. Keeping the parameter by-name pushes that cost
  * behind the same level check as the message itself.
  */
object Trace:

  def timed[A](label: => String)(action: IO[A])(using logger: Logger[IO]): IO[A] =
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
