package com.serenity.testkit

import cats.effect.{IO, Ref}
import org.typelevel.log4cats.SelfAwareStructuredLogger

/** Keeps every message a test's code logs, as `"<level> <message>"`, so a spec can assert what was said and in which
  * order without a log4cats testing dependency.
  */
final class RecordingLogger private (recorded: Ref[IO, Vector[String]]) extends SelfAwareStructuredLogger[IO]:

  def messages: IO[Vector[String]] = recorded.get

  private def record(level: String)(message: => String): IO[Unit] = recorded.update(_ :+ s"$level $message")

  def info(message: => String): IO[Unit]                                         = record("info")(message)
  def info(t: Throwable)(message: => String): IO[Unit]                           = record("info")(message)
  def info(ctx: Map[String, String])(message: => String): IO[Unit]               = record("info")(message)
  def info(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit] = record("info")(message)

  def warn(message: => String): IO[Unit]                                         = record("warn")(message)
  def warn(t: Throwable)(message: => String): IO[Unit]                           = record("warn")(message)
  def warn(ctx: Map[String, String])(message: => String): IO[Unit]               = record("warn")(message)
  def warn(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit] = record("warn")(message)

  def error(message: => String): IO[Unit]                                         = record("error")(message)
  def error(t: Throwable)(message: => String): IO[Unit]                           = record("error")(message)
  def error(ctx: Map[String, String])(message: => String): IO[Unit]               = record("error")(message)
  def error(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit] = record("error")(message)

  def debug(message: => String): IO[Unit]                                         = IO.unit
  def debug(t: Throwable)(message: => String): IO[Unit]                           = IO.unit
  def debug(ctx: Map[String, String])(message: => String): IO[Unit]               = IO.unit
  def debug(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit] = IO.unit
  def trace(message: => String): IO[Unit]                                         = IO.unit
  def trace(t: Throwable)(message: => String): IO[Unit]                           = IO.unit
  def trace(ctx: Map[String, String])(message: => String): IO[Unit]               = IO.unit
  def trace(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit] = IO.unit

  def isTraceEnabled: IO[Boolean] = IO.pure(false)
  def isDebugEnabled: IO[Boolean] = IO.pure(false)
  def isInfoEnabled: IO[Boolean]  = IO.pure(true)
  def isWarnEnabled: IO[Boolean]  = IO.pure(true)
  def isErrorEnabled: IO[Boolean] = IO.pure(true)

object RecordingLogger:

  def create: IO[RecordingLogger] = Ref.of[IO, Vector[String]](Vector.empty).map(new RecordingLogger(_))
