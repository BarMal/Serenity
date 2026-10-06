package com.serenity.app.instance

import java.nio.file.{Files, Path}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import cats.effect.{IO, Resource}
import fs2.Stream
import org.typelevel.log4cats.Logger

/** How this launch runs, decided before anything reads or writes the session. */
enum LaunchRole:
  /** Owns the session; files later launches hand over arrive on `forwardedOpens`. An empty list asks for focus. */
  case Primary(forwardedOpens: Stream[IO, List[Path]])

  /** Handed its files to the running instance; nothing more to do. */
  case Forwarded

  /** Another instance owns the session but could not be reached, so this one keeps a session of its own. */
  case Isolated(sessionRoot: Path)

  def sessionRootOverride: Option[Path] =
    this match
      case Isolated(sessionRoot) => Some(sessionRoot)
      case _                     => None

  def notice: Option[String] =
    sessionRootOverride.map { sessionRoot =>
      "Serenity is already running but could not be reached, so this window has a temporary session of its own " +
        s"($sessionRoot). It won't be restored next time, and it leaves your usual session untouched."
    }

final case class InstanceCoordination(
    acquireLock: Resource[IO, LockAttempt],
    serve: Resource[IO, Stream[IO, List[Path]]],
    forward: List[Path] => IO[Delivery],
    isolatedSessionRoot: IO[Path],
    forwardAttempts: Int = 10,
    forwardRetryDelay: FiniteDuration = 200.millis
)

/** One Serenity per config directory (#2023), so two processes never save over each other's session. */
object SingleInstance:

  def forConfigDirectory(configDirectory: Path, logger: Logger[IO]): InstanceCoordination =
    val socket = configDirectory.resolve("instance.sock")
    InstanceCoordination(
      acquireLock = InstanceLock.acquire(configDirectory.resolve("instance.lock")),
      serve = InstanceMessenger.serve(socket, logger),
      forward = paths => InstanceMessenger.forward(socket, paths),
      isolatedSessionRoot = IO.blocking(Files.createTempDirectory("serenity-isolated-session"))
    )

  /** Holds the lock, and the listener, for as long as the returned role is in use. */
  def claim(coordination: InstanceCoordination, paths: List[Path], logger: Logger[IO]): Resource[IO, LaunchRole] =
    coordination.acquireLock.attempt.flatMap {
      case Right(LockAttempt.Acquired)      => servePrimary(coordination, logger)
      case Right(LockAttempt.HeldElsewhere) => Resource.eval(forwardOrIsolate(coordination, paths, logger))
      // Without the lock this launch cannot know it is alone, so it must not take a socket another instance may own.
      case Left(error) =>
        Resource.eval(
          logger
            .warn(error)("[INSTANCE] Could not take the single-instance lock; running without it")
            .as(LaunchRole.Primary(Stream.empty))
        )
    }

  private def servePrimary(coordination: InstanceCoordination, logger: Logger[IO]): Resource[IO, LaunchRole] =
    coordination.serve.attempt.evalMap {
      case Right(forwardedOpens) => IO.pure(LaunchRole.Primary(forwardedOpens))
      case Left(error) =>
        logger
          .warn(error)("[INSTANCE] Could not listen for later launches; they will run on temporary sessions")
          .as(LaunchRole.Primary(Stream.empty))
    }

  private def forwardOrIsolate(
    coordination: InstanceCoordination,
    paths: List[Path],
    logger: Logger[IO]
  ): IO[LaunchRole] =
    forwardPatiently(coordination, paths, coordination.forwardAttempts).flatMap {
      case Delivery.Delivered => IO.pure(LaunchRole.Forwarded)
      case undelivered =>
        coordination.isolatedSessionRoot
          .flatTap { sessionRoot =>
            logger.warn(s"[INSTANCE] Could not reach the running instance ($undelivered); using session $sessionRoot")
          }
          .map(LaunchRole.Isolated(_))
    }

  /** The running instance may hold the lock a moment before it listens. Only `Unreachable` is retried: a request that
    * got through but went unacknowledged might already have opened its files.
    */
  private def forwardPatiently(coordination: InstanceCoordination, paths: List[Path], remaining: Int): IO[Delivery] =
    coordination.forward(paths).flatMap {
      case Delivery.Unreachable if remaining > 1 =>
        IO.sleep(coordination.forwardRetryDelay) >> forwardPatiently(coordination, paths, remaining - 1)
      case delivery => IO.pure(delivery)
    }
