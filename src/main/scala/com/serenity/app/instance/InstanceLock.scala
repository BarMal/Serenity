package com.serenity.app.instance

import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{Files, Path, StandardOpenOption}

import cats.effect.{IO, Resource}

enum LockAttempt:
  case Acquired
  case HeldElsewhere

/** The lock that makes one Serenity process the owner of a config directory's session (#2023).
  *
  * An OS file lock rather than the lock file's existence: the OS drops it when its process dies, however it dies, so a
  * crash never leaves a stale lock behind -- only the empty file, which means nothing.
  */
object InstanceLock:

  def acquire(lockFile: Path): Resource[IO, LockAttempt] =
    open(lockFile)
      .flatMap(channel => Resource.make(tryLock(channel))(_.fold(IO.unit)(lock => IO.blocking(lock.release()))))
      .map(_.fold(LockAttempt.HeldElsewhere)(_ => LockAttempt.Acquired))

  private def open(lockFile: Path): Resource[IO, FileChannel] =
    Resource.fromAutoCloseable(IO.blocking {
      val _ = Files.createDirectories(lockFile.getParent)
      FileChannel.open(lockFile, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    })

  /** `tryLock` answers null for a lock held by another process, and throws for one held elsewhere in this JVM. */
  private def tryLock(channel: FileChannel): IO[Option[FileLock]] =
    IO.blocking(Option(channel.tryLock())).recover { case _: OverlappingFileLockException => None }
