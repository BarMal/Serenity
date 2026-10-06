package com.serenity.io

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import scala.util.Try

import cats.effect.{IO, Ref}
import cats.syntax.all.*

/** Remembers each file's size and modification time as of the last time it was checked, so a change report for a file
  * that has not changed since is recognised as stale. Native watchers can report a write after the settle window that
  * already covered it (Windows delivers size and last-write notifications lazily, once the directory entry is updated),
  * and checking that file again would reload or prompt twice for one burst of writes (#1885).
  */
final class CheckedFileStamps private (stamps: Ref[IO, Map[Path, (Long, Long)]], stamp: Path => Option[(Long, Long)]):

  /** Whether `path` has changed since it was last claimed here, claiming it as of now. A file that cannot be read is
    * always worth checking: its absence is itself the change.
    */
  def claimIfChanged(path: Path): IO[Boolean] =
    IO.blocking(stamp(path)).flatMap {
      case None => stamps.update(_ - path).as(true)
      case Some(current) =>
        stamps.modify(known => (known.updated(path, current), !known.get(path).contains(current)))
    }

  /** The values of `candidates` whose file has changed since it was last claimed, claiming each. */
  def claimChanged[A](candidates: List[(Path, A)]): IO[List[A]] =
    candidates.traverseFilter((path, value) => claimIfChanged(path).map(Option.when(_)(value)))

object CheckedFileStamps:

  def create: IO[CheckedFileStamps] = create(onDisk)

  private[serenity] def create(stamp: Path => Option[(Long, Long)]): IO[CheckedFileStamps] =
    Ref.of[IO, Map[Path, (Long, Long)]](Map.empty).map(new CheckedFileStamps(_, stamp))

  private def onDisk(path: Path): Option[(Long, Long)] =
    Try(Files.size(path) -> Files.getLastModifiedTime(path).to(TimeUnit.NANOSECONDS)).toOption
