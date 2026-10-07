package com.serenity.io

import java.nio.file.Path

import cats.effect.{IO, Ref}
import cats.syntax.all.*

/** Remembers each file's [[FileStamp]] as of the last time it was checked, so a change report for a file that has not
  * changed since is recognised as stale. Native watchers can report a write after the settle window that already
  * covered it (Windows delivers size and last-write notifications lazily, once the directory entry is updated), and
  * checking that file again would reload or prompt twice for one burst of writes (#1885).
  *
  * A remembered stamp only stands in for the file's content when it vouched for it as it was taken
  * ([[FileStamp.vouchesForContent]]); otherwise a same-size write within one timestamp tick would be ignored, so the
  * file is checked again.
  */
final class CheckedFileStamps private (
    stamps: Ref[IO, Map[Path, FileStamp.Observed]],
    observe: Path => IO[Option[FileStamp.Observed]]
):

  /** Whether `path` needs checking, claiming it as of now. A file that cannot be read is always worth checking: its
    * absence is itself the change.
    */
  def claimIfChanged(path: Path): IO[Boolean] =
    observe(path).flatMap {
      case None => stamps.update(_ - path).as(true)
      case Some(current) =>
        stamps.modify(known => (known.updated(path, current), !known.get(path).exists(vouchesStillFor(current))))
    }

  /** The values of `candidates` whose file needs checking, claiming each. */
  def claimChanged[A](candidates: List[(Path, A)]): IO[List[A]] =
    candidates.traverseFilter((path, value) => claimIfChanged(path).map(Option.when(_)(value)))

  private def vouchesStillFor(current: FileStamp.Observed)(previous: FileStamp.Observed): Boolean =
    previous.vouches && previous.stamp == current.stamp

object CheckedFileStamps:

  def create: IO[CheckedFileStamps] = create(FileStamp.observe(_))

  private[serenity] def create(observe: Path => IO[Option[FileStamp.Observed]]): IO[CheckedFileStamps] =
    Ref.of[IO, Map[Path, FileStamp.Observed]](Map.empty).map(new CheckedFileStamps(_, observe))
