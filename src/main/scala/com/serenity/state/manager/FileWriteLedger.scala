package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{IO, Ref}
import com.serenity.io.DocumentRevision

/** This process's writes to one file (#1671). `written` counts completed writes, so a save can tell whether an earlier
  * save on the same lane wrote the file after it captured its buffer's revision.
  */
final private[manager] case class PathWrites(inFlight: Int, written: Long, lastWritten: Option[DocumentRevision]):

  def begun: PathWrites = copy(inFlight = inFlight + 1)

  def wrote(revision: Option[DocumentRevision]): PathWrites =
    copy(written = written + 1, lastWritten = revision.orElse(lastWritten))

  def settled: PathWrites = copy(inFlight = (inFlight - 1).max(0))

  /** The revision a save must find on disk. `capturedRevision` was read from the buffer when the save was submitted,
    * after `writtenAtSubmit` writes; any write since then was an earlier save on this lane, which moved the disk to
    * `lastWritten` before its result reached the buffer.
    */
  def expectedRevision(writtenAtSubmit: Long, capturedRevision: Option[DocumentRevision]): Option[DocumentRevision] =
    if written == writtenAtSubmit then capturedRevision else lastWritten

private[manager] object PathWrites:
  val none: PathWrites = PathWrites(inFlight = 0, written = 0L, lastWritten = None)

/** Tracks saves per canonical path. A save stays in flight until its result has been applied on the dispatcher, so an
  * external-change observation made in between is recognised as this process's own write.
  */
final private[manager] class FileWriteLedger(ref: Ref[IO, Map[Path, PathWrites]]):

  /** Returns the completed-write count the save must later pass to [[expectedRevision]]. */
  def begin(path: Path): IO[Long] =
    ref.modify { writes =>
      val next = writes.getOrElse(path, PathWrites.none).begun
      (writes.updated(path, next), next.written)
    }

  def expectedRevision(
    path: Path,
    writtenAtSubmit: Long,
    captured: Option[DocumentRevision]
  ): IO[Option[DocumentRevision]] =
    ref.get.map(_.getOrElse(path, PathWrites.none).expectedRevision(writtenAtSubmit, captured))

  def wrote(path: Path, revision: Option[DocumentRevision]): IO[Unit] =
    ref.update(writes => writes.updated(path, writes.getOrElse(path, PathWrites.none).wrote(revision)))

  def settle(path: Path): IO[Unit] =
    ref.update(writes => writes.updatedWith(path)(_.map(_.settled)))

  def isSaving(path: Path): IO[Boolean] =
    ref.get.map(_.get(path).exists(_.inFlight > 0))

private[manager] object FileWriteLedger:
  def create: IO[FileWriteLedger] = Ref.of[IO, Map[Path, PathWrites]](Map.empty).map(new FileWriteLedger(_))

  /** Two buffers on one file must share a lane and a ledger entry; resolving symlinks would need a blocking call. */
  def canonical(path: Path): Path = path.toAbsolutePath.normalize
