package com.serenity.io

import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_MODIFY}
import java.nio.file.{FileSystems, Path, WatchKey}

import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*

/** A real `java.nio.file.WatchService`-backed directory watcher (#1623): the background half of external-change
  * detection, complementing the focus-in re-check `StateManagerEffectHandlers.observeFocusedExternalRevisionEffect`
  * already does. `WatchService` only watches directories, not individual files, so `sync` takes the set of directories
  * the caller currently cares about (an open local buffer's parent) rather than individual file paths;
  * `pollChangedFiles` reports which specific files inside those directories a poll window actually saw change.
  */
final class FileChangeWatcher private (
    watchService: java.nio.file.WatchService,
    registrations: Ref[IO, Map[Path, WatchKey]]
):

  /** Registers every directory in `directories` that isn't already watched, and cancels the watch key for every
    * currently-watched directory no longer in the set -- called on each poll cycle with the current open-buffer
    * directory set, so watches track buffers opening and closing without leaking registrations for closed buffers.
    */
  def sync(directories: Set[Path]): IO[Unit] =
    registrations.get.flatMap { current =>
      val toAdd    = directories -- current.keySet
      val toRemove = current.keySet -- directories
      for
        added <- toAdd.toList.traverse(directory =>
          IO.blocking(
            directory.register(watchService, ENTRY_MODIFY, ENTRY_CREATE)
          ).attempt
            .map(_.toOption.map(directory -> _))
        )
        _ <- toRemove.toList.traverse_(directory => IO.blocking(current(directory).cancel()).attempt.void)
        _ <- registrations.set((current -- toRemove) ++ added.flatten)
      yield ()
    }

  /** Polls for filesystem events up to `timeout`, resolving each event's directory-relative filename against its
    * `WatchKey`'s own watched directory to report the absolute path that changed. A key is reset after being drained so
    * it keeps reporting later changes rather than only firing once (the JDK `WatchService` contract: an unreset key
    * stops queuing new events for its directory).
    */
  def pollChangedFiles(timeout: FiniteDuration): IO[Set[Path]] =
    for
      firstKey <- IO.blocking(Option(watchService.poll(timeout.length, timeout.unit)))
      changed <- firstKey match
        case None => IO.pure(Set.empty[Path])
        case Some(key) =>
          for
            first <- drainKey(key)
            rest  <- drainReadyKeys
          yield first ++ rest
    yield changed

  private def drainReadyKeys: IO[Set[Path]] =
    IO.blocking(Option(watchService.poll())).flatMap {
      case None => IO.pure(Set.empty)
      case Some(key) =>
        for
          fromKey <- drainKey(key)
          rest    <- drainReadyKeys
        yield fromKey ++ rest
    }

  private def drainKey(key: WatchKey): IO[Set[Path]] =
    IO.blocking {
      val changed = key.watchable() match
        case directory: Path =>
          key
            .pollEvents()
            .asScala
            .flatMap(event => Option(event.context()).collect { case name: Path => directory.resolve(name) })
            .toSet
        case _ => Set.empty[Path]
      key.reset()
      changed
    }

  private[io] def close: IO[Unit] =
    IO.blocking(watchService.close()).attempt.void

object FileChangeWatcher:

  def create: Resource[IO, FileChangeWatcher] =
    Resource.make(
      for
        service       <- IO.blocking(FileSystems.getDefault.newWatchService())
        registrations <- Ref.of[IO, Map[Path, WatchKey]](Map.empty)
      yield new FileChangeWatcher(service, registrations)
    )(_.close)
