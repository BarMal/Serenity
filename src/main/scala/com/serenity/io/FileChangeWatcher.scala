package com.serenity.io

import java.io.IOException
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY}
import java.nio.file.{FileSystems, Files, Path, WatchKey}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*

/** A directory watcher (#1623): the background half of external-change detection, complementing the focus-in re-check
  * `StateManagerEffectHandlers.observeFocusedExternalRevisionEffect` already does. Directories rather than files are
  * watched, so `sync` takes the set of directories the caller currently cares about (an open local buffer's parent) and
  * the watcher reports which specific files inside them changed.
  *
  * Backed by the platform `WatchService`, which blocks until the OS reports a change; where the default filesystem
  * offers none -- or only the JDK's own slow poller, as on macOS -- [[FileChangeWatcher.polling]] compares directory
  * listings on an interval instead (#1885).
  */
final class FileChangeWatcher private (backend: FileChangeWatcher.Backend):

  /** Watches exactly `directories` from now on: registers the new ones and drops the rest, so watches track buffers
    * opening and closing without leaking registrations for closed ones.
    */
  def sync(directories: Set[Path]): IO[Unit] = backend.sync(directories)

  /** The files that changed within `timeout`, or nothing. */
  def pollChangedFiles(timeout: FiniteDuration): IO[Set[Path]] = backend.poll(timeout)

  /** Waits, without waking, until a watched file changes, then keeps collecting until `settle` passes with nothing new,
    * so a burst of writes to one file -- an editor saving in several steps, a checkout -- arrives as one report. A file
    * that never stops changing is reported after [[FileChangeWatcher.MaxSettleRounds]] rounds regardless.
    */
  def awaitChangedFiles(settle: FiniteDuration): IO[Set[Path]] =
    backend.await.flatMap(settled(settle, _, FileChangeWatcher.MaxSettleRounds))

  private def settled(settle: FiniteDuration, seen: Set[Path], roundsLeft: Int): IO[Set[Path]] =
    IO.sleep(settle) >> backend.ready.flatMap { more =>
      if more.isEmpty || roundsLeft <= 1 then IO.pure(seen ++ more)
      else settled(settle, seen ++ more, roundsLeft - 1)
    }

object FileChangeWatcher:

  val MaxSettleRounds: Int = 5

  final private[io] case class Backend(
      sync: Set[Path] => IO[Unit],
      poll: FiniteDuration => IO[Set[Path]],
      await: IO[Set[Path]],
      ready: IO[Set[Path]]
  )

  /** The platform watcher, or [[polling]] where the default filesystem has no `WatchService` or only the JDK's built-in
    * polling one.
    */
  def create: Resource[IO, FileChangeWatcher] =
    Resource
      .make(IO.blocking(FileSystems.getDefault.newWatchService()).attempt) {
        case Right(service) => IO.blocking(service.close()).attempt.void
        case Left(_)        => IO.unit
      }
      .flatMap {
        case Right(service) if isJdkPollingService(service.getClass.getName) =>
          Resource.eval(IO.blocking(service.close()).attempt) >> polling(DefaultPollInterval)
        case Right(service)                                          => Resource.eval(native(service))
        case Left(_: UnsupportedOperationException | _: IOException) => polling(DefaultPollInterval)
        case Left(error) => Resource.raiseError[IO, FileChangeWatcher, Throwable](error)
      }

  /** Short because this is the macOS path too: the JDK's own poller there checks every 10 seconds, far too slow for an
    * external edit to show up promptly. One listing per watched directory per second is cheap, and nothing runs while
    * nothing is watched.
    */
  val DefaultPollInterval: FiniteDuration = 1.second

  /** The JDK falls back to `sun.nio.fs.PollingWatchService` where the OS offers no native watching (macOS); it re-scans
    * only every ~10 seconds, so [[polling]] serves better. Matched by class name to avoid reaching into JDK internals.
    */
  private[io] def isJdkPollingService(serviceClassName: String): Boolean =
    serviceClassName == "sun.nio.fs.PollingWatchService"

  /** Compares listings of the watched directories every `interval`. Only wakes while something is watched. */
  def polling(interval: FiniteDuration): Resource[IO, FileChangeWatcher] =
    pollingListings(interval, directory => IO.blocking(list(directory)))

  /** [[polling]] over `listDirectory`, which maps each entry of a directory to its size and modification time. */
  private[serenity] def pollingListings(
    interval: FiniteDuration,
    listDirectory: Path => IO[Map[Path, (Long, Long)]]
  ): Resource[IO, FileChangeWatcher] =
    Resource.eval(Ref.of[IO, Map[Path, Listing]](Map.empty).map { listings =>
      val changes = rescan(listings, listDirectory)
      val await   = (IO.sleep(interval) >> changes).iterateUntil(_.nonEmpty)
      new FileChangeWatcher(
        Backend(
          sync = directories => syncListings(listings, directories, listDirectory),
          poll = timeout => await.timeoutTo(timeout, IO.pure(Set.empty)),
          await = await,
          ready = changes
        )
      )
    })

  private def native(service: java.nio.file.WatchService): IO[FileChangeWatcher] =
    Ref.of[IO, Map[Path, WatchKey]](Map.empty).map { registrations =>
      new FileChangeWatcher(
        Backend(
          sync = directories => syncKeys(service, registrations, directories),
          poll = timeout =>
            IO.blocking(Option(service.poll(timeout.length, timeout.unit))).flatMap {
              case None      => IO.pure(Set.empty)
              case Some(key) => (drainKey(key), drainReadyKeys(service)).mapN(_ ++ _)
            },
          // Interruptible so the loop can stop waiting when the watched set empties or the editor quits.
          await =
            IO.interruptible(service.take()).flatMap(key => (drainKey(key), drainReadyKeys(service)).mapN(_ ++ _)),
          ready = drainReadyKeys(service)
        )
      )
    }

  private def syncKeys(
    service: java.nio.file.WatchService,
    registrations: Ref[IO, Map[Path, WatchKey]],
    directories: Set[Path]
  ): IO[Unit] =
    registrations.get.flatMap { current =>
      val toAdd    = directories -- current.keySet
      val toRemove = current.keySet -- directories
      for
        added <- toAdd.toList.traverse(directory =>
          IO.blocking(directory.register(service, ENTRY_MODIFY, ENTRY_CREATE, ENTRY_DELETE))
            .attempt
            .map(_.toOption.map(directory -> _))
        )
        _ <- toRemove.toList.traverse_(directory => IO.blocking(current(directory).cancel()).attempt.void)
        _ <- registrations.set((current -- toRemove) ++ added.flatten)
      yield ()
    }

  private def drainReadyKeys(service: java.nio.file.WatchService): IO[Set[Path]] =
    IO.blocking(Option(service.poll())).flatMap {
      case None      => IO.pure(Set.empty)
      case Some(key) => (drainKey(key), drainReadyKeys(service)).mapN(_ ++ _)
    }

  // A key is reset after being drained so it keeps reporting: an unreset key stops queuing events for its directory.
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

  /** Each entry's size and modification time, which change when the file is written. */
  private type Listing = Map[Path, (Long, Long)]

  private def syncListings(
    listings: Ref[IO, Map[Path, Listing]],
    directories: Set[Path],
    listDirectory: Path => IO[Listing]
  ): IO[Unit] =
    listings.get.flatMap { current =>
      (directories -- current.keySet).toList
        .traverse(directory => listDirectory(directory).map(directory -> _))
        .flatMap(added => listings.set(current.view.filterKeys(directories.contains).toMap ++ added))
    }

  private def rescan(listings: Ref[IO, Map[Path, Listing]], listDirectory: Path => IO[Listing]): IO[Set[Path]] =
    listings.get.flatMap { previous =>
      previous.keys.toList.traverse(directory => listDirectory(directory).map(directory -> _)).flatMap { now =>
        val changed = now.flatMap { (directory, listing) =>
          val before = previous.getOrElse(directory, Map.empty)
          (before.keySet ++ listing.keySet).filter(path => before.get(path) != listing.get(path))
        }.toSet
        listings.set(now.toMap).as(changed)
      }
    }

  // A directory that cannot be listed (deleted, unmounted) reads as empty, so its files report as removed; an entry
  // gone between listing and stat is simply absent.
  private def list(directory: Path): Listing =
    Using(Files.list(directory)) { entries =>
      entries.iterator.asScala.flatMap { path =>
        Try(path -> (Files.size(path), Files.getLastModifiedTime(path).to(TimeUnit.NANOSECONDS))).toOption
      }.toMap
    }.getOrElse(Map.empty)
