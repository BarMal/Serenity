package com.serenity.app

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.apply.*
import cats.syntax.foldable.*
import com.serenity.io.CheckedFileStamps
import com.serenity.state.models.{AppState, BufferId, BufferMapChanges}
import fs2.Stream
import fs2.concurrent.{Signal, SignallingRef}

private[serenity] object AppRuntimeExternalChanges:

  /** Whether a commit may have changed what [[externalChangeWatchLoop]] watches: the open files, the spell-check
    * dictionaries, or the docked explorers. Errs towards yes; the loop re-derives the set and a no-op resync is free.
    */
  private[serenity] def watchInputsChanged(before: AppState, after: AppState): Boolean =
    (before.runtime.uiSurfaces ne after.runtime.uiSurfaces) ||
      before.persisted.config.languageToolsConfig.spellCheck != after.persisted.config.languageToolsConfig.spellCheck ||
      before.persisted.buffers.size != after.persisted.buffers.size ||
      BufferMapChanges.anyChanged(before.persisted.buffers, after.persisted.buffers)(
        added = _ => true,
        changed = _.document.filePath != _.document.filePath
      )

  private enum WatchResync:
    case SetChanged, FocusLost, FocusRegained

  /** Background half of external-change detection (#1623), complementing the focus-in re-check. Watches the parent
    * directories of open local buffers, `dictionaryWatchDirectories` (#1691) and `explorerWatchDirectories`, re-derived
    * on each `watchedSetChanges` element. A changed buffer file gets the same reload-or-prompt check the focus-in path
    * runs; a change under a dictionary directory refreshes the dictionary fingerprints; a changed explorer directory is
    * marked stale.
    *
    * Event-driven (#1938): with something watched the loop blocks in the watcher until a change arrives, and with
    * nothing watched it never calls the watcher at all, waiting for the set to change. Either way it does not wake on a
    * timer. Never calling the genuinely blocking watcher while nothing is watched also keeps a buffer-less startup
    * compatible with virtual-time tests (`VirtualTime.runVirtual`'s `TestControl` treats `IO.blocking` as
    * non-terminating). Changes are gathered for `settle` after the first, so a burst checks each file once (#1885).
    * Spell check is on by default, but `dictionaryWatchDirectories` names only the directories a discovered dictionary
    * lives in, so a machine (or test) with none installed has nothing to watch for it.
    *
    * While `windowFocused` is false nothing is watched, so a polling backend stops listing directories and a window in
    * the background costs nothing. Regaining focus re-registers the watched set and checks every open buffer, the
    * dictionaries and the explorers once, since changes made meanwhile were never observed.
    */
  private[serenity] def externalChangeWatchLoop(
    watcher: com.serenity.io.FileChangeWatcher,
    openBufferPaths: IO[Map[Path, BufferId]],
    checkBufferForExternalChanges: BufferId => IO[Unit],
    dictionaryWatchDirectories: IO[Set[Path]] = IO.pure(Set.empty),
    refreshDictionaryFingerprints: IO[Unit] = IO.unit,
    explorerWatchDirectories: IO[Set[Path]] = IO.pure(Set.empty),
    markExplorerDirectoriesStale: Set[Path] => IO[Unit] = _ => IO.unit,
    watchedSetChanges: Stream[IO, Unit] = Stream.emit(()),
    settle: FiniteDuration = 200.millis,
    windowFocused: Signal[IO, Boolean] = Signal.constant[IO, Boolean](true)
  ): Stream[IO, Unit] =
    val watched = (openBufferPaths, dictionaryWatchDirectories, explorerWatchDirectories).mapN {
      (paths, dictionaryDirectories, explorerDirectories) =>
        paths.keySet.flatMap(path => Option(path.getParent)) ++ dictionaryDirectories ++ explorerDirectories
    }
    val react = (checkedStamps: CheckedFileStamps) =>
      (changed: Set[Path]) =>
        (openBufferPaths, dictionaryWatchDirectories, explorerWatchDirectories).mapN {
          (paths, dictionaryDirectories, explorerDirectories) =>
            val changedDirectories = changed.flatMap(path => Option(path.getParent))
            val staleExplorers     = changedDirectories.intersect(explorerDirectories)
            val buffers            = changed.toList.flatMap(path => paths.get(path).map(path -> _))
            checkedStamps.claimChanged(buffers).flatMap(_.traverse_(checkBufferForExternalChanges)) >>
              IO.whenA(changedDirectories.exists(dictionaryDirectories.contains))(refreshDictionaryFingerprints) >>
              IO.whenA(staleExplorers.nonEmpty)(markExplorerDirectoriesStale(staleExplorers))
        }.flatten
    def syncWatched(watching: SignallingRef[IO, Boolean]) =
      watched.flatMap(directories => watcher.sync(directories) >> watching.set(directories.nonEmpty))
    val recheckEverything = (openBufferPaths, dictionaryWatchDirectories, explorerWatchDirectories).mapN {
      (paths, dictionaryDirectories, explorerDirectories) =>
        paths.values.toList.traverse_(checkBufferForExternalChanges) >>
          IO.whenA(dictionaryDirectories.nonEmpty)(refreshDictionaryFingerprints) >>
          IO.whenA(explorerDirectories.nonEmpty)(markExplorerDirectoriesStale(explorerDirectories))
    }.flatten
    val focusChanges = windowFocused.discrete.changes.zipWithPrevious.collect {
      case (_, false)          => WatchResync.FocusLost
      case (Some(false), true) => WatchResync.FocusRegained
    }
    Stream.eval((SignallingRef.of[IO, Boolean](false), CheckedFileStamps.create).tupled).flatMap { (watching, stamps) =>
      val resync = watchedSetChanges.as(WatchResync.SetChanged).merge(focusChanges).evalMap {
        case WatchResync.SetChanged =>
          windowFocused.get.flatMap(focused => IO.whenA(focused)(syncWatched(watching)))
        case WatchResync.FocusLost     => watcher.sync(Set.empty) >> watching.set(false)
        case WatchResync.FocusRegained => syncWatched(watching) >> recheckEverything
      }
      watching.discrete.changes
        .switchMap(active =>
          if active then Stream.repeatEval(watcher.awaitChangedFiles(settle)).evalMap(react(stamps)) else Stream.empty
        )
        .concurrently(resync)
    }
