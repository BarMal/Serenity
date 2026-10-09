package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.io.FileManager
import com.serenity.state.models.*

/** A buffer's file seen on disk at a revision other than the one the buffer held when it was read (#1623). */
final private[manager] case class ExternalRevisionObservation(
    bufferId: BufferId,
    path: Path,
    bufferRevision: Option[com.serenity.io.DocumentRevision],
    onDisk: com.serenity.io.DocumentRevision
)

final private[manager] class StateManagerExternalChangeEffects(
    currentState: IO[AppState],
    fileManager: FileManager,
    isSaving: Path => IO[Boolean],
    reloadBuffer: BufferId => IO[Unit],
    openReloadConflictModal: (AppState, BufferId, String) => IO[Unit],
    bufferLabelFor: Buffer => String
):

  /** Reads the focused buffer's on-disk revision (#1623), for the window focus-gain re-check. Runs off the dispatcher;
    * the decision is `resolveExternalRevisionEffect`'s.
    */
  private[manager] def observeFocusedExternalRevisionEffect: IO[Option[ExternalRevisionObservation]] =
    currentState.flatMap(_.focusedBufferId.flatTraverse(observeExternalRevisionEffect))

  /** Reads one buffer's on-disk revision (#1623) when it differs from the revision the buffer holds -- the blocking
    * half of the check both the focus-gain callback and `AppRuntime.externalChangeWatchLoop` drive, run off the
    * dispatcher.
    */
  private[manager] def observeExternalRevisionEffect(bufferId: BufferId): IO[Option[ExternalRevisionObservation]] =
    currentState.flatMap { state =>
      state.persisted.buffers.get(bufferId).flatMap(buffer => buffer.document.filePath.map(buffer -> _)) match
        case Some((buffer, path)) =>
          fileManager.revisionSince(path, buffer.document.revision).map {
            case Some(onDisk) if !buffer.document.revision.exists(_.sameContent(onDisk)) =>
              Some(ExternalRevisionObservation(bufferId, path, buffer.document.revision, onDisk))
            case _ => None
          }
        case None => IO.none
    }

  /** Decides an external change on the dispatcher. An observation whose buffer has since been saved, reloaded, closed
    * or re-pathed is stale and dropped: a save's own disk write is not an external change, and the watcher sees the
    * file again on its next poll anyway. A clean buffer is reloaded silently; a dirty one is prompted, exactly like a
    * stale save.
    */
  private[manager] def resolveExternalRevisionEffect(observation: ExternalRevisionObservation): IO[Unit] =
    isSaving(observation.path).ifM(IO.unit, decideExternalRevision(observation))

  private def decideExternalRevision(observation: ExternalRevisionObservation): IO[Unit] =
    currentState.flatMap { state =>
      state.persisted.buffers
        .get(observation.bufferId)
        .filter(buffer =>
          buffer.document.filePath.contains(observation.path) &&
            buffer.document.revision == observation.bufferRevision
        ) match
        case Some(buffer) if buffer.hasUnsavedChanges =>
          // A blocking modal already up (most likely this buffer's own reload-conflict prompt from an earlier poll or
          // focus-gain) must not get a second one stacked on top of it -- code review finding on PR #1664.
          if state.hasBlockingModal then IO.unit
          else openReloadConflictModal(state, buffer.id, bufferLabelFor(buffer))
        case Some(buffer) => reloadBuffer(buffer.id)
        case None         => IO.unit
    }

  /** The paths of every currently open local buffer, for `FileChangeWatcher.sync`'s directory set -- `AppRuntime`'s
    * background watch loop re-derives this whenever a commit may have opened or closed one.
    */
  private[manager] def openBufferPathsEffect: IO[Map[Path, BufferId]] =
    currentState.map(state =>
      state.persisted.buffers.values.flatMap(buffer => buffer.document.filePath.map(_ -> buffer.id)).toMap
    )
