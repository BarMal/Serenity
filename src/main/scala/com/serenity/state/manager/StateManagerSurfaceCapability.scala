package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{
  AppEffect,
  ModalStateReducer,
  PanelStateReducer,
  PeekStateReducer,
  PinnedPanelContentReducer,
  ReducerResult,
  UndoEffect
}
import com.serenity.state.undo.HistoryEntry
import com.serenity.ui.layout.*

final private[manager] class StateManagerSurfaceCapability(
    stateRef: cats.effect.Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    operations: StateManagerOperationBoundary,
    recordUndoBoundary: (HistoryEntry, Boolean) => IO[Unit]
):

  private def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    operations.validateAndUpdateState(newState, fallbackState)

  private def applyAnimationHooks(previousState: AppState): IO[Unit] =
    operations.enqueueAnimationHooks(previousState)

  /** Applies whatever a [[PanelStateReducer]] result declared -- #1016 PR4: these calls used to take only `.state` and
    * drop `.effects`, so a reducer that's already the right shape to declare undo had nothing that interpreted it.
    * `PanelStateReducer` only ever emits `Undo`, but this stays a generic fold rather than special-cased to that one
    * effect, so a future effect added there doesn't silently get dropped again.
    */
  private def interpretEffects(effects: List[AppEffect]): IO[Unit] =
    effects.traverse_ {
      case AppEffect.Undo(UndoEffect.RecordBoundary(entry, groupable)) => recordUndoBoundary(entry, groupable)
      case _                                                           => IO.unit
    }

  /** Reads state once, commits the pure result once through the validated path, then runs the shell-side follow-ups
    * the reducer can't express as effects yet: the pipeline's animation hooks and the effects' undo boundaries.
    */
  private def commit(reduce: AppState => ReducerResult, withAnimationHooks: Boolean): IO[Unit] =
    stateRef.get.flatMap { state =>
      val result = reduce(state)
      validateAndUpdateState(result.state, state) >>
        applyAnimationHooks(state).whenA(withAnimationHooks) >>
        interpretEffects(result.effects)
    }

  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] =
    commit(PeekStateReducer.show(content, at, _), withAnimationHooks = false)

  def dismissPeek(): IO[Unit] =
    commit(PeekStateReducer.dismiss, withAnimationHooks = false)

  def peekToPin(position: PanelPosition): IO[Unit] =
    commit(PanelStateReducer.pinPeekOverlay(position, _), withAnimationHooks = true)

  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
    commit(PanelStateReducer.pin(content, position, size, _), withAnimationHooks = true)

  // A target that resolves to no panel (a `ByPosition` side holding nothing pinned, or an `ById` surface that isn't
  // a pinned panel) is a deliberate no-op: the reducer returns the unchanged state we handed it, so callers asking
  // to unpin/expand/focus/resize a panel that isn't there just see nothing happen, the same explicit policy as
  // "target doesn't apply, ignore the request" used elsewhere in this façade (e.g. `checkUnsavedChanges` and
  // `saveBufferAs` no-op when the bufferId doesn't resolve to a buffer).
  /** Updates the existing pinned Terminal panel's content in place rather than pinning a new surface every call, so a
    * project task's periodic output refresh (`StateManagerEffectHandlers.runProjectTask`) doesn't leave behind a fresh
    * panel -- and the user's own workspace-tree focus on it -- every 100ms for the task's whole lifetime (issue #1294).
    */
  def pinOrUpdateTerminalPanel(text: String, position: PanelPosition, size: Int): IO[Unit] =
    commit(PinnedPanelContentReducer.pinOrUpdateTerminal(text, position, size, _), withAnimationHooks = true)

  def unpinPanel(target: PanelTarget): IO[Unit] =
    commit(PanelStateReducer.unpin(target, _), withAnimationHooks = true)

  def movePinnedPanel(surfaceId: SurfaceId, position: PanelPosition): IO[Unit] =
    commit(PanelStateReducer.move(surfaceId, position, _), withAnimationHooks = true)

  def expandPinnedPanel(target: PanelTarget): IO[Unit] =
    commit(PanelStateReducer.expand(target, _), withAnimationHooks = false)

  def collapseExpandedPanel(): IO[Unit] =
    commit(PanelStateReducer.collapseExpandedPanel, withAnimationHooks = true)

  def showModal(modal: Modal): IO[Unit] =
    commit(ModalStateReducer.show(modal, _), withAnimationHooks = false)

  def dismissModal(): IO[Unit] =
    commit(ModalStateReducer.dismiss, withAnimationHooks = false)

  def switchToPinnedPanel(target: PanelTarget): IO[Unit] =
    commit(PanelStateReducer.focus(target, _), withAnimationHooks = false)

  def loadDirectoryTree(rootPath: Path, files: List[String]): IO[Unit] =
    commit(PinnedPanelContentReducer.loadDirectoryTree(rootPath, files, _), withAnimationHooks = true)

  def selectFileInExplorer(targetPath: Path): IO[Unit] =
    commit(PinnedPanelContentReducer.selectFileInExplorer(targetPath, _), withAnimationHooks = false)

  def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit] =
    commit(PanelStateReducer.resize(target, newSize, _), withAnimationHooks = false)

  def dragFileToDirectory(src: Path, targetDir: Path): IO[Unit] =
    IO.blocking(Files.move(src, targetDir.resolve(src.getFileName)))
      .flatMap(_ => commit(PinnedPanelContentReducer.forgetMovedFile(src, _), withAnimationHooks = false))
      .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to move $src to $targetDir"))
