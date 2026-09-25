package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.state.reducers.{
  ModalStateReducer,
  PanelStateReducer,
  PeekStateReducer,
  PinnedPanelContentReducer,
  ReducerResult
}
import com.serenity.ui.layout.*

final private[manager] class StateManagerSurfaceCapability(
    logger: org.typelevel.log4cats.Logger[IO],
    operations: StateManagerOperationBoundary,
    modelCommit: ModelCommit
):

  private def applyAnimationHooks(previousState: AppState): IO[Unit] =
    operations.enqueueAnimationHooks(previousState)

  /** Commits the reducer's state together with the undo boundary (and any animation) it declares in one validated model
    * write -- a rejected state records no undo entry -- then queues the pipeline's animation hooks.
    */
  private def commit(reduce: AppState => ReducerResult, withAnimationHooks: Boolean): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      modelCommit.updateValidated(model => Some(EventPipelineTransitions.committed(model, reduce(model.app)))) >>
        applyAnimationHooks(state).whenA(withAnimationHooks)
    }

  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] =
    commit(PeekStateReducer.show(content, at, _), withAnimationHooks = false)

  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
    commit(PanelStateReducer.pin(content, position, size, _), withAnimationHooks = true)

  // A target that resolves to no panel (a `ByPosition` side holding nothing pinned, or an `ById` surface that isn't
  // a pinned panel) is a deliberate no-op: the reducer returns the unchanged state we handed it, so callers asking
  // to unpin/expand/focus/resize a panel that isn't there just see nothing happen, the same explicit policy as
  // "target doesn't apply, ignore the request" used elsewhere in this façade (e.g. `checkUnsavedChanges` and
  // `saveBufferAs` no-op when the bufferId doesn't resolve to a buffer).
  /** Updates the pinned Terminal panel in place rather than pinning a new surface per call, so successive project-task
    * messages don't stack up panels (issue #1294); a running task's output reaches the same reducer through
    * `ProjectTaskTransitions`.
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

  def switchToPinnedPanel(target: PanelTarget): IO[Unit] =
    commit(PanelStateReducer.focus(target, _), withAnimationHooks = false)

  def loadDirectoryTree(rootPath: Path, files: List[String]): IO[Unit] =
    commit(PinnedPanelContentReducer.loadDirectoryTree(rootPath, files, _), withAnimationHooks = true)

  def selectFileInExplorer(targetPath: Path): IO[Unit] =
    commit(PinnedPanelContentReducer.selectFileInExplorer(targetPath, _), withAnimationHooks = false)

  def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit] =
    commit(PanelStateReducer.resize(target, newSize, _), withAnimationHooks = false)

  // On the source file's lane, so the move waits for any save of that file still queued there.
  def dragFileToDirectory(src: Path, targetDir: Path): IO[Unit] =
    operations.submitEffect(
      Lane.Keyed(LaneKey.File(src.toAbsolutePath.normalize), LanePolicy.Sequential),
      IO.blocking(Files.move(src, targetDir.resolve(src.getFileName)))
        .flatMap(_ => operations.dispatch(modelCommit.applyResult(EffectResult.ExplorerFileMoved(src), _ => IO.unit)))
        .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to move $src to $targetDir"))
    )
