package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.{Deferred, IO}
import com.serenity.io.FileManager
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.state.core.EditorState
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import org.typelevel.log4cats.Logger

/** The file, close and session workflows. Their decisions are the pure [[CloseWorkflowTransitions]],
  * [[FileWorkflowTransitions]] and [[SessionWorkflowTransitions]]; each step here commits its result once, validated.
  */
final private[manager] class StateManagerWorkflowCapability(
    modelCommit: ModelCommit,
    quitSignal: Deferred[IO, Unit],
    logger: Logger[IO],
    fileDialog: Option[com.serenity.io.FileDialog],
    fileManager: FileManager,
    sessionPersistence: SessionPersistence,
    sessionManager: SessionManager,
    operations: StateManagerOperationBoundary,
    lanes: EffectLanePort,
    filePersistence: StateManagerFilePersistence
)(using balance: com.serenity.rope.Balance):
  import StateManagerWorkflowCapability.SessionLane

  private val close = new CloseWorkflowTransitions(operations.ensureCommandRunnerSurface)

  private def commit(transition: AppState => AppState): IO[Unit] =
    modelCommit.currentState.flatMap(current => modelCommit.commitState(transition(current), current))

  private val fileWorkflow = new StateManagerFileWorkflow(
    modelCommit.currentState,
    logger,
    fileManager,
    modelCommit.commitState,
    lanes,
    filePersistence.openFile,
    filePersistence.inspectBeforeSave,
    filePersistence.saveBufferAs,
    continueCloseAfterFormSaveAs
  )

  private val replaceWorkflow = new StateManagerReplaceWorkflow(modelCommit.updateValidated)

  /** Opens the reload/overwrite/cancel prompt (#1623) for `bufferId` -- either because a save just discovered the
    * on-disk file changed since it was opened, or because a focus-in re-check found the same thing.
    */
  private[manager] def openReloadConflictModal(state: AppState, bufferId: BufferId, bufferLabel: String): IO[Unit] =
    val modalState =
      ModalStateReducer.show(Modal.ReloadConflict(ReloadConflictState(bufferId, bufferLabel)), state).state
    modelCommit.commitState(modalState, state)

  private def reloadConflictSurface(state: AppState, surfaceId: SurfaceId): Option[ReloadConflictState] =
    state.runtime.modalStack.find(_.id == surfaceId).collect {
      case ModalDialog(_, Modal.ReloadConflict(workflow), _) => workflow
    }

  private[manager] def submitReloadConflictEffect(surfaceId: SurfaceId): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      reloadConflictSurface(state, surfaceId) match
        case Some(workflow) =>
          val dismiss = commit(_.dismissTopModal)
          workflow.selectedChoice match
            case ReloadConflictChoice.Cancel    => dismiss
            case ReloadConflictChoice.Reload    => filePersistence.reloadBuffer(workflow.bufferId) >> dismiss
            case ReloadConflictChoice.Overwrite => filePersistence.forceSaveExistingBuffer(workflow.bufferId) >> dismiss
        case None => IO.unit
    }

  private[manager] def openFileWorkflowModal(
    mode: FileWorkflowMode,
    state: AppState,
    bufferIdOverride: Option[BufferId] = None,
    statusMessage: Option[String] = None
  ): IO[Unit] =
    fileWorkflow.openFileWorkflowModal(mode, state, bufferIdOverride, statusMessage)

  private[manager] def showSaveAsWorkflow(state: AppState, bufferId: BufferId, statusMessage: String): IO[Unit] =
    openFileWorkflowModal(FileWorkflowMode.SaveAs, state, Some(bufferId), Some(statusMessage))

  private[manager] def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
    filePersistence.settlePendingSaves(close.closeTargets(scope, state)) >>
      modelCommit.currentState.flatMap(current => commitClose(current, close.begun(scope, current)))

  /** Commits a close step, then -- if it resolved the last buffer -- quits or shows the start page. */
  private def commitClose(fallback: AppState, transition: CloseTransition): IO[Unit] =
    modelCommit.commitState(transition.state, fallback) >>
      transition.completed.fold(IO.unit)(scope => modelCommit.currentState.flatMap(finishCloseScope(scope, _)))

  /** The terminal step once every buffer a close action targets has been resolved: Quit persists and quits;
    * ReturnToStartPage snapshots the session and swaps the editor for a freshly-built start page; the rest do nothing.
    */
  private def finishCloseScope(scope: CloseScope, committed: AppState): IO[Unit] =
    scope match
      case CloseScope.Quit =>
        sessionPersistence.onAppClose(committed) >> quitSignal.complete(()).attempt.void
      case CloseScope.ReturnToStartPage =>
        snapshotAndShowStartPage(committed)
      case _ =>
        IO.unit

  /** Persist the current session (unsaved buffers included, so [Tab] Quick-resume restores them) and replace the editor
    * with a start page that offers to resume it. Runtime chrome (theme, viewport, terminal/GUI mode, keyboard tier)
    * carries over from the committed editor state so the splash matches the environment it came from.
    */
  private def snapshotAndShowStartPage(committed: AppState): IO[Unit] =
    sessionManager.saveSession(committed, persistUnsavedBuffers = true) >>
      IO.blocking(
        committed.persisted.recentFiles.filter(path => Files.isRegularFile(path) && Files.isReadable(path))
      ).flatMap { readableRecentFiles =>
        val page = StartupPageContent.createStartPage(
          sessionExists = true,
          recentFiles = readableRecentFiles,
          resumeIdentifier = Some(StartupPageContent.sessionResumeIdentifier(committed))
        )
        modelCommit.commitState(startPageStateFrom(committed, page), committed)
      }

  private def startPageStateFrom(committed: AppState, page: StartupPage): AppState =
    val startPageSurfaceId = SurfaceId("surface-0")
    val base               = AppState.empty(committed.persisted.config)
    base.copy(
      persisted = base.persisted.copy(
        focus = Focus.Surface(startPageSurfaceId),
        theme = committed.persisted.theme
      ),
      runtime = base.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            id = startPageSurfaceId,
            content = SurfaceContent.StartPage(page),
            presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        ),
        viewportSize = committed.runtime.viewportSize,
        nextSurfaceId = 1,
        isTuiMode = committed.runtime.isTuiMode,
        keyboardFidelityTier = committed.runtime.keyboardFidelityTier
      )
    )

  private[manager] def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      close.closePrompt(state, surfaceId) match
        case Some(workflow) =>
          workflow.selectedChoice match
            case CloseWorkflowChoice.Cancel =>
              modelCommit.commitState(close.abandoned(surfaceId, workflow, state), state)
            case CloseWorkflowChoice.Discard =>
              commitClose(state, close.resolved(workflow, state))
            case CloseWorkflowChoice.Save =>
              state.persisted.buffers.get(workflow.currentBufferId) match
                case Some(buffer) if buffer.document.filePath.isDefined =>
                  saveBeforeClose(surfaceId, workflow)
                case Some(_) =>
                  requestSaveAsFileDialog(state, Some(workflow.currentBufferId))
                case None =>
                  modelCommit.commitState(close.clearCloseActions(close.dismissModalSurface(state)), state)
        case None =>
          IO.unit
    }

  /** Closes the buffer only once its save has landed and left it clean (#1708). A failed or conflicting save abandons
    * the whole close -- a quit or close-all stops at this buffer -- rather than dropping the edits it could not write.
    */
  private def saveBeforeClose(surfaceId: SurfaceId, workflow: CloseWorkflowState): IO[Unit] =
    val bufferId = workflow.currentBufferId
    filePersistence.saveExistingBuffer(bufferId).attempt.flatMap {
      case Right(()) =>
        modelCommit.currentState.flatMap { saved =>
          if saved.persisted.buffers.get(bufferId).exists(!_.hasUnsavedChanges) then
            commitClose(saved, close.resolved(workflow, saved))
          else modelCommit.commitState(close.abandoned(surfaceId, workflow, saved), saved)
        }
      case Left(error: com.serenity.richtext.LossyRichTextOverwriteException) =>
        // The Save-As form opens over the prompt and resumes this close once it saves (continueCloseAfterFormSaveAs).
        modelCommit.currentState.flatMap(current => showSaveAsWorkflow(current, bufferId, error.getMessage))
      case Left(_: com.serenity.io.FileManagerError.ExternalConflict) =>
        commit(close.conflicted(surfaceId, workflow, _))
      case Left(error) =>
        logger.error(error)(s"[FILE] Failed to save buffer $bufferId before closing it") >>
          commit(close.abandoned(surfaceId, workflow, _))
    }

  private[manager] def clearCloseActions(state: AppState): AppState = close.clearCloseActions(state)

  private[manager] def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    fileWorkflow.refreshFileWorkflowEffect(surfaceId)

  private[manager] def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    fileWorkflow.submitFileWorkflowEffect(surfaceId)

  /** Opens the directory an Open dialog is targeting as a project root (issue #1525). `openProjectRoot` is the caller's
    * route to the panel-pinning machinery (`StateManagerPanelEffects`, owned by `StateManagerEffectHandlers`), which
    * this capability has no access to.
    */
  private[manager] def openFileWorkflowAsProjectRootEffect(
    surfaceId: SurfaceId,
    openProjectRoot: Path => IO[Unit]
  ): IO[Unit] =
    fileWorkflow.openAsProjectRoot(surfaceId, openProjectRoot)

  private[manager] def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    replaceWorkflow.submitReplaceWorkflowEffect(surfaceId)

  /** The explicit, single-step counterpart to submitting twice (Enter to flag `missingPathSegments`, Enter again to
    * confirm): creates the missing directories -- as a side effect of performing the save itself, exactly like the
    * confirmed double-submit path -- immediately, without a second submit (issue #1253). The directories are created by
    * that save's write, on the target's `File` sequential lane, so they are ordered with every other write to it.
    */
  private[manager] def createFileWorkflowDirectoriesEffect(surfaceId: SurfaceId): IO[Unit] =
    fileWorkflow.createFileWorkflowDirectoriesEffect(surfaceId)

  /** What follows a successful in-app Save-As: resume the close workflow this save was the "Save before close" step of,
    * or -- when no close workflow is waiting on this buffer -- just dismiss the dialog.
    */
  private def continueCloseAfterFormSaveAs(surfaceId: SurfaceId, bufferId: BufferId): IO[Unit] =
    modelCommit.currentState.flatMap { saved =>
      close.pendingOn(saved, bufferId) match
        case Some(closeWorkflow) => commitClose(saved, close.resolvedBySaveAs(closeWorkflow, saved))
        case None                => modelCommit.commitState(WorkflowSurfaces.dismissedToEditor(saved, surfaceId), saved)
    }

  private[manager] def requestSaveAsFileDialog(state: AppState, bufferIdOverride: Option[BufferId]): IO[Unit] =
    bufferIdOverride.orElse(state.focusedBufferId) match
      case Some(bufferId) =>
        fileDialog match
          case Some(dialog) =>
            val focusedPath = state.persisted.buffers.get(bufferId).flatMap(_.document.filePath)
            val initialDirectory =
              focusedPath
                .flatMap(path => Option(path.getParent).map(IO.pure))
                .getOrElse(com.serenity.io.FileUtils.getCurrentDirectory)
            val suggestedFileName = focusedPath.flatMap(path => Option(path.getFileName).map(_.toString))

            initialDirectory
              .flatMap(directory => dialog.chooseSaveFile(Some(directory), suggestedFileName))
              .flatMap {
                case Some(path) =>
                  filePersistence.saveBufferAs(bufferId, path) >> continueCloseAfterNativeSaveAs(bufferId)
                case None =>
                  IO.unit
              }
              .handleErrorWith(ex => logger.error(ex)(s"[FILE] Native save-as dialog failed for buffer $bufferId"))
          case None =>
            // No native dialog to show at all -- the in-app form is the only way to collect a path, not a fallback
            // for a dialog the user might have cancelled (that case stays a no-op above, via chooseSaveFile's None).
            openFileWorkflowModal(FileWorkflowMode.SaveAs, state, Some(bufferId))
      case None =>
        logger.debug("[FILE] Save As requested without a focused buffer")

  private def continueCloseAfterNativeSaveAs(bufferId: BufferId): IO[Unit] =
    modelCommit.currentState.flatMap(saved =>
      close
        .pendingOn(saved, bufferId)
        .fold(IO.unit)(workflow => commitClose(saved, close.resolvedBySaveAs(workflow, saved)))
    )

  private[manager] def activeEditorBufferId(state: AppState): Option[BufferId] =
    WorkflowSurfaces.activeEditorBufferId(state)

  /** Every branch commits its whole result once, validated: a corrupted or hand-edited session file, restored on
    * literal app startup, must go through the same check as any other commit (#858).
    */
  private[manager] def restoreStartupSession(): IO[Unit] =
    logger.info("[CMD] Session restore requested") >>
      sessionManager.loadSession().flatMap {
        case Some(restoredState) if restoredState.persisted.bufferOrder.nonEmpty =>
          logger.info("[CMD] Session loaded successfully") >>
            commit(SessionWorkflowTransitions.restoredIntoViewport(restoredState, _))
        case Some(_) =>
          logger.info("[CMD] Session loaded with no buffers - creating default session") >>
            commit(SessionWorkflowTransitions.withDefaultStartupBuffer)
        case None =>
          logger.info("[CMD] No session found - creating default session") >>
            commit(SessionWorkflowTransitions.withDefaultStartupBuffer)
      }

  private[manager] def createStartupSession(): IO[Unit] =
    commit { before =>
      val opened = EditorState.openNewTab(before)
      opened.copy(runtime = opened.runtime.copy(uiSurfaces = List.empty))
    }

  /** Opens the "Save Session As..." name prompt (issue #1390), pre-filled empty -- `ModalSessionReducer` routes its
    * Enter into `submitSessionNamePromptEffect` below. Shown on the current state, so the palette's record of the
    * command that opened it is kept.
    */
  private[manager] def openSaveSessionAsPrompt(): IO[Unit] =
    commit(ModalStateReducer.show(Modal.SessionNamePrompt(SessionNamePromptMode.SaveAs, ""), _).state)

  /** Lists the saved sessions on the Session lane, then opens the picker (issue #1390) for either purpose: `Open` loads
    * the selected session directly on Enter, `Rename` hands it off to the name prompt.
    */
  private[manager] def openSessionPicker(purpose: SessionListPurpose): IO[Unit] =
    lanes.submitEffect(
      SessionLane,
      sessionManager
        .listSessions()
        .flatMap(sessions => lanes.dispatchEffectResult(EffectResult.SessionsListed(purpose, sessions), _ => IO.unit))
    )

  /** Completes the name prompt: `saveSessionAs` for a brand-new named session, `renameSession` for one already picked
    * from the list. The prompt closes at once; the write runs on the Session lane. A blank (post-trim) name is treated
    * as a cancel, matching `ModalSessionReducer`'s own guard on `ModalSubmit`.
    */
  private[manager] def submitSessionNamePromptEffect(surfaceId: SurfaceId): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      val dismissed = WorkflowSurfaces.dismissedToEditor(state, surfaceId)
      val write = SessionWorkflowTransitions.sessionNamePrompt(state, surfaceId) match
        case Some((SessionNamePromptMode.SaveAs, input)) if input.trim.nonEmpty =>
          Some(sessionManager.saveSessionAs(input.trim, dismissed).void)
        case Some((SessionNamePromptMode.Rename(sessionId), input)) if input.trim.nonEmpty =>
          Some(sessionManager.renameSession(sessionId, input.trim))
        case _ =>
          None
      modelCommit.commitState(dismissed, state) >> write.fold(IO.unit)(lanes.submitEffect(SessionLane, _))
    }

  /** Completes an `Open`-purpose `SessionList` selection: the picked session loads on the Session lane and replaces the
    * current one, as `SessionIntent.RestoreSession` does, if the picker is still open when it arrives.
    */
  private[manager] def submitSessionListEffect(surfaceId: SurfaceId): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      SessionWorkflowTransitions
        .sessionPicker(state, surfaceId)
        .collect { case (sessions, selectedIndex, SessionListPurpose.Open) => sessions.lift(selectedIndex) }
        .flatten match
        case Some(session) =>
          lanes.submitEffect(
            SessionLane,
            sessionManager
              .loadSession(session.id)
              .flatMap(restored =>
                lanes.dispatchEffectResult(EffectResult.NamedSessionLoaded(surfaceId, restored), _ => IO.unit)
              )
          )
        case None =>
          modelCommit.commitState(WorkflowSurfaces.dismissedToEditor(state, surfaceId), state)
    }

  private[manager] def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
    SessionWorkflowTransitions.restoredIntoViewport(restoredState, currentState)

private[manager] object StateManagerWorkflowCapability:

  /** Named-session reads and writes, one at a time: a rename never overtakes the save it renames. */
  val SessionLane: Lane.Keyed = Lane.Keyed(LaneKey.Session, LanePolicy.Sequential)
