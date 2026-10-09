package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import com.serenity.command.SessionCommands
import com.serenity.io.{FileManager, FileUtils, ProjectFileWalker}
import com.serenity.session.{SessionId, SessionManager, SessionPersistence}
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
    filePersistence: StateManagerFilePersistence,
    restarter: Option[RestartMode => IO[Unit]] = None
)(using balance: com.serenity.rope.Balance):
  import StateManagerWorkflowCapability.{ProjectFilesLane, SessionLane}

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
    continueCloseAfterFormSaveAs,
    operations.showNotice
  )

  private val replaceWorkflow = new StateManagerReplaceWorkflow(modelCommit.updateValidated)

  /** Opens the reload/overwrite/cancel prompt (#1623) for `bufferId` -- either because a save just discovered the
    * on-disk file changed since it was opened, or because a focus-in re-check found the same thing.
    */
  private[manager] def openReloadConflictModal(state: AppState, bufferId: BufferId, bufferLabel: String): IO[Unit] =
    val modalState =
      ModalStateReducer.show(Modal.Confirm(ConfirmPrompt.reloadConflict(bufferId, bufferLabel)), state).state
    modelCommit.commitState(modalState, state)

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
    if restartRequestedWithoutLauncher(scope) then logger.warn("[CMD] Restarting is not available in this session")
    else
      filePersistence.settlePendingSaves(close.closeTargets(scope, state)) >>
        modelCommit.currentState.flatMap(current => commitClose(current, close.begun(scope, current)))

  private def restartRequestedWithoutLauncher(scope: CloseScope): Boolean =
    scope match
      case CloseScope.Restart(_) => restarter.isEmpty
      case _                     => false

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
      case CloseScope.Restart(mode) =>
        sessionPersistence.onAppClose(committed) >> restarter.traverse_(_(mode)) >>
          quitSignal.complete(()).attempt.void
      case CloseScope.ReturnToStartPage =>
        snapshotAndShowStartPage(committed)
      case _ =>
        IO.unit

  /** Persist the current session (unsaved buffers included, so [Tab] Quick-resume restores them) and replace the editor
    * with a start page that offers to resume it. The write runs on the Session lane. If the editor changed while it was
    * being written, the changed editor is written once more; if it changes again, it stays, with a notice saying why.
    */
  private def snapshotAndShowStartPage(committed: AppState): IO[Unit] =
    lanes.submitEffect(SessionLane, saveThenShowStartPage(committed, rewritesLeft = 1))

  private def saveThenShowStartPage(snapshot: AppState, rewritesLeft: Int): IO[Unit] =
    // On failure the editor stays: the start page would offer to resume a session that was never written.
    sessionManager.saveSession(snapshot, persistUnsavedBuffers = true).attempt.flatMap {
      case Right(_) =>
        modelCommit.currentState.flatMap { live =>
          if StartPageTransitions.sameBuffers(live, snapshot) then
            (readableRecentFiles(snapshot), existingRecentFolders(snapshot)).flatMapN((files, folders) =>
              lanes.dispatchEffectResult(EffectResult.StartPageReady(snapshot, files, folders), _ => IO.unit)
            )
          else if rewritesLeft > 0 then saveThenShowStartPage(live, rewritesLeft - 1)
          else operations.showNotice(FileFailureNotice.stayedInEditor)
        }
      case Left(error) =>
        logger.error(error)("[SESSION] Saving the session before the start page failed") >>
          operations.showNotice(FileFailureNotice.sessionSaveFailed(error))
    }

  private def readableRecentFiles(committed: AppState): IO[List[Path]] =
    IO.blocking(committed.persisted.recentFiles.filter(path => Files.isRegularFile(path) && Files.isReadable(path)))

  private def existingRecentFolders(committed: AppState): IO[List[Path]] =
    IO.blocking(committed.persisted.recentFolders.filter(Files.isDirectory(_)))

  /** Answers the close waiting on the action stack -- what the close prompt's choices run. */
  private[manager] def resolveClose(choice: CloseWorkflowChoice): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      close.pending(state).fold(IO.unit) { workflow =>
        choice match
          case CloseWorkflowChoice.Cancel =>
            modelCommit.commitState(close.abandoned(workflow, state), state)
          case CloseWorkflowChoice.Discard =>
            commitClose(state, close.resolved(workflow, state))
          case CloseWorkflowChoice.Save =>
            state.persisted.buffers.get(workflow.currentBufferId) match
              case Some(buffer) if buffer.document.filePath.isDefined && !buffer.formattingLostOnSave =>
                saveBeforeClose(workflow)
              case Some(_) =>
                // Untitled, or a format that can't store its formatting: Save As keeps it, and cancelling asks again.
                requestSaveAsFileDialog(state, Some(workflow.currentBufferId))
              case None =>
                modelCommit.commitState(close.clearCloseActions(close.dismissModalSurface(state)), state)
      }
    }

  /** Closes the buffer only once its save has landed and left it clean (#1708). A failed or conflicting save abandons
    * the whole close -- a quit or close-all stops at this buffer -- rather than dropping the edits it could not write.
    */
  private def saveBeforeClose(workflow: CloseWorkflowState): IO[Unit] =
    val bufferId = workflow.currentBufferId
    filePersistence.saveExistingBuffer(bufferId).attempt.flatMap {
      case Right(()) =>
        modelCommit.currentState.flatMap { saved =>
          if saved.persisted.buffers.get(bufferId).exists(!_.hasUnsavedChanges) then
            commitClose(saved, close.resolved(workflow, saved))
          else modelCommit.commitState(close.abandoned(workflow, saved), saved)
        }
      case Left(error: com.serenity.richtext.LossyRichTextOverwriteException) =>
        // The Save-As form resumes this close once it saves (continueCloseAfterFormSaveAs); dismissing it asks again.
        modelCommit.currentState.flatMap(current => showSaveAsWorkflow(current, bufferId, error.getMessage))
      case Left(_: com.serenity.io.FileManagerError.ExternalConflict) =>
        commit(close.conflicted(workflow, _))
      case Left(error) =>
        logger.error(error)(s"[FILE] Failed to save buffer $bufferId before closing it") >>
          commit(close.abandoned(workflow, _)) >>
          modelCommit.currentState
            .flatMap(state => operations.showNotice(FileFailureNotice.forBuffer(state, bufferId, error)))
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
        case None => modelCommit.commitState(WorkflowSurfaces.dismissedToPriorFocus(saved, surfaceId), saved)
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
                  filePersistence.saveBufferAs(bufferId, path).attempt.flatMap {
                    case Right(()) => continueCloseAfterNativeSaveAs(bufferId)
                    case Left(error) =>
                      operations.showNotice(
                        FileFailureNotice.fileSaveFailed(bufferId, path, error, state.persisted.config)
                      ) >> repromptCloseOn(bufferId)
                  }
                case None =>
                  repromptCloseOn(bufferId)
              }
              .handleErrorWith(ex =>
                logger.error(ex)(s"[FILE] Native save-as dialog failed for buffer $bufferId") >> repromptCloseOn(
                  bufferId
                )
              )
          case None =>
            // No native dialog to show at all -- the in-app form is the only way to collect a path, not a fallback
            // for a dialog the user might have cancelled (that case stays a no-op above, via chooseSaveFile's None).
            openFileWorkflowModal(FileWorkflowMode.SaveAs, state, Some(bufferId))
      case None =>
        logger.debug("[FILE] Save As requested without a focused buffer")

  /** A Save As that a close's Save opened, cancelled or failed: ask about that buffer again rather than strand the
    * close with no prompt.
    */
  private def repromptCloseOn(bufferId: BufferId): IO[Unit] =
    commit(current => close.pendingOn(current, bufferId).fold(current)(close.reprompted(_, current)))

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
            commit(SessionWorkflowTransitions.restoredIntoViewport(restoredState, _)) >>
            offerHotExitRecovery
        case Some(_) =>
          logger.info("[CMD] Session loaded with no buffers - creating default session") >>
            commit(SessionWorkflowTransitions.withDefaultStartupBuffer)
        case None =>
          logger.info("[CMD] No session found - creating default session") >>
            commit(SessionWorkflowTransitions.withDefaultStartupBuffer)
      }

  /** Asks about each restored buffer whose unsaved text differs from its file (#1904). The files are read after the
    * restore has committed, so the editor is up before the disk is touched.
    */
  private def offerHotExitRecovery: IO[Unit] =
    modelCommit.currentState.flatMap { restored =>
      restored.persisted.buffers.values.toList
        .filter(HotExitRecovery.holdsBackup)
        .sortBy(_.id.value)
        .traverseFilter(recoveryOffer)
        .flatMap(offers => if offers.isEmpty then IO.unit else commit(HotExitRecovery.withRecoveryOffered(_, offers)))
    }

  /** No offer for a file that can no longer be read: the recovered text is then all there is. */
  private def recoveryOffer(backup: Buffer): IO[Option[RecoveryOffer]] =
    backup.document.filePath.fold(IO.pure(Option.empty[RecoveryOffer])) { path =>
      fileManager.loadFile(path, backup.id).attempt.map(_.toOption.flatMap(HotExitRecovery.offer(backup, _)))
    }

  private[manager] def createStartupSession(): IO[Unit] =
    commit { before =>
      val opened = EditorState.openNewTab(before)
      opened.copy(runtime = opened.runtime.copy(uiSurfaces = List.empty))
    }

  /** Replaces the start page with a fresh editor tab, keeping every other surface; a no-op once it has gone. */
  private[manager] def leaveStartPage(): IO[Unit] =
    commit(UiPresetTransitions.seedEditorFromSplash)

  /** Opens the "Save Session As..." name prompt (issue #1390), pre-filled empty -- `ModalSessionReducer` routes its
    * Enter into `submitSessionNamePromptEffect` below. Shown on the current state, so the palette's record of the
    * command that opened it is kept.
    */
  private[manager] def openSaveSessionAsPrompt(): IO[Unit] =
    commit(ModalStateReducer.show(Modal.TextPrompt(TextPrompt.sessionName(SessionNamePromptMode.SaveAs)), _).state)

  /** Opens the session picker (issue #1390) at once, still loading, and lists the saved sessions into it on the Session
    * lane. `Open` loads the picked session; `Rename` hands it to the name prompt.
    */
  private[manager] def openSessionPicker(purpose: SessionListPurpose): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      SessionWorkflowTransitions.withSessionPickerOpened(state, purpose).fold(IO.unit) { (opened, pickerId) =>
        modelCommit.commitState(opened, state) >>
          lanes.submitEffect(
            SessionLane,
            sessionManager
              .listSessions()
              .attempt
              .flatMap(listing =>
                lanes.dispatchEffectResult(
                  EffectResult.SessionsListed(pickerId, purpose, listing.left.map(_.getMessage)),
                  _ => IO.unit
                )
              )
          )
      }
    }

  /** Completes the name prompt: `saveSessionAs` for a brand-new named session, `renameSession` for one already picked
    * from the list. The prompt closes at once; the write runs on the Session lane. A blank (post-trim) name is treated
    * as a cancel, matching `ModalSessionReducer`'s own guard on `ModalSubmit`.
    */
  private[manager] def submitSessionNamePromptEffect(surfaceId: SurfaceId): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      val dismissed = WorkflowSurfaces.dismissedToPriorFocus(state, surfaceId)
      val write = SessionWorkflowTransitions.sessionNamePrompt(state, surfaceId) match
        case Some((SessionNamePromptMode.SaveAs, input)) if input.trim.nonEmpty =>
          Some(
            sessionManager
              .saveSessionAs(input.trim, dismissed)
              .void
              .handleErrorWith(error => operations.showNotice(FileFailureNotice.sessionSaveFailed(error)))
          )
        case Some((SessionNamePromptMode.Rename(sessionId), input)) if input.trim.nonEmpty =>
          Some(sessionManager.renameSession(sessionId, input.trim))
        case _ =>
          None
      modelCommit.commitState(dismissed, state) >> write.fold(IO.unit)(lanes.submitEffect(SessionLane, _))
    }

  /** Loads a saved session on the Session lane and replaces the current one with it, as `SessionIntent.RestoreSession`
    * does. Picked from a session picker, it applies only while that picker is still open and waiting on it.
    */
  private[manager] def openNamedSession(sessionId: SessionId, state: AppState): IO[Unit] =
    val pickerId = ListPicker.pendingOn(state, SessionCommands.openNamedSession(sessionId))
    lanes.submitEffect(
      SessionLane,
      sessionManager
        .loadSession(sessionId)
        .flatMap(restored =>
          lanes.dispatchEffectResult(EffectResult.NamedSessionLoaded(pickerId, restored), _ => IO.unit)
        )
    )

  private[manager] def openRenameSessionPrompt(sessionId: SessionId, currentName: String): IO[Unit] =
    commit(
      ModalStateReducer
        .show(
          Modal.TextPrompt(TextPrompt.sessionName(SessionNamePromptMode.Rename(sessionId), currentName)),
          _
        )
        .state
    )

  /** Opens the "Go to File" finder at once, still loading, over the docked explorer's root -- the project root "Open as
    * root" sets -- or the working directory when no explorer is docked, and walks that root into it on the ProjectFiles
    * lane.
    */
  private[manager] def openFileFinder: IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      FileFinder.explorerRoot(state).fold(FileUtils.getCurrentDirectory)(IO.pure).flatMap { root =>
        FileFinderTransitions.withFinderOpened(state, root).fold(IO.unit) { (opened, pickerId) =>
          modelCommit.commitState(opened, state) >>
            lanes.submitEffect(
              ProjectFilesLane,
              ProjectFileWalker
                .list(root, FileFinder.MaxListedFiles)
                .attempt
                .flatMap(listing =>
                  lanes.dispatchEffectResult(
                    EffectResult.FilesListed(
                      pickerId,
                      root,
                      listing.left.map(error => s"Couldn't list $root: ${error.getClass.getSimpleName}")
                    ),
                    _ => IO.unit
                  )
                )
            )
        }
      }
    }

  private[manager] def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
    SessionWorkflowTransitions.restoredIntoViewport(restoredState, currentState)

private[manager] object StateManagerWorkflowCapability:

  /** Named-session reads and writes, one at a time: a rename never overtakes the save it renames. */
  val SessionLane: Lane.Keyed = Lane.Keyed(LaneKey.Session, LanePolicy.Sequential)

  val ProjectFilesLane: Lane.Keyed = Lane.Keyed(LaneKey.ProjectFiles, LanePolicy.SwitchLatest)
