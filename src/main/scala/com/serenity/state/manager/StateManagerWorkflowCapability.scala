package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.{Deferred, IO, Ref}
import com.serenity.io.FileManager
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.state.undo.UndoState
import com.serenity.ui.layout.LayoutEngine
import org.typelevel.log4cats.Logger

final private[manager] class StateManagerWorkflowCapability(
    stateRef: Ref[IO, AppState],
    undoRef: Ref[IO, UndoState],
    quitSignal: Deferred[IO, Unit],
    logger: Logger[IO],
    fileDialog: Option[com.serenity.io.FileDialog],
    fileManager: FileManager,
    sessionPersistence: SessionPersistence,
    sessionManager: SessionManager,
    operations: StateManagerOperationBoundary,
    editor: StateManagerEditorCapability,
    filePersistence: StateManagerFilePersistence
)(using balance: com.serenity.rope.Balance):

  private def updateState(update: AppState => AppState): IO[Unit] = stateRef.update(update)

  private def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    operations.validateAndUpdateState(newState, fallbackState)

  private def createNewEmptyBuffer(): IO[BufferId] = editor.bufferManager.createNewEmptyBuffer

  private def createPane(bufferId: Option[BufferId]): IO[PaneId] = editor.createPane(bufferId)

  private def switchToPane(paneId: PaneId): IO[Unit] = editor.switchToPane(paneId)

  private def loadSession(): IO[Option[AppState]] = sessionManager.loadSession()

  private def ensureCommandRunnerSurface(state: AppState): AppState = operations.ensureCommandRunnerSurface(state)

  private val fileWorkflow = new StateManagerFileWorkflow(
    stateRef,
    logger,
    fileManager,
    validateAndUpdateState,
    updateFileWorkflowSurface,
    fileWorkflowSurface,
    activeEditorBufferId,
    saveBufferAsEffect,
    continueCloseAfterFormSaveAs
  )

  private val replaceWorkflow =
    new StateManagerReplaceWorkflow(
      stateRef,
      undoRef,
      activeEditorBufferId,
      updateReplaceWorkflowSurface,
      validateAndUpdateState
    )

  private def saveBufferEffect(bufferId: BufferId): IO[Unit] =
    filePersistence.saveExistingBuffer(bufferId).handleErrorWith {
      case error: com.serenity.richtext.LossyRichTextOverwriteException =>
        stateRef.get.flatMap(current => showSaveAsWorkflow(current, bufferId, error.getMessage))
      case error =>
        logger.error(error)(s"[FILE] Failed to save buffer $bufferId")
    }

  private def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
    filePersistence.saveBufferAs(bufferId, path)

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
    val targetBufferIds = closeTargets(scope, state)
    val dirtyBufferIds =
      targetBufferIds.filter(bufferId => state.persisted.buffers.get(bufferId).exists(_.hasUnsavedChanges))
    val cleanBufferIds =
      if preservesBuffers(scope) then Nil
      else targetBufferIds.filterNot(dirtyBufferIds.contains)
    val stateAfterClean = cleanBufferIds.foldLeft(state)(closeBufferUsingExistingFlow)

    dirtyBufferIds match
      case Nil =>
        val finalState = clearCloseActions(stateAfterClean)
        validateAndUpdateState(finalState, state) >>
          stateRef.get.flatMap(committed => finishCloseScope(scope, committed))
      case currentBufferId :: remaining =>
        promptCloseWorkflow(
          stateAfterClean,
          CloseWorkflowState(
            scope = scope,
            currentBufferId = currentBufferId,
            currentBufferLabel = closeBufferLabel(stateAfterClean, currentBufferId),
            remainingBufferIds = remaining
          )
        )

  /** Scopes that leave clean buffers open rather than closing them as they go: Quit (state is discarded on exit anyway)
    * and ReturnToStartPage (the whole session is snapshotted, then replaced by the start page).
    */
  private def preservesBuffers(scope: CloseScope): Boolean =
    scope == CloseScope.Quit || scope == CloseScope.ReturnToStartPage

  /** In the return-to-start-page flow a resolved dirty buffer (saved or discarded) stays open so it is captured by the
    * session snapshot -- unlike Quit/Close, which drop it. Every other scope closes it as before.
    */
  private def closeUnlessSnapshotting(scope: CloseScope, state: AppState, bufferId: BufferId): AppState =
    if scope == CloseScope.ReturnToStartPage then state
    else closeBufferUsingExistingFlow(state, bufferId)

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
        validateAndUpdateState(startPageStateFrom(committed, page), committed)
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

  protected def closeTargets(scope: CloseScope, state: AppState): List[BufferId] =
    scope match
      case CloseScope.Current => activeEditorBufferId(state).toList
      case CloseScope.All     => state.persisted.bufferOrder
      case CloseScope.Others =>
        activeEditorBufferId(state) match
          case Some(focused) => state.persisted.bufferOrder.filterNot(_ == focused)
          case None          => state.persisted.bufferOrder
      case CloseScope.Quit              => state.persisted.bufferOrder
      case CloseScope.ReturnToStartPage => state.persisted.bufferOrder

  protected def promptCloseWorkflow(state: AppState, workflow: CloseWorkflowState): IO[Unit] =
    val focusedState = focusBufferForWorkflow(state, workflow.currentBufferId)
    val modalState =
      ModalStateReducer.show(Modal.CloseWorkflow(workflow), withCloseAction(focusedState, workflow)).state
    validateAndUpdateState(modalState, state)

  private[manager] def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      closeWorkflowSurface(state, surfaceId) match
        case Some(workflow) =>
          workflow.selectedChoice match
            case CloseWorkflowChoice.Cancel =>
              dismissSurfaceAndFocusEditor(surfaceId) >>
                stateRef.get.flatMap(current => validateAndUpdateState(clearCloseActions(current), current))
            case CloseWorkflowChoice.Discard =>
              val dismissedState = clearCloseActions(dismissModalSurface(state))
              val nextState      = closeUnlessSnapshotting(workflow.scope, dismissedState, workflow.currentBufferId)
              validateAndUpdateState(nextState, state) >>
                stateRef.get.flatMap(committed => continueCloseWorkflow(workflow, committed))
            case CloseWorkflowChoice.Save =>
              state.persisted.buffers.get(workflow.currentBufferId) match
                case Some(buffer) if buffer.document.filePath.isDefined =>
                  saveBufferEffect(workflow.currentBufferId) >>
                    stateRef.get.flatMap { savedState =>
                      val dismissedState = clearCloseActions(dismissModalSurface(savedState))
                      val nextState = closeUnlessSnapshotting(workflow.scope, dismissedState, workflow.currentBufferId)
                      validateAndUpdateState(nextState, savedState) >>
                        stateRef.get.flatMap(committed => continueCloseWorkflow(workflow, committed))
                    }
                case Some(_) =>
                  requestSaveAsFileDialog(state, Some(workflow.currentBufferId))
                case None =>
                  validateAndUpdateState(clearCloseActions(dismissModalSurface(state)), state)
        case None =>
          IO.unit
    }

  protected def continueCloseWorkflow(workflow: CloseWorkflowState, state: AppState): IO[Unit] =
    workflow.remainingBufferIds match
      case nextBufferId :: remaining =>
        promptCloseWorkflow(
          state,
          CloseWorkflowState(
            scope = workflow.scope,
            currentBufferId = nextBufferId,
            currentBufferLabel = closeBufferLabel(state, nextBufferId),
            remainingBufferIds = remaining
          )
        )
      case Nil =>
        val finalState = clearCloseActions(state)
        validateAndUpdateState(finalState, state) >>
          stateRef.get.flatMap(committed => finishCloseScope(workflow.scope, committed))

  protected def focusBufferForWorkflow(state: AppState, bufferId: BufferId): AppState =
    EditorState.focusBuffer(EditorState.rebalancePanes(state, Some(bufferId)), bufferId)

  protected def closeBufferUsingExistingFlow(state: AppState, bufferId: BufferId): AppState =
    val focusedState = focusBufferForWorkflow(state, bufferId)
    val closedState  = EditorState.closeFocusedTab(focusedState)
    if closedState.persisted.layout.activeEditorPaneId.isDefined then closedState
    else ensureCommandRunnerSurface(closedState)

  protected def closeBufferLabel(state: AppState, bufferId: BufferId): String =
    state.persisted.buffers
      .get(bufferId)
      .flatMap(_.document.filePath.flatMap(path => Option(path.getFileName).map(_.toString)))
      .getOrElse(s"Buffer ${bufferId.value} - unsaved")

  protected def withCloseAction(state: AppState, workflow: CloseWorkflowState): AppState =
    state.copy(runtime =
      state.runtime.copy(actionStack =
        AppAction.CloseWorkflow(workflow) :: clearCloseActions(state).runtime.actionStack
      )
    )

  private[manager] def clearCloseActions(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(actionStack = Nil))

  protected def dismissModalSurface(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(
        uiSurfaces = state.runtime.uiSurfaces.filterNot {
          case UiSurface(_, SurfaceContent.ModalWorkflow(_), _, _) => true
          case _                                                   => false
        },
        // CloseWorkflow/FileWorkflow (#814) live here instead of uiSurfaces -- every caller of this function is
        // finishing a close/save workflow chain (including a Save-As dialog nested on top of a close confirmation),
        // so clearing the whole stack matches the uiSurfaces filter's original "every ModalWorkflow surface" intent.
        modalStack = Nil
      )
    )

  private[manager] def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    fileWorkflow.refreshFileWorkflowEffect(surfaceId)

  private[manager] def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    fileWorkflow.submitFileWorkflowEffect(surfaceId)

  /** Opens the directory an Open dialog is targeting as a project root (issue #1525): resolves and validates the target
    * through `fileWorkflow` (reporting back into the dialog on failure, exactly like a regular Open submit), then -- on
    * a confirmed directory -- dismisses the dialog and hands the path to `openProjectRoot`, which the caller supplies
    * so this capability doesn't need its own route to the panel-pinning machinery (`StateManagerPanelEffects`, owned by
    * `StateManagerEffectHandlers`, isn't available to this capability -- issue #1525 reuses it rather than duplicating
    * it).
    */
  private[manager] def openFileWorkflowAsProjectRootEffect(
    surfaceId: SurfaceId,
    openProjectRoot: Path => IO[Unit]
  ): IO[Unit] =
    fileWorkflow.resolveOpenAsProjectRoot(surfaceId).flatMap {
      case Some(path) => dismissSurfaceAndFocusEditor(surfaceId) >> openProjectRoot(path)
      case None       => IO.unit
    }

  private[manager] def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    replaceWorkflow.submitReplaceWorkflowEffect(surfaceId)

  /** The explicit, single-step counterpart to submitting twice (Enter to flag `missingPathSegments`, Enter again to
    * confirm): creates the missing directories -- as a side effect of performing the save itself, exactly like the
    * confirmed double-submit path -- immediately, without a second submit (issue #1253).
    */
  private[manager] def createFileWorkflowDirectoriesEffect(surfaceId: SurfaceId): IO[Unit] =
    fileWorkflow.createFileWorkflowDirectoriesEffect(surfaceId)

  /** What follows a successful in-app Save-As: resume the close workflow this save was the "Save before close" step of,
    * or -- when no close workflow is waiting on this buffer -- just dismiss the dialog.
    */
  private def continueCloseAfterFormSaveAs(surfaceId: SurfaceId, bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { savedState =>
      savedState.runtime.actionStack.collectFirst { case AppAction.CloseWorkflow(closeWorkflow) => closeWorkflow } match
        case Some(closeWorkflow) if closeWorkflow.currentBufferId == bufferId =>
          val dismissedState = dismissModalSurface(savedState)
          val nextState =
            if preservesBuffers(closeWorkflow.scope) then dismissedState
            else closeBufferUsingExistingFlow(dismissedState, bufferId)
          validateAndUpdateState(nextState, savedState) >>
            stateRef.get.flatMap(committed => continueCloseWorkflow(closeWorkflow, committed))
        case _ =>
          dismissSurfaceAndFocusEditor(surfaceId)
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
                  saveBufferAsEffect(bufferId, path) >> continueCloseAfterNativeSaveAs(bufferId)
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
    stateRef.get.flatMap { savedState =>
      savedState.runtime.actionStack.collectFirst {
        case AppAction.CloseWorkflow(closeWorkflow) if closeWorkflow.currentBufferId == bufferId => closeWorkflow
      } match
        case Some(closeWorkflow) =>
          val dismissedState = dismissModalSurface(savedState)
          val nextState =
            if preservesBuffers(closeWorkflow.scope) then dismissedState
            else closeBufferUsingExistingFlow(dismissedState, bufferId)
          validateAndUpdateState(nextState, savedState) >>
            stateRef.get.flatMap(committed => continueCloseWorkflow(closeWorkflow, committed))
        case None =>
          IO.unit
    }

  private[manager] def activeEditorBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)

  protected def updateFileWorkflowSurface(surfaceId: SurfaceId, workflow: FileWorkflowState): IO[Unit] =
    stateRef.update { state =>
      if state.runtime.modalStack.exists(_.id == surfaceId) then
        state.copy(runtime =
          state.runtime.copy(modalStack =
            state.runtime.modalStack.map(dialog =>
              if dialog.id == surfaceId then dialog.copy(modal = Modal.FileWorkflow(workflow)) else dialog
            )
          )
        )
      else state
    }

  protected def updateReplaceWorkflowSurface(surfaceId: SurfaceId, workflow: ReplaceWorkflowState): IO[Unit] =
    stateRef.update { state =>
      state.surfaceById(surfaceId) match
        case Some(surface) =>
          val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)))
          state.copy(runtime =
            state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.movedToEndWhere(_.id == surfaceId)(updatedSurface))
          )
        case None =>
          state
    }

  protected def dismissSurfaceAndFocusEditor(surfaceId: SurfaceId): IO[Unit] =
    stateRef.update { state =>
      val baseState = state.copy(runtime =
        state.runtime.copy(
          uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surfaceId),
          modalStack = state.runtime.modalStack.filterNot(_.id == surfaceId)
        )
      )
      state.persisted.layout.activeEditorPaneId match
        case Some(paneId) => baseState.copy(persisted = baseState.persisted.copy(focus = Focus.EditorPane(paneId)))
        case None         => baseState
    }

  protected def fileWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[FileWorkflowState] =
    state.runtime.modalStack.find(_.id == surfaceId).collect {
      case ModalDialog(_, Modal.FileWorkflow(workflow), _) =>
        workflow
    }

  protected def closeWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[CloseWorkflowState] =
    state.runtime.modalStack.find(_.id == surfaceId).collect {
      case ModalDialog(_, Modal.CloseWorkflow(workflow), _) => workflow
    }

  /** Every branch here ends its own structural commit unchecked (session deserialization, or the buffer/pane creation
    * in `createDefaultStartupBuffer`), with nothing downstream re-validating the result -- unlike, say,
    * `ComponentResult.Dismiss`'s equivalent buffer/pane creation, whose caller re-validates the composed result before
    * committing (`StateManagerEventPipeline.applyEvent`). A corrupted or hand-edited session file, restored unchecked
    * on literal app startup, is exactly the kind of public mutation path #858 asks every commit to go through, so each
    * branch below re-validates its own final result rather than trusting the unchecked intermediate steps.
    */
  private[manager] def restoreStartupSession(): IO[Unit] =
    logger.info("[CMD] Session restore requested") >>
      loadSession().flatMap {
        case Some(restoredState) if restoredState.persisted.bufferOrder.nonEmpty =>
          logger.info("[CMD] Session loaded successfully") >>
            stateRef.get.flatMap(current =>
              validateAndUpdateState(restoreSessionIntoCurrentViewport(restoredState, current), current)
            )
        case Some(_) =>
          logger.info("[CMD] Session loaded with no buffers - creating default session") >>
            createDefaultStartupBuffer()
        case None =>
          logger.info("[CMD] No session found - creating default session") >>
            createDefaultStartupBuffer()
      }

  // `createNewEmptyBuffer`/`createPane`/`switchToPane` each now commit through `validateAndUpdateState` in their own
  // right, so clearing `uiSurfaces` (and with it the startup page's surface) has to wait until focus has already
  // moved to the new pane -- doing it first, as this used to, left focus dangling on the just-removed surface for
  // every step in between, which each of those steps' own validation now (correctly) rejects.
  private def createDefaultStartupBuffer(): IO[Unit] =
    stateRef.get.flatMap { before =>
      createNewEmptyBuffer().flatMap { bufferId =>
        updateState(s => s.copy(persisted = s.persisted.copy(bufferOrder = s.persisted.bufferOrder :+ bufferId))) >>
          createPane(Some(bufferId)).flatMap { paneId =>
            switchToPane(paneId) >>
              updateState(state => state.copy(runtime = state.runtime.copy(uiSurfaces = List.empty))) >>
              stateRef.get.flatMap(finalState => validateAndUpdateState(finalState, before))
          }
      }
    }

  private[manager] def createStartupSession(): IO[Unit] =
    stateRef.get.flatMap { before =>
      val opened = EditorState.openNewTab(before)
      validateAndUpdateState(opened.copy(runtime = opened.runtime.copy(uiSurfaces = List.empty)), before)
    }

  /** Opens the "Save Session As..." name prompt (issue #1390), pre-filled empty -- `ModalSessionReducer` routes its
    * Enter into `submitSessionNamePromptEffect` below.
    */
  private[manager] def openSaveSessionAsPrompt(state: AppState): IO[Unit] =
    val shown = ModalStateReducer.show(Modal.SessionNamePrompt(SessionNamePromptMode.SaveAs, ""), state).state
    validateAndUpdateState(shown, state)

  /** Opens the `SessionManager.listSessions()` picker (issue #1390) for either purpose: `Open` loads the selected
    * session directly on Enter, `Rename` hands it off to `openSaveSessionAsPrompt`'s sibling name prompt.
    */
  private[manager] def openSessionPicker(state: AppState, purpose: SessionListPurpose): IO[Unit] =
    sessionManager.listSessions().flatMap { sessions =>
      val shown = ModalStateReducer.show(Modal.SessionList(sessions, 0, purpose), state).state
      validateAndUpdateState(shown, state)
    }

  /** Completes the name prompt: `saveSessionAs` for a brand-new named session, `renameSession` for one already picked
    * from the list. A blank (post-trim) name is treated as a cancel, matching `ModalSessionReducer`'s own guard on
    * `ModalSubmit`.
    */
  private[manager] def submitSessionNamePromptEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      sessionNamePromptSurface(state, surfaceId) match
        case Some((SessionNamePromptMode.SaveAs, input)) if input.trim.nonEmpty =>
          sessionManager.saveSessionAs(input.trim, state).void >> dismissSurfaceAndFocusEditor(surfaceId)
        case Some((SessionNamePromptMode.Rename(sessionId), input)) if input.trim.nonEmpty =>
          sessionManager.renameSession(sessionId, input.trim) >> dismissSurfaceAndFocusEditor(surfaceId)
        case _ =>
          dismissSurfaceAndFocusEditor(surfaceId)
    }

  /** Completes an `Open`-purpose `SessionList` selection: loads the picked session and restores it into the current
    * viewport, exactly like `SessionIntent.RestoreSession` does for the implicit "current" session.
    */
  private[manager] def submitSessionListEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      sessionListSurface(state, surfaceId) match
        case Some((sessions, selectedIndex, SessionListPurpose.Open)) =>
          sessions.lift(selectedIndex) match
            case Some(session) =>
              // `restoreSessionIntoCurrentViewport` replaces `runtime.uiSurfaces` wholesale (see its own doc), which
              // already clears this picker along with everything else -- no separate dismiss needed, exactly like
              // `SessionIntent.RestoreSession`'s equivalent call.
              sessionManager.loadSession(session.id).flatMap {
                case Some(restored) =>
                  validateAndUpdateState(restoreSessionIntoCurrentViewport(restored, state), state)
                case None =>
                  dismissSurfaceAndFocusEditor(surfaceId)
              }
            case None =>
              dismissSurfaceAndFocusEditor(surfaceId)
        case _ =>
          dismissSurfaceAndFocusEditor(surfaceId)
    }

  private def sessionNamePromptSurface(
    state: AppState,
    surfaceId: SurfaceId
  ): Option[(SessionNamePromptMode, String)] =
    state.runtime.uiSurfaces.find(_.id == surfaceId).collect {
      case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.SessionNamePrompt(mode, input)), _, _) => (mode, input)
    }

  private def sessionListSurface(
    state: AppState,
    surfaceId: SurfaceId
  ): Option[(List[com.serenity.session.SessionMetadata], Int, SessionListPurpose)] =
    state.runtime.uiSurfaces.find(_.id == surfaceId).collect {
      case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.SessionList(sessions, index, purpose)), _, _) =>
        (sessions, index, purpose)
    }

  private[manager] def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
    val restored = restoredState.copy(
      runtime = restoredState.runtime.copy(
        uiSurfaces = List.empty,
        viewportSize = currentState.runtime.viewportSize,
        isTuiMode = currentState.runtime.isTuiMode,
        keyboardFidelityTier = currentState.runtime.keyboardFidelityTier
      )
    )
    currentState.runtime.viewportSize
      .map(viewportSize => LayoutEngine.syncViewportDimensions(restored, viewportSize))
      .getOrElse(restored)
