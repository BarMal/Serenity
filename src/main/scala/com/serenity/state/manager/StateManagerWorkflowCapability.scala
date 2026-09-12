package com.serenity.state.manager

import java.nio.file.Path

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
      if scope == CloseScope.Quit then Nil
      else targetBufferIds.filterNot(dirtyBufferIds.contains)
    val stateAfterClean = cleanBufferIds.foldLeft(state)(closeBufferUsingExistingFlow)

    dirtyBufferIds match
      case Nil =>
        val finalState = clearCloseActions(stateAfterClean)
        validateAndUpdateState(finalState, state) >>
          stateRef.get.flatMap { committed =>
            IO.whenA(scope == CloseScope.Quit)(
              sessionPersistence.onAppClose(committed) >>
                quitSignal.complete(()).attempt.void
            )
          }
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

  protected def closeTargets(scope: CloseScope, state: AppState): List[BufferId] =
    scope match
      case CloseScope.Current => activeEditorBufferId(state).toList
      case CloseScope.All     => state.persisted.bufferOrder
      case CloseScope.Others =>
        activeEditorBufferId(state) match
          case Some(focused) => state.persisted.bufferOrder.filterNot(_ == focused)
          case None          => state.persisted.bufferOrder
      case CloseScope.Quit => state.persisted.bufferOrder

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
              val nextState      = closeBufferUsingExistingFlow(dismissedState, workflow.currentBufferId)
              validateAndUpdateState(nextState, state) >>
                stateRef.get.flatMap(committed => continueCloseWorkflow(workflow, committed))
            case CloseWorkflowChoice.Save =>
              state.persisted.buffers.get(workflow.currentBufferId) match
                case Some(buffer) if buffer.document.filePath.isDefined =>
                  saveBufferEffect(workflow.currentBufferId) >>
                    stateRef.get.flatMap { savedState =>
                      val dismissedState = clearCloseActions(dismissModalSurface(savedState))
                      val nextState      = closeBufferUsingExistingFlow(dismissedState, workflow.currentBufferId)
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
          stateRef.get.flatMap { committed =>
            IO.whenA(workflow.scope == CloseScope.Quit)(
              sessionPersistence.onAppClose(committed) >>
                quitSignal.complete(()).attempt.void
            )
          }

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
            if closeWorkflow.scope == CloseScope.Quit then dismissedState
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
            if closeWorkflow.scope == CloseScope.Quit then dismissedState
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

  private def createDefaultStartupBuffer(): IO[Unit] =
    stateRef.get.flatMap { before =>
      updateState(state => state.copy(runtime = state.runtime.copy(uiSurfaces = List.empty))) >>
        createNewEmptyBuffer().flatMap { bufferId =>
          updateState(s => s.copy(persisted = s.persisted.copy(bufferOrder = s.persisted.bufferOrder :+ bufferId))) >>
            createPane(Some(bufferId)).flatMap { paneId =>
              switchToPane(paneId) >>
                stateRef.get.flatMap(finalState => validateAndUpdateState(finalState, before))
            }
        }
    }

  private[manager] def createStartupSession(): IO[Unit] =
    stateRef.get.flatMap { before =>
      val opened = EditorState.openNewTab(before)
      validateAndUpdateState(opened.copy(runtime = opened.runtime.copy(uiSurfaces = List.empty)), before)
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
