package com.serenity.state.manager

import java.nio.file.{Files, Path, Paths}

import cats.effect.IO
import com.serenity.io.{FileUtils, StorageLocation}
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry}
import com.serenity.text.TextEditing
import com.serenity.ui.layout.LayoutEngine

final private[manager] class StateManagerWorkflowCapability(
    dependencies: WorkflowCapabilityPort
)(using balance: com.serenity.rope.Balance):

  import dependencies.*

  private[manager] def openFileWorkflowModal(
    mode: FileWorkflowMode,
    state: AppState,
    bufferIdOverride: Option[BufferId] = None,
    statusMessage: Option[String] = None
  ): IO[Unit] =
    val targetBufferId = bufferIdOverride.orElse(state.focusedBufferId)
    val focusedPath    = targetBufferId.flatMap(id => state.persisted.buffers.get(id)).flatMap(_.document.filePath)
    val filename = mode match
      case FileWorkflowMode.SaveAs =>
        focusedPath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("")
      case FileWorkflowMode.Open => ""

    val pathIO =
      mode match
        case FileWorkflowMode.SaveAs =>
          focusedPath
            .flatMap(path => Option(path.getParent))
            .map(IO.pure)
            .getOrElse(FileUtils.getCurrentDirectory)
        case FileWorkflowMode.Open =>
          FileUtils.getCurrentDirectory

    pathIO.flatMap { basePath =>
      val workflow = FileWorkflowState(
        mode = mode,
        filename = filename,
        path = basePath.toString,
        statusMessage = statusMessage
      )
      val predictedState = ModalStateReducer.show(Modal.FileWorkflow(workflow), state).state
      logger.info(
        s"[FILE-WORKFLOW OPENED] mode=$mode filename=${workflow.filename} path=${workflow.path} " +
          s"surfaceId=${predictedState.modalSurface.map(_.id).getOrElse("none")} focus=${predictedState.persisted.focus}"
      ) >>
        updateState(current => ModalStateReducer.show(Modal.FileWorkflow(workflow), current).state)
    }

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
        stateRef.set(finalState) >>
          IO.whenA(scope == CloseScope.Quit)(
            sessionPersistence.onAppClose(finalState) >>
              quitSignal.complete(()).attempt.void
          )
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
    stateRef.set(modalState)

  private[manager] def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      closeWorkflowSurface(state, surfaceId) match
        case Some((_, workflow)) =>
          workflow.selectedChoice match
            case CloseWorkflowChoice.Cancel =>
              dismissSurfaceAndFocusEditor(surfaceId) >>
                stateRef.update(clearCloseActions)
            case CloseWorkflowChoice.Discard =>
              val dismissedState = clearCloseActions(dismissModalSurface(state))
              val nextState      = closeBufferUsingExistingFlow(dismissedState, workflow.currentBufferId)
              stateRef.set(nextState) >> continueCloseWorkflow(workflow, nextState)
            case CloseWorkflowChoice.Save =>
              state.persisted.buffers.get(workflow.currentBufferId) match
                case Some(buffer) if buffer.document.filePath.isDefined =>
                  saveBufferEffect(workflow.currentBufferId) >>
                    stateRef.get.flatMap { savedState =>
                      val dismissedState = clearCloseActions(dismissModalSurface(savedState))
                      val nextState      = closeBufferUsingExistingFlow(dismissedState, workflow.currentBufferId)
                      stateRef.set(nextState) >> continueCloseWorkflow(workflow, nextState)
                    }
                case Some(_) =>
                  requestSaveAsFileDialog(state, Some(workflow.currentBufferId))
                case None =>
                  stateRef.set(clearCloseActions(dismissModalSurface(state)))
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
        stateRef.set(finalState) >>
          IO.whenA(workflow.scope == CloseScope.Quit)(
            sessionPersistence.onAppClose(finalState) >>
              quitSignal.complete(()).attempt.void
          )

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
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot {
      case UiSurface(_, SurfaceContent.ModalWorkflow(_), _, _) => true
      case _                                                   => false
    }))

  private[manager] def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      fileWorkflowSurface(state, surfaceId) match
        case Some((_, workflow)) =>
          refreshWorkflowState(workflow).flatMap(refreshed => updateFileWorkflowSurface(surfaceId, refreshed))
        case None =>
          IO.unit
    }

  private[manager] def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      fileWorkflowSurface(state, surfaceId) match
        case Some((_, workflow)) =>
          workflow match
            case openWorkflow: OpenFileWorkflowState =>
              completeOpenWorkflow(surfaceId, openWorkflow)
            case saveAsWorkflow: SaveAsFileWorkflowState =>
              completeSaveAsWorkflow(surfaceId, saveAsWorkflow, state)
        case None =>
          IO.unit
    }

  private[manager] def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      replaceWorkflowSurface(state, surfaceId) match
        case Some((_, workflow)) =>
          workflow.selectedAction match
            case ReplaceWorkflowAction.ReplaceAll =>
              submitReplaceAllEffect(surfaceId, workflow, state)
            case ReplaceWorkflowAction.ReplaceNext =>
              submitReplaceNextEffect(surfaceId, workflow, state)

        case None =>
          IO.unit
    }

  private def submitReplaceAllEffect(surfaceId: SurfaceId, workflow: ReplaceWorkflowState, state: AppState): IO[Unit] =
    activeEditorBufferId(state) match
      case None =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("No active buffer"))
        )
      case Some(_) if workflow.findText.isEmpty =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("Enter text to find"))
        )
      case Some(bufferId) =>
        state.persisted.buffers.get(bufferId) match
          case Some(buffer) =>
            workflow.selectedScope.resolve(buffer.primarySelection, offsetForCursor(buffer.document.content, _)) match
              case Left(error) =>
                updateReplaceWorkflowSurface(
                  surfaceId,
                  workflow.copy(statusMessage = Some(error.message))
                )
              case Right(range) =>
                val matches = scopedReplaceMatches(buffer, workflow.findText, range)
                if matches.isEmpty then
                  updateReplaceWorkflowSurface(
                    surfaceId,
                    workflow.copy(statusMessage = Some("No matches found"))
                  )
                else
                  val updatedContent =
                    replaceMatchesInRanges(
                      rope = buffer.document.content,
                      matchOffsets = matches,
                      findText = workflow.findText,
                      replacementText = workflow.replacementText
                    )
                  val cursorOffset =
                    finalCursorOffsetAfterReplacements(
                      matches,
                      workflow.findText.length,
                      workflow.replacementText.length
                    )
                  val newCursor = cursorPositionForOffset(updatedContent, cursorOffset)
                  val updatedFindState =
                    refreshedFindState(updatedContent, workflow.findText, requestedIndex = 0)
                  val updatedBuffer = buffer.copy(
                    document = buffer.document.copy(
                      content = updatedContent,
                      isDirty = true,
                      isNewEmpty = false
                    ),
                    editing = buffer.editing.copy(
                      cursors = List(newCursor),
                      selection = None,
                      selections = Nil,
                      preferredColumn = Some(newCursor.column),
                      preferredXPx = None
                    ),
                    findState = updatedFindState
                  )
                  recordWorkflowUndo(state, bufferId, buffer) >> stateRef.update { current =>
                    val updatedState = current.copy(
                      persisted =
                        current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> updatedBuffer)),
                      runtime =
                        current.runtime.copy(uiSurfaces = current.runtime.uiSurfaces.filterNot(_.id == surfaceId))
                    )
                    current.persisted.layout.activeEditorPaneId match
                      case Some(paneId) =>
                        updatedState.copy(persisted = updatedState.persisted.copy(focus = Focus.EditorPane(paneId)))
                      case None => updatedState
                  }
          case None =>
            updateReplaceWorkflowSurface(
              surfaceId,
              workflow.copy(statusMessage = Some("No active buffer"))
            )

  private def submitReplaceNextEffect(surfaceId: SurfaceId, workflow: ReplaceWorkflowState, state: AppState): IO[Unit] =
    activeEditorBufferId(state) match
      case None =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("No active buffer"))
        )
      case Some(_) if workflow.findText.isEmpty =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("Enter text to find"))
        )
      case Some(bufferId) =>
        state.persisted.buffers.get(bufferId) match
          case Some(buffer) =>
            workflow.selectedScope.resolve(buffer.primarySelection, offsetForCursor(buffer.document.content, _)) match
              case Left(error) =>
                updateReplaceWorkflowSurface(
                  surfaceId,
                  workflow.copy(statusMessage = Some(error.message))
                )
              case Right(range) =>
                val matches = scopedReplaceMatches(buffer, workflow.findText, range)
                if matches.isEmpty then
                  updateReplaceWorkflowSurface(
                    surfaceId,
                    workflow.copy(statusMessage = Some("No matches found"))
                  )
                else
                  val startOffset = nextReplaceMatchOffset(buffer, matches)
                  val endOffset   = startOffset + workflow.findText.length
                  val updatedContent = buffer.document.content
                    .delete(startOffset, endOffset)
                    .insert(startOffset, workflow.replacementText)
                  val cursorOffset = startOffset + workflow.replacementText.length
                  val newCursor    = cursorPositionForOffset(updatedContent, cursorOffset)
                  val updatedFindState =
                    refreshedFindStateAfterOffset(updatedContent, workflow.findText, cursorOffset)
                  val replacementSelection =
                    workflow.selectedScope match
                      case ReplaceWorkflowScope.Selection =>
                        buffer.primarySelection.map(selection =>
                          adjustSelectionAfterReplacement(
                            buffer = buffer,
                            updatedText = updatedContent.collect(),
                            selection = selection,
                            startOffset = startOffset,
                            endOffset = endOffset,
                            replacementLength = workflow.replacementText.length
                          )
                        )
                      case ReplaceWorkflowScope.CurrentBuffer =>
                        None
                  val updatedBuffer = buffer.copy(
                    document = buffer.document.copy(
                      content = updatedContent,
                      isDirty = true,
                      isNewEmpty = false
                    ),
                    editing = buffer.editing.copy(
                      cursors = List(newCursor),
                      selection = replacementSelection,
                      selections = Nil,
                      preferredColumn = Some(newCursor.column),
                      preferredXPx = None
                    ),
                    findState = updatedFindState
                  )
                  recordWorkflowUndo(state, bufferId, buffer) >> stateRef.update { current =>
                    current.copy(persisted =
                      current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> updatedBuffer))
                    )
                  } >> updateReplaceWorkflowSurface(
                    surfaceId,
                    workflow.copy(statusMessage = Some("Replaced next match"))
                  )
          case None =>
            updateReplaceWorkflowSurface(
              surfaceId,
              workflow.copy(statusMessage = Some("No active buffer"))
            )

  protected def refreshWorkflowState(workflow: FileWorkflowState): IO[FileWorkflowState] =
    if remoteWorkflowTarget(workflow).isDefined then
      IO.pure(
        workflow.updated(
          suggestions = Nil,
          selectedSuggestionIndex = 0,
          missingPathSegments = Nil,
          confirmCreateDirectories = false,
          statusMessage = None
        )
      )
    else
      for
        directoryPath <- workflowDirectoryPath(workflow)
        suggestions <- workflow match
          case openWorkflow: OpenFileWorkflowState =>
            openWorkflow.activeField match
              case FileWorkflowField.Path     => pathSuggestions(openWorkflow.path)
              case FileWorkflowField.Filename => filenameSuggestions(openWorkflow)
          case saveAsWorkflow: SaveAsFileWorkflowState =>
            saveAsWorkflow.activeField match
              case FileWorkflowField.Path     => pathSuggestions(saveAsWorkflow.path)
              case FileWorkflowField.Filename => IO.pure(Nil)
        missingSegments <- missingDirectorySegments(directoryPath)
      yield workflow.updated(
        suggestions = suggestions,
        selectedSuggestionIndex =
          if suggestions.isEmpty then 0 else math.min(workflow.selectedSuggestionIndex, suggestions.length - 1),
        missingPathSegments = missingSegments,
        confirmCreateDirectories = false,
        statusMessage = None
      )

  protected def pathSuggestions(pathInput: String): IO[List[FileWorkflowSuggestion]] =
    for
      currentDirectory <- FileUtils.getCurrentDirectory
      basePathInput = if pathInput.trim.isEmpty then currentDirectory.toString else pathInput
      resolvedPath    <- FileUtils.resolvePath(basePathInput)
      isDirectoryPath <- IO.blocking(Files.exists(resolvedPath) && Files.isDirectory(resolvedPath))
      endsWithSeparator = pathInput.endsWith("/") || pathInput.endsWith("\\")
      baseDirectory =
        if endsWithSeparator || isDirectoryPath then resolvedPath
        else Option(resolvedPath.getParent).getOrElse(currentDirectory)
      prefix =
        if endsWithSeparator || isDirectoryPath then ""
        else Option(resolvedPath.getFileName).map(_.toString).getOrElse("")
      entries <- fileManager.getFileBrowser.listDirectory(baseDirectory)
    yield entries
      .filter(_.isDirectory)
      .filter(entry => prefix.isEmpty || entry.name.toLowerCase.startsWith(prefix.toLowerCase))
      .map(entry => FileWorkflowSuggestion(entry.path.toString, isDirectory = true))

  protected def filenameSuggestions(workflow: OpenFileWorkflowState): IO[List[FileWorkflowSuggestion]] =
    for
      directoryPath <- workflowDirectoryPath(workflow)
      entries       <- fileManager.getFileBrowser.listDirectory(directoryPath)
    yield entries
      .filterNot(_.isDirectory)
      .filter(entry =>
        workflow.filename.trim.isEmpty || entry.name.toLowerCase.startsWith(workflow.filename.toLowerCase)
      )
      .filter(entry => FileUtils.isReadableFile(entry.path))
      .map(entry => FileWorkflowSuggestion(entry.name, isDirectory = false))

  protected def workflowDirectoryPath(workflow: FileWorkflowState): IO[Path] =
    if workflow.filename.trim.nonEmpty then FileUtils.resolvePath(workflow.path)
    else FileUtils.resolvePath(workflow.path).map(path => Option(path.getParent).getOrElse(path))

  protected def workflowTargetPath(workflow: FileWorkflowState): IO[Path] =
    if workflow.filename.trim.nonEmpty then
      FileUtils.resolvePath(workflow.path).map(_.resolve(workflow.filename.trim).normalize())
    else FileUtils.resolvePath(workflow.path)

  protected def missingDirectorySegments(directoryPath: Path): IO[List[String]] =
    IO.blocking {
      val normalized   = directoryPath.normalize()
      val segmentNames = (0 until normalized.getNameCount).toList.map(index => normalized.getName(index).toString)
      val initialPath =
        Option(normalized.getRoot).getOrElse(Paths.get(""))

      segmentNames
        .foldLeft((initialPath, false, List.empty[String])) {
          case ((currentPath, alreadyMissing, missing), segment) =>
            val nextPath =
              if currentPath.toString.isEmpty then Paths.get(segment)
              else currentPath.resolve(segment)
            val nextMissing =
              if alreadyMissing || !Files.exists(nextPath) then missing :+ segment
              else missing
            val nextAlreadyMissing = alreadyMissing || !Files.exists(nextPath)
            (nextPath, nextAlreadyMissing, nextMissing)
        }
        ._3
    }

  protected def completeOpenWorkflow(surfaceId: SurfaceId, workflow: OpenFileWorkflowState): IO[Unit] =
    remoteWorkflowTarget(workflow) match
      case Some(remoteTarget) =>
        updateFileWorkflowSurface(surfaceId, workflow.updated(statusMessage = Some(remoteStorageMessage(remoteTarget))))
      case None =>
        workflowTargetPath(workflow).flatMap { targetPath =>
          IO.blocking(FileUtils.isReadableFile(targetPath)).flatMap {
            case false =>
              updateFileWorkflowSurface(
                surfaceId,
                workflow.updated(statusMessage = Some(s"File not found: $targetPath"))
              ) >>
                logger.debug(s"[FILE-WORKFLOW] Open target is not readable: $targetPath")
            case true =>
              stateRef
                .modify { state =>
                  val bufferId = state.runtime.nextBufferId
                  (state.copy(runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))), bufferId)
                }
                .flatMap(bufferId => fileManager.loadFile(targetPath, bufferId))
                .flatMap { loadedBuffer =>
                  stateRef.modify { state =>
                    val newBufferId = loadedBuffer.id
                    val stateWithBuffer = state.copy(
                      persisted = state.persisted.copy(
                        buffers = state.persisted.buffers + (newBufferId -> loadedBuffer),
                        recentFiles = trackRecentFile(state.persisted.recentFiles, targetPath)
                      ),
                      runtime = state.runtime.copy(uiSurfaces = List.empty)
                    )
                    val updatedState = EditorState.insertBufferInOrder(stateWithBuffer, newBufferId)
                    val rebalanced   = EditorState.rebalancePanes(updatedState, Some(newBufferId))
                    val focused      = EditorState.focusBuffer(rebalanced, newBufferId)
                    val resized =
                      focused.runtime.viewportSize
                        .map(viewportSize => LayoutEngine.syncViewportDimensions(focused, viewportSize))
                        .getOrElse(focused)
                    (resized, ())
                  }
                }
                .handleErrorWith(ex => logger.error(ex)(s"[FILE-WORKFLOW] Failed to open $targetPath"))
          }
        }

  protected def completeSaveAsWorkflow(
    surfaceId: SurfaceId,
    workflow: SaveAsFileWorkflowState,
    state: AppState
  ): IO[Unit] =
    activeEditorBufferId(state) match
      case Some(bufferId) =>
        remoteWorkflowTarget(workflow) match
          case Some(remoteTarget) =>
            updateFileWorkflowSurface(
              surfaceId,
              workflow.updated(statusMessage = Some(remoteStorageMessage(remoteTarget)))
            )
          case None if workflow.missingPathSegments.nonEmpty && !workflow.confirmCreateDirectories =>
            updateFileWorkflowSurface(surfaceId, workflow.updated(confirmCreateDirectories = true))
          case None =>
            workflowTargetPath(workflow).flatMap { targetPath =>
              saveBufferAsEffect(bufferId, targetPath)
                .flatMap { _ =>
                  stateRef.get.flatMap { savedState =>
                    savedState.runtime.actionStack.collectFirst {
                      case AppAction.CloseWorkflow(closeWorkflow) => closeWorkflow
                    } match
                      case Some(closeWorkflow) if closeWorkflow.currentBufferId == bufferId =>
                        val dismissedState = dismissModalSurface(savedState)
                        val nextState =
                          if closeWorkflow.scope == CloseScope.Quit then dismissedState
                          else closeBufferUsingExistingFlow(dismissedState, bufferId)
                        stateRef.set(nextState) >> continueCloseWorkflow(closeWorkflow, nextState)
                      case _ =>
                        dismissSurfaceAndFocusEditor(surfaceId)
                  }
                }
                .handleErrorWith { error =>
                  updateFileWorkflowSurface(
                    surfaceId,
                    workflow.updated(statusMessage = Some(saveFailureMessage(error)))
                  )
                }
            }
      case None =>
        logger.debug("[FILE-WORKFLOW] No focused buffer available for save-as")

  private def saveFailureMessage(error: Throwable): String =
    s"Could not save: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"

  private def remoteWorkflowTarget(workflow: FileWorkflowState): Option[String] =
    val filenameInput = workflow.filename.trim
    val pathInput     = workflow.path.trim
    if isRemoteStorageInput(filenameInput) then Some(filenameInput)
    else if isRemoteStorageInput(pathInput) then
      Some(
        if filenameInput.isEmpty then pathInput
        else appendRemoteFilename(pathInput, filenameInput)
      )
    else None

  private def isRemoteStorageInput(value: String): Boolean =
    StorageLocation.parse(value).exists(_.isRemote)

  private def appendRemoteFilename(remoteBase: String, filename: String): String =
    if remoteBase.endsWith("/") then s"$remoteBase$filename"
    else s"$remoteBase/$filename"

  private def remoteStorageMessage(remoteTarget: String): String =
    s"Remote storage is not supported yet: $remoteTarget"

  private[manager] def requestSaveAsFileDialog(state: AppState, bufferIdOverride: Option[BufferId]): IO[Unit] =
    bufferIdOverride.orElse(state.focusedBufferId) match
      case Some(bufferId) =>
        fileDialog match
          case Some(dialog) =>
            val focusedPath = state.persisted.buffers.get(bufferId).flatMap(_.document.filePath)
            val initialDirectory =
              focusedPath
                .flatMap(path => Option(path.getParent).map(IO.pure))
                .getOrElse(FileUtils.getCurrentDirectory)
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
          stateRef.set(nextState) >> continueCloseWorkflow(closeWorkflow, nextState)
        case None =>
          IO.unit
    }

  private[manager] def activeEditorBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)

  protected def updateFileWorkflowSurface(surfaceId: SurfaceId, workflow: FileWorkflowState): IO[Unit] =
    stateRef.update { state =>
      state.surfaceById(surfaceId) match
        case Some(surface) =>
          val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)))
          state.copy(runtime =
            state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surfaceId) :+ updatedSurface)
          )
        case None =>
          state
    }

  protected def updateReplaceWorkflowSurface(surfaceId: SurfaceId, workflow: ReplaceWorkflowState): IO[Unit] =
    stateRef.update { state =>
      state.surfaceById(surfaceId) match
        case Some(surface) =>
          val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)))
          state.copy(runtime =
            state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surfaceId) :+ updatedSurface)
          )
        case None =>
          state
    }

  protected def dismissSurfaceAndFocusEditor(surfaceId: SurfaceId): IO[Unit] =
    stateRef.update { state =>
      val baseState =
        state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surfaceId)))
      state.persisted.layout.activeEditorPaneId match
        case Some(paneId) => baseState.copy(persisted = baseState.persisted.copy(focus = Focus.EditorPane(paneId)))
        case None         => baseState
    }

  protected def fileWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[(UiSurface, FileWorkflowState)] =
    state.surfaceById(surfaceId).flatMap { surface =>
      surface.content match
        case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some((surface, workflow))
        case _                                                          => None
    }

  private def nextReplaceMatchOffset(buffer: Buffer, matches: List[Int]): Int =
    val cursorOffset = buffer.editing.cursors.headOption
      .map(cursor => offsetForCursor(buffer.document.content, cursor))
      .getOrElse(0)
    matches.find(_ >= cursorOffset).getOrElse(matches.head)

  private def scopedReplaceMatches(
    buffer: Buffer,
    findText: String,
    range: ReplaceScopeRange
  ): List[Int] =
    buffer.document.content.searchAll(findText).filter { offset =>
      val insideScope = range match
        case ReplaceScopeRange.Selection(startOffset, endOffset) =>
          offset >= startOffset && (offset + findText.length) <= endOffset
        case ReplaceScopeRange.WholeBuffer =>
          true
      insideScope && isWholeGraphemeMatch(buffer.document.content, offset, findText.length)
    }

  private def refreshedFindState(
    content: com.serenity.rope.Rope,
    findText: String,
    requestedIndex: Int
  ): Option[FindState] =
    val results = content
      .searchAll(findText)
      .filter(offset => isWholeGraphemeMatch(content, offset, findText.length))
      .map(offset => cursorPositionForOffset(content, offset))
      .map(cursor => FindResult(cursor.line, cursor.column))
    val resultSet = FindResultSet.normalized(findText, results, requestedIndex)
    Option.when(resultSet.results.nonEmpty)(FindState.fromResultSet(resultSet))

  private def refreshedFindStateAfterOffset(
    content: com.serenity.rope.Rope,
    findText: String,
    offset: Int
  ): Option[FindState] =
    val matchOffsets =
      content.searchAll(findText).filter(found => isWholeGraphemeMatch(content, found, findText.length))
    val requestedIndex = matchOffsets.indexWhere(_ >= offset) match
      case -1    => 0
      case index => index
    refreshedFindState(content, findText, requestedIndex)

  private def replaceMatchesInRanges(
    rope: com.serenity.rope.Rope,
    matchOffsets: List[Int],
    findText: String,
    replacementText: String
  ): com.serenity.rope.Rope =
    matchOffsets.sorted.reverse.foldLeft(rope) { (current, offset) =>
      current.delete(offset, offset + findText.length).insert(offset, replacementText)
    }

  private def finalCursorOffsetAfterReplacements(
    matchOffsets: List[Int],
    findLength: Int,
    replacementLength: Int
  ): Int =
    matchOffsets.sorted
      .foldLeft((0, 0)) {
        case ((shift, _), offset) =>
          val replacementEnd = offset + shift + replacementLength
          val nextShift      = shift + replacementLength - findLength
          (nextShift, replacementEnd)
      }
      ._2

  private def adjustSelectionAfterReplacement(
    buffer: Buffer,
    updatedText: String,
    selection: Selection,
    startOffset: Int,
    endOffset: Int,
    replacementLength: Int
  ): Selection =
    val oldText = buffer.document.content.collect()
    val delta   = replacementLength - (endOffset - startOffset)

    def adjust(cursor: CursorPosition): CursorPosition =
      val oldOffset = offsetForCursor(oldText, cursor)
      val newOffset =
        if oldOffset <= startOffset then oldOffset
        else if oldOffset >= endOffset then oldOffset + delta
        else startOffset + replacementLength
      cursorPositionForOffset(updatedText, newOffset)

    Selection(adjust(selection.anchor), adjust(selection.focus))

  private def recordWorkflowUndo(bufferState: AppState, bufferId: BufferId, buffer: Buffer): IO[Unit] =
    bufferState.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        undoRef.update { undo =>
          val flushed = undo.flushPendingGroup
          val entry   = HistoryEntry(bufferId, paneId, BufferSnapshot.fromBuffer(buffer))
          flushed.pushUndo(entry)
        }
      case None =>
        IO.unit

  private def offsetForCursor(text: String, cursor: CursorPosition): Int =
    val linesBefore = text.split("\n", -1).take(cursor.line)
    val linePrefixLength =
      if linesBefore.isEmpty then 0
      else linesBefore.map(_.length).sum + linesBefore.length
    linePrefixLength + cursor.column

  private def offsetForCursor(content: com.serenity.rope.Rope, cursor: CursorPosition): Int =
    content.lineColumnToOffset(cursor.line, cursor.column)

  private def cursorPositionForOffset(text: String, offset: Int): CursorPosition =
    val clamped = math.max(0, math.min(offset, text.length))
    text.take(clamped).foldLeft(CursorPosition(0, 0)) { (cursor, char) =>
      if char == '\n' then CursorPosition(cursor.line + 1, 0)
      else cursor.copy(column = cursor.column + 1)
    }

  private def cursorPositionForOffset(content: com.serenity.rope.Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  private def isWholeGraphemeMatch(content: com.serenity.rope.Rope, offset: Int, length: Int): Boolean =
    TextEditing.isWholeGraphemeRange(RopeCharacterSource(content), offset, offset + length)

  final private case class RopeCharacterSource(content: com.serenity.rope.Rope) extends TextEditing.CharacterSource:
    override def length: Int =
      content.weight

    override def charAt(index: Int): Char =
      content.index(index).getOrElse('\u0000')

  protected def replaceWorkflowSurface(
    state: AppState,
    surfaceId: SurfaceId
  ): Option[(UiSurface, ReplaceWorkflowState)] =
    state.surfaceById(surfaceId).flatMap { surface =>
      surface.content match
        case SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)) => Some((surface, workflow))
        case _                                                             => None
    }

  protected def closeWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[(UiSurface, CloseWorkflowState)] =
    state.surfaceById(surfaceId).flatMap { surface =>
      surface.content match
        case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some((surface, workflow))
        case _                                                           => None
    }

  private[manager] def restoreStartupSession(): IO[Unit] =
    logger.info("[CMD] Session restore requested") >>
      loadSession().flatMap {
        case Some(restoredState) =>
          logger.info("[CMD] Session loaded successfully") >>
            updateState(current => restoreSessionIntoCurrentViewport(restoredState, current))
        case None =>
          logger.info("[CMD] No session found - creating default session") >>
            updateState(state => state.copy(runtime = state.runtime.copy(uiSurfaces = List.empty))) >>
            createNewEmptyBuffer().flatMap { bufferId =>
              updateState(s =>
                s.copy(persisted = s.persisted.copy(bufferOrder = s.persisted.bufferOrder :+ bufferId))
              ) >>
                createPane(Some(bufferId)).flatMap(paneId => switchToPane(paneId))
            }
      }

  private[manager] def createStartupSession(): IO[Unit] =
    updateState { state =>
      val opened = EditorState.openNewTab(state)
      opened.copy(runtime = opened.runtime.copy(uiSurfaces = List.empty))
    }

  private[manager] def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
    val restored = restoredState.copy(
      runtime = restoredState.runtime.copy(
        uiSurfaces = List.empty,
        viewportSize = currentState.runtime.viewportSize
      )
    )
    currentState.runtime.viewportSize
      .map(viewportSize => LayoutEngine.syncViewportDimensions(restored, viewportSize))
      .getOrElse(restored)
