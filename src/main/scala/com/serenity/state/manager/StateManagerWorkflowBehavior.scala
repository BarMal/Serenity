package com.serenity.state.manager

import java.nio.file.{Files, Path, Paths}

import cats.effect.IO

import com.serenity.io.FileUtils
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry}

private[manager] trait StateManagerWorkflowBehavior extends StateManagerRuntimeSupport:
  this: StateManager =>

  protected def openFileWorkflowModal(
    mode: FileWorkflowMode,
    state: AppState,
    bufferIdOverride: Option[BufferId] = None
  ): IO[Unit] =
    val targetBufferId = bufferIdOverride.orElse(state.focusedBufferId)
    val focusedPath    = targetBufferId.flatMap(id => state.buffers.get(id)).flatMap(_.filePath)
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
      val workflow       = FileWorkflowState(mode = mode, filename = filename, path = basePath.toString)
      val predictedState = ModalStateReducer.show(Modal.FileWorkflow(workflow), state).state
      logger.info(
        s"[FILE-WORKFLOW OPENED] mode=$mode filename=${workflow.filename} path=${workflow.path} " +
          s"surfaceId=${predictedState.modalSurface.map(_.id).getOrElse("none")} focus=${predictedState.focus}"
      ) >>
        updateState(current => ModalStateReducer.show(Modal.FileWorkflow(workflow), current).state)
    }

  protected def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
    val targetBufferIds = closeTargets(scope, state)
    val dirtyBufferIds  = targetBufferIds.filter(bufferId => state.buffers.get(bufferId).exists(_.isDirty))
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
      case CloseScope.Current => state.focusedBufferId.toList
      case CloseScope.All     => state.bufferOrder
      case CloseScope.Others =>
        state.focusedBufferId match
          case Some(focused) => state.bufferOrder.filterNot(_ == focused)
          case None          => state.bufferOrder
      case CloseScope.Quit => state.bufferOrder

  protected def promptCloseWorkflow(state: AppState, workflow: CloseWorkflowState): IO[Unit] =
    val focusedState = focusBufferForWorkflow(state, workflow.currentBufferId)
    val modalState =
      ModalStateReducer.show(Modal.CloseWorkflow(workflow), withCloseAction(focusedState, workflow)).state
    stateRef.set(modalState)

  protected def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      closeWorkflowSurface(state, surfaceId) match
        case Some((_, workflow)) =>
          workflow.selectedChoice match
            case CloseWorkflowChoice.Cancel =>
              dismissSurfaceAndFocusEditor(surfaceId) >>
                stateRef.update(clearCloseActions)
            case CloseWorkflowChoice.Discard =>
              val dismissedState = clearCloseActions(dismissModalSurface(state))
              val nextState =
                if workflow.scope == CloseScope.Quit then dismissedState
                else closeBufferUsingExistingFlow(dismissedState, workflow.currentBufferId)
              stateRef.set(nextState) >> continueCloseWorkflow(workflow, nextState)
            case CloseWorkflowChoice.Save =>
              state.buffers.get(workflow.currentBufferId) match
                case Some(buffer) if buffer.filePath.isDefined =>
                  saveBufferEffect(workflow.currentBufferId) >>
                    stateRef.get.flatMap { savedState =>
                      val dismissedState = clearCloseActions(dismissModalSurface(savedState))
                      val nextState =
                        if workflow.scope == CloseScope.Quit then dismissedState
                        else closeBufferUsingExistingFlow(dismissedState, workflow.currentBufferId)
                      stateRef.set(nextState) >> continueCloseWorkflow(workflow, nextState)
                    }
                case Some(_) =>
                  val dismissedState = withCloseAction(dismissModalSurface(state), workflow)
                  stateRef.set(dismissedState) >>
                    openFileWorkflowModal(FileWorkflowMode.SaveAs, dismissedState, Some(workflow.currentBufferId))
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
    if closedState.layout.activeEditorPaneId.isDefined then closedState
    else ensureCommandRunnerSurface(closedState)

  protected def closeBufferLabel(state: AppState, bufferId: BufferId): String =
    state.buffers
      .get(bufferId)
      .flatMap(_.filePath.flatMap(path => Option(path.getFileName).map(_.toString)))
      .getOrElse(s"Buffer ${bufferId.value} - unsaved")

  protected def withCloseAction(state: AppState, workflow: CloseWorkflowState): AppState =
    state.copy(actionStack = AppAction.CloseWorkflow(workflow) :: clearCloseActions(state).actionStack)

  protected def clearCloseActions(state: AppState): AppState =
    state.copy(actionStack = Nil)

  protected def dismissModalSurface(state: AppState): AppState =
    state.copy(uiSurfaces = state.uiSurfaces.filterNot {
      case UiSurface(_, SurfaceContent.ModalWorkflow(_), _, _) => true
      case _                                                   => false
    })

  protected def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      fileWorkflowSurface(state, surfaceId) match
        case Some((_, workflow)) =>
          refreshWorkflowState(workflow).flatMap(refreshed => updateFileWorkflowSurface(surfaceId, refreshed))
        case None =>
          IO.unit
    }

  protected def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
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

  protected def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
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
      case Some(bufferId) if workflow.findText.isEmpty =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("Enter text to find"))
        )
      case Some(bufferId) =>
        state.buffers.get(bufferId) match
          case Some(buffer) =>
            replaceScopeRange(buffer, workflow) match
              case Left(message) =>
                updateReplaceWorkflowSurface(
                  surfaceId,
                  workflow.copy(statusMessage = Some(message))
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
                      rope = buffer.content,
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
                  val newCursor = cursorPositionForOffset(updatedContent.collect(), cursorOffset)
                  val updatedBuffer = buffer.copy(
                    content = updatedContent,
                    isDirty = true,
                    isNewEmpty = false,
                    cursors = List(newCursor),
                    selection = None,
                    selections = Nil,
                    preferredColumn = Some(newCursor.column),
                    preferredXPx = None,
                    findState = None
                  )
                  recordWorkflowUndo(state, bufferId, buffer) >> stateRef.update { current =>
                    val updatedState = current.copy(
                      buffers = current.buffers + (bufferId -> updatedBuffer),
                      uiSurfaces = current.uiSurfaces.filterNot(_.id == surfaceId)
                    )
                    current.layout.activeEditorPaneId match
                      case Some(paneId) => updatedState.copy(focus = Focus.EditorPane(paneId))
                      case None         => updatedState
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
      case Some(bufferId) if workflow.findText.isEmpty =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("Enter text to find"))
        )
      case Some(bufferId) =>
        state.buffers.get(bufferId) match
          case Some(buffer) =>
            replaceScopeRange(buffer, workflow) match
              case Left(message) =>
                updateReplaceWorkflowSurface(
                  surfaceId,
                  workflow.copy(statusMessage = Some(message))
                )
              case Right(range) =>
                val matches = scopedReplaceMatches(buffer, workflow.findText, range)
                if matches.isEmpty then
                  updateReplaceWorkflowSurface(
                    surfaceId,
                    workflow.copy(statusMessage = Some("No matches found"))
                  )
                else
                  val startOffset = nextReplaceMatchOffset(buffer, workflow.findText, matches)
                  val endOffset   = startOffset + workflow.findText.length
                  val updatedContent = buffer.content
                    .delete(startOffset, endOffset)
                    .insert(startOffset, workflow.replacementText)
                  val cursorOffset = startOffset + workflow.replacementText.length
                  val newCursor    = cursorPositionForOffset(updatedContent.collect(), cursorOffset)
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
                    content = updatedContent,
                    isDirty = true,
                    isNewEmpty = false,
                    cursors = List(newCursor),
                    selection = replacementSelection,
                    selections = Nil,
                    preferredColumn = Some(newCursor.column),
                    preferredXPx = None,
                    findState = None
                  )
                  recordWorkflowUndo(state, bufferId, buffer) >> stateRef.update { current =>
                    current.copy(buffers = current.buffers + (bufferId -> updatedBuffer))
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
    workflowTargetPath(workflow).flatMap { targetPath =>
      if !FileUtils.isReadableFile(targetPath) then
        updateFileWorkflowSurface(
          surfaceId,
          workflow.updated(statusMessage = Some(s"File not found: $targetPath"))
        ) >>
          logger.debug(s"[FILE-WORKFLOW] Open target is not readable: $targetPath")
      else
        fileManager
          .loadFile(targetPath)
          .flatMap { loadedBuffer =>
            stateRef.modify { state =>
              val newBufferId    = state.nextBufferId
              val bufferToInsert = loadedBuffer.copy(id = newBufferId)
              val stateWithBuffer = state.copy(
                buffers = state.buffers + (newBufferId -> bufferToInsert),
                nextBufferId = BufferId(newBufferId.value + 1),
                uiSurfaces = List.empty
              )
              val updatedState = EditorState.insertBufferInOrder(stateWithBuffer, newBufferId)
              val rebalanced   = EditorState.rebalancePanes(updatedState, Some(newBufferId))
              val focused      = EditorState.focusBuffer(rebalanced, newBufferId)
              (focused, ())
            }
          }
          .handleErrorWith(ex => logger.error(ex)(s"[FILE-WORKFLOW] Failed to open $targetPath"))
    }

  protected def completeSaveAsWorkflow(
    surfaceId: SurfaceId,
    workflow: SaveAsFileWorkflowState,
    state: AppState
  ): IO[Unit] =
    activeEditorBufferId(state) match
      case Some(bufferId) =>
        if workflow.missingPathSegments.nonEmpty && !workflow.confirmCreateDirectories then
          updateFileWorkflowSurface(surfaceId, workflow.updated(confirmCreateDirectories = true))
        else
          workflowTargetPath(workflow).flatMap { targetPath =>
            saveBufferAsEffect(bufferId, targetPath) >>
              stateRef.get.flatMap { savedState =>
                savedState.actionStack.collectFirst {
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
      case None =>
        logger.debug("[FILE-WORKFLOW] No focused buffer available for save-as")

  protected def activeEditorBufferId(state: AppState): Option[BufferId] =
    state.layout.activeEditorPaneId
      .flatMap(state.layout.editorPanes.get)
      .flatMap(_.bufferId)

  protected def updateFileWorkflowSurface(surfaceId: SurfaceId, workflow: FileWorkflowState): IO[Unit] =
    stateRef.update { state =>
      state.surfaceById(surfaceId) match
        case Some(surface) =>
          val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)))
          state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surfaceId) :+ updatedSurface)
        case None =>
          state
    }

  protected def updateReplaceWorkflowSurface(surfaceId: SurfaceId, workflow: ReplaceWorkflowState): IO[Unit] =
    stateRef.update { state =>
      state.surfaceById(surfaceId) match
        case Some(surface) =>
          val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)))
          state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surfaceId) :+ updatedSurface)
        case None =>
          state
    }

  protected def dismissSurfaceAndFocusEditor(surfaceId: SurfaceId): IO[Unit] =
    stateRef.update { state =>
      val baseState = state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surfaceId))
      state.layout.activeEditorPaneId match
        case Some(paneId) => baseState.copy(focus = Focus.EditorPane(paneId))
        case None         => baseState
    }

  protected def fileWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[(UiSurface, FileWorkflowState)] =
    state.surfaceById(surfaceId).flatMap { surface =>
      surface.content match
        case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some((surface, workflow))
        case _                                                          => None
    }

  private def nextReplaceMatchOffset(buffer: Buffer, findText: String, matches: List[Int]): Int =
    val cursorOffset = buffer.cursors.headOption
      .map(cursor => offsetForCursor(buffer.content.collect(), cursor))
      .getOrElse(0)
    matches.find(_ >= cursorOffset).getOrElse(matches.head)

  private def replaceScopeRange(
    buffer: Buffer,
    workflow: ReplaceWorkflowState
  ): Either[String, Option[(Int, Int)]] =
    workflow.selectedScope match
      case ReplaceWorkflowScope.CurrentBuffer =>
        Right(None)
      case ReplaceWorkflowScope.Selection =>
        buffer.primarySelection match
          case Some(selection) =>
            val text        = buffer.content.collect()
            val startOffset = offsetForCursor(text, selection.start)
            val endOffset   = offsetForCursor(text, selection.end)
            Right(Some((startOffset, endOffset)))
          case None =>
            Left("Select text to limit replacement")

  private def scopedReplaceMatches(
    buffer: Buffer,
    findText: String,
    range: Option[(Int, Int)]
  ): List[Int] =
    buffer.content.searchAll(findText).filter { offset =>
      range match
        case Some((startOffset, endOffset)) =>
          offset >= startOffset && (offset + findText.length) <= endOffset
        case None =>
          true
    }

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
    val oldText = buffer.content.collect()
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
    bufferState.layout.activeEditorPaneId match
      case Some(paneId) =>
        undoRef.update { undo =>
          val flushed = undo.flushPendingGroup
          val entry   = HistoryEntry(bufferId, paneId, BufferSnapshot.fromBuffer(buffer))
          flushed.copy(undoStack = entry :: flushed.undoStack, redoStack = Nil)
        }
      case None =>
        IO.unit

  private def offsetForCursor(text: String, cursor: CursorPosition): Int =
    val linesBefore = text.split("\n", -1).take(cursor.line)
    val linePrefixLength =
      if linesBefore.isEmpty then 0
      else linesBefore.map(_.length).sum + linesBefore.length
    linePrefixLength + cursor.column

  private def cursorPositionForOffset(text: String, offset: Int): CursorPosition =
    val clamped = math.max(0, math.min(offset, text.length))
    text.take(clamped).foldLeft(CursorPosition(0, 0)) { (cursor, char) =>
      if char == '\n' then CursorPosition(cursor.line + 1, 0)
      else cursor.copy(column = cursor.column + 1)
    }

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

  protected def restoreStartupSession(): IO[Unit] =
    logger.info("[CMD] Session restore requested") >>
      loadSession().flatMap {
        case Some(restoredState) =>
          logger.info("[CMD] Session loaded successfully") >>
            updateState(_ => restoredState.copy(uiSurfaces = List.empty))
        case None =>
          logger.info("[CMD] No session found - creating default session") >>
            updateState(_.copy(uiSurfaces = List.empty)) >>
            createNewEmptyBuffer().flatMap { bufferId =>
              updateState(s => s.copy(bufferOrder = s.bufferOrder :+ bufferId)) >>
                createPane(Some(bufferId)).flatMap(paneId => switchToPane(paneId))
            }
      }

  protected def createStartupSession(): IO[Unit] =
    updateState(_.copy(uiSurfaces = List.empty)) >>
      createNewEmptyBuffer().flatMap { bufferId =>
        updateState(s => s.copy(bufferOrder = s.bufferOrder :+ bufferId)) >>
          createPane(Some(bufferId)).flatMap(paneId => switchToPane(paneId))
      }
