package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.{IO, Ref}
import com.serenity.io.{FileManager, FileUtils, StorageLocation}
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.ui.layout.LayoutEngine
import org.typelevel.log4cats.Logger

/** Open/Save-As file workflow mechanics: opening the dialog, suggesting paths, and completing an Open. Save-As
  * completion stays with [[StateManagerWorkflowCapability]] because it must coordinate with an in-flight close
  * workflow (Save before close).
  */
final private[manager] class StateManagerFileWorkflow(
    stateRef: Ref[IO, AppState],
    logger: Logger[IO],
    fileManager: FileManager,
    validateAndUpdateState: (AppState, AppState) => IO[Unit],
    updateFileWorkflowSurface: (SurfaceId, FileWorkflowState) => IO[Unit],
    fileWorkflowSurface: (AppState, SurfaceId) => Option[(UiSurface, FileWorkflowState)]
)(using balance: com.serenity.rope.Balance):

  private def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)

  private[manager] def openFileWorkflowModal(
    mode: FileWorkflowMode,
    state: AppState,
    bufferIdOverride: Option[BufferId] = None,
    statusMessage: Option[String] = None
  ): IO[Unit] =
    val targetBufferId = bufferIdOverride.orElse(state.focusedBufferId)
    val targetBuffer   = targetBufferId.flatMap(id => state.persisted.buffers.get(id))
    val focusedPath    = targetBuffer.flatMap(_.document.filePath)
    val filename = mode match
      case FileWorkflowMode.SaveAs =>
        focusedPath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("")
      case FileWorkflowMode.Open => ""
    // Captured once at open time, not re-derived live: the buffer being saved cannot change out from under an open
    // save dialog, and `Open` never saves anything so it stays false regardless (issue #1253).
    val bufferHasRichFormatting =
      mode == FileWorkflowMode.SaveAs && targetBuffer.flatMap(_.richText.richTextDocument).exists(_.hasFormatting)

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
        activeField = if mode == FileWorkflowMode.Open then FileWorkflowField.Path else FileWorkflowField.Filename,
        statusMessage = statusMessage,
        bufferHasRichFormatting = bufferHasRichFormatting
      )
      val predictedState = ModalStateReducer.show(Modal.FileWorkflow(workflow), state).state
      logger.info(
        s"[FILE-WORKFLOW OPENED] mode=$mode filename=${workflow.filename} path=${workflow.path} " +
          s"surfaceId=${predictedState.modalSurface.map(_.id).getOrElse("none")} focus=${predictedState.persisted.focus}"
      ) >>
        stateRef.update(current => ModalStateReducer.show(Modal.FileWorkflow(workflow), current).state) >>
        // Populate the open dialog's directory listing immediately so it never appears as an empty, hung modal (#1289).
        IO.whenA(mode == FileWorkflowMode.Open)(
          stateRef.get.flatMap(_.modalSurface.fold(IO.unit)(surface => refreshFileWorkflowEffect(surface.id)))
        )
    }

  private[manager] def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      fileWorkflowSurface(state, surfaceId) match
        case Some((_, workflow)) =>
          refreshWorkflowState(workflow).flatMap(refreshed => updateFileWorkflowSurface(surfaceId, refreshed))
        case None =>
          IO.unit
    }

  private[manager] def submitFileWorkflowEffect(surfaceId: SurfaceId)(
    completeSaveAsWorkflow: (SurfaceId, SaveAsFileWorkflowState, AppState) => IO[Unit]
  ): IO[Unit] =
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
              case FileWorkflowField.Path     => pathSuggestions(openWorkflow.path, includeFiles = true)
              case FileWorkflowField.Filename => filenameSuggestions(openWorkflow)
              case FileWorkflowField.Format   => IO.pure(Nil)
          case saveAsWorkflow: SaveAsFileWorkflowState =>
            saveAsWorkflow.activeField match
              case FileWorkflowField.Path     => pathSuggestions(saveAsWorkflow.path)
              case FileWorkflowField.Filename => IO.pure(Nil)
              case FileWorkflowField.Format   => IO.pure(Nil)
        missingSegments <- missingDirectorySegments(directoryPath)
      yield workflow.updated(
        suggestions = suggestions,
        selectedSuggestionIndex =
          if suggestions.isEmpty then 0 else math.min(workflow.selectedSuggestionIndex, suggestions.length - 1),
        missingPathSegments = missingSegments,
        confirmCreateDirectories = false,
        statusMessage = None
      )

  // `includeFiles` makes the open dialog a full directory browser (files listed alongside directories); save-as lists
  // directories only, since its filename is typed separately (#1289).
  protected def pathSuggestions(pathInput: String, includeFiles: Boolean = false): IO[List[FileWorkflowSuggestion]] =
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
      entries <- fileManager.listDirectory(baseDirectory)
    yield entries
      .filter(entry => entry.isDirectory || (includeFiles && FileUtils.isReadableFile(entry.path)))
      .filter(entry => prefix.isEmpty || entry.name.toLowerCase.startsWith(prefix.toLowerCase))
      .map(entry => FileWorkflowSuggestion(entry.path.toString, isDirectory = entry.isDirectory))

  protected def filenameSuggestions(workflow: OpenFileWorkflowState): IO[List[FileWorkflowSuggestion]] =
    for
      directoryPath <- workflowDirectoryPath(workflow)
      entries       <- fileManager.listDirectory(directoryPath)
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

  private[manager] def workflowTargetPath(workflow: FileWorkflowState): IO[Path] =
    if workflow.filename.trim.nonEmpty then
      FileUtils.resolvePath(workflow.path).map(_.resolve(workflow.filename.trim).normalize())
    else FileUtils.resolvePath(workflow.path)

  protected def missingDirectorySegments(directoryPath: Path): IO[List[String]] =
    IO.blocking {
      val normalized   = directoryPath.normalize()
      val segmentNames = (0 until normalized.getNameCount).toList.map(index => normalized.getName(index).toString)
      val initialPath =
        Option(normalized.getRoot).getOrElse(java.nio.file.Paths.get(""))

      segmentNames
        .foldLeft((initialPath, false, List.empty[String])) {
          case ((currentPath, alreadyMissing, missing), segment) =>
            val nextPath =
              if currentPath.toString.isEmpty then java.nio.file.Paths.get(segment)
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
              // A path that resolves to a directory is a browse step, not a failure: descend into it and re-list, so
              // Enter walks the tree exactly like the suggestion listing does (#1289).
              IO.blocking(Files.isDirectory(targetPath)).flatMap {
                case true =>
                  updateFileWorkflowSurface(
                    surfaceId,
                    workflow.updated(path = targetPath.toString + java.io.File.separator, statusMessage = None)
                  ) >> refreshFileWorkflowEffect(surfaceId)
                case false =>
                  updateFileWorkflowSurface(
                    surfaceId,
                    workflow.updated(statusMessage = Some(s"File not found: $targetPath"))
                  ) >>
                    logger.debug(s"[FILE-WORKFLOW] Open target is not readable: $targetPath")
              }
            case true =>
              stateRef
                .modify { state =>
                  val bufferId = state.runtime.nextBufferId
                  (state.copy(runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))), bufferId)
                }
                .flatMap(bufferId => fileManager.loadFile(targetPath, bufferId))
                .flatMap { loadedBuffer =>
                  // Structural mutation (adds a buffer, reorders bufferOrder, reassigns pane focus): routed through
                  // the checked commit so a drifted `nextBufferId` (see #858) can't silently duplicate a
                  // bufferOrder entry or overwrite a live buffer instead of being rejected.
                  stateRef.get.flatMap { state =>
                    val newBufferId = loadedBuffer.id
                    val stateWithBuffer = state.copy(
                      persisted = state.persisted.copy(
                        buffers = state.persisted.buffers + (newBufferId -> loadedBuffer),
                        recentFiles = trackRecentFile(state.persisted.recentFiles, targetPath),
                        recentFilesByMode = Persisted.trackRecentFile(
                          state.persisted.recentFilesByMode,
                          state.persisted.config.appMode,
                          targetPath
                        )
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
                    validateAndUpdateState(resized, state)
                  }
                }
                .handleErrorWith(ex => logger.error(ex)(s"[FILE-WORKFLOW] Failed to open $targetPath"))
          }
        }

  /** The explicit, single-step counterpart to submitting twice (Enter to flag `missingPathSegments`, Enter again to
    * confirm): creates the missing directories -- as a side effect of performing the save itself, exactly like the
    * confirmed double-submit path -- immediately, without a second submit (issue #1253).
    */
  private[manager] def createFileWorkflowDirectoriesEffect(surfaceId: SurfaceId)(
    completeSaveAsWorkflow: (SurfaceId, SaveAsFileWorkflowState, AppState) => IO[Unit]
  ): IO[Unit] =
    stateRef.get.flatMap { state =>
      fileWorkflowSurface(state, surfaceId) match
        case Some((_, saveAsWorkflow: SaveAsFileWorkflowState)) if saveAsWorkflow.missingPathSegments.nonEmpty =>
          saveAsWorkflow.updated(confirmCreateDirectories = true) match
            case confirmed: SaveAsFileWorkflowState => completeSaveAsWorkflow(surfaceId, confirmed, state)
            case _                                  => IO.unit
        case _ =>
          IO.unit
    }

  private[manager] def saveFailureMessage(error: Throwable): String =
    s"Could not save: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"

  private[manager] def remoteWorkflowTarget(workflow: FileWorkflowState): Option[String] =
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

  private[manager] def remoteStorageMessage(remoteTarget: String): String =
    s"Remote storage is not supported yet: $remoteTarget"
