package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.IO
import com.serenity.io.{FileManager, FileUtils, StorageLocation}
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import org.typelevel.log4cats.Logger

/** Open/Save-As file workflow mechanics: opening the dialog, suggesting paths, and completing both an Open and a
  * Save-As.
  *
  * Directory listings run on a `Directory` switch-latest lane, and an Open's target is checked on a `Directory`
  * sequential lane; both come back as `EffectResult`s applied only while the dialog still shows the input they were
  * computed for (see [[FileWorkflowTransitions]]). A Save-As waits for its write, on the target's file lane, because
  * what follows depends on whether it landed.
  *
  * A Save-As can be the "Save before close" step of an in-flight close workflow, which this class deliberately knows
  * nothing about: `afterSaveAsCompleted` is the one-way hand-off its owner supplies to decide what follows a successful
  * save (resume the close workflow, or simply dismiss the dialog).
  */
final private[manager] class StateManagerFileWorkflow(
    currentState: IO[AppState],
    logger: Logger[IO],
    fileManager: FileManager,
    commitState: (AppState, AppState) => IO[Unit],
    lanes: EffectLanePort,
    openFile: Path => IO[Unit],
    missingDirectoriesBeforeSave: (Path, IO[List[String]]) => IO[List[String]],
    saveBufferAs: (BufferId, Path) => IO[Unit],
    afterSaveAsCompleted: (SurfaceId, BufferId) => IO[Unit]
):
  import FileWorkflowTransitions.{fileDialog, withFileDialog, withStatus}

  private def commit(transition: AppState => AppState): IO[Unit] =
    currentState.flatMap(current => commitState(transition(current), current))

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
      currentState.flatMap { current =>
        val shown = ModalStateReducer.show(Modal.FileWorkflow(workflow), current).state
        logger.info(
          s"[FILE-WORKFLOW OPENED] mode=$mode filename=${workflow.filename} path=${workflow.path} " +
            s"surfaceId=${shown.topModal.map(_.id).getOrElse("none")} focus=${shown.persisted.focus}"
        ) >> commitState(shown, current) >>
          // Populate the open dialog's directory listing immediately so it never appears as an empty, hung modal (#1289).
          IO.whenA(mode == FileWorkflowMode.Open)(
            currentState.flatMap(_.topModal.fold(IO.unit)(dialog => refreshFileWorkflowEffect(dialog.id)))
          )
      }
    }

  private[manager] def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    currentState.flatMap(fileDialog(_, surfaceId).fold(IO.unit)(requestListing(surfaceId, _)))

  /** A newer listing of the same directory supersedes an older one; one of another directory is dropped on arrival if
    * the dialog has moved on by then.
    */
  private def requestListing(surfaceId: SurfaceId, workflow: FileWorkflowState): IO[Unit] =
    if remoteWorkflowTarget(workflow).isDefined then
      commit(withFileDialog(_, surfaceId, FileWorkflowTransitions.refreshed(workflow, FileWorkflowListing(Nil, Nil))))
    else
      workflowDirectoryPath(workflow).flatMap { directory =>
        lanes.submitEffect(
          Lane.Keyed(LaneKey.Directory(directory.normalize()), LanePolicy.SwitchLatest),
          listing(workflow).flatMap(found =>
            lanes.dispatchEffectResult(EffectResult.FileWorkflowListed(surfaceId, workflow, found), _ => IO.unit)
          )
        )
      }

  private[manager] def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    currentState.flatMap { state =>
      fileDialog(state, surfaceId) match
        case Some(openWorkflow: OpenFileWorkflowState) =>
          completeOpenWorkflow(surfaceId, openWorkflow)
        case Some(saveAsWorkflow: SaveAsFileWorkflowState) =>
          completeSaveAsWorkflow(surfaceId, saveAsWorkflow, state)
        case None =>
          IO.unit
    }

  private def listing(workflow: FileWorkflowState): IO[FileWorkflowListing] =
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
    yield FileWorkflowListing(suggestions, missingSegments)

  // `includeFiles` makes the open dialog a full directory browser (files listed alongside directories); save-as lists
  // directories only, since its filename is typed separately (#1289).
  private def pathSuggestions(pathInput: String, includeFiles: Boolean = false): IO[List[FileWorkflowSuggestion]] =
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

  private def filenameSuggestions(workflow: OpenFileWorkflowState): IO[List[FileWorkflowSuggestion]] =
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

  private def workflowDirectoryPath(workflow: FileWorkflowState): IO[Path] =
    if workflow.filename.trim.nonEmpty then FileUtils.resolvePath(workflow.path)
    else FileUtils.resolvePath(workflow.path).map(path => Option(path.getParent).getOrElse(path))

  private def workflowTargetPath(workflow: FileWorkflowState): IO[Path] =
    if workflow.filename.trim.nonEmpty then
      FileUtils.resolvePath(workflow.path).map(_.resolve(workflow.filename.trim).normalize())
    else FileUtils.resolvePath(workflow.path)

  private def missingDirectorySegments(directoryPath: Path): IO[List[String]] =
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

  /** Checks the target off the dispatcher; the dialog then closes once the file has loaded, browses into a directory,
    * or reports a missing file.
    */
  private def completeOpenWorkflow(surfaceId: SurfaceId, workflow: OpenFileWorkflowState): IO[Unit] =
    remoteWorkflowTarget(workflow) match
      case Some(remoteTarget) =>
        commit(withStatus(_, surfaceId, workflow, remoteStorageMessage(remoteTarget)))
      case None =>
        workflowTargetPath(workflow).flatMap { targetPath =>
          lanes.submitEffect(
            targetLane(targetPath),
            openTarget(targetPath).flatMap { target =>
              val resolved = EffectResult.FileWorkflowTargetResolved(surfaceId, workflow, target)
              target match
                // Loaded before the dialog closes, so the load's own merge -- which picks a free buffer id -- is what
                // commits first, even from a state whose next buffer id has drifted onto a live buffer (#858).
                case FileWorkflowTarget.ReadableFile(path) =>
                  openFile(path) >> lanes.dispatchEffectResult(resolved, _ => IO.unit)
                case _ =>
                  lanes.dispatchEffectResult(resolved, committed => afterTargetResolved(surfaceId, target, committed))
            }
          )
        }

  private def openTarget(targetPath: Path): IO[FileWorkflowTarget] =
    IO.blocking(
      if FileUtils.isReadableFile(targetPath) then FileWorkflowTarget.ReadableFile(targetPath)
      else if Files.isDirectory(targetPath) then FileWorkflowTarget.Directory(targetPath)
      else FileWorkflowTarget.Missing(targetPath)
    )

  private def afterTargetResolved(surfaceId: SurfaceId, target: FileWorkflowTarget, committed: AppState): IO[Unit] =
    target match
      // A path that resolves to a directory is a browse step, not a failure: re-list it, so Enter walks the tree
      // exactly like the suggestion listing does (#1289).
      case FileWorkflowTarget.Directory(_) =>
        fileDialog(committed, surfaceId).fold(IO.unit)(requestListing(surfaceId, _))
      case FileWorkflowTarget.Missing(path)   => logger.debug(s"[FILE-WORKFLOW] Open target is not readable: $path")
      case FileWorkflowTarget.ReadableFile(_) => IO.unit

  /** Opens the directory an Open dialog is targeting as a project root (issue #1525), reporting back into the
    * still-open dialog -- exactly like `completeOpenWorkflow` -- when the target is remote storage or isn't actually a
    * directory. On a confirmed directory the dialog closes and `openProjectRoot`, which the owner supplies, takes the
    * path.
    */
  private[manager] def openAsProjectRoot(surfaceId: SurfaceId, openProjectRoot: Path => IO[Unit]): IO[Unit] =
    currentState.flatMap { state =>
      fileDialog(state, surfaceId) match
        case Some(openWorkflow: OpenFileWorkflowState) =>
          remoteWorkflowTarget(openWorkflow) match
            case Some(remoteTarget) =>
              commit(withStatus(_, surfaceId, openWorkflow, remoteStorageMessage(remoteTarget)))
            case None =>
              workflowTargetPath(openWorkflow).flatMap { targetPath =>
                lanes.submitEffect(
                  targetLane(targetPath),
                  IO.blocking(Files.isDirectory(targetPath))
                    .flatMap(isDirectory =>
                      lanes.dispatchEffectResult(
                        EffectResult.FileWorkflowProjectRootResolved(surfaceId, openWorkflow, targetPath, isDirectory),
                        _ => IO.whenA(isDirectory)(openProjectRoot(targetPath))
                      )
                    )
                )
              }
        case _ =>
          IO.unit
    }

  /** The explicit, single-step counterpart to submitting twice (Enter to flag `missingPathSegments`, Enter again to
    * confirm): creates the missing directories -- as a side effect of performing the save itself, exactly like the
    * confirmed double-submit path -- immediately, without a second submit (issue #1253).
    */
  private[manager] def createFileWorkflowDirectoriesEffect(surfaceId: SurfaceId): IO[Unit] =
    currentState.flatMap { state =>
      fileDialog(state, surfaceId) match
        case Some(saveAsWorkflow: SaveAsFileWorkflowState) if saveAsWorkflow.missingPathSegments.nonEmpty =>
          saveAsWorkflow.updated(confirmCreateDirectories = true) match
            case confirmed: SaveAsFileWorkflowState => completeSaveAsWorkflow(surfaceId, confirmed, state)
            case _                                  => IO.unit
        case _ =>
          IO.unit
    }

  /** The missing directories are re-checked just before the write, on the target's file lane, rather than trusted from
    * the dialog's listing: that listing arrives asynchronously and may predate the path being submitted.
    */
  private def completeSaveAsWorkflow(
    surfaceId: SurfaceId,
    workflow: SaveAsFileWorkflowState,
    state: AppState
  ): IO[Unit] =
    WorkflowSurfaces.activeEditorBufferId(state) match
      case Some(bufferId) =>
        remoteWorkflowTarget(workflow) match
          case Some(remoteTarget) =>
            commit(withStatus(_, surfaceId, workflow, remoteStorageMessage(remoteTarget)))
          case None =>
            workflowTargetPath(workflow).flatMap { targetPath =>
              workflowDirectoryPath(workflow)
                .flatMap(directory => missingDirectoriesBeforeSave(targetPath, missingDirectorySegments(directory)))
                .flatMap {
                  case missing if missing.nonEmpty && !workflow.confirmCreateDirectories =>
                    commit(
                      withFileDialog(
                        _,
                        surfaceId,
                        workflow.updated(missingPathSegments = missing, confirmCreateDirectories = true)
                      )
                    )
                  case _ =>
                    saveBufferAs(bufferId, targetPath)
                      .flatMap(_ => afterSaveAsCompleted(surfaceId, bufferId))
                      .handleErrorWith(error => commit(withStatus(_, surfaceId, workflow, saveFailureMessage(error))))
                }
            }
      case None =>
        logger.debug("[FILE-WORKFLOW] No focused buffer available for save-as")

  private def targetLane(target: Path): Lane.Keyed =
    Lane.Keyed(LaneKey.Directory(target.normalize()), LanePolicy.Sequential)

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
