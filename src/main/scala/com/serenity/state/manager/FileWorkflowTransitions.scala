package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.state.models.*

/** What a file dialog's directory listing found for the input it was taken for. */
final private[manager] case class FileWorkflowListing(
    suggestions: List[FileWorkflowSuggestion],
    missingPathSegments: List[String]
)

/** What an Open dialog's target turned out to be on disk. */
private[manager] enum FileWorkflowTarget:
  case ReadableFile(path: Path)
  case Directory(path: Path)
  case Missing(path: Path)

/** The Open/Save-As dialog's state changes as pure functions (#1697 Wave 3). Listings and target checks read the disk
  * on a lane; each result applies only while the dialog still shows the input it was computed for, so a listing for a
  * path the user has since typed past is dropped.
  */
private[manager] object FileWorkflowTransitions:

  def fileDialog(state: AppState, surfaceId: SurfaceId): Option[FileWorkflowState] =
    state.runtime.modalStack.find(_.id == surfaceId).collect {
      case ModalDialog(_, Modal.FileWorkflow(workflow), _) => workflow
    }

  def withFileDialog(state: AppState, surfaceId: SurfaceId, workflow: FileWorkflowState): AppState =
    if state.runtime.modalStack.exists(_.id == surfaceId) then
      state.copy(runtime =
        state.runtime.copy(modalStack =
          state.runtime.modalStack.map(dialog =>
            if dialog.id == surfaceId then dialog.copy(modal = Modal.FileWorkflow(workflow)) else dialog
          )
        )
      )
    else state

  def withStatus(state: AppState, surfaceId: SurfaceId, workflow: FileWorkflowState, message: String): AppState =
    withFileDialog(state, surfaceId, workflow.updated(statusMessage = Some(message)))

  /** A refreshed dialog shows no status and no pending create-directories confirmation. */
  def refreshed(workflow: FileWorkflowState, listing: FileWorkflowListing): FileWorkflowState =
    workflow.updated(
      suggestions = listing.suggestions,
      selectedSuggestionIndex =
        if listing.suggestions.isEmpty then 0
        else math.min(workflow.selectedSuggestionIndex, listing.suggestions.length - 1),
      missingPathSegments = listing.missingPathSegments,
      confirmCreateDirectories = false,
      statusMessage = None
    )

  def withListing(
    state: AppState,
    surfaceId: SurfaceId,
    requested: FileWorkflowState,
    listing: FileWorkflowListing
  ): AppState =
    showing(state, surfaceId, requested).fold(state)(current =>
      withFileDialog(state, surfaceId, refreshed(current, listing))
    )

  /** A readable file, already loaded, closes the dialog and leaves the editor focused; a directory is browsed into. */
  def withTargetResolved(
    state: AppState,
    surfaceId: SurfaceId,
    requested: FileWorkflowState,
    target: FileWorkflowTarget
  ): AppState =
    showing(state, surfaceId, requested).fold(state) { current =>
      target match
        case FileWorkflowTarget.ReadableFile(_) =>
          WorkflowSurfaces.dismissedToEditor(state, surfaceId)
        case FileWorkflowTarget.Directory(path) =>
          withFileDialog(
            state,
            surfaceId,
            current.updated(path = path.toString + java.io.File.separator, statusMessage = None)
          )
        case FileWorkflowTarget.Missing(path) =>
          withStatus(state, surfaceId, current, s"File not found: $path")
    }

  def withProjectRootResolved(
    state: AppState,
    surfaceId: SurfaceId,
    requested: FileWorkflowState,
    target: Path,
    isDirectory: Boolean
  ): AppState =
    showing(state, surfaceId, requested).fold(state)(current =>
      if isDirectory then WorkflowSurfaces.dismissedToEditor(state, surfaceId)
      else withStatus(state, surfaceId, current, s"Not a directory: $target")
    )

  /** The dialog `surfaceId`, if it still holds the input `requested` was taken from. */
  private def showing(state: AppState, surfaceId: SurfaceId, requested: FileWorkflowState): Option[FileWorkflowState] =
    fileDialog(state, surfaceId).filter(current =>
      current.mode == requested.mode && current.path == requested.path && current.filename == requested.filename &&
        current.activeField == requested.activeField
    )
