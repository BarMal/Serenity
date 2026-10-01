package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.io.ProjectFileListing
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer

/** The "Go to File" finder (see [[FileFinder]]) as pure functions of the state; the walk of the project runs on the
  * ProjectFiles lane.
  */
private[manager] object FileFinderTransitions:

  /** The finder over `root`, opened still loading, and its surface id -- none while a blocking modal holds the screen.
    */
  def withFinderOpened(state: AppState, root: Path): Option[(AppState, SurfaceId)] =
    val opened = ModalStateReducer.show(Modal.ListPicker(FileFinder.picker(root)), state).state
    opened.runtime.uiSurfaces.map(_.id).find(id => !state.runtime.uiSurfaces.exists(_.id == id)).map(opened -> _)

  /** Fills the finder `pickerId` with the listing of `root` and ranks it for whatever has been typed so far -- only
    * while that finder is still open on that root.
    */
  def withFilesListed(
    state: AppState,
    pickerId: SurfaceId,
    root: Path,
    listing: Either[String, ProjectFileListing]
  ): AppState =
    finderOn(state, pickerId, root).fold(state) { picker =>
      val filled = ListPickerSearch.refreshed(FileFinder.listed(picker, root, listing), state)
      WorkflowSurfaces.withModal(state, pickerId, Modal.ListPicker(filled))
    }

  private def finderOn(state: AppState, pickerId: SurfaceId, root: Path): Option[ListPicker] =
    state.runtime.uiSurfaces.find(_.id == pickerId).collect {
      case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)), _, _) if picker.source.exists {
            case PickerSource.ProjectFiles(pickerRoot, _) => pickerRoot == root
            case _                                        => false
          } =>
        picker
    }
