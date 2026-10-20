package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ReducerResult}

/** A list picker's matching done off the dispatcher (see [[ListPickerSearch.isCheap]]) as pure functions of the state:
  * a result is for the query it was started with, and lands only while the picker still holds that query.
  */
private[manager] object PickerSearchTransitions:

  /** The open picker `surfaceId` recomputed for `query` -- none once it has closed or been typed in since. */
  def searched(state: AppState, surfaceId: SurfaceId, query: String): Option[ListPicker] =
    pickerHolding(state, surfaceId, query).map(ListPickerSearch.refreshed(_, state))

  /** `searched`'s items shown in the picker, previewing the choice they highlight; unchanged if the picker no longer
    * holds `query`.
    */
  def applied(state: AppState, surfaceId: SurfaceId, query: String, searched: ListPicker): ReducerResult =
    pickerHolding(state, surfaceId, query).fold(ReducerResult.noEffects(state)) { current =>
      val updated = current.copy(
        title = searched.title,
        items = searched.items,
        source = searched.source,
        searchPending = false
      )
      val preview =
        if updated.selectedChoice == current.selectedChoice then None else updated.selectedChoice.flatMap(_.preview)
      ReducerResult(
        WorkflowSurfaces.withModal(state, surfaceId, Modal.ListPicker(updated)),
        preview.map(AppEffect.ExecuteCommandUnrecorded(_)).toList
      )
    }

  private def pickerHolding(state: AppState, surfaceId: SurfaceId, query: String): Option[ListPicker] =
    state.runtime.uiSurfaces.find(_.id == surfaceId).collect {
      case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)), _, _) if picker.queryText == query =>
        picker
    }
