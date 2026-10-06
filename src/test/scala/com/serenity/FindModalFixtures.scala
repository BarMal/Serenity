package com.serenity

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.CursorViewport
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalEventReducer
import com.serenity.ui.widget.TextField

/** An editor with a find surface open over `content`, and the means to complete its search without the debounce. */
trait FindModalFixtures:

  given Balance = Balance.default

  def stateWithFindModal(
    query: String,
    content: String,
    cursor: CursorPosition = CursorPosition(0, 0),
    viewport: Viewport = Viewport(0, 0, 24, 80)
  ): AppState =
    val bufferId = BufferId(0)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        focus = Focus.Surface(SurfaceId("find")),
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = Rope(content)),
              editing = EditingState(List(cursor)),
              viewport = viewport
            )
        )
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("find"),
            SurfaceContent.ModalWorkflow(Modal.Find(TextField.of(query), Vector.empty, 0)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  def activeFindModal(state: AppState): Option[Modal] =
    state.modalSurface.flatMap {
      _.content match
        case SurfaceContent.ModalWorkflow(find: Modal.Find) => Some(find)
        case _                                              => None
    }

  def completeFind(state: AppState): AppState =
    activeFindModal(state) match
      case Some(Modal.Find(query, _, _, _, _)) =>
        val bufferId = BufferId(0)
        val content  = state.persisted.buffers(bufferId).document.content
        val reducedState = ModalEventReducer.applyFindSearchResults(
          state,
          FindSearchRequest(SurfaceId("find"), bufferId, query.text, content),
          FindSearch.results(content, query.text)
        )
        CursorViewport.ensureVisibleCursors(state, reducedState)
      case _ =>
        state
