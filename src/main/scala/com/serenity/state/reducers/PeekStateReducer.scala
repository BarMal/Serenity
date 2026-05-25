package com.serenity.state.reducers

import com.serenity.state.models.{AppState, CursorPosition, Focus, PaneId}
import com.serenity.ui.layout.{PeekContent, PeekOverlay}

object PeekStateReducer:

  def show(content: PeekContent, at: CursorPosition, state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state.copy(
        peekOverlay = Some(PeekOverlay(content, at)),
        focus = Focus.PeekOverlay
      )
    )

  def dismiss(state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state.copy(
        peekOverlay = None,
        focus = fallbackEditorFocus(state)
      )
    )

  private def fallbackEditorFocus(state: AppState): Focus =
    state.layout.activeEditorPaneId match
      case Some(paneId) => Focus.EditorPane(paneId)
      case None         => Focus.EditorPane(PaneId(0))

