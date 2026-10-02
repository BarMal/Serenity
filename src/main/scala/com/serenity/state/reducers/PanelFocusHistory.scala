package com.serenity.state.reducers

import com.serenity.state.models.{AppState, Focus, PaneId, SurfaceId}

/** Focus moving onto a docked panel and back off it again, for Escape's "return to the previous focus" setting. */
object PanelFocusHistory:

  /** Focus on the panel, remembering what had it -- unless it already has focus, so re-focusing records nothing. */
  def enter(state: AppState, surfaceId: SurfaceId): AppState =
    if state.persisted.focus == Focus.Surface(surfaceId) then state
    else state.pushFocus(Focus.Surface(surfaceId))

  /** Focus back on whatever had it before the focused panel, skipping anything since closed and falling back to the
    * active pane. A restored pane becomes the active pane too, so the editor's idea of "current" follows the focus.
    */
  def returnToPrevious(state: AppState): AppState =
    val leaving  = state.persisted.focus
    val history  = state.runtime.focusHistory.filterNot(_ == leaving)
    val restored = state.copy(runtime = state.runtime.copy(focusHistory = history)).popFocus
    restored.persisted.focus match
      case Focus.EditorPane(paneId) if restored.persisted.layout.editorPanes.contains(paneId) =>
        activatingPane(restored, paneId)
      case Focus.EditorPane(_) if history.nonEmpty =>
        returnToPrevious(restored.copy(persisted = restored.persisted.copy(focus = leaving)))
      case _ =>
        restored

  private def activatingPane(state: AppState, paneId: PaneId): AppState =
    state.copy(persisted =
      state.persisted.copy(layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId)))
    )
