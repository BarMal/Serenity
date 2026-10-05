package com.serenity.state.reducers

import com.serenity.state.models.*

/** The pane and buffer a spelling action applies to: the focused editor pane, which after a right-click menu closes is
  * the pane that was clicked, or else the active one.
  */
private[reducers] object SpellingTarget:

  def of(state: AppState): Option[(PaneId, Buffer)] =
    val paneId = state.persisted.focus match
      case Focus.EditorPane(focused) => Some(focused)
      case _                         => state.persisted.layout.activeEditorPaneId
    paneId.flatMap(id => Focused.bufferOf(state, id).map(id -> _))
