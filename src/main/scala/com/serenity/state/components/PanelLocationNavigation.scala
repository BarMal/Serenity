package com.serenity.state.components

import com.serenity.document.CommentRendering
import com.serenity.state.manager.CursorViewport
import com.serenity.state.models.*
import com.serenity.state.reducers.Focused
import com.serenity.ui.layout.Location

/** Opening a panel row's location in the editor -- shared by a click on the row and Enter on its highlight. */
object PanelLocationNavigation:

  /** The active editor pane's cursor moved to `location` (clamped to its document) and focused. */
  def editorAt(state: AppState, location: Location): AppState =
    state.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        Focused.bufferOf(state, paneId) match
          case Some(buffer) =>
            val line =
              math.max(0, math.min(location.line, math.max(0, buffer.document.content.lineCount - 1)))
            val column =
              math.max(0, math.min(location.column, buffer.document.content.getLine(line).getOrElse("").length))
            val cursor   = CursorPosition(line, column)
            val viewport = CursorViewport.adjustForCursor(buffer, state, cursor)
            val updatedBuffer = buffer.copy(
              editing = EditingState(List(cursor)),
              viewport = viewport
            )
            state.copy(persisted =
              state.persisted.copy(
                buffers = state.persisted.buffers.updated(buffer.id, updatedBuffer),
                focus = Focus.EditorPane(paneId),
                layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId))
              )
            )
          case None =>
            state
      case None =>
        state

  /** As [[editorAt]], then opens the comment lens on the comment there. */
  def commentAt(state: AppState, location: Location): AppState =
    CommentRendering.openLensAtCursor(editorAt(state, location))
