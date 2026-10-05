package com.serenity.state.reducers

import com.serenity.lsp.model.LspTextEdit
import com.serenity.state.models.*
import com.serenity.state.undo.EditGrouping

/** Applies a server's `workspace/applyEdit` (#1847) to every open buffer whose document uri the edit names. Each
  * buffer's edits are recorded as one undo step, in a pane that shows the buffer; a buffer shown in no pane is edited
  * without one, as there is no pane to restore the cursor into.
  */
object WorkspaceEditReducer:

  def apply(edits: Map[String, List[LspTextEdit]], state: AppState): ReducerResult =
    state.persisted.buffers.values.foldLeft(ReducerResult.noEffects(state)) { (result, buffer) =>
      val bufferEdits = buffer.document.filePath.flatMap(path => edits.get(path.toUri.toString)).getOrElse(Nil)
      if bufferEdits.isEmpty then result
      else
        val (editedState, undoEffects) = applyToBuffer(result.state, buffer, bufferEdits)
        ReducerResult(editedState, result.effects ++ undoEffects)
    }

  private def applyToBuffer(
    state: AppState,
    buffer: Buffer,
    edits: List[LspTextEdit]
  ): (AppState, List[AppEffect]) =
    val content = buffer.document.content
    val multiCursorEdits = edits.map { edit =>
      val start = content.lineColumnToOffset(edit.range.start.line, edit.range.start.character)
      val end   = content.lineColumnToOffset(edit.range.end.line, edit.range.end.character)
      EditorEditSupport.MultiCursorEdit(0, start, end, edit.newText)
    }
    val initialOffsets =
      buffer.editing.cursorPositions.map(cursor => content.lineColumnToOffset(cursor.line, cursor.column))
    val (updatedBuffer, appliedEdits) = EditorEditSupport.applyTrackedEdits(buffer, initialOffsets, multiCursorEdits)
    val undoEffects = state.persisted.layout.editorPanes
      .collectFirst { case (paneId, pane) if pane.bufferId.contains(buffer.id) => paneId }
      .toList
      .flatMap(paneId =>
        EditorEditSupport.undoBoundaryEffects(buffer.id, paneId, buffer, appliedEdits, grouping = EditGrouping.Standalone)
      )
    (Focused.replaceBuffer(state, updatedBuffer), undoEffects)
