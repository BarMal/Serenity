package com.serenity.state.reducers

import com.serenity.lsp.model.LspTextEdit
import com.serenity.state.models.*
import com.serenity.ui.layout.PeekContent

/** Applies a `textDocument/rename` response's `WorkspaceEdit` (#1467) to the buffer open in the currently focused
  * editor pane -- the common case this client supports today. A `WorkspaceEdit` can in principle touch files that
  * aren't open in any buffer; this client has no path yet to open, edit and persist a document it never focuses, so
  * edits for any uri other than the focused buffer's are dropped rather than applied. The peek message shown afterward
  * always reports how many locations were actually edited versus left untouched, so a multi-file rename is never
  * silently partial.
  */
object RenameEditReducer:

  def apply(edits: Map[String, List[LspTextEdit]], anchor: CursorPosition, state: AppState): ReducerResult =
    Focused.bufferOf(state) match
      case None =>
        summaryPeek(state, anchor, appliedCount = 0, skippedUris = edits.keySet)
      case Some(buffer) =>
        val currentUri   = buffer.document.filePath.map(_.toUri.toString)
        val currentEdits = currentUri.flatMap(edits.get).getOrElse(Nil)
        val skippedUris  = edits.keySet -- currentUri.toSet
        if currentEdits.isEmpty then summaryPeek(state, anchor, appliedCount = 0, skippedUris)
        else
          val content = buffer.document.content
          val multiCursorEdits = currentEdits.map { edit =>
            val start = content.lineColumnToOffset(edit.range.start.line, edit.range.start.character)
            val end   = content.lineColumnToOffset(edit.range.end.line, edit.range.end.character)
            EditorEditSupport.MultiCursorEdit(0, start, end, edit.newText)
          }
          val initialOffsets =
            buffer.editing.cursorPositions.map(cursor => content.lineColumnToOffset(cursor.line, cursor.column))
          val (updatedBuffer, appliedEdits) =
            EditorEditSupport.applyTrackedEdits(buffer, initialOffsets, multiCursorEdits)
          val stateWithEdit = Focused.replaceBuffer(state, updatedBuffer)
          val animationEffects =
            EditorEditSupport.animationRemapEffects(buffer.id, content, updatedBuffer.document.content, appliedEdits)
          val undoEffects = state.persisted.layout.activeEditorPaneId
            .map(paneId => EditorEditSupport.undoBoundaryEffects(buffer.id, paneId, buffer, appliedEdits, groupable = false))
            .getOrElse(Nil)
          val summary = summaryPeek(stateWithEdit, anchor, appliedCount = currentEdits.length, skippedUris)
          ReducerResult(summary.state, animationEffects ++ undoEffects ++ summary.effects)

  private def summaryPeek(
    state: AppState,
    anchor: CursorPosition,
    appliedCount: Int,
    skippedUris: Set[String]
  ): ReducerResult =
    val appliedPart = s"Renamed $appliedCount location(s) in this file."
    val skippedPart =
      if skippedUris.isEmpty then ""
      else s" ${skippedUris.size} other file(s) were not updated -- rename currently applies to the open file only."
    PeekStateReducer.show(PeekContent.QuickInfo(appliedPart + skippedPart), anchor, state)
