package com.serenity.state.reducers

import com.serenity.state.models.*
import com.serenity.state.undo.EditGrouping

/** Replaces a misspelled word in the focused buffer with a chosen suggestion as one undoable edit (#1939). */
object SpellingReplacementReducer:

  /** `None` when the text at `line`/`start`..`end` is no longer `misspelled` -- the suggestion was offered for an
    * earlier version of the buffer, and replacing whatever is there now would corrupt the user's text.
    */
  def replace(
    state: AppState,
    line: Int,
    start: Int,
    end: Int,
    misspelled: String,
    replacement: String
  ): Option[ReducerResult] =
    for
      (paneId, buffer) <- SpellingTarget.of(state)
      text             <- buffer.document.content.getLine(line)
      if start >= 0 && end <= text.length && start < end && text.substring(start, end) == misspelled
    yield
      val content     = buffer.document.content
      val startOffset = content.lineColumnToOffset(line, start)
      val edit = EditorEditSupport.MultiCursorEdit(0, startOffset, content.lineColumnToOffset(line, end), replacement)
      val cursorOffsets =
        buffer.editing.cursorPositions.map(cursor => content.lineColumnToOffset(cursor.line, cursor.column))
      val (edited, applied) = EditorEditSupport.applyTrackedEdits(buffer, cursorOffsets, List(edit))
      ReducerResult(
        Focused.replaceBuffer(state, edited),
        EditorEditSupport.undoBoundaryEffects(buffer.id, paneId, buffer, applied, grouping = EditGrouping.Standalone)
      )
