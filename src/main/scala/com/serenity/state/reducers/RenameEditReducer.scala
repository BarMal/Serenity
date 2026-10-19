package com.serenity.state.reducers

import com.serenity.lsp.model.LspTextEdit
import com.serenity.state.models.*
import com.serenity.ui.layout.PeekContent

/** Applies a `textDocument/rename` response's `WorkspaceEdit` (#1467, #1965) to every open buffer whose document it
  * names, each as its own undo step. This client has no path yet to open, edit and persist a document that no buffer
  * holds, so edits for those uris are dropped; the peek message shown afterward always reports how many locations were
  * edited and how many files were left untouched, so a multi-file rename is never silently partial.
  */
object RenameEditReducer:

  def apply(edits: Map[String, List[LspTextEdit]], anchor: CursorPosition, state: AppState): ReducerResult =
    val outcome = WorkspaceEditReducer.applyToOpenBuffers(edits, state)
    val summary = summaryPeek(outcome.result.state, anchor, outcome)
    ReducerResult(summary.state, outcome.result.effects ++ summary.effects)

  private def summaryPeek(
    state: AppState,
    anchor: CursorPosition,
    outcome: WorkspaceEditReducer.Outcome
  ): ReducerResult =
    val where =
      if outcome.editedBuffers > 1 then s"in ${outcome.editedBuffers} open file(s)" else "in this file"
    val appliedPart = s"Renamed ${outcome.appliedCount} location(s) $where." +
      (if outcome.heldBack == 0 then "" else s" ${outcome.heldBack} beside a read-only block were left as they were.")
    val skippedPart =
      if outcome.unopenedUris.isEmpty then ""
      else s" ${outcome.unopenedUris.size} file(s) not open in any buffer were not updated."
    PeekStateReducer.show(PeekContent.QuickInfo(appliedPart + skippedPart), anchor, state)
