package com.serenity.state.reducers

import com.serenity.io.StorageLocation
import com.serenity.lsp.model.LspTextEdit
import com.serenity.state.models.*
import com.serenity.ui.layout.PeekContent

/** Applies a `textDocument/rename` response's `WorkspaceEdit` (#1467, #1965). Every open buffer the edit names is
  * edited at once, each as its own undo step. A file no buffer holds is opened as a buffer first and then edited the
  * same way, left unsaved: [[apply]] edits what is open and asks for the rest to be opened ([[PendingRename]]), and
  * [[afterOpening]] finishes once they are. The peek shown at the end says how many locations were edited and how many
  * files could not be, so a multi-file rename is never silently partial. An edit whose changes to one file overlap is
  * applied nowhere.
  */
object RenameEditReducer:

  def apply(edits: Map[String, List[LspTextEdit]], anchor: CursorPosition, state: AppState): ReducerResult =
    val outcome = WorkspaceEditReducer.applyToOpenBuffers(edits, state)
    if outcome.overlappingUris.nonEmpty then rejectedPeek(state, anchor, outcome.overlappingUris)
    else if outcome.unopenedUris.isEmpty then summarised(outcome, anchor)
    else
      val pending = PendingRename(
        edits.filter((uri, _) => outcome.unopenedUris.contains(uri)),
        anchor,
        outcome.appliedCount,
        outcome.editedBuffers,
        outcome.heldBack,
        state.persisted.focus,
        state.persisted.layout.activeEditorPaneId,
        outcome.undoEntries
      )
      ReducerResult(
        outcome.result.state,
        outcome.result.effects :+ AppEffect.File(FileEffect.OpenForRenameEdit(pending))
      )

  def afterOpening(pending: PendingRename, state: AppState): ReducerResult =
    val outcome = WorkspaceEditReducer.applyToOpenBuffers(pending.edits, state)
    val whole = outcome.copy(
      result = outcome.result.copy(state = focusRestored(outcome.result.state, pending)),
      undoEntries = pending.undoEntries ++ outcome.undoEntries,
      appliedCount = pending.applied + outcome.appliedCount,
      editedBuffers = pending.editedBuffers + outcome.editedBuffers,
      heldBack = pending.heldBack + outcome.heldBack
    )
    summarised(whole, pending.anchor)

  private def summarised(outcome: WorkspaceEditReducer.Outcome, anchor: CursorPosition): ReducerResult =
    val summary = summaryPeek(outcome.result.state, anchor, outcome)
    ReducerResult(
      summary.state,
      outcome.result.effects ++ WorkspaceEditReducer.undoEffects(outcome.undoEntries) ++ summary.effects
    )

  /** Opening a file focuses it; the user was somewhere else. */
  private def focusRestored(state: AppState, pending: PendingRename): AppState =
    val layout = state.persisted.layout
    pending.activePane.filter(layout.editorPanes.contains).fold(state) { paneId =>
      state.copy(persisted =
        state.persisted.copy(focus = pending.focus, layout = layout.copy(activeEditorPaneId = Some(paneId)))
      )
    }

  private def rejectedPeek(state: AppState, anchor: CursorPosition, overlappingUris: Set[String]): ReducerResult =
    val names = overlappingUris.toList.sorted.map(fileName).mkString(", ")
    PeekStateReducer.show(
      PeekContent.QuickInfo(s"Rename not applied: ${overlappingUris.size} file(s) have overlapping edits ($names)."),
      anchor,
      state
    )

  private def fileName(uri: String): String =
    StorageLocation.localPath(uri).flatMap(path => Option(path.getFileName)).fold(uri)(_.toString)

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
