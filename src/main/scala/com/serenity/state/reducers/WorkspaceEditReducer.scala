package com.serenity.state.reducers

import com.serenity.lsp.model.LspTextEdit
import com.serenity.state.models.*
import com.serenity.state.undo.{EditGrouping, HistoryEntry}

/** Applies a server's `workspace/applyEdit` (#1847) to every open buffer whose document uri the edit names, as one undo
  * step (#1965): in a pane that shows the buffer, or, for a buffer shown in no pane, as a
  * [[HistoryEntry.DetachedBufferEdit]] that brings the cursors back with the text; a [[HistoryEntry.Group]] when it
  * edits several.
  */
object WorkspaceEditReducer:

  /** What [[applyToOpenBuffers]] did: `editedBuffers` counts buffers that took at least one edit, `heldBack` the edits
    * left unapplied (e.g. beside a read-only block), `unopenedUris` the uris no open buffer matched, and
    * `overlappingUris` the documents whose edits overlap -- when there are any, nothing is applied anywhere -- and
    * `undoEntries` what restores each edited buffer, for the caller to record as one step ([[undoEffects]]).
    */
  final case class Outcome(
      result: ReducerResult,
      appliedCount: Int,
      editedBuffers: Int,
      heldBack: Int,
      unopenedUris: Set[String],
      overlappingUris: Set[String] = Set.empty,
      undoEntries: List[HistoryEntry] = Nil
  )

  def apply(edits: Map[String, List[LspTextEdit]], state: AppState): ReducerResult =
    val outcome = applyToOpenBuffers(edits, state)
    ReducerResult(outcome.result.state, outcome.result.effects ++ undoEffects(outcome.undoEntries))

  /** One undo step for a whole edit (#1965): the buffer's own entry when it touched one buffer, a
    * [[HistoryEntry.Group]] of detached entries -- so undoing does not move focus between panes -- when it touched
    * several.
    */
  def undoEffects(entries: List[HistoryEntry]): List[AppEffect] =
    val recorded = entries match
      case Nil           => None
      case single :: Nil => Some(single)
      case several       => Some(HistoryEntry.Group(several.map(detached)))
    recorded.toList.map(entry => AppEffect.Undo(UndoEffect.RecordBoundary(entry, EditGrouping.Standalone)))

  private def detached(entry: HistoryEntry): HistoryEntry = entry match
    case edit: HistoryEntry.BufferEdit => HistoryEntry.DetachedBufferEdit(edit.bufferId, edit.snapshot)
    case other                         => other

  def applyToOpenBuffers(edits: Map[String, List[LspTextEdit]], state: AppState): Outcome =
    val openUris    = state.persisted.buffers.values.flatMap(_.document.filePath.map(_.toUri.toString)).toSet
    val overlapping = edits.collect { case (uri, documentEdits) if LspTextEdit.overlap(documentEdits) => uri }.toSet
    val initial     = Outcome(ReducerResult.noEffects(state), 0, 0, 0, edits.keySet -- openUris, overlapping)
    if overlapping.nonEmpty then initial
    else applyToBuffers(edits, state, initial)

  private def applyToBuffers(edits: Map[String, List[LspTextEdit]], state: AppState, initial: Outcome): Outcome =
    state.persisted.buffers.values.foldLeft(initial) { (outcome, buffer) =>
      val bufferEdits = buffer.document.filePath.flatMap(path => edits.get(path.toUri.toString)).getOrElse(Nil)
      if bufferEdits.isEmpty then outcome
      else
        val (editedState, undoEntry, applied) = applyToBuffer(outcome.result.state, buffer, bufferEdits)
        outcome.copy(
          result = ReducerResult(editedState, outcome.result.effects),
          undoEntries = outcome.undoEntries ++ undoEntry,
          appliedCount = outcome.appliedCount + applied,
          editedBuffers = outcome.editedBuffers + (if applied > 0 then 1 else 0),
          heldBack = outcome.heldBack + bufferEdits.length - applied
        )
    }

  private def applyToBuffer(
    state: AppState,
    buffer: Buffer,
    edits: List[LspTextEdit]
  ): (AppState, Option[HistoryEntry], Int) =
    val content = buffer.document.content
    val multiCursorEdits = edits.map { edit =>
      val start = content.lineColumnToOffset(edit.range.start.line, edit.range.start.character)
      val end   = content.lineColumnToOffset(edit.range.end.line, edit.range.end.character)
      EditorEditSupport.MultiCursorEdit(0, start, end, edit.newText)
    }
    val initialOffsets =
      buffer.editing.cursorPositions.map(cursor => content.lineColumnToOffset(cursor.line, cursor.column))
    val (updatedBuffer, appliedEdits) = EditorEditSupport.applyTrackedEdits(buffer, initialOffsets, multiCursorEdits)
    val shownIn = state.persisted.layout.editorPanes.collectFirst {
      case (paneId, pane) if pane.bufferId.contains(buffer.id) => paneId
    }
    val undoEntry = EditorEditSupport.undoEntry(buffer, shownIn, appliedEdits)
    (Focused.replaceBuffer(state, updatedBuffer), undoEntry, appliedEdits.length)
