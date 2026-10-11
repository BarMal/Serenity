package com.serenity.state.manager

import cats.effect.IO
import com.serenity.rope.{Rope, isWholeGraphemeRange}
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEditSupport
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, UndoState}

/** Submits the Find/Replace prompt: the decision is [[ReplaceWorkflowTransitions.submitted]], committed as one model
  * write so the replaced text, its undo entry and the prompt's new state are never seen apart.
  */
final private[manager] class StateManagerReplaceWorkflow(
    updateModelValidated: (Model => Option[Model]) => IO[Unit]
):

  private[manager] def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    updateModelValidated(ReplaceWorkflowTransitions.submitted(_, surfaceId))

  private[manager] def replaceWorkflowSurface(
    state: AppState,
    surfaceId: SurfaceId
  ): Option[(UiSurface, ReplaceWorkflowState)] =
    ReplaceWorkflowTransitions.replaceWorkflowSurface(state, surfaceId)

/** Replace-all, replace-next, and the offset/selection bookkeeping they share, as pure functions of the model. Both
  * replace through [[EditorEditSupport.applyTrackedEdits]], the path typing takes, so the edit reaches the document as
  * a `ChangeSet` and annotations, rich text, diagnostics and language-server sync follow it.
  */
private[manager] object ReplaceWorkflowTransitions:

  /** What a submit found to replace: the whole-grapheme matches inside the chosen scope. */
  private case class ReplaceTarget(bufferId: BufferId, buffer: Buffer, scopedMatches: List[Int])

  /** `None` when `surfaceId` is not an open replace prompt. */
  def submitted(model: Model, surfaceId: SurfaceId): Option[Model] =
    replaceWorkflowSurface(model.app, surfaceId).map { (_, workflow) =>
      resolveTarget(model.app, workflow) match
        case Left(status) => model.copy(app = withReplaceWorkflowSurface(model.app, surfaceId, status))
        case Right(target) =>
          workflow.selectedAction match
            case ReplaceWorkflowAction.ReplaceAll  => replaceAllMatches(model, surfaceId, workflow, target)
            case ReplaceWorkflowAction.ReplaceNext => replaceNextMatch(model, surfaceId, workflow, target)
    }

  /** The buffer and matches to replace, or the prompt showing why there are none. The document is searched once. */
  private def resolveTarget(
    state: AppState,
    workflow: ReplaceWorkflowState
  ): Either[ReplaceWorkflowState, ReplaceTarget] =
    def status(message: String) = workflow.copy(statusMessage = Some(message))
    activeEditorBufferId(state).flatMap(id => state.persisted.buffers.get(id).map(id -> _)) match
      case None                                 => Left(status("No active buffer"))
      case Some(_) if workflow.findText.isEmpty => Left(status("Enter text to find"))
      case Some((bufferId, buffer)) =>
        workflow.selectedScope.resolve(buffer.primarySelection, offsetForCursor(buffer.document.content, _)) match
          case Left(error) => Left(status(error.message))
          case Right(range) =>
            val scoped = wholeGraphemeMatches(buffer.document.content, workflow.findText)
              .filter(inScope(range, _, workflow.findText.length))
            if scoped.isEmpty then Left(status("No matches found"))
            else Right(ReplaceTarget(bufferId, buffer, scoped))

  private def activeEditorBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)

  private def replaceAllMatches(
    model: Model,
    surfaceId: SurfaceId,
    workflow: ReplaceWorkflowState,
    target: ReplaceTarget
  ): Model =
    val buffer = target.buffer
    val edits = target.scopedMatches.zipWithIndex.map { (offset, index) =>
      EditorEditSupport.MultiCursorEdit(index, offset, offset + workflow.findText.length, workflow.replacementText)
    }
    val (edited, replaced) = EditorEditSupport.applyTrackedEdits(buffer, Nil, edits)
    val updatedContent     = edited.document.content
    val cursorOffset =
      finalCursorOffsetAfterReplacements(
        replaced.map(_.start),
        workflow.findText.length,
        workflow.replacementText.length
      )
    val newCursor = updatedContent.offsetToCursorPosition(cursorOffset)
    val updatedBuffer = edited.copy(
      editing = EditingState(List(newCursor)),
      findState = findStateFor(
        updatedContent,
        workflow.findText,
        wholeGraphemeMatches(updatedContent, workflow.findText),
        requestedIndex = 0
      )
    )
    val current = model.app
    val withReplacement = current.copy(
      persisted = current.persisted.copy(buffers = current.persisted.buffers + (target.bufferId -> updatedBuffer)),
      runtime = current.runtime.copy(uiSurfaces = current.runtime.uiSurfaces.filterNot(_.id == surfaceId))
    )
    val updatedState = current.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        withReplacement.copy(persisted = withReplacement.persisted.copy(focus = Focus.EditorPane(paneId)))
      case None => withReplacement
    Model(updatedState, withWorkflowUndo(model.undo, current, target.bufferId, buffer))

  private def replaceNextMatch(
    model: Model,
    surfaceId: SurfaceId,
    workflow: ReplaceWorkflowState,
    target: ReplaceTarget
  ): Model =
    val buffer                  = target.buffer
    val findLength              = workflow.findText.length
    val startOffset             = nextReplaceMatchOffset(buffer, target.scopedMatches)
    val endOffset               = startOffset + findLength
    val edit                    = EditorEditSupport.MultiCursorEdit(0, startOffset, endOffset, workflow.replacementText)
    val (edited, replacedEdits) = EditorEditSupport.applyTrackedEdits(buffer, Nil, List(edit))
    val updatedContent          = edited.document.content
    // A match that would join a block is left alone, so nothing was replaced and the text stays as long as it was.
    val replacementLength =
      if replacedEdits.isEmpty then findLength else workflow.replacementText.length
    val cursorOffset = startOffset + replacementLength
    val newCursor    = updatedContent.offsetToCursorPosition(cursorOffset)
    val matchesAfter = wholeGraphemeMatches(updatedContent, workflow.findText)
    val requestedIndex = matchesAfter.indexWhere(_ >= cursorOffset) match
      case -1    => 0
      case index => index
    val replacementSelection =
      workflow.selectedScope match
        case ReplaceWorkflowScope.Selection =>
          buffer.primarySelection.map(selection =>
            adjustSelectionAfterReplacement(
              before = buffer.document.content,
              after = updatedContent,
              selection = selection,
              startOffset = startOffset,
              endOffset = endOffset,
              replacementLength = replacementLength
            )
          )
        case ReplaceWorkflowScope.CurrentBuffer =>
          None
    val updatedBuffer = edited.copy(
      editing = EditingState.fromCursors(
        List(replacementSelection.fold(Cursor(newCursor))(Cursor(_)))
      ),
      findState = findStateFor(updatedContent, workflow.findText, matchesAfter, requestedIndex)
    )
    val current = model.app
    val replaced =
      current.copy(persisted =
        current.persisted.copy(buffers = current.persisted.buffers + (target.bufferId -> updatedBuffer))
      )
    Model(
      withReplaceWorkflowSurface(replaced, surfaceId, workflow.copy(statusMessage = Some("Replaced next match"))),
      withWorkflowUndo(model.undo, current, target.bufferId, buffer)
    )

  /** Shows `workflow` in the replace prompt `surfaceId`, raised to the top of the surfaces. */
  def withReplaceWorkflowSurface(state: AppState, surfaceId: SurfaceId, workflow: ReplaceWorkflowState): AppState =
    state.surfaceById(surfaceId) match
      case Some(surface) =>
        val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)))
        state.copy(runtime =
          state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.movedToEndWhere(_.id == surfaceId)(updatedSurface))
        )
      case None =>
        state

  /** Searches forward from the currently highlighted match's own start, not its end (the cursor position/selection
    * focus) -- so replacing the match find-next just landed on replaces that match itself rather than skipping past it
    * to the next one. Before `#1577`'s unification of cursor position with selection focus, `editing.cursors` and
    * `editing.selection` were independent fields and every write path here happened to leave `cursors` pointed at the
    * match start while `selection` tracked its full highlighted range; `primarySelection.map(_.start)` is that same
    * intent made explicit now that a cursor's position and its own selection's focus are the same field.
    */
  private def nextReplaceMatchOffset(buffer: Buffer, matches: List[Int]): Int =
    val cursorOffset = buffer.primarySelection
      .map(_.start)
      .orElse(buffer.editing.cursorPositions.headOption)
      .map(cursor => offsetForCursor(buffer.document.content, cursor))
      .getOrElse(0)
    // The only caller checks matches.isEmpty first, so matches is always non-empty here; 0 is an
    // unreachable fallback rather than a real offset choice.
    matches.find(_ >= cursorOffset).orElse(matches.headOption).getOrElse(0)

  private def inScope(range: ReplaceScopeRange, offset: Int, findLength: Int): Boolean =
    range match
      case ReplaceScopeRange.Selection(startOffset, endOffset) =>
        offset >= startOffset && (offset + findLength) <= endOffset
      case ReplaceScopeRange.WholeBuffer =>
        true

  private def wholeGraphemeMatches(content: Rope, findText: String): List[Int] =
    content.searchAll(findText).filter(offset => isWholeGraphemeMatch(content, offset, findText.length))

  private def findStateFor(
    content: Rope,
    findText: String,
    matchOffsets: List[Int],
    requestedIndex: Int
  ): Option[FindState] =
    val results = matchOffsets
      .map(offset => content.offsetToCursorPosition(offset))
      .map(cursor => FindResult(cursor.line, cursor.column))
      .toVector
    val resultSet = FindResultSet.normalized(findText, results, requestedIndex)
    Option.when(resultSet.results.nonEmpty)(FindState.fromResultSet(resultSet, FindOptions.default))

  private def finalCursorOffsetAfterReplacements(
    matchOffsets: List[Int],
    findLength: Int,
    replacementLength: Int
  ): Int =
    matchOffsets.sorted
      .foldLeft((0, 0)) {
        case ((shift, _), offset) =>
          val replacementEnd = offset + shift + replacementLength
          val nextShift      = shift + replacementLength - findLength
          (nextShift, replacementEnd)
      }
      ._2

  private def adjustSelectionAfterReplacement(
    before: Rope,
    after: Rope,
    selection: Selection,
    startOffset: Int,
    endOffset: Int,
    replacementLength: Int
  ): Selection =
    val delta = replacementLength - (endOffset - startOffset)

    def adjust(cursor: CursorPosition): CursorPosition =
      val oldOffset = offsetForCursor(before, cursor)
      val newOffset =
        if oldOffset <= startOffset then oldOffset
        else if oldOffset >= endOffset then oldOffset + delta
        else startOffset + replacementLength
      after.offsetToCursorPosition(newOffset)

    Selection(adjust(selection.anchor), adjust(selection.focus))

  private def withWorkflowUndo(undo: UndoState, bufferState: AppState, bufferId: BufferId, buffer: Buffer): UndoState =
    bufferState.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        undo.pushUndo(HistoryEntry.BufferEdit(bufferId, paneId, BufferSnapshot.fromBuffer(buffer)))
      case None =>
        undo

  private def offsetForCursor(content: Rope, cursor: CursorPosition): Int =
    content.lineColumnToOffset(cursor.line, cursor.column)

  private def isWholeGraphemeMatch(content: Rope, offset: Int, length: Int): Boolean =
    content.isWholeGraphemeRange(offset, offset + length)

  def replaceWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[(UiSurface, ReplaceWorkflowState)] =
    state.surfaceById(surfaceId).flatMap { surface =>
      surface.content match
        case SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)) => Some((surface, workflow))
        case _                                                             => None
    }
