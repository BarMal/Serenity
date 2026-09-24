package com.serenity.state.manager

import cats.effect.IO
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, UndoState}

/** Submits the Find/Replace prompt: the decision is [[ReplaceWorkflowTransitions.submitted]], committed as one model
  * write so the replaced text, its undo entry and the prompt's new state are never seen apart.
  */
final private[manager] class StateManagerReplaceWorkflow(
    updateModelValidated: (Model => Option[Model]) => IO[Unit]
)(using balance: Balance):

  private[manager] def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    updateModelValidated(ReplaceWorkflowTransitions.submitted(_, surfaceId))

  private[manager] def replaceWorkflowSurface(
    state: AppState,
    surfaceId: SurfaceId
  ): Option[(UiSurface, ReplaceWorkflowState)] =
    ReplaceWorkflowTransitions.replaceWorkflowSurface(state, surfaceId)

/** Replace-all, replace-next, and the offset/selection bookkeeping they share, as pure functions of the model. */
private[manager] object ReplaceWorkflowTransitions:

  /** `None` when `surfaceId` is not an open replace prompt. */
  def submitted(model: Model, surfaceId: SurfaceId)(using Balance): Option[Model] =
    replaceWorkflowSurface(model.app, surfaceId).map { (_, workflow) =>
      resolveTarget(model.app, workflow) match
        case Left(status) => model.copy(app = withReplaceWorkflowSurface(model.app, surfaceId, status))
        case Right((bufferId, buffer, matches)) =>
          workflow.selectedAction match
            case ReplaceWorkflowAction.ReplaceAll =>
              replaceAllMatches(model, surfaceId, workflow, bufferId, buffer, matches)
            case ReplaceWorkflowAction.ReplaceNext =>
              replaceNextMatch(model, surfaceId, workflow, bufferId, buffer, matches)
    }

  /** The buffer and matches to replace, or the prompt showing why there are none. */
  private def resolveTarget(
    state: AppState,
    workflow: ReplaceWorkflowState
  ): Either[ReplaceWorkflowState, (BufferId, Buffer, List[Int])] =
    def status(message: String) = workflow.copy(statusMessage = Some(message))
    activeEditorBufferId(state).flatMap(id => state.persisted.buffers.get(id).map(id -> _)) match
      case None                                 => Left(status("No active buffer"))
      case Some(_) if workflow.findText.isEmpty => Left(status("Enter text to find"))
      case Some((bufferId, buffer)) =>
        workflow.selectedScope.resolve(buffer.primarySelection, offsetForCursor(buffer.document.content, _)) match
          case Left(error) => Left(status(error.message))
          case Right(range) =>
            val matches = scopedReplaceMatches(buffer, workflow.findText, range)
            if matches.isEmpty then Left(status("No matches found")) else Right((bufferId, buffer, matches))

  private def activeEditorBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)

  private def replaceAllMatches(
    model: Model,
    surfaceId: SurfaceId,
    workflow: ReplaceWorkflowState,
    bufferId: BufferId,
    buffer: Buffer,
    matches: List[Int]
  ): Model =
    val updatedContent =
      replaceMatchesInRanges(
        rope = buffer.document.content,
        matchOffsets = matches,
        findText = workflow.findText,
        replacementText = workflow.replacementText
      )
    val cursorOffset =
      finalCursorOffsetAfterReplacements(
        matches,
        workflow.findText.length,
        workflow.replacementText.length
      )
    val newCursor = updatedContent.offsetToCursorPosition(cursorOffset)
    val updatedFindState =
      refreshedFindState(updatedContent, workflow.findText, requestedIndex = 0)
    val updatedBuffer = buffer.copy(
      document = buffer.document.copy(
        content = updatedContent,
        isDirty = true,
        isNewEmpty = false
      ),
      editing = EditingState(List(newCursor)),
      findState = updatedFindState
    )
    val current = model.app
    val withReplacement = current.copy(
      persisted = current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> updatedBuffer)),
      runtime = current.runtime.copy(uiSurfaces = current.runtime.uiSurfaces.filterNot(_.id == surfaceId))
    )
    val updatedState = current.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        withReplacement.copy(persisted = withReplacement.persisted.copy(focus = Focus.EditorPane(paneId)))
      case None => withReplacement
    Model(updatedState, withWorkflowUndo(model.undo, current, bufferId, buffer), model.bufferAnimations)

  private def replaceNextMatch(
    model: Model,
    surfaceId: SurfaceId,
    workflow: ReplaceWorkflowState,
    bufferId: BufferId,
    buffer: Buffer,
    matches: List[Int]
  )(using Balance): Model =
    val startOffset = nextReplaceMatchOffset(buffer, matches)
    val endOffset   = startOffset + workflow.findText.length
    // startOffset/endOffset come from a match found against this same content, so this is expected to
    // always succeed; no-op back to the unedited content rather than crash if that invariant ever breaks.
    val updatedContent = buffer.document.content
      .delete(startOffset, endOffset)
      .flatMap(_.insert(startOffset, workflow.replacementText))
      .getOrElse(buffer.document.content)
    val cursorOffset = startOffset + workflow.replacementText.length
    val newCursor    = updatedContent.offsetToCursorPosition(cursorOffset)
    val updatedFindState =
      refreshedFindStateAfterOffset(updatedContent, workflow.findText, cursorOffset)
    val replacementSelection =
      workflow.selectedScope match
        case ReplaceWorkflowScope.Selection =>
          buffer.primarySelection.map(selection =>
            adjustSelectionAfterReplacement(
              buffer = buffer,
              updatedText = updatedContent.collect(),
              selection = selection,
              startOffset = startOffset,
              endOffset = endOffset,
              replacementLength = workflow.replacementText.length
            )
          )
        case ReplaceWorkflowScope.CurrentBuffer =>
          None
    val updatedBuffer = buffer.copy(
      document = buffer.document.copy(
        content = updatedContent,
        isDirty = true,
        isNewEmpty = false
      ),
      editing = EditingState.fromCursors(
        List(replacementSelection.fold(Cursor(newCursor))(Cursor(_)))
      ),
      findState = updatedFindState
    )
    val current = model.app
    val replaced =
      current.copy(persisted =
        current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> updatedBuffer))
      )
    Model(
      withReplaceWorkflowSurface(replaced, surfaceId, workflow.copy(statusMessage = Some("Replaced next match"))),
      withWorkflowUndo(model.undo, current, bufferId, buffer),
      model.bufferAnimations
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

  private def scopedReplaceMatches(
    buffer: Buffer,
    findText: String,
    range: ReplaceScopeRange
  ): List[Int] =
    buffer.document.content.searchAll(findText).filter { offset =>
      val insideScope = range match
        case ReplaceScopeRange.Selection(startOffset, endOffset) =>
          offset >= startOffset && (offset + findText.length) <= endOffset
        case ReplaceScopeRange.WholeBuffer =>
          true
      insideScope && isWholeGraphemeMatch(buffer.document.content, offset, findText.length)
    }

  private def refreshedFindState(
    content: com.serenity.rope.Rope,
    findText: String,
    requestedIndex: Int
  ): Option[FindState] =
    val results = content
      .searchAll(findText)
      .filter(offset => isWholeGraphemeMatch(content, offset, findText.length))
      .map(offset => content.offsetToCursorPosition(offset))
      .map(cursor => FindResult(cursor.line, cursor.column))
    val resultSet = FindResultSet.normalized(findText, results, requestedIndex)
    Option.when(resultSet.results.nonEmpty)(FindState.fromResultSet(resultSet))

  private def refreshedFindStateAfterOffset(
    content: com.serenity.rope.Rope,
    findText: String,
    offset: Int
  ): Option[FindState] =
    val matchOffsets =
      content.searchAll(findText).filter(found => isWholeGraphemeMatch(content, found, findText.length))
    val requestedIndex = matchOffsets.indexWhere(_ >= offset) match
      case -1    => 0
      case index => index
    refreshedFindState(content, findText, requestedIndex)

  private def replaceMatchesInRanges(
    rope: com.serenity.rope.Rope,
    matchOffsets: List[Int],
    findText: String,
    replacementText: String
  ): com.serenity.rope.Rope =
    matchOffsets.sorted.reverse.foldLeft(rope) { (current, offset) =>
      // `offset` comes from a match found against `current` (offsets are processed highest-first, so earlier
      // replacements never shift a not-yet-processed one), so this is expected to always succeed; no-op that one
      // replacement rather than corrupt the rope if that invariant ever breaks.
      current
        .delete(offset, offset + findText.length)
        .flatMap(_.insert(offset, replacementText))
        .getOrElse(current)
    }

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
    buffer: Buffer,
    updatedText: String,
    selection: Selection,
    startOffset: Int,
    endOffset: Int,
    replacementLength: Int
  )(using Balance): Selection =
    val oldText = buffer.document.content.collect()
    val delta   = replacementLength - (endOffset - startOffset)

    def adjust(cursor: CursorPosition): CursorPosition =
      val oldOffset = offsetForCursor(oldText, cursor)
      val newOffset =
        if oldOffset <= startOffset then oldOffset
        else if oldOffset >= endOffset then oldOffset + delta
        else startOffset + replacementLength
      cursorPositionForOffset(updatedText, newOffset)

    Selection(adjust(selection.anchor), adjust(selection.focus))

  private def withWorkflowUndo(undo: UndoState, bufferState: AppState, bufferId: BufferId, buffer: Buffer): UndoState =
    bufferState.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        undo.flushPendingGroup.pushUndo(HistoryEntry.BufferEdit(bufferId, paneId, BufferSnapshot.fromBuffer(buffer)))
      case None =>
        undo

  private def offsetForCursor(text: String, cursor: CursorPosition): Int =
    val linesBefore = text.split("\n", -1).take(cursor.line)
    val linePrefixLength =
      if linesBefore.isEmpty then 0
      else linesBefore.map(_.length).sum + linesBefore.length
    linePrefixLength + cursor.column

  private def offsetForCursor(content: com.serenity.rope.Rope, cursor: CursorPosition): Int =
    content.lineColumnToOffset(cursor.line, cursor.column)

  // Routed through the canonical Rope-based `offsetToCursorPosition` rather than a hand-rolled character walk,
  // so the string-backed replace path can never drift from the rope-backed one (see #1061).
  private def cursorPositionForOffset(text: String, offset: Int)(using Balance): CursorPosition =
    com.serenity.rope.Rope(text).offsetToCursorPosition(offset)

  private def isWholeGraphemeMatch(content: com.serenity.rope.Rope, offset: Int, length: Int): Boolean =
    content.isWholeGraphemeRange(offset, offset + length)

  def replaceWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[(UiSurface, ReplaceWorkflowState)] =
    state.surfaceById(surfaceId).flatMap { surface =>
      surface.content match
        case SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)) => Some((surface, workflow))
        case _                                                             => None
    }
