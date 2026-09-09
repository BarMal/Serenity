package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, UndoState}

/** Find/Replace workflow mechanics: replace-all, replace-next, and the offset/selection bookkeeping they share. */
final private[manager] class StateManagerReplaceWorkflow(
    stateRef: Ref[IO, AppState],
    undoRef: Ref[IO, UndoState],
    activeEditorBufferId: AppState => Option[BufferId],
    updateReplaceWorkflowSurface: (SurfaceId, ReplaceWorkflowState) => IO[Unit]
)(using balance: Balance):

  private[manager] def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    stateRef.get.flatMap { state =>
      replaceWorkflowSurface(state, surfaceId) match
        case Some((_, workflow)) =>
          workflow.selectedAction match
            case ReplaceWorkflowAction.ReplaceAll =>
              submitReplaceAllEffect(surfaceId, workflow, state)
            case ReplaceWorkflowAction.ReplaceNext =>
              submitReplaceNextEffect(surfaceId, workflow, state)

        case None =>
          IO.unit
    }

  private def submitReplaceAllEffect(surfaceId: SurfaceId, workflow: ReplaceWorkflowState, state: AppState): IO[Unit] =
    activeEditorBufferId(state) match
      case None =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("No active buffer"))
        )
      case Some(_) if workflow.findText.isEmpty =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("Enter text to find"))
        )
      case Some(bufferId) =>
        state.persisted.buffers.get(bufferId) match
          case Some(buffer) =>
            workflow.selectedScope.resolve(buffer.primarySelection, offsetForCursor(buffer.document.content, _)) match
              case Left(error) =>
                updateReplaceWorkflowSurface(
                  surfaceId,
                  workflow.copy(statusMessage = Some(error.message))
                )
              case Right(range) =>
                val matches = scopedReplaceMatches(buffer, workflow.findText, range)
                if matches.isEmpty then
                  updateReplaceWorkflowSurface(
                    surfaceId,
                    workflow.copy(statusMessage = Some("No matches found"))
                  )
                else
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
                    editing = buffer.editing.copy(
                      cursors = List(newCursor),
                      selection = None,
                      selections = Nil,
                      preferredColumn = Some(newCursor.column),
                      preferredXPx = None
                    ),
                    findState = updatedFindState
                  )
                  recordWorkflowUndo(state, bufferId, buffer) >> stateRef.update { current =>
                    val updatedState = current.copy(
                      persisted =
                        current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> updatedBuffer)),
                      runtime =
                        current.runtime.copy(uiSurfaces = current.runtime.uiSurfaces.filterNot(_.id == surfaceId))
                    )
                    current.persisted.layout.activeEditorPaneId match
                      case Some(paneId) =>
                        updatedState.copy(persisted = updatedState.persisted.copy(focus = Focus.EditorPane(paneId)))
                      case None => updatedState
                  }
          case None =>
            updateReplaceWorkflowSurface(
              surfaceId,
              workflow.copy(statusMessage = Some("No active buffer"))
            )

  private def submitReplaceNextEffect(surfaceId: SurfaceId, workflow: ReplaceWorkflowState, state: AppState): IO[Unit] =
    activeEditorBufferId(state) match
      case None =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("No active buffer"))
        )
      case Some(_) if workflow.findText.isEmpty =>
        updateReplaceWorkflowSurface(
          surfaceId,
          workflow.copy(statusMessage = Some("Enter text to find"))
        )
      case Some(bufferId) =>
        state.persisted.buffers.get(bufferId) match
          case Some(buffer) =>
            workflow.selectedScope.resolve(buffer.primarySelection, offsetForCursor(buffer.document.content, _)) match
              case Left(error) =>
                updateReplaceWorkflowSurface(
                  surfaceId,
                  workflow.copy(statusMessage = Some(error.message))
                )
              case Right(range) =>
                val matches = scopedReplaceMatches(buffer, workflow.findText, range)
                if matches.isEmpty then
                  updateReplaceWorkflowSurface(
                    surfaceId,
                    workflow.copy(statusMessage = Some("No matches found"))
                  )
                else replaceNextMatch(surfaceId, workflow, state, bufferId, buffer, matches)
          case None =>
            updateReplaceWorkflowSurface(
              surfaceId,
              workflow.copy(statusMessage = Some("No active buffer"))
            )

  /** The state transition for `submitReplaceNextEffect`'s match-found case, split out so the dispatcher above stays
    * under the method-length ratchet -- behaviorally this is still one linear step of that method.
    */
  private def replaceNextMatch(
    surfaceId: SurfaceId,
    workflow: ReplaceWorkflowState,
    state: AppState,
    bufferId: BufferId,
    buffer: Buffer,
    matches: List[Int]
  ): IO[Unit] =
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
      editing = buffer.editing.copy(
        cursors = List(newCursor),
        selection = replacementSelection,
        selections = Nil,
        preferredColumn = Some(newCursor.column),
        preferredXPx = None
      ),
      findState = updatedFindState
    )
    recordWorkflowUndo(state, bufferId, buffer) >> stateRef.update { current =>
      current.copy(persisted =
        current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> updatedBuffer))
      )
    } >> updateReplaceWorkflowSurface(
      surfaceId,
      workflow.copy(statusMessage = Some("Replaced next match"))
    )

  private def nextReplaceMatchOffset(buffer: Buffer, matches: List[Int]): Int =
    val cursorOffset = buffer.editing.cursors.headOption
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
  ): Selection =
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

  private def recordWorkflowUndo(bufferState: AppState, bufferId: BufferId, buffer: Buffer): IO[Unit] =
    bufferState.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        undoRef.update { undo =>
          val flushed = undo.flushPendingGroup
          val entry   = HistoryEntry.BufferEdit(bufferId, paneId, BufferSnapshot.fromBuffer(buffer))
          flushed.pushUndo(entry)
        }
      case None =>
        IO.unit

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
  private def cursorPositionForOffset(text: String, offset: Int): CursorPosition =
    com.serenity.rope.Rope(text).offsetToCursorPosition(offset)

  private def isWholeGraphemeMatch(content: com.serenity.rope.Rope, offset: Int, length: Int): Boolean =
    content.isWholeGraphemeRange(offset, offset + length)

  private[manager] def replaceWorkflowSurface(
    state: AppState,
    surfaceId: SurfaceId
  ): Option[(UiSurface, ReplaceWorkflowState)] =
    state.surfaceById(surfaceId).flatMap { surface =>
      surface.content match
        case SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)) => Some((surface, workflow))
        case _                                                             => None
    }
