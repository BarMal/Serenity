package com.serenity.state.reducers

import com.serenity.animation.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.richtext.{RichTextDocument, RichTextPosition, RichTextRange}
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry}

/** Low-level infrastructure shared by every family of [[EditorEventReducer]] event handling: applying one or many
  * [[MultiCursorEdit]]s to a buffer's content, rich text document, comments and animations in lockstep. Extracted from
  * `EditorEventReducer` (which grew past its 600-line target) rather than any one event family, since every family that
  * edits content -- text editing, clipboard, deletion -- depends on this pairing being kept consistent.
  */
private[reducers] object EditorEditSupport:

  /** Every call site in this file computes `start`/`end`/`index` from `content` itself (a cursor offset, a selection
    * boundary, a grapheme boundary, an LSP/tracked edit range) immediately before calling `insert`/`delete`, so an
    * out-of-range result would mean that coordinate computation is already broken, not that this particular edit was
    * unusual. No-op'ing back to the unedited content is the deliberate policy here: it keeps a latent coordinate bug
    * from crashing the editor on every keystroke, at the cost of that one edit silently not applying if the invariant
    * is ever violated.
    */
  def insertOrUnchanged(content: Rope, index: Int, text: String): Rope =
    content.insert(index, text).getOrElse(content)

  def deleteOrUnchanged(content: Rope, start: Int, end: Int): Rope =
    content.delete(start, end).getOrElse(content)

  final case class MultiCursorEdit(ownerIndex: Int, start: Int, end: Int, insertedText: String)

  def animationRemapEffects(
    bufferId: BufferId,
    before: Rope,
    after: Rope,
    edits: List[MultiCursorEdit]
  ): List[AppEffect] =
    if edits.isEmpty then Nil
    else
      List(
        AppEffect.Animation(
          AnimationEffect.RemapThroughEdits(bufferId, before, after, edits.map(toTextEdit))
        )
      )

  def animationMergeEffects(bufferId: BufferId, delta: Map[CharacterKey, AnimatedCell]): List[AppEffect] =
    if delta.isEmpty then Nil else List(AppEffect.Animation(AnimationEffect.Merge(bufferId, delta)))

  /** Declares the edit(s) just performed as undoable -- see #1016. `before` is the buffer as it stood immediately
    * before this call's edits; every caller already has it in scope as the receiver it edited. `groupable` mirrors
    * whether the triggering event was a character/tab insertion, the only two event kinds a consecutive run of which
    * coalesces into one undo step.
    */
  def undoBoundaryEffects(
    bufferId: BufferId,
    paneId: PaneId,
    before: Buffer,
    edits: List[MultiCursorEdit],
    groupable: Boolean
  ): List[AppEffect] =
    if edits.isEmpty then Nil
    else
      val entry = HistoryEntry.BufferEdit(bufferId, paneId, BufferSnapshot.fromBuffer(before))
      List(AppEffect.Undo(UndoEffect.RecordBoundary(entry, groupable)))

  private def toTextEdit(edit: MultiCursorEdit): TextEdit =
    TextEdit(edit.start, edit.end, edit.insertedText)

  def backwardGraphemeDeletionRange(content: Rope, offset: Int): Option[(Int, Int)] =
    val beforeOrAt = content.graphemeBoundaryBeforeOrAt(offset)
    val afterOrAt  = content.graphemeBoundaryAfterOrAt(offset)
    if beforeOrAt < offset && offset < afterOrAt then Some(beforeOrAt -> afterOrAt)
    else
      val start = content.previousGraphemeBoundary(offset)
      Option.when(start < offset)(start -> offset)

  def forwardGraphemeDeletionRange(content: Rope, offset: Int): Option[(Int, Int)] =
    val beforeOrAt = content.graphemeBoundaryBeforeOrAt(offset)
    val afterOrAt  = content.graphemeBoundaryAfterOrAt(offset)
    if beforeOrAt < offset && offset < afterOrAt then Some(beforeOrAt -> afterOrAt)
    else
      val end = content.nextGraphemeBoundary(offset)
      Option.when(offset < end)(offset -> end)

  /** Folds `edits` over `content` and `richTextDocument` together, so a caller's rich-text document stays remapped in
    * lockstep with the plain-text edits it applies -- edits must already be in the order `applyContentEdit` expects
    * (callers sort descending by offset so earlier edits don't shift later ones). Shared by every multi-edit path
    * instead of copied per call site, after `#1072` and `#1291` both found edit paths that had drifted from this exact
    * pairing and silently let `richTextDocument` go stale.
    */
  def foldEditsWithRichText(
    buffer: Buffer,
    edits: List[MultiCursorEdit]
  )(applyContentEdit: (Rope, MultiCursorEdit) => Rope): (Rope, Option[RichTextDocument]) =
    edits.foldLeft((buffer.document.content, buffer.richText.richTextDocument)) {
      case ((content, document), edit) =>
        val nextContent = applyContentEdit(content, edit)
        val nextDocument = richTextDocumentAfterEdit(
          buffer.copy(
            document = buffer.document.copy(content = content),
            richText = buffer.richText.copy(richTextDocument = document)
          ),
          edit.start,
          edit.end,
          edit.insertedText
        )
        (nextContent, nextDocument)
    }

  def applyTrackedEdits(
    buffer: Buffer,
    initialOffsets: List[Int],
    edits: List[MultiCursorEdit]
  ): (Buffer, List[MultiCursorEdit]) =
    if edits.isEmpty then (buffer, Nil)
    else
      val trackedOffsets = initialOffsets.toArray
      val sortedEdits    = edits.sortBy(edit => (-edit.start, -edit.end))
      val (updatedContent, updatedRichTextDocument) =
        foldEditsWithRichText(buffer, sortedEdits) { (content, edit) =>
          insertOrUnchanged(deleteOrUnchanged(content, edit.start, edit.end), edit.start, edit.insertedText)
        }
      val finalOffsets = sortedEdits.foldLeft(trackedOffsets) { (offsets, edit) =>
        val delta = edit.insertedText.length - (edit.end - edit.start)
        offsets.indices.foreach { i =>
          val offset = offsets(i)
          offsets(i) =
            if offset < edit.start then offset
            else if offset > edit.end then offset + delta
            else if i == edit.ownerIndex then edit.start + edit.insertedText.length
            else edit.start + edit.insertedText.length
        }
        offsets
      }
      val finalCursors = finalOffsets.toList
        .map(offset => updatedContent.offsetToCursorPosition(offset))
        .distinct
      val baseBuffer = buffer.withEditedContent(
        content = updatedContent,
        cursors = finalCursors,
        documentComments =
          adjustDocumentComments(buffer.annotations.documentComments, buffer.document.content, updatedContent, edits),
        richTextDocument = updatedRichTextDocument
      )
      (baseBuffer, edits)

  /** Merged-range sibling of [[applyTrackedEdits]] -- deletion-only, so no `insertedText` bookkeeping, but the content
    * and `richTextDocument` folds mirror it exactly: both need each range applied against the content (and rich-text
    * document) left by the previous one, not the pre-edit buffer. Previously this path silently dropped
    * `richTextDocument` updates entirely (`#1072`) -- the rich text would desync from the plain-text content on any
    * multi-cursor word/grapheme deletion that merged overlapping ranges.
    */
  def applyMergedDeletionEdits(
    buffer: Buffer,
    initialOffsets: List[Int],
    edits: List[MultiCursorEdit]
  ): (Buffer, List[MultiCursorEdit]) =
    if edits.isEmpty then (buffer, Nil)
    else
      val mergedRanges = mergeOverlappingDeletionRanges(edits.map(edit => (edit.start, edit.end)))
      val sortedMergedEdits = mergedRanges
        .sortBy { case (start, end) => (-start, -end) }
        .map { case (start, end) => MultiCursorEdit(0, start, end, "") }
      val (updatedContent, updatedRichTextDocument) =
        foldEditsWithRichText(buffer, sortedMergedEdits)((content, edit) =>
          deleteOrUnchanged(content, edit.start, edit.end)
        )
      val mergedEdits = mergedRanges.zipWithIndex.map {
        case ((start, end), index) =>
          MultiCursorEdit(index, start, end, "")
      }
      val finalOffsets = initialOffsets.map(offset => remapOffsetAfterDeletions(offset, mergedRanges))
      val finalCursors = finalOffsets
        .map(offset => updatedContent.offsetToCursorPosition(offset))
        .distinct
      val baseBuffer = buffer.withEditedContent(
        content = updatedContent,
        cursors = finalCursors,
        documentComments = adjustDocumentComments(
          buffer.annotations.documentComments,
          buffer.document.content,
          updatedContent,
          mergedEdits
        ),
        richTextDocument = updatedRichTextDocument
      )
      (baseBuffer, mergedEdits)

  def mergeOverlappingDeletionRanges(
    ranges: List[(Int, Int)]
  ): List[(Int, Int)] =
    ranges
      .sortBy { case (start, end) => (start, end) }
      .foldLeft(List.empty[(Int, Int)]) {
        case (Nil, range) => range :: Nil
        case ((currentStart, currentEnd) :: rest, (nextStart, nextEnd)) =>
          if nextStart < currentEnd then (currentStart, math.max(currentEnd, nextEnd)) :: rest
          else (nextStart, nextEnd) :: (currentStart, currentEnd) :: rest
      }
      .reverse

  def remapOffsetAfterDeletions(
    offset: Int,
    deletions: List[(Int, Int)]
  ): Int =
    deletions.foldLeft(offset) {
      case (currentOffset, (start, end)) =>
        if currentOffset < start then currentOffset
        else if currentOffset > end then currentOffset - (end - start)
        else start
    }

  def adjustDocumentComments(
    comments: List[DocumentComment],
    initialContent: Rope,
    updatedContent: Rope,
    edits: List[MultiCursorEdit]
  ): List[DocumentComment] =
    if comments.isEmpty || edits.isEmpty then comments
    else
      val sortedEdits = edits.sortBy(edit => (edit.start, edit.end))
      comments.map { comment =>
        val startOffset              = initialContent.lineColumnToOffset(comment.start.line, comment.start.column)
        val endOffset                = initialContent.lineColumnToOffset(comment.end.line, comment.end.column)
        val nextStart                = remapCommentStart(startOffset, sortedEdits)
        val nextEnd                  = remapCommentEnd(endOffset, sortedEdits).max(nextStart)
        val (startLine, startColumn) = updatedContent.offsetToLineColumn(nextStart)
        val (endLine, endColumn)     = updatedContent.offsetToLineColumn(nextEnd)

        DocumentComment(
          CursorPosition(startLine, startColumn),
          CursorPosition(endLine, endColumn),
          comment.text
        )
      }

  private def remapCommentStart(offset: Int, edits: List[MultiCursorEdit]): Int =
    remapEditBoundary(offset, edits, insertionAtBoundaryMoves = true)

  private def remapCommentEnd(offset: Int, edits: List[MultiCursorEdit]): Int =
    remapEditBoundary(offset, edits, insertionAtBoundaryMoves = false)

  def remapEditBoundary(
    offset: Int,
    edits: List[MultiCursorEdit],
    insertionAtBoundaryMoves: Boolean
  ): Int =
    val (_, remappedOffset) = edits.foldLeft((0, offset)) {
      case ((deltaSoFar, currentOffset), edit) =>
        val removedLength     = edit.end - edit.start
        val insertedLength    = edit.insertedText.length
        val editDelta         = insertedLength - removedLength
        val remappedEditStart = edit.start + deltaSoFar
        val isInsertion       = edit.start == edit.end
        val nextOffset =
          if isInsertion then
            if offset > edit.start || (offset == edit.start && insertionAtBoundaryMoves) then
              currentOffset + insertedLength
            else currentOffset
          else if offset < edit.start then currentOffset
          else if offset > edit.end || (offset == edit.end && !insertionAtBoundaryMoves) then currentOffset + editDelta
          else remappedEditStart

        (deltaSoFar + editDelta, nextOffset)
    }

    remappedOffset.max(0)

  /** Replaces the primary selection with `insertedText` if one exists, otherwise inserts at `cursor` -- the single-
    * cursor edit both plain typing (`EditorTextEditReducer.insertAtCursor`) and paste (`EditorClipboardEventReducer`)
    * reduce to, since a no-selection insert is just a zero-width "selection" replacement.
    */
  def replaceSelectionOrInsert(
    buffer: Buffer,
    cursor: CursorPosition,
    insertedText: String
  ): (Buffer, MultiCursorEdit) =
    val (baseContent, insertionStart, startOffset, endOffset) = buffer.primarySelection match
      case Some(selection) =>
        val startOffset = EditorCursorMovement.selectionStartOffset(selection, buffer.document.content)
        val endOffset   = EditorCursorMovement.selectionEndOffset(selection, buffer.document.content)
        (
          deleteOrUnchanged(buffer.document.content, startOffset, endOffset),
          buffer.document.content.offsetToCursorPosition(startOffset),
          startOffset,
          endOffset
        )
      case None =>
        val startOffset =
          buffer.document.content.graphemeBoundaryAfterOrAt(
            buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
          )
        (
          buffer.document.content,
          buffer.document.content.offsetToCursorPosition(startOffset),
          startOffset,
          startOffset
        )

    val newContent      = insertOrUnchanged(baseContent, startOffset, insertedText)
    val newCursor       = cursorAfterInsertion(insertionStart, insertedText)
    val replacementEdit = MultiCursorEdit(0, startOffset, endOffset, insertedText)

    (
      buffer.copy(
        document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
        editing = buffer.editing.copy(
          cursors = EditorCursorMovement.replacePrimaryCursor(newCursor, buffer.editing.cursors),
          selection = None,
          selections = Nil,
          preferredColumn = Some(newCursor.column),
          preferredXPx = None
        ),
        annotations = buffer.annotations.copy(
          documentComments = adjustDocumentComments(
            buffer.annotations.documentComments,
            buffer.document.content,
            newContent,
            List(replacementEdit)
          )
        ),
        richText = buffer.richText.copy(
          richTextDocument = richTextDocumentAfterEdit(buffer, startOffset, endOffset, insertedText)
        )
      ),
      replacementEdit
    )

  private def cursorAfterInsertion(start: CursorPosition, insertedText: String): CursorPosition =
    val lines = insertedText.split("\n", -1)
    if lines.length == 1 then start.copy(column = start.column + insertedText.length)
    else CursorPosition(start.line + lines.length - 1, lines.last.length)

  def deleteSelectedRanges(
    buffer: Buffer
  ): (Buffer, List[MultiCursorEdit]) =
    val ranges  = EditorCursorMovement.mergedActiveSelectionRanges(buffer, buffer.document.content)
    val offsets = ranges.map(_._1)
    val edits = ranges.zipWithIndex.map {
      case ((start, end), index) =>
        MultiCursorEdit(index, start, end, "")
    }
    applyTrackedEdits(buffer, offsets, edits)

  def deleteSelectedRange(
    buffer: Buffer,
    selection: Selection
  ): (Buffer, MultiCursorEdit) =
    val startOffset = EditorCursorMovement.selectionStartOffset(selection, buffer.document.content)
    val endOffset   = EditorCursorMovement.selectionEndOffset(selection, buffer.document.content)
    val newContent  = deleteOrUnchanged(buffer.document.content, startOffset, endOffset)
    val newCursor   = newContent.offsetToCursorPosition(startOffset)
    val baseBuffer = buffer.copy(
      document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
      editing = buffer.editing.copy(
        cursors = EditorCursorMovement.replacePrimaryCursor(newCursor, buffer.editing.cursors),
        selection = None,
        selections = Nil,
        preferredColumn = Some(newCursor.column),
        preferredXPx = None
      ),
      annotations = buffer.annotations.copy(
        documentComments = adjustDocumentComments(
          buffer.annotations.documentComments,
          buffer.document.content,
          newContent,
          List(MultiCursorEdit(0, startOffset, endOffset, ""))
        )
      ),
      richText = buffer.richText.copy(richTextDocument = richTextDocumentAfterEdit(buffer, startOffset, endOffset, ""))
    )
    (baseBuffer, MultiCursorEdit(0, startOffset, endOffset, ""))

  def applyMultiSelectionReplacement(
    buffer: Buffer,
    insertedText: String
  ): (Buffer, List[MultiCursorEdit]) =
    val ranges  = EditorCursorMovement.mergedActiveSelectionRanges(buffer, buffer.document.content)
    val offsets = ranges.map(_._1)
    val edits = ranges.zipWithIndex.map {
      case ((start, end), index) =>
        MultiCursorEdit(index, start, end, insertedText)
    }
    applyTrackedEdits(buffer, offsets, edits)

  def richTextDocumentAfterEdit(
    buffer: Buffer,
    startOffset: Int,
    endOffset: Int,
    insertedText: String
  ): Option[RichTextDocument] =
    buffer.richText.richTextDocument.flatMap { document =>
      Option.when(document.matchesPlainText(buffer.document.content.collect())) {
        val updatedDocument = document
          .replaceRange(
            RichTextRange(
              richTextPositionForOffset(buffer.document.content, startOffset),
              richTextPositionForOffset(buffer.document.content, endOffset)
            ),
            insertedText
          )
          .normalized
        buffer.richText.insertionRichTextStyle
          .filter(_ => insertedText.nonEmpty)
          .map { style =>
            val updatedContent =
              insertOrUnchanged(
                deleteOrUnchanged(buffer.document.content, startOffset, endOffset),
                startOffset,
                insertedText
              )
            updatedDocument
              .updateInlineStyle(
                RichTextRange(
                  richTextPositionForOffset(updatedContent, startOffset),
                  richTextPositionForOffset(updatedContent, startOffset + insertedText.length)
                )
              )(_ => style)
              .normalized
          }
          .getOrElse(updatedDocument)
      }
    }

  def richTextPositionForOffset(content: Rope, offset: Int): RichTextPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    RichTextPosition(line, column)

  /** Returns the buffer with content/comments/etc. applied but animations untouched, plus the delta of newly animated
    * cells for the caller to hand to the presentation layer (`#1001`) -- this function never had access to the buffer's
    * *current* animations beyond merging into them, so it never needed to read them; only the merge itself moves to the
    * caller.
    */
  def addInsertionAnimations(
    buffer: Buffer,
    state: AppState,
    edits: List[MultiCursorEdit]
  ): (Buffer, Map[CharacterKey, AnimatedCell]) =
    val sortedEdits = edits
      .filter(_.insertedText.nonEmpty)
      .sortBy(edit => (edit.start, edit.end))

    if sortedEdits.isEmpty then (buffer, Map.empty)
    else
      val insertedCells = insertedTransitionCells(buffer.document.content, sortedEdits, state)
      if insertedCells.isEmpty then (buffer, Map.empty)
      else
        val plan = ElementTransitionPlanner.plan(
          ElementTransitionRequest(TransitionScope.EditorInsertion),
          state.persisted.config.editorInsertionTransitionSettings
        )
        if plan.kind == TransitionKind.Disabled then (buffer, Map.empty)
        else if plan.kind == TransitionKind.Fade then
          state.persisted.config.scaledCharacterAnimation match
            case Some(animConfig) =>
              insertedCells.headOption match
                case Some((key, cell)) if insertedCells.size == 1 =>
                  val delta = Map(
                    key -> AnimatedCell.parametricForeground(
                      cell.char,
                      cell.startColor,
                      cell.endColor,
                      animConfig.steps
                    )
                  )
                  (buffer, delta)
                case _ =>
                  val staggeredCells = insertedCells
                    .groupBy { case (key, _) => key.line }
                    .valuesIterator
                    .flatMap(lineCells =>
                      FlowAnimationBuilder.build(
                        cells = lineCells,
                        direction = FlowDirection.ByColumn,
                        sweep = SweepDirection.Forward,
                        steps = animConfig.steps,
                        staggerFrames = 1
                      )
                    )
                    .toMap
                  (buffer, staggeredCells)
            case None =>
              (buffer, Map.empty)
        else
          val animationState = ElementTransitionLowerer.lower(
            plan,
            ElementTransitionCells(content = insertedCells),
            tickRateMs = 16
          )
          (buffer, animationState.animations)

  private def insertedTransitionCells(
    content: Rope,
    edits: List[MultiCursorEdit],
    state: AppState,
    maxAnimatedCells: Int = com.serenity.state.manager.VisibleBufferAnimationCells.DefaultMaxAnimatedCells
  ): Map[CharacterKey, CellAnimation] =
    edits.foldLeft(Map.empty[CharacterKey, CellAnimation]) { (cells, edit) =>
      val remainingBudget = maxAnimatedCells - cells.size
      if remainingBudget <= 0 then cells
      else
        val finalStartOffset = remapEditBoundary(edit.start, edits, insertionAtBoundaryMoves = false)
        cells ++ insertedCellsFromText(
          content,
          finalStartOffset,
          edit.insertedText.take(remainingBudget),
          state.persisted.theme.backgroundColor,
          state.persisted.theme.foregroundColor
        )
    }

  private def insertedCellsFromText(
    content: Rope,
    startOffset: Int,
    insertedText: String,
    startColor: java.awt.Color,
    endColor: java.awt.Color
  ): Map[CharacterKey, CellAnimation] =
    insertedText
      .foldLeft((Map.empty[CharacterKey, CellAnimation], startOffset)) {
        case ((cells, offset), char) if char == '\n' =>
          (cells, offset + 1)
        case ((cells, offset), char) =>
          val (line, column) = content.offsetToLineColumn(offset)
          (
            cells + (CharacterKey(column, line) -> CellAnimation(char, startColor, endColor)),
            offset + 1
          )
      }
      ._1
