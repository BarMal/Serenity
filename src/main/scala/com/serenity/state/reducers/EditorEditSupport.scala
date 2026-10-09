package com.serenity.state.reducers

import com.serenity.richtext.{RichTextDocument, RichTextPosition, RichTextRange}
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry}

/** Low-level infrastructure shared by every family of [[EditorEventReducer]] event handling: applying one or many
  * [[MultiCursorEdit]]s to a buffer's content, rich text document and comments in lockstep. Extracted from
  * `EditorEventReducer` (which grew past its 600-line target) rather than any one event family, since every family that
  * edits content -- text editing, clipboard, deletion -- depends on this pairing being kept consistent.
  */
private[state] object EditorEditSupport:

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

  /** From the start of the cursor's line up to it; at the start of a line, the line break before it instead. */
  def lineStartDeletionRange(content: Rope, offset: Int): Option[(Int, Int)] =
    val (line, _) = content.offsetToLineColumn(offset)
    val lineStart = content.lineColumnToOffset(line, 0)
    if lineStart < offset then Some(lineStart -> offset) else backwardGraphemeDeletionRange(content, offset)

  /** From the cursor to the end of its line; at the end of a line, the line break after it instead. */
  def lineEndDeletionRange(content: Rope, offset: Int): Option[(Int, Int)] =
    val (line, _) = content.offsetToLineColumn(offset)
    val lineEnd   = content.lineColumnToOffset(line, content.getLine(line).fold(0)(_.length))
    if offset < lineEnd then Some(offset -> lineEnd) else forwardGraphemeDeletionRange(content, offset)

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
    val folded = foldEditsTracked(buffer, edits)(applyContentEdit)
    (folded.content, folded.richText)

  /** What a fold of edits left: the text, the remapped rich-text document and the edits that were applied, which are
    * all of them unless one would have joined a block (see [[joinsBlock]]).
    */
  final case class FoldedEdits(content: Rope, richText: Option[RichTextDocument], applied: List[MultiCursorEdit]):

    /** The members of `edits` that were applied, for the adjustments (annotations, cursors, undo) that follow the text:
      * a skipped edit changed nothing, so it must not move anything.
      */
    def appliedAmong(edits: List[MultiCursorEdit]): List[MultiCursorEdit] =
      edits.filter(edit =>
        applied.exists(done =>
          done.start == edit.start && done.end == edit.end && done.insertedText == edit.insertedText
        )
      )

  def foldEditsTracked(
    buffer: Buffer,
    edits: List[MultiCursorEdit]
  )(applyContentEdit: (Rope, MultiCursorEdit) => Rope): FoldedEdits =
    // Seeded with the document only while it still describes the content, since each step re-stamps it as matching.
    val seed = FoldedEdits(
      buffer.document.content,
      buffer.richText.richTextDocument.filter(_ => buffer.richTextInSync),
      Nil
    )
    edits.foldLeft(seed) { (folded, edit) =>
      val nextDocument = richTextDocumentAfterEdit(
        buffer.withEditedDocument(folded.content, folded.richText),
        edit.start,
        edit.end,
        edit.insertedText
      )
      if joinsBlock(folded.content, folded.richText, nextDocument, edit) then folded
      else FoldedEdits(applyContentEdit(folded.content, edit), nextDocument, folded.applied :+ edit)
    }

  /** Whether `edit` would leave other content beside a block atom. A block line takes no text, so every path that edits
    * several ranges at once (replace-all, rename, multi-cursor, Markdown formatting) skips such an edit and applies the
    * others, leaving the block exactly as it was. The lines around the edit are looked at first, so a document with no
    * block near the edit costs a lookup.
    */
  private def joinsBlock(
    content: Rope,
    document: Option[RichTextDocument],
    next: Option[RichTextDocument],
    edit: MultiCursorEdit
  ): Boolean =
    document.exists { current =>
      val firstLine = content.offsetToLineColumn(edit.start)._1
      val lastLine  = content.offsetToLineColumn(edit.end)._1
      current.hasOpaqueBlockBetween((firstLine - 1).max(0), lastLine + 1) && next.exists(_.hasMixedBlock)
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
      val folded = foldEditsTracked(buffer, sortedEdits) { (content, edit) =>
        insertOrUnchanged(deleteOrUnchanged(content, edit.start, edit.end), edit.start, edit.insertedText)
      }
      val (updatedContent, updatedRichTextDocument) = (folded.content, folded.richText)
      val appliedEdits                              = folded.appliedAmong(edits)
      val finalOffsets = folded.applied.foldLeft(trackedOffsets) { (offsets, edit) =>
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
        adjustedAnnotations = adjustAnnotations(
          buffer.annotations,
          buffer.document.content,
          updatedContent,
          appliedEdits
        ),
        richTextDocument = updatedRichTextDocument
      )
      (baseBuffer, appliedEdits)

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
      val folded =
        foldEditsTracked(buffer, sortedMergedEdits)((content, edit) => deleteOrUnchanged(content, edit.start, edit.end))
      val (updatedContent, updatedRichTextDocument) = (folded.content, folded.richText)
      val appliedRanges =
        mergedRanges.filter((start, end) => folded.applied.exists(done => done.start == start && done.end == end))
      val mergedEdits = appliedRanges.zipWithIndex.map {
        case ((start, end), index) =>
          MultiCursorEdit(index, start, end, "")
      }
      val finalOffsets = initialOffsets.map(offset => remapOffsetAfterDeletions(offset, appliedRanges))
      val finalCursors = finalOffsets
        .map(offset => updatedContent.offsetToCursorPosition(offset))
        .distinct
      val baseBuffer = buffer.withEditedContent(
        content = updatedContent,
        cursors = finalCursors,
        adjustedAnnotations = adjustAnnotations(
          buffer.annotations,
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

  /** Maps an offset in the pre-deletion content to the post-deletion content. `deletions` are in original coordinates,
    * so each is compared against the unshifted offset; an offset inside (or on the edges of) a deletion collapses to
    * that deletion's start.
    */
  def remapOffsetAfterDeletions(
    offset: Int,
    deletions: List[(Int, Int)]
  ): Int =
    def removedUpTo(position: Int): Int =
      deletions.collect { case (start, end) if end <= position => end - start }.sum

    deletions.find { case (start, end) => start <= offset && offset <= end } match
      case Some((start, _)) => start - removedUpTo(start)
      case None             => offset - removedUpTo(offset)

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

        comment.copy(
          anchor = CursorPosition(startLine, startColumn),
          focus = CursorPosition(endLine, endColumn)
        )
      }

  /** A placeholder stays put when text is typed exactly at its position: it marks where writing is still owed, so an
    * Enter at the end of a heading must leave it on the heading line rather than carrying it into the new paragraph.
    */
  def adjustPlaceholders(
    placeholders: List[Placeholder],
    initialContent: Rope,
    updatedContent: Rope,
    edits: List[MultiCursorEdit]
  ): List[Placeholder] =
    if placeholders.isEmpty || edits.isEmpty then placeholders
    else
      val sortedEdits = edits.sortBy(edit => (edit.start, edit.end))
      placeholders.map(placeholder =>
        placeholder.copy(position = remapPoint(placeholder.position, initialContent, updatedContent, sortedEdits))
      )

  /** Bookmarks follow edits the way placeholders do, and two that a deletion collapses onto one spot become one. */
  def adjustBookmarks(
    bookmarks: List[CursorPosition],
    initialContent: Rope,
    updatedContent: Rope,
    edits: List[MultiCursorEdit]
  ): List[CursorPosition] =
    if bookmarks.isEmpty || edits.isEmpty then bookmarks
    else
      val sortedEdits = edits.sortBy(edit => (edit.start, edit.end))
      bookmarks.map(remapPoint(_, initialContent, updatedContent, sortedEdits)).distinct

  private def remapPoint(
    position: CursorPosition,
    initialContent: Rope,
    updatedContent: Rope,
    sortedEdits: List[MultiCursorEdit]
  ): CursorPosition =
    val offset = initialContent.lineColumnToOffset(position.line, position.column)
    updatedContent.offsetToCursorPosition(remapEditBoundary(offset, sortedEdits, insertionAtBoundaryMoves = false))

  /** The one place an edit's effect on user-authored annotations is decided, so a new annotation kind cannot be added
    * to `Annotations` and then forgotten by one of the edit paths.
    */
  def adjustAnnotations(
    annotations: Annotations,
    initialContent: Rope,
    updatedContent: Rope,
    edits: List[MultiCursorEdit]
  ): Annotations =
    annotations.copy(
      bookmarks = adjustBookmarks(annotations.bookmarks, initialContent, updatedContent, edits),
      documentComments = adjustDocumentComments(annotations.documentComments, initialContent, updatedContent, edits),
      placeholders = adjustPlaceholders(annotations.placeholders, initialContent, updatedContent, edits)
    )

  /** For a whole-content swap that carries no edit list -- undo and redo restore a snapshot's text outright. The
    * difference is treated as one replaced region (common prefix and suffix trimmed), so annotations keep their place
    * relative to the text around the change. Inside a run of identical characters the region is ambiguous, and a marker
    * there may land anywhere within the run.
    */
  def adjustAnnotationsAcrossReplacement(annotations: Annotations, before: Rope, after: Rope): Annotations =
    if annotations.bookmarks.isEmpty && annotations.documentComments.isEmpty && annotations.placeholders.isEmpty then
      annotations
    else
      val beforeText = before.collect()
      val afterText  = after.collect()
      val limit      = math.min(beforeText.length, afterText.length)
      val prefix     = Iterator.range(0, limit).takeWhile(i => beforeText(i) == afterText(i)).size
      val suffix = Iterator
        .range(0, limit - prefix)
        .takeWhile(i => beforeText(beforeText.length - 1 - i) == afterText(afterText.length - 1 - i))
        .size
      if prefix == beforeText.length && prefix == afterText.length then annotations
      else
        val replacement = afterText.substring(prefix, afterText.length - suffix)
        val edit        = MultiCursorEdit(0, prefix, beforeText.length - suffix, replacement)
        adjustAnnotations(annotations, before, after, List(edit))

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
      buffer
        .withEditedDocument(newContent, richTextDocumentAfterEdit(buffer, startOffset, endOffset, insertedText))
        .copy(
          editing = buffer.editing.withPrimary(Cursor(newCursor)),
          annotations = adjustAnnotations(
            buffer.annotations,
            buffer.document.content,
            newContent,
            List(replacementEdit)
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
    val baseBuffer = buffer
      .withEditedDocument(newContent, richTextDocumentAfterEdit(buffer, startOffset, endOffset, ""))
      .copy(
        editing = buffer.editing.withPrimary(Cursor(newCursor)),
        annotations = adjustAnnotations(
          buffer.annotations,
          buffer.document.content,
          newContent,
          List(MultiCursorEdit(0, startOffset, endOffset, ""))
        )
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
    buffer.richText.richTextDocument.filter(_ => buffer.richTextInSync).map { document =>
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

  def richTextPositionForOffset(content: Rope, offset: Int): RichTextPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    RichTextPosition(line, column)
