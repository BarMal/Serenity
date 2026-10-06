package com.serenity.state.reducers

import com.serenity.document.ChapterRenumbering
import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.text.SmartPunctuation

/** Character/newline/tab insertion, indent/unindent and the four deletions -- the family that mutates document content
  * directly at the cursor(s). Split out of `EditorEventReducer.reduceCursorsEditEvent` when that file grew past its
  * 600-line target.
  */
private[reducers] object EditorTextEditReducer:
  import EditorCursorMovement.*
  import EditorEditSupport.*
  import EditorCursorSupport.{CursorEventContext, TabInsertion}

  def reduce(event: TextEntryEvent, ctx: CursorEventContext): ReducerResult =
    import ctx.*

    /** Like a plain buffer update, but `f` also reports the edits it made, so the undo boundary can record them.
      * `groupable` mirrors the calling event: whether a consecutive run of edits like this one coalesces into one undo
      * step (#1016).
      */
    def applyEditedBuffer(groupable: Boolean)(f: Buffer => (Buffer, List[MultiCursorEdit])): ReducerResult =
      val (updated, edits) = f(buffer)
      ReducerResult(
        Focused.replaceBuffer(currentState, updated),
        undoBoundaryEffects(buffer.id, paneId, buffer, edits, groupable)
      )

    event match
      case InsertChar(char) =>
        if hasSelection then applyEditedBuffer(groupable = true)(applyMultiSelectionReplacement(_, char.toString))
        else if isMulti then applyEditedBuffer(groupable = true)(applyMultiCursorInsertion(_, char.toString))
        else
          smartPunctuationReplacement(buffer, head, char, currentState) match
            case Some(substitution) =>
              insertWithSmartPunctuation(buffer, head, char, substitution, currentState, paneId)
            case None =>
              insertAtCursor(buffer, head, char.toString, currentState, paneId, groupable = true)

      case TabKey =>
        if hasSelection then
          val (updated, edits) = applyLineIndent(buffer, selectionLines(buffer))
          ReducerResult(
            Focused.replaceBuffer(currentState, updated),
            undoBoundaryEffects(buffer.id, paneId, buffer, edits, groupable = true)
          )
        else if isMulti then applyEditedBuffer(groupable = true)(applyMultiCursorInsertion(_, TabInsertion))
        else insertAtCursor(buffer, head, TabInsertion, currentState, paneId, groupable = true)

      case NewLine | Enter =>
        if hasSelection then applyEditedBuffer(groupable = false)(applyMultiSelectionReplacement(_, "\n"))
        else if isMulti then applyEditedBuffer(groupable = false)(applyMultiCursorInsertion(_, "\n"))
        else insertNewlineWithChapterRenumbering(buffer, head, currentState, paneId)

      case ReverseTabKey =>
        val targetLines =
          if hasSelection then selectionLines(buffer)
          else if isMulti then distinctCursorLines(buffer)
          else List(head.line)
        applyEditedBuffer(groupable = false)(applyLineUnindent(_, targetLines))

      case DeleteBackward =>
        if hasSelection then applyEditedBuffer(groupable = false)(deleteSelectedRanges)
        else if isMulti then applyEditedBuffer(groupable = false)(applyMultiCursorDeletion(_, backward = true))
        else reduceDeletion(buffer, currentState, paneId, graphemeBackwardDeletion(_, head))

      case DeleteForward =>
        if hasSelection then applyEditedBuffer(groupable = false)(deleteSelectedRanges)
        else if isMulti then applyEditedBuffer(groupable = false)(applyMultiCursorDeletion(_, backward = false))
        else reduceDeletion(buffer, currentState, paneId, graphemeForwardDeletion(_, head))

      case DeleteWordBackward =>
        if hasSelection then applyEditedBuffer(groupable = false)(deleteSelectedRanges)
        else if isMulti then applyEditedBuffer(groupable = false)(applyMultiCursorWordDeletion(_, backward = true))
        else reduceDeletion(buffer, currentState, paneId, wordBackwardDeletion(_, head))

      case DeleteWordForward =>
        if hasSelection then applyEditedBuffer(groupable = false)(deleteSelectedRanges)
        else if isMulti then applyEditedBuffer(groupable = false)(applyMultiCursorWordDeletion(_, backward = false))
        else reduceDeletion(buffer, currentState, paneId, wordForwardDeletion(_, head))

      case _ =>
        ReducerResult.noEffects(currentState)

  /** All four deletions share a selection arm and differ only in the range they delete when there is none. Deletions
    * are never groupable (#1016) -- only a run of character/tab insertions coalesces into one undo step.
    */
  private def reduceDeletion(
    buffer: Buffer,
    currentState: AppState,
    paneId: PaneId,
    withoutSelection: Buffer => Option[(Buffer, MultiCursorEdit)]
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithIdAndEmit(buffer.id) { current =>
        val result = current.primarySelection match
          case Some(selection) => Some(deleteSelectedRange(current, selection))
          case None            => withoutSelection(current)
        result match
          case Some((updated, edit)) =>
            val edits = List(edit)
            (updated, undoBoundaryEffects(buffer.id, paneId, buffer, edits, groupable = false))
          case None => (current, Nil)
      }
    )

  private def graphemeBackwardDeletion(
    buffer: Buffer,
    cursor: CursorPosition
  ): Option[(Buffer, MultiCursorEdit)] =
    val offset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    backwardGraphemeDeletionRange(buffer.document.content, offset).map {
      case (start, end) =>
        val newContent = deleteOrUnchanged(buffer.document.content, start, end)
        val newCursor  = newContent.offsetToCursorPosition(start)
        val updated = buffer
          .withEditedDocument(newContent, richTextDocumentAfterEdit(buffer, start, end, ""))
          .copy(
            editing = buffer.editing.withPrimary(Cursor(newCursor)),
            annotations = adjustAnnotations(
              buffer.annotations,
              buffer.document.content,
              newContent,
              List(MultiCursorEdit(0, start, end, ""))
            )
          )
        (updated, MultiCursorEdit(0, start, end, ""))
    }

  /** Forward deletion leaves the cursor where it is, so unlike the backward case it does not adjust the viewport. */
  private def graphemeForwardDeletion(buffer: Buffer, cursor: CursorPosition): Option[(Buffer, MultiCursorEdit)] =
    val offset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    forwardGraphemeDeletionRange(buffer.document.content, offset).map {
      case (start, end) =>
        val newContent = deleteOrUnchanged(buffer.document.content, start, end)
        val newCursor  = newContent.offsetToCursorPosition(start)
        val updated = buffer
          .withEditedDocument(newContent, richTextDocumentAfterEdit(buffer, start, end, ""))
          .copy(
            editing = buffer.editing.withPrimary(Cursor(newCursor)),
            annotations = adjustAnnotations(
              buffer.annotations,
              buffer.document.content,
              newContent,
              List(MultiCursorEdit(0, start, end, ""))
            )
          )
        (updated, MultiCursorEdit(0, start, end, ""))
    }

  private def wordBackwardDeletion(buffer: Buffer, cursor: CursorPosition): Option[(Buffer, MultiCursorEdit)] =
    val offset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    val start  = buffer.document.content.previousWordBoundary(offset)
    Option.when(start < offset)(deleteOffsetRange(buffer, start, offset, start))

  private def wordForwardDeletion(buffer: Buffer, cursor: CursorPosition): Option[(Buffer, MultiCursorEdit)] =
    val offset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    val end    = buffer.document.content.nextWordBoundary(offset)
    Option.when(offset < end)(deleteOffsetRange(buffer, offset, end, offset))

  private def deleteOffsetRange(
    buffer: Buffer,
    startOffset: Int,
    endOffset: Int,
    cursorOffset: Int
  ): (Buffer, MultiCursorEdit) =
    val newContent = deleteOrUnchanged(buffer.document.content, startOffset, endOffset)
    val newCursor  = newContent.offsetToCursorPosition(cursorOffset)
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

  /** The single-cursor, no-selection typing path only (#1442-adjacent QoL feature): the multi-cursor/selection cases
    * are rarer for the kind of quote/dash/ellipsis runs this looks at, and are left as plain insertion for now. Returns
    * how many characters before the caret the substitution replaces, and its text.
    */
  private def smartPunctuationReplacement(
    buffer: Buffer,
    cursor: CursorPosition,
    char: Char,
    currentState: AppState
  ): Option[(Int, String)] =
    if !currentState.persisted.config.languageToolsConfig.smartPunctuationEnabled then None
    else
      val content           = buffer.document.content
      val lineStart         = content.lineColumnToOffset(cursor.line, 0)
      val offset            = content.lineColumnToOffset(cursor.line, cursor.column)
      val precedingLineText = content.sliceString(lineStart, offset)
      val replacement       = SmartPunctuation.replacementFor(char, precedingLineText)
      replacement.filter(_ => smartPunctuationApplies(buffer, cursor, precedingLineText, currentState))

  /** Prose only (#1954): a literal `--` or `"` matters in code, whether that's a code buffer or Markdown's code spans
    * and fenced blocks. The fence index is built on demand, so it is consulted only once a rule has already matched.
    */
  private def smartPunctuationApplies(
    buffer: Buffer,
    cursor: CursorPosition,
    precedingLineText: String,
    currentState: AppState
  ): Boolean =
    EditingContext.bufferKind(buffer) match
      case BufferKind.Code(_)                         => false
      case BufferKind.PlainText | BufferKind.RichText => true
      case BufferKind.Markdown =>
        !SmartPunctuation.withinInlineCode(precedingLineText) && !withinFencedCode(currentState, buffer.id, cursor.line)

  private def withinFencedCode(currentState: AppState, bufferId: BufferId, line: Int): Boolean =
    currentState.markdownFenceIndex(bufferId).exists(_.rangeAt(line).isDefined)

  /** Types `char` literally as one more step of the typing run, then substitutes it as an undo step of its own, so the
    * first undo after a substitution restores the literal characters and keeps the rest of the run (#1954).
    */
  private def insertWithSmartPunctuation(
    buffer: Buffer,
    cursor: CursorPosition,
    char: Char,
    substitution: (Int, String),
    currentState: AppState,
    paneId: PaneId
  ): ReducerResult =
    val (charsToReplace, substitutedText) = substitution
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithIdAndEmit(buffer.id) { current =>
        val (literal, literalEdit)        = replaceSelectionOrInsert(current, cursor, char.toString)
        val substitutionStart             = literalEdit.start - charsToReplace
        val literalEnd                    = literalEdit.start + literalEdit.insertedText.length
        val (substituted, substituteEdit) = replaceRange(literal, substitutionStart, literalEnd, substitutedText)
        val literalStep    = undoBoundaryEffects(buffer.id, paneId, buffer, List(literalEdit), groupable = true)
        val substituteStep = undoBoundaryEffects(buffer.id, paneId, literal, List(substituteEdit), groupable = false)
        (substituted, literalStep ++ substituteStep)
      }
    )

  /** Replaces `[startOffset, endOffset)` with `insertedText` and moves the cursor to just past it. */
  private def replaceRange(
    buffer: Buffer,
    startOffset: Int,
    endOffset: Int,
    insertedText: String
  ): (Buffer, MultiCursorEdit) =
    val newContent =
      insertOrUnchanged(
        deleteOrUnchanged(buffer.document.content, startOffset, endOffset),
        startOffset,
        insertedText
      )
    val newCursor = newContent.offsetToCursorPosition(startOffset + insertedText.length)
    val edit      = MultiCursorEdit(0, startOffset, endOffset, insertedText)
    val replaced = buffer
      .withEditedDocument(newContent, richTextDocumentAfterEdit(buffer, startOffset, endOffset, insertedText))
      .copy(
        editing = buffer.editing.withPrimary(Cursor(newCursor)),
        annotations = adjustAnnotations(
          buffer.annotations,
          buffer.document.content,
          newContent,
          List(edit)
        )
      )
    (replaced, edit)

  /** `NewLine`/`Enter`'s single-cursor, no-selection path: inserts the newline, then -- Markdown buffers only, and only
    * when a "Chapter <number>" heading is now out of sequence -- resequences every chapter heading's number in one more
    * edit, folded into the same undo boundary as the newline itself.
    */
  private def insertNewlineWithChapterRenumbering(
    buffer: Buffer,
    cursor: CursorPosition,
    currentState: AppState,
    paneId: PaneId
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithIdAndEmit(buffer.id) { current =>
        val (afterNewline, primaryEdit) = replaceSelectionOrInsert(current, cursor, "\n")
        val newlineCursor = afterNewline.editing.cursorPositions.headOption.getOrElse(CursorPosition(0, 0))
        val newlineCursorOffset =
          afterNewline.document.content.lineColumnToOffset(newlineCursor.line, newlineCursor.column)

        val renumbers = ChapterRenumbering.pendingRenumbers(afterNewline)
        val (contentBuffer, renumberEdits) =
          if renumbers.isEmpty then (afterNewline, Nil)
          else
            val edits = renumbers.zipWithIndex.map {
              case ((start, end, text), index) => MultiCursorEdit(index, start, end, text)
            }
            applyTrackedEdits(afterNewline, List(newlineCursorOffset), edits)

        (contentBuffer, undoBoundaryEffects(buffer.id, paneId, buffer, primaryEdit :: renumberEdits, groupable = false))
      }
    )

  private def insertAtCursor(
    buffer: Buffer,
    cursor: CursorPosition,
    text: String,
    currentState: AppState,
    paneId: PaneId,
    groupable: Boolean
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithIdAndEmit(buffer.id) { current =>
        val (replaced, edit) = replaceSelectionOrInsert(current, cursor, text)
        (replaced, undoBoundaryEffects(buffer.id, paneId, buffer, List(edit), groupable))
      }
    )

  private def applyMultiCursorInsertion(
    buffer: Buffer,
    insertedText: String
  ): (Buffer, List[MultiCursorEdit]) =
    val insertionOffsets =
      multiCursorEntries(buffer).map(entry => buffer.document.content.graphemeBoundaryAfterOrAt(entry.offset))
    val edits = insertionOffsets.zipWithIndex.map {
      case (offset, index) =>
        MultiCursorEdit(index, offset, offset, insertedText)
    }
    applyTrackedEdits(buffer, insertionOffsets, edits)

  private def applyMultiCursorDeletion(
    buffer: Buffer,
    backward: Boolean
  ): (Buffer, List[MultiCursorEdit]) =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.flatMap {
      case (entry, index) =>
        val range =
          if backward then backwardGraphemeDeletionRange(buffer.document.content, entry.offset)
          else forwardGraphemeDeletionRange(buffer.document.content, entry.offset)
        range.map { case (start, end) => MultiCursorEdit(index, start, end, "") }
    }
    applyTrackedEdits(buffer, entries.map(_.offset), edits)

  private def applyMultiCursorWordDeletion(
    buffer: Buffer,
    backward: Boolean
  ): (Buffer, List[MultiCursorEdit]) =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.flatMap {
      case (entry, index) =>
        if backward then
          val start = buffer.document.content.previousWordBoundary(entry.offset)
          Option.when(start < entry.offset)(MultiCursorEdit(index, start, entry.offset, ""))
        else
          val end = buffer.document.content.nextWordBoundary(entry.offset)
          Option.when(entry.offset < end)(MultiCursorEdit(index, entry.offset, end, ""))
    }
    applyMergedDeletionEdits(buffer, entries.map(_.offset), edits)

  private def applyLineIndent(buffer: Buffer, targetLines: List[Int]): (Buffer, List[MultiCursorEdit]) =
    val targetSet = targetLines.filter(line => line >= 0 && line < buffer.document.content.lineCount).toSet

    if targetSet.isEmpty then (buffer, Nil)
    else
      val edits = targetSet.toList.sorted.zipWithIndex.map {
        case (line, index) =>
          val offset = buffer.document.content.lineColumnToOffset(line, 0)
          MultiCursorEdit(index, offset, offset, TabInsertion)
      }
      val folded = foldEditsTracked(buffer, edits.sortBy(edit => (-edit.start, -edit.end))) { (content, edit) =>
        insertOrUnchanged(content, edit.start, edit.insertedText)
      }
      val (updatedContent, updatedRichTextDocument) = (folded.content, folded.richText)
      val appliedEdits                              = folded.appliedAmong(edits)
      val finalCursors = buffer.editing.cursorPositions.map { cursor =>
        if targetSet.contains(cursor.line) then cursor.copy(column = cursor.column + TabInsertion.length)
        else cursor
      }.distinct
      val indented = buffer.withEditedContent(
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
      (indented.clampedToContent, appliedEdits)

  private def applyLineUnindent(
    buffer: Buffer,
    targetLines: List[Int]
  ): (Buffer, List[MultiCursorEdit]) =
    val targetSet = targetLines.filter(line => line >= 0 && line < buffer.document.content.lineCount).toSet
    val removals = targetSet.toList.sorted.map { line =>
      val (_, removed) = unindentLine(buffer.document.content.getLine(line).getOrElse(""))
      line -> removed
    }.toMap

    if removals.values.forall(_ == 0) then (buffer, Nil)
    else
      val edits = removals.toList.sortBy(_._1).zipWithIndex.collect {
        case ((line, removed), index) if removed > 0 =>
          val start = buffer.document.content.lineColumnToOffset(line, 0)
          MultiCursorEdit(index, start, start + removed, "")
      }
      val folded = foldEditsTracked(buffer, edits.sortBy(edit => (-edit.start, -edit.end))) { (content, edit) =>
        deleteOrUnchanged(content, edit.start, edit.end)
      }
      val (updatedContent, updatedRichTextDocument) = (folded.content, folded.richText)
      val appliedEdits                              = folded.appliedAmong(edits)
      val finalCursors = buffer.editing.cursorPositions
        .map(cursor => cursor.copy(column = math.max(0, cursor.column - removals.getOrElse(cursor.line, 0))))
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
      (baseBuffer.clampedToContent, appliedEdits)

  private def unindentLine(lineText: String): (String, Int) =
    if lineText.startsWith("\t") then (lineText.drop(1), 1)
    else
      val spacesToRemove = lineText.take(TabInsertion.length).takeWhile(_ == ' ').length
      if spacesToRemove == 0 then (lineText, 0)
      else (lineText.drop(spacesToRemove), spacesToRemove)
