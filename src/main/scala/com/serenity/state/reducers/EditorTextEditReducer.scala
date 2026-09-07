package com.serenity.state.reducers

import com.serenity.animation.*
import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*

/** Character/newline/tab insertion, indent/unindent and the four deletions -- the family that mutates document content
  * directly at the cursor(s). Split out of `EditorEventReducer.reduceCursorsEditEvent` when that file grew past its
  * 600-line target.
  */
private[reducers] object EditorTextEditReducer:
  import EditorCursorMovement.*
  import EditorEditSupport.*
  import EditorEventReducer.{CursorEventContext, TabInsertion}

  def reduce(event: TextEntryEvent, ctx: CursorEventContext): ReducerResult =
    import ctx.*

    /** Like a plain buffer update, but `f` also reports the edits it made, so their animations can be remapped in the
      * presentation layer (`#1001`) instead of inside `Buffer` itself.
      */
    def applyEditedBuffer(f: Buffer => (Buffer, List[MultiCursorEdit])): ReducerResult =
      val (updated, edits) = f(buffer)
      ReducerResult(
        Focused.replaceBuffer(currentState, updated),
        animationRemapEffects(buffer.id, buffer.document.content, updated.document.content, edits)
      )

    event match
      case InsertChar(char) =>
        if hasSelection then applyEditedBuffer(applyMultiSelectionReplacement(_, char.toString))
        else if isMulti then applyEditedBuffer(applyMultiCursorInsertion(_, char.toString))
        else insertAtCursor(buffer, head, char.toString, currentState)

      case TabKey =>
        if hasSelection then
          val (updated, edits, delta) = applyLineIndent(buffer, currentState, selectionLines(buffer))
          val effects =
            animationRemapEffects(buffer.id, buffer.document.content, updated.document.content, edits) ++
              animationMergeEffects(buffer.id, delta)
          ReducerResult(Focused.replaceBuffer(currentState, updated), effects)
        else if isMulti then applyEditedBuffer(applyMultiCursorInsertion(_, TabInsertion))
        else insertAtCursor(buffer, head, TabInsertion, currentState)

      case NewLine | Enter =>
        if hasSelection then applyEditedBuffer(applyMultiSelectionReplacement(_, "\n"))
        else if isMulti then applyEditedBuffer(applyMultiCursorInsertion(_, "\n"))
        else insertAtCursor(buffer, head, "\n", currentState)

      case ReverseTabKey =>
        val targetLines =
          if hasSelection then selectionLines(buffer)
          else if isMulti then distinctCursorLines(buffer)
          else List(head.line)
        applyEditedBuffer(applyLineUnindent(_, targetLines))

      case DeleteBackward =>
        if hasSelection then applyEditedBuffer(deleteSelectedRanges)
        else if isMulti then applyEditedBuffer(applyMultiCursorDeletion(_, backward = true))
        else reduceDeletion(buffer, currentState, graphemeBackwardDeletion(_, head))

      case DeleteForward =>
        if hasSelection then applyEditedBuffer(deleteSelectedRanges)
        else if isMulti then applyEditedBuffer(applyMultiCursorDeletion(_, backward = false))
        else reduceDeletion(buffer, currentState, graphemeForwardDeletion(_, head))

      case DeleteWordBackward =>
        if hasSelection then applyEditedBuffer(deleteSelectedRanges)
        else if isMulti then applyEditedBuffer(applyMultiCursorWordDeletion(_, backward = true))
        else reduceDeletion(buffer, currentState, wordBackwardDeletion(_, head))

      case DeleteWordForward =>
        if hasSelection then applyEditedBuffer(deleteSelectedRanges)
        else if isMulti then applyEditedBuffer(applyMultiCursorWordDeletion(_, backward = false))
        else reduceDeletion(buffer, currentState, wordForwardDeletion(_, head))

      case _ =>
        ReducerResult.noEffects(currentState)

  /** All four deletions share a selection arm and differ only in the range they delete when there is none. */
  private def reduceDeletion(
    buffer: Buffer,
    currentState: AppState,
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
            (updated, animationRemapEffects(buffer.id, current.document.content, updated.document.content, List(edit)))
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
        val updated = buffer.copy(
          document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
          editing = buffer.editing.copy(
            cursors = replacePrimaryCursor(newCursor, buffer.editing.cursors),
            selection = None,
            preferredColumn = Some(newCursor.column),
            preferredXPx = None
          ),
          annotations = buffer.annotations.copy(
            documentComments = adjustDocumentComments(
              buffer.annotations.documentComments,
              buffer.document.content,
              newContent,
              List(MultiCursorEdit(0, start, end, ""))
            )
          ),
          richText = buffer.richText.copy(richTextDocument = richTextDocumentAfterEdit(buffer, start, end, ""))
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
        val updated = buffer.copy(
          document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
          editing = buffer.editing.copy(
            cursors = replacePrimaryCursor(newCursor, buffer.editing.cursors),
            preferredColumn = Some(newCursor.column),
            preferredXPx = None
          ),
          annotations = buffer.annotations.copy(
            documentComments = adjustDocumentComments(
              buffer.annotations.documentComments,
              buffer.document.content,
              newContent,
              List(MultiCursorEdit(0, start, end, ""))
            )
          ),
          richText = buffer.richText.copy(richTextDocument = richTextDocumentAfterEdit(buffer, start, end, ""))
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
    val baseBuffer = buffer.copy(
      document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
      editing = buffer.editing.copy(
        cursors = replacePrimaryCursor(newCursor, buffer.editing.cursors),
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

  private[reducers] def insertAtCursor(
    buffer: Buffer,
    cursor: CursorPosition,
    text: String,
    currentState: AppState
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithIdAndEmit(buffer.id) { current =>
        val (replaced, edit)  = replaceSelectionOrInsert(current, cursor, text)
        val (animated, delta) = addInsertionAnimations(replaced, currentState, List(edit))
        val effects =
          animationRemapEffects(buffer.id, current.document.content, animated.document.content, List(edit)) ++
            animationMergeEffects(buffer.id, delta)
        (animated, effects)
      }
    )

  private[reducers] def replaceSelectionOrInsert(
    buffer: Buffer,
    cursor: CursorPosition,
    insertedText: String
  ): (Buffer, MultiCursorEdit) =
    val (baseContent, insertionStart, startOffset, endOffset) = buffer.primarySelection match
      case Some(selection) =>
        val startOffset = selectionStartOffset(selection, buffer.document.content)
        val endOffset   = selectionEndOffset(selection, buffer.document.content)
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
          cursors = replacePrimaryCursor(newCursor, buffer.editing.cursors),
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

  private[reducers] def cursorAfterInsertion(start: CursorPosition, insertedText: String): CursorPosition =
    val lines = insertedText.split("\n", -1)
    if lines.length == 1 then start.copy(column = start.column + insertedText.length)
    else CursorPosition(start.line + lines.length - 1, lines.last.length)

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

  /** Returns the buffer with the indent applied and its own edits (for the caller's animation remap), plus the
    * insertion-animation delta from `addInsertionAnimations` (for the caller's animation merge) -- two independent
    * animation effects, since one shifts existing animations and the other adds new ones.
    */
  private def applyLineIndent(
    buffer: Buffer,
    currentState: AppState,
    targetLines: List[Int]
  ): (Buffer, List[MultiCursorEdit], Map[CharacterKey, AnimatedCell]) =
    val targetSet = targetLines.filter(line => line >= 0 && line < buffer.document.content.lineCount).toSet

    if targetSet.isEmpty then (buffer, Nil, Map.empty)
    else
      val edits = targetSet.toList.sorted.zipWithIndex.map {
        case (line, index) =>
          val offset = buffer.document.content.lineColumnToOffset(line, 0)
          MultiCursorEdit(index, offset, offset, TabInsertion)
      }
      val (updatedContent, updatedRichTextDocument) =
        foldEditsWithRichText(buffer, edits.sortBy(edit => (-edit.start, -edit.end))) { (content, edit) =>
          insertOrUnchanged(content, edit.start, edit.insertedText)
        }
      val finalCursors = buffer.editing.cursors.map { cursor =>
        if targetSet.contains(cursor.line) then cursor.copy(column = cursor.column + TabInsertion.length)
        else cursor
      }.distinct
      val baseBuffer = buffer.withEditedContent(
        content = updatedContent,
        cursors = finalCursors,
        documentComments =
          adjustDocumentComments(buffer.annotations.documentComments, buffer.document.content, updatedContent, edits),
        richTextDocument = updatedRichTextDocument
      )
      val (animatedBuffer, delta) = addInsertionAnimations(baseBuffer, currentState, edits)
      (animatedBuffer, edits, delta)

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
      val (updatedContent, updatedRichTextDocument) =
        foldEditsWithRichText(buffer, edits.sortBy(edit => (-edit.start, -edit.end))) { (content, edit) =>
          deleteOrUnchanged(content, edit.start, edit.end)
        }
      val finalCursors = buffer.editing.cursors
        .map(cursor => cursor.copy(column = math.max(0, cursor.column - removals.getOrElse(cursor.line, 0))))
        .distinct
      val baseBuffer = buffer.withEditedContent(
        content = updatedContent,
        cursors = finalCursors,
        documentComments =
          adjustDocumentComments(buffer.annotations.documentComments, buffer.document.content, updatedContent, edits),
        richTextDocument = updatedRichTextDocument
      )
      (baseBuffer, edits)

  private def unindentLine(lineText: String): (String, Int) =
    if lineText.startsWith("\t") then (lineText.drop(1), 1)
    else
      val spacesToRemove = lineText.take(TabInsertion.length).takeWhile(_ == ' ').length
      if spacesToRemove == 0 then (lineText, 0)
      else (lineText.drop(spacesToRemove), spacesToRemove)
