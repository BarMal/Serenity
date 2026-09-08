package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*

/** Copy/Cut/Paste -- the family that reads or writes the clipboard alongside the buffer. Each event gets its own helper
  * (rather than one large match) since all three independently compute a clipboard string alongside their buffer
  * update. Split out of `EditorEventReducer.reduceCursorsEditEvent`'s sibling dispatch when that file grew past its
  * 600-line target.
  */
private[reducers] object EditorClipboardEventReducer:
  import EditorCursorMovement.*
  import EditorEditSupport.*
  import EditorCursorSupport.CursorEventContext

  def reduce(event: TextEntryEvent, ctx: CursorEventContext): ReducerResult =
    event match
      case Copy  => reduceCopy(ctx)
      case Cut   => reduceCut(ctx)
      case Paste => reducePaste(ctx)
      case _     => ReducerResult.noEffects(ctx.currentState)

  private def reduceCopy(ctx: CursorEventContext): ReducerResult =
    import ctx.*
    if hasSelection then
      ReducerResult.noEffects(
        currentState.copy(runtime = currentState.runtime.copy(clipboard = Some(selectedTexts(buffer).mkString("\n"))))
      )
    else
      val clipboardText =
        distinctCursorLines(buffer)
          .map(line => buffer.document.content.getLine(line).getOrElse(""))
          .mkString("\n")
      ReducerResult.noEffects(
        currentState.copy(runtime = currentState.runtime.copy(clipboard = Some(clipboardText)))
      )

  private def reduceCut(ctx: CursorEventContext): ReducerResult =
    import ctx.*
    if hasSelection then
      val (updated, edits) = deleteSelectedRanges(buffer)
      ReducerResult(
        currentState.copy(
          persisted = currentState.persisted.copy(buffers = currentState.persisted.buffers + (buffer.id -> updated)),
          runtime = currentState.runtime.copy(clipboard = Some(selectedTexts(buffer).mkString("\n")))
        ),
        animationRemapEffects(buffer.id, buffer.document.content, updated.document.content, edits) ++
          undoBoundaryEffects(buffer.id, paneId, buffer, edits, groupable = false)
      )
    else
      val targetLines = distinctCursorLines(buffer)
      val clipboardText =
        targetLines.map(line => buffer.document.content.getLine(line).getOrElse("")).mkString("\n")
      val (updated, edits) = applyMultiCursorLineCut(buffer, targetLines)
      ReducerResult(
        currentState.copy(
          persisted = currentState.persisted.copy(buffers = currentState.persisted.buffers + (buffer.id -> updated)),
          runtime = currentState.runtime.copy(clipboard = Some(clipboardText))
        ),
        animationRemapEffects(buffer.id, buffer.document.content, updated.document.content, edits) ++
          undoBoundaryEffects(buffer.id, paneId, buffer, edits, groupable = false)
      )

  private def reducePaste(ctx: CursorEventContext): ReducerResult =
    import ctx.*

    def applyEditedBuffer(f: Buffer => (Buffer, List[MultiCursorEdit])): ReducerResult =
      val (updated, edits) = f(buffer)
      ReducerResult(
        Focused.replaceBuffer(currentState, updated),
        animationRemapEffects(buffer.id, buffer.document.content, updated.document.content, edits) ++
          undoBoundaryEffects(buffer.id, paneId, buffer, edits, groupable = false)
      )

    currentState.runtime.clipboard.filter(_.nonEmpty) match
      case None => ReducerResult.noEffects(currentState)
      case Some(text) if hasSelection =>
        applyEditedBuffer(applyMultiSelectionReplacement(_, text))
      case Some(text) if isMulti =>
        applyEditedBuffer(applyMultiCursorInsertion(_, text))
      case Some(text) =>
        val (replacedBuffer, replacementEdit) = replaceSelectionOrInsert(buffer, head, text)
        val newCursor                         = replacedBuffer.editing.cursors.headOption.getOrElse(head)
        val withoutAnimations = buffer.copy(
          document = buffer.document.copy(
            content = replacedBuffer.document.content,
            isDirty = replacedBuffer.document.isDirty,
            isNewEmpty = replacedBuffer.document.isNewEmpty
          ),
          editing = buffer.editing.copy(
            cursors = replacedBuffer.editing.cursors,
            selection = replacedBuffer.editing.selection,
            preferredColumn = Some(newCursor.column),
            preferredXPx = None
          ),
          annotations = replacedBuffer.annotations,
          richText = replacedBuffer.richText
        )
        val (updatedBuffer, delta) = addInsertionAnimations(withoutAnimations, currentState, List(replacementEdit))
        val edits                  = List(replacementEdit)
        val effects =
          animationRemapEffects(
            buffer.id,
            buffer.document.content,
            updatedBuffer.document.content,
            edits
          ) ++
            animationMergeEffects(buffer.id, delta) ++
            undoBoundaryEffects(buffer.id, paneId, buffer, edits, groupable = false)
        ReducerResult(
          currentState.copy(persisted =
            currentState.persisted.copy(buffers = currentState.persisted.buffers + (buffer.id -> updatedBuffer))
          ),
          effects
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

  private def applyMultiCursorLineCut(
    buffer: Buffer,
    targetLines: List[Int]
  ): (Buffer, List[MultiCursorEdit]) =
    if targetLines.isEmpty then (buffer, Nil)
    else
      val totalLines = buffer.document.content.lineCount
      val lineEdits = targetLines.distinct.sorted.map { line =>
        val lineText  = buffer.document.content.getLine(line).getOrElse("")
        val lineStart = buffer.document.content.lineColumnToOffset(line, 0)
        val lineEnd   = buffer.document.content.lineColumnToOffset(line, lineText.length)
        val (deleteStart, deleteEnd) =
          if line == 0 && totalLines == 1 then (0, lineEnd)
          else if line < totalLines - 1 then (lineStart, lineEnd + 1)
          else (math.max(0, lineStart - 1), lineEnd)
        (line, deleteStart, deleteEnd)
      }
      val sortedLineEdits = lineEdits
        .sortBy { case (_, start, end) => (-start, -end) }
        .map { case (_, start, end) => MultiCursorEdit(0, start, end, "") }
      val (updatedContent, updatedRichTextDocument) =
        foldEditsWithRichText(buffer, sortedLineEdits)((content, edit) =>
          deleteOrUnchanged(content, edit.start, edit.end)
        )
      val edits = lineEdits.zipWithIndex.map {
        case ((_, start, end), index) =>
          MultiCursorEdit(index, start, end, "")
      }
      val maxFinalLine = math.max(0, updatedContent.lineCount - 1)
      val finalCursors = targetLines.distinct.sorted.map { line =>
        val deletedBefore = targetLines.count(_ < line)
        val targetLine =
          if totalLines == 1 then 0
          else if line < totalLines - 1 then line - deletedBefore
          else line - targetLines.count(_ <= line)
        val clampedLine = math.max(0, math.min(targetLine, maxFinalLine))
        updatedContent.offsetToCursorPosition(updatedContent.lineColumnToOffset(clampedLine, 0))
      }.distinct
      val baseBuffer = buffer.withEditedContent(
        content = updatedContent,
        cursors = finalCursors,
        documentComments =
          adjustDocumentComments(buffer.annotations.documentComments, buffer.document.content, updatedContent, edits),
        richTextDocument = updatedRichTextDocument
      )
      (baseBuffer, edits)
