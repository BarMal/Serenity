package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.state.undo.EditGrouping

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
      case Copy                    => reduceCopy(ctx)
      case Cut                     => reduceCut(ctx)
      case Paste                   => reducePaste(ctx)
      case PasteFromHistory(entry) => pasteEntry(ctx, entry)
      case CutToDarlings           => reduceCutToDarlings(ctx)
      case RestoreDarling          => reduceRestoreDarling(ctx)
      case _                       => ReducerResult.noEffects(ctx.currentState)

  /** The selection, or with none every cursor's whole line, one per line. */
  private def copiedEntry(ctx: CursorEventContext): ClipboardEntry =
    import ctx.*
    if hasSelection then ClipboardEntry(selectedTexts(buffer).mkString("\n"), wholeLine = false)
    else
      ClipboardEntry(
        distinctCursorLines(buffer).map(line => buffer.document.content.getLine(line).getOrElse("")).mkString("\n"),
        wholeLine = true
      )

  private def withCopied(state: AppState, entry: ClipboardEntry): AppState =
    state.copy(runtime =
      state.runtime
        .copy(clipboard = Some(entry.text), clipboardHistory = state.runtime.clipboardHistory.recorded(entry))
    )

  private def reduceCopy(ctx: CursorEventContext): ReducerResult =
    ReducerResult.noEffects(withCopied(ctx.currentState, copiedEntry(ctx)))

  private def reduceCut(ctx: CursorEventContext): ReducerResult =
    import ctx.*
    val entry = copiedEntry(ctx)
    val (updated, edits) =
      if hasSelection then deleteSelectedRanges(buffer)
      else applyMultiCursorLineCut(buffer, distinctCursorLines(buffer))
    ReducerResult(
      withCopied(
        currentState.copy(persisted =
          currentState.persisted.copy(buffers = currentState.persisted.buffers + (buffer.id -> updated))
        ),
        entry
      ),
      undoBoundaryEffects(buffer.id, paneId, buffer, edits, grouping = EditGrouping.Standalone)
    )

  private def reducePaste(ctx: CursorEventContext): ReducerResult =
    val runtime = ctx.currentState.runtime
    runtime.clipboard
      .map(text => runtime.clipboardHistory.entryFor(LineEndings.normalized(text)))
      .filter(entry => entry.text.nonEmpty || entry.wholeLine)
      .fold(ReducerResult.noEffects(ctx.currentState))(pasteEntry(ctx, _))

  private def pasteEntry(ctx: CursorEventContext, entry: ClipboardEntry): ReducerResult =
    import ctx.*

    def applyEditedBuffer(f: Buffer => (Buffer, List[MultiCursorEdit])): ReducerResult =
      val (updated, edits) = f(buffer)
      ReducerResult(
        Focused.replaceBuffer(currentState, updated),
        undoBoundaryEffects(buffer.id, paneId, buffer, edits, grouping = EditGrouping.Standalone)
      )

    val text = entry.text
    if hasSelection then applyEditedBuffer(applyMultiSelectionReplacement(_, text))
    else if entry.wholeLine then applyEditedBuffer(applyWholeLineInsertion(_, text))
    else if isMulti then applyEditedBuffer(applyMultiCursorInsertion(_, text))
    else
      val (replacedBuffer, replacementEdit) = replaceSelectionOrInsert(buffer, head, text)
      val replacedCursor                    = replacedBuffer.editing.cursors.head
      val newCursor                         = replacedCursor.position
      val updatedBuffer = buffer.copy(
        document = replacedBuffer.document,
        editing = buffer.editing.withPrimary(
          Cursor(newCursor, replacedCursor.selectionAnchor, Some(newCursor.column), None)
        ),
        annotations = replacedBuffer.annotations,
        richText = replacedBuffer.richText
      )
      val effects = undoBoundaryEffects(buffer.id, paneId, buffer, List(replacementEdit), grouping = EditGrouping.Standalone)
      ReducerResult(
        currentState.copy(persisted =
          currentState.persisted.copy(buffers = currentState.persisted.buffers + (buffer.id -> updatedBuffer))
        ),
        effects
      )

  /** Neo's "darlings": cuts the active selection into `annotations.darlings` instead of the ordinary clipboard, so it
    * can be brought back later with [[reduceRestoreDarling]] even after other cuts/copies have overwritten the
    * clipboard. A no-op without an active selection -- unlike `Cut`, there is no sensible "whole line" fallback for a
    * passage someone deliberately set aside.
    */
  private def reduceCutToDarlings(ctx: CursorEventContext): ReducerResult =
    import ctx.*
    if hasSelection then
      val cutText          = selectedTexts(buffer).mkString("\n")
      val originalPosition = buffer.primarySelection.map(_.start).getOrElse(head)
      val (updated, edits) = deleteSelectedRanges(buffer)
      val withDarling = updated.copy(
        annotations = updated.annotations.copy(
          darlings = Darling(cutText, originalPosition) :: updated.annotations.darlings
        )
      )
      ReducerResult(
        currentState.copy(persisted =
          currentState.persisted.copy(buffers = currentState.persisted.buffers + (buffer.id -> withDarling))
        ),
        undoBoundaryEffects(buffer.id, paneId, buffer, edits, grouping = EditGrouping.Standalone)
      )
    else ReducerResult.noEffects(currentState)

  /** Restores the most recently cut darling at the cursor (LIFO, mirroring undo), replacing an active selection if
    * there is one -- the single-cursor path only, like `Paste`'s own single-cursor branch this mirrors.
    */
  private def reduceRestoreDarling(ctx: CursorEventContext): ReducerResult =
    import ctx.*
    buffer.annotations.darlings match
      case Nil => ReducerResult.noEffects(currentState)
      case mostRecent :: rest =>
        val bufferWithoutDarling              = buffer.copy(annotations = buffer.annotations.copy(darlings = rest))
        val (replacedBuffer, replacementEdit) = replaceSelectionOrInsert(bufferWithoutDarling, head, mostRecent.text)
        val replacedCursor                    = replacedBuffer.editing.cursors.head
        val newCursor                         = replacedCursor.position
        val updatedBuffer = bufferWithoutDarling.copy(
          document = replacedBuffer.document,
          editing = bufferWithoutDarling.editing.withPrimary(
            Cursor(newCursor, replacedCursor.selectionAnchor, Some(newCursor.column), None)
          ),
          annotations = replacedBuffer.annotations,
          richText = replacedBuffer.richText
        )
        val effects =
          undoBoundaryEffects(buffer.id, paneId, buffer, List(replacementEdit), grouping = EditGrouping.Standalone)
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

  /** Every caret line gets `lines` above it, the carets staying on the text they were on. */
  private def applyWholeLineInsertion(buffer: Buffer, lines: String): (Buffer, List[MultiCursorEdit]) =
    val content = buffer.document.content
    val edits = distinctCursorLines(buffer).zipWithIndex.map {
      case (line, index) =>
        val lineStart = content.lineColumnToOffset(line, 0)
        MultiCursorEdit(index, lineStart, lineStart, lines + "\n")
    }
    applyTrackedEdits(buffer, multiCursorEntries(buffer).map(_.offset), edits)

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
        adjustedAnnotations = adjustAnnotations(
          buffer.annotations,
          buffer.document.content,
          updatedContent,
          edits
        ),
        richTextDocument = updatedRichTextDocument
      )
      (baseBuffer, edits)
