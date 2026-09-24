package com.serenity.state.reducers

import com.serenity.command.RichTextIntent
import com.serenity.richtext.*
import com.serenity.state.models.*

/** Rich-text formatting commands (marks, font, color, paragraph role/alignment) applied to the active editor selection,
  * materializing a plain-text buffer's `RichTextDocument` on first use.
  */
object RichTextReducer:

  /** The size an un-sized (body) run is treated as when a relative font-size adjust has no explicit size to start from.
    */
  val DefaultBodyFontSize: Float = 12.0f

  def reduce(intent: RichTextIntent, state: AppState): ReducerResult =
    ReducerResult.noEffects(intent match
      case RichTextIntent.ToggleRichTextMark(mark) =>
        toggleMark(state, mark)
      case RichTextIntent.SetRichTextFontFamily(family) =>
        updateInlineStyles(state)((document, range) => document.setFontFamily(range, family))
      case RichTextIntent.SetRichTextFontSize(size) =>
        updateInlineStyles(state)((document, range) => document.setFontSize(range, size))
      case RichTextIntent.AdjustRichTextFontSize(deltaPt) =>
        updateInlineStyles(state)((document, range) => document.adjustFontSize(range, deltaPt, DefaultBodyFontSize))
      case RichTextIntent.SetRichTextColor(color) =>
        updateInlineStyles(state)((document, range) => document.setColor(range, color))
      case RichTextIntent.SetRichTextParagraphRole(role) =>
        updateParagraphs(state)((document, range) => document.setParagraphRole(range, role))
      case RichTextIntent.SetRichTextParagraphAlignment(alignment) =>
        updateParagraphs(state)((document, range) => document.setParagraphAlignment(range, alignment)))

  private def toggleMark(state: AppState, mark: InlineMark): AppState =
    activeEditorBuffer(state) match
      case Some(buffer) =>
        val ranges       = selectionRanges(buffer)
        val baseDocument = currentDocument(buffer)
        val insertionStyle =
          if ranges.isEmpty then
            buffer.richText.insertionRichTextStyle.getOrElse(RichTextStyle.empty) match
              case style if style.marks.contains(mark) => style.withoutMark(mark)
              case style                               => style.withMark(mark)
          else RichTextStyle.empty
        val updatedDocument =
          if ranges.isEmpty then baseDocument
          else ranges.foldLeft(baseDocument)((document, range) => document.toggleMark(range, mark)).normalized
        withEditedBuffer(
          state,
          buffer,
          buffer.richText.copy(richTextDocument = Some(updatedDocument), insertionRichTextStyle = Some(insertionStyle))
        )
      case None =>
        state

  private def updateInlineStyles(state: AppState)(
    update: (RichTextDocument, RichTextRange) => RichTextDocument
  ): AppState =
    updateDocument(state, selectionRanges)(update)

  private def updateParagraphs(state: AppState)(
    update: (RichTextDocument, RichTextRange) => RichTextDocument
  ): AppState =
    updateDocument(state, paragraphRanges)(update)

  private def updateDocument(state: AppState, rangesOf: Buffer => List[RichTextRange])(
    update: (RichTextDocument, RichTextRange) => RichTextDocument
  ): AppState =
    activeEditorBuffer(state) match
      case Some(buffer) =>
        val ranges = rangesOf(buffer)
        if ranges.isEmpty then state
        else
          val baseDocument    = currentDocument(buffer)
          val updatedDocument = ranges.foldLeft(baseDocument)(update).normalized
          if updatedDocument == baseDocument.normalized then state
          else withEditedBuffer(state, buffer, buffer.richText.copy(richTextDocument = Some(updatedDocument)))
      case None =>
        state

  private def withEditedBuffer(state: AppState, buffer: Buffer, richText: RichTextState): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          buffer.id,
          buffer.copy(document = buffer.document.copy(isDirty = true, isNewEmpty = false), richText = richText)
        )
      )
    )

  /** The buffer's rich-text document, or a fresh one from its plain text when there is none or it has gone stale. */
  private def currentDocument(buffer: Buffer): RichTextDocument =
    val text = buffer.document.content.collect()
    buffer.richText.richTextDocument
      .filter(_.matchesPlainText(text))
      .getOrElse(RichTextDocument.fromPlainText(text))

  private def selectionRanges(buffer: Buffer): List[RichTextRange] =
    buffer.allSelections.filter(selection => selection.start != selection.end).map(richTextRange)

  private def paragraphRanges(buffer: Buffer): List[RichTextRange] =
    val selections = selectionRanges(buffer)
    if selections.nonEmpty then selections
    else
      buffer.editing.cursorPositions.distinct.map { cursor =>
        val position = RichTextPosition(cursor.line, cursor.column)
        RichTextRange(start = position, end = position)
      }

  private def activeEditorBuffer(state: AppState): Option[Buffer] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)

  private def richTextRange(selection: Selection): RichTextRange =
    RichTextRange(
      start = RichTextPosition(selection.start.line, selection.start.column),
      end = RichTextPosition(selection.end.line, selection.end.column)
    )
