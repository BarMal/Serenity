package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.RichTextIntent
import com.serenity.richtext.*
import com.serenity.state.models.*

/** Applies rich-text formatting commands (marks, font, color, paragraph role/alignment) to the active editor
  * selection, materializing a plain-text buffer's `RichTextDocument` on first use.
  */
final private[manager] class StateManagerRichTextEffects(stateRef: Ref[IO, AppState]):

  private def updateState(update: AppState => AppState): IO[Unit] = stateRef.update(update)

  private[manager] def interpret(intent: RichTextIntent): IO[Unit] =
    intent match
      case RichTextIntent.ToggleRichTextMark(mark) =>
        updateState(current => toggleRichTextMark(current, mark))
      case RichTextIntent.SetRichTextFontFamily(family) =>
        updateState(current => setRichTextFontFamily(current, family))
      case RichTextIntent.SetRichTextFontSize(size) =>
        updateState(current => setRichTextFontSize(current, size))
      case RichTextIntent.SetRichTextColor(color) =>
        updateState(current => setRichTextColor(current, color))
      case RichTextIntent.SetRichTextParagraphRole(role) =>
        updateState(current => setRichTextParagraphRole(current, role))
      case RichTextIntent.SetRichTextParagraphAlignment(alignment) =>
        updateState(current => setRichTextParagraphAlignment(current, alignment))

  private def toggleRichTextMark(state: AppState, mark: InlineMark): AppState =
    activeEditorContentBuffer(state) match
      case Some(buffer) =>
        val selections = buffer.allSelections.filter(selection => selection.start != selection.end)
        val text       = buffer.document.content.collect()
        val baseDocument = buffer.richText.richTextDocument
          .filter(_.matchesPlainText(text))
          .getOrElse(RichTextDocument.fromPlainText(text))
        val insertionStyle =
          if selections.isEmpty then
            buffer.richText.insertionRichTextStyle.getOrElse(RichTextStyle.empty) match
              case style if style.marks.contains(mark) => style.withoutMark(mark)
              case style                               => style.withMark(mark)
          else RichTextStyle.empty
        val updatedDocument =
          if selections.isEmpty then baseDocument
          else
            selections
              .foldLeft(baseDocument)((document, selection) => document.toggleMark(richTextRange(selection), mark))
              .normalized
        state.copy(persisted =
          state.persisted.copy(buffers =
            state.persisted.buffers.updated(
              buffer.id,
              buffer.copy(
                document = buffer.document.copy(isDirty = true, isNewEmpty = false),
                richText = buffer.richText.copy(
                  richTextDocument = Some(updatedDocument),
                  insertionRichTextStyle = Some(insertionStyle)
                )
              )
            )
          )
        )
      case None =>
        state

  private def setRichTextParagraphRole(state: AppState, role: ParagraphRole): AppState =
    updateRichTextParagraphs(state)((document, range) => document.setParagraphRole(range, role))

  private def setRichTextParagraphAlignment(state: AppState, alignment: ParagraphAlignment): AppState =
    updateRichTextParagraphs(state)((document, range) => document.setParagraphAlignment(range, alignment))

  private def setRichTextFontFamily(state: AppState, family: String): AppState =
    updateRichTextInlineStyles(state)((document, range) => document.setFontFamily(range, family))

  private def setRichTextFontSize(state: AppState, size: Float): AppState =
    updateRichTextInlineStyles(state)((document, range) => document.setFontSize(range, size))

  private def setRichTextColor(state: AppState, color: String): AppState =
    updateRichTextInlineStyles(state)((document, range) => document.setColor(range, color))

  private def updateRichTextInlineStyles(
    state: AppState
  )(update: (RichTextDocument, RichTextRange) => RichTextDocument): AppState =
    activeEditorContentBuffer(state) match
      case Some(buffer) =>
        val ranges = buffer.allSelections.filter(selection => selection.start != selection.end).map(richTextRange)
        if ranges.isEmpty then state
        else
          val text = buffer.document.content.collect()
          val baseDocument = buffer.richText.richTextDocument
            .filter(_.matchesPlainText(text))
            .getOrElse(RichTextDocument.fromPlainText(text))
          val updatedDocument = ranges.foldLeft(baseDocument)(update).normalized
          if updatedDocument == baseDocument.normalized then state
          else
            state.copy(persisted =
              state.persisted.copy(buffers =
                state.persisted.buffers.updated(
                  buffer.id,
                  buffer.copy(
                    document = buffer.document.copy(isDirty = true, isNewEmpty = false),
                    richText = buffer.richText.copy(richTextDocument = Some(updatedDocument))
                  )
                )
              )
            )
      case None =>
        state

  private def updateRichTextParagraphs(
    state: AppState
  )(update: (RichTextDocument, RichTextRange) => RichTextDocument): AppState =
    activeEditorContentBuffer(state) match
      case Some(buffer) =>
        val ranges = richTextParagraphRanges(buffer)
        if ranges.isEmpty then state
        else
          val text = buffer.document.content.collect()
          val baseDocument = buffer.richText.richTextDocument
            .filter(_.matchesPlainText(text))
            .getOrElse(RichTextDocument.fromPlainText(text))
          val updatedDocument = ranges.foldLeft(baseDocument)(update).normalized
          if updatedDocument == baseDocument.normalized then state
          else
            state.copy(persisted =
              state.persisted.copy(buffers =
                state.persisted.buffers.updated(
                  buffer.id,
                  buffer.copy(
                    document = buffer.document.copy(isDirty = true, isNewEmpty = false),
                    richText = buffer.richText.copy(richTextDocument = Some(updatedDocument))
                  )
                )
              )
            )
      case None =>
        state

  private def richTextParagraphRanges(buffer: Buffer): List[RichTextRange] =
    val selections = buffer.allSelections.filter(selection => selection.start != selection.end).map(richTextRange)
    if selections.nonEmpty then selections
    else
      buffer.editing.cursors.distinct.map { cursor =>
        RichTextRange(
          start = RichTextPosition(cursor.line, cursor.column),
          end = RichTextPosition(cursor.line, cursor.column)
        )
      }

  private def activeEditorContentBuffer(state: AppState): Option[Buffer] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)

  private def richTextRange(selection: Selection): RichTextRange =
    RichTextRange(
      start = RichTextPosition(selection.start.line, selection.start.column),
      end = RichTextPosition(selection.end.line, selection.end.column)
    )
