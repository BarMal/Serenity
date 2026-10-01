package com.serenity.state.reducers

import com.serenity.command.RichTextIntent
import com.serenity.richtext.*
import com.serenity.state.models.*

/** What a rich-text command does, given the kind of buffer it would format. */
enum RichTextRoute:
  case Apply
  case AskToConvert(prompt: ConfirmPrompt)
  case Refuse(reason: String)

/** Rich-text formatting commands (marks, font, color, paragraph role/alignment) applied to the active editor selection,
  * materializing a plain-text buffer's `RichTextDocument` on first use.
  */
object RichTextReducer:

  /** The size an un-sized (body) run is treated as when a relative font-size adjust has no explicit size to start from.
    */
  val DefaultBodyFontSize: Float = 12.0f

  /** An untitled buffer has no format yet, so it takes formatting without asking; Save As warns if its chosen format
    * would drop it. A plain-text file can't store formatting, so converting one is asked first.
    */
  def route(intent: RichTextIntent, state: AppState): RichTextRoute =
    activeEditorBuffer(state).fold(RichTextRoute.Apply) { buffer =>
      EditingContext.bufferKind(buffer) match
        case BufferKind.Code(_) => RichTextRoute.Refuse("Formatting isn't available in code files.")
        case BufferKind.PlainText =>
          (intent, buffer.document.filePath) match
            case (RichTextIntent.ConvertToRichText(_), _) | (_, None) => RichTextRoute.Apply
            case (_, Some(path)) =>
              val label = Option(path.getFileName).fold(path.toString)(_.toString)
              RichTextRoute.AskToConvert(ConfirmPrompt.convertToRichText(label, intent))
        case BufferKind.Markdown =>
          MarkdownFormattingReducer.syntaxFor(intent).fold(RichTextRoute.Refuse(_), _ => RichTextRoute.Apply)
        case BufferKind.RichText => RichTextRoute.Apply
    }

  /** A Markdown file is formatted by editing its source (see [[MarkdownFormattingReducer]]); every other buffer through
    * its rich-text document.
    */
  def reduce(intent: RichTextIntent, state: AppState): ReducerResult =
    activeMarkdownBuffer(state) match
      case Some((paneId, buffer)) => MarkdownFormattingReducer.reduce(intent, state, paneId, buffer)
      case None                   => ReducerResult.noEffects(reduceRichText(intent, state))

  private def activeMarkdownBuffer(state: AppState): Option[(PaneId, Buffer)] =
    for
      paneId <- state.persisted.layout.activeEditorPaneId
      buffer <- activeEditorBuffer(state)
      if EditingContext.bufferKind(buffer) == BufferKind.Markdown
    yield (paneId, buffer)

  private def reduceRichText(intent: RichTextIntent, state: AppState): AppState =
    intent match
      case RichTextIntent.ConvertToRichText(andThen) =>
        val converted = convert(state)
        andThen.fold(converted)(reduceRichText(_, converted))
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
        updateParagraphs(state)((document, range) => document.setParagraphAlignment(range, alignment))

  private def convert(state: AppState): AppState =
    activeEditorBuffer(state).fold(state) { buffer =>
      val converted =
        buffer.richText.withSyncedDocument(Some(currentDocument(buffer)), buffer.document.contentVersion)
      state.copy(persisted =
        state.persisted.copy(buffers = state.persisted.buffers.updated(buffer.id, buffer.copy(richText = converted)))
      )
    }

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
          buffer.richText
            .withSyncedDocument(Some(updatedDocument), buffer.document.contentVersion)
            .copy(insertionRichTextStyle = Some(insertionStyle))
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
          else
            withEditedBuffer(
              state,
              buffer,
              buffer.richText.withSyncedDocument(Some(updatedDocument), buffer.document.contentVersion)
            )
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

  /** The buffer's rich-text document, or a fresh one from its plain text when there is none or it has gone stale.
    * `O(1)` in the common (in-sync) case (`#1663`): `buffer.document.content.collect()` -- itself `O(n)` -- is only
    * ever forced inside `getOrElse`, so it isn't paid unless the document actually needs rebuilding.
    */
  private def currentDocument(buffer: Buffer): RichTextDocument =
    buffer.richText.richTextDocument
      .filter(_ => buffer.richTextInSync)
      .getOrElse(RichTextDocument.fromPlainText(buffer.document.content.collect()))

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
