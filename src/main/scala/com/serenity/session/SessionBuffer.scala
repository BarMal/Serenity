package com.serenity.session

import cats.effect.IO
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*
import com.serenity.state.models.*

final case class SessionBuffer(
    id: Int,
    filePath: Option[String], // Use String instead of Path for JSON serialization
    isDirty: Boolean,
    language: Option[String],
    isNewEmpty: Boolean,
    cursors: List[SessionCursorPosition],
    viewport: SessionViewport,
    // Persist buffer text so restore does not depend on disk reads
    unsavedContent: Option[String] = None,
    richTextDocument: Option[RichTextDocument] = None,
    richTextFidelity: Option[RichTextFidelity] = None,
    findState: Option[SessionFindState] = None,
    bookmarks: List[SessionCursorPosition] = Nil,
    documentComments: List[SessionDocumentComment] = Nil
)

final case class SessionCursorPosition(
    line: Int,
    column: Int
)

final case class SessionViewport(
    leftColumn: Int,
    topLine: Int,
    visibleColumns: Int,
    visibleLines: Int,
    topVisualLine: Int = 0
)

final case class SessionFindState(
    query: String,
    results: List[SessionFindResult],
    currentIndex: Int
)

final case class SessionFindResult(
    line: Int,
    column: Int
)

final case class SessionDocumentComment(
    anchor: SessionCursorPosition,
    focus: SessionCursorPosition,
    text: String
)

object SessionBuffer:

  def fromBuffer(buffer: Buffer, persistUnsaved: Boolean = true): SessionBuffer =
    val text = buffer.document.content.toString
    SessionBuffer(
      id = buffer.id.value,
      filePath = buffer.document.filePath.map(_.toString),
      isDirty = buffer.document.isDirty,
      language = buffer.document.language.map(_.id),
      isNewEmpty = buffer.document.isNewEmpty,
      cursors = buffer.editing.cursors.map(SessionCursorPosition.fromCursorPosition),
      viewport = SessionViewport.fromViewport(buffer.viewport),
      unsavedContent =
        if persistUnsaved || (!buffer.document.isDirty && !buffer.document.isNewEmpty) then Some(text)
        else None,
      richTextDocument = buffer.richText.richTextDocument.filter(_.matchesPlainText(text)),
      richTextFidelity = buffer.richText.richTextFidelity,
      findState = buffer.findState.map(SessionFindState.fromFindState),
      bookmarks = buffer.annotations.bookmarks.map(SessionCursorPosition.fromCursorPosition),
      documentComments = buffer.annotations.documentComments.map(SessionDocumentComment.fromDocumentComment)
    )

  def toBuffer(sessionBuffer: SessionBuffer)(using balance: com.serenity.rope.Balance): Buffer =
    import com.serenity.rope.Rope
    import java.nio.file.Paths

    Buffer(
      id = BufferId(sessionBuffer.id),
      document = Document(
        content = sessionBuffer.unsavedContent.map(Rope.apply).getOrElse(Rope.empty),
        filePath = sessionBuffer.filePath.map(path => Paths.get(path)),
        isDirty = sessionBuffer.isDirty,
        language = sessionBuffer.language.flatMap(LanguageId.fromString),
        isNewEmpty = sessionBuffer.isNewEmpty
      ),
      editing = EditingState(cursors = sessionBuffer.cursors.map(SessionCursorPosition.toCursorPosition)),
      viewport = SessionViewport.toViewport(sessionBuffer.viewport),
      findState = sessionBuffer.findState.map(SessionFindState.toFindState),
      annotations = Annotations(
        bookmarks = sessionBuffer.bookmarks.map(SessionCursorPosition.toCursorPosition),
        documentComments = sessionBuffer.documentComments.map(SessionDocumentComment.toDocumentComment)
      ),
      richText = RichTextState(
        richTextDocument = sessionBuffer.richTextDocument,
        richTextFidelity = sessionBuffer.richTextFidelity
      )
    )

  def toBufferIO(sessionBuffer: SessionBuffer)(using balance: com.serenity.rope.Balance): IO[Buffer] =
    import com.serenity.rope.Rope
    import java.nio.file.{Files, Paths}

    sessionBuffer.unsavedContent match
      case Some(_) =>
        IO.pure(toBuffer(sessionBuffer))
      case None =>
        sessionBuffer.richTextDocument match
          case Some(document) =>
            val buffer = toBuffer(sessionBuffer)
            IO.pure(buffer.copy(document = buffer.document.copy(content = Rope(document.plainText), isDirty = false)))
          case None =>
            sessionBuffer.filePath match
              case Some(pathText) =>
                val path = Paths.get(pathText)
                IO.blocking(Files.readString(path))
                  .map { diskContent =>
                    val buffer = toBuffer(sessionBuffer)
                    buffer.copy(document = buffer.document.copy(content = Rope(diskContent), isDirty = false))
                  }
                  .handleError(_ => toBuffer(sessionBuffer))
              case None =>
                IO.pure(toBuffer(sessionBuffer))

object SessionCursorPosition:
  def fromCursorPosition(cursor: CursorPosition): SessionCursorPosition =
    SessionCursorPosition(cursor.line, cursor.column)

  def toCursorPosition(sessionCursor: SessionCursorPosition): CursorPosition =
    CursorPosition(sessionCursor.line, sessionCursor.column)

object SessionViewport:

  def fromViewport(viewport: Viewport): SessionViewport =
    SessionViewport(
      leftColumn = viewport.leftColumn,
      topLine = viewport.topLine,
      visibleColumns = viewport.visibleColumns,
      visibleLines = viewport.visibleLines,
      topVisualLine = viewport.topVisualLine
    )

  def toViewport(sessionViewport: SessionViewport): Viewport =
    Viewport(
      leftColumn = sessionViewport.leftColumn,
      topLine = sessionViewport.topLine,
      visibleColumns = sessionViewport.visibleColumns,
      visibleLines = sessionViewport.visibleLines,
      topVisualLine = sessionViewport.topVisualLine
    )

object SessionFindState:

  def fromFindState(findState: FindState): SessionFindState =
    SessionFindState(
      query = findState.query,
      results = findState.results.map(SessionFindResult.fromFindResult),
      currentIndex = findState.currentIndex
    )

  def toFindState(sessionFindState: SessionFindState): FindState =
    FindState(
      query = sessionFindState.query,
      results = sessionFindState.results.map(SessionFindResult.toFindResult),
      currentIndex = sessionFindState.currentIndex
    )

object SessionFindResult:

  def fromFindResult(findResult: FindResult): SessionFindResult =
    SessionFindResult(findResult.line, findResult.column)

  def toFindResult(sessionFindResult: SessionFindResult): FindResult =
    FindResult(sessionFindResult.line, sessionFindResult.column)

object SessionDocumentComment:

  def fromDocumentComment(comment: DocumentComment): SessionDocumentComment =
    SessionDocumentComment(
      anchor = SessionCursorPosition.fromCursorPosition(comment.anchor),
      focus = SessionCursorPosition.fromCursorPosition(comment.focus),
      text = comment.text
    )

  def toDocumentComment(sessionComment: SessionDocumentComment): DocumentComment =
    DocumentComment(
      anchor = SessionCursorPosition.toCursorPosition(sessionComment.anchor),
      focus = SessionCursorPosition.toCursorPosition(sessionComment.focus),
      text = sessionComment.text
    )
