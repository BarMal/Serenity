package com.serenity.session

import java.nio.file.Paths

import cats.effect.IO
import com.serenity.io.{DocumentRevision, FileManager}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.text.LineEnding

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
    documentComments: List[SessionDocumentComment] = Nil,
    lineEnding: Option[String] = None,
    // The on-disk revision the buffer's content was based on (#1670), so a dirty buffer restored from the session
    // still detects a file changed since.
    revision: Option[String] = None
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
      lineEnding = Some(buffer.document.lineEnding.configKey),
      cursors = buffer.editing.cursorPositions.map(SessionCursorPosition.fromCursorPosition),
      viewport = SessionViewport.fromViewport(buffer.viewport),
      // Clean, file-backed buffers rely on the on-disk file (see toBufferIO's disk-read fallback) --
      // only a buffer with actual unsaved content needs its text re-serialized into session JSON.
      unsavedContent =
        if persistUnsaved || buffer.hasUnsavedChanges then Some(text)
        else None,
      richTextDocument = buffer.richText.richTextDocument.filter(_.matchesPlainText(text)),
      richTextFidelity = buffer.richText.richTextFidelity,
      findState = buffer.findState.map(SessionFindState.fromFindState),
      bookmarks = buffer.annotations.bookmarks.map(SessionCursorPosition.fromCursorPosition),
      documentComments = buffer.annotations.documentComments.map(SessionDocumentComment.fromDocumentComment),
      revision = buffer.document.revision.map(_.value)
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
        isNewEmpty = sessionBuffer.isNewEmpty,
        // A session written before line endings were recorded has no key to restore; LineEnding.default matches
        // what that session's buffers would have been saved with anyway.
        lineEnding = sessionBuffer.lineEnding.flatMap(LineEnding.fromConfigKey).getOrElse(LineEnding.default),
        revision = sessionBuffer.revision.map(DocumentRevision.apply)
      ),
      editing = EditingState(sessionBuffer.cursors.map(SessionCursorPosition.toCursorPosition)),
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

  /** A clean file-backed buffer is read from disk through `FileManager` (#1670): the disk is the truth for it, and the
    * read captures the revision a later save checks against. A dirty one keeps the session's unsaved text and the
    * revision it was edited from. Anything unreadable falls back to what the session recorded.
    */
  def toBufferIO(sessionBuffer: SessionBuffer)(using balance: com.serenity.rope.Balance): IO[Buffer] =
    val recorded = recordedBuffer(sessionBuffer)
    sessionBuffer.filePath.map(Paths.get(_)) match
      case Some(path) if !(sessionBuffer.isDirty && sessionBuffer.unsavedContent.isDefined) =>
        FileManager().loadFile(path, recorded.id).map(fromDisk(recorded, _)).handleError(_ => recorded)
      case _ => IO.pure(recorded)

  private def recordedBuffer(sessionBuffer: SessionBuffer)(using com.serenity.rope.Balance): Buffer =
    val buffer = toBuffer(sessionBuffer)
    (sessionBuffer.unsavedContent, sessionBuffer.richTextDocument) match
      case (None, Some(document)) =>
        buffer.copy(document =
          buffer.document.copy(content = com.serenity.rope.Rope(document.plainText), isDirty = false)
        )
      case _ => buffer

  private def fromDisk(recorded: Buffer, disk: Buffer): Buffer =
    if disk.document.content.collect() == recorded.document.content.collect() then
      recorded.copy(document = recorded.document.copy(revision = disk.document.revision, isDirty = false))
    else
      recorded
        .copy(
          document = disk.document.copy(language = recorded.document.language.orElse(disk.document.language)),
          richText = disk.richText,
          findState = None
        )
        .clampedToContent

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
