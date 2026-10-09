package com.serenity.session

import java.nio.file.Paths
import java.time.Instant

import cats.effect.IO
import com.serenity.io.{DocumentRevision, FileManager, FileType}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.text.{LineEnding, TextEncoding}

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
    richTextFidelity: Option[FidelityReport] = None,
    findState: Option[SessionFindState] = None,
    bookmarks: List[SessionCursorPosition] = Nil,
    documentComments: List[SessionDocumentComment] = Nil,
    placeholders: List[SessionPlaceholder] = Nil,
    darlings: List[SessionDarling] = Nil,
    lineEnding: Option[String] = None,
    encoding: Option[String] = None,
    hasBom: Boolean = false,
    // The on-disk revision the buffer's content was based on (#1670), so a dirty buffer restored from the session
    // still detects a file changed since.
    revision: Option[String] = None,
    hidden: Boolean = false,
    notes: List[SessionNote] = Nil,
    // Names the file holding this buffer's unsaved text, in place of `unsavedContent`, once the session is on disk
    // (#1912); SessionContentStore moves the text between the two.
    contentRef: Option[String] = None,
    // The next comment id to hand out (#1903), so a deleted comment's id is not given to a new one after a restore. A
    // session written before comment ids has none, and the ids are assigned on restore.
    nextCommentId: Option[Int] = None
)

/** One note, flattened for JSON: `kind` says whether `title` is a chapter heading (with its `occurrence`) or a keyword.
  */
final case class SessionNote(kind: String, title: String, occurrence: Int, overview: Int, extra: Option[Int])

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

/** One comment, with its thread. Everything after `text` is absent from a session written before comments had ids,
  * authors and threads (#1903); times are epoch milliseconds.
  */
final case class SessionDocumentComment(
    anchor: SessionCursorPosition,
    focus: SessionCursorPosition,
    text: String,
    id: Option[Int] = None,
    author: Option[String] = None,
    createdAt: Option[Long] = None,
    editedAt: Option[Long] = None,
    replies: List[SessionCommentReply] = Nil,
    resolved: Boolean = false
)

final case class SessionCommentReply(author: String, at: Long, text: String)

final case class SessionPlaceholder(
    position: SessionCursorPosition,
    note: String
)

final case class SessionDarling(
    text: String,
    originalPosition: SessionCursorPosition
)

/** A buffer's text as of one content version, still a rope: collecting it is left to whoever needs the characters. */
final private[session] case class UnsavedText(content: Rope, version: Long)

object SessionBuffer:

  def fromBuffer(buffer: Buffer, persistUnsaved: Boolean = true): SessionBuffer =
    val (session, unsaved) = snapshot(buffer, persistUnsaved)
    session.copy(unsavedContent = unsaved.map(_.content.toString))

  /** The buffer's session entry without its unsaved text, and that text where the entry is to carry it. Clean buffers
    * are described without touching their content, so a snapshot costs nothing in the size of what is merely open.
    */
  private[session] def snapshot(buffer: Buffer, persistUnsaved: Boolean): (SessionBuffer, Option[UnsavedText]) =
    val content = buffer.document.content
    val unsaved =
      Option.when(persistUnsaved || buffer.hidden || buffer.hasUnsavedChanges)(
        UnsavedText(content, buffer.document.contentVersion)
      )
    // The stamp vouches for a document in sync; one without it is compared with the text, which is the only case
    // that collects it.
    val richTextDocument =
      buffer.richText.richTextDocument.filter(document =>
        buffer.richTextInSync || document.matchesPlainText(content.toString)
      )
    val session = SessionBuffer(
      id = buffer.id.value,
      filePath = buffer.document.filePath.map(_.toString),
      isDirty = buffer.document.isDirty,
      language = buffer.document.language.map(_.id),
      isNewEmpty = buffer.document.isNewEmpty,
      lineEnding = Some(buffer.document.lineEnding.configKey),
      encoding = Some(buffer.document.encoding.configKey),
      hasBom = buffer.document.hasBom,
      cursors = buffer.editing.cursorPositions.map(SessionCursorPosition.fromCursorPosition),
      viewport = SessionViewport.fromViewport(buffer.viewport),
      // Clean, file-backed buffers rely on the on-disk file (see toBufferIO's disk-read fallback) --
      // only a buffer with actual unsaved content needs its text re-serialized into session JSON.
      richTextDocument = richTextDocument,
      richTextFidelity = buffer.richText.richTextFidelity,
      findState = buffer.findState.map(SessionFindState.fromFindState),
      bookmarks = buffer.annotations.bookmarks.map(SessionCursorPosition.fromCursorPosition),
      documentComments = buffer.annotations.documentComments.map(SessionDocumentComment.fromDocumentComment),
      nextCommentId = Some(buffer.annotations.nextCommentId.value),
      placeholders = buffer.annotations.placeholders.map(SessionPlaceholder.fromPlaceholder),
      darlings = buffer.annotations.darlings.map(SessionDarling.fromDarling),
      revision = buffer.document.revision.map(_.value),
      hidden = buffer.hidden,
      notes = buffer.annotations.notes.toList.map(SessionNote.fromEntry).sortBy(SessionNote.order)
    )
    (session, unsaved)

  def toBuffer(sessionBuffer: SessionBuffer)(using balance: com.serenity.rope.Balance): Buffer =
    Buffer(
      id = BufferId(sessionBuffer.id),
      document = Document(
        // A clean rich-text buffer's session entry holds the formatted document instead of the text it describes.
        content = sessionBuffer.unsavedContent
          .orElse(sessionBuffer.richTextDocument.map(_.plainText))
          .map(Rope.apply)
          .getOrElse(Rope.empty),
        filePath = sessionBuffer.filePath.map(path => Paths.get(path)),
        isDirty = sessionBuffer.isDirty,
        language = sessionBuffer.language.flatMap(LanguageId.fromString),
        isNewEmpty = sessionBuffer.isNewEmpty,
        // A session written before line endings were recorded has no key to restore; LineEnding.default matches
        // what that session's buffers would have been saved with anyway.
        lineEnding = sessionBuffer.lineEnding.flatMap(LineEnding.fromConfigKey).getOrElse(LineEnding.default),
        // Likewise for a session written before encodings were recorded (#1627): every file was read as UTF-8.
        encoding = sessionBuffer.encoding.flatMap(TextEncoding.fromConfigKey).getOrElse(TextEncoding.default),
        hasBom = sessionBuffer.hasBom,
        revision = sessionBuffer.revision.map(DocumentRevision(_))
      ),
      editing = EditingState(sessionBuffer.cursors.map(SessionCursorPosition.toCursorPosition)),
      viewport = SessionViewport.toViewport(sessionBuffer.viewport),
      findState = sessionBuffer.findState.map(SessionFindState.toFindState),
      annotations = Annotations(
        bookmarks = sessionBuffer.bookmarks.map(SessionCursorPosition.toCursorPosition),
        documentComments = sessionBuffer.documentComments.map(SessionDocumentComment.toDocumentComment),
        placeholders = sessionBuffer.placeholders.map(SessionPlaceholder.toPlaceholder),
        darlings = sessionBuffer.darlings.map(SessionDarling.toDarling),
        notes = sessionBuffer.notes.flatMap(SessionNote.toEntry).toMap,
        nextCommentId = sessionBuffer.nextCommentId.fold(CommentId(1))(CommentId.apply)
      ).withCommentIdsAssigned,
      // `fromBuffer` only ever persists a `richTextDocument` that passed `matchesPlainText` against the exact text
      // being saved, and the `Document` just built above starts at its default `contentVersion` of `0L` -- so a
      // restored document is, by construction, in sync with this fresh buffer at that version (#1663).
      richText = RichTextState()
        .withSyncedDocument(sessionBuffer.richTextDocument, contentVersion = 0L)
        .copy(richTextFidelity = sessionBuffer.richTextFidelity),
      hidden = sessionBuffer.hidden
    )

  /** A clean file-backed buffer is read from disk through `FileManager` (#1670): the disk is the truth for it, and the
    * read captures the revision a later save checks against. A dirty one keeps the session's unsaved text and the
    * revision it was edited from. Anything unreadable falls back to what the session recorded.
    */
  def toBufferIO(sessionBuffer: SessionBuffer, files: FileManager)(using com.serenity.rope.Balance): IO[Buffer] =
    val recorded = recordedBuffer(sessionBuffer)
    val restored = sessionBuffer.filePath.map(Paths.get(_)) match
      case Some(path) if !(sessionBuffer.isDirty && sessionBuffer.unsavedContent.isDefined) =>
        files.loadFile(path, recorded.id).map(fromDisk(recorded, _)).handleError(_ => recorded)
      case _ => IO.pure(recorded)
    restored.flatMap(withSourcePackage(_, files))

  /** A restored rich document keeps no package bytes, so a DOCX or ODT one is linked to its file read again. The file
    * counts only when it is still at the revision the buffer was read or written at (the stat check): a file changed
    * since has other body blocks than the ones the document remembers. A document that cannot be linked is saved with a
    * warning that the rest of the original package would be lost, never silently.
    */
  private def withSourcePackage(buffer: Buffer, files: FileManager): IO[Buffer] =
    (buffer.richText.richTextDocument, buffer.document.filePath) match
      case (Some(document), Some(path)) if document.source.isEmpty && keepsPackage(path) =>
        files
          .importPackage(path, buffer.document.revision)
          .map(_.fold(detached(buffer, document, path))(imported => linked(buffer, document.relinkedTo(imported))))
          .handleError(_ => detached(buffer, document, path))
      case _ => IO.pure(buffer)

  private def keepsPackage(path: java.nio.file.Path): Boolean =
    FileType.saveTarget(FileType.fromPath(path)).packageFormat.isDefined

  private def linked(buffer: Buffer, document: RichTextDocument): Buffer =
    buffer.copy(richText = buffer.richText.copy(richTextDocument = Some(document)))

  private def detached(buffer: Buffer, document: RichTextDocument, path: java.nio.file.Path): Buffer =
    buffer.copy(richText =
      buffer.richText.copy(richTextFidelity =
        Some(
          FidelityReport.forDetached(
            document,
            FileType.saveTarget(FileType.fromPath(path)),
            buffer.richText.richTextFidelity
          )
        )
      )
    )

  private def recordedBuffer(sessionBuffer: SessionBuffer)(using com.serenity.rope.Balance): Buffer =
    val buffer = toBuffer(sessionBuffer)
    (sessionBuffer.unsavedContent, sessionBuffer.richTextDocument) match
      case (None, Some(_)) => buffer.copy(document = buffer.document.copy(isDirty = false))
      case _               => buffer

  private def fromDisk(recorded: Buffer, disk: Buffer): Buffer =
    if disk.document.content == recorded.document.content then
      recorded.copy(
        document = recorded.document.copy(revision = disk.document.revision, isDirty = false),
        // The file just read is the package the unchanged document came from, with every paragraph's source.
        richText =
          if recorded.richText.richTextDocument == disk.richText.richTextDocument then
            recorded.richText.copy(richTextDocument = disk.richText.richTextDocument)
          else recorded.richText
      )
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
      results = findState.results.map(SessionFindResult.fromFindResult).toList,
      currentIndex = findState.currentIndex
    )

  def toFindState(sessionFindState: SessionFindState): FindState =
    FindState(
      query = sessionFindState.query,
      results = sessionFindState.results.map(SessionFindResult.toFindResult).toVector,
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
      text = comment.text,
      id = Some(comment.id.value),
      author = comment.author,
      createdAt = comment.createdAt.map(_.toEpochMilli),
      editedAt = comment.editedAt.map(_.toEpochMilli),
      replies = comment.replies.map(reply => SessionCommentReply(reply.author, reply.at.toEpochMilli, reply.text)),
      resolved = comment.resolved
    )

  def toDocumentComment(sessionComment: SessionDocumentComment): DocumentComment =
    DocumentComment(
      anchor = SessionCursorPosition.toCursorPosition(sessionComment.anchor),
      focus = SessionCursorPosition.toCursorPosition(sessionComment.focus),
      text = sessionComment.text,
      id = sessionComment.id.fold(CommentId.Unassigned)(CommentId.apply),
      author = sessionComment.author,
      createdAt = sessionComment.createdAt.map(Instant.ofEpochMilli),
      editedAt = sessionComment.editedAt.map(Instant.ofEpochMilli),
      replies =
        sessionComment.replies.map(reply => CommentReply(reply.author, Instant.ofEpochMilli(reply.at), reply.text)),
      resolved = sessionComment.resolved
    )

object SessionNote:

  private val ChapterKind = "chapter"
  private val KeywordKind = "keyword"

  def order(note: SessionNote): (String, String, Int) = (note.kind, note.title, note.occurrence)

  def fromEntry(entry: (NoteKey, Notes)): SessionNote =
    val (key, notes) = entry
    key match
      case NoteKey.Chapter(heading) =>
        SessionNote(ChapterKind, heading.title, heading.occurrence, notes.overview.value, notes.extra.map(_.value))
      case NoteKey.Keyword(term) =>
        SessionNote(KeywordKind, term, 0, notes.overview.value, notes.extra.map(_.value))

  /** `None` for a kind this version does not know, so a session written by a newer version still restores. */
  def toEntry(note: SessionNote): Option[(NoteKey, Notes)] =
    val notes = Notes(BufferId(note.overview), note.extra.map(BufferId.apply))
    note.kind match
      case ChapterKind => Some(NoteKey.Chapter(HeadingIdentity(note.title, note.occurrence)) -> notes)
      case KeywordKind => Some(NoteKey.Keyword(note.title) -> notes)
      case _           => None

object SessionPlaceholder:

  def fromPlaceholder(placeholder: Placeholder): SessionPlaceholder =
    SessionPlaceholder(
      position = SessionCursorPosition.fromCursorPosition(placeholder.position),
      note = placeholder.note
    )

  def toPlaceholder(sessionPlaceholder: SessionPlaceholder): Placeholder =
    Placeholder(
      position = SessionCursorPosition.toCursorPosition(sessionPlaceholder.position),
      note = sessionPlaceholder.note
    )

object SessionDarling:

  def fromDarling(darling: Darling): SessionDarling =
    SessionDarling(
      text = darling.text,
      originalPosition = SessionCursorPosition.fromCursorPosition(darling.originalPosition)
    )

  def toDarling(sessionDarling: SessionDarling): Darling =
    Darling(
      text = sessionDarling.text,
      originalPosition = SessionCursorPosition.toCursorPosition(sessionDarling.originalPosition)
    )
