package com.serenity.io

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import cats.effect.IO
import com.serenity.lsp.config.{FileExtension, LanguageId}
import com.serenity.richtext.{
  LossyRichTextOverwriteException,
  OdtDocumentCodec,
  RichTextDocument,
  RichTextFidelity,
  RtfDocumentCodec
}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.text.LineEnding

/** Failures `FileManager` raises for its own file-open/save workflow, distinct from [[LossyRichTextOverwriteException]]
  * (a richtext-package concern about a specific re-import losing content, not about format support in general).
  */
sealed abstract class FileManagerError(message: String) extends RuntimeException(message)

object FileManagerError:
  /** Raised by `saveBuffer(buffer)` when the buffer has never been saved to a path. */
  final case class NoFilePath() extends FileManagerError("Buffer has no file path - use Save As")

  /** Raised when opening a file whose format's `DocumentFormatCapabilities.canOpen` is false. */
  final case class UnsupportedForOpen(fileType: FileType)
      extends FileManagerError(s"Unsupported document format for open: ${fileType.displayName}")

  /** Raised when saving to a path whose format's `DocumentFormatCapabilities.canSave` is false. */
  final case class UnsupportedForSave(fileType: FileType)
      extends FileManagerError(s"Unsupported document format for save: ${fileType.displayName}")

  /** Raised when the on-disk file changed since the buffer's revision was captured (#1623) -- saving would silently
    * discard whatever changed it. Callers surface this as a reload/overwrite prompt rather than retrying the save.
    */
  final case class ExternalConflict(location: StorageLocation)
      extends FileManagerError(s"File changed on disk since it was opened: $location")

  /** Raised for any other [[DocumentStorageError]] the local storage provider reports (not found, access denied, etc.)
    * -- distinguishable from the format-decode failures the RTF/ODT/DOCX codecs raise directly.
    */
  final case class StorageFailure(error: DocumentStorageError)
      extends FileManagerError(s"Document storage error: $error")

class FileManager(using balance: Balance):

  private val storage: DocumentStorageProvider = LocalDocumentStorageProvider()

  def loadFile(path: Path, bufferId: BufferId): IO[Buffer] =
    val location = StorageLocation.Local(path)
    FileUtils.detectFileType(path) match
      case FileType.RichText =>
        openStored(location).flatMap { stored =>
          IO.blocking(RtfDocumentCodec.readBytes(stored.content))
            .map(document => bufferFromRichText(bufferId, path, document, revision = stored.revision))
        }
      case FileType.OpenDocumentText =>
        openStored(location).flatMap { stored =>
          IO.fromEither(OdtDocumentCodec.readBytesWithFidelity(stored.content))
            .map(imported =>
              bufferFromRichText(bufferId, path, imported.document, Some(imported.fidelity), stored.revision)
            )
        }
      case FileType.WordOpenXmlDocument =>
        openStored(location).flatMap { stored =>
          IO.fromEither(com.serenity.richtext.DocxDocumentCodec.readBytesWithFidelity(stored.content))
            .map(imported =>
              bufferFromRichText(bufferId, path, imported.document, Some(imported.fidelity), stored.revision)
            )
        }
      case _ =>
        ensureSupported(path, _.canOpen, FileManagerError.UnsupportedForOpen.apply) >>
          openStored(location).map { stored =>
            bufferFromContent(bufferId, path, new String(stored.content, StandardCharsets.UTF_8), stored.revision)
          }

  def saveBuffer(buffer: Buffer, path: Path): IO[Buffer] =
    val expectedRevision = expectedRevisionFor(buffer, path)
    preventLossyOverwrite(buffer, path) >> (FileUtils.detectFileType(path) match
      case FileType.Markdown =>
        for
          _ <- ensureSupported(path, _.canSave, FileManagerError.UnsupportedForSave.apply)
          content = contentForSave(buffer, markdownContentForSave(buffer)).getBytes(StandardCharsets.UTF_8)
          stored <- saveStored(path, content, expectedRevision)
        yield savedBuffer(buffer, path, None, stored.revision)
      case FileType.RichText =>
        val document = richTextDocumentForSave(buffer)
        saveStored(path, RtfDocumentCodec.writeBytes(document), expectedRevision)
          .map(stored => savedBuffer(buffer, path, Some(document), stored.revision))
      case FileType.OpenDocumentText =>
        val document = richTextDocumentForSave(buffer)
        saveStored(path, OdtDocumentCodec.writeBytes(document), expectedRevision)
          .map(stored => savedBuffer(buffer, path, Some(document), stored.revision))
      case FileType.WordOpenXmlDocument =>
        val document = richTextDocumentForSave(buffer)
        saveStored(path, com.serenity.richtext.DocxDocumentCodec.writeBytes(document), expectedRevision)
          .map(stored => savedBuffer(buffer, path, Some(document), stored.revision))
      case _ =>
        for
          _ <- ensureSupported(path, _.canSave, FileManagerError.UnsupportedForSave.apply)
          content = contentForSave(buffer, buffer.document.content.collect()).getBytes(StandardCharsets.UTF_8)
          stored <- saveStored(path, content, expectedRevision)
        yield savedBuffer(buffer, path, None, stored.revision))

  /** Save buffer to its existing file path */
  def saveBuffer(buffer: Buffer): IO[Buffer] =
    buffer.document.filePath match
      case Some(path) => saveBuffer(buffer, path)
      case None       => IO.raiseError(FileManagerError.NoFilePath())

  def listDirectory(directory: Path): IO[List[FileEntry]] = FileBrowser.listDirectory(directory)

  /** Re-reads `buffer`'s file from disk in place (#1623): reuses `loadFile`'s per-format decode so a reload sees
    * exactly what a fresh open would, but keeps this buffer's identity, cursor/selection, viewport, and annotations
    * untouched -- only `document`/`richText` are replaced, the same fields `saveBuffer` updates on a successful save.
    */
  def reloadBuffer(buffer: Buffer): IO[Buffer] =
    buffer.document.filePath match
      case None => IO.raiseError(FileManagerError.NoFilePath())
      case Some(path) =>
        loadFile(path, buffer.id).map(reloaded =>
          buffer.copy(document = reloaded.document, richText = reloaded.richText)
        )

  /** The on-disk revision of `path` right now, for a focus-in re-check against a buffer's captured `Document.revision`
    * (#1623) -- `None` for a file that no longer exists or otherwise can't be read, which a focus-in check treats as
    * nothing to compare against rather than a conflict.
    */
  def currentRevision(path: Path): IO[Option[DocumentRevision]] =
    storage.open(StorageLocation.Local(path)).map(_.toOption.flatMap(_.revision))

  /** Editor content is LF-only, because `Rope` normalised it on the way in. A file that arrived with CRLF is written
    * back with CRLF, so an ordinary save does not rewrite every line of it.
    */
  private def contentForSave(buffer: Buffer, normalizedContent: String): String =
    buffer.document.lineEnding.applyTo(normalizedContent)

  private def bufferFromContent(
    bufferId: BufferId,
    path: Path,
    content: String,
    revision: Option[DocumentRevision]
  ): Buffer =
    Buffer(
      id = bufferId,
      document = com.serenity.state.models.Document(
        content = com.serenity.rope.Rope(content),
        filePath = Some(path),
        language = languageFromPath(path),
        lineEnding = LineEnding.detect(content),
        revision = revision
      )
    )

  private def bufferFromRichText(
    bufferId: BufferId,
    path: Path,
    document: RichTextDocument,
    fidelity: Option[RichTextFidelity] = None,
    revision: Option[DocumentRevision]
  ): Buffer =
    val normalized = document.normalized
    Buffer(
      id = bufferId,
      document = com.serenity.state.models.Document(
        content = com.serenity.rope.Rope(normalized.plainText),
        filePath = Some(path),
        revision = revision
      ),
      richText = com.serenity.state.models.RichTextState(
        richTextDocument = Some(normalized),
        richTextFidelity = fidelity
      )
    )

  private def savedBuffer(
    buffer: Buffer,
    path: Path,
    richTextDocument: Option[RichTextDocument],
    revision: Option[DocumentRevision]
  ): Buffer =
    buffer.copy(
      document = buffer.document.copy(
        filePath = Some(path),
        isDirty = false,
        language = languageFromPath(path),
        revision = revision
      ),
      richText = buffer.richText.copy(
        richTextDocument = richTextDocument.map(_.normalized),
        richTextFidelity = None
      )
    )

  /** The buffer's captured revision is only a valid conflict check against `path` when `path` is the same file the
    * buffer was last opened from or saved to -- a Save As to a different (or brand new) path has nothing to compare
    * that revision against, so no conflict check applies there.
    */
  private def expectedRevisionFor(buffer: Buffer, path: Path): Option[DocumentRevision] =
    if buffer.document.filePath.contains(path) then buffer.document.revision else None

  private def openStored(location: StorageLocation): IO[StoredDocument] =
    storage.open(location).flatMap {
      case Right(stored) => IO.pure(stored)
      case Left(error)   => IO.raiseError(FileManagerError.StorageFailure(error))
    }

  private def saveStored(
    path: Path,
    content: Array[Byte],
    expectedRevision: Option[DocumentRevision]
  ): IO[StoredDocument] =
    storage.save(StorageLocation.Local(path), content, expectedRevision).flatMap {
      case Right(stored)                                 => IO.pure(stored)
      case Left(DocumentStorageError.Conflict(location)) => IO.raiseError(FileManagerError.ExternalConflict(location))
      case Left(error)                                   => IO.raiseError(FileManagerError.StorageFailure(error))
    }

  private def preventLossyOverwrite(buffer: Buffer, path: Path): IO[Unit] =
    val replacesImportedFile = buffer.document.filePath.contains(path)
    val isLossyImport        = buffer.richText.richTextFidelity.exists(!_.isLossless)
    IO.raiseWhen(replacesImportedFile && isLossyImport)(
      LossyRichTextOverwriteException(
        s"Saving $path would discard unsupported rich document content. Use Save As to write a new file."
      )
    )

  private def richTextDocumentForSave(buffer: Buffer): RichTextDocument =
    val text = buffer.document.content.collect()
    buffer.richText.richTextDocument
      .filter(_.matchesPlainText(text))
      .getOrElse(RichTextDocument.fromPlainText(text))

  private def markdownContentForSave(buffer: Buffer): String =
    richTextDocumentForSave(buffer).paragraphs
      .map { paragraph =>
        val prefix = paragraph.role match
          case com.serenity.richtext.ParagraphRole.Heading(level) => "#" * level.max(1).min(6) + " "
          case _                                                  => ""
        prefix + paragraph.runs.map(markdownRun).mkString
      }
      .mkString("\n")

  private def markdownRun(run: com.serenity.richtext.RichTextRun): String =
    val marks = run.style.marks
    val marked =
      if marks.contains(com.serenity.richtext.InlineMark.Bold) && marks.contains(
            com.serenity.richtext.InlineMark.Italic
          )
      then s"***${run.text}***"
      else if marks.contains(com.serenity.richtext.InlineMark.Bold) then s"**${run.text}**"
      else if marks.contains(com.serenity.richtext.InlineMark.Italic) then s"*${run.text}*"
      else run.text
    if marks.contains(com.serenity.richtext.InlineMark.Underline) then s"<u>$marked</u>" else marked

  private def languageFromPath(path: Path): Option[LanguageId] =
    Option(path.getFileName)
      .map(_.toString)
      .flatMap(n =>
        n.lastIndexOf('.') match
          case -1 => None
          case i  => Some(n.substring(i + 1))
      )
      .flatMap(FileExtension.languageIdFor)

  private def ensureSupported(
    path: Path,
    operationSupported: DocumentFormatCapabilities => Boolean,
    error: FileType => FileManagerError
  ): IO[Unit] =
    val fileType     = FileUtils.detectFileType(path)
    val capabilities = DocumentFormat.capabilities(fileType)
    IO.unlessA(operationSupported(capabilities))(IO.raiseError(error(fileType)))
