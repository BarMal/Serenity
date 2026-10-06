package com.serenity.io

import java.nio.file.Path

import cats.effect.IO
import com.serenity.lsp.config.{FileExtension, LanguageId}
import com.serenity.richtext.{
  DocxDocumentCodec,
  FidelityReport,
  InlineAtom,
  LossyRichTextOverwriteException,
  OdtDocumentCodec,
  PackageFormat,
  RichTextDocument,
  RtfDocumentCodec
}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.text.{LineEnding, TextEncoding}

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

  /** Raised instead of opening a file whose bytes look binary (#1627): shown as text it would be garbage, and saved
    * back it would be destroyed.
    */
  final case class BinaryContent(path: Path) extends FileManagerError(s"Not a text file: $path")

  /** Raised when a reopen in a chosen encoding (#1627) meets bytes that encoding cannot represent, or a format that
    * stores formatting rather than text.
    */
  final case class NotReadableAs(path: Path, encoding: TextEncoding)
      extends FileManagerError(s"Can't read $path as ${encoding.configKey}")

class FileManager(storage: DocumentStorageProvider)(using balance: Balance):

  def this()(using balance: Balance) = this(LocalDocumentStorageProvider())

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
          IO.fromEither(DocxDocumentCodec.readBytesWithFidelity(stored.content))
            .map(imported =>
              bufferFromRichText(bufferId, path, imported.document, Some(imported.fidelity), stored.revision)
            )
        }
      case _ =>
        ensureSupported(path, _.canOpen, FileManagerError.UnsupportedForOpen.apply) >>
          openStored(location).flatMap { stored =>
            IO.fromOption(TextFileCodec.decode(stored.content))(FileManagerError.BinaryContent(path))
              .map(decoded => bufferFromContent(bufferId, path, decoded, stored.revision))
          }

  def saveBuffer(buffer: Buffer, path: Path): IO[Buffer] =
    val expectedRevision = expectedRevisionFor(buffer, path)
    preventLossyOverwrite(buffer, path) >> (FileUtils.detectFileType(path) match
      case FileType.Markdown =>
        for
          _ <- ensureSupported(path, _.canSave, FileManagerError.UnsupportedForSave.apply)
          encoded = encodedForSave(buffer, markdownContentForSave(buffer))
          stored <- saveStored(path, encoded.bytes, expectedRevision)
        yield savedAs(encoded, savedBuffer(buffer, path, None, stored.revision, withoutBlockLines(buffer)))
      case FileType.RichText =>
        val document           = richTextDocumentForSave(buffer)
        val (settled, content) = settledFor(document, None)
        saveStored(path, RtfDocumentCodec.writeBytes(document), expectedRevision)
          .map(stored => savedBuffer(buffer, path, Some(settled), stored.revision, content))
      case FileType.OpenDocumentText =>
        val document           = richTextDocumentForSave(buffer)
        val bytes              = OdtDocumentCodec.writeBytes(document)
        val (settled, content) = settledFor(document, Some(PackageFormat.Odt))
        saveStored(path, bytes, expectedRevision)
          .map(stored =>
            savedBuffer(
              buffer,
              path,
              Some(rebasedOnSaved(settled, bytes, PackageFormat.Odt)),
              stored.revision,
              content
            )
          )
      case FileType.WordOpenXmlDocument =>
        val document           = richTextDocumentForSave(buffer)
        val bytes              = DocxDocumentCodec.writeBytes(document)
        val (settled, content) = settledFor(document, Some(PackageFormat.Docx))
        saveStored(path, bytes, expectedRevision)
          .map(stored =>
            savedBuffer(
              buffer,
              path,
              Some(rebasedOnSaved(settled, bytes, PackageFormat.Docx)),
              stored.revision,
              content
            )
          )
      case _ =>
        for
          _ <- ensureSupported(path, _.canSave, FileManagerError.UnsupportedForSave.apply)
          encoded = encodedForSave(buffer, plainTextForSave(buffer))
          stored <- saveStored(path, encoded.bytes, expectedRevision)
        yield savedAs(encoded, savedBuffer(buffer, path, None, stored.revision, withoutBlockLines(buffer))))

  /** The document in the DOCX or ODT at `path` as it is now, when that file is still at `expected`, the revision a
    * document restored from a session was last read or written at. Only then do the body blocks a restored document
    * remembers still mean what they did, so any other file yields `None`.
    */
  def importPackage(path: Path, expected: Option[DocumentRevision]): IO[Option[RichTextDocument]] =
    openStored(StorageLocation.Local(path)).flatMap { stored =>
      val unchanged = stored.revision.zip(expected).exists((onDisk, recorded) => onDisk.sameContent(recorded))
      if !unchanged then IO.pure(None)
      else
        FileUtils.detectFileType(path) match
          case FileType.WordOpenXmlDocument =>
            IO.fromEither(DocxDocumentCodec.readBytes(stored.content)).map(Some(_))
          case FileType.OpenDocumentText =>
            IO.fromEither(OdtDocumentCodec.readBytes(stored.content)).map(Some(_))
          case _ => IO.pure(None)
    }

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
        loadFile(path, buffer.id).map(reloadedInto(buffer, _))

  /** [[reloadBuffer]], reading a plain-text file as `encoding` instead of detecting one (#1627). */
  def reloadBufferAs(buffer: Buffer, encoding: TextEncoding): IO[Buffer] =
    buffer.document.filePath match
      case None                                 => IO.raiseError(FileManagerError.NoFilePath())
      case Some(path) if storesFormatting(path) => IO.raiseError(FileManagerError.NotReadableAs(path, encoding))
      case Some(path) =>
        openStored(StorageLocation.Local(path)).flatMap { stored =>
          IO.fromOption(TextFileCodec.decodeAs(stored.content, encoding))(
            FileManagerError.NotReadableAs(path, encoding)
          ).map(decoded => reloadedInto(buffer, bufferFromContent(buffer.id, path, decoded, stored.revision)))
        }

  private def storesFormatting(path: Path): Boolean =
    FileUtils.detectFileType(path) match
      case FileType.RichText | FileType.OpenDocumentText | FileType.WordOpenXmlDocument | FileType.WordDocument => true
      case _                                                                                                    => false

  /** A fresh read starts at content version 0; carrying that over would move `buffer`'s version backwards (#1935). */
  private def reloadedInto(buffer: Buffer, reloaded: Buffer): Buffer =
    val document = reloaded.document.copy(contentVersion = buffer.document.contentVersion + 1)
    buffer.copy(
      document = document,
      richText = reloaded.richText.withSyncedDocument(reloaded.richText.richTextDocument, document.contentVersion)
    )

  /** The on-disk revision of `path` right now, for a focus-in re-check against a buffer's captured `Document.revision`
    * (#1623) -- `None` for a file that no longer exists or otherwise can't be read, which a focus-in check treats as
    * nothing to compare against rather than a conflict.
    */
  def currentRevision(path: Path): IO[Option[DocumentRevision]] =
    storage.open(StorageLocation.Local(path)).map(_.toOption.flatMap(_.revision))

  /** `known` itself while the file's stat still matches it, so an unchanged file costs a stat rather than a read;
    * otherwise [[currentRevision]].
    */
  def revisionSince(path: Path, known: Option[DocumentRevision]): IO[Option[DocumentRevision]] =
    storage.stat(StorageLocation.Local(path)).flatMap {
      case Right(stamp) if known.exists(_.vouchesFor(stamp)) => IO.pure(known)
      case Right(_)                                          => currentRevision(path)
      case Left(_)                                           => IO.none
    }

  /** Editor content is LF-only, because `Rope` normalised it on the way in. A file that arrived with CRLF is written
    * back with CRLF, and in the encoding and BOM it arrived with, so an ordinary save does not rewrite every line of
    * it.
    */
  private def encodedForSave(buffer: Buffer, normalizedContent: String): EncodedText =
    val document = buffer.document
    TextFileCodec.encodeOrFallBackToUtf8(
      document.lineEnding.applyTo(normalizedContent),
      document.encoding,
      document.hasBom
    )

  /** Records what the file was actually written as, which differs from what it was opened as when
    * [[TextFileCodec.encodeOrFallBackToUtf8]] had to fall back.
    */
  private def savedAs(encoded: EncodedText, saved: Buffer): Buffer =
    saved.copy(document = saved.document.copy(encoding = encoded.encoding, hasBom = encoded.hasBom))

  private def bufferFromContent(
    bufferId: BufferId,
    path: Path,
    decoded: DecodedText,
    revision: Option[DocumentRevision]
  ): Buffer =
    Buffer(
      id = bufferId,
      document = com.serenity.state.models.Document(
        content = com.serenity.rope.Rope(decoded.content),
        filePath = Some(path),
        language = languageFromPath(path),
        lineEnding = LineEnding.detect(decoded.content),
        encoding = decoded.encoding,
        hasBom = decoded.hasBom,
        revision = revision
      )
    )

  private def bufferFromRichText(
    bufferId: BufferId,
    path: Path,
    document: RichTextDocument,
    fidelity: Option[FidelityReport] = None,
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
      // `content` above is `Rope(normalized.plainText)`, so `normalized` is in sync with it by construction, at the
      // fresh `Document`'s default `contentVersion` of `0L` (#1663).
      richText = com.serenity.state.models
        .RichTextState()
        .withSyncedDocument(Some(normalized), contentVersion = 0L)
        .copy(richTextFidelity = fidelity)
    )

  private def savedBuffer(
    buffer: Buffer,
    path: Path,
    richTextDocument: Option[RichTextDocument],
    revision: Option[DocumentRevision],
    settledContent: Option[com.serenity.rope.Rope]
  ): Buffer =
    val base = settledContent.fold(buffer)(buffer.withSettledContent)
    base.copy(
      document = base.document.copy(
        filePath = Some(path),
        isDirty = false,
        language = languageFromPath(path),
        revision = revision
      ),
      // `richTextDocument` (when present) is `richTextDocumentForSave(buffer)`'s result, already proven to match
      // `buffer.document.content` -- or, when the save settled the text, that text -- so it is synced at `base`'s
      // current version (#1663).
      richText = base.richText
        .withSyncedDocument(richTextDocument.map(_.normalized), base.document.contentVersion)
        .copy(richTextFidelity = None)
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
    val dropped              = buffer.richText.richTextFidelity.toList.flatMap(_.wouldDrop)
    IO.raiseWhen(replacesImportedFile && dropped.nonEmpty)(
      LossyRichTextOverwriteException(
        s"Saving $path would drop ${FidelityReport.listed(dropped)}. Use Save As to write a new file."
      )
    )

  /** `document` linked to the package just written, which is what a later save copies from: reading it back is what
    * gives every paragraph the place it now has in the file. A document with another format's package keeps it.
    */
  private def rebasedOnSaved(
    document: RichTextDocument,
    written: Array[Byte],
    format: PackageFormat
  ): RichTextDocument =
    if document.source.exists(_.format != format) then document
    else
      val readBack = format match
        case PackageFormat.Docx => DocxDocumentCodec.readBytes(written)
        case PackageFormat.Odt  => OdtDocumentCodec.readBytes(written)
      readBack.fold(_ => document, document.rebasedOn)

  /** The buffer's text with each block line emptied, when it has any: Markdown and plain text write a block as an empty
    * line, and the buffer, which loses its rich document with the save, keeps no placeholder character that the file
    * does not have. An empty line rather than a marker such as "[table]": the file must say exactly what the buffer
    * shows, and a marker would be text the user never wrote.
    */
  private def withoutBlockLines(buffer: Buffer): Option[com.serenity.rope.Rope] =
    buffer.richText.richTextDocument
      .filter(_.hasOpaqueBlock)
      .map(_ => com.serenity.rope.Rope(buffer.document.content.collect().filterNot(_ == InlineAtom.BlockCharacter)))

  /** What the buffer holds after a save in `format`, which is what the file now says. A format that cannot hold blocks
    * wrote each block line as an empty paragraph, so the buffer's lines become empty too and a later save cannot differ
    * from what is shown. The text is returned when it changed.
    */
  private def settledFor(
    document: RichTextDocument,
    format: Option[PackageFormat]
  ): (RichTextDocument, Option[com.serenity.rope.Rope]) =
    if !document.hasOpaqueBlock || document.source.exists(source => format.contains(source.format)) then
      (document, None)
    else
      val withoutBlocks = document.withoutBlocks
      (withoutBlocks, Some(com.serenity.rope.Rope(withoutBlocks.plainText)))

  private def richTextDocumentForSave(buffer: Buffer): RichTextDocument =
    val text = buffer.document.content.collect()
    buffer.richText.richTextDocument
      .filter(_.matchesPlainText(text))
      .getOrElse(RichTextDocument.fromPlainText(text))

  /** The rope's text, except that a rich document's inline atoms (soft breaks) become the text they stand for. */
  private def plainTextForSave(buffer: Buffer): String =
    val text = buffer.document.content.collect()
    buffer.richText.richTextDocument.fold(text)(document =>
      if document.matchesPlainText(text) then document.exportText else text.filterNot(_ == InlineAtom.BlockCharacter)
    )

  private def markdownContentForSave(buffer: Buffer): String =
    richTextDocumentForSave(buffer).paragraphs
      .map { paragraph =>
        val prefix = paragraph.role match
          case com.serenity.richtext.ParagraphRole.Heading(level) => "#" * level.max(1).min(6) + " "
          case _                                                  => ""
        prefix + paragraph.runs.map(markdownRun).mkString
      }
      .mkString("\n")
      .filterNot(char => buffer.richText.richTextDocument.isDefined && char == InlineAtom.BlockCharacter)

  private def markdownRun(run: com.serenity.richtext.RichTextRun): String =
    run.atom match
      case Some(InlineAtom.Block(_, _)) => ""
      case Some(_)                      => MarkdownHardBreak
      case None                         => markdownMarkedText(run)

  private def markdownMarkedText(run: com.serenity.richtext.RichTextRun): String =
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

  private val MarkdownHardBreak = "\\\n"

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
