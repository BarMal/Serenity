package com.serenity.io

import java.nio.file.Path

import cats.effect.IO
import com.serenity.lsp.config.{FileExtension, LanguageId}
import com.serenity.richtext.{OdtDocumentCodec, RichTextDocument, RichTextFidelity, RtfDocumentCodec}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}

class FileManager(using balance: Balance):
  private val fileBrowser = new FileBrowser()

  /** Load file into a new buffer */
  def loadFile(path: Path, bufferId: BufferId): IO[Buffer] =
    FileUtils.detectFileType(path) match
      case FileType.RichText =>
        RtfDocumentCodec.read(path).map(document => bufferFromRichText(bufferId, path, document))
      case FileType.OpenDocumentText =>
        OdtDocumentCodec
          .readWithFidelity(path)
          .map(imported => bufferFromRichText(bufferId, path, imported.document, Some(imported.fidelity)))
      case FileType.WordOpenXmlDocument =>
        com.serenity.richtext.DocxDocumentCodec
          .readWithFidelity(path)
          .map(imported => bufferFromRichText(bufferId, path, imported.document, Some(imported.fidelity)))
      case _ =>
        ensureSupported(path, _.canOpen, "open") >>
          FileUtils.readFileContent(path).map(content => bufferFromContent(bufferId, path, content))

  /** Save buffer to file */
  def saveBuffer(buffer: Buffer, path: Path): IO[Buffer] =
    preventLossyOverwrite(buffer, path) >> (FileUtils.detectFileType(path) match
      case FileType.Markdown =>
        for
          _ <- ensureSupported(path, _.canSave, "save")
          _ <- FileUtils.writeFileContent(path, markdownContentForSave(buffer))
        yield savedBuffer(buffer, path, None)
      case FileType.RichText =>
        val document = richTextDocumentForSave(buffer)
        RtfDocumentCodec.write(document, path).as(savedBuffer(buffer, path, Some(document)))
      case FileType.OpenDocumentText =>
        val document = richTextDocumentForSave(buffer)
        OdtDocumentCodec.write(document, path).as(savedBuffer(buffer, path, Some(document)))
      case FileType.WordOpenXmlDocument =>
        val document = richTextDocumentForSave(buffer)
        com.serenity.richtext.DocxDocumentCodec.write(document, path).as(savedBuffer(buffer, path, Some(document)))
      case _ =>
        for
          _ <- ensureSupported(path, _.canSave, "save")
          _ <- FileUtils.writeFileContent(path, buffer.content.collect())
        yield savedBuffer(buffer, path, None))

  /** Save buffer to its existing file path */
  def saveBuffer(buffer: Buffer): IO[Buffer] =
    buffer.filePath match
      case Some(path) => saveBuffer(buffer, path)
      case None       => IO.raiseError(new RuntimeException("Buffer has no file path - use Save As"))

  /** Check if buffer has unsaved changes */
  def hasUnsavedChanges(buffer: Buffer): Boolean =
    buffer.isDirty

  /** Get file browser */
  def getFileBrowser: FileBrowser = fileBrowser

  /** Create a new empty buffer */
  def createNewBuffer(bufferId: BufferId): IO[Buffer] =
    IO.pure(
      Buffer(
        id = bufferId,
        content = com.serenity.rope.Rope.empty,
        filePath = None,
        isDirty = false,
        language = None
      )
    )

  /** Check if file exists */
  def fileExists(path: Path): IO[Boolean] =
    IO.blocking(FileUtils.isReadableFile(path))

  /** Get file info */
  def getFileInfo(path: Path): IO[Option[FileInfo]] =
    if FileUtils.isReadableFile(path) then
      for
        size         <- FileUtils.getFileSize(path)
        lastModified <- FileUtils.getLastModified(path)
        fileType = FileUtils.detectFileType(path)
      yield Some(FileInfo(path, size, lastModified, fileType))
    else IO.pure(None)

  private def bufferFromContent(bufferId: BufferId, path: Path, content: String): Buffer =
    Buffer(
      id = bufferId,
      content = com.serenity.rope.Rope(content),
      filePath = Some(path),
      isDirty = false,
      language = languageFromPath(path)
    )

  private def bufferFromRichText(
    bufferId: BufferId,
    path: Path,
    document: RichTextDocument,
    fidelity: Option[RichTextFidelity] = None
  ): Buffer =
    val normalized = document.normalized
    Buffer(
      id = bufferId,
      content = com.serenity.rope.Rope(normalized.plainText),
      filePath = Some(path),
      isDirty = false,
      language = None,
      richTextDocument = Some(normalized),
      richTextFidelity = fidelity
    )

  private def savedBuffer(buffer: Buffer, path: Path, richTextDocument: Option[RichTextDocument]): Buffer =
    buffer.copy(
      filePath = Some(path),
      isDirty = false,
      language = languageFromPath(path),
      richTextDocument = richTextDocument.map(_.normalized),
      richTextFidelity = None
    )

  private def preventLossyOverwrite(buffer: Buffer, path: Path): IO[Unit] =
    val replacesImportedFile = buffer.filePath.contains(path)
    val isLossyImport        = buffer.richTextFidelity.exists(!_.isLossless)
    IO.raiseWhen(replacesImportedFile && isLossyImport)(
      RuntimeException(
        s"Saving $path would discard unsupported rich document content. Use Save As to write a new file."
      )
    )

  private def richTextDocumentForSave(buffer: Buffer): RichTextDocument =
    val text = buffer.content.collect()
    buffer.richTextDocument
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
    operationName: String
  ): IO[Unit] =
    val fileType     = FileUtils.detectFileType(path)
    val capabilities = DocumentFormat.capabilities(fileType)
    IO.unlessA(operationSupported(capabilities))(
      IO.raiseError(new RuntimeException(s"Unsupported document format for $operationName: ${fileType.displayName}"))
    )

case class FileInfo(
    path: Path,
    size: Long,
    lastModified: Long,
    fileType: FileType
)
