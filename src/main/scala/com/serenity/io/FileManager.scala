package com.serenity.io

import java.nio.file.Path

import cats.effect.IO

import com.serenity.lsp.config.FileExtension
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}

class FileManager(using balance: Balance):
  private val fileBrowser = new FileBrowser()

  /** Load file into a new buffer */
  def loadFile(path: Path): IO[Buffer] =
    for
      content <- FileUtils.readFileContent(path)
      bufferId = BufferId(System.currentTimeMillis().toInt) // Temporary ID generation
    yield Buffer(
      id = bufferId,
      content = com.serenity.rope.Rope(content),
      filePath = Some(path),
      isDirty = false,
      language = Option(path.getFileName)
        .map(_.toString)
        .flatMap(n =>
          n.lastIndexOf('.') match
            case -1 => None;
            case i  => Some(n.substring(i + 1))
        )
        .flatMap(FileExtension.languageIdFor)
    )

  /** Save buffer to file */
  def saveBuffer(buffer: Buffer, path: Path): IO[Buffer] =
    for _ <- FileUtils.writeFileContent(path, buffer.content.collect())
    yield buffer.copy(
      filePath = Some(path),
      isDirty = false,
      language = Option(path.getFileName)
        .map(_.toString)
        .flatMap(n =>
          n.lastIndexOf('.') match
            case -1 => None;
            case i  => Some(n.substring(i + 1))
        )
        .flatMap(FileExtension.languageIdFor)
    )

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
  def createNewBuffer: IO[Buffer] =
    val bufferId = BufferId(System.currentTimeMillis().toInt)
    IO.pure(
      Buffer(
        id = bufferId,
        content = com.serenity.rope.Rope.empty,
        filePath = None,
        isDirty = false,
        language = None
      )
    )

  def getRecentFiles: IO[List[Path]] =
    IO.pure(List.empty)

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

case class FileInfo(
    path: Path,
    size: Long,
    lastModified: Long,
    fileType: FileType
)
