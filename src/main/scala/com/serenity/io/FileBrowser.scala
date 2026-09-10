package com.serenity.io

import java.nio.file.Path

import cats.effect.IO
import cats.effect.syntax.all.*

final case class FileEntry(
    path: Path,
    name: String,
    isDirectory: Boolean,
    fileType: Option[FileType],
    size: Long
)

/** Stateless directory listing -- the only file-browsing workflow production code uses. */
object FileBrowser:

  /** Upper bound on in-flight `createFileEntry` calls per `listDirectory` (issue #1416): high enough to overlap a large
    * directory's blocking stat calls, low enough not to flood the filesystem/thread pool for very large ones.
    */
  private[io] val MaxConcurrentEntries = 32

  /** List files and directories in `directory`, directories first, then alphabetically by name. */
  def listDirectory(directory: Path): IO[List[FileEntry]] =
    for
      files   <- FileUtils.listFiles(directory)
      entries <- files.parTraverseN(MaxConcurrentEntries)(createFileEntry)
    yield entries.sortBy(entry => (!entry.isDirectory, entry.name.toLowerCase))

  private def createFileEntry(path: Path): IO[FileEntry] =
    for
      isDir <- IO.blocking(java.nio.file.Files.isDirectory(path))
      size  <- if isDir then IO.pure(0L) else FileUtils.getFileSize(path)
      fileType = if isDir then None else Some(FileUtils.detectFileType(path))
    yield FileEntry(
      path = path,
      name = path.getFileName.toString,
      isDirectory = isDir,
      fileType = fileType,
      size = size
    )
