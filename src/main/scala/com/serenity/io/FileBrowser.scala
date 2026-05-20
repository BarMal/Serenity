package com.serenity.io

import cats.effect.IO
import cats.syntax.traverse.*
import java.nio.file.{Path, Paths}

case class FileEntry(
  path: Path,
  name: String,
  isDirectory: Boolean,
  fileType: Option[FileType],
  size: Long
)

class FileBrowser:
  private var currentDirectory: Path = Paths.get(System.getProperty("user.dir"))

  /** Get current directory */
  def getCurrentDirectory: IO[Path] = IO.pure(currentDirectory)

  /** Change to specified directory */
  def changeDirectory(path: Path): IO[Unit] = 
    IO.blocking {
      if java.nio.file.Files.exists(path) && java.nio.file.Files.isDirectory(path) then
        currentDirectory = path.normalize()
      else
        throw new RuntimeException(s"Directory does not exist: $path")
    }

  /** List files and directories in current directory */
  def listCurrentDirectory: IO[List[FileEntry]] =
    listDirectory(currentDirectory)

  /** List files and directories in specified directory */
  def listDirectory(directory: Path): IO[List[FileEntry]] =
    for
      files <- FileUtils.listFiles(directory)
      entries <- files.traverse(createFileEntry)
    yield entries.sortBy(entry => (!entry.isDirectory, entry.name.toLowerCase))

  /** Go up one directory level */
  def goUp: IO[Unit] =
    IO.blocking {
      val parent = Option(currentDirectory.getParent)
      parent match
        case Some(parentPath) => currentDirectory = parentPath
        case None => // Already at root, do nothing
    }

  /** Go to home directory */
  def goHome: IO[Unit] =
    changeDirectory(Paths.get(System.getProperty("user.home")))

  /** Search for files matching pattern in current directory */
  def searchFiles(pattern: String): IO[List[FileEntry]] =
    for
      entries <- listCurrentDirectory
      filtered = entries.filter(_.name.toLowerCase.contains(pattern.toLowerCase))
    yield filtered

  private def createFileEntry(path: Path): IO[FileEntry] =
    for
      isDir <- IO.blocking(java.nio.file.Files.isDirectory(path))
      size <- if isDir then IO.pure(0L) else FileUtils.getFileSize(path)
      fileType = if isDir then None else Some(FileUtils.detectFileType(path))
    yield FileEntry(
      path = path,
      name = path.getFileName.toString,
      isDirectory = isDir,
      fileType = fileType,
      size = size
    )

extension (files: List[FileEntry])
  /** Filter to show only files (no directories) */
  def filesOnly: List[FileEntry] = files.filterNot(_.isDirectory)
  
  /** Filter to show only directories */
  def directoriesOnly: List[FileEntry] = files.filter(_.isDirectory)
  
  /** Filter by file type */
  def byFileType(fileType: FileType): List[FileEntry] = 
    files.filter(_.fileType.contains(fileType))