package com.serenity.io

import java.nio.file.{Files, Path, Paths}

import cats.effect.IO

object FileUtils:

  /** Detect file type based on extension */
  def detectFileType(path: Path): FileType =
    FileType.fromPath(path)

  /** Check if file exists and is readable */
  def isReadableFile(path: Path): Boolean =
    Files.exists(path) && Files.isRegularFile(path) && Files.isReadable(path)

  /** Check if file is writable (exists and writable, or parent directory writable for new files) */
  def isWritableFile(path: Path): Boolean =
    if Files.exists(path) then Files.isWritable(path)
    else Option(path.getParent).exists(Files.isWritable)

  /** Read file content as string */
  def readFileContent(path: Path): IO[String] =
    IO.blocking {
      if !isReadableFile(path) then throw new RuntimeException(s"File not readable: $path")
      Files.readString(path)
    }

  /** Write content to file */
  def writeFileContent(path: Path, content: String): IO[Unit] =
    IO.blocking {
      // Create parent directories if they don't exist
      Option(path.getParent).foreach(Files.createDirectories(_))
      Files.writeString(path, content)
    }

  /** Get file size in bytes */
  def getFileSize(path: Path): IO[Long] =
    IO.blocking {
      if Files.exists(path) then Files.size(path) else 0L
    }

  /** List files in directory */
  def listFiles(directory: Path): IO[List[Path]] =
    IO.blocking {
      if !Files.exists(directory) || !Files.isDirectory(directory) then List.empty
      else
        import scala.jdk.CollectionConverters.*
        Files.list(directory).toList.asScala.toList.sorted
    }

  /** Get current working directory */
  def getCurrentDirectory: IO[Path] =
    IO.blocking(Paths.get(System.getProperty("user.dir")))

  /** Resolve path relative to current directory */
  def resolvePath(pathString: String): IO[Path] =
    for
      currentDir <- getCurrentDirectory
      path =
        if pathString.startsWith("/") || pathString.contains(":") then Paths.get(pathString) // Absolute path
        else currentDir.resolve(pathString)                                                  // Relative path
    yield path.normalize()

  /** Check if file has been modified since last read */
  def getLastModified(path: Path): IO[Long] =
    IO.blocking {
      if Files.exists(path) then Files.getLastModifiedTime(path).toMillis
      else 0L
    }
