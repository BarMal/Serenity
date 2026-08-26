package com.serenity.io

import java.nio.file.{Files, Path, Paths}

import cats.effect.{IO, Resource}

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
    for
      readable <- IO.blocking(isReadableFile(path))
      _        <- IO.unlessA(readable)(IO.raiseError(new RuntimeException(s"File not readable: $path")))
      content  <- IO.blocking(Files.readString(path))
    yield content

  /** Write content to file */
  def writeFileContent(path: Path, content: String): IO[Unit] =
    AtomicFileWriter.writeString(path, content)

  /** Get file size in bytes */
  def getFileSize(path: Path): IO[Long] =
    IO.blocking {
      if Files.exists(path) then Files.size(path) else 0L
    }

  /** List files in directory */
  def listFiles(directory: Path): IO[List[Path]] =
    if !Files.exists(directory) || !Files.isDirectory(directory) then IO.pure(List.empty)
    else
      Resource
        .fromAutoCloseable(IO.blocking(Files.list(directory)))
        .use(stream =>
          IO.blocking {
            import scala.jdk.CollectionConverters.*
            stream.iterator().asScala.toList.sorted
          }
        )

  /** Get current working directory */
  def getCurrentDirectory: IO[Path] =
    IO.blocking(Paths.get(System.getProperty("user.dir")))

  /** Resolve path relative to current directory, expanding a leading `~` to the user's home directory first. */
  def resolvePath(pathString: String): IO[Path] =
    for
      currentDir <- getCurrentDirectory
      path <-
        if pathString == "~" || pathString.startsWith("~/") then
          IO.blocking(Paths.get(System.getProperty("user.home"))).map { home =>
            if pathString == "~" then home else home.resolve(pathString.stripPrefix("~/"))
          }
        else if pathString.startsWith("/") || pathString.contains(":") then IO.pure(Paths.get(pathString))
        else IO.pure(currentDir.resolve(pathString))
    yield path.normalize()

  /** Check if file has been modified since last read */
  def getLastModified(path: Path): IO[Long] =
    IO.blocking {
      if Files.exists(path) then Files.getLastModifiedTime(path).toMillis
      else 0L
    }
