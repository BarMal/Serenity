package com.serenity.io

import java.nio.file.{Files, Path, Paths}

import cats.effect.{IO, Resource}

object FileUtils:

  def detectFileType(path: Path): FileType =
    FileType.fromPath(path)

  def isReadableFile(path: Path): Boolean =
    Files.exists(path) && Files.isRegularFile(path) && Files.isReadable(path)

  def isWritableFile(path: Path): Boolean =
    if Files.exists(path) then Files.isWritable(path)
    else Option(path.getParent).exists(Files.isWritable)

  def readFileContent(path: Path): IO[String] =
    for
      readable <- IO.blocking(isReadableFile(path))
      _        <- IO.unlessA(readable)(IO.raiseError(new RuntimeException(s"File not readable: $path")))
      content  <- IO.blocking(Files.readString(path))
    yield content

  def writeFileContent(path: Path, content: String): IO[Unit] =
    AtomicFileWriter.writeString(path, content)

  def getFileSize(path: Path): IO[Long] =
    IO.blocking {
      if Files.exists(path) then Files.size(path) else 0L
    }

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

  def getLastModified(path: Path): IO[Long] =
    IO.blocking {
      if Files.exists(path) then Files.getLastModifiedTime(path).toMillis
      else 0L
    }
