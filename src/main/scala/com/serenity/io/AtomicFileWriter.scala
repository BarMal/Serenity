package com.serenity.io

import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import scala.util.control.NonFatal
import scala.util.{Failure, Try}

import cats.effect.IO

/** Failure raised when a durable replacement of a target file cannot be completed. */
final class AtomicFileWriteException(val path: Path, cause: Throwable)
    extends RuntimeException(s"Failed to write $path atomically", cause)

/** JDK filesystem operations used by [[AtomicFileWriter]]. */
private[serenity] trait AtomicFileSystem:
  def createDirectories(path: Path): Path
  def createTempFile(directory: Path, prefix: String, suffix: String): Path
  def exists(path: Path): Boolean
  def copyAttributes(source: Path, target: Path): Path
  def write(path: Path, bytes: Array[Byte]): Path
  def moveAtomically(source: Path, target: Path): Path
  def moveReplacing(source: Path, target: Path): Path
  def deleteIfExists(path: Path): Boolean

/** Writes complete files through a temporary sibling before replacing the target. */
object AtomicFileWriter:

  private object JdkFileSystem extends AtomicFileSystem:
    def createDirectories(path: Path): Path = Files.createDirectories(path)

    def createTempFile(directory: Path, prefix: String, suffix: String): Path =
      Files.createTempFile(directory, prefix, suffix)

    def exists(path: Path): Boolean = Files.exists(path)

    def copyAttributes(source: Path, target: Path): Path =
      Files.copy(source, target, StandardCopyOption.COPY_ATTRIBUTES, StandardCopyOption.REPLACE_EXISTING)

    def write(path: Path, bytes: Array[Byte]): Path = Files.write(path, bytes)

    def moveAtomically(source: Path, target: Path): Path =
      Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

    def moveReplacing(source: Path, target: Path): Path =
      Files.move(source, target, StandardCopyOption.REPLACE_EXISTING)

    def deleteIfExists(path: Path): Boolean = Files.deleteIfExists(path)

  /** Atomically replace `path` with UTF-8 text, falling back when atomic moves are unsupported. */
  def writeString(path: Path, content: String): IO[Unit] =
    writeBytes(path, content.getBytes(StandardCharsets.UTF_8))

  /** Atomically replace `path` with bytes, falling back when atomic moves are unsupported. */
  def writeBytes(path: Path, bytes: Array[Byte]): IO[Unit] =
    IO.blocking(writeBytesBlocking(path, bytes))

  /** Atomically replace `path` with bytes from an existing blocking boundary. */
  def writeBytesBlocking(path: Path, bytes: Array[Byte]): Unit =
    writeBytesBlocking(path, bytes, JdkFileSystem)

  private[serenity] def writeString(path: Path, content: String, fileSystem: AtomicFileSystem): IO[Unit] =
    writeBytes(path, content.getBytes(StandardCharsets.UTF_8), fileSystem)

  private[serenity] def writeBytes(path: Path, bytes: Array[Byte], fileSystem: AtomicFileSystem): IO[Unit] =
    IO.blocking(writeBytesBlocking(path, bytes, fileSystem))

  private def writeBytesBlocking(path: Path, bytes: Array[Byte], fileSystem: AtomicFileSystem): Unit =
    val target    = path.toAbsolutePath.normalize
    val directory = Option(target.getParent).getOrElse(target)

    // This is the synchronous boundary both writeBytes (via IO.blocking, which converts a thrown
    // exception into a failed IO) and ConfigManager.saveConfig (plain try/catch) rely on -- so it must
    // keep raising on failure. Try#get raises for us instead of a literal `throw`, and Try already only
    // catches NonFatal, matching the two catch clauses this replaces.
    Try {
      val _         = fileSystem.createDirectories(directory)
      val prefix    = s".${target.getFileName.toString}."
      val temporary = fileSystem.createTempFile(directory, prefix, ".tmp")
      try
        if fileSystem.exists(target) then
          val _ = fileSystem.copyAttributes(target, temporary)
        val _ = fileSystem.write(temporary, bytes)
        try
          val _ = fileSystem.moveAtomically(temporary, target)
        catch
          case _: AtomicMoveNotSupportedException =>
            val _ = fileSystem.moveReplacing(temporary, target)
      finally deleteQuietly(temporary, fileSystem)
    }.recoverWith {
      case error: AtomicFileWriteException => Failure(error)
      case NonFatal(error)                 => Failure(AtomicFileWriteException(path, error))
    }.get

  private def deleteQuietly(path: Path, fileSystem: AtomicFileSystem): Unit =
    try fileSystem.deleteIfExists(path): Unit
    catch case NonFatal(_) => ()
