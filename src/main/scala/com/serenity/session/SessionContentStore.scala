package com.serenity.session

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.io.AtomicFileWriter
import org.typelevel.log4cats.Logger

/** Keeps each buffer's unsaved text in its own content-addressed file beside the session file, so a save rewrites only
  * the buffers whose text changed and the session file stays small metadata (#1912).
  *
  * A content file is named by the digest of its text and never changes once written, so writing it before the session
  * file that names it is safe to interrupt: the old session file still names the old files, which [[prune]] removes
  * only after the new session file is in place.
  *
  * `directoryFor` resolves a session file name to its content directory, or `None` where that is not a safe path.
  */
final private[session] class SessionContentStore(directoryFor: String => Option[Path], logger: Logger[IO]):

  /** `state` with each buffer's text moved into a content file and replaced by the file's name. */
  def externalise(sessionFileName: String, state: SessionState): IO[SessionState] =
    state.buffers
      .traverse(buffer =>
        buffer.unsavedContent.traverse(text => store(sessionFileName, text)).map {
          case Some(ref) => buffer.copy(unsavedContent = None, contentRef = Some(ref))
          case None      => buffer
        }
      )
      .map(buffers => state.copy(buffers = buffers))

  /** `state` with each named content file read back into its buffer. A missing or unreadable file leaves that buffer
    * without text, so the restore falls back to the file on disk rather than failing the whole session.
    */
  def internalise(sessionFileName: String, state: SessionState): IO[SessionState] =
    state.buffers
      .traverse(buffer =>
        buffer.contentRef.fold(IO.pure(buffer))(ref =>
          read(sessionFileName, ref).attempt.flatMap {
            case Right(text) => IO.pure(buffer.copy(unsavedContent = Some(text), contentRef = None))
            case Left(error) =>
              logger.warn(error)(s"[SESSION] Unsaved text $ref for buffer ${buffer.id} is unreadable") >>
                IO.pure(buffer.copy(contentRef = None))
          }
        )
      )
      .map(buffers => state.copy(buffers = buffers))

  /** Removes the session's content files that `state` no longer names. */
  def prune(sessionFileName: String, state: SessionState): IO[Unit] =
    removeExcept(sessionFileName, state.buffers.flatMap(_.contentRef).map(fileName).toSet)

  /** Removes all of the session's content files, for a deleted session. */
  def delete(sessionFileName: String): IO[Unit] =
    removeExcept(sessionFileName, Set.empty) >> withDirectory(sessionFileName)(directory =>
      IO.blocking(Files.deleteIfExists(directory)).void.handleErrorWith(_ => IO.unit)
    )

  private def removeExcept(sessionFileName: String, kept: Set[String]): IO[Unit] =
    withDirectory(sessionFileName)(directory =>
      IO.blocking {
        if Files.isDirectory(directory) then
          val listing = Files.list(directory)
          try listing.iterator().asScala.filterNot(path => kept.contains(path.getFileName.toString)).toList
          finally listing.close()
        else Nil
      }.flatMap(_.traverse_(deleteQuietly))
    ).handleErrorWith(error => logger.warn(error)(s"[SESSION] Failed to prune content files of $sessionFileName"))

  private def store(sessionFileName: String, text: String): IO[String] =
    val ref = SessionContentStore.digest(text)
    withDirectory(sessionFileName)(directory =>
      val file = directory.resolve(fileName(ref))
      IO.blocking(Files.exists(file)).ifM(IO.unit, AtomicFileWriter.writeString(file, text))
    ).as(ref)

  private def read(sessionFileName: String, ref: String): IO[String] =
    withDirectory(sessionFileName)(directory =>
      IO.blocking(new String(Files.readAllBytes(directory.resolve(fileName(ref))), StandardCharsets.UTF_8))
    )

  private def withDirectory[A](sessionFileName: String)(use: Path => IO[A]): IO[A] =
    IO.blocking(directoryFor(sessionFileName)).flatMap {
      case Some(directory) => use(directory)
      case None            => IO.raiseError(new IllegalArgumentException(s"Unsafe session path: $sessionFileName"))
    }

  private def fileName(ref: String): String = s"$ref.txt"

  private def deleteQuietly(path: Path): IO[Unit] =
    IO.blocking(Files.deleteIfExists(path))
      .void
      .handleErrorWith(error => logger.warn(error)(s"[SESSION] Failed to delete content file $path"))

private[session] object SessionContentStore:

  def digest(text: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(text.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"$byte%02x")
      .mkString
