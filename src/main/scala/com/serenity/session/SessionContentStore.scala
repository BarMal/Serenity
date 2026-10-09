package com.serenity.session

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.HexFormat

import scala.jdk.CollectionConverters.*

import cats.effect.{IO, Ref}
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
  * A save costs in proportion to what changed, not to what is open: the digest of each buffer's text is remembered
  * against the rope and content version it was computed from, so a buffer untouched since the last save is neither
  * collected, encoded nor hashed again, and a directory that the last prune already left holding exactly the files
  * wanted is not listed again.
  *
  * `directoryFor` resolves a session file name to its content directory, or `None` where that is not a safe path.
  */
final private[session] class SessionContentStore(directoryFor: String => Option[Path], logger: Logger[IO]):

  // Only the open buffers of the latest save, so it cannot outgrow them.
  private val digests: Ref[IO, Map[Int, SessionContentStore.Digested]] = Ref.unsafe(Map.empty)

  // The files each session's content directory held after its last prune; dropped as soon as a file is added.
  private val prunedTo: Ref[IO, Map[String, Set[String]]] = Ref.unsafe(Map.empty)

  /** The snapshot's state with each buffer's text moved into a content file and replaced by the file's name. */
  def externalise(sessionFileName: String, snapshot: SessionSnapshot): IO[SessionState] =
    for
      known <- digests.get
      staged <- snapshot.state.buffers.traverse(buffer =>
        snapshot.unsavedText.get(buffer.id).traverse(stage(sessionFileName, known.get(buffer.id), _)).map(buffer -> _)
      )
      _ <- digests.set(staged.flatMap((buffer, digested) => digested.map(buffer.id -> _)).toMap)
    yield snapshot.state.copy(buffers = staged.map {
      case (buffer, Some(digested)) => buffer.copy(contentRef = Some(digested.ref))
      case (buffer, None)           => buffer
    })

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

  /** Removes the session's content files that `state` no longer names, unless the last prune already left exactly
    * those.
    */
  def prune(sessionFileName: String, state: SessionState): IO[Unit] =
    val kept = state.buffers.flatMap(_.contentRef).map(fileName).toSet
    prunedTo.get.flatMap { pruned =>
      if pruned.get(sessionFileName).contains(kept) then IO.unit
      else
        removeExcept(sessionFileName, kept)
          .flatTap(_ => prunedTo.update(_.updated(sessionFileName, kept)))
          .handleErrorWith(error => logger.warn(error)(s"[SESSION] Failed to prune content files of $sessionFileName"))
    }

  /** Removes all of the session's content files, for a deleted session. */
  def delete(sessionFileName: String): IO[Unit] =
    prunedTo.update(_ - sessionFileName) >>
      removeExcept(sessionFileName, Set.empty).handleErrorWith(error =>
        logger.warn(error)(s"[SESSION] Failed to prune content files of $sessionFileName")
      ) >> withDirectory(sessionFileName)(directory =>
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
    )

  // A remembered digest still names the right file only while the rope it was taken from is the one being saved, so
  // the content version is checked beside identity: neither alone is trusted to survive every edit path.
  private def stage(
    sessionFileName: String,
    known: Option[SessionContentStore.Digested],
    unsaved: UnsavedText
  ): IO[SessionContentStore.Digested] =
    known.filter(_.describes(unsaved)) match
      case Some(digested) => ensureFile(sessionFileName, digested.ref, IO.blocking(encode(unsaved))).as(digested)
      case None =>
        IO.blocking {
          val bytes = encode(unsaved)
          bytes -> SessionContentStore.digest(bytes)
        }.flatMap((bytes, ref) =>
          ensureFile(sessionFileName, ref, IO.pure(bytes)).as(SessionContentStore.Digested(unsaved, ref))
        )

  private def encode(unsaved: UnsavedText): Array[Byte] =
    unsaved.content.collect().getBytes(StandardCharsets.UTF_8)

  private def ensureFile(sessionFileName: String, ref: String, bytes: IO[Array[Byte]]): IO[Unit] =
    withDirectory(sessionFileName)(directory =>
      val file = directory.resolve(fileName(ref))
      IO.blocking(Files.exists(file))
        .ifM(
          IO.unit,
          prunedTo.update(_ - sessionFileName) >> bytes.flatMap(AtomicFileWriter.writeBytes(file, _))
        )
    )

  private def read(sessionFileName: String, ref: String): IO[String] =
    withDirectory(sessionFileName)(directory =>
      IO.blocking(new String(Files.readAllBytes(directory.resolve(fileName(ref))), StandardCharsets.UTF_8))
    )

  private def withDirectory[A](sessionFileName: String)(use: Path => IO[A]): IO[A] =
    IO.blocking(directoryFor(sessionFileName)).flatMap {
      case Some(directory) => use(directory)
      case None            => IO.raiseError(new IllegalArgumentException(s"Unsafe session path: $sessionFileName"))
    }

  private def fileName(ref: String): String = SessionContentStore.contentFileName(ref)

  private def deleteQuietly(path: Path): IO[Unit] =
    IO.blocking(Files.deleteIfExists(path))
      .void
      .handleErrorWith(error => logger.warn(error)(s"[SESSION] Failed to delete content file $path"))

private[session] object SessionContentStore:

  /** The name of the directory holding a session file's content files, beside that file. */
  def directoryName(sessionFileName: String): String = s"${sessionFileName.stripSuffix(".json")}.content"

  def contentFileName(ref: String): String = s"$ref.txt"

  /** What a buffer's text hashed to, remembered with the text it was computed from. */
  final case class Digested(source: UnsavedText, ref: String):
    def describes(unsaved: UnsavedText): Boolean =
      source.version == unsaved.version && (source.content eq unsaved.content)

  def digest(bytes: Array[Byte]): String =
    HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes))
