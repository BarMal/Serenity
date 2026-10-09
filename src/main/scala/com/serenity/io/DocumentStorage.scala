package com.serenity.io

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.time.Instant

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files as Fs2Files, Path as Fs2Path}

/** What a stat says about a file without reading it: any write that lands changes at least one of these, except a
  * same-size rewrite within one timestamp tick (see [[FileStamp.vouchesForContent]]).
  */
final case class FileStamp(size: Long, modifiedNanos: Long, fileKey: Option[String])

object FileStamp:

  // A timestamp with no sub-millisecond part comes from a coarse filesystem (FAT, HFS+, ext3, many network mounts),
  // whose tick can hide a second same-size write; while the file is that young its stamp cannot stand in for its
  // content -- git's "racily clean" case, https://git-scm.com/docs/racy-git. Fine-grained filesystems keep the
  // stamp: their tick is a few milliseconds, the same risk Vim, Emacs and VS Code accept.
  //
  // That holds only while the tick is shorter than the stamp's age. Linux ext4/xfs before 6.13 stamp from a coarse
  // kernel clock ticking every 1-10 ms (CONFIG_HZ 100..1000) whose values look unrounded, so such a stamp must also
  // be older than FineWindowNanos at observation: 20 ms is twice the 10 ms worst tick, and a stamp younger than that
  // forces a re-read rather than being trusted as unchanged.
  private val CoarseTickNanos   = 1_000_000L
  private val CoarseWindowNanos = 2_000_000_000L
  private val FineWindowNanos   = 20_000_000L

  /** How a stat is taken; a seam so a test can model a filesystem with a coarse timestamp tick. */
  type Attributes = Path => BasicFileAttributes

  val filesystem: Attributes = Files.readAttributes(_, classOf[BasicFileAttributes])

  def of(attributes: BasicFileAttributes): FileStamp =
    val modified = attributes.lastModifiedTime.toInstant
    FileStamp(
      size = attributes.size,
      modifiedNanos = modified.getEpochSecond * 1_000_000_000L + modified.getNano,
      fileKey = Option(attributes.fileKey).map(_.toString)
    )

  /** `observedAtNanos` must be a clock reading taken BEFORE the stat that produced `stamp`. A reading taken after it
    * can fall past the end of the tick while the stat did not: a same-size write landing in that gap keeps the stamp
    * and is never noticed.
    */
  def vouchesForContent(stamp: FileStamp, observedAtNanos: Long): Boolean =
    val window = if stamp.modifiedNanos % CoarseTickNanos != 0L then FineWindowNanos else CoarseWindowNanos
    observedAtNanos - stamp.modifiedNanos >= window

  final case class Observed(stamp: FileStamp, vouches: Boolean)

  /** Reads the clock, then stats: the order [[vouchesForContent]] requires. */
  def observe(
    path: Path,
    clock: IO[FiniteDuration] = IO.realTime,
    attributes: Attributes = filesystem
  ): IO[Option[Observed]] =
    clock.flatMap(observedAt =>
      IO.blocking(read(path, attributes))
        .map(_.map(stamp => Observed(stamp, vouchesForContent(stamp, observedAt.toNanos))))
    )

  /** Blocking. `None` for a path that is not a regular file. */
  def read(path: Path, attributes: Attributes = filesystem): Option[FileStamp] =
    Option.when(Files.isRegularFile(path))(of(attributes(path)))

/** Identifies the revision of a document as reported by its storage provider. `value` is the content digest; `stamp` is
  * the file as it stood when that digest was taken, so a later identical stat confirms the revision without a read.
  */
final case class DocumentRevision(value: String, stamp: Option[FileStamp] = None):

  def sameContent(other: DocumentRevision): Boolean = value == other.value

  def vouchesFor(current: FileStamp): Boolean = stamp.contains(current)

/** Provider-neutral metadata for a document or directory entry. */
final case class DocumentMetadata(
    location: StorageLocation,
    displayName: String,
    size: Long,
    lastModified: Option[Instant],
    revision: Option[DocumentRevision]
)

/** A document read through a [[DocumentStorageProvider]]. Content is raw bytes, not `String`, so a provider can carry
  * binary formats (RTF/ODT/DOCX) as well as text -- callers that need text decode it themselves.
  */
final case class StoredDocument(content: Array[Byte], metadata: DocumentMetadata):
  def location: StorageLocation = metadata.location

  def revision: Option[DocumentRevision] = metadata.revision

/** Failures that providers can report without exposing service-specific SDK errors to editor code. */
enum DocumentStorageError:
  case UnsupportedLocation(location: StorageLocation)
  case NotFound(location: StorageLocation)
  case AccessDenied(location: StorageLocation)
  case AuthenticationFailed(providerId: String)
  case Offline(providerId: String)
  case Cancelled
  case Conflict(location: StorageLocation)
  case Failed(message: String)

/** A provider-neutral document-storage boundary.
  *
  * Providers own their authentication, provider identifiers, and network implementation. Callers use locations,
  * document metadata, and typed outcomes only.
  *
  * A cold capability (user-initiated, not a per-frame/per-glyph boundary) expressed as a record of functions rather
  * than a trait -- see #1017. A test double is a record literal, not a subclass; wrapping one in logging or retry is
  * `copy(open = ...)`.
  */
final case class DocumentStorageProvider(
    /** Stable provider identifier used in provider-neutral failures and configuration. */
    id: String,
    /** Whether this provider owns the supplied location. */
    supports: StorageLocation => Boolean,
    /** List direct children of a document directory. */
    list: StorageLocation => Stream[IO, Either[DocumentStorageError, DocumentMetadata]],
    /** Open a document and return the storage revision used for stale-save detection. */
    open: StorageLocation => IO[Either[DocumentStorageError, StoredDocument]],
    /** The document's current stamp, without reading its content. */
    stat: StorageLocation => IO[Either[DocumentStorageError, FileStamp]],
    /** Save document content, rejecting an out-of-date expected revision with [[DocumentStorageError.Conflict]]. */
    save: (StorageLocation, Array[Byte], Option[DocumentRevision]) => IO[Either[DocumentStorageError, StoredDocument]],
    /** Copy a document to another location handled by this provider. */
    copy: (StorageLocation, StorageLocation) => IO[Either[DocumentStorageError, StoredDocument]]
)

/** Local filesystem implementation of [[DocumentStorageProvider]].
  *
  * This adapter is intentionally independent from [[FileManager]] so its generic document contract does not alter
  * existing format-specific local open and save behavior.
  */
object LocalDocumentStorageProvider:

  /** How this adapter samples time and stats files: the order is load-bearing, see [[FileStamp.vouchesForContent]]. */
  final private case class Sampling(clock: IO[FiniteDuration], attributes: FileStamp.Attributes)

  def apply(
    probe: StorageIoProbe = StorageIoProbe.none,
    clock: IO[FiniteDuration] = IO.realTime,
    attributes: FileStamp.Attributes = FileStamp.filesystem
  ): DocumentStorageProvider =
    val sampling = Sampling(clock, attributes)
    DocumentStorageProvider(
      id = "local",
      supports = {
        case StorageLocation.Local(_)  => true
        case StorageLocation.Remote(_) => false
      },
      list = directory =>
        localPath(directory) match
          case Left(error) => Stream.emit(Left(error))
          case Right(path) => listLocal(path),
      open = location =>
        localPath(location) match
          case Left(error) => IO.pure(Left(error))
          case Right(path) => readLocal(path, location, probe, sampling),
      stat = location =>
        localPath(location) match
          case Left(error) => IO.pure(Left(error))
          case Right(path) => statLocal(path, location, sampling),
      save = (location, content, expectedRevision) =>
        localPath(location) match
          case Left(error) => IO.pure(Left(error))
          case Right(path) => saveLocal(path, location, content, expectedRevision, probe, sampling),
      copy = (source, destination) =>
        (localPath(source), localPath(destination)) match
          case (Left(error), _) => IO.pure(Left(error))
          case (_, Left(error)) => IO.pure(Left(error))
          case (Right(sourcePath), Right(destinationPath)) =>
            readLocal(sourcePath, source, probe, sampling).flatMap {
              case Left(error)     => IO.pure(Left(error))
              case Right(document) => saveLocal(destinationPath, destination, document.content, None, probe, sampling)
            }
    )

  private def localPath(location: StorageLocation): Either[DocumentStorageError, Path] =
    location match
      case StorageLocation.Local(path) => Right(path)
      case _                           => Left(DocumentStorageError.UnsupportedLocation(location))

  /** Streams directory entries incrementally via `fs2.io.file.Files[IO].list` (backed by a lazy
    * `java.nio.file.DirectoryStream`) rather than materializing the whole listing up front, so a large directory does
    * not have to load entirely into memory before the first entry reaches a consumer.
    */
  private def listLocal(directory: Path): Stream[IO, Either[DocumentStorageError, DocumentMetadata]] =
    Stream
      .eval(IO.blocking((Files.exists(directory), Files.isDirectory(directory))))
      .flatMap {
        case (false, _) =>
          Stream.emit(Left(DocumentStorageError.NotFound(StorageLocation.Local(directory))))
        case (_, false) =>
          Stream.emit(Left(DocumentStorageError.Failed(s"Not a directory: $directory")))
        case _ =>
          Fs2Files[IO]
            .list(Fs2Path.fromNioPath(directory))
            .evalMap(entry => IO.blocking(Right(metadata(entry.toNioPath, attributes(entry.toNioPath), None))))
      }
      .handleErrorWith(error => Stream.emit(Left(storageError(StorageLocation.Local(directory), error))))

  // The stat comes before the read: a write landing in between leaves a stamp older than the content, which costs one
  // later re-hash, whereas the other order would vouch for bytes that were never read.
  private def readLocal(
    path: Path,
    location: StorageLocation,
    probe: StorageIoProbe,
    sampling: Sampling
  ): IO[Either[DocumentStorageError, StoredDocument]] =
    sampling.clock
      .flatMap(observedAt =>
        IO.blocking[Either[DocumentStorageError, BasicFileAttributes]] {
          if !Files.exists(path) then Left(DocumentStorageError.NotFound(location))
          else if !Files.isRegularFile(path) || !Files.isReadable(path) then
            Left(DocumentStorageError.AccessDenied(location))
          else Right(sampling.attributes(path))
        }.flatMap(_.traverse { before =>
          for
            content  <- readContent(path, probe)
            revision <- revisionOf(content, before, observedAt.toNanos, probe)
          yield StoredDocument(content, metadata(path, before, Some(revision)))
        })
      )
      .handleError(error => Left(storageError(location, error)))

  private def statLocal(
    path: Path,
    location: StorageLocation,
    sampling: Sampling
  ): IO[Either[DocumentStorageError, FileStamp]] =
    IO.blocking(FileStamp.read(path, sampling.attributes))
      .map(_.toRight(DocumentStorageError.NotFound(location)))
      .handleError(error => Left(storageError(location, error)))

  private def saveLocal(
    path: Path,
    location: StorageLocation,
    content: Array[Byte],
    expectedRevision: Option[DocumentRevision],
    probe: StorageIoProbe,
    sampling: Sampling
  ): IO[Either[DocumentStorageError, StoredDocument]] =
    expectedRevision
      .fold(IO.pure(true))(onDiskMatches(path, _, probe, sampling))
      .flatMap[Either[DocumentStorageError, StoredDocument]] {
        case false => IO.pure(Left(DocumentStorageError.Conflict(location)))
        case true =>
          for
            _          <- AtomicFileWriter.writeBytes(path, content)
            observedAt <- sampling.clock
            written    <- IO.blocking(sampling.attributes(path))
            revision   <- revisionOf(content, written, observedAt.toNanos, probe)
          yield Right(StoredDocument(content, metadata(path, written, Some(revision))))
      }
      .handleError(error => Left(storageError(location, error)))

  /** A stat that `expected` vouches for settles it; only a file whose stat moved is read and hashed. */
  private def onDiskMatches(
    path: Path,
    expected: DocumentRevision,
    probe: StorageIoProbe,
    sampling: Sampling
  ): IO[Boolean] =
    IO.blocking(FileStamp.read(path, sampling.attributes)).flatMap {
      case None                                      => IO.pure(false)
      case Some(stamp) if expected.vouchesFor(stamp) => IO.pure(true)
      case Some(_) => readContent(path, probe).flatMap(digestOf(_, probe)).map(_ == expected.value)
    }

  private def readContent(path: Path, probe: StorageIoProbe): IO[Array[Byte]] =
    IO.blocking(Files.readAllBytes(path)).flatTap(content => probe.contentRead(content.length.toLong))

  private def digestOf(content: Array[Byte], probe: StorageIoProbe): IO[String] =
    IO(MessageDigest.getInstance("SHA-256").digest(content).map(byte => f"$byte%02x").mkString)
      .flatTap(_ => probe.hashed(content.length.toLong))

  /** The stamp is kept only when it can stand in for `content`: same size, and not too young to trust. */
  private def revisionOf(
    content: Array[Byte],
    stated: BasicFileAttributes,
    observedAtNanos: Long,
    probe: StorageIoProbe
  ): IO[DocumentRevision] =
    digestOf(content, probe).map { digest =>
      val stamp = FileStamp.of(stated)
      DocumentRevision(
        digest,
        Option.when(stamp.size == content.length.toLong && FileStamp.vouchesForContent(stamp, observedAtNanos))(stamp)
      )
    }

  private def attributes(path: Path): BasicFileAttributes =
    Files.readAttributes(path, classOf[BasicFileAttributes])

  private def metadata(
    path: Path,
    stated: BasicFileAttributes,
    revision: Option[DocumentRevision]
  ): DocumentMetadata =
    DocumentMetadata(
      location = StorageLocation.Local(path),
      displayName = Option(path.getFileName).fold(path.toString)(_.toString),
      size = stated.size,
      lastModified = Some(stated.lastModifiedTime.toInstant),
      revision = revision
    )

  private def storageError(location: StorageLocation, error: Throwable): DocumentStorageError =
    error match
      case _: java.nio.file.NoSuchFileException   => DocumentStorageError.NotFound(location)
      case _: java.nio.file.AccessDeniedException => DocumentStorageError.AccessDenied(location)
      case NonFatal(exception) =>
        DocumentStorageError.Failed(Option(exception.getMessage).getOrElse(exception.getClass.getSimpleName))

/** Counts the content bytes a local provider reads and hashes, so specs and measurements can hold it to a budget. */
final case class StorageIoProbe(contentRead: Long => IO[Unit], hashed: Long => IO[Unit])

object StorageIoProbe:
  val none: StorageIoProbe = StorageIoProbe(_ => IO.unit, _ => IO.unit)
