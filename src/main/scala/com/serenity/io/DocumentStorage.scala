package com.serenity.io

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.time.Instant

import scala.util.control.NonFatal

import cats.effect.IO
import fs2.Stream

/** Identifies the revision of a document as reported by its storage provider. */
final case class DocumentRevision(value: String)

/** Provider-neutral metadata for a document or directory entry. */
final case class DocumentMetadata(
    location: StorageLocation,
    displayName: String,
    size: Long,
    lastModified: Option[Instant],
    revision: Option[DocumentRevision]
)

/** A document read through a [[DocumentStorageProvider]]. */
final case class StoredDocument(content: String, metadata: DocumentMetadata):
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
  */
trait DocumentStorageProvider:

  /** Stable provider identifier used in provider-neutral failures and configuration. */
  def id: String

  /** Whether this provider owns the supplied location. */
  def supports(location: StorageLocation): Boolean

  /** List direct children of a document directory. */
  def list(directory: StorageLocation): Stream[IO, Either[DocumentStorageError, DocumentMetadata]]

  /** Open a document and return the storage revision used for stale-save detection. */
  def open(location: StorageLocation): IO[Either[DocumentStorageError, StoredDocument]]

  /** Save document content, rejecting an out-of-date expected revision with [[DocumentStorageError.Conflict]]. */
  def save(
    location: StorageLocation,
    content: String,
    expectedRevision: Option[DocumentRevision]
  ): IO[Either[DocumentStorageError, StoredDocument]]

  /** Copy a document to another location handled by this provider. */
  def copy(
    source: StorageLocation,
    destination: StorageLocation
  ): IO[Either[DocumentStorageError, StoredDocument]]

/** Local filesystem implementation of [[DocumentStorageProvider]].
  *
  * This adapter is intentionally independent from [[FileManager]] so its generic document contract does not alter
  * existing format-specific local open and save behavior.
  */
final class LocalDocumentStorageProvider extends DocumentStorageProvider:

  override val id: String = "local"

  override def supports(location: StorageLocation): Boolean =
    location match
      case StorageLocation.Local(_)  => true
      case StorageLocation.Remote(_) => false

  override def list(directory: StorageLocation): Stream[IO, Either[DocumentStorageError, DocumentMetadata]] =
    localPath(directory) match
      case Left(error) => Stream.emit(Left(error))
      case Right(path) =>
        Stream.eval(listLocal(path)).flatMap(Stream.emits)

  override def open(location: StorageLocation): IO[Either[DocumentStorageError, StoredDocument]] =
    localPath(location) match
      case Left(error) => IO.pure(Left(error))
      case Right(path) => readLocal(path, location)

  override def save(
    location: StorageLocation,
    content: String,
    expectedRevision: Option[DocumentRevision]
  ): IO[Either[DocumentStorageError, StoredDocument]] =
    localPath(location) match
      case Left(error) => IO.pure(Left(error))
      case Right(path) => saveLocal(path, location, content, expectedRevision)

  override def copy(
    source: StorageLocation,
    destination: StorageLocation
  ): IO[Either[DocumentStorageError, StoredDocument]] =
    (localPath(source), localPath(destination)) match
      case (Left(error), _) => IO.pure(Left(error))
      case (_, Left(error)) => IO.pure(Left(error))
      case (Right(sourcePath), Right(destinationPath)) =>
        readLocal(sourcePath, source).flatMap {
          case Left(error)     => IO.pure(Left(error))
          case Right(document) => saveLocal(destinationPath, destination, document.content, None)
        }

  private def localPath(location: StorageLocation): Either[DocumentStorageError, Path] =
    location match
      case StorageLocation.Local(path) => Right(path)
      case _                           => Left(DocumentStorageError.UnsupportedLocation(location))

  private def listLocal(directory: Path): IO[List[Either[DocumentStorageError, DocumentMetadata]]] =
    IO.blocking((Files.exists(directory), Files.isDirectory(directory)))
      .flatMap {
        case (false, _) =>
          IO.pure(List(Left(DocumentStorageError.NotFound(StorageLocation.Local(directory)))))
        case (_, false) =>
          IO.pure(List(Left(DocumentStorageError.Failed(s"Not a directory: $directory"))))
        case _ =>
          FileUtils
            .listFiles(directory)
            .flatMap(paths => IO.blocking(paths.map(path => Right(metadata(path, None)))))
      }
      .handleError(error => List(Left(storageError(StorageLocation.Local(directory), error))))

  private def readLocal(path: Path, location: StorageLocation): IO[Either[DocumentStorageError, StoredDocument]] =
    IO.blocking {
      if !Files.exists(path) then Left(DocumentStorageError.NotFound(location))
      else if !Files.isRegularFile(path) || !Files.isReadable(path) then
        Left(DocumentStorageError.AccessDenied(location))
      else
        val content = Files.readString(path)
        Right(StoredDocument(content, metadata(path, Some(content))))
    }.handleError(error => Left(storageError(location, error)))

  private def saveLocal(
    path: Path,
    location: StorageLocation,
    content: String,
    expectedRevision: Option[DocumentRevision]
  ): IO[Either[DocumentStorageError, StoredDocument]] =
    IO.blocking {
      val currentRevision =
        if Files.exists(path) && Files.isRegularFile(path) then Some(revision(Files.readString(path))) else None
      if expectedRevision.exists(expected => !currentRevision.contains(expected)) then
        Left(DocumentStorageError.Conflict(location))
      else Right(())
    }.flatMap {
      case Left(conflict) => IO.pure(Left(conflict))
      case Right(_) =>
        AtomicFileWriter
          .writeString(path, content)
          .flatMap(_ => IO.blocking(Right(StoredDocument(content, metadata(path, Some(content))))))
    }.handleError(error => Left(storageError(location, error)))

  private def metadata(path: Path, content: Option[String]): DocumentMetadata =
    DocumentMetadata(
      location = StorageLocation.Local(path),
      displayName = Option(path.getFileName).fold(path.toString)(_.toString),
      size = Files.size(path),
      lastModified = Some(Files.getLastModifiedTime(path).toInstant),
      revision = content.map(revision)
    )

  private def revision(content: String): DocumentRevision =
    val digest = MessageDigest.getInstance("SHA-256").digest(content.getBytes(StandardCharsets.UTF_8))
    DocumentRevision(digest.map(byte => f"$byte%02x").mkString)

  private def storageError(location: StorageLocation, error: Throwable): DocumentStorageError =
    error match
      case _: java.nio.file.NoSuchFileException   => DocumentStorageError.NotFound(location)
      case _: java.nio.file.AccessDeniedException => DocumentStorageError.AccessDenied(location)
      case NonFatal(exception) =>
        DocumentStorageError.Failed(Option(exception.getMessage).getOrElse(exception.getClass.getSimpleName))
