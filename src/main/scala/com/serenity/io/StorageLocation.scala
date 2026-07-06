package com.serenity.io

import java.net.URI
import java.nio.file.{Path, Paths}

import scala.util.control.NonFatal

/** A parsed document storage target.
  *
  * This model identifies local paths separately from remote URIs without claiming that every recognized storage
  * location can already be opened or saved by the current file IO.
  */
sealed trait StorageLocation:

  /** Whether this location is backed by a non-local URI scheme. */
  def isRemote: Boolean

  /** Whether the current file IO can open this location. */
  def canOpenWithCurrentStorage: Boolean

  /** Whether the current file IO can save this location. */
  def canSaveWithCurrentStorage: Boolean

object StorageLocation:

  /** A storage location backed by the local filesystem. */
  final case class Local(path: Path) extends StorageLocation:
    override val isRemote: Boolean                  = false
    override val canOpenWithCurrentStorage: Boolean = true
    override val canSaveWithCurrentStorage: Boolean = true

  /** A storage location backed by a remote URI scheme not yet handled by file IO. */
  final case class Remote(uri: URI) extends StorageLocation:
    override val isRemote: Boolean                  = true
    override val canOpenWithCurrentStorage: Boolean = false
    override val canSaveWithCurrentStorage: Boolean = false

  /** Create a supported local storage location from an already-resolved path. */
  def fromPath(path: Path): StorageLocation =
    Local(path)

  /** Parse a user-entered storage location as either a local path, file URI, or remote URI. */
  def parse(value: String): Either[String, StorageLocation] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left("Storage location is empty")
    else if hasUriScheme(trimmed) && !isWindowsAbsolutePath(trimmed) then parseUri(trimmed)
    else Right(Local(Path.of(trimmed)))

  private def parseUri(value: String): Either[String, StorageLocation] =
    catchMessage("Invalid storage URI")(URI.create(value)).flatMap { uri =>
      Option(uri.getScheme).map(_.toLowerCase) match
        case Some("file") =>
          catchMessage("Invalid file URI")(Paths.get(uri)).map(Local.apply)
        case Some(_) =>
          Right(Remote(uri))
        case None =>
          Right(Local(Path.of(value)))
    }

  private def catchMessage[A](prefix: String)(value: => A): Either[String, A] =
    try Right(value)
    catch case NonFatal(error) => Left(s"$prefix: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")

  private def hasUriScheme(value: String): Boolean =
    value.indexOf(':') match
      case -1 => false
      case 0  => false
      case index =>
        val scheme = value.substring(0, index)
        scheme.headOption.exists(_.isLetter) &&
        scheme.tail.forall(ch => ch.isLetterOrDigit || ch == '+' || ch == '.' || ch == '-')

  private def isWindowsAbsolutePath(value: String): Boolean =
    value.length >= 3 &&
      value.charAt(0).isLetter &&
      value.charAt(1) == ':' &&
      (value.charAt(2) == '\\' || value.charAt(2) == '/')
