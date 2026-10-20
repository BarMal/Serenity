package com.serenity.richtext

import java.io.ByteArrayInputStream
import java.nio.file.{Files, Path}
import java.util.zip.ZipInputStream

import scala.annotation.tailrec
import scala.util.Using

import cats.effect.IO
import org.xml.sax.{ErrorHandler, SAXParseException}

/** Every entry name of a package, plus the bytes of the entries a decoder asked for. */
final case class ArchiveContents(entryNames: Set[String], entries: Map[String, Array[Byte]])

class RichTextCodecException(message: String, cause: Option[Throwable]) extends RuntimeException(message):
  cause.foreach(initCause)

  def this(message: String) = this(message, None)
  def this(message: String, cause: Throwable) = this(message, Some(cause))

object RichTextArchive:
  private[richtext] val MaxArchiveBytes: Long = 64L * 1024L * 1024L
  private[richtext] val MaxXmlEntryBytes: Int = 8 * 1024 * 1024

  /** The JAXP `ErrorHandler` contract reports a parse error by throwing; this is the only way to make the parser stop.
    * `RichTextXmlParser.parse` contains the exception and returns it as a `Left`.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  val SilentXmlErrorHandler: ErrorHandler = new ErrorHandler:
    override def warning(exception: SAXParseException): Unit =
      ()

    override def error(exception: SAXParseException): Unit =
      throw exception

    override def fatalError(exception: SAXParseException): Unit =
      throw exception

  def readFile(path: Path, format: String): IO[Array[Byte]] =
    IO.blocking(Files.size(path)).flatMap { size =>
      if size > MaxArchiveBytes then IO.raiseError(tooLarge(format, size))
      else IO.blocking(Files.readAllBytes(path))
    }

  def requireArchiveSize(bytes: Array[Byte], format: String): Either[RichTextCodecException, Unit] =
    Either.cond(bytes.length <= MaxArchiveBytes, (), tooLarge(format, bytes.length.toLong))

  def zipEntry(bytes: Array[Byte], name: String, format: String): Either[RichTextCodecException, Option[Array[Byte]]] =
    read(bytes, format, Set(name)).map(_.entries.get(name))

  /** Reads the package in one pass: all entry names, and the bytes of the `wanted` entries only. */
  def read(bytes: Array[Byte], format: String, wanted: Set[String]): Either[RichTextCodecException, ArchiveContents] =
    requireArchiveSize(bytes, format).flatMap { _ =>
      Using(ZipInputStream(ByteArrayInputStream(bytes)))(collect(_, format, wanted, Set.empty, Map.empty)).toEither.left
        .map(error => RichTextCodecException(s"$format archive could not be read", error))
        .flatten
    }

  @tailrec
  private def collect(
    input: ZipInputStream,
    format: String,
    wanted: Set[String],
    names: Set[String],
    entries: Map[String, Array[Byte]]
  ): Either[RichTextCodecException, ArchiveContents] =
    Option(input.getNextEntry) match
      case None => Right(ArchiveContents(names, entries))
      case Some(entry) =>
        val name = entry.getName
        if wanted.contains(name) then
          readBoundedEntry(input, name, format, MaxXmlEntryBytes) match
            case Left(error)  => Left(error)
            case Right(bytes) => collect(input, format, wanted, names + name, entries.updated(name, bytes))
        else collect(input, format, wanted, names + name, entries)

  private def readBoundedEntry(
    input: ZipInputStream,
    name: String,
    format: String,
    maxBytes: Int
  ): Either[RichTextCodecException, Array[Byte]] =
    val bytes = input.readNBytes(maxBytes + 1)
    Either.cond(
      bytes.length <= maxBytes,
      bytes,
      RichTextCodecException(s"$format entry $name is too large (limit $maxBytes bytes)")
    )

  private def tooLarge(format: String, size: Long): RichTextCodecException =
    RichTextCodecException(s"$format document is too large ($size bytes, limit $MaxArchiveBytes bytes)")
