package com.serenity.richtext

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Path}
import java.util.zip.ZipInputStream

import scala.annotation.tailrec
import scala.util.control.NonFatal

import org.xml.sax.{ErrorHandler, SAXParseException}

class RichTextCodecException(message: String, cause: Throwable | Null = null) extends RuntimeException(message, cause)

object RichTextArchive:
  private[richtext] val MaxArchiveBytes: Long = 64L * 1024L * 1024L
  private[richtext] val MaxXmlEntryBytes: Int = 8 * 1024 * 1024

  private val BufferSize = 8192

  val SilentXmlErrorHandler: ErrorHandler = new ErrorHandler:
    override def warning(exception: SAXParseException): Unit =
      ()

    override def error(exception: SAXParseException): Unit =
      throw exception

    override def fatalError(exception: SAXParseException): Unit =
      throw exception

  def readFile(path: Path, format: String): Array[Byte] =
    val size = Files.size(path)
    if size > MaxArchiveBytes then
      throw RichTextCodecException(s"$format document is too large ($size bytes, limit $MaxArchiveBytes bytes)")
    Files.readAllBytes(path)

  def requireArchiveSize(bytes: Array[Byte], format: String): Unit =
    if bytes.length > MaxArchiveBytes then
      throw RichTextCodecException(
        s"$format document is too large (${bytes.length} bytes, limit $MaxArchiveBytes bytes)"
      )

  def zipEntry(bytes: Array[Byte], name: String, format: String): Option[Array[Byte]] =
    requireArchiveSize(bytes, format)
    val input = ZipInputStream(ByteArrayInputStream(bytes))
    try
      Iterator
        .continually(input.getNextEntry)
        .takeWhile(_ != null)
        .find(_.getName == name)
        .map(_ => readBoundedEntry(input, name, format, MaxXmlEntryBytes))
    catch
      case error: RichTextCodecException => throw error
      case NonFatal(error)               => throw RichTextCodecException(s"$format archive could not be read", error)
    finally input.close()

  def entryNames(bytes: Array[Byte], format: String): Set[String] =
    requireArchiveSize(bytes, format)
    val input = ZipInputStream(ByteArrayInputStream(bytes))
    try
      Iterator
        .continually(input.getNextEntry)
        .takeWhile(_ != null)
        .map(_.getName)
        .toSet
    catch case NonFatal(error) => throw RichTextCodecException(s"$format archive could not be read", error)
    finally input.close()

  private def readBoundedEntry(
    input: ZipInputStream,
    name: String,
    format: String,
    maxBytes: Int
  ): Array[Byte] =
    val output = ByteArrayOutputStream()
    val buffer = Array.ofDim[Byte](BufferSize)

    @tailrec
    def loop(totalBytes: Int): Unit =
      val read = input.read(buffer)
      if read >= 0 then
        val nextTotal = totalBytes + read
        if nextTotal > maxBytes then
          throw RichTextCodecException(s"$format entry $name is too large (limit $maxBytes bytes)")
        output.write(buffer, 0, read)
        loop(nextTotal)

    loop(0)
    output.toByteArray
