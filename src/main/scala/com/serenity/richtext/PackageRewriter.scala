package com.serenity.richtext

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.util.zip.{CRC32, ZipEntry, ZipInputStream, ZipOutputStream}

import scala.annotation.tailrec
import scala.util.control.NonFatal

/** Writes a package that is the source archive with some parts replaced and some added (when absent). Every other entry
  * is copied unchanged, in the original order, with its original compression method and timestamp, so a save touches
  * only the parts the model owns.
  */
private[richtext] object PackageRewriter:
  private val MaxExpandedBytes: Long = RichTextArchive.MaxArchiveBytes * 8L

  def rewrite(
    archive: Array[Byte],
    format: String,
    replacements: Map[String, Array[Byte]],
    additions: List[(String, Array[Byte])]
  ): Array[Byte] =
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output, StandardCharsets.UTF_8)
    val input  = ZipInputStream(ByteArrayInputStream(archive))
    try
      val existing = copyEntries(input, zip, replacements, format, 0L, Set.empty)
      additions
        .filterNot((name, _) => existing.contains(name))
        .foreach((name, bytes) => writeEntry(zip, ZipEntry(name), bytes))
      zip.finish()
      output.toByteArray
    catch
      case error: RichTextCodecException => throw error
      case NonFatal(error) => throw RichTextCodecException(s"$format archive could not be rewritten", error)
    finally
      input.close()
      zip.close()

  @tailrec
  private def copyEntries(
    input: ZipInputStream,
    zip: ZipOutputStream,
    replacements: Map[String, Array[Byte]],
    format: String,
    copied: Long,
    names: Set[String]
  ): Set[String] =
    Option(input.getNextEntry) match
      case None => names
      case Some(entry) =>
        val bytes = replacements.getOrElse(entry.getName, input.readNBytes((MaxExpandedBytes - copied + 1L).toInt))
        if copied + bytes.length > MaxExpandedBytes then
          throw RichTextCodecException(s"$format archive expands beyond $MaxExpandedBytes bytes")
        writeEntry(zip, copyOf(entry), bytes)
        copyEntries(input, zip, replacements, format, copied + bytes.length, names + entry.getName)

  private def copyOf(entry: ZipEntry): ZipEntry =
    val copy = ZipEntry(entry.getName)
    copy.setMethod(entry.getMethod)
    if entry.getTime >= 0 then copy.setTime(entry.getTime)
    copy

  private def writeEntry(zip: ZipOutputStream, entry: ZipEntry, bytes: Array[Byte]): Unit =
    if entry.getMethod == ZipEntry.STORED then
      val checksum = CRC32()
      checksum.update(bytes)
      entry.setSize(bytes.length.toLong)
      entry.setCompressedSize(bytes.length.toLong)
      entry.setCrc(checksum.getValue)
    zip.putNextEntry(entry)
    zip.write(bytes)
    zip.closeEntry()
