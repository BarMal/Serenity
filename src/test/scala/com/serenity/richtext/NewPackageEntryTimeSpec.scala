package com.serenity.richtext

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.zip.ZipInputStream

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A new package's bytes depend on the document alone: an entry stamped with the clock makes two encodes a DOS tick
  * apart differ (seen on Windows in RichTextCodecFailureSpec).
  */
class NewPackageEntryTimeSpec extends AnyFlatSpec with Matchers:

  private val text = RichTextDocument(List(RichTextParagraph.plain("hello")))

  private val FixedEntryTime = LocalDateTime.of(1980, 1, 1, 0, 0)

  private def entryTimes(archive: Array[Byte]): List[LocalDateTime] =
    val zip = ZipInputStream(ByteArrayInputStream(archive), StandardCharsets.UTF_8)
    try
      Iterator
        .continually(zip.getNextEntry)
        .takeWhile(_ != null)
        .map(_.getTimeLocal)
        .toList
    finally zip.close()

  "OdtDocumentCodec.writeBytes" should "stamp every entry of a new package with a fixed time" in {
    val times = entryTimes(OdtDocumentCodec.writeBytes(text))

    times should not be empty
    all(times) shouldBe FixedEntryTime
  }

  "DocxDocumentCodec.writeBytes" should "stamp every entry of a new package with a fixed time" in {
    val times = entryTimes(DocxDocumentCodec.writeBytes(text))

    times should not be empty
    all(times) shouldBe FixedEntryTime
  }
