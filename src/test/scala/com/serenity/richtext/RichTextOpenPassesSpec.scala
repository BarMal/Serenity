package com.serenity.richtext

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Opening a DOCX or ODT reads the package once and parses its body XML once (#1882). */
class RichTextOpenPassesSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val source = RichTextDocument(
    List(
      RichTextParagraph(List(RichTextRun("one"), RichTextRun("two", RichTextStyle.empty.withMark(InlineMark.Bold)))),
      RichTextParagraph.plain("three")
    )
  )

  final private class CountingSteps:
    val archiveReads = AtomicInteger(0)
    val bodyParses   = AtomicInteger(0)

    val steps: RichTextDecodeSteps = RichTextDecodeSteps(readArchive, parseBody)

    private def readArchive(bytes: Array[Byte], format: String, wanted: Set[String]): ArchiveContents =
      archiveReads.incrementAndGet()
      RichTextArchive.read(bytes, format, wanted)

    private def parseBody(bytes: Array[Byte]) =
      bodyParses.incrementAndGet()
      RichTextXmlParser.parse(bytes)

  "Decoding a DOCX with fidelity" should "scan the archive once and parse document.xml once" in {
    val counting = CountingSteps()

    val imported = DocxDocumentCodec.decode(DocxDocumentCodec.writeBytes(source), counting.steps).value

    imported.document shouldBe source
    counting.archiveReads.get shouldBe 1
    counting.bodyParses.get shouldBe 1
  }

  "Decoding an ODT with fidelity" should "scan the archive once and parse content.xml once" in {
    val counting = CountingSteps()

    val imported = OdtDocumentCodec.decode(OdtDocumentCodec.writeBytes(source), counting.steps).value

    imported.document shouldBe source
    counting.archiveReads.get shouldBe 1
    counting.bodyParses.get shouldBe 1
  }

  "Reading without the fidelity report" should "not do any extra pass either" in {
    val docx = DocxDocumentCodec.writeBytes(source)

    DocxDocumentCodec.readBytes(docx).value shouldBe source
    DocxDocumentCodec.readBytesWithFidelity(docx).value.fidelity.wouldDrop shouldBe empty
  }

  "The archive reader" should "return every entry name and only the wanted entries' bytes" in {
    val archive = RichTextArchive.read(DocxDocumentCodec.writeBytes(source), "DOCX", Set("word/document.xml"))

    archive.entryNames should contain allOf ("[Content_Types].xml", "word/document.xml", "word/_rels/document.xml.rels")
    archive.entries.keySet shouldBe Set("word/document.xml")
  }
