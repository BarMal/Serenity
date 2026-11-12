package com.serenity.richtext

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Shadow mark reads from and writes to every rich-text format that can say it. */
class ShadowMarkCodecSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val shadow = RichTextStyle(marks = Set(InlineMark.Shadow))

  private def documentOf(runs: RichTextRun*): RichTextDocument =
    RichTextDocument(List(RichTextParagraph(runs.toList)))

  private def marksOf(document: RichTextDocument, text: String): Set[InlineMark] =
    document.paragraphs
      .flatMap(_.runs)
      .find(_.text.contains(text))
      .map(_.style.marks)
      .getOrElse(fail(s"no run holds '$text'"))

  private def zipped(entryName: String, content: String): Array[Byte] =
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output)
    try
      zip.putNextEntry(ZipEntry(entryName))
      zip.write(content.getBytes(StandardCharsets.UTF_8))
      zip.closeEntry()
    finally zip.close()
    output.toByteArray

  private def entryOf(bytes: Array[Byte], entryName: String): String =
    val zip = ZipInputStream(java.io.ByteArrayInputStream(bytes))
    try
      Iterator
        .continually(zip.getNextEntry)
        .takeWhile(_ != null)
        .collectFirst { case entry if entry.getName == entryName => String(zip.readAllBytes(), StandardCharsets.UTF_8) }
        .getOrElse(fail(s"no entry $entryName"))
    finally zip.close()

  private def docxBody(runs: String): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
       |  <w:body><w:p>$runs</w:p></w:body>
       |</w:document>""".stripMargin

  "DOCX" should "read w:shadow as the Shadow mark, as a modelled property" in {
    val xml =
      docxBody(
        """<w:r><w:t>plain </w:t></w:r><w:r><w:rPr><w:shadow/></w:rPr><w:t>shaded</w:t></w:r>"""
      )
    val document = DocxDocumentCodec.readBytes(zipped("word/document.xml", xml)).value

    marksOf(document, "shaded") shouldBe Set(InlineMark.Shadow)
    marksOf(document, "plain") shouldBe Set.empty
    document.paragraphs.flatMap(_.runs).find(_.text == "shaded").map(_.style.extras) shouldBe Some(Nil)
  }

  it should "not read a switched-off w:shadow as a mark" in {
    val xml = docxBody(
      """<w:r><w:rPr><w:shadow w:val="0"/></w:rPr><w:t>off</w:t></w:r>""" +
        """<w:r><w:rPr><w:shadow w:val="false"/></w:rPr><w:t>never</w:t></w:r>"""
    )
    val document = DocxDocumentCodec.readBytes(zipped("word/document.xml", xml)).value

    marksOf(document, "off") shouldBe Set.empty
    marksOf(document, "never") shouldBe Set.empty
  }

  it should "write the mark as w:shadow in schema order, and read it back" in {
    val source = documentOf(
      RichTextRun("shaded", shadow),
      RichTextRun("both", RichTextStyle(marks = Set(InlineMark.Bold, InlineMark.Shadow)))
    )
    val bytes = DocxDocumentCodec.writeBytes(source)
    val xml   = entryOf(bytes, "word/document.xml")

    xml should include("<w:rPr><w:shadow/></w:rPr>")
    xml should include("<w:rPr><w:b/><w:shadow/></w:rPr>")
    val decoded = DocxDocumentCodec.readBytes(bytes).value
    marksOf(decoded, "shaded") shouldBe Set(InlineMark.Shadow)
    marksOf(decoded, "both") shouldBe Set(InlineMark.Bold, InlineMark.Shadow)
  }

  "RTF" should "read \\shad as the Shadow mark and \\shad0 as off" in {
    val rtf      = """{\rtf1\ansi\pard plain \shad shaded\shad0  after\par}"""
    val document = RtfDocumentCodec.readBytes(rtf.getBytes(StandardCharsets.UTF_8)).value

    marksOf(document, "shaded") shouldBe Set(InlineMark.Shadow)
    marksOf(document, "after") shouldBe Set.empty
  }

  it should "write the mark as \\shad, and read it back" in {
    val source  = documentOf(RichTextRun("plain "), RichTextRun("shaded", shadow))
    val bytes   = RtfDocumentCodec.writeBytes(source)
    val decoded = RtfDocumentCodec.readBytes(bytes).value

    String(bytes, StandardCharsets.ISO_8859_1) should include("""{\shad shaded}""")
    marksOf(decoded, "shaded") shouldBe Set(InlineMark.Shadow)
    marksOf(decoded, "plain") shouldBe Set.empty
  }

  private def odtContent(textProperties: String): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<office:document-content
       |    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
       |    xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
       |    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
       |    xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0">
       |  <office:automatic-styles>
       |    <style:style style:name="T1" style:family="text"><style:text-properties $textProperties/></style:style>
       |  </office:automatic-styles>
       |  <office:body><office:text>
       |    <text:p>plain <text:span text:style-name="T1">styled</text:span></text:p>
       |  </office:text></office:body>
       |</office:document-content>""".stripMargin

  "ODT" should "read fo:text-shadow as the Shadow mark unless it is none" in {
    val shaded = OdtDocumentCodec.readBytes(zipped("content.xml", odtContent("""fo:text-shadow="2pt 2pt""""))).value
    val none   = OdtDocumentCodec.readBytes(zipped("content.xml", odtContent("""fo:text-shadow="none""""))).value

    marksOf(shaded, "styled") shouldBe Set(InlineMark.Shadow)
    marksOf(none, "styled") shouldBe Set.empty
  }

  it should "write the mark as fo:text-shadow, and read it back" in {
    val source  = documentOf(RichTextRun("plain "), RichTextRun("shaded", shadow))
    val bytes   = OdtDocumentCodec.writeBytes(source)
    val decoded = OdtDocumentCodec.readBytes(bytes).value

    entryOf(bytes, "content.xml") should include("""fo:text-shadow="1pt 1pt"""")
    marksOf(decoded, "shaded") shouldBe Set(InlineMark.Shadow)
    marksOf(decoded, "plain") shouldBe Set.empty
  }
end ShadowMarkCodecSpec
