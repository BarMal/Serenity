package com.serenity.richtext

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocxDocumentCodecSpec extends AnyFlatSpec with Matchers:

  "DocxDocumentCodec" should "read inline marks and paragraph alignment from DOCX" in {
    val documentXml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
        |  <w:body>
        |    <w:p>
        |      <w:pPr><w:jc w:val="center"/></w:pPr>
        |      <w:r><w:t>plain </w:t></w:r>
        |      <w:r><w:rPr><w:b/></w:rPr><w:t>bold</w:t></w:r>
        |      <w:r><w:t> </w:t></w:r>
        |      <w:r><w:rPr><w:i/></w:rPr><w:t>italic</w:t></w:r>
        |      <w:r><w:t> </w:t></w:r>
        |      <w:r><w:rPr><w:u w:val="single"/></w:rPr><w:t>under</w:t></w:r>
        |    </w:p>
        |  </w:body>
        |</w:document>""".stripMargin

    val document  = DocxDocumentCodec.readBytes(docxBytes(documentXml))
    val paragraph = singleParagraph(document)

    paragraph.alignment shouldBe ParagraphAlignment.Center
    paragraph.plainText shouldBe "plain bold italic under"
    marksForText(paragraph, "bold") should contain(InlineMark.Bold)
    marksForText(paragraph, "italic") should contain(InlineMark.Italic)
    marksForText(paragraph, "under") should contain(InlineMark.Underline)
  }

  it should "write native rich text documents as readable DOCX" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(
          runs = List(
            RichTextRun("Hello ", RichTextStyle.empty),
            RichTextRun("world", RichTextStyle(marks = Set(InlineMark.Bold, InlineMark.Underline)))
          ),
          alignment = ParagraphAlignment.Right
        )
      )
    )

    val decoded   = DocxDocumentCodec.readBytes(DocxDocumentCodec.writeBytes(source))
    val paragraph = singleParagraph(decoded)

    paragraph.alignment shouldBe ParagraphAlignment.Right
    paragraph.plainText shouldBe "Hello world"
    marksForText(paragraph, "world") should contain allOf (InlineMark.Bold, InlineMark.Underline)
  }

  it should "preserve heading roles and font metadata" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(
          List(
            RichTextRun(
              "Chapter One",
              RichTextStyle(fontFamily = Some("Serif"), fontSize = Some(18.0f), color = Some("#336699"))
            )
          ),
          role = ParagraphRole.Heading(2)
        )
      )
    )

    val decoded   = DocxDocumentCodec.readBytes(DocxDocumentCodec.writeBytes(source))
    val paragraph = singleParagraph(decoded)
    val style     = paragraph.runs.head.style

    paragraph.role shouldBe ParagraphRole.Heading(2)
    style.fontFamily shouldBe Some("Serif")
    style.fontSize.map(_.round) shouldBe Some(18)
    style.color shouldBe Some("#336699")
  }

  it should "write tabs and line breaks as native DOCX run elements" in {
    val source = RichTextDocument.oneParagraph("alpha\tbeta\ngamma")

    val bytes       = DocxDocumentCodec.writeBytes(source)
    val documentXml = zipEntryText(bytes, "word/document.xml")
    val decoded     = DocxDocumentCodec.readBytes(bytes)

    documentXml should include("<w:tab/>")
    documentXml should include("<w:br/>")
    singleParagraph(decoded).plainText shouldBe "alpha\tbeta\ngamma"
  }

  it should "read and write DOCX files through IO" in {
    val path   = Files.createTempFile("serenity-rich-text", ".docx")
    val source = RichTextDocument.oneParagraph("Saved text")

    try
      DocxDocumentCodec.write(source, path).unsafeRunSync()

      val loaded = DocxDocumentCodec.read(path).unsafeRunSync()

      loaded.plainText shouldBe "Saved text"
    finally Files.deleteIfExists(path)
  }

  it should "fail safely when the DOCX document entry is missing" in {
    val error = the[RichTextCodecException] thrownBy DocxDocumentCodec.readBytes(emptyZipBytes())

    error.getMessage should include("missing word/document.xml")
  }

  it should "fail safely when the DOCX document entry is oversized" in {
    val bytes = docxRawBytes("word/document.xml", Array.fill(RichTextArchive.MaxXmlEntryBytes + 1)(0.toByte))

    val error = the[RichTextCodecException] thrownBy DocxDocumentCodec.readBytes(bytes)

    error.getMessage should include("word/document.xml is too large")
  }

  it should "wrap malformed DOCX XML in a codec exception" in {
    val error = the[RichTextCodecException] thrownBy DocxDocumentCodec.readBytes(docxBytes("<w:document>"))

    error.getMessage should include("DOCX document could not be decoded")
  }

  private def docxBytes(documentXml: String): Array[Byte] =
    docxRawBytes("word/document.xml", documentXml.getBytes(StandardCharsets.UTF_8))

  private def emptyZipBytes(): Array[Byte] =
    val output = java.io.ByteArrayOutputStream()
    ZipOutputStream(output).close()
    output.toByteArray

  private def docxRawBytes(entryName: String, content: Array[Byte]): Array[Byte] =
    val output = java.io.ByteArrayOutputStream()
    val zip    = ZipOutputStream(output)
    try
      zip.putNextEntry(ZipEntry(entryName))
      zip.write(content)
      zip.closeEntry()
    finally zip.close()
    output.toByteArray

  private def zipEntryText(bytes: Array[Byte], name: String): String =
    val input = ZipInputStream(java.io.ByteArrayInputStream(bytes))
    try
      Iterator
        .continually(input.getNextEntry)
        .takeWhile(_ != null)
        .find(_.getName == name)
        .map { _ =>
          val output = java.io.ByteArrayOutputStream()
          input.transferTo(output)
          output.toString(StandardCharsets.UTF_8)
        }
        .getOrElse(fail(s"Missing zip entry: $name"))
    finally input.close()

  private def marksForText(paragraph: RichTextParagraph, text: String): Set[InlineMark] =
    paragraph.runs
      .find(_.text.contains(text))
      .map(_.style.marks)
      .getOrElse(Set.empty)

  private def singleParagraph(document: RichTextDocument): RichTextParagraph =
    document.paragraphs match
      case paragraph :: Nil => paragraph
      case other            => fail(s"Expected one paragraph, got ${other.size}")
