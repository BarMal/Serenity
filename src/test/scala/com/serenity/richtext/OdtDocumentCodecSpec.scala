package com.serenity.richtext

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipOutputStream}

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OdtDocumentCodecSpec extends AnyFlatSpec with Matchers:

  "OdtDocumentCodec" should "read inline marks and paragraph alignment from ODT" in {
    val contentXml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<office:document-content
        |    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
        |    xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
        |    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
        |    xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0">
        |  <office:automatic-styles>
        |    <style:style style:name="Pcenter" style:family="paragraph">
        |      <style:paragraph-properties fo:text-align="center"/>
        |    </style:style>
        |    <style:style style:name="Tbold" style:family="text">
        |      <style:text-properties fo:font-weight="bold"/>
        |    </style:style>
        |    <style:style style:name="Titalic" style:family="text">
        |      <style:text-properties fo:font-style="italic"/>
        |    </style:style>
        |    <style:style style:name="Tunder" style:family="text">
        |      <style:text-properties style:text-underline-style="solid"/>
        |    </style:style>
        |  </office:automatic-styles>
        |  <office:body>
        |    <office:text>
        |      <text:p text:style-name="Pcenter">plain <text:span text:style-name="Tbold">bold</text:span> <text:span text:style-name="Titalic">italic</text:span> <text:span text:style-name="Tunder">under</text:span></text:p>
        |    </office:text>
        |  </office:body>
        |</office:document-content>""".stripMargin

    val document  = OdtDocumentCodec.readBytes(odtBytes(contentXml))
    val paragraph = singleParagraph(document)

    paragraph.alignment shouldBe ParagraphAlignment.Center
    paragraph.plainText shouldBe "plain bold italic under"
    marksForText(paragraph, "bold") should contain(InlineMark.Bold)
    marksForText(paragraph, "italic") should contain(InlineMark.Italic)
    marksForText(paragraph, "under") should contain(InlineMark.Underline)
  }

  it should "write native rich text documents as readable ODT" in {
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

    val decoded   = OdtDocumentCodec.readBytes(OdtDocumentCodec.writeBytes(source))
    val paragraph = singleParagraph(decoded)

    paragraph.alignment shouldBe ParagraphAlignment.Right
    paragraph.plainText shouldBe "Hello world"
    marksForText(paragraph, "world") should contain allOf (InlineMark.Bold, InlineMark.Underline)
  }

  it should "preserve explicit font size, family, and colour metadata" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(
          List(
            RichTextRun(
              "styled",
              RichTextStyle(fontFamily = Some("Serif"), fontSize = Some(18.0f), color = Some("#336699"))
            )
          )
        )
      )
    )

    val decodedStyle = singleParagraph(OdtDocumentCodec.readBytes(OdtDocumentCodec.writeBytes(source))).runs
      .find(_.text.contains("styled"))
      .map(_.style)

    decodedStyle.flatMap(_.fontFamily) shouldBe Some("Serif")
    decodedStyle.flatMap(_.fontSize).map(_.round) shouldBe Some(18)
    decodedStyle.flatMap(_.color) shouldBe Some("#336699")
  }

  it should "read and write ODT files through IO" in {
    val path   = Files.createTempFile("serenity-rich-text", ".odt")
    val source = RichTextDocument.oneParagraph("Saved text")

    try
      OdtDocumentCodec.write(source, path).unsafeRunSync()

      val loaded = OdtDocumentCodec.read(path).unsafeRunSync()

      loaded.plainText shouldBe "Saved text"
    finally Files.deleteIfExists(path)
  }

  private def odtBytes(contentXml: String): Array[Byte] =
    val output = java.io.ByteArrayOutputStream()
    val zip    = ZipOutputStream(output)
    try
      zip.putNextEntry(ZipEntry("content.xml"))
      zip.write(contentXml.getBytes(StandardCharsets.UTF_8))
      zip.closeEntry()
    finally zip.close()
    output.toByteArray

  private def marksForText(paragraph: RichTextParagraph, text: String): Set[InlineMark] =
    paragraph.runs
      .find(_.text.contains(text))
      .map(_.style.marks)
      .getOrElse(Set.empty)

  private def singleParagraph(document: RichTextDocument): RichTextParagraph =
    document.paragraphs match
      case paragraph :: Nil => paragraph
      case other            => fail(s"Expected one paragraph, got ${other.size}")
