package com.serenity.richtext

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RtfDocumentCodecSpec extends AnyFlatSpec with Matchers:

  "RtfDocumentCodec" should "read inline marks and paragraph alignment from RTF" in {
    val rtf =
      """{\rtf1\ansi\pard\qc plain \b bold\b0  \i italic\i0  \ul under\ul0\par}"""

    val document  = RtfDocumentCodec.readBytes(rtf.getBytes(StandardCharsets.UTF_8))
    val paragraph = singleParagraph(document)

    paragraph.alignment shouldBe ParagraphAlignment.Center
    paragraph.plainText should include("plain bold italic under")
    marksForText(paragraph, "bold") should contain(InlineMark.Bold)
    marksForText(paragraph, "italic") should contain(InlineMark.Italic)
    marksForText(paragraph, "under") should contain(InlineMark.Underline)
  }

  it should "write native rich text documents as readable RTF" in {
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

    val decoded   = RtfDocumentCodec.readBytes(RtfDocumentCodec.writeBytes(source))
    val paragraph = singleParagraph(decoded)

    paragraph.alignment shouldBe ParagraphAlignment.Right
    paragraph.plainText shouldBe "Hello world"
    marksForText(paragraph, "world") should contain allOf (InlineMark.Bold, InlineMark.Underline)
  }

  it should "preserve explicit font size and colour metadata" in {
    val source = RichTextDocument
      .oneParagraph(
        "styled"
      )
      .copy(
        paragraphs = List(
          RichTextParagraph(
            List(
              RichTextRun(
                "styled",
                RichTextStyle(fontSize = Some(18.0f), color = Some("#336699"))
              )
            )
          )
        )
      )

    val decodedStyle = singleParagraph(RtfDocumentCodec.readBytes(RtfDocumentCodec.writeBytes(source))).runs
      .find(_.text.contains("styled"))
      .map(_.style)

    decodedStyle.flatMap(_.fontSize).map(_.round) shouldBe Some(18)
    decodedStyle.flatMap(_.color) shouldBe Some("#336699")
  }

  it should "read native RTF tab and line controls as inline structural text" in {
    val rtf =
      """{\rtf1\ansi\pard alpha\tab beta\line gamma\par}"""

    val decoded = RtfDocumentCodec.readBytes(rtf.getBytes(StandardCharsets.UTF_8))

    singleParagraph(decoded).plainText shouldBe "alpha\tbeta\ngamma"
  }

  it should "write tabs and line breaks as native RTF controls" in {
    val source = RichTextDocument.oneParagraph("alpha\tbeta\ngamma")

    val bytes   = RtfDocumentCodec.writeBytes(source)
    val rtfText = String(bytes, StandardCharsets.UTF_8)
    val decoded = RtfDocumentCodec.readBytes(bytes)

    rtfText should include("\\tab")
    rtfText should include("\\line")
    singleParagraph(decoded).plainText shouldBe "alpha\tbeta\ngamma"
  }

  it should "read and write RTF files through IO" in {
    val path   = Files.createTempFile("serenity-rich-text", ".rtf")
    val source = RichTextDocument.oneParagraph("Saved text")

    try
      RtfDocumentCodec.write(source, path).unsafeRunSync()

      val loaded = RtfDocumentCodec.read(path).unsafeRunSync()

      loaded.plainText shouldBe "Saved text"
    finally Files.deleteIfExists(path)
  }

  private def marksForText(paragraph: RichTextParagraph, text: String): Set[InlineMark] =
    paragraph.runs
      .find(_.text.contains(text))
      .map(_.style.marks)
      .getOrElse(Set.empty)

  private def singleParagraph(document: RichTextDocument): RichTextParagraph =
    document.paragraphs match
      case paragraph :: Nil => paragraph
      case other            => fail(s"Expected one paragraph, got ${other.size}")
