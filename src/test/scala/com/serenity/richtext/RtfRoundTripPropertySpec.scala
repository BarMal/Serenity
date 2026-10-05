package com.serenity.richtext

import java.nio.charset.StandardCharsets

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Whatever the model can express must survive `writeBytes` then `readBytes`, and the bytes written must stay 7-bit
  * ASCII so no code page can corrupt them.
  */
class RtfRoundTripPropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  private val textPiece: Gen[String] = Gen.oneOf(
    "a",
    "word",
    " ",
    "  ",
    "é",
    "€",
    "Ж",
    "日本",
    "😀",
    "\\",
    "{",
    "}",
    "\t",
    "\n",
    "?",
    ";",
    "\\line",
    "\\u233"
  )

  private val genText: Gen[String] = Gen.listOf(textPiece).map(_.mkString)

  private val genMarks: Gen[Set[InlineMark]] =
    Gen.someOf(InlineMark.values.toList).map(_.toSet)

  private val genFamily: Gen[Option[String]] =
    Gen.option(Gen.oneOf("Georgia", "Times New Roman", "Courier", "Noto Sans CJK"))

  private val genSize: Gen[Option[Float]] =
    Gen.option(Gen.choose(2, 144).map(_ / 2.0f))

  private val genColor: Gen[Option[String]] =
    Gen.option(Gen.listOfN(6, Gen.hexChar).map(digits => s"#${digits.mkString.toLowerCase}"))

  private val genStyle: Gen[RichTextStyle] =
    for
      marks  <- genMarks
      family <- genFamily
      size   <- genSize
      color  <- genColor
    yield RichTextStyle(marks, family, size, color)

  private val genRun: Gen[RichTextRun] =
    for
      text  <- genText
      style <- genStyle
    yield RichTextRun(text, style)

  private val genAlignment: Gen[ParagraphAlignment] = Gen.oneOf(ParagraphAlignment.values.toList)

  private val genRole: Gen[ParagraphRole] = Gen.frequency(
    6 -> Gen.const(ParagraphRole.Body),
    3 -> Gen.choose(1, 12).map(ParagraphRole.Heading(_)),
    1 -> Gen.choose(1, 6).map(ParagraphRole.DropCap(_))
  )

  private val genParagraph: Gen[RichTextParagraph] =
    for
      runs      <- Gen.listOf(genRun)
      alignment <- genAlignment
      role      <- genRole
    yield RichTextParagraph(runs, alignment, role).normalized

  private val genDocument: Gen[RichTextDocument] =
    Gen.nonEmptyListOf(genParagraph).map(RichTextDocument(_).normalized)

  property("writing then reading reproduces the document") {
    forAll(genDocument) { document =>
      val decoded = RtfDocumentCodec.readBytes(RtfDocumentCodec.writeBytes(document))

      decoded.map(_.paragraphs) shouldBe Right(document.paragraphs)
    }
  }

  property("written RTF is 7-bit ASCII with balanced braces") {
    forAll(genDocument) { document =>
      val bytes = RtfDocumentCodec.writeBytes(document)
      val rtf   = String(bytes, StandardCharsets.US_ASCII)

      bytes.forall(byte => byte >= 0) shouldBe true
      RtfParser.parse(bytes).isRight shouldBe true
      rtf should startWith("{\\rtf1")
    }
  }

  property("reading is total: any prefix of written RTF yields Left or Right, never an exception") {
    forAll(genDocument, Gen.choose(0, 400)) { (document, cut) =>
      val bytes = RtfDocumentCodec.writeBytes(document)

      noException should be thrownBy RtfDocumentCodec.readBytes(bytes.take(cut))
    }
  }

  property("reading arbitrary bytes never throws") {
    forAll { (bytes: Array[Byte]) =>
      noException should be thrownBy RtfDocumentCodec.readBytes(bytes)
    }
  }

  property("rewriting an imported document is stable") {
    forAll(genDocument) { document =>
      val once  = RtfDocumentCodec.writeBytes(document)
      val again = RtfDocumentCodec.readBytes(once).map(RtfDocumentCodec.writeBytes)

      again.map(String(_, StandardCharsets.US_ASCII)) shouldBe Right(String(once, StandardCharsets.US_ASCII))
    }
  }
