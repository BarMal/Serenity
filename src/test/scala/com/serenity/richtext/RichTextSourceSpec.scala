package com.serenity.richtext

import com.serenity.session.given
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How the provenance a paragraph carries from its package behaves inside the model. */
class RichTextSourceSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val provenance = ParagraphSource(
    blockIndex = 3,
    openTagAttributes = """ w14:paraId="ABCD0001" w:rsidR="0001"""",
    properties = List(RawProperty("keepNext", "<w:keepNext/>")),
    imported = None
  )

  "A paragraph" should "equal another with the same content whatever its provenance" in {
    val plain = RichTextParagraph.plain("same")

    plain.copy(source = Some(provenance)) shouldBe plain
    plain.copy(source = Some(provenance)).hashCode shouldBe plain.hashCode
  }

  it should "be a different paragraph when its text differs, even with the same provenance" in {
    RichTextParagraph.plain("one").copy(source = Some(provenance)) should not be
      RichTextParagraph.plain("two").copy(source = Some(provenance))
  }

  "A derived paragraph" should "keep the unmodelled properties but neither the block nor the unique identifiers" in {
    val derived = RichTextParagraph.plain("half").copy(source = Some(provenance)).derivedWith(Nil).source

    derived.map(_.blockIndex) shouldBe Some(ParagraphSource.NoBlock)
    derived.map(_.openTagAttributes) shouldBe Some(""" w:rsidR="0001"""")
    derived.map(_.properties) shouldBe Some(provenance.properties)
    derived.flatMap(_.imported) shouldBe None
  }

  "A document edit" should "keep the first paragraph's provenance and derive the others" in {
    val document = RichTextDocument(List(RichTextParagraph.plain("hello world").copy(source = Some(provenance))))

    val split = document.replaceRange(RichTextRange(RichTextPosition(0, 5), RichTextPosition(0, 5)), "\n")

    split.paragraphs.map(_.plainText) shouldBe List("hello", " world")
    split.paragraphAt(0).flatMap(_.source).map(_.blockIndex) shouldBe Some(3)
    split.paragraphAt(1).flatMap(_.source).map(_.blockIndex) shouldBe Some(ParagraphSource.NoBlock)
  }

  it should "keep the document's source package" in {
    val source = DocumentSource(PackageFormat.Docx, Array[Byte](1), "word/document.xml", None, None, None)
    val edited = RichTextDocument
      .oneParagraph("text")
      .withSource(Some(source))
      .replaceRange(RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 0)), "a")
      .applyMark(RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 1)), InlineMark.Bold)
      .normalized

    edited.source shouldBe Some(source)
  }

  "An opaque atom" should "occupy one rope character that exports as nothing" in {
    val marker = RichTextRun.opaque("""<w:bookmarkStart w:id="0"/>""", visible = false)

    marker.text shouldBe InlineAtom.OpaqueCharacter.toString
    marker.exportText shouldBe ""
    RichTextRun.softBreak().text shouldBe RichTextRun.AtomCharacter.toString
    marker.mergesWith(marker) shouldBe false
  }

  "The session format" should "round-trip opaque atoms and run extras and leave provenance out" in {
    val styled = RichTextStyle.empty.copy(extras = List(RawProperty("lang", """<w:lang w:val="en-GB"/>""")))
    val document = RichTextDocument(
      List(
        RichTextParagraph(
          List(
            RichTextRun("text", styled),
            RichTextRun.opaque("<w:drawing/>", visible = true),
            RichTextRun.softBreak()
          )
        ).copy(source = Some(provenance))
      )
    )

    val json    = document.asJson.noSpaces
    val decoded = decode[RichTextDocument](json).value

    decoded shouldBe document
    decoded.paragraphAt(0).flatMap(_.source) shouldBe None
    json should not include "keepNext"
  }

  it should "read a style saved before extras existed" in {
    val legacy = """{"marks":["Bold"],"fontFamily":null,"fontSize":null,"color":null,"link":null}"""

    decode[RichTextStyle](legacy).value shouldBe RichTextStyle(marks = Set(InlineMark.Bold))
  }

  it should "not write empty extras" in {
    RichTextStyle.empty.asJson.noSpaces should not include "extras"
  }
