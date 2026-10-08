package com.serenity.richtext

import com.serenity.richtext.RichTextTestPackages.{entries, entry, names, replaceText}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Saving an OpenDocument text keeps the parts and paragraphs the model does not own. */
class OdtPassthroughSpec extends AnyFlatSpec with Matchers with EitherValues:
  private val source: Array[Byte] = GoldenFixtures.zip(GoldenFixtures.writerNotes.entries, Set("mimetype"))

  private def opened: RichTextDocument = OdtDocumentCodec.readBytes(source).value

  private def contentXml(archive: Array[Byte]): String = entry(archive, "content.xml").text

  private val sourceXml = contentXml(source)

  "An ODT opened and saved without edits" should "contain every entry in order with the same bytes and methods" in {
    val saved = OdtDocumentCodec.writeBytes(opened)

    names(saved) shouldBe names(source)
    entries(saved).map(entry => (entry.name, entry.bytes, entry.method)) shouldBe
      entries(source).map(entry => (entry.name, entry.bytes, entry.method))
    entries(saved).head.name shouldBe "mimetype"
    entries(saved).head.method shouldBe RichTextTestPackages.ZipStored
  }

  "An ODT with styles, a table and an image" should "keep them when one paragraph is edited" in {
    val saved = OdtDocumentCodec.writeBytes(replaceText(opened, 2, "online", "offline"))

    List("styles.xml", "meta.xml", "Pictures/pixel.png", "META-INF/manifest.xml", "mimetype")
      .foreach(name => entry(saved, name) shouldBe entry(source, name))
    val xml = contentXml(saved)
    xml should include(sourceXml.substring(sourceXml.indexOf("<table:table "), sourceXml.indexOf("</table:table>")))
    xml should include("""<draw:image xlink:href="Pictures/pixel.png"""")
    xml should include("""text:style-name="Internet_20_link">the notes</text:a> offline.""")
  }

  it should "copy every untouched paragraph exactly" in {
    val xml = contentXml(OdtDocumentCodec.writeBytes(replaceText(opened, 2, "online", "offline")))

    xml should include("""<text:h text:style-name="Heading_20_1" text:outline-level="1">Field notes</text:h>""")
    xml should include("""<text:bookmark text:name="here"/>bookmark.</text:p>""")
    xml should include("""<text:p text:style-name="Standard">Final paragraph.</text:p>""")
  }

  "An edited ODT paragraph" should "keep its bookmark and its span style" in {
    val xml = contentXml(OdtDocumentCodec.writeBytes(replaceText(opened, 1, "centred", "centered")))

    xml should include("""<text:p text:style-name="P1">A centered line with """)
    xml should include("""<text:bookmark text:name="here"/>""")
    xml should include("bold words")
  }

  it should "get an automatic style when its formatting changes, leaving the existing ones alone" in {
    val document = opened.setParagraphAlignment(
      RichTextRange(RichTextPosition(2, 0), RichTextPosition(2, 0)),
      ParagraphAlignment.Right
    )
    val xml = contentXml(OdtDocumentCodec.writeBytes(document))

    xml should include("""<style:style style:name="P1" style:family="paragraph" style:parent-style-name="Standard">""")
    xml should include("""style:parent-style-name="Standard"><style:paragraph-properties fo:text-align="end">""")
    xml should include("""<text:p text:style-name="SerenityParagraph1">See """)
    OdtDocumentCodec.readBytes(OdtDocumentCodec.writeBytes(document)).value.paragraphAt(2).map(_.alignment) shouldBe
      Some(ParagraphAlignment.Right)
  }

  it should "write a text style for newly bold text" in {
    val document = opened.applyMark(RichTextRange(RichTextPosition(4, 0), RichTextPosition(4, 5)), InlineMark.Bold)
    val saved    = OdtDocumentCodec.writeBytes(document)

    contentXml(saved) should include("""<text:span text:style-name="SerenityText1">Final</text:span> paragraph.""")
    OdtDocumentCodec.readBytes(saved).value.paragraphAt(4).flatMap(_.runs.headOption).map(_.style.marks) shouldBe
      Some(Set(InlineMark.Bold))
  }

  it should "model the image and the bookmark as opaque atoms" in {
    opened.paragraphAt(3).map(_.runs.flatMap(_.atom)) match
      case Some(List(InlineAtom.Opaque(raw, true))) => raw should include("draw:frame")
      case other                                    => fail(s"Expected one visible opaque atom, got $other")
    opened.paragraphAt(1).map(_.runs.flatMap(_.atom).collect { case InlineAtom.Opaque(_, visible) => visible }) shouldBe
      Some(List(false))
  }

  "A saved ODT edit" should "read back and save again unchanged" in {
    val once  = OdtDocumentCodec.writeBytes(replaceText(opened, 2, "online", "offline"))
    val twice = OdtDocumentCodec.writeBytes(OdtDocumentCodec.readBytes(once).value)

    entries(twice).map(entry => entry.name -> entry.bytes) shouldBe entries(once).map(entry =>
      entry.name -> entry.bytes
    )
  }
