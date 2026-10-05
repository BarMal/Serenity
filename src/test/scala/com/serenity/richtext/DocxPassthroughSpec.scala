package com.serenity.richtext

import com.serenity.richtext.RichTextTestPackages.{entries, entry, names, replaceText}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1896: saving a Word document writes back everything the model does not own. */
class DocxPassthroughSpec extends AnyFlatSpec with Matchers with EitherValues:
  private val source: Array[Byte] = GoldenFixtures.zip(GoldenFixtures.wordReport.entries)

  private def opened: RichTextDocument = DocxDocumentCodec.readBytes(source).value

  private def documentXml(archive: Array[Byte]): String = entry(archive, "word/document.xml").text

  private val sourceXml = documentXml(source)

  "A DOCX opened and saved without edits" should "contain every entry in order with the same bytes" in {
    val saved = DocxDocumentCodec.writeBytes(opened)

    names(saved) shouldBe names(source)
    entries(saved).map(entry => entry.name -> entry.bytes) shouldBe entries(source).map(entry =>
      entry.name -> entry.bytes
    )
  }

  it should "keep each entry's compression method" in {
    entries(DocxDocumentCodec.writeBytes(opened)).map(_.method) shouldBe entries(source).map(_.method)
  }

  "A DOCX with styles, a comment, a table and an image" should "keep all of them when one body paragraph is edited" in {
    val edited = replaceText(opened, 1, "first", "edited")
    val saved  = DocxDocumentCodec.writeBytes(edited)

    names(saved) shouldBe names(source)
    List("word/styles.xml", "word/comments.xml", "word/settings.xml", "word/media/pixel.png", "docProps/core.xml")
      .foreach(name => entry(saved, name) shouldBe entry(source, name))
    val xml = documentXml(saved)
    xml should include("This is the edited body paragraph")
    xml should include(
      sourceXml.substring(sourceXml.indexOf("<w:tbl>"), sourceXml.indexOf("</w:tbl>") + "</w:tbl>".length)
    )
    xml should include("""<a:blip r:embed="rId4"/>""")
    xml should include("""<w:commentReference w:id="1"/>""")
    xml should include("""<w:commentRangeStart w:id="1"/>""")
  }

  it should "change only the edited paragraph's slice of document.xml" in {
    val saved = documentXml(DocxDocumentCodec.writeBytes(replaceText(opened, 1, "first", "edited")))

    val identifier = sourceXml.indexOf("""w14:paraId="1A2B3C02"""")
    val sliceStart = sourceXml.lastIndexOf("<w:p ", identifier)
    val sliceEnd   = sourceXml.indexOf("</w:p>", identifier) + "</w:p>".length
    val samePrefix = sourceXml.zip(saved).takeWhile(_ == _).size
    val sameSuffix = sourceXml.reverse.zip(saved.reverse).takeWhile(_ == _).size

    samePrefix should be >= sliceStart
    sourceXml.length - sameSuffix should be <= sliceEnd
    saved.length should be > sourceXml.length
  }

  it should "copy every untouched paragraph's markup exactly" in {
    val saved = documentXml(DocxDocumentCodec.writeBytes(replaceText(opened, 1, "first", "edited")))

    List("1A2B3C01", "1A2B3C03", "1A2B3C04", "1A2B3C05").foreach { id =>
      val start = sourceXml.indexOf(s"""w14:paraId="$id"""")
      val slice =
        sourceXml.substring(sourceXml.lastIndexOf("<w:p ", start), sourceXml.indexOf("</w:p>", start) + "</w:p>".length)
      saved should include(slice)
    }
  }

  it should "keep a hyperlink's URL when another paragraph is edited" in {
    val saved = DocxDocumentCodec.writeBytes(replaceText(opened, 4, "Closing", "Final"))

    entry(saved, "word/_rels/document.xml.rels") shouldBe entry(source, "word/_rels/document.xml.rels")
    val link = DocxDocumentCodec.readBytes(saved).value.paragraphAt(2).flatMap(_.runs.flatMap(_.style.link).headOption)
    link shouldBe Some("https://example.com/project?tab=1&view=full")
  }

  it should "reuse a hyperlink's relationship when text in its own paragraph is edited" in {
    val saved = DocxDocumentCodec.writeBytes(replaceText(opened, 2, "for details", "for more details"))

    entry(saved, "word/_rels/document.xml.rels") shouldBe entry(source, "word/_rels/document.xml.rels")
    documentXml(saved) should include("""<w:hyperlink r:id="rId5" w:history="1">""")
    documentXml(saved) should include("for more details")
  }

  it should "add a relationship with a fresh id for a link made in an edited paragraph" in {
    val document = opened.updateInlineStyle(
      RichTextRange(RichTextPosition(4, 0), RichTextPosition(4, 7))
    )(_.withLink("https://new.example/page"))
    val saved = DocxDocumentCodec.writeBytes(document)

    val relationships = entry(saved, "word/_rels/document.xml.rels").text
    relationships should include(
      """Id="rId5" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink" Target="https://example.com/project?tab=1&amp;view=full""""
    )
    relationships should include(
      """Id="rId6" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink" Target="https://new.example/page" TargetMode="External""""
    )
    documentXml(saved) should include("""<w:hyperlink r:id="rId6">""")
    entry(saved, "word/media/pixel.png") shouldBe entry(source, "word/media/pixel.png")
  }

  "An edited paragraph" should "keep its unmodelled paragraph properties, run properties and anchors" in {
    val xml = documentXml(DocxDocumentCodec.writeBytes(replaceText(opened, 1, "first", "edited")))

    xml should include("""<w:pPr><w:keepNext/><w:spacing w:after="120"/><w:jc w:val="both"/></w:pPr>""")
    xml should include("""<w:rPr><w:lang w:val="en-GB"/></w:rPr>""")
    xml should include("""w:rsidR="00A10002" w14:paraId="1A2B3C02"""")
  }

  it should "keep a heading's bookmark and style" in {
    val xml = documentXml(DocxDocumentCodec.writeBytes(replaceText(opened, 0, "Quarterly", "Annual")))

    xml should include("""<w:pStyle w:val="Heading1"/>""")
    xml should include("""<w:bookmarkStart w:id="0" w:name="_Toc1000"/>""")
    xml should include("""<w:bookmarkEnd w:id="0"/>""")
    xml should include("Annual report")
  }

  it should "keep an image written in the same paragraph" in {
    val xml = documentXml(
      DocxDocumentCodec.writeBytes(
        opened.replaceRange(
          RichTextRange(RichTextPosition(3, 1), RichTextPosition(3, 1)),
          " caption"
        )
      )
    )

    xml should include("<w:drawing>")
    xml should include("""<a:blip r:embed="rId4"/>""")
    xml should include(" caption")
  }

  it should "write a changed role over the one it was imported with" in {
    val document = opened
      .setParagraphRole(RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 0)), ParagraphRole.Body)
      .setParagraphRole(RichTextRange(RichTextPosition(4, 0), RichTextPosition(4, 0)), ParagraphRole.Heading(2))
    val xml = documentXml(DocxDocumentCodec.writeBytes(document))

    xml should not include """<w:pStyle w:val="Heading1"/>"""
    xml should include("""<w:pStyle w:val="Heading2"/>""")
  }

  it should "give both halves of a split paragraph its properties without repeating its identifier" in {
    val xml = documentXml(
      DocxDocumentCodec.writeBytes(
        opened.replaceRange(
          RichTextRange(RichTextPosition(1, 8), RichTextPosition(1, 8)),
          "\n"
        )
      )
    )

    xml.split("<w:keepNext/>").length - 1 shouldBe 2
    xml.split("""w14:paraId="1A2B3C02"""").length - 1 shouldBe 1
  }

  "A document with a table between paragraphs" should "keep the table in place when the paragraph before it is joined" in {
    val joined = opened.replaceRange(RichTextRange(RichTextPosition(1, 0), RichTextPosition(2, 0)), "")
    val xml    = documentXml(DocxDocumentCodec.writeBytes(joined))

    xml.indexOf("<w:tbl>") should be > xml.indexOf("w14:paraId=\"1A2B3C02\"")
    xml.indexOf("<w:tbl>") should be < xml.indexOf("<w:drawing>")
    xml should not include "1A2B3C03"
  }

  it should "end with the original section properties" in {
    val xml = documentXml(DocxDocumentCodec.writeBytes(replaceText(opened, 4, "Closing", "Final")))

    xml should endWith(
      """<w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:header="708" w:footer="708" w:gutter="0"/></w:sectPr></w:body></w:document>"""
    )
  }

  "A saved edit" should "read back and save again with every other part unchanged" in {
    val once  = DocxDocumentCodec.writeBytes(replaceText(opened, 1, "first", "edited"))
    val twice = DocxDocumentCodec.writeBytes(DocxDocumentCodec.readBytes(once).value)

    entries(twice).map(entry => entry.name -> entry.bytes) shouldBe entries(once).map(entry =>
      entry.name -> entry.bytes
    )
  }

  "Non-text inline content" should "be opaque atoms that keep a paragraph's text and line shape" in {
    val document = opened

    document.paragraphs.map(_.plainTextLength) shouldBe List(18, 74, 35, 1, 14)
    document.paragraphAt(0).map(_.exportText) shouldBe Some("Quarterly report")
    document.paragraphAt(3).map(_.runs.flatMap(_.atom)) match
      case Some(List(InlineAtom.Opaque(raw, true))) => raw should include("<w:drawing>")
      case other                                    => fail(s"Expected one visible opaque atom, got $other")
    document
      .paragraphAt(1)
      .map(_.runs.flatMap(_.atom).collect { case InlineAtom.Opaque(_, visible) => visible }) shouldBe
      Some(List(false, false, false))
  }

  "A document with a DOCX source" should "write no DOCX markup into an ODT" in {
    val odt = OdtDocumentCodec.writeBytes(opened)

    entry(odt, "content.xml").text should not include "commentRange"
    entry(odt, "content.xml").text should not include "w:drawing"
    OdtDocumentCodec.readBytes(odt).value.exportText should include("This is the first body paragraph")
  }

  "A document with an ODT source" should "write no ODT markup into a DOCX" in {
    val odt  = OdtDocumentCodec.readBytes(GoldenFixtures.zip(GoldenFixtures.writerNotes.entries, Set("mimetype"))).value
    val docx = DocxDocumentCodec.writeBytes(odt)

    entry(docx, "word/document.xml").text should not include "draw:frame"
    entry(docx, "word/document.xml").text should not include "text:bookmark"
    DocxDocumentCodec.readBytes(docx).value.exportText should include("Field notes")
  }
