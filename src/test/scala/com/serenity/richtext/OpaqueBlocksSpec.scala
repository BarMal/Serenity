package com.serenity.richtext

import java.nio.charset.StandardCharsets

import com.serenity.richtext.RichTextTestPackages.{entry, replaceText}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1896: a body child the model does not hold is a read-only line that passes through to the saved file untouched. */
class OpaqueBlocksSpec extends AnyFlatSpec with Matchers with EitherValues:
  private val word: Array[Byte] = GoldenFixtures.zip(GoldenFixtures.wordReport.entries)
  private val writer: Array[Byte] =
    GoldenFixtures.zip(GoldenFixtures.writerNotes.entries, Set("mimetype"))

  private def opened: RichTextDocument       = DocxDocumentCodec.readBytes(word).value
  private def openedWriter: RichTextDocument = OdtDocumentCodec.readBytes(writer).value

  private val wordXml = entry(word, "word/document.xml").text
  private val table =
    wordXml.substring(wordXml.indexOf("<w:tbl>"), wordXml.indexOf("</w:tbl>") + "</w:tbl>".length)

  private val writerXml = entry(writer, "content.xml").text

  private val odfTable =
    writerXml.substring(
      writerXml.indexOf("<table:table "),
      writerXml.indexOf("</table:table>") + "</table:table>".length
    )

  private def savedWord(document: RichTextDocument): String =
    entry(DocxDocumentCodec.writeBytes(document), "word/document.xml").text

  private def savedWriter(document: RichTextDocument): String =
    entry(OdtDocumentCodec.writeBytes(document), "content.xml").text

  private def deleteLine(document: RichTextDocument, index: Int): RichTextDocument =
    document.replaceRange(RichTextRange(RichTextPosition(index, 0), RichTextPosition(index, 1)), "")

  private def docx(body: String): Array[Byte] =
    val namespaces =
      """xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math""""
    GoldenFixtures.zip(
      List(
        "[Content_Types].xml" -> "<Types/>".getBytes(StandardCharsets.UTF_8),
        "word/document.xml" ->
          s"""<?xml version="1.0" encoding="UTF-8"?><w:document $namespaces><w:body>$body</w:body></w:document>"""
            .getBytes(StandardCharsets.UTF_8)
      )
    )

  "A DOCX table between two paragraphs" should "be a read-only line of its own, so rope line i is still paragraph i" in {
    val document = opened

    document.paragraphs.size shouldBe 6
    document.paragraphAt(3).exists(_.isOpaqueBlock) shouldBe true
    document.paragraphAt(3).map(_.plainText) shouldBe Some(InlineAtom.BlockCharacter.toString)
    document.paragraphAt(3).flatMap(_.opaqueBlock).map(_.feature) shouldBe Some(DocumentFeature.Tables)
    document.paragraphAt(3).flatMap(_.opaqueBlock).map(_.raw) shouldBe Some(table)
    document.paragraphs.filter(_.isOpaqueBlock).size shouldBe 1
  }

  it should "contribute no text to a plain-text export" in {
    opened.exportText.linesIterator.toList(3) shouldBe ""
    opened.exportText should not include "Region"
  }

  it should "survive editing the paragraph before it" in {
    savedWord(replaceText(opened, 1, "first", "edited")) should include(table)
  }

  it should "survive editing the paragraph after it" in {
    savedWord(replaceText(opened, 5, "Closing", "Final")) should include(table)
  }

  it should "stay between the same two paragraphs" in {
    val xml = savedWord(replaceText(opened, 5, "Closing", "Final"))

    xml.indexOf(table) should be > xml.indexOf("the project page")
    xml.indexOf(table) should be < xml.indexOf("<w:drawing>")
  }

  it should "be written back as it was even when formatting is applied to its line" in {
    val formatted = opened.applyMark(RichTextRange(RichTextPosition(3, 0), RichTextPosition(3, 1)), InlineMark.Bold)

    val xml = savedWord(formatted)

    xml should include(table)
    xml.indexOf("<w:tbl>") shouldBe xml.lastIndexOf("<w:tbl>")
  }

  it should "keep a new empty paragraph in front of it when Enter is pressed before it" in {
    val split = opened.replaceRange(RichTextRange(RichTextPosition(3, 0), RichTextPosition(3, 0)), "\n")

    savedWord(split) should include(table)
    val readBack = DocxDocumentCodec.readBytes(DocxDocumentCodec.writeBytes(split)).value
    readBack.paragraphs.size shouldBe 7
    readBack.paragraphAt(3).map(_.plainText) shouldBe Some("")
    readBack.paragraphAt(4).exists(_.isOpaqueBlock) shouldBe true
  }

  it should "be removed from the saved file when its line is deleted" in {
    val xml = savedWord(deleteLine(opened, 3))

    xml should not include "<w:tbl>"
    xml should include("the project page")
    xml should include("<w:drawing>")
  }

  it should "be removed, along with its line, by deleting a selection that holds the whole line" in {
    val removed = opened.replaceRange(RichTextRange(RichTextPosition(2, 0), RichTextPosition(4, 0)), "")

    savedWord(removed) should not include "<w:tbl>"
    removed.paragraphs.exists(_.isOpaqueBlock) shouldBe false
  }

  "A DOCX with a content control, an equation and an embedded chunk" should "show each as a block of its own kind" in {
    val document = DocxDocumentCodec
      .readBytes(
        docx(
          """<w:p><w:r><w:t>one</w:t></w:r></w:p><w:sdt><w:sdtContent><w:p><w:r><w:t>inside</w:t></w:r></w:p></w:sdtContent></w:sdt><m:oMathPara><m:oMath/></m:oMathPara><w:altChunk/><w:p><w:r><w:t>two</w:t></w:r></w:p><w:sectPr/>"""
        )
      )
      .value

    document.paragraphs.map(_.opaqueBlock.map(_.feature)) shouldBe List(
      None,
      Some(DocumentFeature.ContentControls),
      Some(DocumentFeature.Equations),
      Some(DocumentFeature.Embedded),
      None
    )
    document.exportText should not include "inside"
  }

  it should "keep body-level markers and section properties in place without a line for them" in {
    val xml =
      """<w:p><w:r><w:t>one</w:t></w:r></w:p><w:bookmarkStart w:id="7" w:name="b"/><w:bookmarkEnd w:id="7"/><w:p><w:r><w:t>two</w:t></w:r></w:p><w:sectPr/>"""
    val document = DocxDocumentCodec.readBytes(docx(xml)).value

    document.paragraphs.map(_.plainText) shouldBe List("one", "two")
    entry(
      DocxDocumentCodec.writeBytes(replaceText(document, 0, "one", "uno")),
      "word/document.xml"
    ).text should include(
      """<w:bookmarkStart w:id="7" w:name="b"/><w:bookmarkEnd w:id="7"/>"""
    )
  }

  "A DOCX opened with a table" should "write no table into another format" in {
    val odt = OdtDocumentCodec.writeBytes(opened)

    entry(odt, "content.xml").text should not include "w:tbl"
    val readBack = OdtDocumentCodec.readBytes(odt).value
    readBack.paragraphs.size shouldBe 6
    readBack.paragraphAt(3).map(_.plainText) shouldBe Some("")
  }

  it should "write no table into a package it has no source for" in {
    val xml = savedWord(opened.withSource(None))

    xml should not include "<w:tbl>"
    xml should not include "Region"
  }

  "An ODT table between paragraphs" should "be a read-only line of its own" in {
    val document = openedWriter

    document.paragraphs.size shouldBe 6
    document.paragraphAt(3).flatMap(_.opaqueBlock).map(_.feature) shouldBe Some(DocumentFeature.Tables)
    document.paragraphAt(3).flatMap(_.opaqueBlock).map(_.raw) shouldBe Some(odfTable)
  }

  it should "survive editing the paragraphs around it" in {
    savedWriter(replaceText(openedWriter, 2, "online", "offline")) should include(odfTable)
    savedWriter(replaceText(openedWriter, 5, "Final", "Last")) should include(odfTable)
  }

  it should "be removed from the saved file when its line is deleted" in {
    val xml = savedWriter(deleteLine(openedWriter, 3))

    xml should not include "<table:table "
    xml should include("<draw:image")
  }

  it should "leave declarations that have no content of their own out of the lines" in {
    val content =
      """<?xml version="1.0" encoding="UTF-8"?><office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"><office:body><office:text><text:sequence-decls><text:sequence-decl text:name="Illustration"/></text:sequence-decls><text:p>one</text:p><text:list><text:list-item><text:p>item</text:p></text:list-item></text:list></office:text></office:body></office:document-content>"""
    val archive = GoldenFixtures.zip(
      List(
        "mimetype"    -> "application/vnd.oasis.opendocument.text".getBytes(StandardCharsets.UTF_8),
        "content.xml" -> content.getBytes(StandardCharsets.UTF_8)
      ),
      Set("mimetype")
    )

    val document = OdtDocumentCodec.readBytes(archive).value

    document.paragraphs.map(_.opaqueBlock.map(_.feature)) shouldBe List(None, Some(DocumentFeature.Lists))
    entry(OdtDocumentCodec.writeBytes(replaceText(document, 0, "one", "uno")), "content.xml").text should include(
      "<text:sequence-decls><text:sequence-decl"
    )
  }
