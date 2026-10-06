package com.serenity.manuscript.docx

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import java.util.zip.ZipInputStream
import javax.xml.parsers.DocumentBuilderFactory

import com.serenity.manuscript.{ManuscriptFormat, PaperSize}
import com.serenity.richtext.{DocxDocumentCodec, ParagraphRole}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.w3c.dom.{Document, Element}

class ManuscriptDocxWriterSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val WNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

  private lazy val bytes = ManuscriptDocxWriter.write(ManuscriptDocxFixture.manuscript(), ManuscriptFormat.Modern)

  private def entries(archive: Array[Byte]): List[(String, LocalDateTime, String)] =
    val input = ZipInputStream(ByteArrayInputStream(archive))
    try
      Iterator
        .continually(Option(input.getNextEntry))
        .takeWhile(_.isDefined)
        .flatten
        .map(entry => (entry.getName, entry.getTimeLocal, String(input.readAllBytes(), StandardCharsets.UTF_8)))
        .toList
    finally input.close()

  private def part(name: String, archive: Array[Byte] = bytes): String =
    entries(archive).collectFirst { case (`name`, _, content) => content }.getOrElse(fail(s"missing $name"))

  private def xml(content: String): Document =
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.newDocumentBuilder().parse(ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8)))

  private def elements(document: Document, localName: String): List[Element] =
    val nodes = document.getElementsByTagNameNS(WNs, localName)
    (0 until nodes.getLength).toList.map(nodes.item).collect { case element: Element => element }

  private def golden(name: String): String =
    Files.readString(Path.of(getClass.getResource(s"/export/golden/smf-$name").toURI), StandardCharsets.UTF_8)

  private def canonical(content: String): String =
    content.replace("\r\n", "\n").linesIterator.map(_.stripTrailing).mkString("\n").trim

  "ManuscriptDocxWriter" should "write the package parts in a fixed order with fixed timestamps" in {
    entries(bytes).map(_._1) shouldBe List(
      "[Content_Types].xml",
      "_rels/.rels",
      "docProps/core.xml",
      "word/document.xml",
      "word/styles.xml",
      "word/settings.xml",
      "word/header1.xml",
      "word/_rels/document.xml.rels"
    )
    entries(bytes).map(_._2).distinct shouldBe List(LocalDateTime.of(1980, 1, 1, 0, 0))
  }

  it should "be byte-for-byte deterministic" in {
    ManuscriptDocxWriter.write(ManuscriptDocxFixture.manuscript(), ManuscriptFormat.Modern) shouldBe bytes
  }

  private val goldenParts = List(
    "document.xml"      -> "word/document.xml",
    "styles.xml"        -> "word/styles.xml",
    "settings.xml"      -> "word/settings.xml",
    "header1.xml"       -> "word/header1.xml",
    "content-types.xml" -> "[Content_Types].xml",
    "document.xml.rels" -> "word/_rels/document.xml.rels",
    "package.rels"      -> "_rels/.rels",
    "core.xml"          -> "docProps/core.xml"
  )

  goldenParts.foreach { (goldenName, partName) =>
    it should s"match the golden $partName" in {
      canonical(part(partName)) shouldBe canonical(golden(goldenName))
    }
  }

  it should "write only well-formed XML" in
    entries(bytes).foreach((_, _, content) => noException should be thrownBy xml(content))

  it should "set the standard manuscript defaults in styles.xml" in {
    val styles = xml(part("word/styles.xml"))
    val fonts  = elements(styles, "rFonts").headOption.getOrElse(fail("no rFonts"))
    fonts.getAttributeNS(WNs, "ascii") shouldBe "Times New Roman"
    elements(styles, "sz").map(_.getAttributeNS(WNs, "val")) shouldBe List("24")
    val normal =
      elements(styles, "style").find(_.getAttributeNS(WNs, "styleId") == "Normal").getOrElse(fail("no Normal"))
    val normalSpacing = normal.getElementsByTagNameNS(WNs, "spacing").item(0) match
      case element: Element => element
      case _                => fail("Normal has no spacing")
    normalSpacing.getAttributeNS(WNs, "line") shouldBe "480"
    val indent = normal.getElementsByTagNameNS(WNs, "ind").item(0) match
      case element: Element => element
      case _                => fail("Normal has no indent")
    indent.getAttributeNS(WNs, "firstLine") shouldBe "720"
  }

  it should "give the body a Surname / KEYWORD / page header, page numbers from 1 and none on the title page" in {
    val header = xml(part("word/header1.xml"))
    elements(header, "t").map(_.getTextContent).headOption shouldBe Some("Writer / NIGHT / ")
    elements(header, "instrText").map(_.getTextContent.trim) shouldBe List("PAGE")

    val sections = elements(xml(part("word/document.xml")), "sectPr")
    sections.map(_.getElementsByTagNameNS(WNs, "headerReference").getLength) shouldBe List(0, 1)
    sections.map(_.getElementsByTagNameNS(WNs, "pgNumType").getLength) shouldBe List(0, 1)
    val document = xml(part("word/document.xml"))
    elements(document, "pgSz").map(size => (size.getAttributeNS(WNs, "w"), size.getAttributeNS(WNs, "h"))) shouldBe
      List(("12240", "15840"), ("12240", "15840"))
    elements(document, "pgMar").map(_.getAttributeNS(WNs, "left")) shouldBe List("1440", "1440")
  }

  it should "start every chapter after the first on a new page" in {
    val paragraphs      = elements(xml(part("word/document.xml")), "p")
    val chapterOpenings = paragraphs.filter(_.getTextContent.startsWith("Chapter "))

    chapterOpenings.map(_.getTextContent) shouldBe List("Chapter 1", "Chapter 2")
    chapterOpenings.map(_.getElementsByTagNameNS(WNs, "pageBreakBefore").getLength) shouldBe List(0, 1)
  }

  it should "keep every paragraph's text when the export is read back as a document" in {
    val document = DocxDocumentCodec.readBytes(bytes).value

    document.paragraphs.map(_.plainText) shouldBe List(
      "Jane Q. Writer\tabout 1,000 words",
      "1 High Street",
      "Springfield",
      "jane@example.com",
      "The Long Night",
      "by Jane Q. Writer",
      "For M.",
      "Chapter 1",
      "Arrival",
      "The train was late, and “nobody” said a word.",
      "She waited – quietly – by the door.",
      "#",
      "Morning came.",
      "Chapter 2",
      "The Storm",
      "A quoted letter & a <tag>.",
      "It rained.",
      "END"
    )
    document.paragraphs.filter(_.role == ParagraphRole.Heading(1)).map(_.plainText) shouldBe
      List("Chapter 1", "Arrival", "Chapter 2", "The Storm")
  }

  it should "lay out the classic preset in Courier on A4 when asked" in {
    val classic = ManuscriptFormat.Classic.copy(paper = PaperSize.A4)
    val archive = ManuscriptDocxWriter.write(ManuscriptDocxFixture.manuscript(classic), classic)

    elements(xml(part("word/styles.xml", archive)), "rFonts").map(_.getAttributeNS(WNs, "ascii")) shouldBe
      List("Courier New")
    elements(xml(part("word/document.xml", archive)), "pgSz").map(_.getAttributeNS(WNs, "w")).distinct shouldBe
      List("11906")
    part("word/document.xml", archive) should include("""<w:u w:val="single"/>""")
  }
