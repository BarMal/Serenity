package com.serenity.manuscript.layout

import com.serenity.manuscript.docx.{ManuscriptDocxFixture, ManuscriptDocxWriter}
import com.serenity.manuscript.typography.{FontFamily, PageTypography}
import com.serenity.manuscript.{ManuscriptFormat, ManuscriptPageNumbering}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The DOCX writer and the paginator share [[ManuscriptPageNumbering]]; these examples hold them to it end to end. */
class ManuscriptNumberingAgreementSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val FieldRun = "<w:r><w:fldChar w:fldCharType=\"begin\"/>.*?<w:fldChar w:fldCharType=\"end\"/></w:r>".r

  private def docxParts(format: ManuscriptFormat): Map[String, String] =
    ManuscriptDocxWriter.parts(ManuscriptDocxFixture.manuscript(format), format).toMap

  /** The header as Word would print it on page `number`. */
  private def docxHeadOn(format: ManuscriptFormat, number: Int): String =
    FieldRun
      .replaceAllIn(docxParts(format)("word/header1.xml"), number.toString)
      .replaceAll("<[^>]+>", "")
      .replace("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>", "")
      .trim

  private def paginated(format: ManuscriptFormat): PagedDocument =
    Paginator
      .paginate(
        ManuscriptDocxFixture.manuscript(format),
        PageTypography.fromFormat(format, FontFamily.CourierPrime),
        PaginatorFixture.measurer
      )
      .fold(error => sys.error(error.message), identity)

  for format <- ManuscriptFormat.all do

    s"the ${format.key} format" should "print the same running head on every body page in DOCX and in the paginator" in {
      val body = paginated(format).pages.filterNot(_.kind.isFront)

      body.size should be > 1
      body.foreach(page => page.head.map(_.text) shouldBe Some(docxHeadOn(format, page.printedNumber.value)))
    }

    it should "start numbering at the same number on the first page of body text" in {
      val start = """<w:pgNumType w:start="(\d+)"/>""".r
        .findFirstMatchIn(docxParts(format)("word/document.xml"))
        .map(_.group(1).toInt)

      start shouldBe Some(ManuscriptPageNumbering.FirstBodyPage)
      paginated(format).pages.flatMap(_.printedNumber).headOption shouldBe start
    }

    it should "give front matter no head and no number in either output" in {
      val document = docxParts(format)("word/document.xml")
      val frontEnd = document.indexOf("</w:sectPr>")

      document.substring(0, frontEnd) should not include "headerReference"
      document.substring(frontEnd) should include("headerReference")
      val front = paginated(format).pages.filter(_.kind.isFront)
      front.map(_.kind) shouldBe Vector(PageKind.Title, PageKind.Dedication)
      front.map(page => (page.head, page.printedNumber)) shouldBe Vector((None, None), (None, None))
    }
