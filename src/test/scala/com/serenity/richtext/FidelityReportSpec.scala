package com.serenity.richtext

import java.nio.charset.StandardCharsets

import com.serenity.richtext.DocumentFeature.*
import com.serenity.richtext.Treatment.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1896: what a save would do with each feature, instead of one yes-or-no for the whole file. */
class FidelityReportSpec extends AnyFlatSpec with Matchers with EitherValues:
  private val word: Array[Byte] = GoldenFixtures.zip(GoldenFixtures.wordReport.entries)
  private val writer: Array[Byte] =
    GoldenFixtures.zip(GoldenFixtures.writerNotes.entries, Set("mimetype"))

  private def opened: RichTextDocument       = DocxDocumentCodec.readBytes(word).value
  private def openedWriter: RichTextDocument = OdtDocumentCodec.readBytes(writer).value

  private val plainWord: Array[Byte] = GoldenFixtures.zip(
    GoldenFixtures.wordReport.entries.map {
      case ("word/document.xml", bytes) =>
        val text = String(bytes, StandardCharsets.UTF_8)
        val from = text.indexOf("<w:tbl>")
        val to   = text.indexOf("</w:tbl>") + "</w:tbl>".length
        "word/document.xml" -> (text.substring(0, from) + text.substring(to)).getBytes(StandardCharsets.UTF_8)
      case other => other
    }
  )

  "A Word file with styles, a comment and an image but no table" should "report nothing that a save would drop" in {
    val imported = DocxDocumentCodec.readBytesWithFidelity(plainWord).value

    imported.fidelity.wouldDrop shouldBe empty
    imported.fidelity.summary shouldBe ""
  }

  "A Word file with a table" should "report the table as preserved read-only and nothing to drop" in {
    val imported = DocxDocumentCodec.readBytesWithFidelity(word).value

    imported.fidelity.wouldDrop shouldBe empty
    imported.fidelity.count(Tables, ReadOnly) shouldBe 1
    imported.fidelity.summary shouldBe "1 table preserved read-only"
  }

  it should "keep its image, styles and comments without mentioning them in the summary" in {
    val report = FidelityReport.forSave(opened, SaveTarget.Docx)

    report.items should contain(FidelityItem(Images, Preserved, 1))
    report.items should contain(FidelityItem(Styles, Preserved, 1))
    report.items should contain(FidelityItem(Comments, Preserved, 1))
    report.summary should not include "image"
  }

  it should "report the table as dropped when saved as RTF" in {
    val report = FidelityReport.forSave(opened, SaveTarget.Rtf)

    report.wouldDrop should contain(FidelityItem(Tables, Dropped, 1))
    report.count(Tables, ReadOnly) shouldBe 0
    report.dropSummary should startWith("1 table")
  }

  it should "report the table, image and style sheet as dropped when saved as ODT" in {
    val report = FidelityReport.forSave(opened, SaveTarget.Odt)

    report.wouldDrop should contain allOf (
      FidelityItem(Tables, Dropped, 1),
      FidelityItem(Images, Dropped, 1),
      FidelityItem(
        Styles,
        Dropped,
        1
      )
    )
  }

  it should "report the table as dropped when saved as Markdown or plain text" in {
    FidelityReport.forSave(opened, SaveTarget.Markdown).wouldDrop should contain(FidelityItem(Tables, Dropped, 1))
    FidelityReport.forSave(opened, SaveTarget.PlainText).wouldDrop should contain(FidelityItem(Tables, Dropped, 1))
  }

  it should "report headings as converted when saved as RTF and not when saved as DOCX" in {
    FidelityReport.forSave(opened, SaveTarget.Rtf).items should contain(FidelityItem(Headings, Converted, 1))
    FidelityReport.forSave(opened, SaveTarget.Docx).items.map(_.treatment) should not contain Converted
  }

  it should "report the table as removed, and no longer preserved, once its line is deleted" in {
    val removed = opened.replaceRange(RichTextRange(RichTextPosition(3, 0), RichTextPosition(3, 1)), "")

    val report = FidelityReport.forSave(removed, SaveTarget.Docx)

    report.count(Tables, Removed) shouldBe 1
    report.count(Tables, ReadOnly) shouldBe 0
    report.wouldDrop shouldBe empty
    report.summary shouldBe "1 table removed"
  }

  it should "report the table as dropped from a save in place when the package it came from is gone" in {
    val report = FidelityReport.forSave(opened.withSource(None), SaveTarget.Docx)

    report.wouldDrop should contain(FidelityItem(Tables, Dropped, 1))
  }

  "An ODT file with a table" should "report the table as preserved read-only and nothing to drop" in {
    val imported = OdtDocumentCodec.readBytesWithFidelity(writer).value

    imported.fidelity.wouldDrop shouldBe empty
    imported.fidelity.summary shouldBe "1 table preserved read-only"
  }

  it should "report the table as dropped when saved as DOCX" in {
    FidelityReport.forSave(openedWriter, SaveTarget.Docx).wouldDrop should contain(FidelityItem(Tables, Dropped, 1))
  }

  "A document that was never imported" should "report nothing" in {
    FidelityReport.forSave(RichTextDocument.oneParagraph("hello"), SaveTarget.Rtf) shouldBe FidelityReport.empty
  }

  "Listing items" should "join them with commas and a final and" in {
    FidelityReport.listed(List(FidelityItem(Tables, Dropped, 1))) shouldBe "1 table"
    FidelityReport.listed(List(FidelityItem(Tables, Dropped, 1), FidelityItem(Images, Dropped, 2))) shouldBe
      "1 table and 2 images"
    FidelityReport.listed(
      List(FidelityItem(Tables, Dropped, 2), FidelityItem(Images, Dropped, 1), FidelityItem(Fields, Dropped, 3))
    ) shouldBe "2 tables, 1 image and 3 fields"
  }

  "The summary" should "name each treatment's items in turn" in {
    val report = FidelityReport(
      List(
        FidelityItem(Tables, ReadOnly, 1),
        FidelityItem(Headings, Converted, 2),
        FidelityItem(Images, Dropped, 1),
        FidelityItem(Equations, Removed, 1)
      )
    )

    report.summary shouldBe
      "1 table preserved read-only; 2 headings converted; 1 image dropped; 1 equation removed"
  }
