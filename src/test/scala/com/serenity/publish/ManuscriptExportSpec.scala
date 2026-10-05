package com.serenity.publish

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.manuscript.{CompileError, SourceDocument}
import com.serenity.richtext.{DocxDocumentCodec, RichTextDocument}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ManuscriptExportSpec extends AnyFlatSpec with Matchers with EitherValues:

  private def readBack(bytes: Array[Byte]): List[String] =
    DocxDocumentCodec.readBytes(bytes).value.paragraphs.map(_.plainText)

  "ManuscriptExport.suggestedFileName" should "never suggest the draft's own name" in {
    ManuscriptExport.suggestedFileName(Some(Path.of("books", "novel.md"))) shouldBe "novel-manuscript.docx"
    ManuscriptExport.suggestedFileName(Some(Path.of("draft.docx"))) shouldBe "draft-manuscript.docx"
    ManuscriptExport.suggestedFileName(None) shouldBe "manuscript.docx"
  }

  "ManuscriptExport" should "export the snapshot alone, titled after its file, when there is no manuscript.conf" in {
    val directory = Files.createTempDirectory("manuscript-export-single")
    val origin    = ExportOrigin(Some(directory.resolve("novel.md")), SourceDocument.Markdown("# One\n\nUnsaved text."))

    val paragraphs = readBack(ManuscriptExport.compiledDocx(origin).unsafeRunSync())

    paragraphs should contain allOf ("novel", "One", "Unsaved text.")
  }

  it should "compile the included sources a manuscript.conf lists, in order, taking the origin from its snapshot" in {
    val directory = Files.createTempDirectory("manuscript-export-set")
    Files.writeString(directory.resolve("01.md"), "# Arrival\n\nFrom disk.")
    Files.writeString(directory.resolve("02.md"), "Stale text on disk.")
    Files.writeString(directory.resolve("notes.md"), "# Notes\n\nNever exported.")
    DocxDocumentCodec
      .write(RichTextDocument.fromPlainText("From a Word file."), directory.resolve("03.docx"))
      .unsafeRunSync()
    Files.writeString(
      directory.resolve("manuscript.conf"),
      """title = "The Long Night"
        |author = "Jane Q. Writer"
        |chapter-heading = "Chapter <$n>"
        |sources = [{ path = "01.md" }, { path = "notes.md", exclude = true }, { path = "02.md" }, { path = "03.docx" }]
        |""".stripMargin
    )
    val origin = ExportOrigin(Some(directory.resolve("02.md")), SourceDocument.Markdown("Edited, unsaved."))

    val paragraphs = readBack(ManuscriptExport.compiledDocx(origin).unsafeRunSync())

    paragraphs should contain inOrder (
      "The Long Night",
      "Chapter 1",
      "From disk.",
      "Chapter 2",
      "Edited, unsaved.",
      "Chapter 3",
      "From a Word file."
    )
    paragraphs should not contain "Never exported."
    paragraphs should not contain "Stale text on disk."
  }

  it should "fail with the compile error when manuscript.conf is invalid" in {
    val directory = Files.createTempDirectory("manuscript-export-invalid")
    Files.writeString(directory.resolve("manuscript.conf"), "format = fancy")
    val origin = ExportOrigin(Some(directory.resolve("novel.md")), SourceDocument.Markdown("Text."))

    val failure = ManuscriptExport.compiledDocx(origin).attempt.unsafeRunSync().left.value

    failure shouldBe a[ManuscriptExportException]
    failure.getMessage shouldBe CompileError.InvalidConfiguration("unknown format 'fancy'").message
  }

  it should "write the DOCX to the target" in {
    val directory = Files.createTempDirectory("manuscript-export-write")
    val target    = directory.resolve("out.docx")

    ManuscriptExport.writeDocx(ExportOrigin(None, SourceDocument.Markdown("Text.")), target).unsafeRunSync()

    readBack(Files.readAllBytes(target)) should contain("Text.")
  }
