package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.io.FileManager
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Saving a document with read-only block lines (a table kept from a DOCX): what is written, and what the buffer shows
  * afterwards (#1896).
  */
class RichDocumentBlockSaveSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "A DOCX import with a table" should "be saved over its file, keeping the table and the other parts, keeping the table and the other parts" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-table-source", ".docx")

    try
      val original = GoldenFixtures.zip(GoldenFixtures.wordReport.entries)
      Files.write(sourceFile, original)

      val buffer = fileManager.loadFile(sourceFile, BufferId(108)).unsafeRunSync()

      buffer.richText.richTextFidelity.map(_.summary) shouldBe Some("1 table preserved read-only")
      val document = buffer.richText.richTextDocument.getOrElse(fail("no rich document"))
      val edited   = RichTextTestPackages.replaceText(document, 1, "first", "edited")
      val dirty    = buffer.withEditedDocument(com.serenity.rope.Rope(edited.plainText), Some(edited))

      fileManager.saveBuffer(dirty).unsafeRunSync()

      val saved = Files.readAllBytes(sourceFile)
      val xml   = RichTextTestPackages.entry(saved, "word/document.xml").text
      xml should include("<w:tbl>")
      xml should include("edited body paragraph")
      RichTextTestPackages.entry(saved, "word/styles.xml") shouldBe
        RichTextTestPackages.entry(original, "word/styles.xml")
    finally Files.deleteIfExists(sourceFile)
  }

  it should "require Save As before replacing a file with a document that would drop its table" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-lossy-source", ".docx")
    val savedFile   = Files.createTempFile("serenity-lossy-copy", ".rtf")

    try
      Files.write(sourceFile, GoldenFixtures.zip(GoldenFixtures.wordReport.entries))
      val sourceBytes = Files.readAllBytes(sourceFile)

      val loaded   = fileManager.loadFile(sourceFile, BufferId(109)).unsafeRunSync()
      val document = loaded.richText.richTextDocument.getOrElse(fail("no rich document")).withSource(None)
      val buffer = loaded.copy(richText =
        loaded.richText
          .withSyncedDocument(Some(document), loaded.document.contentVersion)
          .copy(richTextFidelity = Some(FidelityReport.forSave(document, SaveTarget.Docx)))
      )

      fileManager.saveBuffer(buffer).attempt.unsafeRunSync().left.map(_.getMessage) shouldBe Left(
        s"Saving $sourceFile would drop 1 table and 1 image. Use Save As to write a new file."
      )
      Files.readAllBytes(sourceFile) shouldBe sourceBytes

      val saved = fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()

      saved.document.filePath shouldBe Some(savedFile)
      saved.richText.richTextFidelity shouldBe None
      String(Files.readAllBytes(savedFile), "ISO-8859-1") should not include "Region"
    finally
      Files.deleteIfExists(sourceFile)
      Files.deleteIfExists(savedFile)
  }

  it should "turn the block lines of a buffer into empty lines once saved in a format that cannot hold them" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-convert-source", ".docx")
    val savedRtf    = Files.createTempFile("serenity-convert-copy", ".rtf")
    val savedOdt    = Files.createTempFile("serenity-convert-copy", ".odt")

    try
      Files.write(sourceFile, GoldenFixtures.zip(GoldenFixtures.wordReport.entries))
      val buffer = fileManager.loadFile(sourceFile, BufferId(110)).unsafeRunSync()
      val lines  = buffer.document.content.lineCount

      List(savedRtf, savedOdt).foreach { target =>
        val saved = fileManager.saveBuffer(buffer, target).unsafeRunSync()

        saved.document.content.collect() should not include InlineAtom.BlockCharacter.toString
        saved.document.content.lineCount shouldBe lines
        saved.richText.richTextDocument.map(_.paragraphs.exists(_.isOpaqueBlock)) shouldBe Some(false)
        saved.richTextInSync shouldBe true
      }
      buffer.richText.richTextDocument.map(_.paragraphs.exists(_.isOpaqueBlock)) shouldBe Some(true)
    finally
      Files.deleteIfExists(sourceFile)
      Files.deleteIfExists(savedRtf)
      Files.deleteIfExists(savedOdt)
  }

  it should "write no block placeholder to Markdown or plain text and leave the block line empty in the buffer" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-text-source", ".docx")
    val savedMd     = Files.createTempFile("serenity-text-copy", ".md")
    val savedTxt    = Files.createTempFile("serenity-text-copy", ".txt")

    try
      Files.write(sourceFile, GoldenFixtures.zip(GoldenFixtures.wordReport.entries))
      val buffer = fileManager.loadFile(sourceFile, BufferId(112)).unsafeRunSync()
      val lines  = buffer.document.content.lineCount

      List(savedMd, savedTxt).foreach { target =>
        val saved = fileManager.saveBuffer(buffer, target).unsafeRunSync()
        val bytes = Files.readAllBytes(target)

        markersIn(String(bytes, "UTF-8")) shouldBe Nil
        markersIn(saved.document.content.collect()) shouldBe Nil
        String(bytes, "UTF-8") shouldBe saved.document.content.collect()
        saved.document.content.lineCount shouldBe lines
        saved.richText.richTextDocument shouldBe None

        fileManager.saveBuffer(saved).unsafeRunSync()
        Files.readAllBytes(target) shouldBe bytes
      }
    finally
      Files.deleteIfExists(sourceFile)
      Files.deleteIfExists(savedMd)
      Files.deleteIfExists(savedTxt)
  }

  it should "write a soft break as a line break and no internal marker to Markdown or plain text" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-break-source", ".docx")
    val savedMd     = Files.createTempFile("serenity-break-copy", ".md")
    val savedTxt    = Files.createTempFile("serenity-break-copy", ".txt")
    val body        = """<w:p><w:r><w:t>a</w:t><w:br/><w:t>b</w:t></w:r></w:p>"""
    val xml =
      s"""<?xml version="1.0" encoding="UTF-8"?><w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>$body</w:body></w:document>"""

    try
      Files.write(
        sourceFile,
        GoldenFixtures.zip(List("word/document.xml" -> xml.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
      )
      val buffer = fileManager.loadFile(sourceFile, BufferId(113)).unsafeRunSync()

      fileManager.saveBuffer(buffer, savedTxt).unsafeRunSync()
      fileManager.saveBuffer(buffer, savedMd).unsafeRunSync()

      String(Files.readAllBytes(savedTxt), "UTF-8") shouldBe "a\nb"
      String(Files.readAllBytes(savedMd), "UTF-8") shouldBe "a\\\nb"
      List(savedTxt, savedMd).foreach(target => markersIn(String(Files.readAllBytes(target), "UTF-8")) shouldBe Nil)
    finally
      Files.deleteIfExists(sourceFile)
      Files.deleteIfExists(savedMd)
      Files.deleteIfExists(savedTxt)
  }

  it should "keep the block lines of a buffer saved in the format it was read from" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-keep-source", ".docx")
    val savedCopy   = Files.createTempFile("serenity-keep-copy", ".docx")

    try
      Files.write(sourceFile, GoldenFixtures.zip(GoldenFixtures.wordReport.entries))
      val buffer = fileManager.loadFile(sourceFile, BufferId(111)).unsafeRunSync()

      val saved = fileManager.saveBuffer(buffer, savedCopy).unsafeRunSync()

      saved.richText.richTextDocument.map(_.paragraphs.exists(_.isOpaqueBlock)) shouldBe Some(true)
      saved.document.content.collect() shouldBe buffer.document.content.collect()
    finally
      Files.deleteIfExists(sourceFile)
      Files.deleteIfExists(savedCopy)
  }

  private def markersIn(text: String): List[Char] =
    List(InlineAtom.BlockCharacter, InlineAtom.OpaqueCharacter, InlineAtom.SoftBreakCharacter).filter(text.contains(_))
