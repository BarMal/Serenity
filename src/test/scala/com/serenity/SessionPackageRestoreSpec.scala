package com.serenity

import java.nio.file.{Files, Path}

import _root_.io.circe.syntax.*
import cats.effect.unsafe.implicits.global
import com.serenity.io.FileManager
import com.serenity.richtext.*
import com.serenity.richtext.RichTextTestPackages.{entry, replaceText}
import com.serenity.rope.{Balance, Rope}
import com.serenity.session.{SessionBuffer, given}
import com.serenity.state.models.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A restored rich document is linked to the package it was read from again, so saving it still writes the table, the
  * styles and every untouched paragraph back (#1896).
  */
class SessionPackageRestoreSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val original = GoldenFixtures.zip(GoldenFixtures.wordReport.entries)

  private def withDocx[A](body: (Path, FileManager) => A): A =
    val file = Files.createTempFile("session-package", ".docx")
    try
      Files.write(file, original)
      body(file, new FileManager())
    finally Files.deleteIfExists(file)

  private def edited(buffer: Buffer, find: String, replacement: String): Buffer =
    val document = buffer.richText.richTextDocument.value
    val changed  = replaceText(document, 1, find, replacement)
    val dirty    = buffer.withEditedDocument(Rope(changed.plainText), Some(changed))
    dirty.copy(document = dirty.document.copy(isDirty = true))

  private def throughSession(buffer: Buffer): Buffer =
    val json     = SessionBuffer.fromBuffer(buffer).asJson
    val restored = json.as[SessionBuffer].toOption.value
    SessionBuffer.toBufferIO(restored).unsafeRunSync()

  "A dirty DOCX buffer restored from the session" should "save the table, styles and paragraphs of its package" in
    withDocx { (file, manager) =>
      val dirty    = edited(manager.loadFile(file, BufferId(1)).unsafeRunSync(), "first", "edited")
      val restored = throughSession(dirty)

      restored.richText.richTextDocument.value.source should not be empty
      manager.saveBuffer(restored).unsafeRunSync()

      val saved = Files.readAllBytes(file)
      val xml   = entry(saved, "word/document.xml").text
      xml should include("<w:tbl>")
      xml should include("edited body paragraph")
      xml should include("""<w:keepNext/>""")
      entry(saved, "word/styles.xml") shouldBe entry(original, "word/styles.xml")
      entry(saved, "word/comments.xml") shouldBe entry(original, "word/comments.xml")
    }

  it should "still link to the package after a save and a further edit" in
    withDocx { (file, manager) =>
      val first = manager
        .saveBuffer(edited(manager.loadFile(file, BufferId(1)).unsafeRunSync(), "first", "edited"))
        .unsafeRunSync()
      val dirty = edited(first, "edited", "revised")

      val restored = throughSession(dirty)
      manager.saveBuffer(restored).unsafeRunSync()

      val saved = Files.readAllBytes(file)
      val xml   = entry(saved, "word/document.xml").text
      xml should include("<w:tbl>")
      xml should include("revised body paragraph")
      xml should include("""<w:keepNext/>""")
      entry(saved, "word/styles.xml") shouldBe entry(original, "word/styles.xml")
    }

  it should "refuse an in-place save, naming what would be lost, when the file changed since it was read" in
    withDocx { (file, manager) =>
      val dirty = edited(manager.loadFile(file, BufferId(1)).unsafeRunSync(), "first", "edited")
      Files.write(file, GoldenFixtures.zip(GoldenFixtures.wordReport.entries.filterNot(_._1 == "docProps/core.xml")))

      val restored = throughSession(dirty)

      restored.richText.richTextDocument.value.source shouldBe empty
      restored.richText.richTextFidelity.value.wouldDrop.map(_.feature) should contain(DocumentFeature.Tables)
      restored.richText.richTextFidelity.value.dropSummary should include("style sheet")
      restored.richText.richTextFidelity.value.dropSummary should not include "document part"
      manager.saveBuffer(restored).attempt.unsafeRunSync().swap.toOption.value.getMessage should include("would drop")
    }

  "A paragraph in a session file" should "remember only the block it came from" in
    withDocx { (file, manager) =>
      val buffer = manager.loadFile(file, BufferId(1)).unsafeRunSync()
      val json   = SessionBuffer.fromBuffer(buffer).asJson.noSpaces

      json should include(""""block":0""")
      json should not include "openTagAttributes"
      json should not include "1A2B3C01"
    }
