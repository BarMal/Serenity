package com.serenity.io

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.richtext.{DocxDocumentCodec, RichTextDocument}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.{BufferId, CursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A reload replaces the buffer's whole document, so it must not take the fresh read's content version with it: that
  * would move the version backwards, and a stale outline parse stamped earlier could match again (#1935).
  */
class FileManagerReloadSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Reloading a buffer from disk" should "advance its content version past every edit made before" in {
    val path    = Files.writeString(TestTemp.directory("file-manager-reload").resolve("draft.txt"), "saved")
    val manager = FileManager()
    val opened  = manager.loadFile(path, BufferId(1)).unsafeRunSync()
    val edited = opened
      .withEditedContent(Rope("saved!"), List(CursorPosition(0, 6)))
      .withEditedContent(Rope("saved!!"), List(CursorPosition(0, 7)))

    val reloaded = manager.reloadBuffer(edited).unsafeRunSync()

    reloaded.document.content.collect() shouldBe "saved"
    reloaded.document.contentVersion should be > edited.document.contentVersion
  }

  it should "keep a reloaded formatted document in sync with the reloaded text" in {
    val path = TestTemp.directory("file-manager-reload").resolve("draft.docx")
    val _ = Files.write(
      path,
      DocxDocumentCodec.writeBytes(RichTextDocument.fromPlainText("saved"))
    )
    val manager = FileManager()
    val opened  = manager.loadFile(path, BufferId(1)).unsafeRunSync()
    val edited  = opened.withEditedContent(Rope("saved!"), List(CursorPosition(0, 6)), richTextDocument = None)

    manager.reloadBuffer(edited).unsafeRunSync().richTextInSync shouldBe true
  }

end FileManagerReloadSpec
