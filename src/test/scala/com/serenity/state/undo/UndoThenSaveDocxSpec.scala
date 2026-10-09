package com.serenity.state.undo

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.io.FileManager
import com.serenity.keystroke.events.InsertChar
import com.serenity.richtext.{DocxDocumentCodec, InlineMark, RichTextDocument, RichTextPosition, RichTextRange}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, EditorEventReducer, UndoEffect}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1935's end-to-end case: typing into a formatted DOCX, undoing it and saving must write the document as it was
  * opened -- the undo has to bring the formatting back with the text, not leave the typed version's runs behind.
  */
class UndoThenSaveDocxSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private val boldHello =
    RichTextDocument
      .fromPlainText("hello world")
      .toggleMark(RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 5)), InlineMark.Bold)

  private def stateWith(buffer: Buffer): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  "Undoing typing in a formatted DOCX and saving" should "write back the document as it was opened" in {
    val path    = TestTemp.directory("serenity-undo-docx").resolve("draft.docx")
    val _       = Files.write(path, DocxDocumentCodec.writeBytes(boldHello))
    val manager = FileManager()
    val opened  = manager.loadFile(path, bufferId).unsafeRunSync()
    val atEnd   = opened.copy(editing = EditingState(List(CursorPosition(0, 11))))

    val typed    = EditorEventReducer.reduce(InsertChar('!'), paneId, stateWith(atEnd))
    val boundary = typed.effects.collectFirst { case AppEffect.Undo(recorded: UndoEffect.RecordBoundary) => recorded }
    val (undone, _) = boundary.value.entry.restore(typed.state).value
    val _           = manager.saveBuffer(undone.persisted.buffers(bufferId)).unsafeRunSync()

    val saved = DocxDocumentCodec.readBytes(Files.readAllBytes(path))
    saved.map(_.plainText) shouldBe Right("hello world")
    saved shouldBe DocxDocumentCodec.readBytes(DocxDocumentCodec.writeBytes(boldHello))
  }

end UndoThenSaveDocxSpec
