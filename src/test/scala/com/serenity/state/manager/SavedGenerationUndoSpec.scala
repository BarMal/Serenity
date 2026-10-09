package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, UndoState}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A save result merged by `FileResults.saved` moves what "clean" means, so undo must agree with it (#1930). */
class SavedGenerationUndoSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)
  private val path     = Path.of("/workspace/notes.txt")

  private val opened: Buffer =
    val buffer = Buffer.fromString(bufferId, "alpha")
    buffer.copy(document = buffer.document.copy(filePath = Some(path)))

  private def stateWith(buffer: Buffer): AppState =
    val initial = AppState.initial
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  private def withText(buffer: Buffer, text: String): Buffer =
    buffer.copy(document = buffer.document.withContent(Rope(text)))

  private val editedBuffer = withText(opened, "alpha!")
  private val editedStep   = HistoryEntry.BufferEdit(bufferId, paneId, BufferSnapshot.fromBuffer(opened))

  private def undone(state: AppState): Buffer =
    UndoRecording
      .undone(Model(state, UndoState().pushUndo(editedStep)))
      .value
      .app
      .persisted
      .buffers(bufferId)

  "Undo to the pre-edit text after a save of the edited text" should "leave the buffer dirty" in {
    val save   = FileSave(bufferId, path, editedBuffer, SaveKind.Save, writtenAtSubmit = 0L)
    val merged = FileResults.saved(stateWith(editedBuffer), save, editedBuffer)

    merged.persisted.buffers(bufferId).document.isDirty shouldBe false
    undone(merged).document.isDirty shouldBe true
  }

  "Undo to the pre-edit text after a save that finished while the text moved on" should "leave the buffer dirty" in {
    val written = FileSave(bufferId, path, editedBuffer, SaveKind.Save, writtenAtSubmit = 0L)
    val typed   = withText(editedBuffer, "alpha!?")
    val merged  = FileResults.saved(stateWith(typed), written, editedBuffer)

    merged.persisted.buffers(bufferId).document.isDirty shouldBe true
    undone(merged).document.isDirty shouldBe true
  }

  "Undo to the pre-edit text with no save in between" should "return the buffer to clean" in {
    undone(stateWith(editedBuffer)).document.isDirty shouldBe false
  }
