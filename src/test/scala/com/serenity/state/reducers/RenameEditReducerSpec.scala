package com.serenity.state.reducers

import java.nio.file.Path

import com.serenity.lsp.model.{LspPosition, LspRange, LspTextEdit}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.undo.HistoryEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for [[RenameEditReducer]] (#1467): applying a `textDocument/rename` response's `WorkspaceEdit` to the
  * buffer open in the focused editor pane, and reporting locations this client could not apply because they target a
  * document that isn't open in any buffer.
  */
class RenameEditReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val path     = Path.of("/tmp/serenity-rename-spec/example.scala")
  private val uri      = path.toUri.toString
  private val anchor   = CursorPosition(0, 0)

  private def stateWith(content: String, cursor: CursorPosition = CursorPosition(0, 0)): AppState =
    val buffer = Buffer(bufferId, Document(Rope(content), filePath = Some(path)))
      .copy(editing = EditingState(List(cursor)))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def quickInfoText(state: AppState): Option[String] =
    state.runtime.uiSurfaces.collectFirst { case UiSurface(_, SurfaceContent.QuickInfo(text), _, _) => text }

  "RenameEditReducer" should "apply the response's edits to the focused buffer's matching document" in {
    val before = stateWith("val old = 1")
    val edits = Map(
      uri -> List(LspTextEdit(LspRange(LspPosition(0, 4), LspPosition(0, 7)), "renamed"))
    )

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "val renamed = 1"
    quickInfoText(result.state) shouldBe Some("Renamed 1 location(s) in this file.")
  }

  it should "apply edits right-to-left so an earlier edit's offsets aren't shifted by a later one" in {
    val before = stateWith("val a = a + a")
    val edits = Map(
      uri -> List(
        LspTextEdit(LspRange(LspPosition(0, 4), LspPosition(0, 5)), "renamed"),
        LspTextEdit(LspRange(LspPosition(0, 8), LspPosition(0, 9)), "renamed"),
        LspTextEdit(LspRange(LspPosition(0, 12), LspPosition(0, 13)), "renamed")
      )
    )

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe
      "val renamed = renamed + renamed"
    quickInfoText(result.state) shouldBe Some("Renamed 3 location(s) in this file.")
  }

  private val secondId   = BufferId(1)
  private val secondPath = Path.of("/tmp/serenity-rename-spec/second.scala")
  private val secondUri  = secondPath.toUri.toString

  private def stateWithTwoBuffers(first: String, second: String): AppState =
    val base       = stateWith(first)
    val other      = Buffer(secondId, Document(Rope(second), filePath = Some(secondPath)))
    val secondPane = PaneId(1)
    val layout     = base.persisted.layout
    base.copy(persisted =
      base.persisted.copy(
        buffers = base.persisted.buffers + (secondId -> other),
        layout =
          layout.copy(editorPanes = layout.editorPanes + (secondPane -> EditorPane.withBuffer(secondPane, secondId)))
      )
    )

  private def undoBufferIds(result: ReducerResult): List[BufferId] =
    result.effects.collect {
      case AppEffect.Undo(UndoEffect.RecordBoundary(entry: HistoryEntry.BufferEdit, _)) =>
        entry.bufferId
    }

  it should "apply edits to every open buffer, not only the focused one, and record an undo step for each" in {
    val before = stateWithTwoBuffers("val old = 1", "def f = old")
    val edits = Map(
      uri       -> List(LspTextEdit(LspRange(LspPosition(0, 4), LspPosition(0, 7)), "renamed")),
      secondUri -> List(LspTextEdit(LspRange(LspPosition(0, 8), LspPosition(0, 11)), "renamed"))
    )

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "val renamed = 1"
    result.state.persisted.buffers(secondId).document.content.collect() shouldBe "def f = renamed"
    undoBufferIds(result).toSet shouldBe Set(bufferId, secondId)
    quickInfoText(result.state) shouldBe Some("Renamed 2 location(s) in 2 open file(s).")
  }

  it should "apply edits to an open buffer that is not the focused one when the focused buffer has none" in {
    val before = stateWithTwoBuffers("val old = 1", "def f = old")
    val edits  = Map(secondUri -> List(LspTextEdit(LspRange(LspPosition(0, 8), LspPosition(0, 11)), "renamed")))

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "val old = 1"
    result.state.persisted.buffers(secondId).document.content.collect() shouldBe "def f = renamed"
  }

  it should "leave the buffer untouched and report files that are not open in any buffer" in {
    val before   = stateWith("val old = 1")
    val otherUri = Path.of("/tmp/serenity-rename-spec/other.scala").toUri.toString
    val edits = Map(
      otherUri -> List(LspTextEdit(LspRange(LspPosition(0, 4), LspPosition(0, 7)), "renamed"))
    )

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "val old = 1"
    quickInfoText(result.state) shouldBe Some(
      "Renamed 0 location(s) in this file. 1 file(s) not open in any buffer were not updated."
    )
  }
