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

  private def recordedEntries(result: ReducerResult): List[HistoryEntry] =
    result.effects.collect { case AppEffect.Undo(UndoEffect.RecordBoundary(entry, _)) => entry }

  private def membersOf(entry: HistoryEntry): List[HistoryEntry] = entry match
    case HistoryEntry.Group(entries) => entries
    case single                      => List(single)

  private def undoBufferIds(result: ReducerResult): List[BufferId] =
    recordedEntries(result).flatMap(membersOf).collect {
      case edit: HistoryEntry.BufferEdit         => edit.bufferId
      case edit: HistoryEntry.DetachedBufferEdit => edit.bufferId
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
    recordedEntries(result) should matchPattern { case List(HistoryEntry.Group(_)) => }
    quickInfoText(result.state) shouldBe Some("Renamed 2 location(s) in 2 open file(s).")
  }

  it should "apply edits to an open buffer that is not the focused one when the focused buffer has none" in {
    val before = stateWithTwoBuffers("val old = 1", "def f = old")
    val edits  = Map(secondUri -> List(LspTextEdit(LspRange(LspPosition(0, 8), LspPosition(0, 11)), "renamed")))

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "val old = 1"
    result.state.persisted.buffers(secondId).document.content.collect() shouldBe "def f = renamed"
  }

  it should "leave the open buffer untouched and ask for a file that no buffer holds to be opened" in {
    val before   = stateWith("val old = 1")
    val otherUri = Path.of("/tmp/serenity-rename-spec/other.scala").toUri.toString
    val edits = Map(
      otherUri -> List(LspTextEdit(LspRange(LspPosition(0, 4), LspPosition(0, 7)), "renamed"))
    )

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "val old = 1"
    result.effects.collect {
      case AppEffect.File(FileEffect.OpenForRenameEdit(pending)) =>
        pending.edits.keySet
    } shouldBe List(Set(otherUri))
  }

  it should "apply nothing, in any file, when one document's edits overlap, and say so" in {
    val before = stateWithTwoBuffers("val old = 1", "def f = old")
    val edits = Map(
      uri -> List(LspTextEdit(LspRange(LspPosition(0, 4), LspPosition(0, 7)), "renamed")),
      secondUri -> List(
        LspTextEdit(LspRange(LspPosition(0, 4), LspPosition(0, 9)), "renamed"),
        LspTextEdit(LspRange(LspPosition(0, 8), LspPosition(0, 11)), "renamed")
      )
    )

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "val old = 1"
    result.state.persisted.buffers(secondId).document.content.collect() shouldBe "def f = old"
    undoBufferIds(result) shouldBe Nil
    quickInfoText(result.state) shouldBe Some(
      "Rename not applied: 1 file(s) have overlapping edits (" + secondPath.getFileName + ")."
    )
  }

  it should "record an undo step for an open buffer that no pane shows" in {
    val base   = stateWithTwoBuffers("val old = 1", "def f = old")
    val layout = base.persisted.layout
    val before =
      base.copy(persisted = base.persisted.copy(layout = layout.copy(editorPanes = layout.editorPanes - PaneId(1))))
    val edits = Map(secondUri -> List(LspTextEdit(LspRange(LspPosition(0, 8), LspPosition(0, 11)), "renamed")))

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(secondId).document.content.collect() shouldBe "def f = renamed"
    result.effects.collect {
      case AppEffect.Undo(UndoEffect.RecordBoundary(entry: HistoryEntry.DetachedBufferEdit, _)) =>
        entry.bufferId
    } shouldBe List(secondId)
  }

  private val otherPath = Path.of("/tmp/serenity-rename-spec/unopened.scala")
  private val otherUri  = otherPath.toUri.toString
  private def renameTo(from: Int, to: Int, text: String) =
    LspTextEdit(LspRange(LspPosition(0, from), LspPosition(0, to)), text)

  it should "ask for files that no buffer holds to be opened, with their edits, and show no summary yet" in {
    val before = stateWith("val old = 1")
    val edits  = Map(uri -> List(renameTo(4, 7, "renamed")), otherUri -> List(renameTo(8, 11, "renamed")))

    val result = RenameEditReducer.apply(edits, anchor, before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "val renamed = 1"
    result.effects.collect {
      case AppEffect.File(FileEffect.OpenForRenameEdit(pending)) => pending
    } should matchPattern {
      case List(PendingRename(`unopenedEdits`, `anchor`, 1, 1, 0, _, _, List(_: HistoryEntry.BufferEdit))) =>
    }
    recordedEntries(result) shouldBe Nil
    quickInfoText(result.state) shouldBe None
  }

  private val unopenedEdits = Map(otherUri -> List(renameTo(8, 11, "renamed")))

  "RenameEditReducer.afterOpening" should "edit the files that are now open, summarise the whole rename, and restore focus" in {
    val base      = stateWithTwoBuffers("val old = 1", "def f = old")
    val reopened  = base.copy(persisted = base.persisted.copy(focus = Focus.EditorPane(PaneId(1))))
    val withOther = Path.of("/tmp/serenity-rename-spec/second.scala")
    val pending = PendingRename(
      Map(withOther.toUri.toString -> List(renameTo(8, 11, "renamed"))),
      anchor,
      applied = 1,
      editedBuffers = 1,
      heldBack = 0,
      focus = Focus.EditorPane(PaneId(0)),
      activePane = Some(PaneId(0)),
      undoEntries = Nil
    )

    val result = RenameEditReducer.afterOpening(pending, reopened)

    result.state.persisted.buffers(secondId).document.content.collect() shouldBe "def f = renamed"
    undoBufferIds(result) shouldBe List(secondId)
    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    result.state.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(0))
    quickInfoText(result.state) shouldBe Some("Renamed 2 location(s) in 2 open file(s).")
  }

  it should "report a file that still could not be opened as not updated" in {
    val before  = stateWith("val old = 1")
    val pending = PendingRename(unopenedEdits, anchor, 1, 1, 0, Focus.EditorPane(PaneId(0)), Some(PaneId(0)), Nil)

    val result = RenameEditReducer.afterOpening(pending, before)

    quickInfoText(result.state) shouldBe Some(
      "Renamed 1 location(s) in this file. 1 file(s) not open in any buffer were not updated."
    )
  }

  it should "record one undo step for the open buffer's edit and the opened file's together" in {
    val base  = stateWithTwoBuffers("val old = 1", "def f = old")
    val first = RenameEditReducer.apply(Map(uri -> List(renameTo(4, 7, "renamed"))), anchor, base)
    val queued = PendingRename(
      Map(secondUri -> List(renameTo(8, 11, "renamed"))),
      anchor,
      1,
      1,
      0,
      Focus.EditorPane(PaneId(0)),
      Some(PaneId(0)),
      undoEntries = first.effects.collect { case AppEffect.Undo(UndoEffect.RecordBoundary(entry, _)) => entry }
    )

    val result = RenameEditReducer.afterOpening(queued, first.state)

    recordedEntries(result) should matchPattern { case List(HistoryEntry.Group(List(_, _))) => }
    undoBufferIds(result).toSet shouldBe Set(bufferId, secondId)
  }

  it should "record a plain undo step, not a group, when only one buffer is edited" in {
    val before = stateWith("val old = 1")
    val edits  = Map(uri -> List(renameTo(4, 7, "renamed")))

    recordedEntries(RenameEditReducer.apply(edits, anchor, before)) should matchPattern {
      case List(_: HistoryEntry.BufferEdit) =>
    }
  }
