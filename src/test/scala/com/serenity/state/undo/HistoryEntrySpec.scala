package com.serenity.state.undo

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{SplitAxis, WorkspaceNodeId}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1016 widened `HistoryEntry` from a buffer-only snapshot to a sealed hierarchy so undo could cover more than buffer
  * content; this locks in `restore` for each case directly, independent of the reducer/pipeline wiring that declares
  * and replays them (covered by `EditorUndoEffectSpec`, `UndoRedoSpec`, and the pane-close specs).
  */
class HistoryEntrySpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  "HistoryEntry.BufferEdit.restore" should "restore the snapshotted content and return the pre-restore state as the inverse" in {
    val bufferId = BufferId(0)
    val paneId   = PaneId(0)
    val original = Buffer.fromString(bufferId, "hello")
    val snapshot = BufferSnapshot.fromBuffer(original)
    val edited   = original.copy(document = original.document.copy(content = com.serenity.rope.Rope("hello!")))
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> edited), focus = Focus.EditorPane(PaneId(1)))
    )

    val (restoredState, inverse) = HistoryEntry.BufferEdit(bufferId, paneId, snapshot).restore(state).value

    restoredState.persisted.buffers(bufferId).document.content.collect() shouldBe "hello"
    inverse shouldBe a[HistoryEntry.BufferEdit]
    inverse.asInstanceOf[HistoryEntry.BufferEdit].snapshot shouldBe BufferSnapshot.fromBuffer(edited)
  }

  it should "snap focus to the entry's pane when restoring" in {
    val bufferId = BufferId(0)
    val paneId   = PaneId(0)
    val buffer   = Buffer.fromString(bufferId, "hi")
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer), focus = Focus.EditorPane(PaneId(1)))
    )

    val (restoredState, _) =
      HistoryEntry.BufferEdit(bufferId, paneId, BufferSnapshot.fromBuffer(buffer)).restore(state).value

    restoredState.persisted.focus shouldBe Focus.EditorPane(paneId)
    restoredState.persisted.layout.activeEditorPaneId shouldBe Some(paneId)
  }

  it should "return None when the buffer no longer exists" in {
    val bufferId = BufferId(999)
    val buffer   = Buffer.fromString(bufferId, "hi")
    val entry    = HistoryEntry.BufferEdit(bufferId, PaneId(0), BufferSnapshot.fromBuffer(buffer))

    entry.restore(AppState.initial) shouldBe None
  }

  "HistoryEntry.DetachedBufferEdit.restore" should "restore the snapshotted text and cursors without moving focus" in {
    val bufferId = BufferId(0)
    val original = Buffer.fromString(bufferId, "hello").copy(editing = EditingState(List(CursorPosition(0, 2))))
    val snapshot = BufferSnapshot.fromBuffer(original)
    val edited = original.copy(
      document = original.document.copy(content = com.serenity.rope.Rope("hello!")),
      editing = EditingState(List(CursorPosition(0, 6)))
    )
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> edited), focus = Focus.EditorPane(PaneId(1)))
    )

    val (restoredState, inverse) = HistoryEntry.DetachedBufferEdit(bufferId, snapshot).restore(state).value

    val restored = restoredState.persisted.buffers(bufferId)
    (restored.document.content.collect(), restored.editing.cursorPositions) shouldBe (
      "hello",
      List(CursorPosition(0, 2))
    )
    restoredState.persisted.focus shouldBe Focus.EditorPane(PaneId(1))
    inverse shouldBe HistoryEntry.DetachedBufferEdit(bufferId, BufferSnapshot.fromBuffer(edited))
  }

  it should "return None when the buffer no longer exists" in {
    val buffer = Buffer.fromString(BufferId(999), "hi")

    HistoryEntry
      .DetachedBufferEdit(buffer.id, BufferSnapshot.fromBuffer(buffer))
      .restore(AppState.initial) shouldBe None
  }

  "HistoryEntry.Group.restore" should "restore every member buffer, and its inverse should put the edits back" in {
    val first  = Buffer.fromString(BufferId(0), "one")
    val second = Buffer.fromString(BufferId(1), "two")
    val editedOf = (buffer: Buffer, text: String) =>
      buffer.copy(document = buffer.document.copy(content = com.serenity.rope.Rope(text)))
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(buffers = Map(first.id -> editedOf(first, "ONE"), second.id -> editedOf(second, "TWO")))
    )
    val group = HistoryEntry.Group(
      List(
        HistoryEntry.DetachedBufferEdit(first.id, BufferSnapshot.fromBuffer(first)),
        HistoryEntry.DetachedBufferEdit(second.id, BufferSnapshot.fromBuffer(second))
      )
    )

    val (undone, inverse) = group.restore(state).value
    val (redone, _)       = inverse.restore(undone).value

    def texts(of: AppState) = of.persisted.buffers.values.toList.sortBy(_.id.value).map(_.document.content.collect())
    (texts(undone), texts(redone)) shouldBe (List("one", "two"), List("ONE", "TWO"))
  }

  it should "skip a member whose buffer is gone and restore the rest" in {
    val kept    = Buffer.fromString(BufferId(0), "kept")
    val closed  = Buffer.fromString(BufferId(7), "closed")
    val changed = kept.copy(document = kept.document.copy(content = com.serenity.rope.Rope("changed")))
    val state   = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(kept.id -> changed)))
    val group = HistoryEntry.Group(
      List(
        HistoryEntry.DetachedBufferEdit(closed.id, BufferSnapshot.fromBuffer(closed)),
        HistoryEntry.DetachedBufferEdit(kept.id, BufferSnapshot.fromBuffer(kept))
      )
    )

    val (restored, _) = group.restore(state).value

    restored.persisted.buffers(kept.id).document.content.collect() shouldBe "kept"
  }

  it should "return None when none of its buffers exist" in {
    val gone = Buffer.fromString(BufferId(7), "gone")
    val group =
      HistoryEntry.Group(List(HistoryEntry.DetachedBufferEdit(gone.id, BufferSnapshot.fromBuffer(gone))))

    group.restore(AppState.initial) shouldBe None
  }

  "HistoryEntry.PaneClose.restore" should "restore the pre-removal layout and focus, and capture the current ones as the inverse" in {
    val closedLayout = AppState.initial.persisted.layout
    val closedFocus  = AppState.initial.persisted.focus

    val secondPane = PaneId(1)
    val splitTree = closedLayout.workspaceTree
      .flatMap(
        _.split(
          PaneId(0),
          secondPane,
          SplitAxis.Horizontal,
          WorkspaceNodeId("split-0-1"),
          WorkspaceNodeId("editor-1")
        )
      )
      .value
    val twoPaneLayout = closedLayout.copy(
      editorPanes = closedLayout.editorPanes.updated(secondPane, EditorPane.empty(secondPane)),
      workspaceTree = Some(splitTree),
      activeEditorPaneId = Some(secondPane)
    )
    val twoPaneFocus = Focus.EditorPane(secondPane)
    val stateAfterClose =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(layout = closedLayout, focus = closedFocus))

    val (restoredState, inverse) =
      HistoryEntry.PaneClose(twoPaneLayout, twoPaneFocus).restore(stateAfterClose).value

    restoredState.persisted.layout shouldBe twoPaneLayout
    restoredState.persisted.focus shouldBe twoPaneFocus
    inverse shouldBe HistoryEntry.PaneClose(closedLayout, closedFocus)
  }

  "HistoryEntry.PanelChange.restore" should "restore the pre-change surfaces, workspace tree, and focus, capturing the current ones as the inverse" in {
    val panelSurface = UiSurface.fromPanelContent(
      SurfaceId("outline"),
      com.serenity.ui.layout.PanelContent.Outline(Nil)
    )
    val beforeChange = HistoryEntry.PanelChange(
      uiSurfaces = Nil,
      workspaceTree = AppState.initial.persisted.layout.workspaceTree,
      maximizedWorkspaceNodeId = None,
      focus = AppState.initial.persisted.focus
    )
    val stateAfterPin = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(panelSurface.id)),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(panelSurface))
    )

    val (restoredState, inverse) = beforeChange.restore(stateAfterPin).value

    restoredState.runtime.uiSurfaces shouldBe Nil
    restoredState.persisted.focus shouldBe AppState.initial.persisted.focus
    inverse shouldBe a[HistoryEntry.PanelChange]
    val inversePanelChange = inverse.asInstanceOf[HistoryEntry.PanelChange]
    inversePanelChange.uiSurfaces shouldBe List(panelSurface)
    inversePanelChange.focus shouldBe Focus.Surface(panelSurface.id)
  }

  it should "always succeed, unlike a buffer edit whose target may no longer exist" in {
    val entry = HistoryEntry.PanelChange.capture(AppState.initial)

    entry.restore(AppState.initial) shouldBe defined
  }
