package com.serenity

import java.nio.file.Files

import _root_.io.circe.syntax.*
import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.session.given
import com.serenity.state.models.*
import com.serenity.ui.layout.{
  Layout,
  PanelContent,
  PanelPosition,
  SplitAxis,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SessionStateLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "SessionState" should "survive a multi-pane multi-buffer layout round trip" in {
    val file1 = Files.createTempFile("session-multi-pane-1", ".txt")
    val file2 = Files.createTempFile("session-multi-pane-2", ".txt")
    Files.writeString(file1, "pane one content")
    Files.writeString(file2, "pane two content")

    val buffer1 = Buffer.fromFile(BufferId(30), file1, "pane one content")
    val buffer2 = Buffer.fromFile(BufferId(31), file2, "pane two content")
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer1.id -> buffer1, buffer2.id -> buffer2),
        bufferOrder = List(buffer1.id, buffer2.id),
        layout = Layout(
          editorPanes = Map(
            PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer1.id),
            PaneId(1) -> EditorPane.withBuffer(PaneId(1), buffer2.id)
          ),
          activeEditorPaneId = Some(PaneId(1)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0), PaneId(1)))
        ),
        focus = Focus.EditorPane(PaneId(1))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(32),
        nextPaneId = PaneId(2)
      )
    )

    val restored = SessionState.toAppState(SessionState.fromAppState(appState), Theme.default)

    restored.persisted.buffers should have size 2
    restored.persisted.buffers(buffer1.id).document.content.toString shouldBe "pane one content"
    restored.persisted.buffers(buffer2.id).document.content.toString shouldBe "pane two content"
    restored.persisted.layout.editorPanes should have size 2
    restored.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(1))
    restored.persisted.focus shouldBe Focus.EditorPane(PaneId(1))
    restored.persisted.bufferOrder shouldBe List(buffer1.id, buffer2.id)
  }

  it should "serialize buffers and panes in their canonical order" in {
    val buffer1 = Buffer.fromString(BufferId(1), "one")
    val buffer2 = Buffer.fromString(BufferId(2), "two")
    val pane1   = EditorPane.withBuffer(PaneId(1), buffer1.id)
    val pane2   = EditorPane.withBuffer(PaneId(2), buffer2.id)
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer1.id -> buffer1, buffer2.id -> buffer2),
        bufferOrder = List(buffer2.id, buffer1.id),
        layout = Layout(
          editorPanes = Map(pane1.id -> pane1, pane2.id -> pane2),
          activeEditorPaneId = Some(pane2.id),
          workspaceTree = Some(TestWorkspaceTrees.linear(pane2.id, pane1.id))
        )
      )
    )

    val sessionState = SessionState.fromAppState(appState)

    sessionState.buffers.map(_.id) shouldBe List(2, 1)
    sessionState.layout.editorPanes.map(_.id) shouldBe List(2, 1)
  }

  it should "preserve pane split axis through round trip" in {
    val pane1 = EditorPane.empty(PaneId(1))
    val pane2 = EditorPane.empty(PaneId(2))
    val tree = WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("editors"),
        SplitAxis.Vertical,
        0.5,
        WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${pane1.id.value}"), pane1.id),
        WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${pane2.id.value}"), pane2.id)
      )
    )
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = Map(pane1.id -> pane1, pane2.id -> pane2),
          activeEditorPaneId = Some(pane2.id),
          workspaceTree = Some(tree)
        ),
        focus = Focus.EditorPane(pane2.id)
      )
    )

    val sessionState = SessionState.fromAppState(appState)
    val restored     = SessionState.toAppState(sessionState, Theme.default)

    restored.persisted.layout.workspaceTree.flatMap(_.root.axis) shouldBe Some(SplitAxis.Vertical)
  }

  it should "round-trip nested workspace trees, docked panels on every edge, and maximisation" in {
    val pane0     = PaneId(0)
    val pane1     = PaneId(1)
    val positions = List(PanelPosition.Left, PanelPosition.Right, PanelPosition.Top, PanelPosition.Bottom)
    val panels = positions.zipWithIndex.map { (_, index) =>
      UiSurface.fromPanelContent(SurfaceId(s"surface-$index"), PanelContent.Diagnostics(Nil))
    }
    val editorTree = WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("editors"),
        SplitAxis.Vertical,
        0.4,
        WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), pane0),
        WorkspaceNode.Leaf(WorkspaceNodeId("editor-1"), pane1)
      )
    )
    val workspaceTree = panels.zip(positions).zipWithIndex.foldLeft(editorTree) {
      case (tree, ((panel, position), index)) =>
        tree
          .dock(
            panel.id,
            position,
            WorkspaceNodeId(s"dock-split-$index"),
            WorkspaceNodeId(s"dock-leaf-$index")
          )
          .getOrElse(fail(s"Panel $index should dock"))
    }
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = Map(
            pane0 -> EditorPane.withBuffer(pane0, BufferId(0)),
            pane1 -> EditorPane.empty(pane1)
          ),
          activeEditorPaneId = Some(pane0),
          workspaceTree = Some(workspaceTree),
          maximizedWorkspaceNodeId = workspaceTree.nodeIdForSurface(panels(2).id)
        )
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = panels,
        nextPaneId = PaneId(2),
        nextSurfaceId = panels.size
      )
    )

    val encoded = SessionState.fromAppState(state).asJson
    val restored = encoded
      .as[SessionState]
      .map(SessionState.toAppState(_, Theme.default))
      .getOrElse(fail("workspace session should decode"))

    restored.persisted.layout.workspaceTree shouldBe Some(workspaceTree)
    restored.persisted.layout.maximizedWorkspaceNodeId shouldBe workspaceTree.nodeIdForSurface(panels(2).id)
    restored.pinnedSurfaces.map(_.id) shouldBe state.pinnedSurfaces.map(_.id)
    restored.pinnedSurfaces.map(_.presentation) shouldBe state.pinnedSurfaces.map(_.presentation)
    restored.runtime.nextSurfaceId shouldBe panels.size
    restored.isValid shouldBe true
    encoded.hcursor.downField("schemaVersion").as[Int] shouldBe Right(SessionState.CurrentSchemaVersion)
  }
