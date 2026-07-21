package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SceneSnapshotSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(100, 32)

  "UiSceneSnapshot" should "describe editor panes, pinned panels, and floating surfaces in paint order" in {
    val paneId = PaneId(0)
    val panel = UiSurface(
      SurfaceId("outline"),
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Pinned(PanelPosition.Left, 20)
    )
    val floating = UiSurface(
      SurfaceId("quick-info"),
      SurfaceContent.QuickInfo("map"),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.AboveCursor)
    )
    val state = AppState.initial.copy(uiSurfaces = List(panel, floating))

    val scene = UiSceneSnapshot.from(state, viewport)

    scene.workspace.map(_.id) should contain(SceneNodeId.EditorPane(paneId))
    scene.workspace.map(_.id) should contain(SceneNodeId.EditorPaneHeader(paneId))
    scene.workspace.map(_.id) should contain(SceneNodeId.Surface(panel.id))
    scene.floating.map(_.id) shouldBe List(SceneNodeId.Surface(floating.id))
    scene.nodesInPaintOrder.map(_.layer) shouldBe List(
      SceneLayer.Workspace,
      SceneLayer.Workspace,
      SceneLayer.Workspace,
      SceneLayer.Floating
    )
    scene.nodesInPaintOrder.foreach { node =>
      node.hitRegions.foreach(region => node.frameRect.containsRect(region.rect) shouldBe true)
    }
  }

  it should "own the rendered active-pane header as a contained workspace node" in {
    val paneId = PaneId(0)
    val scene  = UiSceneSnapshot.from(AppState.initial, viewport)
    val pane = scene.workspace
      .find(_.id == SceneNodeId.EditorPane(paneId))
      .getOrElse(fail("expected editor pane"))
    val header = scene.workspace
      .find(_.id == SceneNodeId.EditorPaneHeader(paneId))
      .getOrElse(fail("expected active-pane header"))

    header.layer shouldBe SceneLayer.Workspace
    header.frameRect shouldBe scene.paneLayouts(paneId).headerRect
    header.frameRect.width should be >= pane.frameRect.width
    header.hitRegions.map(_.kind) should contain(SceneHitKind.Header)
    header.hitRegions.foreach(region => header.frameRect.containsRect(region.rect) shouldBe true)
    scene.focusOrder should not contain header.id
  }

  it should "retain the current floating presentation for modal workflows" in {
    val modal = UiSurface(
      SurfaceId("find"),
      SurfaceContent.ModalWorkflow(Modal.Find("needle", Nil, 0)),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.BelowCursor)
    )

    val scene = UiSceneSnapshot.from(AppState.initial.copy(uiSurfaces = List(modal)), viewport)

    scene.modalBackdrop shouldBe None
    scene.modal shouldBe Nil
    scene.floating.map(_.id) shouldBe List(SceneNodeId.Surface(modal.id))
  }

  it should "characterize multi-pane geometry and place the focused pane first in focus order" in {
    val firstPane  = PaneId(0)
    val secondPane = PaneId(1)
    val state = AppState.initial.copy(
      layout = Layout(
        editorPanes = Map(
          firstPane  -> EditorPane.empty(firstPane),
          secondPane -> EditorPane.empty(secondPane)
        ),
        activeEditorPaneId = Some(secondPane),
        paneOrder = List(firstPane, secondPane)
      ),
      focus = Focus.EditorPane(secondPane)
    )

    val scene = UiSceneSnapshot.from(state, viewport)

    scene.workspace.map(_.id) should contain allOf (
      SceneNodeId.EditorPane(firstPane),
      SceneNodeId.EditorPane(secondPane)
    )
    scene.focusOrder.headOption shouldBe Some(SceneNodeId.EditorPane(secondPane))
    scene.workspace.foreach { node =>
      node.hitRegions.foreach(region => node.frameRect.containsRect(region.rect) shouldBe true)
    }
  }

  it should "characterize an expanded surface as a workspace node and focus target" in {
    val surface = UiSurface(
      SurfaceId("diagnostics"),
      SurfaceContent.Diagnostics(Nil),
      SurfacePresentation.Expanded(PanelPosition.Right, 22)
    )
    val state = AppState.initial.copy(uiSurfaces = List(surface), focus = Focus.Surface(surface.id))

    val scene = UiSceneSnapshot.from(state, viewport)
    val expanded =
      scene.workspace.find(_.id == SceneNodeId.Surface(surface.id)).getOrElse(fail("expected expanded surface"))

    scene.calculatedLayout.expandedPanelRect shouldBe Some(expanded.frameRect)
    scene.focusOrder.headOption shouldBe Some(SceneNodeId.Surface(surface.id))
  }

  it should "expose the calculated layout as a temporary compatibility adapter" in {
    val state = AppState.initial

    val scene = UiSceneSnapshot.from(state, viewport)

    scene.calculatedLayout shouldBe LayoutEngine.calculateLayoutWithUI(state, viewport)
    scene.paneLayouts shouldBe LayoutEngine.calculateEditorPaneLayouts(state, scene.calculatedLayout)
  }
end SceneSnapshotSpec
