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
    scene.workspace.map(_.id) should contain(SceneNodeId.Surface(panel.id))
    scene.floating.map(_.id) shouldBe List(SceneNodeId.Surface(floating.id))
    scene.nodesInPaintOrder.map(_.layer) shouldBe List(SceneLayer.Workspace, SceneLayer.Workspace, SceneLayer.Floating)
    scene.nodesInPaintOrder.foreach { node =>
      node.hitRegions.foreach(region => node.frameRect.containsRect(region.rect) shouldBe true)
    }
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

  it should "expose the calculated layout as a temporary compatibility adapter" in {
    val state = AppState.initial

    val scene = UiSceneSnapshot.from(state, viewport)

    scene.calculatedLayout shouldBe LayoutEngine.calculateLayoutWithUI(state, viewport)
    scene.paneLayouts shouldBe LayoutEngine.calculateEditorPaneLayouts(state, scene.calculatedLayout)
  }
end SceneSnapshotSpec
