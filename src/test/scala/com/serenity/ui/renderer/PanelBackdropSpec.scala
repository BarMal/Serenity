package com.serenity.ui.renderer

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.{DockedPanelFixtures, TestWorkspaceTrees}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PanelBackdropSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId    = PaneId(0)
  private val bufferId  = BufferId(1)
  private val surfaceId = SurfaceId("outline")
  private val viewport  = ViewportSize(100, 30)

  private def dockedState: AppState =
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma")
    val base = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    DockedPanelFixtures.dock(base, surfaceId, SurfaceContent.Outline(Nil), PanelPosition.Right, 22)

  private def panelNode(scene: UiSceneSnapshot): SceneNode =
    scene.workspace
      .find(_.id == SceneNodeId.Surface(surfaceId))
      .getOrElse(fail("expected the docked panel in the scene"))

  "PanelBackdrop.isClearedBackground" should "hold for a panel docked in its own workspace slot" in {
    val scene = UiSceneSnapshot.from(dockedState, viewport)

    PanelBackdrop.isClearedBackground(scene, surfaceId, panelNode(scene).frameRect) shouldBe true
  }

  it should "not hold once another workspace node is painted under the panel" in {
    val scene      = UiSceneSnapshot.from(dockedState, viewport)
    val panel      = panelNode(scene)
    val underneath = panel.copy(id = SceneNodeId.EditorPane(PaneId(7)), frameRect = panel.frameRect.copy(width = 2))

    PanelBackdrop.isClearedBackground(
      scene.copy(workspace = scene.workspace :+ underneath),
      surfaceId,
      panel.frameRect
    ) shouldBe false
  }

  it should "not hold where an editor spacer column runs under the panel" in {
    val scene    = UiSceneSnapshot.from(dockedState, viewport)
    val panel    = panelNode(scene)
    val contract = scene.editorContract.copy(rightSpacerRect = panel.frameRect.copy(height = 1))

    PanelBackdrop.isClearedBackground(scene.copy(editorContract = contract), surfaceId, panel.frameRect) shouldBe false
  }
