package com.serenity.state.manager

import com.serenity.DockedPanelFixtures
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PointerShapeRecordingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(100, 32)

  private def stateWithLeftDock(): AppState =
    val base = AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(viewport)))
    DockedPanelFixtures.dock(
      base,
      SurfaceId("explorer"),
      SurfaceContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/repo")), None),
      PanelPosition.Left,
      28
    )

  "MouseHitTesting.recordPointerShape" should "record a new shape" in {
    val state  = AppState.initial
    val result = MouseTransition.run(state)(MouseHitTesting.recordPointerShape(PointerShape.Hand))._1

    result.state.runtime.pointerGesture.pointerShape shouldBe PointerShape.Hand
  }

  it should "leave the state untouched when the shape is unchanged" in {
    val state = AppState.initial

    MouseTransition.run(state)(MouseHitTesting.recordPointerShape(PointerShape.Default))._1.state shouldBe
      theSameInstanceAs(state)
  }

  "PinnedPanelLayoutEngine.pinnedPanelEdgeAt" should "find a docked panel only on its inner edge column" in {
    val state  = stateWithLeftDock()
    val layout = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val rect   = layout.pinnedPanelRects(PanelPosition.Left)

    PinnedPanelLayoutEngine.pinnedPanelEdgeAt(layout, rect.right - 1, rect.y + 2) shouldBe Some(PanelPosition.Left)
    PinnedPanelLayoutEngine.pinnedPanelEdgeAt(layout, rect.x + 1, rect.y + 2) shouldBe None
  }

  it should "find nothing without a docked panel" in {
    val layout = LayoutEngine.calculateLayoutWithUI(AppState.initial, viewport)

    PinnedPanelLayoutEngine.pinnedPanelEdgeAt(layout, 5, 5) shouldBe None
  }
