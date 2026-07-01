package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.TextAreaInsets
import com.serenity.keystroke.events.{MouseDrag, ResizeEvent}
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextAreaResizeSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  "Text area resizing" should "update the left text area inset from mouse drags without resizing side panels" in {
    val stateManager = createStateManager("TextAreaResizeSpec")

    stateManager
      .updateState(state =>
        state.copy(config = state.config.withTextAreaInsets(TextAreaInsets(left = 0.10, right = 0.0)))
      )
      .unsafeRunSync()
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Left, 10).unsafeRunSync()
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 20).unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()

    val before       = stateManager.getCurrentState.unsafeRunSync()
    val beforeLayout = LayoutEngine.calculateLayout(before, ViewportSize(100, 30))
    val dragColumn   = beforeLayout.leftSpacerRect.x + 4

    stateManager.applyEvent(MouseDrag(dragColumn, beforeLayout.leftSpacerRect.y + 2)).unsafeRunSync()

    val after       = stateManager.getCurrentState.unsafeRunSync()
    val afterLayout = LayoutEngine.calculateLayout(after, ViewportSize(100, 30))

    after.config.textAreaInsets.left shouldBe (4.0 / 70.0) +- 0.0001
    after.config.textAreaInsets.right shouldBe before.config.textAreaInsets.right
    afterLayout.pinnedPanelRects(PanelPosition.Left).width shouldBe 10
    afterLayout.pinnedPanelRects(PanelPosition.Right).width shouldBe 20
    afterLayout.editorPanelRect.x should be < beforeLayout.editorPanelRect.x
  }
