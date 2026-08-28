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
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withTextAreaInsets(TextAreaInsets(left = 0.10, right = 0.0)))
        )
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

    after.persisted.config.surfaceConfig.textAreaInsets.left shouldBe (4.0 / 70.0) +- 0.0001
    after.persisted.config.surfaceConfig.textAreaInsets.right shouldBe before.persisted.config.surfaceConfig.textAreaInsets.right
    afterLayout.pinnedPanelRects(PanelPosition.Left).width shouldBe 10
    afterLayout.pinnedPanelRects(PanelPosition.Right).width shouldBe 20
    afterLayout.editorPanelRect.x should be < beforeLayout.editorPanelRect.x
  }

  it should "update the top text area inset from mouse drags before release" in {
    val stateManager = createStateManager("TextAreaResizeSpec")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config.withTextAreaInsets(TextAreaInsets(left = 0.0, right = 0.0, top = 0.20))
          )
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(80, 30))).unsafeRunSync()

    val before        = stateManager.getCurrentState.unsafeRunSync()
    val beforeLayout  = LayoutEngine.calculateLayout(before, ViewportSize(80, 30))
    val dragRow       = beforeLayout.topSpacerRect.y + 3
    val contentHeight = beforeLayout.editorPanelRect.bottom - beforeLayout.topSpacerRect.y

    stateManager.applyEvent(MouseDrag(beforeLayout.topSpacerRect.x + 2, dragRow)).unsafeRunSync()

    val after       = stateManager.getCurrentState.unsafeRunSync()
    val afterLayout = LayoutEngine.calculateLayout(after, ViewportSize(80, 30))

    after.persisted.config.surfaceConfig.textAreaInsets.top shouldBe (3.0 / contentHeight.toDouble) +- 0.0001
    after.persisted.config.surfaceConfig.textAreaInsets.bottom shouldBe before.persisted.config.surfaceConfig.textAreaInsets.bottom
    afterLayout.topSpacerRect.height should be < beforeLayout.topSpacerRect.height
    afterLayout.editorPanelRect.y shouldBe beforeLayout.editorPanelRect.y
  }

  it should "update the bottom text area inset from mouse drags before release" in {
    val stateManager = createStateManager("TextAreaResizeSpec")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config.withTextAreaInsets(TextAreaInsets(left = 0.0, right = 0.0, bottom = 0.20))
          )
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(80, 30))).unsafeRunSync()

    val before        = stateManager.getCurrentState.unsafeRunSync()
    val beforeLayout  = LayoutEngine.calculateLayout(before, ViewportSize(80, 30))
    val dragRow       = beforeLayout.bottomSpacerRect.bottom - 3
    val contentHeight = beforeLayout.editorPanelRect.bottom - beforeLayout.topSpacerRect.y

    stateManager.applyEvent(MouseDrag(beforeLayout.bottomSpacerRect.x + 2, dragRow)).unsafeRunSync()

    val after       = stateManager.getCurrentState.unsafeRunSync()
    val afterLayout = LayoutEngine.calculateLayout(after, ViewportSize(80, 30))

    after.persisted.config.surfaceConfig.textAreaInsets.top shouldBe before.persisted.config.surfaceConfig.textAreaInsets.top
    after.persisted.config.surfaceConfig.textAreaInsets.bottom shouldBe (3.0 / contentHeight.toDouble) +- 0.0001
    afterLayout.bottomSpacerRect.height should be < beforeLayout.bottomSpacerRect.height
    afterLayout.editorPanelRect.y shouldBe beforeLayout.editorPanelRect.y
  }
