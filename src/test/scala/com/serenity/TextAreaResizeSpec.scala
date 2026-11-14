package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.{Gutter, TextAreaInsets}
import com.serenity.keystroke.events.{MouseDrag, ResizeEvent}
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.testkit.ActivePane
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextAreaResizeSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def share(gutter: Gutter): Double =
    gutter match
      case Gutter.Percent(share) => share
      case other                 => fail(s"expected a percentage, got $other")

  "Text area resizing" should "update the left text area inset from mouse drags without resizing side panels" in {
    val stateManager = createStateManager("TextAreaResizeSpec")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withTextAreaInsets(TextAreaInsets.percent(left = 0.10, right = 0.0)))
        )
      )
      .unsafeRunSync()
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Left, 10).unsafeRunSync()
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 20).unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()

    val before       = stateManager.getCurrentState.unsafeRunSync()
    val beforeLayout = LayoutEngine.calculateLayout(before, ViewportSize(100, 30))
    val dragColumn   = ActivePane.of(before, beforeLayout).leftSpacerRect.x + 4

    stateManager
      .applyEvent(MouseDrag(dragColumn, ActivePane.of(before, beforeLayout).leftSpacerRect.y + 2))
      .unsafeRunSync()

    val after       = stateManager.getCurrentState.unsafeRunSync()
    val afterLayout = LayoutEngine.calculateLayout(after, ViewportSize(100, 30))

    // Denominator is 68, not 70: with both Left and Right docked, the workspace width now also gives up a cell to
    // each side for the unset `ui.element_gap`'s GUI default (previously zero on both).
    share(after.persisted.config.surfaceConfig.textAreaInsets.left) shouldBe (4.0 / 68.0) +- 0.0001
    after.persisted.config.surfaceConfig.textAreaInsets.right shouldBe before.persisted.config.surfaceConfig.textAreaInsets.right
    afterLayout.pinnedPanelRects(PanelPosition.Left).width shouldBe 10
    afterLayout.pinnedPanelRects(PanelPosition.Right).width shouldBe 20
    ActivePane.of(after, afterLayout).contentRect.x should be < ActivePane.of(before, beforeLayout).contentRect.x
  }

  it should "update the top text area inset from mouse drags before release" in {
    val stateManager = createStateManager("TextAreaResizeSpec")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config.withTextAreaInsets(TextAreaInsets.percent(left = 0.0, right = 0.0, top = 0.20))
          )
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(80, 30))).unsafeRunSync()

    val before        = stateManager.getCurrentState.unsafeRunSync()
    val beforeLayout  = LayoutEngine.calculateLayout(before, ViewportSize(80, 30))
    val dragRow       = ActivePane.of(before, beforeLayout).topSpacerRect.y + 3
    val contentHeight = beforeLayout.editorPanelRect.bottom - ActivePane.of(before, beforeLayout).topSpacerRect.y

    stateManager.applyEvent(MouseDrag(ActivePane.of(before, beforeLayout).topSpacerRect.x + 2, dragRow)).unsafeRunSync()

    val after       = stateManager.getCurrentState.unsafeRunSync()
    val afterLayout = LayoutEngine.calculateLayout(after, ViewportSize(80, 30))

    share(after.persisted.config.surfaceConfig.textAreaInsets.top) shouldBe (3.0 / contentHeight.toDouble) +- 0.0001
    after.persisted.config.surfaceConfig.textAreaInsets.bottom shouldBe before.persisted.config.surfaceConfig.textAreaInsets.bottom
    ActivePane
      .of(after, afterLayout)
      .topSpacerRect
      .height should be < ActivePane.of(before, beforeLayout).topSpacerRect.height
    afterLayout.editorPanelRect.y shouldBe beforeLayout.editorPanelRect.y
  }

  it should "update the bottom text area inset from mouse drags before release" in {
    val stateManager = createStateManager("TextAreaResizeSpec")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config.withTextAreaInsets(TextAreaInsets.percent(left = 0.0, right = 0.0, bottom = 0.20))
          )
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(80, 30))).unsafeRunSync()

    val before        = stateManager.getCurrentState.unsafeRunSync()
    val beforeLayout  = LayoutEngine.calculateLayout(before, ViewportSize(80, 30))
    val dragRow       = ActivePane.of(before, beforeLayout).bottomSpacerRect.bottom - 3
    val contentHeight = beforeLayout.editorPanelRect.bottom - ActivePane.of(before, beforeLayout).topSpacerRect.y

    stateManager
      .applyEvent(MouseDrag(ActivePane.of(before, beforeLayout).bottomSpacerRect.x + 2, dragRow))
      .unsafeRunSync()

    val after       = stateManager.getCurrentState.unsafeRunSync()
    val afterLayout = LayoutEngine.calculateLayout(after, ViewportSize(80, 30))

    after.persisted.config.surfaceConfig.textAreaInsets.top shouldBe before.persisted.config.surfaceConfig.textAreaInsets.top
    share(after.persisted.config.surfaceConfig.textAreaInsets.bottom) shouldBe (3.0 / contentHeight.toDouble) +- 0.0001
    ActivePane
      .of(after, afterLayout)
      .bottomSpacerRect
      .height should be < ActivePane.of(before, beforeLayout).bottomSpacerRect.height
    afterLayout.editorPanelRect.y shouldBe beforeLayout.editorPanelRect.y
  }
