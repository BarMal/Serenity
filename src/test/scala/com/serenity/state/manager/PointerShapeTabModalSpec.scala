package com.serenity.state.manager

import com.serenity.keystroke.events.{MouseClick, MouseMove}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PointerShapeTabModalSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(60, 20)

  private def twoBufferState: AppState =
    val second = Buffer.fromString(BufferId(1), "second")
    val base   = AppState.initial
    base.copy(
      runtime = base.runtime.copy(viewportSize = Some(viewport)),
      persisted = base.persisted.copy(
        buffers = base.persisted.buffers + (second.id -> second),
        bufferOrder = base.persisted.bufferOrder :+ second.id
      )
    )

  private def withModal(state: AppState): AppState =
    val dialog = ModalDialog(
      SurfaceId("close-confirmation"),
      Modal.Confirm(ConfirmPrompt.closeUnsaved("notes.scala")),
      ModalPlacement.Centered
    )
    state.copy(runtime = state.runtime.copy(modalStack = List(dialog)))

  private val stripCols = 0 until viewport.width

  "TabBarMouseHitTesting.pointerTargetAt" should "keep the default pointer over a tab" in {
    TabBarMouseHitTesting.pointerTargetAt(twoBufferState, col = 3, row = 0) shouldBe Some(PointerHitTarget.Inert)
  }

  it should "show the hand only over a tab's close button or the new-tab button" in {
    val state   = twoBufferState
    val targets = stripCols.map(col => col -> TabBarMouseHitTesting.pointerTargetAt(state, col, row = 0))

    val hands = targets.collect { case (col, Some(PointerHitTarget.Control)) => col }
    hands should not be empty
    hands.foreach { col =>
      val onButton =
        TabBarMouseHitTesting.closeClickTarget(state, col, 0).isDefined ||
          TabBarMouseHitTesting.newTabClickTarget(state, col, 0)
      onButton shouldBe true
    }
    targets.collect { case (col, Some(PointerHitTarget.Inert)) => col } should not be empty
  }

  it should "leave points outside the strip to other targets" in {
    TabBarMouseHitTesting.pointerTargetAt(twoBufferState, col = 3, row = 5) shouldBe None
  }

  "PointerShape.shown" should "reset to the default when a blocking modal opens over a resolved shape" in {
    val hovering = twoBufferState.copy(runtime =
      twoBufferState.runtime.copy(pointerGesture = PointerGestureState(pointerShape = PointerShape.Text))
    )

    PointerShape.shown(hovering) shouldBe PointerShape.Text
    PointerShape.shown(withModal(hovering)) shouldBe PointerShape.Default
  }

  it should "show the shape the modal itself resolved while it is up, and drop it once it closes" in {
    val modalHover = withModal(twoBufferState).copy(runtime =
      withModal(twoBufferState).runtime
        .copy(pointerGesture = PointerGestureState(pointerShape = PointerShape.Hand, shapeUnderModal = true))
    )

    PointerShape.shown(modalHover) shouldBe PointerShape.Hand
    PointerShape.shown(modalHover.copy(runtime = modalHover.runtime.copy(modalStack = Nil))) shouldBe
      PointerShape.Default
  }

  "ModalMouseHitTesting.hover" should "use the hand over a modal action and the default elsewhere" in {
    val state = withModal(twoBufferState)
    val points = for
      col <- 0 until viewport.width
      row <- 0 until viewport.height
    yield (col, row)
    val onAction = points.find { (col, row) =>
      ModalMouseHitTesting.modalHitAt(MouseClick(col, row), state).exists(_._2.actionId.nonEmpty)
    }
    val (col, row) = onAction.getOrElse(fail("Expected a modal action button"))

    def shownAfter(c: Int, r: Int): PointerShape =
      PointerShape.shown(
        MouseTransition.run(state)(ModalMouseHitTesting.hover(MouseMove(c, r), state))._1.state
      )

    shownAfter(col, row) shouldBe PointerShape.Hand
    shownAfter(0, viewport.height - 1) shouldBe PointerShape.Default
  }
