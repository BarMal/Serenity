package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end scenario coverage (#821) for the blocking modal layer (#814): a parent dialog with a nested child
  * confirmation on top, rendered through the full state-pipeline-plus-renderer pass rather than only at the reducer
  * or layer-compositing unit level (see `ModalStateReducerSpec`, `ModalLayerCompositingSpec`).
  */
class ModalWorkflowUiScenarioSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "UiScenarioDriver" should "render a blocking modal with a nested child confirmation on top, trapping focus" in {
    val driver  = UiScenarioDriver.create("modal-with-child-confirmation").unsafeRunSync()
    val bufferId = BufferId(0)

    val parent = ModalDialog(
      SurfaceId("close-confirmation"),
      Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala")),
      ModalPlacement.Centered
    )
    val child = ModalDialog(
      SurfaceId("save-before-close"),
      Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "notes.scala")),
      ModalPlacement.Centered
    )

    driver
      .updateState { state =>
        state.copy(
          persisted = state.persisted.copy(focus = Focus.Modal),
          runtime = state.runtime.copy(modalStack = List(parent, child))
        )
      }
      .unsafeRunSync()

    val state = driver.state.unsafeRunSync()
    state.persisted.focus shouldBe Focus.Modal
    state.runtime.modalStack shouldBe List(parent, child)
    state.runtime.modalStack.last shouldBe child

    val frame = driver.renderFrame("blocking-modal-with-child").unsafeRunSync()
    frame.evidence.focus shouldBe Focus.Modal
    frame.evidence.layoutViolations shouldBe empty
    // The topmost (child) dialog is the one actually visible/interactive: its content renders.
    val drawn = frame.evidence.drawnText.map(_.text).mkString(" ")
    drawn should include("notes.scala")
  }
