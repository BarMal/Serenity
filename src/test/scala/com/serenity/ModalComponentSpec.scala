package com.serenity

import com.serenity.keystroke.events.{InsertChar, TabKey}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, ModalComponent}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def modalState(modal: Modal): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        focus = Focus.Surface(SurfaceId("modal"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("modal"),
            SurfaceContent.ModalWorkflow(modal),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  "ModalComponent" should "update goto line modals through the reducer path" in {
    val component = ModalComponent(ModalType.TextPrompt)

    component.processEvent(InsertChar('4'), modalState(Modal.TextPrompt(TextPrompt.gotoLine("1")))) match
      case ComponentResult.ReducerUpdate(result) =>
        result.state.modalSurface.map(_.content) shouldBe
          Some(SurfaceContent.ModalWorkflow(Modal.TextPrompt(TextPrompt.gotoLine("14"))))
        result.effects shouldBe Nil
      case other =>
        fail(s"Expected reducer update, got $other")
  }

  it should "route file workflow modals through the reducer path" in {
    val component = ModalComponent(ModalType.FileWorkflow)
    val initial = modalState(
      Modal.FileWorkflow(
        FileWorkflowState(mode = FileWorkflowMode.SaveAs)
      )
    )

    component.processEvent(TabKey, initial) match
      case ComponentResult.ReducerUpdate(result) =>
        // SaveAs now cycles Filename -> Format -> Path, so a single tab lands on Format.
        result.state.modalSurface.map(_.content) shouldBe
          Some(
            SurfaceContent.ModalWorkflow(
              Modal.FileWorkflow(
                FileWorkflowState(
                  mode = FileWorkflowMode.SaveAs,
                  activeField = FileWorkflowField.Format
                )
              )
            )
          )
        result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(SurfaceId("modal"))))
      case other =>
        fail(s"Expected reducer update, got $other")
  }

  it should "route replace workflow modals through the reducer path" in {
    val component = ModalComponent(ModalType.ReplaceWorkflow)
    val initial = modalState(
      Modal.ReplaceWorkflow(
        ReplaceWorkflowState()
      )
    )

    component.processEvent(TabKey, initial) match
      case ComponentResult.ReducerUpdate(result) =>
        result.state.modalSurface.map(_.content) shouldBe
          Some(
            SurfaceContent.ModalWorkflow(
              Modal.ReplaceWorkflow(
                ReplaceWorkflowState(
                  activeField = ReplaceWorkflowField.ReplaceWith
                )
              )
            )
          )
        result.effects shouldBe Nil
      case other =>
        fail(s"Expected reducer update, got $other")
  }

  it should "route the close prompt through the reducer path" in {
    val component = ModalComponent(ModalType.Confirm)
    val initial   = modalState(Modal.Confirm(ConfirmPrompt.closeUnsaved("notes.scala")))

    component.processEvent(TabKey, initial) match
      case ComponentResult.ReducerUpdate(result) =>
        result.state.modalSurface.flatMap(_.content match
          case SurfaceContent.ModalWorkflow(Modal.Confirm(prompt)) => prompt.selectedChoice.map(_.label)
          case _                                                   => None) shouldBe Some("Close Anyway")
        result.effects shouldBe Nil
      case other =>
        fail(s"Expected reducer update, got $other")
  }
end ModalComponentSpec
