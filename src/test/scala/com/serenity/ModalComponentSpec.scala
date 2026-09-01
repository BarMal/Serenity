package com.serenity

import com.serenity.keystroke.events.{InsertChar, TabKey, UnhandledEvent}
import com.serenity.keystroke.translators.Translator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, ModalComponent}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private object NoopTranslator extends Translator[com.serenity.keystroke.events.Event]:
    val converters = List.empty

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
    val component = ModalComponent(ModalType.GotoLine)

    component.processEvent(InsertChar('4'), modalState(Modal.GotoLine("1"))) match
      case ComponentResult.ReducerUpdate(result) =>
        result.state.modalSurface.map(_.content) shouldBe
          Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("14")))
        result.effects shouldBe Nil
      case other =>
        fail(s"Expected reducer update, got $other")
  }

  it should "dismiss custom modals on enter and escape" in {
    val component   = ModalComponent(ModalType.Custom("signature-help"))
    val enterEvent  = UnhandledEvent(KeyStrokeInfo(InputKey.Enter, None, Set.empty), NoopTranslator)
    val escapeEvent = UnhandledEvent(KeyStrokeInfo(InputKey.Escape, None, Set.empty), NoopTranslator)

    component.processEvent(
      enterEvent,
      modalState(Modal.Custom("signature-help", "map("))
    ) shouldBe ComponentResult.Dismiss
    component.processEvent(
      escapeEvent,
      modalState(Modal.Custom("signature-help", "map("))
    ) shouldBe ComponentResult.Dismiss
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

  it should "route close workflow modals through the reducer path" in {
    val component = ModalComponent(ModalType.CloseWorkflow)
    val initial = modalState(
      Modal.CloseWorkflow(
        CloseWorkflowState(
          scope = CloseScope.Current,
          currentBufferId = BufferId(0),
          currentBufferLabel = "notes.scala"
        )
      )
    )

    component.processEvent(TabKey, initial) match
      case ComponentResult.ReducerUpdate(result) =>
        result.state.modalSurface.flatMap(_.content match
          case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow.selectedChoice)
          case _ => None) shouldBe Some(CloseWorkflowChoice.Discard)
        result.effects shouldBe Nil
      case other =>
        fail(s"Expected reducer update, got $other")
  }
end ModalComponentSpec
