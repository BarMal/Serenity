package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalEventReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalGotoLineReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ModalEventReducer" should "append digits in goto line mode" in {
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        focus = Focus.Surface(SurfaceId("goto-line"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("goto-line"),
            SurfaceContent.ModalWorkflow(Modal.GotoLine("1")),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val updatedState = ModalEventReducer.reduce(ModalType.GotoLine, InsertChar('2'), initialState).state

    updatedState.modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("12")))
  }

  it should "jump to the requested line and dismiss the goto line modal" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        focus = Focus.Surface(SurfaceId("goto-line")),
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(document =
              AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("a\nb\nc\nd"))
            )
        )
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("goto-line"),
            SurfaceContent.ModalWorkflow(Modal.GotoLine("3")),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val updatedState = ModalEventReducer.reduce(ModalType.GotoLine, Enter, initialState).state

    updatedState.modalSurface shouldBe None
    updatedState.persisted.focus shouldBe Focus.EditorPane(paneId)
    updatedState.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(2, 0)
  }
