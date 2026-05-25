package com.serenity

import com.serenity.keystroke.events.{Enter, InsertChar}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, CursorPosition, FindState, Focus, Modal, ModalType, PaneId}
import com.serenity.state.reducers.ModalEventReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ModalEventReducer" should "append digits in goto line mode" in {
    val initialState = AppState.initial.copy(
      modal = Some(Modal.GotoLine("1")),
      focus = Focus.Modal(ModalType.GotoLine)
    )

    val updatedState = ModalEventReducer.reduce(ModalType.GotoLine, InsertChar('2'), initialState).state

    updatedState.modal shouldBe Some(Modal.GotoLine("12"))
  }

  it should "jump to the requested line and dismiss the goto line modal" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      modal = Some(Modal.GotoLine("3")),
      focus = Focus.Modal(ModalType.GotoLine),
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(content = com.serenity.rope.Rope("a\nb\nc\nd"))
      )
    )

    val updatedState = ModalEventReducer.reduce(ModalType.GotoLine, Enter, initialState).state

    updatedState.modal shouldBe None
    updatedState.focus shouldBe Focus.EditorPane(paneId)
    updatedState.buffers(bufferId).cursors.head shouldBe CursorPosition(2, 0)
  }

  it should "compute find results and move the cursor to the first hit" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      modal = Some(Modal.Find("needle", Nil, 0)),
      focus = Focus.Modal(ModalType.Find),
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(content = com.serenity.rope.Rope("x\nneedle here\ny\nneedle again"))
      )
    )

    val updatedState = ModalEventReducer.reduce(ModalType.Find, Enter, initialState).state

    updatedState.modal shouldBe None
    updatedState.focus shouldBe Focus.EditorPane(paneId)
    updatedState.findState shouldBe Some(FindState("needle", List(1, 3), 0))
    updatedState.buffers(bufferId).cursors.head shouldBe CursorPosition(1, 0)
  }
