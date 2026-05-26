package com.serenity

import com.serenity.keystroke.events.{InsertChar, OpenGotoLine, ScrollDown}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Focus, Modal, PaneId, SurfaceContent}
import com.serenity.state.reducers.EditorEventReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorEventReducer" should "insert characters into the focused pane buffer" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)

    val updatedState = EditorEventReducer.reduce(InsertChar('x'), paneId, initialState).state
    val bufferId     = updatedState.layout.editorPanes(paneId).bufferId.get
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "x"
    buffer.cursors.head shouldBe com.serenity.state.models.CursorPosition(0, 1)
    buffer.isDirty shouldBe true
  }

  it should "update viewport position for scroll events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)
    val bufferId     = initialState.layout.editorPanes(paneId).bufferId.get
    val seededState = initialState.copy(
      buffers = initialState.buffers.updated(
        bufferId,
        initialState.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("a\nb\nc\nd\ne\nf\ng"),
          viewport = initialState.buffers(bufferId).viewport.copy(visibleLines = 2)
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(ScrollDown(3), paneId, seededState).state

    updatedState.buffers(bufferId).viewport.topLine shouldBe 3
  }

  it should "open the goto line modal from editor events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)

    val updatedState = EditorEventReducer.reduce(OpenGotoLine, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }
